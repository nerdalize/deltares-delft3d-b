module flow_data
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: flow_data.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/data/src/flow_data.f90 $
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision_basics
!
integer                                 , save :: num_subdomains        = 0
integer                                 , save :: num_muddomains        = 0
logical                                 , save :: flow_data_initialized = .false.
character(256), dimension(:),allocatable, save :: runids
character(256), dimension(:),allocatable, save :: mudids
!
!
contains
!
!
!===============================================================================
subroutine flow_init (mode, it01, tscale)
!
! Initializes communication with flow computation
! Fills flow_data parameters
!
   use swan_input    ! Needed when running stand_alone
   use sync_flowwave
   use wave_data
   !
   implicit none
!
! Global variables
!
   integer, intent(in) :: mode
   integer             :: it01
   real                :: tscale
!
! Local variables
!
   integer                          :: idom
   real(hp)                         :: d_tscale
   real                             :: dummy     = -999.0
   logical                          :: mud
   character(80), dimension(25)     :: subdom_names
   character(256)                   :: filnam
!
!! executable statements -------------------------------------------------------
!
   mud          = .false.
   subdom_names = ' '
   if (mode == stand_alone) then
      if (swan_run%useflowdata .or. swan_run%swwav) then
         num_subdomains = 1
         allocate (runids(num_subdomains))
         runids(1) = swan_run%casl
         write(*,'(a,i0,a)') '*** MESSAGE: Using data from the following FLOW domain:'
         write(*,'(13x,a)') trim(runids(1))
         write(filnam,'(2a)') 'com-',trim(runids(1))
         call get_params(tscale, dummy, filnam)
         flow_data_initialized = .true.
      endif
   else
      write (*,'(a)') '  Waiting for initialisation from FLOW'
      num_subdomains = wave_from_flow_init(subdom_names, it01, d_tscale, mud)
      tscale        = real(d_tscale,sp)
      if (num_subdomains < 1) then
         write(*,'(a)') '*** ERROR: Delftio initialization WAVE side failed'
         write(*,'(a)') '           Is file ''dioconfig.ini'' present?'
         stop
      endif
      allocate (runids(num_subdomains))
      write(*,'(a,i0,a)') '*** MESSAGE: Connected to the following ',num_subdomains,' FLOW domain(s):'
      do idom = 1,num_subdomains
         runids(idom) = subdom_names(idom)
         write(*,'(13x,a)') trim(runids(idom))
      enddo
      call wave_to_flow_status(flow_wave_comm_result_ok, mud)
      !
      if (mode == flow_mud_online) then
         mud          = .true.
         subdom_names = ' '
         write (*,'(a)') '  Waiting for initialisation from MUD layer'
         num_muddomains = wave_from_flow_init(subdom_names, it01, d_tscale, mud)
         if (num_muddomains < 1) then
            write(*,'(a)') '*** ERROR: Delftio initialization WAVE side failed'
            write(*,'(a)') '           Is file ''dioconfig.ini'' present?'
            stop
         endif
         allocate (mudids(num_muddomains))
         write(*,'(a,i0,a)') '*** MESSAGE: Connected to the following ',num_muddomains,' MUD domain(s):'
         do idom = 1,num_muddomains
            mudids(idom) = subdom_names(idom)
            write(*,'(13x,a)') trim(mudids(idom))
         enddo
         if (num_muddomains /= 1) then
            write(*,'(a)') '*** ERROR: Interaction with Fluid Mud is currently only possible for one domain'
            call wave_to_flow_status(flow_wave_comm_error, mud)
            stop
         endif
         if (num_muddomains /= num_subdomains) then
            write(*,'(a,2(i0,a))') '*** ERROR: number of mud domains (', num_muddomains, &
                & ') is not equal to the number of water domains (',num_subdomains,').'
            call wave_to_flow_status(flow_wave_comm_error, mud)
            stop
         endif
         call wave_to_flow_status(flow_wave_comm_result_ok, mud)
      endif
      flow_data_initialized = .true.
   endif
end subroutine flow_init
!
!
!===============================================================================
subroutine deallocate_flow_data ()
   integer :: ierr
   deallocate (runids, stat=ierr)
   if (num_muddomains > 0) then
      deallocate (mudids, stat=ierr)
   endif
end subroutine deallocate_flow_data

end module flow_data
