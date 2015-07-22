subroutine grids_and_gridmaps (n_swan_grids, n_flow_grids, sr, mode)
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
!  $Id: grids_and_gridmaps.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/kernel/src/grids_and_gridmaps.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use swan_flow_grid_maps
   use swan_input
   use flow_data
   use wave_data
   !
   implicit none
!
! local parameters
!
   integer, parameter :: nswmax=10
!
! Global variables
!
   integer                             :: n_swan_grids     ! number of SWAN grids
   integer                             :: n_flow_grids     ! number of FLOW grids
   integer                             :: mode
   type(swan)                          :: sr
!
! Local variables
!
   integer                           :: i
   integer                           :: j
   integer                           :: i_swan
   integer                           :: lun              ! file unit
   integer, external                 :: new_lun
   logical                           :: exists
   character (256) ,dimension(nswmax):: swangrid
   character (256)                   :: grid_name        ! name of grid
   character (4)                     :: grid_file_type   ! type of grid file (SWAN/FLOW/COM/TRIM)
   character (6)                     :: xy_loc           ! location of xy coords (CORNER/CENTER)
   type(grid)    ,pointer            :: f,s              ! local pointers to grids
   type(grid_map),pointer            :: f2s,s2f          ! local pointers to gridmaps
!
!! executable statements -------------------------------------------------------
!
   ! Find out number of FLOW domains
   !
   write(*,'(a)') '  Initialising grids and grid mappings'
   if (mode == stand_alone) then
      !
      ! If flow results are used
      !
      if (sr%useflowdata .or. sr%swwav) then
         n_flow_grids = 1
      else
         n_flow_grids = 0
      endif
   else
      n_flow_grids=num_subdomains
   endif

! Find out number of SWAN grids

   lun = new_lun()
   i_swan=1
   inquire(file='swangrids', exist=exists)
   if (exists) then
      open(lun, file='swangrids')
      do
         read(lun,'(a)')swangrid(i_swan)
         if (trim(swangrid(i_swan)).eq.'end') exit
         i_swan=i_swan+1
      enddo
      n_swan_grids=i_swan-1
      close(lun)
   elseif (sr%nnest>0) then
      do i_swan=1,sr%nnest
         swangrid(i_swan)=sr%dom(i_swan)%curlif
      enddo
   else
      write(*,'(a)') '*** ERROR: File swangrids not available or error reading mdw file'
      stop
   endif

   ! Allocate pointer arrays grid structures
   call Init_Grids (n_swan_grids, n_flow_grids)

   do i=1,n_swan_grids
      grid_name = swangrid(i)
      grid_file_type ='FLOW'
      xy_loc         ='CORNER'
      call Alloc_and_get_grid(swan_grids(i),grid_name,grid_file_type,xy_loc)
      call write_bnd_loc(i,swan_grids(i))
   enddo

   do i=1,n_flow_grids
      if (.not. flow_data_initialized) then
         write(*,'(a)') '*** ERROR: FLOW data (runid(s)) is not initialized.'
         stop
      endif
      write(grid_name,'(a,a)')'com-',trim(runids(i))
      grid_file_type ='COM'
      xy_loc         ='CENTER'
      call Alloc_and_get_grid(flow_grids(i),grid_name,grid_file_type,xy_loc)
   enddo

   do i=1,n_swan_grids
      s=>swan_grids(i)
      do j=1,n_flow_grids
         f=>flow_grids(j)
         f2s=>flow2swan_maps(i,j)
         s2f=>swan2flow_maps(i,j)
         call make_grid_map(f,s,f2s)
         call make_grid_map(s,f,s2f)
      enddo
   enddo

end subroutine grids_and_gridmaps
