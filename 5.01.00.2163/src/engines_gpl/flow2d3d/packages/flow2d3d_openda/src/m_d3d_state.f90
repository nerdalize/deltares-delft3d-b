module m_d3d_state
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
!  $Id: m_d3d_state.f90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/flow2d3d_openda/src/m_d3d_state.f90 $
!-------------------------------------------------------------------------------
!
use precision   ! for fp

type t_d3d_corestate
    real(fp), dimension(:,:,:), allocatable  :: u
    real(fp), dimension(:,:,:), allocatable  :: v
    real(fp), dimension(:,:), allocatable  :: sep
    real(fp), dimension(:,:), allocatable  :: dp
    real(fp), dimension(:,:), allocatable  :: umnldf
    real(fp), dimension(:,:), allocatable  :: vmnldf
    real(fp), dimension(:,:,:,:), allocatable  :: rtur
    real(fp), dimension(:,:,:,:), allocatable  :: r1 

end type t_d3d_corestate
    
type t_d3d_pseudostate    
    
    character*256 :: runid, trifil, comfil

    ! can better be recomputed than saved in the pseudostate:    
    real(fp), dimension(:,:,:), allocatable  :: w
    real(fp), dimension(:,:,:), allocatable  :: vicww
    real(fp), dimension(:,:,:), allocatable  :: vicuv
   
    ! necessary for drying and flooding
    integer, dimension(:,:), allocatable :: kfu
    integer, dimension(:,:), allocatable :: kfv
    integer, dimension(:,:), allocatable :: kfs
  
    
  
    real(fp), dimension(:,:), allocatable :: rint
    real(fp), dimension(:,:,:), allocatable :: hydrbc 
    real(fp), dimension(:,:,:,:), allocatable :: procbc 
    real(fp), dimension(:), allocatable :: disch
             
    integer :: timestep
    integer :: trisol_ifirst
    integer :: gdinttim_itstrt, gdinttim_itfinish,gdinttim_ntstep
    integer :: gdinttim_itstop, gdinttim_itinit
    integer :: gdinttim_time_nodal_update_bnd,gdinttim_time_nodal_update_tgf

end type t_d3d_pseudostate

type t_d3d_outputstate
    integer :: itcomc
    integer :: ithisc, ithisi, iphisc
    integer :: ipmapc  
    integer :: itmapc, itmapi, itmapl
    integer :: itdroi
    integer :: nofou
    integer :: itrstc
end type t_d3d_outputstate


!--------------------
type t_d3d_state
   type (t_d3d_corestate) :: core
   type (t_d3d_pseudostate) :: pseudo 
   type (t_d3d_outputstate) :: output
   integer :: color
end type t_d3d_state

type t_size_d3d_state
  integer :: d3d_state
  integer :: core, pseudo, output
  integer :: sep, u,  rtur, r1
  integer :: w, vicuv, kfs, rint, disch, hydrbc, procbc

end type t_size_d3d_state 

!------------------------

integer, parameter :: background_state = 1
integer, parameter :: ordinary_state = 2   
integer, parameter :: mean_state = 3



!---------------------------
integer, dimension(11) :: d3d_dims

type(t_size_d3d_state) :: size_d3d_state

! here the a total of <nmodes> states can be defined
type(t_d3d_state),allocatable :: d3d_state(:)


contains


  subroutine m_d3d_adjust_output_state(d3d_state, color,imode)
   
   implicit none

   type(t_d3d_state) :: d3d_state
   
   character*3 :: ch_imode
   character*256 :: tmpchar
   integer :: ic, color  ,imode

    d3d_state%color  = color
    if (d3d_state%color .ne. background_state) then
 
      d3d_state%output%itcomc = -1
      d3d_state%output%ithisi = 0
      d3d_state%output%iphisc = -1
      d3d_state%output%ipmapc = -1         
      d3d_state%output%itmapi = 0
      d3d_state%output%itmapl = -1
      d3d_state%output%itdroi = 0
      d3d_state%output%nofou = 0
      d3d_state%output%itrstc = -1 

    endif

   write(ch_imode,'(I3.3)') imode
   
   if (.false.) then
     ! doe dit nog even niet. Voorlopig willen we alleen bij mode 0 uitvoer schrijven!
     tmpchar = d3d_state%pseudo%trifil
     call noextspaces(tmpchar, ic)
     d3d_state%pseudo%trifil(1:ic) = tmpchar(1:ic)
     d3d_state%pseudo%trifil(ic+1:ic+3) = ch_imode(1:3)

     print *,' adjusted trifil of state ',imode, 'to ',d3d_state%pseudo%trifil(1:ic+3),'!'
     
   endif
   
   ! Now this new information has to be transported back to d3d!
   call set_d3d_state_to_d3d(imode)
   
   
   
   end subroutine m_d3d_adjust_output_state

!----------------------------------------------------------

subroutine m_d3d_dump_d3d_state(d3d_state,lun,fname)
    use precision     ! pntrsize, used in fsm.i

    implicit none


    !include 'fsm.i'
    !include 'tri-dyn.igd'
 
    type(t_d3d_state) :: d3d_state  
    integer :: lun
   
    character(len=*) :: fname




      open(unit = lun, file = fname)
      write(lun,*) 'u = '
      write(lun,*) d3d_state%core%u
      write(lun,*) 'v = '
      write(lun,*) d3d_state%core%v
      write(lun,*) 'sep = '
      write(lun,*) d3d_state%core%sep
      write(lun,*) 'r1 = '
      write(lun,*) d3d_state%core%r1      
      write(lun,*) 'rtur1 = '      
      write(lun,*) d3d_state%core%rtur
      write(lun,*) 'dp = '      
      write(lun,*) d3d_state%core%dp

      write(lun,*) 'pseudo: timestep:='                  
      write(lun,*) d3d_state%pseudo%timestep
      write(lun,*) 'pseudo: w:='                  
      write(lun,*) d3d_state%pseudo%w      
      write(lun,*) 'pseudo: vicuv:='                  
      write(lun,*) d3d_state%pseudo%vicuv 
      write(lun,*) 'pseudo: vicww:='                  
      write(lun,*) d3d_state%pseudo%vicww  
      write(lun,*) 'pseudo: rint:='                  
      write(lun,*) d3d_state%pseudo%rint 
      write(lun,*) 'pseudo: disch:='                  
      write(lun,*) d3d_state%pseudo%disch
      write(lun,*) 'pseudo: kfs:='                  
      write(lun,*) d3d_state%pseudo%kfs
      
      write(lun,*) 'output:= '      
      write(lun,*) d3d_state%output%itcomc,  d3d_state%output%ithisc, d3d_state%output%itmapc           
     close(lun)
  
  end subroutine m_d3d_dump_d3d_state
  
 
  
  !----------------------------------------------------------
    
  subroutine allocate_d3d_state(d3d_one_state, nlb  ,nub   ,mlb   ,mub   ,kmax, &
  &                             ltur,      lstsci,lstsc ,nsrc  ,nto   ,kcd  )
    
  implicit none

  type(t_d3d_state) :: d3d_one_state  
  
  integer :: nlb,nub,mlb,mub
  integer :: kmax
  integer :: ltur
  integer :: lstsci, lstsc
  integer :: nsrc, nto, kcd
  
  integer :: ierr
          
  allocate (d3d_one_state%core%u (nlb:nub, mlb:mub, kmax), stat=ierr)
  if (ierr .ne. 0) then
    print *,'error allocating u1'
    stop
  endif
  allocate (d3d_one_state%core%v (nlb:nub, mlb:mub, kmax), stat = ierr)   
  allocate (d3d_one_state%core%dp (nlb:nub, mlb:mub) )  
  allocate (d3d_one_state%core%sep (nlb:nub, mlb:mub))
  allocate (d3d_one_state%core%umnldf (nlb:nub, mlb:mub))
  allocate (d3d_one_state%core%vmnldf (nlb:nub, mlb:mub))
  allocate (d3d_one_state%core%rtur (nlb:nub, mlb:mub, 0:kmax, ltur))
  allocate (d3d_one_state%core%r1 (nlb:nub, mlb:mub, kmax,lstsci), stat = ierr) 

  size_d3d_state%sep = (nub-nlb+1)*(mub-mlb+1)
  size_d3d_state%u = size_d3d_state%sep * kmax
  size_d3d_state%rtur = size_d3d_state%sep * (kmax+1)*ltur
  size_d3d_state%r1  = size_d3d_state%sep * kmax *lstsci
  size_d3d_state%core = 4 * size_d3d_state%sep  + 2 * size_d3d_state%u + &
                          size_d3d_state%rtur + size_d3d_state%r1

  allocate (d3d_one_state%pseudo%w (nlb:nub, mlb:mub, 0:kmax), stat = ierr)   
  if (ierr .ne. 0) then
    print *,'error allocating w'
    stop
  endif

  allocate (d3d_one_state%pseudo%vicuv (nlb:nub, mlb:mub, 1:kmax+2), stat = ierr)
  allocate (d3d_one_state%pseudo%vicww (nlb:nub, mlb:mub, 0:kmax), stat = ierr) 
  
  allocate (d3d_one_state%pseudo%kfs(nlb:nub, mlb:mub), stat = ierr)
  allocate (d3d_one_state%pseudo%kfu(nlb:nub, mlb:mub), stat = ierr)
  allocate (d3d_one_state%pseudo%kfv(nlb:nub, mlb:mub), stat = ierr)

  allocate (d3d_one_state%pseudo%rint (lstsc, nsrc), stat = ierr)   
  allocate (d3d_one_state%pseudo%hydrbc (4, nto, kcd), stat = ierr) 
  allocate (d3d_one_state%pseudo%procbc (4, nto, kmax, lstsci), stat = ierr)
  allocate (d3d_one_state%pseudo%disch (nsrc), stat = ierr) 

  size_d3d_state%kfs = (nub-nlb+1)*(mub-mlb+1)  
  size_d3d_state%w = size_d3d_state%kfs * (kmax+1)  
  size_d3d_state%vicuv = size_d3d_state%kfs * (kmax+2)     
  size_d3d_state%rint = lstsc * nsrc
  size_d3d_state%hydrbc = 4 * nto * kcd
  size_d3d_state%procbc = 4 * nto * kmax * lstsci
  size_d3d_state%disch = nsrc
  size_d3d_state%pseudo = 3 * size_d3d_state%kfs + 2* size_d3d_state%w + &
          size_d3d_state%vicuv +  size_d3d_state%rint +  size_d3d_state%hydrbc + &
  size_d3d_state%procbc +  size_d3d_state%disch + 9 + 3   !9 integers and 3 strings                            
    
  size_d3d_state%output = 11
  size_d3d_state%d3d_state = size_d3d_state%core + size_d3d_state%pseudo + size_d3d_state%output  
    
  end subroutine allocate_d3d_state

!----------------------------------------------------------
subroutine deallocate_d3d_state(d3d_one_state)
    
implicit none

  type(t_d3d_state) :: d3d_one_state
integer :: ierr
   deallocate(d3d_one_state%core%u, stat = ierr)
     if (ierr .ne. 0) then
      print *,'error de-allocating '
      stop
    endif
   deallocate(d3d_one_state%core%v, stat = ierr)   

  deallocate (d3d_one_state%core%dp, stat=ierr )  
  deallocate (d3d_one_state%core%sep, stat=ierr )
  deallocate (d3d_one_state%core%umnldf, stat=ierr )
  deallocate (d3d_one_state%core%vmnldf, stat=ierr)
  deallocate (d3d_one_state%core%rtur, stat=ierr )
  deallocate (d3d_one_state%core%r1, stat=ierr) 
  deallocate (d3d_one_state%pseudo%w, stat=ierr) 

  deallocate (d3d_one_state%pseudo%vicuv, stat=ierr)
  deallocate (d3d_one_state%pseudo%vicww, stat=ierr) 
  deallocate (d3d_one_state%pseudo%kfs, stat=ierr)
  deallocate (d3d_one_state%pseudo%kfu, stat=ierr)
  deallocate (d3d_one_state%pseudo%kfv, stat=ierr)


  deallocate (d3d_one_state%pseudo%rint, stat=ierr) 
  deallocate (d3d_one_state%pseudo%hydrbc, stat=ierr) 
  deallocate (d3d_one_state%pseudo%procbc, stat=ierr) 
  deallocate (d3d_one_state%pseudo%disch, stat=ierr)   
 
end subroutine deallocate_d3d_state

!----------------------------------------------------------

end module m_d3d_state
