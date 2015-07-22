subroutine trisim (numdom, nummap, context_id, fsm_flags, runid)
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
!  $Id: trisim.F90 1894 2012-10-17 08:06:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/manager/src/trisim.F90 $
!!--description-----------------------------------------------------------------
!
!    Function: Main routine for the 2d / 3d program
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use mod_trisim
    use precision
    use dfparall
    use d3d_olv_class

    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following line
    ! See also statements below
    !
    ! use ifcore
    !
    ! global data declaration; compare with include 'globdat.igd'
    !
    use globaldata
    implicit none
    type(globDat),pointer :: gdp
!
! Parameters
!
    integer       , intent(in)  :: context_id
    integer       , intent(in)  :: fsm_flags
    integer       , intent(in)  :: numdom        ! Number of subdomains (0 means single domain)
                                                 ! as detected by hydra
    integer       , intent(in)  :: nummap        ! Number of mappers (one for each DD boundaries connected with this subdomain)
                                                 ! as detected by hydra
    character(*)              :: runid
!
! Local variables
!
    integer         :: ierr
    integer         :: retval
    type(OLVHandle) :: olv_handle
    !
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following line
    ! See also statements below
    !
    ! INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
!
!! executable statements -------------------------------------------------------

    ! Initialize MPI
    ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
    ! Activate the following two lines
    ! See also use statement above
    !
    ! NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
    ! OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
    !
    ! create and initialize GDP structure
    !
    allocate(gdp)
    !
    ! gdp%runid must be nullified before calling trisim_init
    ! When trisim_init is called via OpenDA/OpenMI, gdp%runid is set before the call
    !
    nullify(gdp%runid)
    !
    retval = trisim_init(numdom, nummap, context_id, fsm_flags, runid, olv_handle, gdp)
    if (retval /= 0) then
       return
    endif
    !
    retval = trisim_step(olv_handle, gdp)
    if (retval /= 0) then
       return
    endif
    !
    retval = trisim_finish(olv_handle, gdp)
    if (retval /= 0) then
       return
    endif
    !
    call gdp_dealloc(gdp)
    deallocate(gdp, stat=ierr)
    !
    ! Finish using a semaphore
    ! Related psemnefis is in tricom.f90
    !
    call vsemfinish
    !
    ! Finalizes MPI
    !
    if (parll) then
       call dfexitmpi(0)
    endif
end subroutine trisim
