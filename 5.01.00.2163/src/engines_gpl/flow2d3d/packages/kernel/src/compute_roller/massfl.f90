subroutine massfl(c         ,dir       ,ewave1    ,eroll1    , &
                & rmasu     ,rmasv     ,rmasur    ,rmasvr    , &
                & nmax      ,mmax      ,kmax      ,gdp       )
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
!  $Id: massfl.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/massfl.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: rhow
!
! Global variables
!
    integer                                                  , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                  , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                  , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dir
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: eroll1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ewave1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: rmasu
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: rmasv
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: rmasur
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: rmasvr
!
! Local variables
!
    integer :: m
    integer :: n
    real(fp):: c1
    real(fp):: mass
!
!! executable statements -------------------------------------------------------
!
    rhow      => gdp%gdphysco%rhow
    !
    ! 2DH: original implementation
    ! 3D : only roller part of Massfl is set in rmasu/vr
    !
    do m = 1, mmax
       do n = 1, nmax
          c1 = max(c(n, m), 0.1_fp)
          if (kmax > 1) then
             !
             ! 3D: massflux resulting from roller is stored in rmasur/rmasvr
             ! Waves massflux is obtained from stokes drift distribution
             !
             mass         =  2.0*eroll1(n, m)/rhow/c1
             rmasur(n, m) = mass*cos(dir(n, m)*degrad)
             rmasvr(n, m) = mass*sin(dir(n, m)*degrad)
          endif
          !
          ! massflux resulting from waves and roller is stored in rmasu/v
          !
          mass        = (ewave1(n, m) + 2.0*eroll1(n, m))/rhow/c1
          rmasu(n, m) = mass*cos(dir(n, m)*degrad)
          rmasv(n, m) = mass*sin(dir(n, m)*degrad)
       enddo
    enddo
end subroutine massfl
