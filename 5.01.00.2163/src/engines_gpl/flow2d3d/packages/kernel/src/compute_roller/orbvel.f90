subroutine orbvel(nmmax     ,kfs       ,dps       ,ubot      , &
                & s1        ,rlabda    ,tp        ,ewave1    ,uorb      , &
                & hrms      ,gdp       )
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
!  $Id: orbvel.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/orbvel.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    logical                , pointer :: ubot_from_com
!
! Global variables
!
    integer                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: ewave1 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: rlabda !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: ubot   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: hrms   !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: nm
    real(fp) :: e
    real(fp) :: h
    real(fp) :: hk
!
!! executable statements -------------------------------------------------------
!
    rhow            => gdp%gdphysco%rhow
    ag              => gdp%gdphysco%ag
    ubot_from_com   => gdp%gdprocs%ubot_from_com
    !
    do nm = 1, nmmax
       if (kfs(nm)/=0) then
          h = max(0.01_fp, s1(nm) + real(dps(nm),fp))
          e = ewave1(nm)
          if (e>1.0e-2 .and. tp(nm)>1.0e-2 .and. rlabda(nm)>1.0e-2) then
             hrms(nm) = sqrt(8.0*e/rhow/ag)
             if (ubot_from_com) then
                uorb(nm) = ubot(nm)
             else
                hk = 2.0*pi/rlabda(nm)*h
                hk = min(100.0_fp, hk)
                uorb(nm) = pi*hrms(nm)/tp(nm)/sinh(hk)
             endif
          else
             uorb(nm) = 0.0
             hrms(nm) = 0.0
          endif
       else
          uorb(nm) = 0.0
          hrms(nm) = 0.0
       endif
    enddo
end subroutine orbvel
