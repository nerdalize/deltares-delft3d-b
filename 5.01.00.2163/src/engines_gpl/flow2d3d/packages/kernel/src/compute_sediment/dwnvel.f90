subroutine dwnvel(nmmax     ,kmax      ,icx       , &
                & kcs       ,kfu       ,kfv       , &
                & kcu       ,kcv       ,s1        ,dps       ,u0eul     , &
                & v0eul     ,uuu       ,vvv       ,umod      ,zumod     , &
                & sig       ,hu        ,hv        ,kfsed     ,gdp       )
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
!  $Id: dwnvel.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/dwnvel.f90 $
!!--description-----------------------------------------------------------------
! Calculate velocity components and magnitude at the
! zeta points based on velocity in the bottom
! computational layer
!
! Note: uses downwind velocity at any internal point,
! uses internal velocity at any open boundary, uses
! half of internal velocity in direction of any
! closed boundary or dry point.
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: eps
    type (mornumericstype)               , pointer :: mornum
!
! Global variables
!
    integer                                           , intent(in)  :: icx
    integer                                           , intent(in)  :: kmax  !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: nmmax !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcv   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsed !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv   !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u0eul
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v0eul
    real(fp)  , dimension(kmax)                       , intent(in)  :: sig   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: umod
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: uuu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: vvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: zumod
!
! Local variables
!
    integer  :: k
    integer  :: kmx
    integer  :: ndm
    integer  :: ndmd
    integer  :: ndmu
    integer  :: nm
    integer  :: nm_u1
    integer  :: nm_u2
    integer  :: nm_v1
    integer  :: nm_v2
    integer  :: nmd
    integer  :: nmu
    integer  :: num
    integer  :: numd
    real(fp) :: cc
    real(fp) :: h1
    real(fp) :: ufac
    real(fp) :: uu
    real(fp) :: vfac
    real(fp) :: vv
!
!! executable statements -------------------------------------------------------
!
    eps                 => gdp%gdconst%eps
    mornum              => gdp%gdmorpar%mornum
    !
    do nm = 1, nmmax
       !
       if (kfsed(nm) == 0) then
          uuu  (nm) = 0.0
          vvv  (nm) = 0.0
          umod (nm) = 0.0
          zumod(nm) = 0.0
          cycle
       endif
       !
       nmd  = nm  - icx
       numd = nmd + 1
       ndmd = nmd - 1
       num  = nm  + 1
       ndm  = nm  - 1
       nmu  = nm  + icx
       ndmu = nmu - 1
       !
       uu = 0.0
       vv = 0.0
       h1 = s1(nm) + real(dps(nm),fp)
       !
       do k = kmax, 1, -1
          cc  = (1.0 + sig(k))*h1
          kmx = k
          if (cc>=0.05*h1 .or. cc>=0.05) then
             exit
          endif         
       enddo
       !
       ufac = 0.5_fp
       vfac = 0.5_fp
       if (kcs(nm) == 1) then
          !
          ! Internal point
          ! Set velocity in U direction.
          !
          nm_u1 = nm
          nm_u2 = nmd
          !
          ! Set velocity in V direction.
          !
          nm_v1 = nm
          nm_v2 = ndm
       elseif (kcu(nm) + kcu(nmd) == 1) then
          !
          ! Open boundary (kcs(nm)==2) in v-direction
          !
          ! Set velocity in U direction.
          !
          nm_u1 = nm
          nm_u2 = nmd
          ufac  = 1.0_fp
          !
          ! Set velocity in V direction.
          !
          if (kcu(nm) == 1) then
             !
             ! Open boundary at left-hand side
             !
             nm_v1 = nmu
             nm_v2 = ndmu
          else
             !
             ! Open boundary at right-hand side
             !
             nm_v1 = nmd
             nm_v2 = ndmd
          endif
       else
          !
          ! Open boundary (kcs(nm)==2) in u-direction
          !
          ! Set velocity in U direction.
          !
          if (kcv(nm) == 1) then
             !
             ! Open boundary at lower side
             !
             nm_u1 = num
             nm_u2 = numd
          else
             !
             ! Open boundary at upper side
             !
             nm_u1 = ndm
             nm_u2 = ndmd
          endif
          !
          ! Set velocity in V direction.
          !
          nm_v1 = nm
          nm_v2 = ndm
          vfac  = 1.0_fp
       endif
       !
       uu = ufac * (  kfu(nm_u1)*u0eul(nm_u1, kmx)*hu(nm_u1) &
          &         + kfu(nm_u2)*u0eul(nm_u2, kmx)*hu(nm_u2)  )
       vv = vfac * (  kfv(nm_v1)*v0eul(nm_v1, kmx)*hv(nm_v1) &
          &         + kfv(nm_v2)*v0eul(nm_v2, kmx)*hv(nm_v2)  )
       !
       if (mornum%maximumwaterdepth) then
          !
          ! prevent any increase in velocity due to a decrease in water depth
          !
          if (kfu(nm_u1)==1) h1 = max(h1,hu(nm_u1))
          if (kfu(nm_u2)==1) h1 = max(h1,hu(nm_u2))
          if (kfv(nm_v1)==1) h1 = max(h1,hv(nm_v1))
          if (kfv(nm_v2)==1) h1 = max(h1,hv(nm_v2))
       endif
       !
       ! h1 will not be zero, because of cycle statement when kfsed==0
       !
       uu = uu/h1
       vv = vv/h1
       !
       if (abs(uu) < eps) then
          uu = 0.0
       endif
       if (abs(vv) < eps) then
          vv = 0.0
       endif
       !
       ! Calculate resultant velocity magnitude and height
       !
       umod(nm) = (uu*uu + vv*vv)**0.5
       if (kmax > 1) then
          zumod(nm) = h1 * (1.0+sig(kmx))
       else
          !
          ! (1+h1/z0rou)**(z0rou/h1)*exp(-1)*(z0rou+h1)-z0rou
          ! equals approximately exp(-1)*h1 if h1>>z0rou
          ! exp(-1) is approx 0.368
          !
          zumod(nm) = h1*0.368
       endif
       uuu(nm) = uu
       vvv(nm) = vv
    enddo
end subroutine dwnvel
