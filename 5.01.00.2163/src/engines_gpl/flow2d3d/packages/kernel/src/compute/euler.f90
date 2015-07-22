subroutine euler(j         ,nmmax     ,nmmaxj    ,kmax      ,icx       , &
               & kcu       ,kcv       ,kfu       ,kfv       ,kfumax    , &
               & kfumin    ,kfvmax    ,kfvmin    ,dzu1      ,dzv1      , &
               & u1        ,uwork     ,v1        ,vwork     , &
               & grmasu    ,grmasv    ,hu        ,hv        , &
               & tp        ,hrms      ,sig       ,teta      , &
               & grmsur    ,grmsvr    ,grfacu    ,grfacv    ,gdp       )
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
!  $Id: euler.f90 2162 2013-02-01 13:11:40Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/euler.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: EULER corrects the velocities with the
!              mass fluxes to work array which will be used in
!              TAUBOT and for postprocessing Delft3D-FLOW
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                 , pointer :: wave
    logical                 , pointer :: struct
    logical                 , pointer :: zmodel
    logical                 , pointer :: roller
    real(fp)                , pointer :: ag
    real(fp), dimension(:,:), pointer :: ustokes
    real(fp), dimension(:,:), pointer :: vstokes
!
! Global variables
!
    integer                                         , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                       :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                       :: nmmaxj !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grmasu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grmasv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grmsur !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grmsvr !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grfacu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: grfacv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: teta   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: uwork  !!  U-velocities corrected with mass flux
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: vwork  !!  V-velocities corrected with mass flux
    real(fp), dimension(kmax)                       , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: icy
    integer       :: k
    integer       :: kk
    integer       :: nm
    integer       :: nmu
    integer       :: num
    real(fp)      :: amp
    real(fp)      :: costu
    real(fp)      :: f1
    real(fp)      :: f2
    real(fp)      :: f3
    real(fp)      :: h
    real(fp)      :: kwav
    real(fp)      :: omega
    real(fp)      :: p1
    real(fp)      :: p2
    real(fp)      :: sintv
    real(fp)      :: tpu
    real(fp)      :: z
    integer       :: nm_pos ! indicating the nm index (u1(nm,k): nm_pos=1, dbodsd(lsedtot,nm): nm_pos=2)
!
!! executable statements -------------------------------------------------------
!
    ag         => gdp%gdphysco%ag
    wave       => gdp%gdprocs%wave
    struct     => gdp%gdprocs%struct
    zmodel     => gdp%gdprocs%zmodel
    roller     => gdp%gdprocs%roller
    ustokes    => gdp%gdtrisol%ustokes
    vstokes    => gdp%gdtrisol%vstokes
    !
    ! Correct Velocities with mass flux
    ! Added vertical structure of mass flux according to 2nd Order Stokes
    !
    ! in 3D roller part of mass flux was not included
    ! grmasu/v now contain complete mass flux in 2DH or
    ! roller contribution in 3D (this is done in massfl-routine)
    ! Roller part of mass flux is assumed to be distributed uniformly
    ! over the water column
    ! Furthermore correction for breaker delay
    ! in 3D grmasu/grmasv contains breaker delay scaling factors
    ! in 2DH grmsur/grmsvr contains breaker delay scaling factors
    !
    nm_pos = 1
    kwav = 0.0_fp
    p1   = 0.0_fp
    p2   = 0.0_fp
    if (wave) then
       if (kmax > 1) then
          !
          ! 3D case
          !
          icy = 1
          do nm = 1, nmmax
             !
             ! U-velocity point
             !
             if (kfu(nm)==1 .and. kcu(nm)>-1 .and. kcu(nm)<=2) then
                nmu = nm + icx
                tpu = (tp(nm) + tp(nmu))/2.0_fp
                if (tpu > 0.1_fp) then
                   costu = 0.5_fp*(cos(degrad*teta(nm)) + cos(degrad*teta(nmu)))
                   amp   = (hrms(nm) + hrms(nmu))/4.0_fp
                   omega = 2.0_fp*pi/tpu
                   h = hu(nm)
                   !
                   ! Determine Wave number
                   !
                   call wavenr(h         ,tpu       ,kwav      ,ag        )
                   !
                   ! Determine Second order Stokes drift
                   !
                   f1 = omega*kwav*amp**2
                   f3 = (1.0_fp - exp(-2.0_fp*kwav*h))**2
                   !
                   ! Exponents p1 and p2 used in computation of Stokes velocity limited 
                   ! to avoid underflow for very small wave influence
                   !
                   if (.not. zmodel) then
                      do k = 1, kmax
                         z             = (1.0_fp+sig(k)) * h
                         p1            = max(-25.0_fp,  2.0_fp*kwav*(z-h))
                         p2            = max(-25.0_fp, -4.0_fp*kwav*z)
                         f2            = exp(p1) * (1.0_fp+exp(p2))
                         ustokes(nm,k) = f1 * (f2/f3) * costu
                         uwork  (nm,k) = u1(nm,k) - ustokes(nm,k)          &
                                       & - (grmsur(nm) + grfacu(nm))/hu(nm)
                      enddo
                   else
                      z = 0.0_fp
                      do k = kfumin(nm), kfumax(nm)
                         z             = z + dzu1(nm, k)
                         p1            = max(-25.0_fp,  2.0_fp*kwav*(z-h))
                         p2            = max(-25.0_fp, -4.0_fp*kwav*z)
                         f2            = exp(p1) * (1.0_fp+exp(p2))
                         ustokes(nm,k) = f1 * (f2/f3) * costu
                         uwork  (nm,k) = u1(nm,k) - ustokes(nm,k)           &
                                       & - (grmsur(nm) + grfacu(nm))/hu(nm)
                      enddo
                   endif
                else
                   !
                   ! tp <= 0.1: ustokes=0
                   !
                   do k = 1, kmax
                      uwork(nm, k) = u1(nm, k)
                   enddo
                endif
             else
                !
                ! kfu = 0 or kcu >= 3
                !
                do k = 1, kmax
                   uwork(nm, k) = u1(nm, k)
                enddo
             endif
             !
             !   V- velocity point
             !
             if (kfv(nm)==1 .and. kcv(nm)>-1 .and. kcv(nm)<=2) then
                num = nm + icy
                tpu = (tp(nm) + tp(num))/2.0_fp
                if (tpu > 0.1_fp) then
                   sintv = 0.5_fp*(sin(degrad*teta(nm)) + sin(degrad*teta(num)))
                   amp   = (hrms(nm) + hrms(num))/4.0_fp
                   omega = 2.0_fp*pi/tpu
                   h = hv(nm)
                   !
                   ! Determine Wave number
                   !
                   call wavenr(h         ,tpu       ,kwav      ,ag        )
                   !
                   ! Determine Second order Stokes drift
                   !
                   f1 = omega*kwav*amp**2
                   f3 = (1.0_fp - exp(-2.0_fp*kwav*h))**2
                   !
                   ! Exponents p1 and p2 used in computation of Stokes velocity limited 
                   ! to avoid underflow for very small wave influence
                   !
                   if (.not. zmodel) then
                      do k = 1, kmax
                         z             = (1.0_fp+sig(k)) * h
                         p1            = max(-25.0_fp,  2.0_fp*kwav*(z - h))
                         p2            = max(-25.0_fp, -4.0_fp*kwav*z)
                         f2            = exp(p1) * (1.0_fp+exp(p2))
                         vstokes(nm,k) = f1 * (f2/f3) * sintv
                         vwork  (nm,k) = v1(nm,k) - vstokes(nm,k)          &
                                       & - (grmsvr(nm) + grfacv(nm))/hv(nm) 
                      enddo
                   else
                      z = 0.0_fp
                      do k = kfvmin(nm), kfvmax(nm)
                         z             = z + dzv1(nm, k)
                         p1            = max(-25.0_fp,  2.0_fp*kwav*(z - h))
                         p2            = max(-25.0_fp, -4.0_fp*kwav*z)
                         f2            = exp(p1) * (1.0_fp+exp(p2))
                         vstokes(nm,k) = f1 * (f2/f3) * sintv
                         vwork  (nm,k) = v1(nm,k) - vstokes(nm,k)          &
                                       & - (grmsvr(nm) + grfacv(nm))/hv(nm) 
                      enddo
                   endif
                else
                   !
                   ! tp <= 0.1: ustokes=0
                   !
                   do k = 1, kmax
                      vwork(nm, k) = v1(nm, k)
                   enddo
                endif
             else
                !
                ! kfv = 0 or kcu >= 3
                !
                do k = 1, kmax
                   vwork(nm, k) = v1(nm, k)
                enddo
             endif
          enddo ! nm loop
          !
       else
          !
          ! 2D case: kmax=1
          !
          do nm = 1, nmmax
             if (kfu(nm)==1 .and. kcu(nm)>-1 .and. kcu(nm)<=2) then
                uwork(nm, 1) = u1(nm, 1) - (grmasu(nm)+grfacu(nm))/hu(nm) 
             else
                uwork(nm, 1) = u1(nm, 1)
             endif
             if (kfv(nm)==1 .and. kcv(nm)>-1 .and. kcv(nm)<=2) then
                vwork(nm, 1) = v1(nm, 1) - (grmasv(nm)+grfacv(nm))/hv(nm) 
             else
                vwork(nm, 1) = v1(nm, 1)
             endif
          enddo
       endif
    else
       !
       ! wave = .false.
       !
       uwork = u1
       vwork = v1
    endif
    !
    ! exchange corrected velocities with neighbours for parallel runs
    !
    call dfexchg ( uwork, 1, kmax, dfloat, nm_pos, gdp )
    call dfexchg ( vwork, 1, kmax, dfloat, nm_pos, gdp )
end subroutine euler
