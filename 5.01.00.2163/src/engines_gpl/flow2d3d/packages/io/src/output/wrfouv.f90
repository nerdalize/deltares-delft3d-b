subroutine wrfouv(nmax      ,mmax      ,nmaxus    ,kmax      ,nofou     , &
                & ifou      ,lunfou    ,dtsec     ,kcs       ,xz        , &
                & yz        ,alfas     ,xcor      ,ycor      ,kfu       , &
                & kfv       ,itdate    ,gdp       )
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
!  $Id: wrfouv.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrfouv.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - writes results of fourier analysis to output
!                file LUNFOU for vectorial quantities
!              - determines elliptic parameters and writes to
!                file LUNFOU (if requested)
!              - all input array values for IFOU are equal to
!                input array values for IFOU+1 (see RDFOUR),
!                except for FOUSMA and FOUSMB, who contain the
!                fourier analysis values
!
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
    integer        , dimension(:)        , pointer :: flayno
    integer        , dimension(:)        , pointer :: fnumcy
    integer        , dimension(:)        , pointer :: ftmsto
    integer        , dimension(:)        , pointer :: ftmstr
    integer                              , pointer :: ibluv
    integer                              , pointer :: iblqf
    integer                              , pointer :: iblbs
    integer                              , pointer :: iblep
    real(fp)       , dimension(:)        , pointer :: fknfac
    real(fp)       , dimension(:)        , pointer :: foufas
    real(fp)       , dimension(:,:,:)    , pointer :: fousma
    real(fp)       , dimension(:,:,:)    , pointer :: fousmb
    real(fp)       , dimension(:,:,:)    , pointer :: fouvec
    real(fp)       , dimension(:)        , pointer :: fv0pu
    character(1)   , dimension(:)        , pointer :: fouelp
    character(16)  , dimension(:)        , pointer :: founam
    real(fp)                             , pointer :: tzone
    real(fp)                             , pointer :: hdt
!
! Global variables
!
    integer                                                                    , intent(in) :: ifou   !!  Fourier counter
    integer                                                                                 :: itdate !  Reference time in YYYYMMDD
    integer                                                                    , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: lunfou !!  Unit number fourier output file
    integer                                                                    , intent(in) :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in) :: nofou  !  Description and declaration in dimens.igs
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                                   , intent(in) :: dtsec  !!  Integration time step [in seconds]
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in) :: yz     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                 :: m           ! Loop counter over MMAX 
    integer                 :: md
    integer                 :: n           ! Loop counter over NMAXUS 
    integer                 :: ncol        ! Number of column to write to TEKAL data file 
    integer                 :: nd
    logical                 :: ltest       ! Help variable for atan2 function test 
    real(fp)                :: a1          ! Used in the computation of el. par. 
    real(fp)                :: a2          ! Used in the computation of el. par. 
    real(fp)                :: alfa        ! Value of ALFAS(N,M) in radials 
    real(fp)                :: amp         ! Fourier amplitude 
    real(fp)                :: b1          ! Used in the computation of el. par. 
    real(fp)                :: b2          ! Used in the computation of el. par. 
    real(fp)                :: defaul      ! Default value 
    real(fp)                :: elam        ! Eleptic parameter (amplitude) 
    real(fp)                :: elex        ! Eleptic parameter (eccentricity) 
    real(fp)                :: elfi        ! Eleptic parameter (phase) 
    real(fp)                :: elps        ! Eleptic parameter (inclination) 
    real(fp)                :: fas         ! Fourier phase 
    real(fp)                :: freqnt      ! Frequency in degrees per hour 
    real(fp)                :: fuzeta      ! Help variable for fourier value in zeta point in U direction 
    real(fp)                :: fvzeta      ! Help variable for fourier value in zeta point in V direction 
    real(fp)                :: r1          ! Used in the computation of el. par. 
    real(fp)                :: r2          ! Used in the computation of el. par. 
    real(fp)                :: shift       ! Phase shift 
    real(fp)                :: t1          ! Used in the computation of el. par. 
    real(fp)                :: t2          ! Used in the computation of el. par. 
    real(fp)                :: tfasto      ! Stop time in minutes 
    real(fp)                :: tfastr      ! Start time in minutes 
    character(20)           :: namfun      ! Local name for fourier function 
    character(4)            :: blnm
!
!
!! executable statements -------------------------------------------------------
!
!
    flayno      => gdp%gdfourier%flayno
    fnumcy      => gdp%gdfourier%fnumcy
    ftmsto      => gdp%gdfourier%ftmsto
    ftmstr      => gdp%gdfourier%ftmstr
    ibluv       => gdp%gdfourier%ibluv
    iblqf       => gdp%gdfourier%iblqf
    iblbs       => gdp%gdfourier%iblbs
    iblep       => gdp%gdfourier%iblep
    fknfac      => gdp%gdfourier%fknfac
    foufas      => gdp%gdfourier%foufas
    fousma      => gdp%gdfourier%fousma
    fousmb      => gdp%gdfourier%fousmb
    fouvec      => gdp%gdfourier%fouvec
    fv0pu       => gdp%gdfourier%fv0pu
    fouelp      => gdp%gdfourier%fouelp
    founam      => gdp%gdfourier%founam
    tzone       => gdp%gdexttim%tzone
    hdt         => gdp%gdnumeco%hdt
    !
    !-----Initialize local variables
    !
    defaul = 999.999_fp
    !
    !-----Frequention := 360 degree / period
    !     where period = [ (FTMSTO - FMTSTR) * DTSEC ] / [ FNUMCY * 3600 ]
    !     FOUFAS is defined in RDFOUR as
    !     FOUFAS =  2 * PI * FNUMCY / [(FTMSTO - FMTSTR) ]
    !     so FREQNT = FOUFAS * RADDEG * 3600 / DTSEC is OK
    !
    shift = ftmstr(ifou)*foufas(ifou)
    freqnt = foufas(ifou)*raddeg*3600.0_fp/dtsec
    tfastr = real(ftmstr(ifou),fp)*dtsec/60.0_fp
    tfasto = real(ftmsto(ifou),fp)*dtsec/60.0_fp
    !
    namfun = founam(ifou)
    if (founam(ifou)(:2)=='u1') then
       ibluv = ibluv + 1
       blnm = 'UV??'
       write (blnm(3:4), '(i2.2)') ibluv
       namfun = 'velocity'
    endif
    if (founam(ifou)(:2)=='qx') then
       iblqf = iblqf + 1
       blnm = 'QF??'
       write (blnm(3:4), '(i2.2)') iblqf
       namfun = 'unit discharge'
    endif
    if (founam(ifou)(:2)=='ta') then
       iblbs = iblbs + 1
       blnm = 'BS??'
       write (blnm(3:4), '(i2.2)') iblbs
       namfun = 'bed stress'
    endif
    !
    !-----Write information to "TEKAL" data file
    !
    write (lunfou, '(a,a16  )') '* Results fourier analysis on: ', namfun
    !
    if (kmax>1) then
       write (lunfou, '(a,i3)') '* Layer number               : ', flayno(ifou)
    endif
    !
    write (lunfou, '(a,i0   )') '* Reference date in YYYYMMDD : ', itdate
    write (lunfou, '(a,f12.3)') '* Starttime fourier analysis : ', tfastr
    write (lunfou, '(a,f12.3)') '* Stoptime  fourier analysis : ', tfasto
    write (lunfou, '(a,i6   )') '* Number of cycles           : ', fnumcy(ifou)
    write (lunfou, '(a,f12.6)') '* Frequency [degrees/hour]   : ', freqnt
    !
    write (lunfou, '(a     )') '*'
    write (lunfou, '(a     )') '* Block definition:'
    !
    !-----For GPP: description in columns 17 to 37
    !
    write (lunfou, '(a     )') '* column    1 : X-coor, zeta point'
    write (lunfou, '(a     )') '* column    2 : Y-coor, zeta point'
    write (lunfou, '(a     )') '* column    3 : X-coor, depth point'
    write (lunfou, '(a     )') '* column    4 : Y-coor, depth point'
    write (lunfou, '(a     )') '* column    5 : M-index '
    write (lunfou, '(a     )') '* column    6 : N-index '
    if (fouelp(ifou)=='x') then
       ncol = 12
       write (lunfou, '(a,a16 )') '* column    7 : Max ', founam(ifou)
       write (lunfou, '(a,a16 )') '* column    8 : Max ', founam(ifou + 1)
       write (lunfou, '(a     )') '* column    9 : Maximum magnitude'
       write (lunfou, '(a     )') '* column   10 : KCS'
       write (lunfou, '(a     )') '* column   11 : KFU'
       write (lunfou, '(a     )') '* column   12 : KFV'
    elseif (fouelp(ifou)=='i') then
       ncol = 12
       write (lunfou, '(a,a16 )') '* column    7 : Min ', founam(ifou)
       write (lunfou, '(a,a16 )') '* column    8 : Min ', founam(ifou + 1)
       write (lunfou, '(a     )') '* column    9 : Minimum magnitude'
       write (lunfou, '(a     )') '* column   10 : KCS'
       write (lunfou, '(a     )') '* column   11 : KFU'
       write (lunfou, '(a     )') '* column   12 : KFV'
    else
       ncol = 13
       write (lunfou, '(a,a16 )') '* column    7 : Fou amp ', founam(ifou)
       write (lunfou, '(a,a16 )') '* column    8 : Fou phs ', founam(ifou)
       write (lunfou, '(a,a16 )') '* column    9 : Fou amp ', founam(ifou + 1)
       write (lunfou, '(a,a16 )') '* column   10 : Fou phs ', founam(ifou + 1)
       write (lunfou, '(a     )') '* column   11 : KCS'
       write (lunfou, '(a     )') '* column   12 : KFU'
       write (lunfou, '(a     )') '* column   13 : KFV'
    endif
    !
    !-----Write Block code and data to "TEKAL" data file
    !     Frequency is shown in GPP (20 characters total)
    !
    write (lunfou, '(a4,a5,f11.6)') blnm, ' freq', freqnt
    write (lunfou, '(4i8)') mmax*nmaxus, ncol, mmax, nmaxus
    !
    !-----Write data for user defined dimensions, hence NMAXUS and MMAX
    !
    if (fouelp(ifou)=='x' .or. fouelp(ifou)=='i') then
       do n = 1, nmaxus
          do m = 1, mmax
             !
             !-----------Test for active point
             !           when KCS (N,M) = 1 N > 1 and M > 1 per definition
             !
             if (kcs(n, m)==1) then
                write (lunfou,'(4(f12.3,1x),2(i5,1x),3(e14.6E3,1x),3(i2,1x))')    &
                    & xz(n, m), yz(n, m), xcor(n, m), ycor(n, m), m, n,           &
                    & fousma(n, m, ifou), fousma(n, m, ifou + 1), fouvec(n, m, ifou), kcs(n, m), &
                    & kfu(n, m), kfv(n, m)
             else
                fousma(n, m, ifou) = defaul
                fousma(n, m, ifou + 1) = defaul
                !
                !-------------Write to file
                !             defaul instead of xz/yz needed for GPP
                !             '0' instead of kcs, because TEKAL does not accept '2'
                !
                write (lunfou,'(4(f12.3,1x),2(i5,1x),3(f14.3,1x),3(i2,1x))')  &
                   & defaul, defaul, xcor(n, m), ycor(n, m), m, n,            &
                   & fousma(n, m, ifou), fousma(n, m, ifou + 1), defaul, 0,   &
                   & kfu(n, m), kfv(n, m)
             endif
          enddo
       enddo
    else
       do n = 1, nmaxus
          do m = 1, mmax
             !
             !-----------Test for active point
             !           when KCS (N,M) = 1 N > 1 and M > 1 per definition
             !
             if (kcs(n, m)==1) then
                !
                !--------------Test FOUSMA and FOUSMB values (<> 0.) for IFOU
                !
                ltest = (fousma(n, m, ifou)==0.0_fp .and. fousmb(n, m, ifou)==0.0_fp)
                if (.not.ltest) then
                   fousma(n, m, ifou) = fousma(n, m, ifou)                      &
                                      & *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                   fousmb(n, m, ifou) = fousmb(n, m, ifou)                      &
                                      & *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                   amp = sqrt(fousma(n, m, ifou)*fousma(n, m, ifou)             &
                       & + fousmb(n, m, ifou)*fousmb(n, m, ifou))
                   fas = atan2(fousmb(n, m, ifou), fousma(n, m, ifou)) + shift
                   if (fnumcy(ifou)==0) then
                      amp = 0.5_fp*amp*cos(fas)
                      fas = 0.0_fp
                   endif
                   !
                   ! Timezone correction added timezone*phase [degrees/hr].
                   ! foufas       is in [rad/timestep]
                   ! halftimestep is in [sec/timestep]
                   ! => timezonecorr = tzone [-] * foufas [rad/timestep] * raddeg [deg/rad] * [sec/hr] / (2 * halftimestep [sec/timestep])
                   !
                   fas = fas*raddeg + fv0pu(ifou) - tzone*foufas(ifou)*raddeg*1800.0_fp/hdt
                   !
                   ! To define FAS between 0 and 360. add 720. to the mod
                   ! function of FAS and re-use the mod function
                   !
                   fas = mod(mod(fas, 360.0_fp) + 720.0_fp, 360.0_fp)
                   amp = amp/fknfac(ifou)
                   fousma(n, m, ifou) = amp
                   fousmb(n, m, ifou) = fas
                else
                   fousma(n, m, ifou) = 0.0_fp
                   fousmb(n, m, ifou) = 0.0_fp
                endif
                !
                !--------------Test FOUSMA and FOUSMB values (<> 0.) for IFOU + 1
                !
                ltest = (fousma(n, m, ifou + 1)==0.0_fp .and.                      &
                      & fousmb(n, m, ifou + 1)==0.0_fp)
                if (.not.ltest) then
                   fousma(n, m, ifou + 1) &
                       & = fousma(n, m, ifou + 1) *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                   fousmb(n, m, ifou + 1) &
                       & = fousmb(n, m, ifou + 1) *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                   amp = sqrt(fousma(n, m, ifou + 1)*fousma(n, m, ifou + 1)     &
                       & + fousmb(n, m, ifou + 1)*fousmb(n, m, ifou + 1))
                   fas = atan2(fousmb(n, m, ifou + 1), fousma(n, m, ifou + 1))  &
                       & + shift
                   if (fnumcy(ifou)==0) then
                      amp = 0.5_fp*amp*cos(fas)
                      fas = 0.0_fp
                   endif
                   !
                   ! Timezone correction added timezone*phase [degrees/hr].
                   ! foufas       is in [rad/timestep]
                   ! halftimestep is in [sec/timestep]
                   ! => timezonecorr = tzone [-] * foufas [rad/timestep] * raddeg [deg/rad] * [sec/hr] / (2 * halftimestep [sec/timestep])
                   !
                   fas = fas*raddeg + fv0pu(ifou) - tzone*foufas(ifou)*raddeg*1800.0_fp/hdt
                   !
                   ! To define FAS between 0 and 360. add 720. to the mod
                   ! function of FAS and re-use the mod function
                   !
                   fas = mod(mod(fas, 360.0_fp) + 720.0_fp, 360.0_fp)
                   amp = amp/fknfac(ifou)
                   fousma(n, m, ifou + 1) = amp
                   fousmb(n, m, ifou + 1) = fas
                else
                   fousma(n, m, ifou + 1) = 0.0_fp
                   fousmb(n, m, ifou + 1) = 0.0_fp
                endif
                write (lunfou,'(4(f12.3,1x),2(i5,1x),4(e14.6E3,1x),3(i2,1x))')  &
                   & xz(n, m), yz(n, m), xcor(n, m), ycor(n, m), m, n,          &
                   & fousma(n, m, ifou), fousmb(n, m, ifou),                    &
                   & fousma(n, m, ifou + 1), fousmb(n, m, ifou + 1), kcs(n, m), &
                   & kfu(n, m), kfv(n, m)
             else
                fousma(n, m, ifou) = defaul
                fousmb(n, m, ifou) = defaul
                fousma(n, m, ifou + 1) = defaul
                fousmb(n, m, ifou + 1) = defaul
                !
                !--------------Write to file
                !              defaul instead of xz/yz needed for GPP
                !              '0' instead of kcs, because TEKAL does not accept '2'
                !
                write (lunfou,'(4(f12.3,1x),2(i5,1x),4(f14.3,1x),3(i2,1x))')    &
                   & defaul, defaul, xcor(n, m), ycor(n, m), m, n,              &
                   & fousma(n, m, ifou), fousmb(n, m, ifou),                    &
                   & fousma(n, m, ifou + 1), fousmb(n, m, ifou + 1), 0,         &
                   & kfu(n, m), kfv(n, m)
             endif
          enddo
       enddo
    endif
    !
    !-----Write elliptic parameters to file (if requested)
    !
    if (fouelp(ifou)=='y') then
       iblep = iblep + 1
       blnm = 'EP??'
       write (blnm(3:4), '(i2.2)') iblep
       write (lunfou, '(a,a16)') '* Elliptic parameters of      : ', namfun
       !
       if (kmax>1) then
          write (lunfou, '(a,i3)') '* Layer number                : ', flayno(ifou)
       endif
       !
       write (lunfou, '(a,i0   )') '* Reference date in YYYYMMDD  : ', itdate
       write (lunfou, '(a,f12.3)') '* Starttime fourier analysis  : ', tfastr
       write (lunfou, '(a,f12.3)') '* Stoptime  fourier analysis  : ', tfasto
       write (lunfou, '(a,i6   )') '* Number of cycles            : ', fnumcy(ifou)
       write (lunfou, '(a,f12.6)') '* Frequency [degrees/hour]    : ', freqnt
       !
       write (lunfou, '(a     )') '* Block definition:'
       write (lunfou, '(a     )') '* column    1 : X-coor, zeta point'
       write (lunfou, '(a     )') '* column    2 : Y-coor, zeta point'
       write (lunfou, '(a     )') '* column    3 : X-coor, depth point'
       write (lunfou, '(a     )') '* column    4 : Y-coor, depth point'
       write (lunfou, '(a     )') '* column    5 : M-index '
       write (lunfou, '(a     )') '* column    6 : N-index '
       write (lunfou, '(a     )') '* column    7 : Amplitude'
       write (lunfou, '(a     )') '* column    8 : Eccentricity'
       write (lunfou, '(a     )') '* column    9 : Phase'
       write (lunfou, '(a     )') '* column   10 : Inclination'
       write (lunfou, '(a     )') '* column   11 : KCS'
       write (lunfou, '(a     )') '* column   12 : KFU'
       write (lunfou, '(a     )') '* column   13 : KFV'
       write (lunfou, '(a4,a5,f11.6)') blnm, ' freq', freqnt
       write (lunfou, '(4i8)') mmax*nmaxus, 13, mmax, nmaxus
       !
       do n = 1, nmaxus
          do m = 1, mmax
             !
             !------------Test for active point and for FOUSMA values (<> defaul)
             !            for IFOU and IFOU + 1
             !            when KCS (N,M) = 1 N > 1 and M > 1 per definition
             !
             ltest = (fousma(n, m, ifou)==defaul .and. fousma(n, m, ifou + 1)   &
                   & ==defaul)
             if (kcs(n, m)==1 .and. .not.ltest) then
                !
                !--------------Define ellips parameters
                !
                a1 = fousma(n, m, ifou)*cos(fousmb(n, m, ifou)/raddeg)
                a2 = fousma(n, m, ifou + 1)*cos(fousmb(n, m, ifou + 1)/raddeg)
                b1 = fousma(n, m, ifou)*sin(fousmb(n, m, ifou)/raddeg)
                b2 = fousma(n, m, ifou + 1)*sin(fousmb(n, m, ifou + 1)/raddeg)
                !
                r1 = 0.5_fp*sqrt((a1 + b2)*(a1 + b2) + (a2 - b1)*(a2 - b1))
                r2 = 0.5_fp*sqrt((a1 - b2)*(a1 - b2) + (a2 + b1)*(a2 + b1))
                !
                !--------------Test ATAN2 input values
                !
                if ((a2 - b1)==0.0_fp .and. (a1 + b2)==0.0_fp) then
                   t1 = 0.0_fp
                else
                   t1 = atan2((a2 - b1), (a1 + b2))
                endif
                !
                if ((a2 + b1)==0.0_fp .and. (a1 - b2)==0.0_fp) then
                   t2 = 0.0_fp
                else
                   t2 = atan2((a2 + b1), (a1 - b2))
                endif
                !
                elam = r1 + r2
                !
                if ((r1 - r2)==0.0_fp .and. (r1 + r2)==0.0_fp) then
                   elex = 0.0_fp
                elseif ((r1 + r2)==0.0_fp) then
                   elex = defaul
                else
                   elex = (r1 - r2)/(r1 + r2)
                endif
                !
                elfi = 0.5_fp*(t2 - t1)
                elps = 0.5_fp*(t2 + t1)
                !
                !--------------To define ELFI and ELPS between 0 and 360. add 720.
                !              to the mod functions and re-use the mod function
                !
                elfi = mod(mod(elfi*raddeg, 360.0_fp) + 720.0_fp, 360.0_fp)
                elps = mod(mod(elps*raddeg, 360.0_fp) + 720.0_fp, 360.0_fp)
                !
                !--------------Write to file
                !
                write (lunfou,'(4(f12.3,1x),2(i5,1x),4(e14.6E3,1x),3(i2,1x))') &
                    & xz(n, m), yz(n, m), xcor(n, m), ycor(n, m), m, n, elam,  &
                    & elex, elfi, elps, kcs(n, m), kfu(n, m), kfv(n, m)
             else
                write (lunfou,'(4(f12.3,1x),2(i5,1x),4(f14.3,1x),3(i2,1x))') &
                    & defaul, defaul, xcor(n, m), ycor(n, m), m, n, defaul,  &
                    & defaul, defaul, defaul, 0, kfu(n, m), kfv(n, m)
             endif
          enddo
       enddo
    !
    endif
end subroutine wrfouv
