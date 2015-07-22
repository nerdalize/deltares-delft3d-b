subroutine eqtran(sig       ,thick     ,kmax      , &
                & aks       ,ustarc    ,ws        ,ltur      , &
                & frac      ,sigmol    , &
                & ce_nm     ,taurat    ,dicww     ,seddif    ,rsedeq    , &
                & kmaxsd    ,crep      ,sbcu      ,sbcv      ,sbwu      , &
                & sbwv      ,sswu      ,sswv      ,lundia    , &
                & taucr0    ,dss       ,rksrs     ,i2d3d     , &
                & ce_nmtmp  ,akstmp    ,lsecfl    ,spirint   , &
                & suspfrac  ,ust2      ,tetacr    ,gamtcr    , &
                & salmax    ,ws0       ,t_relax   ,concin    , &
                & dzduu     ,dzdvv     ,ubot      ,tauadd    , &
                & sus       ,bed       ,susw      ,bedw      ,espir     , &
                & wave      , &
                & scour     ,epspar    ,ubot_from_com,camax     , &
                & aksfac    ,rwave     ,rdc       ,rdw       ,pangle    , &
                & fpco      ,iopsus    ,iopkcw    ,subiw     ,eps       , &
                & iform     ,par       , &
                & numintpar ,numrealpar,numstrpar ,dllfunc   ,dllhandle , &
                & intpar    ,realpar   ,strpar    ,error     )
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
!  $Id: eqtran.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/eqtran.f90 $
!!--description-----------------------------------------------------------------
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
    !
    include 'trapar.inc'
!
! Global variables
!
    integer(pntrsize)                   , intent(in)    :: dllhandle
    integer                             , intent(in)    :: i2d3d
    integer                             , intent(in)    :: iform
    integer                             , intent(in)    :: iopsus
    integer                             , intent(in)    :: iopkcw
    integer                             , intent(in)    :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                             , intent(out)   :: kmaxsd
    integer                             , intent(in)    :: lsecfl   !  Description and declaration in esm_alloc_int.f90
    integer                             , intent(in)    :: ltur     !  Description and declaration in esm_alloc_int.f90
    integer                             , intent(in)    :: lundia   !  Description and declaration in inout.igs
    integer                             , intent(in)    :: numintpar
    integer                             , intent(in)    :: numrealpar
    integer                             , intent(in)    :: numstrpar
    integer                             , intent(in)    :: subiw
    integer      , dimension(numintpar) , intent(inout) :: intpar
    real(fp)                                            :: aks      !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: aksfac
    real(fp)                            , intent(out)   :: akstmp
    real(fp)                            , intent(in)    :: bed
    real(fp)                            , intent(in)    :: bedw
    real(fp)                            , intent(in)    :: camax
    real(fp)                            , intent(out)   :: ce_nm
    real(fp)                            , intent(out)   :: ce_nmtmp
    real(fp)     , dimension(kmax)      , intent(out)   :: concin
    real(fp)                            , intent(out)   :: crep
    real(fp)     , dimension(0:kmax)    , intent(in)    :: dicww    !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(out)   :: dss      !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: dzduu     !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: dzdvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: eps
    real(fp)                            , intent(in)    :: espir
    real(fp)                            , intent(in)    :: fpco
    real(fp)                            , intent(in)    :: frac     !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: gamtcr
    real(fp)                            , intent(in)    :: pangle
    real(fp)     , dimension(30)        , intent(inout) :: par
    real(fp)                            , intent(in)    :: rdc
    real(fp)                            , intent(in)    :: rdw
    real(fp)                            , intent(in)    :: rksrs    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)      , intent(out)   :: rsedeq   !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: rwave
    real(fp)                            , intent(in)    :: salmax
    real(fp)                            , intent(out)   :: sbcu
    real(fp)                            , intent(out)   :: sbcv
    real(fp)                            , intent(out)   :: sbwu
    real(fp)                            , intent(out)   :: sbwv
    real(fp)     , dimension(0:kmax)    , intent(out)   :: seddif   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)      , intent(in)    :: sig      !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: sigmol   !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: spirint  !  Spiral flow intensity
    real(fp)                            , intent(out)   :: sswu
    real(fp)                            , intent(out)   :: sswv
    real(fp)                            , intent(in)    :: sus
    real(fp)                            , intent(in)    :: susw
    real(fp)                            , intent(out)   :: t_relax
    real(fp)                            , intent(in)    :: tauadd
    real(fp)                            , intent(in)    :: taucr0
    real(fp)                            , intent(out)   :: taurat
    real(fp)                            , intent(in)    :: tetacr
    real(fp)     , dimension(kmax)      , intent(in)    :: thick    !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: ubot     !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(out)   :: ustarc
    real(fp)                            , intent(out)   :: ust2
    real(fp)     , dimension(0:kmax)    , intent(in)    :: ws       !  Description and declaration in esm_alloc_real.f90
    real(fp)                            , intent(in)    :: ws0
    real(hp)     , dimension(numrealpar), intent(inout) :: realpar
    logical                             , intent(in)    :: epspar
    logical                             , intent(out)   :: error
    logical                             , intent(in)    :: scour
    logical                             , intent(in)    :: suspfrac !  suspended sediment fraction
    logical                             , intent(in)    :: ubot_from_com
    logical                             , intent(in)    :: wave
    character(256)                      , intent(in)    :: dllfunc
    character(256), dimension(numstrpar), intent(inout) :: strpar
!
! Local variables
!
    integer(pntrsize)           :: ierror_ptr
    integer                     :: k
    integer                     :: kvalue
    integer(pntrsize), external :: perf_function_eqtran
    real(fp)                    :: ag
    real(fp)                    :: aks0
    real(fp)                    :: alphaspir
    real(fp)                    :: apower
    real(fp)                    :: avgcu
    real(fp)                    :: avgu
    real(fp)                    :: bakdif
    real(fp)                    :: cavg
    real(fp)                    :: cesus
    real(fp)                    :: chezy
    real(fp)                    :: cosa
    real(fp)                    :: d10
    real(fp)                    :: d90
    real(fp)                    :: deltas
    real(fp)                    :: delw
    real(fp)                    :: delm
    real(fp)                    :: delr
    real(fp)                    :: di50
    real(fp)                    :: diffbt
    real(fp)                    :: dg
    real(fp)                    :: dgsd
    real(fp)                    :: drho
    real(fp)                    :: dstar
    real(fp)                    :: dz
    real(fp)                    :: ee
    real(fp)                    :: epsbed
    real(fp)                    :: epsmax
    real(fp)                    :: epsmxc
    real(fp)                    :: facce
    real(fp)                    :: fact1
    real(fp)                    :: fc1
    real(fp)                    :: fcc
    real(fp)                    :: fdamp
    real(fp)                    :: ff
    real(fp)                    :: fi
    real(fp)                    :: fsilt
    real(fp)                    :: fw1
    real(fp)                    :: h1
    real(fp)                    :: hidexp
    real(fp)                    :: hrms
    real(fp)                    :: htdif
    real(fp)                    :: lci
    real(fp)                    :: muc
    real(fp)                    :: mudfrac
    real(fp)                    :: phicur
    real(fp)                    :: psi
    real(fp)                    :: ra
    real(fp)                    :: rhosol
    real(fp)                    :: rhowat
    real(fp)                    :: rlabda
    real(fp)                    :: sag
    real(fp)                    :: salinity
    real(fp)                    :: sandfrac
    real(fp)                    :: sbot
    real(fp)                    :: sina
    real(fp)                    :: sk
    real(fp)                    :: ssus
    real(fp)                    :: ssusx
    real(fp)                    :: ssusy
    real(fp)                    :: ta
    real(fp)                    :: taub
    real(fp)                    :: taubcw
    real(fp)                    :: tauc
    real(fp)                    :: taucr1
    real(fp)                    :: tauwav
    real(fp)                    :: teta
    real(fp)                    :: tp
    real(fp)                    :: txg
    real(fp)                    :: tyg
    real(fp)                    :: u
    real(fp)                    :: u2dhim
    real(fp)                    :: umod
    real(fp)                    :: uon
    real(fp)                    :: uoff
    real(fp)                    :: uorb
    real(fp)                    :: usus
    real(fp)                    :: utot
    real(fp)                    :: uuu
    real(fp)                    :: uwb
    real(fp)                    :: uwbih
    real(fp)                    :: uwc
    real(fp)                    :: v
    real(fp)                    :: vicmol
    real(fp)                    :: vonkar
    real(fp)                    :: vvv
    real(fp)                    :: z
    real(fp)                    :: z0cur
    real(fp)                    :: z0rou
    real(fp)                    :: zumod
    real(fp)                    :: zusus
    !
    ! Interface to dll is in High precision!
    !
    real(hp)          :: cesus_dll
    real(hp)          :: sbc_dll
    real(hp)          :: sbcu_dll
    real(hp)          :: sbcv_dll
    real(hp)          :: sbwu_dll
    real(hp)          :: sbwv_dll
    real(hp)          :: ssus_dll
    real(hp)          :: sswu_dll
    real(hp)          :: sswv_dll
    real(hp)          :: t_relax_dll
    character(256)    :: message     ! Contains message from
    logical           :: equi_conc   ! equilibrium concentration near bedlevel
    logical           :: sbc_total   ! total bed load given (instead of m,n components)
    logical           :: sus_total   ! total suspended load given (instead of m,n components)
!
!! executable statements -------------------------------------------------------
!
    ierror_ptr = 0
    error      = .false.
    equi_conc  = .false.
    sbc_total  = .false.
    sus_total  = .false.
    akstmp     = aks
    !
    uuu       = real(realpar(RP_UCHAR),fp)
    vvv       = real(realpar(RP_VCHAR),fp)
    umod      = real(realpar(RP_VELCH),fp)
    zumod     = real(realpar(RP_ZVLCH),fp)
    h1        = real(realpar(RP_DEPTH),fp)
    chezy     = real(realpar(RP_CHEZY),fp)
    hrms      = real(realpar(RP_HRMS) ,fp)
    tp        = real(realpar(RP_TPEAK),fp)
    teta      = real(realpar(RP_TETA) ,fp)
    rlabda    = real(realpar(RP_RLAMB),fp)
    uorb      = real(realpar(RP_UORB) ,fp)
    di50      = real(realpar(RP_D50)  ,fp)
    ! realpar(RP_DSS) = real(dss,hp)
    dstar     = real(realpar(RP_DSTAR),fp)
    d10       = real(realpar(RP_D10MX),fp)
    d90       = real(realpar(RP_D90MX),fp)
    mudfrac   = real(realpar(RP_MUDFR),fp)
    hidexp    = real(realpar(RP_HIDEX),fp)
    ! ws        = real(realpar(RP_SETVL),fp)
    rhosol    = real(realpar(RP_RHOSL),fp)
    rhowat    = real(realpar(RP_RHOWT),fp)
    salinity  = real(realpar(RP_SALIN),fp)
    ! temp      = real(realpar(RP_TEMP) ,fp)
    ag        = real(realpar(RP_GRAV) ,fp)
    vicmol    = real(realpar(RP_VICML),fp)
    taub      = real(realpar(RP_TAUB) ,fp)
    ! ubed      = real(realpar(RP_UBED ),fp)
    ! vbed      = real(realpar(RP_VBED ),fp)
    ! velb      = real(realpar(RP_VELBD),fp)
    ! zvelb     = real(realpar(RP_ZVLBD),fp)
    vonkar    = real(realpar(RP_VNKAR),fp)
    z0cur     = real(realpar(RP_Z0CUR),fp)
    z0rou     = real(realpar(RP_Z0ROU),fp)
    dg        = real(realpar(RP_DG)   ,fp)
    sandfrac  = real(realpar(RP_SNDFR),fp)
    dgsd      = real(realpar(RP_DGSD) ,fp)
    !
    cesus  = 0.0_fp
    sbot   = 0.0_fp
    sbcu   = 0.0_fp
    sbcv   = 0.0_fp
    ssus   = 0.0_fp
    ssusx  = 0.0_fp
    ssusy  = 0.0_fp
    sbwu   = 0.0_fp
    sbwv   = 0.0_fp
    sswu   = 0.0_fp
    sswv   = 0.0_fp
    ee     = exp(1.0_fp)
    sag    = sqrt(ag)
    !
    if (suspfrac) then
       !
       ! Suspended sediment (mud or sand)
       !
       if (iform == -2) then
          call bedbc2004(tp        ,rhosol    ,rhowat    , &
                       & h1        ,umod      ,d10       ,zumod     ,di50      , &
                       & d90       ,z0cur     ,z0rou     ,drho      ,dstar     , &
                       & taucr0    ,u2dhim    ,aks       ,ra        ,usus      , &
                       & zusus     ,uwb       ,muc       ,tauwav    ,ustarc    , &
                       & tauc      ,taurat    ,ta        ,ce_nm     ,dss       , &
                       & uwc       ,uuu       ,vvv       ,rlabda    , &
                       & hrms      ,delw      ,uon       ,uoff      ,uwbih     , &
                       & delm      ,fc1       ,fw1       ,phicur    ,rksrs     , &
                       & i2d3d     ,mudfrac   ,fsilt     ,taucr1    ,psi       , &
                       & dzduu     ,dzdvv     ,eps       ,camax     ,iopsus    , &
                       & ag        ,wave      ,tauadd    ,gamtcr    ) 
       else
          call bedbc1993(tp        ,uorb      ,rhowat    ,h1        ,umod      , &
                       & zumod     ,di50      ,d90       ,z0cur     ,z0rou     , &
                       & dstar     ,taucr0    ,aks       ,usus      ,zusus     , &
                       & uwb       ,delr      ,muc       ,tauwav    ,ustarc    , &
                       & tauc      ,taubcw    ,taurat    ,ta        ,ce_nm     , &
                       & dss       ,mudfrac   ,eps       ,aksfac    ,rwave     , &
                       & camax     ,rdc       ,rdw       ,iopkcw    ,iopsus    , &
                       & vonkar    ,wave      ,tauadd    )
       endif
       realpar(RP_DSS)   = real(dss    ,hp)
       !
       ! Find bottom cell for SAND sediment calculations and store for use
       ! in DIFU and DIF_WS
       !
       kmaxsd = 1
       do k = kmax - 1, 1, -1
          !
          ! Calculate level of lower cell interface
          !
          lci = (1.0_fp + sig(k) - thick(k)/2.0_fp) * h1
          if (lci >= aks) then
             kmaxsd = k
             exit
          endif
       enddo
       !
       ! Adjust ce_nm for presence of multiple sediment fractions.
       !
       if (iform <= 0) then
          ce_nm    = ce_nm * frac
          ce_nmtmp = ce_nm
          akstmp   = aks
       endif
       !
       ! Calculate vertical sediment diffusion coefficient
       !
       if (iform == -2) then
          !
          ! Calculate sediment mixing due to waves following
          ! Van Rijn 2004 - intra-wave approach for bed load (original TR2004)
          !
          if (ce_nm > 1.0e-6_fp) then
             call calseddf2004(ustarc    ,ws        ,tp        ,hrms      ,h1        , &
                             & seddif    ,kmax      ,sig       ,thick     ,dicww     , &
                             & tauwav    ,tauc      ,ltur      ,delw      ,rhowat    , &
                             & uwbih     ,aks       ,ce_nm     ,ce_nmtmp  ,deltas    , &
                             & akstmp    ,di50      ,salinity  ,ws0       ,fdamp     , &
                             & psi       ,epsbed    ,epsmax    ,epsmxc    ,epspar    , &
                             & eps       ,bed       ,vonkar    ,salmax    ,wave      )
          else
             do k=1, kmax
                seddif(k) = dicww(k)
             enddo
          endif
       else
          call calseddf1993(ustarc    ,ws        ,tp        ,delr      ,dstar     , &
                          & uwb       ,hrms      ,h1        ,seddif    ,kmax      , &
                          & sig       ,thick     ,dicww     ,tauwav    ,tauc      , &
                          & ltur      ,eps       ,vonkar    ,wave      )
       endif
       !
       ! Calculate equilibrium concentration profile for sediment
       ! Note: option of selecting either Rouse profile or solution
       ! by numerical integration has been removed; only numerical
       ! integration.
       ! set background diffusion and effective beta factor
       !
       bakdif = vicmol / sigmol
       !
       ! Use simple expression based on upwind approximation for
       ! concentration and fall velocity, and central difference for
       ! concentration gradient.
       ! solution to stationary advection/diffusion equation in vertical.
       !
       if (iform == -2) then
          !
          ! In case of Van Rijn 2004
          !
          if (i2d3d==2 .or. epspar) then
             !
             ! write concentration concin in an empty array if 2D or
             ! Van Rijn's parametric model and susp. transp. due to waves is used       
             !
             if (ce_nm>1.0e-6_fp) then
                rsedeq(kmaxsd+1) = ce_nmtmp
                aks0             = akstmp / h1
                do k = kmaxsd, 1, -1
                   if (rsedeq(k+1) > 1.0e-6_fp) then
                      dz        = h1 * (sig(k)-sig(k+1))
                      fi        = max(1.0_fp + ((rsedeq(k+1)/0.65_fp)**0.8_fp) - 2.0_fp*((rsedeq(k+1)/0.65_fp)**0.4_fp), 0.01_fp)
                      seddif(k) = seddif(k) * fi                      
                      fcc       = -ws(k) * rsedeq(k+1) * (max(1.0_fp-rsedeq(k+1),0.5_fp))**5.0_fp / seddif(k)
                      ff        = 1.0_fp / rsedeq(k+1) * fcc
                      rsedeq(k) = exp(log(rsedeq(k+1))+dz*ff)
                   else
                      rsedeq(k) = 0.0_fp
                   endif
                enddo
                !
                ! And then work down
                !
                if (kmax > kmaxsd+1) then
                   do k = kmaxsd + 2, kmax
                      rsedeq(k) = rsedeq(k-1)
                   enddo
                endif
                do k = 1,kmax
                   rsedeq(k) = rsedeq(k) * rhosol
                   concin(k) = rsedeq(k)
                enddo
             else
                !
                ! ce_nm<=1.0e-6_fp
                !
                do k = 1, kmax
                   rsedeq(k) = 0.0_fp
                   concin(k) = 0.0_fp
                enddo
             endif
          else
             !
             ! use the r0 values copied into concin array by erosed
             !
             if (ce_nm<=1.0e-6_fp) then
                do k = 1, kmax
                   rsedeq(k) = 0.0_fp
                enddo
             endif
          endif
       else
          !
          ! If transport formula is not Transpor2004
          !
          if (ce_nm>1.0e-6_fp .or. iform>0) then
             ! Use simple expression based on upwind approximation for
             ! concentration and fall velocity, and central difference for
             ! concentration gradient.
             ! solution to stationary advection/diffusion equation in vertical.
             !
             aks0 = aks / h1
             !
             ! Put concentration in kmaxsd cell
             !
             sk     = 1.0_fp + sig(kmaxsd)
             dz     = h1 * (sk-aks0)
             diffbt = seddif(kmaxsd) + bakdif
             diffbt = max(diffbt , 0.1_fp*ws(kmaxsd)*dz)
             fact1  = 1.0_fp + dz * ws(kmaxsd) / diffbt
             !
             ! In case of other formulation than Van Rijn, ce_nm is not known but
             ! crep is given (computed furtheron). Compute vertical using ce_nm=1
             ! to obtain relation between ce_nm and crep
             !
             if (iform <= 0) then
                !
                ! Van Rijn
                !
                rsedeq(kmaxsd) = ce_nm / fact1 * rhosol
             else
                !
                ! All other formulations based on 2DH formulae
                !
                rsedeq(kmaxsd) = 1.0_fp
             endif
             !
             ! Now work upward
             !
             do k = kmaxsd - 1, 1, -1
                sk = 1.0_fp + sig(k)
                !
                ! Set diffusion coefficient at bottom of layer
                !
                diffbt    = seddif(k) + bakdif
                diffbt    = max(diffbt , 0.1_fp*ws(k)*dz)
                dz        = h1 * (sig(k)-sig(k+1))
                fact1     = 1.0_fp + dz * ws(k) / diffbt
                rsedeq(k) = rsedeq(k+1) / fact1
             enddo
             !
             ! And then work down
             !
             do k = kmaxsd + 1, kmax
                rsedeq(k) = rsedeq(k-1)
             enddo
          else
             !
             ! if ce_nm <= 1.0e-6 and iform <= 0 and .not.iform == -2
             !
             do k = 1, kmax
                rsedeq(k) = 0.0_fp
             enddo
          endif
       endif ! end calculation of equilibrium concentration profile for sediment
       !
       ! Compute depth-averaged velocity, representative concentration and transport
       !
       ! imaginary "depth-averaged current" which has a logarithmic
       ! velocity profile, and a velocity at the bottom zeta point equivalent
       ! to that calculated by the model for 3D current and waves is
       ! calculated in bedbc2004/ (also 1993??) = u2dhim
       !
       avgu     = 0.0_fp
       avgcu    = 0.0_fp
       if (zumod > 0.0_fp) then
          do k = 1, kmax
             z     = (1.0_fp + sig(k)) * h1
             u     = log(1.0_fp + z/z0rou)
             avgu  = avgu  + u*thick(k)
             avgcu = avgcu + u*rsedeq(k)*thick(k)
          enddo
          crep = avgcu / max(avgu,eps)
          avgu = avgu * umod / log(1.0_fp + zumod/z0rou)
       else
          crep = 0.0_fp
       endif
    else
       !
       ! Non suspended sediment (bedload)
       !
       if (kmax == 1) then
          !
          ! for kmax == 1, zumod should be
          ! (1+h1/z0rou)**(z0rou/h1)*exp(-1)*(z0rou+h1)-z0rou
          ! the general formula below (kmax>1) then gives
          !
          avgu = umod
          !
          ! we need to set it here directly because zumod was
          ! approximated in DWNVEL as exp(-1)*h1
          !
       else
          if (zumod > 0.0_fp) then
             !
             ! the numerical integration
             !
             !avgu = 0.0
             !do k = 1, kmax
             !   z = (1.0 + sig(k))*h1
             !   u = log(1.0 + z/z0rou)
             !   avgu  = avgu  + u*thick(k)
             !enddo
             !
             ! can be replaced by analytical integration
             !        h1
             !  1    /
             ! --- * | log(1+z/z0rou) dz =
             !  h1   /
             !      z=0
             !
             ! (z0rou/h1+1) * log(1+h1/z0rou) - 1 =
             !
             ! log (1 + zavg/z0rou) where
             ! zavg = (1+h1/z0rou)**(z0rou/h1)*exp(-1)*(z0rou+h1)-z0rou
             ! the level at which u(z) = avgu
             !
             avgu = (z0rou/h1+1.0_fp)*log(1.0_fp+h1/z0rou) - 1.0_fp
             avgu = avgu * umod / log(1.0_fp+zumod/z0rou)
          else
             avgu = 0.0_fp
          endif
       endif
       kmaxsd = kmax
    endif
    if (scour) then
       utot = ustarc * chezy / sag
    else
       utot = avgu
    endif
    u     = utot * uuu / (umod+eps)
    v     = utot * vvv / (umod+eps)
    !
    if (iform == -1) then
       if (bed > 0.0_fp) then
          call bedtr1993(uuu       ,vvv       ,utot      ,di50      ,d90       , &
                       & h1        ,taurat    ,ustarc    ,muc       ,rhosol    , &
                       & dstar     ,ws(1)     ,hrms      ,tp        ,teta      , &
                       & rlabda    ,umod      ,sbcu      ,sbcv      ,sbwu      , &
                       & sbwv      ,sswu      ,sswv      ,lundia    ,rhowat    , &
                       & ag        ,wave      ,eps       ,error     )
          if (error) return
       endif
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == -2) then
       !
       ! VAN RIJN 2004 Instantaneous bed load
       !
       if ((bed>0.0_fp .or. bedw>0.0_fp .or. susw>0.0_fp) .and. ce_nm>0.0_fp) then
          call bedtr2004(u2dhim    ,di50      ,d90       ,h1        ,rhosol    , &
                       & tp        ,teta      ,lundia    ,uon       ,uoff      , &
                       & uwb       ,taucr1    ,delm      ,ra        ,z0cur     , &
                       & fc1       ,fw1       ,dstar     ,drho      ,phicur    , &
                       & sbcu      ,sbcv      ,sbwu      ,sbwv      ,sswu      , &
                       & sswv      ,tetacr    ,aks       ,fsilt     ,sig       , &
                       & thick     ,concin    ,kmax      ,deltas    ,ws(1)     , &
                       & rksrs     ,dzduu     ,dzdvv     ,rhowat    , &
                       & ag        ,bedw      ,pangle    ,fpco      ,susw      , &
                       & wave      ,eps       ,subiw     ,error     )
          if (error) return
       endif
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 1) then
       !
       ! Engelund-Hansen
       !
       call tranb1(utot      ,di50      ,chezy     ,h1        ,par        , &
                 & sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 2) then
       !
       ! Meyer-Peter-Muller
       !
       call tranb2(utot       ,di50      ,d90       ,chezy     ,h1        , &
                 & par        ,hidexp    ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 3) then
       !
       ! Ackers-White
       !
       call tranb3(utot      ,d90       ,chezy     ,h1        ,par        , &
                 & sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 4) then
       !
       ! general relation for bed load
       !
       call tranb4(utot      ,di50      ,chezy     ,par        ,hidexp    , &
                 & sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 5) then
       !
       ! Bijker
       !
       call tranb5(u         ,v         ,di50      ,d90       ,chezy      , &
                 & h1        ,hrms      ,tp        ,teta      ,par        , &
                 & dzduu     ,dzdvv     ,sbcu      ,sbcv      ,ssusx      , &
                 & ssusy     ,cesus     ,vonkar    )
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 6) then
       !
       ! Bailard
       !
       call prterr (lundia,'U021','Bailard method is disabled')
       error = .true.
       return
       !
       !call tranb6(utot      ,u          ,v         ,chezy     ,h1        , &
       !          & hrms      ,tp         ,teta      ,diss      ,dzduu     , &
       !          & dzdvv     ,par        ,sbcu      ,sbcv      ,ssusx     , &
       !          & ssusy     )
       !
       ! sbc_total = .false.
       ! sus_total = .false.
    elseif (iform == 7) then
       !
       ! Van Rijn (1984, modified)
       !
       call tranb7(utot      ,di50       ,d90       ,h1        ,par        , &
                 & sbot      ,ssus      ,vonkar    )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 8) then
       !
       ! Van Rijn / Ribberink (1994)
       !
       call prterr (lundia,'U021','Van Rijn/Ribberink (1994) method is disabled')
       error = .true.
       return
       !
       !call tranb8(u         ,v         ,hrms      ,h1         ,teta      , &
       !          & tp        ,di50      ,d90       ,diss       ,dzduu     , &
       !          & dzdvv     ,par       ,sbcu      ,sbcv       ,ssusx     , &
       !          & ssusy     )
       !
       ! sbc_total = .false.
       ! sus_total = .false.
    elseif (iform == 9) then
       !
       ! Silt module
       !
       call prterr (lundia,'U021','Original Delft3D-MOR Silt module is disabled')
       error = .true.
       return
       !
       !call tranb9(utot      ,h1        ,alfs      ,sbot      ,ssus      )
       !
       ! sbc_total = .true.
       ! sus_total = .true.
    elseif (iform == 10) then
       !
       ! Ashida and Michiue
       !
       call prterr (lundia,'U021','Ashida and Michiue method is disabled')
       error = .true.
       return
       !
       !call trab10(utot      ,di50      ,chezy     ,h1         ,cosa      , &
       !          & sina      ,dzduu     ,dzdvv     ,par        ,sbot      , &
       !          & ssus      )
       !
       ! sbc_total = .true.
       ! sus_total = .true.
    elseif (iform == 11) then
       !
       ! Soulsby and Van Rijn
       !
       call trab11(u         ,v          ,hrms      ,h1        ,tp        , &
                 & di50      ,par        ,sbcu      ,sbcv      ,ssusx     , &
                 & ssusy     ,ubot       ,vonkar    ,ubot_from_com        )
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 12) then
       !
       ! Soulsby
       !
       call trab12(u         ,v         ,hrms       ,h1        ,tp        , &
                 & teta      ,di50      ,par        ,sbcu      ,sbcv      , &
                 & ssusx     ,ssusy     ,ubot       ,vonkar    ,ubot_from_com)
       !
       sbc_total = .false.
       sus_total = .false.
    elseif (iform == 13) then
       !
       ! test transport (Wang) Fredsoe
       !
       call tran9t(utot      ,di50       ,d90       ,chezy     ,h1        , &
                 & ustarc    ,par        ,sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 14) then
       !
       ! generalized Ashida and Michiue
       !
       call trab14(utot      ,di50      ,chezy     ,par        ,hidexp    , &
                 & sbot      ,ssus      )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 16) then 
       !
       ! Wilcock & Crowe
       !
       call trabwc(utot      ,di50      ,taub      ,par       ,sbot      , &
                 & ssus      ,dg        ,sandfrac  ,chezy     )
       !
       sbc_total = .true.
       sus_total = .true. 
    elseif (iform == 17) then 
       !
       ! Modified Wilcock & Crowe
       !
       call trabwc2(utot       ,di50      ,taub       ,par       ,sbot      , &
                  & ssus       ,dg        ,dgsd       ,chezy     )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 18) then 
       !
       ! Gaeuman et al. (development of Wilcock & Crowe
       !
       call trabg(utot       ,di50      ,taub       ,par       ,sbot      , &
                & ssus       ,dg        ,dgsd       ,chezy     )
       !
       sbc_total = .true.
       sus_total = .true.
    elseif (iform == 15) then
       !
       ! User defined formula in DLL
       ! Input parameters are passed via realpar/intpar/strpar-arrays that have
       ! been mostly filled in calling routine
       !
       realpar(RP_UMEAN) = real(u      ,hp)
       realpar(RP_VMEAN) = real(v      ,hp)
       realpar(RP_VELMN) = real(utot   ,hp)
       ! Initialisation of output variables of user defined transport formulae
       !
       sbc_total   = .false. ! may be changed by user defined formulae
       sus_total   = .true.  ! always true for user defined formulae
       sbc_dll     = 0.0_hp
       sbcu_dll    = 0.0_hp
       sbcv_dll    = 0.0_hp
       sbwu_dll    = 0.0_hp
       sbwv_dll    = 0.0_hp
       !
       equi_conc   = .false.
       cesus_dll   = 0.0_hp
       ssus_dll    = 0.0_hp
       sswu_dll    = 0.0_hp
       sswv_dll    = 0.0_hp
       t_relax_dll = 0.0_hp
       message     = ' '
       !
       ! psem/vsem is used to be sure this works fine in DD calculations
       !
       call psemlun
       ierror_ptr = perf_function_eqtran(dllhandle       , dllfunc           , &
                                         intpar          , numintpar         , &
                                         realpar         , numrealpar        , &
                                         strpar          , numstrpar         , &
                                         sbc_total, sbc_dll  , sbcu_dll      , &
                                         sbcv_dll , sbwu_dll , sbwv_dll      , &
                                         equi_conc, cesus_dll, ssus_dll      , &
                                         sswu_dll , sswv_dll , t_relax_dll   , &
                                         message)
       call vsemlun
       if (ierror_ptr /= 0) then
          write(lundia,'(a,a,a)') '*** ERROR Cannot find function "',trim(dllfunc),'" in dynamic library.'
          error = .true.
          return
       endif
       if (message /= ' ') then
          write (lundia,'(a,a,a)') '*** ERROR Message from user defined transport formulae ',trim(dllfunc),' :'
          write (lundia,'(a,a  )') '          ', trim(message)
          write (lundia,'(a    )') ' '
          error = .true.
          return
       endif
       !
       ! Output parameters
       !
       sbot    = real(sbc_dll ,fp)
       sbcu    = real(sbcu_dll,fp)
       sbcv    = real(sbcv_dll,fp)
       sbwu    = real(sbwu_dll,fp)
       sbwv    = real(sbwv_dll,fp)
       !
       cesus   = real(cesus_dll,fp)
       ssus    = real(ssus_dll ,fp)
       sswu    = real(sswu_dll ,fp)
       sswv    = real(sswv_dll ,fp)
       !
       t_relax = real(t_relax_dll,fp)
    else
       call prterr (lundia,'U021','Transport formula not recognized')
       error = .true.
       return
    endif
    !
    if (iform > 0) then
       !
       ! Change from volume to mass concentrations/transport rates.
       !
       ! Van Rijn 1993 or Van Rijn 2004 include rhosol in the transport
       ! rates, the other formulae don't. So, multiply the transport
       ! rates and concentrations by rhosol now.
       !
       if (sbc_total) then
          sbot  = sbot * rhosol
       else
          sbcu  = sbcu * rhosol
          sbcv  = sbcv * rhosol
       endif
       !
       cesus = cesus * rhosol
       if (sus_total) then
          ssus = ssus * rhosol
       else
          ssusx = ssusx * rhosol
          ssusy = ssusy * rhosol
          ssus  = sqrt(ssusx**2 + ssusy**2)
       endif
       !
       sbwu = sbwu * rhosol
       sbwv = sbwv * rhosol
       sswu = sswu * rhosol
       sswv = sswv * rhosol
    endif
    !
    ! If only bed load transport magnitude is given, then the bed load
    ! should be oriented based on the near bed velocity. The near bed
    ! velocity is given by either the velocity (uuu,vvv) in 3D or
    ! the depth averaged velocity (uuu,vvv) corrected for spiral flow in 2D.
    !
    ust2 = (ag/chezy**2) * umod**2
    if (sbc_total) then
       if (umod > 0.0_fp) then
          !
          ! Correct bed load transport direction for spiral flow intensity
          !
          if (lsecfl == 0) then
             alphaspir = 0.0_fp
          else
             alphaspir = sqrt(ag) / 0.4_fp / chezy
             alphaspir = 12.5_fp * espir * (1.0_fp-0.5_fp*alphaspir)
             alphaspir = alphaspir * spirint / umod
          endif
          txg  = ust2 * (uuu + alphaspir*vvv) / umod
          tyg  = ust2 * (vvv - alphaspir*uuu) / umod
          ust2 = sqrt(txg**2 + tyg**2)
          if (ust2 > eps) then
             cosa = txg / ust2
             sina = tyg / ust2
          else
             cosa = 0.0_fp
             sina = 0.0_fp
          endif
          sbcu  = sbot * cosa
          sbcv  = sbot * sina
       else
          sbcu  = 0.0_fp
          sbcv  = 0.0_fp
      endif
    endif
    !
    ! Adjust for calibration factors
    !
    sbcu = bed  * sbcu
    sbcv = bed  * sbcv
    sbwu = bedw * sbwu
    sbwv = bedw * sbwv
    sswu = susw * sswu
    sswv = susw * sswv
    !
    if (iform>0) then
       cesus    = sus * cesus
       ssus     = sus * ssus
       ssusx    = sus * ssusx
       ssusy    = sus * ssusy
    else
       ce_nm    = sus * ce_nm
       ce_nmtmp = sus * ce_nmtmp
       do k = 1, kmax
          rsedeq(k) = sus * rsedeq(k)
       enddo
       crep     = sus * crep
    endif
    !
    if (iform>0 .and. suspfrac) then
       !
       ! If we are not using Van Rijn 1993 or Van Rijn 2004
       ! then we still need to compute values for ce_nm,
       ! ce_nmtmp, crep and rsedeq.
       !
       if (equi_conc) then
           !
           ! Concentration given by transport formula
           !
       else
           !
           ! Suspended transport rate given by transport formula,
           ! derive concentration
           !
           cesus = ssus / (utot+eps) / h1
       endif
       !
       ! Correct for fraction presence in the bed and calibration factor.
       ! Note that this multiplication has been done much earlier for
       ! Van Rijn 1993 and 2004.
       !
       cesus = cesus * frac
       !
       facce = cesus / (crep+eps)
       do k = 1, kmax
          rsedeq(k) = rsedeq(k) * facce
       enddo
       crep     = cesus
       sk       = 1.0_fp + sig(kmaxsd)
       dz       = h1 * (sk-aks0)
       diffbt   = seddif(kmaxsd) + bakdif
       diffbt   = max(diffbt , 0.1_fp*ws(kmaxsd)*dz)
       fact1    = 1.0_fp + dz * ws(kmaxsd) / diffbt
       ce_nm    = fact1 * facce / rhosol
       ce_nmtmp = ce_nm
    elseif (.not.suspfrac) then
       !
       ! Note: in case of bedload sediment type, the suspended load is added to the
       ! bed load to compute the total load. Bed slope effect will be applied to
       ! both the bed- and the suspended part.
       !
       ! The effect of frac is included in the bed load at a later stage.
       !
       if (iform <= 0) then
           !
           ! Van Rijn 1993 or 2004 formula
           ! NOTE 1: This doesn't work yet because Van Rijn 1993 and 2004
           ! routines get only called if suspfrac=true (so I can't get here
           ! or I don't have the right values).
           ! NOTE 2: crep is based on ce_nm which has been multiplied by
           ! frac above. Since sbcu/v will be multiplied by frac later (again)
           ! there is the risk of double correction. So I would have to
           ! "unmultiply" by division which actually doesn't work if frac=0.
           !
           sbcu = sbcu + crep * h1 * u / frac
           sbcv = sbcv + crep * h1 * v / frac
       elseif (equi_conc) then
           !
           ! Concentration given by transport formula
           !
           sbcu = sbcu + cesus * h1 * u
           sbcv = sbcv + cesus * h1 * v
       elseif (sus_total) then
           !
           ! Total suspended transport rate given by transport formula,
           ! assume that it is transported in depth-averaged direction
           !
           sbcu = sbcu + ssus * u / (utot+eps)
           sbcv = sbcv + ssus * v / (utot+eps)
       else
           !
           ! Suspended transport rate components given by transport
           ! formula, add them individually to the bed load
           !
           sbcu = sbcu + ssusx
           sbcv = sbcv + ssusy
       endif
    endif
    if (comparereal(akstmp,0.0_fp) == 0) then
        akstmp = aks
    endif
end subroutine eqtran
