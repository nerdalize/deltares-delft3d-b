subroutine wrsedm(lundia    ,error     ,mmax      ,kmax      ,nmaxus    , &
                & lsed      ,lsedtot   ,irequest  ,fds       ,grpnam    , &
                & sbuu      ,sbvv      ,ssuu      ,ssvv      ,ws        , &
                & rsedeq    ,dps       ,rca       ,gdp       )
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
!  $Id: wrsedm.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrsedm.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for sediment transport to the
!              sediment group on the NEFIS FLOW MAP file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: celidt
    type (nefiselement)                  , pointer :: nefiselem
    integer                              , pointer :: nxx
    integer  , dimension(:)              , pointer :: smlay
    type (moroutputtype)                 , pointer :: moroutput
    logical                              , pointer :: scour
    real(fp), dimension(:)               , pointer :: xx
    real(fp), dimension(:)               , pointer :: rhosol
    real(fp), dimension(:)               , pointer :: cdryb
    real(fp), dimension(:)               , pointer :: dm
    real(fp), dimension(:)               , pointer :: dg
    real(fp), dimension(:)               , pointer :: dgsd
    real(fp), dimension(:,:)             , pointer :: dxx
    real(fp), dimension(:)               , pointer :: dzduu
    real(fp), dimension(:)               , pointer :: dzdvv
    real(fp), dimension(:,:)             , pointer :: fixfac
    real(fp), dimension(:,:)             , pointer :: frac
    real(fp), dimension(:)               , pointer :: mudfrac
    real(fp), dimension(:)               , pointer :: sandfrac
    real(fp), dimension(:,:)             , pointer :: hidexp
    real(fp), dimension(:,:)             , pointer :: sbcu
    real(fp), dimension(:,:)             , pointer :: sbcv
    real(fp), dimension(:,:)             , pointer :: sbcuu
    real(fp), dimension(:,:)             , pointer :: sbcvv
    real(fp), dimension(:,:)             , pointer :: sbwu
    real(fp), dimension(:,:)             , pointer :: sbwv
    real(fp), dimension(:,:)             , pointer :: sbwuu
    real(fp), dimension(:,:)             , pointer :: sbwvv
    real(fp), dimension(:,:)             , pointer :: sswu
    real(fp), dimension(:,:)             , pointer :: sswv
    real(fp), dimension(:,:)             , pointer :: sswuu
    real(fp), dimension(:,:)             , pointer :: sswvv
    real(fp), dimension(:,:)             , pointer :: sucor
    real(fp), dimension(:,:)             , pointer :: svcor
    real(fp), dimension(:,:)             , pointer :: sinkse
    real(fp), dimension(:,:)             , pointer :: sourse
    real(fp), dimension(:,:)             , pointer :: taurat
    real(fp), dimension(:)               , pointer :: ust2
    real(fp), dimension(:)               , pointer :: umod
    real(fp), dimension(:)               , pointer :: uuu
    real(fp), dimension(:)               , pointer :: vvv
    real(fp), dimension(:)               , pointer :: zumod
!
! Global variables
!
    integer                                                                    , intent(in)  :: irequest !! Action flag: 1 = define, 2 = write
    character(16)                                                              , intent(in)  :: grpnam !!  Group name
    integer                                                                    , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lsedtot!  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lundia !  Description and declaration in inout.igs
    integer                                                                    , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                                    , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, lsed), intent(in)  :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lsed)  , intent(in)  :: rsedeq !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed)        , intent(in)  :: rca    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbuu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbvv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed)        , intent(in)  :: ssuu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed)        , intent(in)  :: ssvv   !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    real(fp)                :: rhol
    real(fp)                :: tauadd
    integer                 :: ierror     ! Local errorflag for NEFIS files 
    integer                 :: fds
    integer                 :: i
    integer                 :: k          ! Help var. 
    integer                 :: kmaxout    ! number of layers to be written to the (history) output files
    integer                 :: l          ! Help var. 
    integer                 :: m          ! Help var. 
    integer                 :: n          ! Help var. 
    integer                 :: nm         ! Help var. 
    integer, dimension(3,5) :: uindex
    integer, external       :: putelt
    integer, external       :: neferr
    character(10)           :: transpunit
    character(16)           :: dxname
    character(256)          :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64)           :: dxdescr
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedminf)
    celidt              => nefiselem%celidt
    nxx                 => gdp%gdmorpar%nxx
    smlay               => gdp%gdpostpr%smlay
    moroutput           => gdp%gdmorpar%moroutput
    xx                  => gdp%gdmorpar%xx
    rhosol              => gdp%gdsedpar%rhosol
    cdryb               => gdp%gdsedpar%cdryb
    scour               => gdp%gdscour%scour
    dm                  => gdp%gderosed%dm
    dg                  => gdp%gderosed%dg
    dgsd                => gdp%gderosed%dgsd
    dxx                 => gdp%gderosed%dxx
    dzduu               => gdp%gderosed%dzduu
    dzdvv               => gdp%gderosed%dzdvv
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    mudfrac             => gdp%gderosed%mudfrac
    sandfrac            => gdp%gderosed%sandfrac
    hidexp              => gdp%gderosed%hidexp
    sbcu                => gdp%gderosed%sbcu
    sbcv                => gdp%gderosed%sbcv
    sbcuu               => gdp%gderosed%sbcuu
    sbcvv               => gdp%gderosed%sbcvv
    sbwu                => gdp%gderosed%sbwu
    sbwv                => gdp%gderosed%sbwv
    sbwuu               => gdp%gderosed%sbwuu
    sbwvv               => gdp%gderosed%sbwvv
    sswu                => gdp%gderosed%sswu
    sswv                => gdp%gderosed%sswv
    sswuu               => gdp%gderosed%sswuu
    sswvv               => gdp%gderosed%sswvv
    sucor               => gdp%gderosed%sucor
    svcor               => gdp%gderosed%svcor
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    taurat              => gdp%gderosed%taurat
    ust2                => gdp%gderosed%ust2
    umod                => gdp%gderosed%umod
    uuu                 => gdp%gderosed%uuu
    vvv                 => gdp%gderosed%vvv
    zumod               => gdp%gderosed%zumod
    !
    kmaxout = size(smlay)
    select case (irequest)
    case (1)
       !
       ! Define elements
       !
       select case(moroutput%transptype)
       case (0)
          transpunit = '[ KG/S/M]'
       case (1)
          transpunit = '[ M3/S/M]'
       case (2)
          transpunit = '[ M3/S/M]'
       end select
       !
       ! map-sed-series
       !
       call addelm(nefiswrsedm,'WS',' ','[  M/S  ]','REAL',4             , &
          & 'Settling velocity per layer'                                , &
          & 4         ,nmaxus    ,mmax      ,kmaxout   ,lsed      ,0     , &
          & lundia    ,gdp       )
       if (kmax==1) then
          call addelm(nefiswrsedm,'RSEDEQ',' ','[ KG/M3 ]','REAL',4         , &
             & 'Equilibrium concentration of sediment (2D only)'            , &
             & 4         ,nmaxus    ,mmax      ,kmax      ,lsed      ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%uuuvvv) then
          call addelm(nefiswrsedm,'UUU',' ','[  M/S  ]','REAL',4            , &
             & 'Characteristic velocity u-direction (zeta point)'           , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'VVV',' ','[  M/S  ]','REAL',4            , &
             & 'Characteristic velocity v-direction (zeta point)'           , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%umod) then
          call addelm(nefiswrsedm,'UMOD',' ','[  M/S  ]','REAL',4           , &
             & 'Characteristic velocity magnitude (zeta point)'             , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%zumod) then
          call addelm(nefiswrsedm,'ZUMOD',' ','[  M/S  ]','REAL',4          , &
             & 'Height above bed for characteristic velocity (zeta point)'  , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%ustar) then
          call addelm(nefiswrsedm,'USTAR',' ','[  M/S  ]','REAL',4          , &
             & 'Bed shear velocity U* (zeta point)'                         , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%sbcuv) then
         call addelm(nefiswrsedm,'SBCU',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport u-direction due to currents (zeta point)', &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBCV',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport v-direction due to currents (zeta point)', &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sbcuuvv) then
         call addelm(nefiswrsedm,'SBCUU',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport u-direction due to currents (u point)'   , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBCVV',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport v-direction due to currents (v point)'   , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sbwuv) then
         call addelm(nefiswrsedm,'SBWU',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport u-direction due to waves (zeta point)'   , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBWV',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport v-direction due to waves (zeta point)'   , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sbwuuvv) then
         call addelm(nefiswrsedm,'SBWUU',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport u-direction due to waves (u point)'      , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBWVV',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport v-direction due to waves (v point)'      , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sswuv) then
         call addelm(nefiswrsedm,'SSWU',' ',transpunit ,'REAL',4           , &
            & 'Suspended transport u-direction due to waves (zeta point)'  , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SSWV',' ',transpunit ,'REAL',4           , &
            & 'Suspended transport v-direction due to waves (zeta point)'  , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sswuuvv) then
         call addelm(nefiswrsedm,'SSWUU',' ',transpunit ,'REAL',4          , &
            & 'Suspended transport u-direction due to waves (u point)'     , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SSWVV',' ',transpunit ,'REAL',4          , &
            & 'Suspended transport v-direction due to waves (v point)'     , &
            & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       call addelm(nefiswrsedm,'SBUU',' ',transpunit ,'REAL',4           , &
          & 'Bed-load transport u-direction (u point)'                   , &
          & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'SBVV',' ',transpunit ,'REAL',4           , &
          & 'Bed-load transport v-direction (v point)'                   , &
          & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'SSUU',' ',transpunit ,'REAL',4           , &
          & 'Suspended-load transport u-direction (u point)'             , &
          & 3         ,nmaxus    ,mmax      ,lsed      ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'SSVV',' ',transpunit ,'REAL',4           , &
          & 'Suspended-load transport v-direction (v point)'             , &
          & 3         ,nmaxus    ,mmax      ,lsed      ,0         ,0     , &
          & lundia    ,gdp       )
       if (moroutput%suvcor) then
         call addelm(nefiswrsedm,'SUCOR',' ',transpunit ,'REAL',4          , &
            & 'Near-bed transport correction u-direction (u point)'        , &
            & 3         ,nmaxus    ,mmax      ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SVCOR',' ',transpunit ,'REAL',4          , &
            & 'Near-bed transport correction v-direction (v point)'        , &
            & 3         ,nmaxus    ,mmax      ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sourcesink) then
         call addelm(nefiswrsedm,'SOURSE',' ','[KG/M3/S]' ,'REAL',4        , &
            & 'Source term suspended sediment fractions'                   , &
            & 3         ,nmaxus    ,mmax      ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SINKSE',' ','[  1/S  ]' ,'REAL',4        , &
            & 'Sink term suspended sediment fractions'                     , &
            & 3         ,nmaxus    ,mmax      ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       call addelm(nefiswrsedm,'RCA',' ','[ KG/M3 ]','REAL',4            , &
          & 'Near-bed reference concentration of sediment'               , &
          & 3         ,nmaxus    ,mmax      ,lsed      ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'DPS',' ','[   M   ]','REAL',4            , &
          & 'Bottom depth (zeta point)'                                  , &
          & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
          & lundia    ,gdp       )
       if (moroutput%dzduuvv) then
          call addelm(nefiswrsedm,'DZDUU',' ','[   -   ]','REAL',4          , &
             & 'Bed slope in u-direction (u point)'                         , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'DZDVV',' ','[   -   ]','REAL',4          , &
             & 'Bed slope in v-direction (v point)'                         , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (scour) then
          call addelm(nefiswrtmap,'TAUADD',' ','[  N/M2 ]','REAL',4          , &
             & 'Extra shear stress due to scour feature                     ', &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (moroutput%taurat) then
          call addelm(nefiswrsedm,'TAURAT',' ','[   -   ]','REAL',4         , &
             & 'Excess bed shear ratio'                                     , &
             & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%dm) then
          call addelm(nefiswrsedm,'DM',' ','[   M   ]','REAL',4             , &
             & 'Arithmetic mean sediment diameter'                          , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%dg) then
          call addelm(nefiswrsedm,'DG',' ','[   M   ]','REAL',4             , &
             & 'Geometric mean sediment diameter'                           , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%dgsd) then
          call addelm(nefiswrsedm,'DGSD',' ','[   M   ]','REAL',4           , &
             & 'Geometric standard deviation of particle size mix'          , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%percentiles) then
          do l = 1, nxx
             write(dxname,'(A,I2.2)') 'DXX',l
             write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , &
                & xx(l)*100.0,' %'
             call addelm(nefiswrsedm,dxname,' ','[   M   ]','REAL',4        , &
                & dxdescr                                                   , &
                & 2         ,nmaxus    ,mmax      ,0         ,0         ,0  , &
                & lundia    ,gdp       )
          enddo
       endif
       if (moroutput%frac) then
          call addelm(nefiswrsedm,'FRAC',' ','[   -   ]','REAL',4           , &
             & 'Availability fraction in top layer'                         , &
             & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%mudfrac) then
          call addelm(nefiswrsedm,'MUDFRAC',' ','[   -   ]','REAL',4        , &
             & 'Mud fraction in top layer'                                  , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%sandfrac) then
          call addelm(nefiswrsedm,'SANDFRAC',' ','[   -   ]','REAL',4        , &
             & 'Sand fraction in top layer'                                  , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%fixfac) then
          call addelm(nefiswrsedm,'FIXFAC',' ','[   -   ]','REAL',4         , &
             & 'Reduction factor due to limited sediment thickness'         , &
             & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%hidexp) then
          call addelm(nefiswrsedm,'HIDEXP',' ','[   -   ]','REAL',4         , &
             & 'Hiding and exposure factor'                                 , &
             & 3         ,nmaxus    ,mmax      ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       !
       ! Add mor fields
       !
       call wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & 1         ,0         ,grpnam    ,gdp       )
    case (2)
       !
       ! Write data to file
       !
       uindex (1,1) = celidt
       uindex (2,1) = celidt
       uindex (3,1) = 1 ! increment in time
       !
       ! element 'WS'
       !
       call sbuff_checksize(lsed*(kmaxout)*mmax*nmaxus)
       i = 0
       do l = 1, lsed
          do k = 1, kmaxout
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   sbuff(i) = real(ws(n, m, smlay(k), l),sp)
                enddo
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'WS', uindex, 1, sbuff)
       if (ierror/=0) goto 9999
       !
       if (kmax==1) then
          !
          ! element 'RSEDEQ'
          ! kmax=1: don't use kmaxout/smlay
          !
          call sbuff_checksize(lsed*kmax*mmax*nmaxus)
          i = 0
          do l = 1, lsed
             do k = 1, kmax
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      sbuff(i) = real(rsedeq(n, m, k, l),sp)
                   enddo
                enddo
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'RSEDEQ', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%uuuvvv) then
          !
          ! element 'UUU'
          !
          call sbuff_checksize(mmax*nmaxus)
          if (associated(uuu)) then
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(uuu(nm),sp)
                enddo
             enddo
          else
             sbuff(1:mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'UUU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'VVV'
          !
          call sbuff_checksize(mmax*nmaxus)
          if (associated(vvv)) then
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(vvv(nm),sp)
                enddo
             enddo
          else
             sbuff(1:mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'VVV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%umod) then
          !
          ! element 'UMOD'
          !
          call sbuff_checksize(mmax*nmaxus)
          if (associated(umod)) then
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(umod(nm),sp)
                enddo
             enddo
          else
             sbuff(1:mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'UMOD', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%zumod) then
          !
          ! element 'ZUMOD'
          !
          call sbuff_checksize(mmax*nmaxus)
          if (associated(zumod)) then
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(zumod(nm),sp)
                enddo
             enddo
          else
             sbuff(1:mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'ZUMOD', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%ustar) then
          !
          ! element 'USTAR'
          !
          call sbuff_checksize(mmax*nmaxus)
          if (associated(ust2)) then
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(sqrt(ust2(nm)),sp)
                enddo
             enddo
          else
             sbuff(1:mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'USTAR', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%sbcuv) then
          !
          ! element 'SBCU'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbcu)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbcu(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBCU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SBCV'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbcv)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbcv(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBCV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%sbcuuvv) then
          !
          ! element 'SBCUU'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbcuu)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbcuu(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBCUU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SBCVV'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbcvv)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbcvv(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBCVV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%sbwuv) then
          !
          ! element 'SBWU'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbwu)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbwu(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBWU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SBWV'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbwv)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbwv(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBWV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%sbwuuvv) then
          !
          ! element 'SBWUU'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbwuu)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbwuu(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBWUU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SBWVV'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sbwvv)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sbwvv(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SBWVV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%sswuv) then
          !
          ! element 'SSWU'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sswu)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sswu(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SSWU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SSWV'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sswv)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sswv(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SSWV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%sswuuvv) then
          !
          ! element 'SSWUU'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sswuu)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sswuu(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SSWUU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SSWVV'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(sswvv)) then
             i = 0
             do l = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(l)
                case (2)
                   rhol = rhosol(l)
                end select
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sswvv(nm,l)/rhol,sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SSWVV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'SBUU'
       !
       call sbuff_checksize(lsedtot*mmax*nmaxus)
       i = 0
       do l = 1, lsedtot
          select case(moroutput%transptype)
          case (0)
             rhol = 1.0_fp
          case (1)
             rhol = cdryb(l)
          case (2)
             rhol = rhosol(l)
          end select
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                sbuff(i) = real(sbuu(n, m, l)/rhol,sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'SBUU', uindex, 1, sbuff)
       if (ierror/=0) goto 9999
       !
       ! element 'SBVV'
       !
       call sbuff_checksize(lsedtot*mmax*nmaxus)
       i = 0
       do l = 1, lsedtot
          select case(moroutput%transptype)
          case (0)
             rhol = 1.0_fp
          case (1)
             rhol = cdryb(l)
          case (2)
             rhol = rhosol(l)
          end select
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                sbuff(i) = real(sbvv(n, m, l)/rhol,sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'SBVV', uindex, 1, sbuff)
       if (ierror/=0) goto 9999
       !
       ! element 'SSUU'
       !
       call sbuff_checksize(lsed*mmax*nmaxus)
       i = 0
       do l = 1, lsed
          select case(moroutput%transptype)
          case (0)
             rhol = 1.0_fp
          case (1)
             rhol = cdryb(l)
          case (2)
             rhol = rhosol(l)
          end select
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                sbuff(i) = real(ssuu(n, m, l)/rhol,sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'SSUU', uindex, 1, sbuff)
       if (ierror/=0) goto 9999
       !
       ! element 'SSVV'
       !
       call sbuff_checksize(lsed*mmax*nmaxus)
       i = 0
       do l = 1, lsed
          select case(moroutput%transptype)
          case (0)
             rhol = 1.0_fp
          case (1)
             rhol = cdryb(l)
          case (2)
             rhol = rhosol(l)
          end select
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                sbuff(i) = real(ssvv(n, m, l)/rhol,sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'SSVV', uindex, 1, sbuff)
       if (ierror/=0) goto 9999
       !
       ! element 'SUCOR'
       !
       if (moroutput%suvcor) then
          call sbuff_checksize(lsed*mmax*nmaxus)
          i = 0
          do l = 1, lsed
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(sucor(nm, l)/rhol,sp)
                enddo
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'SUCOR', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SVCOR'
          !
          call sbuff_checksize(lsed*mmax*nmaxus)
          i = 0
          do l = 1, lsed
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(svcor(nm, l)/rhol,sp)
                enddo
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'SVCOR', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'SOURSE'
       !
       if (moroutput%sourcesink) then
          call sbuff_checksize(lsed*mmax*nmaxus)
          if (associated(sourse)) then
             i = 0
             do l = 1, lsed
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sourse(nm, l),sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsed*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SOURSE', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'SINKSE'
          !
          call sbuff_checksize(lsed*mmax*nmaxus)
          if (associated(sinkse)) then
             i = 0
             do l = 1, lsed
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(sinkse(nm, l),sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'SINKSE', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'RCA'
       !
       call sbuff_checksize(lsed*mmax*nmaxus)
       i = 0
       do l = 1, lsed
          do m = 1, mmax
             do n = 1, nmaxus
                !
                ! near-bed reference concentration of sediment
                !
                i        = i+1
                sbuff(i) = real(rca(n, m, l),sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'RCA', uindex, 1, sbuff)
       if (ierror/=0) goto 9999
       !
       ! element 'DPS'
       !
       call sbuff_checksize(mmax*nmaxus)
       i = 0
       do m = 1, mmax
          do n = 1, nmaxus
             i        = i+1
             sbuff(i) = real(dps(n, m),sp)
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'DPS', uindex, 1, sbuff)
       if (ierror/=0) goto 9999
       !
       if (moroutput%dzduuvv) then
          !
          ! element 'DZDUU'
          !
          call sbuff_checksize(mmax*nmaxus)
          if (associated(dzduu)) then
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(dzduu(nm),sp)
                enddo
             enddo
          else
             sbuff(1:mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'DZDUU', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'DZDVV'
          !
          call sbuff_checksize(mmax*nmaxus)
          if (associated(dzdvv)) then
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(dzdvv(nm),sp)
                enddo
             enddo
          else
             sbuff(1:mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'DZDVV', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (scour) then
          !
          ! element 'TAUADD'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                call shearx(tauadd, nm, gdp)
                sbuff(i) = real(tauadd,sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'TAUADD', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%taurat) then
          !
          ! element 'TAURAT'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(taurat)) then
             i = 0
             do l = 1, lsedtot
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(taurat(nm,l),sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'TAURAT', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'DM'
       !
       if (moroutput%dm) then
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(dm(nm),sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'DM', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'DG'
       !
       if (moroutput%dg) then
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(dg(nm),sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'DG', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       ! element 'DGSD'
       !
       if (moroutput%dgsd) then
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(dgsd(nm),sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'DGSD', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%percentiles) then
          call sbuff_checksize(mmax*nmaxus)
          do l = 1, nxx
             write(dxname,'(A,I2.2)') 'DXX',l
             i = 0
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(dxx(nm,l),sp)
                enddo
             enddo
             ierror = putelt(fds, grpnam, dxname, uindex, 1, sbuff)
             if (ierror/=0) goto 9999
          enddo
       endif
       !
       if (moroutput%frac) then
          !
          ! element 'FRAC'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          i = 0
          do l = 1, lsedtot
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(frac(nm,l),sp)
                enddo
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'FRAC', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%mudfrac) then
          !
          ! element 'MUDFRAC'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(mudfrac(nm),sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'MUDFRAC', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%sandfrac) then
          !
          ! element 'SANDFRAC'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(sandfrac(nm),sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'SANDFRAC', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif       
       !
       if (moroutput%fixfac) then
          !
          ! element 'FIXFAC'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          if (associated(fixfac)) then
             i = 0
             do l = 1, lsedtot
                do m = 1, mmax
                   do n = 1, nmaxus
                      i        = i+1
                      call n_and_m_to_nm(n, m, nm, gdp)
                      sbuff(i) = real(fixfac(nm,l),sp)
                   enddo
                enddo
             enddo
          else
             sbuff(1:lsedtot*mmax*nmaxus) = -999.0
          endif
          ierror = putelt(fds, grpnam, 'FIXFAC', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       if (moroutput%hidexp) then
          !
          ! element 'HIDEXP'
          !
          call sbuff_checksize(lsedtot*mmax*nmaxus)
          i = 0
          do l = 1, lsedtot
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(hidexp(nm, l),sp)
                enddo
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'HIDEXP', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
       !
       ! Add mor fields
       !
       call wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & 2         ,fds       ,grpnam    ,gdp       )
       if (error) goto 9999
       !
       ! write errormessage if error occurred and set error = .true.
       ! the files will be closed in clsnef (called in triend)
       !
 9999  continue
       if (ierror/= 0) then
          ierror = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error = .true.
       endif
    endselect
end subroutine wrsedm
