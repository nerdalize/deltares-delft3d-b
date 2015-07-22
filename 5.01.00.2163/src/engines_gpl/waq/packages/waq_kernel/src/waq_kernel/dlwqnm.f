!!  Copyright(C) Stichting Deltares, 2012.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine dlwqnm ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Self adjusting theta method (21 & 22)
!>
!>                         Performs time dependent integration upwind in space,
!>                         implicit in time using the GMRES method with Krilov
!>                         orthonormal subspace vectors.
!>                         Method adjust each time step for each cell the theta.
!>                         Is implemented as:
!>                            - method 21 with the flux limiter of Salezac
!>                            - method 22 with the flux limiter of Boris and Book

!     Created            :          2007 by Paulien van Slingerland

!     Modified           : July      2009 Leo Postma Double precission version
!                          August    2010 Leo Postma OMP parallel version
!                          January   2011 Leo Postma Boris and Book limiter added

c     logical units      : lun(19) , output, monitoring file
c                          lun(20) , output, formatted dump file
c                          lun(21) , output, unformatted hist. file
c                          lun(22) , output, unformatted dump file
c                          lun(23) , output, unformatted dump file
c
c     subroutines called : dlwqtr, user transport routine
c                          dlwqwq, user waterquality routine
c                          proces, delwaq proces system
c                          dlwqo2, delwaq output system
c                          dlwqpp, user postprocessing routine
c                          dlwq13, system postpro-dump routine
c                          dlwq14, scales waterquality
c                          dlwq15, wasteload routine
c                          dlwq17, boundary routine
c                          dlwq41, updates volumes
c                          dlwqt0, updates other time dependent items
c                          dlwq62, adds transport to matrix and rhs
c                          dlwqb3, computes volumes
c                          dlwqb4, computation of mass array
c                          dlwqb5, performs mass balance computation
c                          dlwqb6, updates right hand side
c                          dlwqb7, adds open boundaries to deriv
c                          dlwqb8, restores conc array
c                          dlwqf1, initializes matrix pointer administration
c                          dlwql1, computes variable theta coefficients
c                          dlwql2, fills matrix
c                          dlwql3, sets (scaled) rhs of system of equations
c                          move,   copies one array to another
c                          proint, integration of fluxes
c                          dhopnf, opens files
c                          sgmres, solves (iteratively) system of equations
c                          zercum, zero's the cummulative array's

      use grids
      use timers
      use waqmem                         ! Global memory with allocatable GMRES arrays
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress

      implicit none

      include 'actions.inc'

!     Declaration of arguments

      real                        :: a     (*)  !< real linear workspace array
      integer                     :: j     (*)  !< integer linear workspace array
      integer                     :: lun   (*)  !< file unit numbers
      character(*)                :: c     (*)  !< character linear workspace array
      character(*)                :: lchar (*)  !< file names
      integer                     :: action     !< handle to stepwise call
      type(delwaq_data), target   :: dlwqd      !< data structure stepwize call
      type(GridPointerColl)       :: GridPs     !< collection of all grid definitions

!$    include "omp_lib.h"

!     common  /  sysn   /   system characteristics
      include 'sysn.inc'

!     common  /  sysi  /    timer characteristics
      include 'sysi.inc'

!     common  /  sysa   /   pointers in real array workspace
      include 'sysa.inc'

!     common  /  sysj   /   pointers in integer array workspace
      include 'sysj.inc'

!     common  /  sysc   /   pointers in character array workspace
      include 'sysc.inc'

!     common to define external communications in SOBEK
!     olcfwq             flag indicating ONLINE running of CF and WQ
!     srwact             flag indicating active data exchange with SRW
!     rtcact             flag indicating output for RTC

      logical            olcfwq, srwact, rtcact
      common /commun/    olcfwq, srwact, rtcact

! local declarations

      real            rdummy(1)
      logical         imflag , idflag , ihflag
      logical         updatr , update , lstrec , lrewin
      logical         litrep , ldummy,  timon_old
      real(8)         tol
      integer         laatst

      logical         antidiffusion

      integer       :: ithandl
      integer, save :: ithand1 = 0 ! Leave local

      integer         itime
      integer         itimel
      integer         ifflag
      integer         iaflag
      integer         ibflag
      integer         nddim
      integer         nvdim
      integer         noqt
      integer         nosss
      integer         noqtt
      integer         nopred
      integer         inwtyp
      integer         isys
      integer         nstep

      integer         noth
      integer         ith

      integer         ibnd

!       Variables specific to this method: leave them SAVEd

      integer, save          :: ioptpc
      integer, save          :: iter
      integer, save          :: iscale


!       Dummy variables - used in DLWQD

      logical                :: forester
      integer                :: nowarn
      integer                :: lleng
      integer                :: ioptzb

      include 'state_data.inc'

c     special remarks    : mass-array is used for rhs vector!!
c
c     this option is a mix of option 1 (discretization of transport
c     in space) and option 6 (matrix inversion to perform implicit
c     integration in time.
c     the processes part is integrated explicitly, in order to allow
c     for any complexity of the processes.
c     strictly speaking, a loop over substances should be added
c     to anticipate this, the method uses an
c     extra volume-array (ivol2), and uses the amass-array (imass)
c     for the rhs-matrix, instead of the deriv-array as in method 6.
c     (jvg, may 8 1992)
c
c     this option implements:
c     1) euler backward time integration
c     2) upwind differences for advection
c     3) central differences for diffusion
c     the resulting systems of equations are solved by an iterative
c     solution method (gmres).
c     with such an iterative method, systems with multiple rhs cannot be solved
c     (simultaneously). so we loop over the substances and solve each system
c     individually. so rhs can be reduced to an real array of size noseg+nobnd.
c
c     possible improvements:
c
c     - use fgmres instead of gmres for solving system of equations.
c       this makes it possible to keep search directions which have already
c       been computed in previous fgmres calls and hence find the solution
c       of new systems at lower costs!
c
c     - tune the preconditioner to speed up the iteration process.
c       only gaus-seidel, and ssor preconditioning has been implemented yet.
c
c     - integrate processes in an implicit way as well. enables users to
c       potentially take larger time steps (better stability properties)
c       or even compute steady states in "one time step" (the latter subject
c       to constraint that proces formulation is time independent).
c       implicit time integration of processes requires the inexact newton
c       solution method described in:
c
c       "delwaq fastsolver ii"
c       newton-krylov methods for solving linear and non-linear equations
c       report t1596, january 1996, Deltares
c                                                              (kht, 13/11/96)

      if ( action == ACTION_FINALISATION ) then
          include 'dlwqdata_restore.inc'
          goto 50
      endif

      if ( action == ACTION_INITIALISATION  .or.
     &     action == ACTION_FULLCOMPUTATION        ) then

c        some initialisation
c        ioptpc = preconditioner switch [0 = none, 1 = gs (l), 2 = gs (u),
c        3 = ssor], iter = maximum number of iterations [ > 0],
c        tol = relative tolerance [10^-3, 10^-10], iscale = row scaling
c        of system of equations [0 = no, 1 =yes], klat = number of
c        layers in preconditioner [1,kmax]

          call dlwqf5 ( lun(19) , nocons  , c(icnam), a(icons), ioptpc  ,
     &                  iter    , tol     , iscale  , litrep  , noseg   ,
     &                  noq3    , noq     , nobnd   , novec   , nomat   ,
     &                  nolay   , intsrt  , intopt  )

          ithandl = 0
          itime   = itstrt
          nstep   = (itstop-itstrt)/idt
          ifflag  = 0
          iaflag  = 0
          ibflag  = 0
          if ( mod(intopt,16) .ge. 8 ) ibflag = 1
          if ( ndspn .eq. 0 ) then
             nddim = nodisp
          else
             nddim = ndspn
          endif
          if ( nveln .eq. 0 ) then
             nvdim = novelo
          else
             nvdim = nveln
          endif
          updatr  = .true.
          lstrec = icflag .eq. 1
          nosss  = noseg + nseg2
          noqtt  = noq + noq4
          inwtyp = intyp + nobnd

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )

! initialize second volume array with the first one
          call move   ( a(ivol ), a(ivol2) , nosss   )

! initialize pointer matices for fast solvers
          call dlwqf1 ( noseg   , nobnd   , noq     , noq1    , noq2    ,
     &                  nomat   , j(ixpnt), j(iwrk) , j(imat) , rowpnt  ,
     &                  fmat    , tmat    )
      endif

!
!     Save/restore the local persistent variables,
!     if the computation is split up in steps
!
!     Note: the handle to the timer (ithandl) needs to be
!     properly initialised and restored
!
      if ( action == ACTION_INITIALISATION ) then
          if ( timon ) call timstrt ( "dlwqnm", ithandl )
          include 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          return
      endif

      if ( action == ACTION_SINGLESTEP ) then
          include 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      endif

      antidiffusion = btest(intopt,16)

      if ( timon ) call timstrt ( "dlwqnm", ithandl )

!======================= simulation loop ============================
   10 continue

!     Determine the volumes and areas that ran dry at start of time step

         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , nopa     ,
     &                 c(ipnam) , a(iparm) , nosfun   , c(isfna) , a(isfun) ,
     &                 j(iknmr) , iknmkv   )

!     user transport processes
         update = updatr
         call dlwqtr ( notot   , nosys   , noseg   , noq     , noq1    ,
     &                 noq2    , noq3    , nopa    , nosfun  , nodisp  ,
     &                 novelo  , j(ixpnt), a(ivol) , a(iarea), a(iflow),
     &                 a(ileng), a(iconc), a(idisp), a(icons), a(iparm),
     &                 a(ifunc), a(isfun), a(idiff), a(ivelo), itime   ,
     &                 idt     , c(isnam), nocons  , nofun   , c(icnam),
     &                 c(ipnam), c(ifnam), c(isfna), update  , ilflag  ,
     &                 npartp  )
         if ( update ) updatr = .true.

! jvb
!     temporary ? set the variables grid-setting for the delwaq variables
         call setset ( lun(19), nocons, nopa  , nofun   , nosfun,
     &                 nosys  , notot , nodisp, novelo  , nodef ,
     &                 noloc  , ndspx , nvelx , nlocx   , nflux ,
     &                 nopred , novar , nogrid, j(ivset)        )

! jvb
!     call proces subsystem
         call hsurf  ( nosys   , notot   , noseg   , nopa    , c(ipnam),
     +              a(iparm), nosfun  , c(isfna), a(isfun), surface ,
     +              lun(19) )
         call proces ( notot   , noseg   , a(iconc), a(ivol) , itime   ,
     &                 idt     , a(iderv), ndmpar  , nproc   , nflux   ,
     &                 j(iipms), j(insva), j(iimod), j(iiflu), j(iipss),
     &                 a(iflux), a(iflxd), a(istoc), ibflag  , ipbloo  ,
     &                 ipchar  , ioffbl  , ioffch  , a(imass), nosys   ,
     &                 itfact  , a(imas2), iaflag  , intopt  , a(iflxi),
     &                 j(ixpnt), iknmkv  , noq1    , noq2    , noq3    ,
     &                 noq4    , ndspn   , j(idpnw), a(idnew), nodisp  ,
     &                 j(idpnt), a(idiff), ndspx   , a(idspx), a(idsto),
     &                 nveln   , j(ivpnw), a(ivnew), novelo  , j(ivpnt),
     &                 a(ivelo), nvelx   , a(ivelx), a(ivsto), a(idmps),
     &                 j(isdmp), j(ipdmp), ntdmpq  , a(idefa), j(ipndt),
     &                 j(ipgrd), j(ipvar), j(iptyp), j(ivarr), j(ividx),
     &                 j(ivtda), j(ivdag), j(ivtag), j(ivagg), j(iapoi),
     &                 j(iaknd), j(iadm1), j(iadm2), j(ivset), j(ignos),
     &                 j(igseg), novar   , a       , nogrid  , ndmps   ,
     &                 c(iprna), intsrt  , j(iowns), j(iownq), mypart  ,
     &                 j(iprvpt), j(iprdon), nrref , j(ipror), nodef   ,
     &                 surface  ,lun(19) )

!     add user defined routine
         call dlwqwq ( notot   , nosys   , noseg   , nopa    , nosfun  ,
     &                 a(ivol) , a(iconc), a(icons), a(iparm), a(ifunc),
     &                 a(isfun), a(iderv), itime   , idt     , a(ismas),
     &                 ibflag  , c(isnam), nocons  , nofun   , c(icnam),
     &                 c(ipnam), c(ifnam), c(isfna), nodump  , j(idump))

!     communicate boundaries
         call dlwq_boundio( lun  (19), notot   , nosys   , noseg   , nobnd   ,
     &                      c(isnam) , c(ibnid), j(ibpnt), a(iconc), a(ibset),
     &                      lchar(19))

!     set new boundaries
         if ( itime .ge. 0   ) then
             ! first: adjust boundaries by OpenDA
             if ( dlwqd%inopenda ) then
                 do ibnd = 1,nobnd
                     do isys = 1,nosys
                         call get_openda_buffer(isys,ibnd, 1,1,
     &                                   A(ibset+(ibnd-1)*nosys + isys-1))
                     enddo
                 enddo
             endif

            call dlwq17 ( a(ibset), a(ibsav), j(ibpnt), nobnd   , nosys   ,
     &                    notot   , idt     , a(iconc), a(iflow), a(iboun))
         endif

!     call output system
         call dlwqo2 ( notot   , noseg   , nopa    , nosfun  , itime   ,
     &                 c(imnam), c(isnam), c(idnam), j(idump), nodump  ,
     &                 a(iconc), a(icons), a(iparm), a(ifunc), a(isfun),
     &                 a(ivol) , nocons  , nofun   , idt     , noutp   ,
     &                 lchar   , lun     , j(iiout), j(iiopo), a(iriob),
     &                 c(ionam), nx      , ny      , j(igrid), c(iedit),
     &                 nosys   , a(iboun), j(ilp)  , a(imass), a(imas2),
     &                 a(ismas), nflux   , a(iflxi), isflag  , iaflag  ,
     &                 ibflag  , imstrt  , imstop  , imstep  , idstrt  ,
     &                 idstop  , idstep  , ihstrt  , ihstop  , ihstep  ,
     &                 imflag  , idflag  , ihflag  , noloc   , a(iploc),
     &                 nodef   , a(idefa), itstrt  , itstop  , ndmpar  ,
     &                 c(idana), ndmpq   , ndmps   , j(iqdmp), j(isdmp),
     &                 j(ipdmp), a(idmpq), a(idmps), a(iflxd), ntdmpq  ,
     &                 c(icbuf), noraai  , ntraaq  , j(ioraa), j(nqraa),
     &                 j(iqraa), a(itrra), c(irnam), a(istoc), nogrid  ,
     &                 novar   , j(ivarr), j(ividx), j(ivtda), j(ivdag),
     &                 j(iaknd), j(iapoi), j(iadm1), j(iadm2), j(ivset),
     &                 j(ignos), j(igseg), a       , nobnd   , nobtyp  ,
     &                 c(ibtyp), j(intyp), c(icnam), noq     , j(ixpnt),
     &                 intopt  , c(ipnam), c(ifnam), c(isfna), j(idmpb),
     &                 nowst   , nowtyp  , c(iwtyp), j(iwast), j(inwtyp),
     &                 a(iwdmp), iknmkv  , j(iowns), mypart  )

! zero cumulative arrays
         if ( imflag .or. ( ihflag .and. noraai .gt. 0 ) ) then
            call zercum ( notot   , nosys   , nflux   , ndmpar  , ndmpq   ,
     &                    ndmps   , a(ismas), a(iflxi), a(imas2), a(iflxd),
     &                    a(idmpq), a(idmps), noraai  , imflag  , ihflag  ,
     &                    a(itrra), ibflag  , nowst   , a(iwdmp))
         endif

! progress file
         call write_progress( dlwqd%progress )

!     simulation done ?
         if ( itime .lt. 0      ) goto 9999
         if ( itime .ge. itstop ) goto 50

         call delpar01 ( itime   , noseg   , noq     , a(ivol) , a(iflow),
     &                   nosfun  , c(isfna), a(isfun))

!     restore conc-array from mass array
         call dlwqb8 ( nosys   , notot   , noseg   , a(ivol ), a(imass),
     &                 a(iconc), nopa    , c(ipnam), a(iparm), nosfun  ,
     &                 c(isfna), a(isfun))

!     add processes
         call dlwq14 ( a(iderv), notot , noseg   , itfact, a(imas2),
     &                 idt     , iaflag, a(idmps), intopt, j(isdmp),
     &                 j(iowns), mypart)

!     get new volumes
         itimel = itime
         itime  = itime + idt
         if ( ivflag .eq. 1 ) then     ! computation of volumes for computed volumes only
            call move   ( a(ivol) , a(ivol2), noseg   )
            call dlwqb3 ( a(iarea), a(iflow), a(ivnew), j(ixpnt), notot   ,
     &                    noq     , nvdim   , j(ivpnw), a(ivol2), intopt  ,
     &                    a(imas2), idt     , iaflag  , nosys   , a(idmpq),
     &                    ndmpq   , j(iqdmp))
            updatr = .true.
         else                          ! read new volumes from files
            call dlwq41 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                    j(inrha), j(inrh2), j(inrft), noseg   , a(ivol2),
     &                    j(ibulk), lchar   , ftype   , isflag  , ivflag  ,
     &                    update  , j(inisp), a(inrsp), j(intyp), j(iwork),
     &                    lstrec  , lrewin  , a(ivoll), mypart  , dlwqd   )
            if ( update ) updatr = .true.
         endif

!     update the info on dry volumes with the new volumes
         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , nopa     , c(ipnam) , a(iparm) ,
     &                 nosfun   , c(isfna) , a(isfun) , j(iknmr) , iknmkv   )

! add the waste loads

         call dlwq15 ( nosys     , notot    , noseg    , noq      , nowst    ,
     &                 nowtyp    , ndmps    , intopt   , idt      , itime    ,
     &                 iaflag    , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &                 a(iflow ) , j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &                 j(inwtyp) , j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &                 iknmkv    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna ) , a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &                 a(iwdmp)  , 1        , notot    , j(iowns ), mypart   )

!          Here we implement a loop that inverts the same matrix
!          for series of subsequent substances having the same
!          additional VELO and DISPER array. (JvG, April 24, 1993).

!          In solving equations with multiple rhs, the *FULL* fast (or should
!          I say slow?) solver algorithm needs to be applied to each rhs vector
!          So DELMAT may outperform FS when we deal with a large number of
!          substances with the same additional velocity and dispersion field
!          in future we need a smart switch between DELMAT and FS at this place
!          For now always do FS!
!                                                               (KHT, 11/11/96)

         call dlwqm7 ( noq     , noq1    , noq2    , a(iarea), a(iflow),
     &                 a(ileng), ilflag  , intopt  , j(ixpnt), mixlen  ,
     &                 iknmkv  )

         if ( timon ) call timstrt ( "ADE solver", ithand1 )
         timon_old = timon
         noth = OMP_GET_NUM_THREADS()
         if ( noth .gt. 1 ) timon = .false.
!$OMP PARALLEL
!$OMP DO PRIVATE(ith)
! start of loop over substances

      do 40 isys = 1, nosys

         ith = OMP_GET_THREAD_NUM()+1

!     make flow and dispersion arrays
         call dlwqm0 ( isys          , nosys          , noq           , noq1          , noq2          ,
     &                 a(iarea)      , a(iflow)       , flowtot(1,ith), nvdim         , j(ivpnw)      ,
     &                 a(ivnew)      , a(idisp)       , disptot(1,ith), nddim         , j(idpnw)      ,
     &                 a(idnew)      , mixlen         )

!     compute variable theta coefficients
         call dlwqm1 ( idt           , noseg          , nobnd         , a(ivol)       , noq           ,
     &                 noq1          , noq2           , j(ixpnt)      , flowtot(1,ith), disptot(1,ith),
     &                 theta(1,ith)  , thetaseg(1,ith), antidiffusion , iexseg (1,ith))

         if ( isys .eq. 1 ) call dlwq_output_theta (nrvart  , c(ionam), j(iiopo)       , nocons, nopa ,
     &                                              nofun   , nosfun  , notot          , noseg , noloc,
     &                                              a(iploc), nodef   , thetaseg(1,ith))

!     construct matrix
         call dlwqm2 ( idt           , noseg          , a(ivol2)      , nobnd         , noq           ,
     &                 j(ixpnt)      , flowtot(1,ith) , disptot(1,ith), theta(1,ith)  , gm_diag(1,ith),
     &                 iscale        , gm_diac(1,ith) , nomat         , gm_amat(1,ith), rowpnt        ,
     &                 fmat          , tmat           , iexseg (1,ith))

!     construct rhs
         call dlwqm3 ( idt           , isys           , nosys         , notot         , noseg         ,
     &                 a(iconc)      , a(iderv)       , a(ivol)       , nobnd         , a(iboun)      ,
     &                 noq           , j(ixpnt)       , flowtot(1,ith), disptot(1,ith), theta(1,ith)  ,
     &                 gm_diac(1,ith), iscale         , gm_rhs (1,ith), gm_sol(1,ith) )

!     solve linear system of equations by means of gmres to obtain local theta solution estimation
         call sgmres ( noseg+nobnd   , gm_rhs (1,ith) , gm_sol(1,ith) , novec         , gm_work(1,ith),
     &                 noseg+nobnd   , gm_hess(1,ith) , novec+1       , iter          , tol           ,
     &                 nomat         , gm_amat(1,ith) , j(imat)       , gm_diag(1,ith), rowpnt        ,
     &                 nolay         , ioptpc         , nobnd         , gm_trid(1,ith), iexseg (1,ith),
     &                 lun(19)       , litrep        )

!     mass balance of transport
         call dlwqm4 ( isys          , nosys          , notot         , noseg         , a(iconc)      ,
     &                 gm_sol(1,ith) , nobnd          , a(iboun)      , noq           , j(ixpnt)      ,
     &                 theta (1,ith) , flowtot(1,ith) , disptot(1,ith), a(imas2)      , ndmpq         ,
     &                 j(iqdmp)      , a(idmpq)       , idt           )

!     apply flux corrected transport to obtain the local theta fct solution estimation
         if ( intsrt .eq. 21 )      ! Flux correction according to Salezac  (Pauline)
     &   call dlwqm5 ( idt           , isys           , nosys         , notot         , noseg         ,
     &                 a(iconc)      , gm_sol(1,ith)  , a(ivol2)      , nobnd         , a(iboun)      ,
     &                 noq           , noq1           , noq2          , noq3          , j(ixpnt)      ,
     &                 iknmkv        , a(iarea)       , a(ileng)      , theta(1,ith)  , flowtot(1,ith),
     &                 disptot(1,ith), intopt         , a(imas2)      , ndmpq         , j(iqdmp)      ,
     &                 a(idmpq)      , flux(1,ith)    , lim(1,ith)    , maxi (1,ith)  , mini   (1,ith),
     &                 l1(1,ith)     , l2  (1,ith)    , m1 (1,ith)    , m2   (1,ith)  , n1     (1,ith),
     &                 n2(1,ith)     )
         if ( intsrt .eq. 22 )      ! Flux correction according to Boris and Book  (Leo)
     &   call dlwqm8 ( idt           , isys           , nosys         , notot         , noseg         ,
     &                 a(iconc)      , gm_sol (1,ith) , a(ivol2)      , nobnd         , a(iboun)      ,
     &                 noq           , iknmkv         , j(ixpnt)      , a(iarea)      , a(ileng)      ,
     &                 theta(1,ith)  , flowtot(1,ith) , intopt        , a(imas2)      , ndmpq         ,
     &                 j(iqdmp)      , a(idmpq)       )

   40 continue
!$OMP ENDDO
!$OMP ENDPARALLEL
      if ( noth .gt. 1 ) timon = timon_old

      if ( timon ) call timstop ( ithand1 )

! update mass array, explicit step for passive substances

         call dlwqb4 ( nosys   , notot   , noseg   , a(ivol2), a(imass),
     &                 a(iconc), a(iderv), nopa    , c(ipnam), a(iparm),
     &                 nosfun  , c(isfna), a(isfun), idt     )

! replace old by new volumes
         call move   ( a(ivol2), a(ivol) , noseg )

!     calculate closure error
         if ( lrewin .and. lstrec ) then
            call dlwqce ( a(imass), a(ivoll), a(ivol2), nosys, notot,
     &                    noseg   , lun(19)                         )
            call move   ( a(ivoll), a(ivol) , noseg )
         endif

!     integrate the fluxes at dump segments fill asmass with mass
         if ( ibflag .gt. 0 ) then
            call proint ( nflux   , ndmpar  , idt     , itfact, a(iflxd),
     &                    a(iflxi), j(isdmp), j(ipdmp), ntdmpq          )
         endif

      if ( rtcact ) call rtcshl (itime, a, j, c) ! Interface to RTC (i)
      if ( srwact ) call srwshl (itime, a, j, c) ! Interface to SRW (i)

      if ( olcfwq ) then
          call putpcf('wqtocf','datawqtocf')
          if ( itime+idt .lt. itstop ) then
              call getpcf('cftowq','datacftowq')
              laatst = 0
          else
              laatst = -1
          endif
      endif

!     new time values, volumes excluded
         if ( olcfwq .or. srwact ) then
            call putpev ( 'WQtoWQI', 'DataWQtoWQI', laatst )
            call getper ( 'WQItoWQ', 'DataWQItoWQ' )
         endif

!     update all other time functions
         call dlwqt0 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                 j(inrha), j(inrh2), j(inrft), idt     , a(ivol) ,
     &                 a(idiff), a(iarea), a(iflow), a(ivelo), a(ileng),
     &                 a(iwste), a(ibset), a(icons), a(iparm), a(ifunc),
     &                 a(isfun), j(ibulk), lchar   , c(ilunt), ftype   ,
     &                 intsrt  , isflag  , ifflag  , ivflag  , ilflag  ,
     &                 update  , j(iktim), j(iknmr), j(inisp), a(inrsp),
     &                 j(intyp), j(iwork), .false. , ldummy  , rdummy  ,
     &                 .false. , gridps  , dlwqd   )
         if ( update ) updatr = .true.

!     end of time loop
         if ( action == ACTION_FULLCOMPUTATION ) then
            goto 10
         endif

   50 continue

      if ( action == ACTION_FINALISATION    .or.
     &     action == ACTION_FULLCOMPUTATION      ) then

! close files, except monitor file

          call CloseHydroFiles( dlwqd%collcoll )
          call close_files( lun )

! write restart file
          call dlwq13 ( lun     , lchar , a(iconc) , itime , c(imnam) ,
     &                  c(isnam), notot , noseg    )
      endif

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      return

      end subroutine dlwqnm
