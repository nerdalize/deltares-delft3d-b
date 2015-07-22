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

      subroutine dlwqnf ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Upwind discretisation, implicit in time, GMRES solver (15)
!>
!>                         Performs time dependent integration. Upwind advection, implicit
!>                         in time. Uses the GMRES iterative solver with Krilov
!>                         orthonormal subspaces vectors.\n

!     Created            : April     1992 by Jos van Gils

!     Modified           : September 1996 Leo Postma Fast solver data structure
!                             up to       Kian Tan   Fast solver GMRES method
!                          November  1996 Robert Vos Fast solver enhancements
!                          July      2009 Leo Postma Double precission version
!                          August    2010 Leo Postma OMP parallel version

!     Logical units      : lun(19) , output, monitoring file
!                          lun(20) , output, formatted dump file
!                          lun(21) , output, unformatted hist. file
!                          lun(22) , output, unformatted dump file
!                          lun(23) , output, unformatted dump file

!     Subroutines called in their order of appearance in the code:
!                          dlwqf5: sets the numerical parameters for GMRES and reports to monitoring file
!                          move  : copies one array to another (usefull for ranges in the big array)
!                          dlwqf1: initializes matrix pointer administration fast solver
!     VVV                  dryfld: changes the property array for drying and flooding of start volume
!      V                   dlwqtr: user transport routine (a value in delwaq2 dir. is used for reasons)
!      |                   setset: not so clear what this does, probably process related
!      |                   hsurf : set the surface array from the proces parameters
!      |                   proces: DELWAQ water quality process system
!      |                   dlwqwq: user linkeable waterquality routine, generally not used
!      |                   dlwq_boundio: interface to on line boundary provision in bigger systems
!      |                   dlwqo2: DELWAQ output system, provides all output to files
!    time  ===> jump out   zercum: zero's the cummulative array's of balances and monitoring areas
!    loop        point     dlwqb8: restores conc array (in case other routines did distroy ?!?)
!      |                   dlwq14: scales waterquality derivative according to right time step size
!      |                   dlwqb3: computes new volumes in case 'computed volumes' was switched 'on'
!      |                   dlwq41: updates new volumes from file/core in case 'comp vols' was 'off'
!      |                   dryfle: does the same as dryfld now taking into account the last volumes
!      |                   dlwq15: updates derivative with all specified wasteload forcing
!      |                   dlwqm7: makes mixleng once for all substances based on area/(leng1+leng2)
!      |            V      dlwqm0: makes flowtot and disptot for the substance in the substance loop
!      |      paralellized dlwqf3: fills the sparse transport matrix, scales with diagonal value
!      |       substances  dlwqf4: sets the (scaled) right-hand-side of the equations
!      |          loop     sgmres: solves (iteratively) the system of equations
!      |            V      dlwqf7: copies solution in the conc. array updates mass balance arrays
!      |                   dlwqb4: update mass arrays, set explicit step for all passive substances
!      |                   dlwqce: computes closure error correction at rewind of volume file
!      |                   proint: integration of fluxes for mass balances per monitoring area
!      |                   rtcshl: call to the real-time-control interface if switched 'on'
!     VVV                  srwshl: call to the standaard-raamwerk-water interface if switched 'on'
!      V                   dlwqt0: updates all time dependent items (in this case exclusive of volumes)
!                          dlwq13: system dump routine of restart files at the end
!   Not mentioned are the routines: to start and stop the performance timers
!                                   the routines used for stepwise execution within a
!                                   stepwise executing user interface

      use grids
      use timers
      use waqmem                         ! Global memory with allocatable GMRES arrays
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress

      implicit none

      include 'actions.inc'

!     Arguments:

!     Kind                           Name         Description

      real                        :: a     (*)  !< real linear workspace array
      integer                     :: j     (*)  !< integer linear workspace array
      integer                     :: lun   (*)  !< file unit numbers
      character(*)                :: c     (*)  !< character linear workspace array
      character(*)                :: lchar (*)  !< file names
      integer                     :: action     !< handle to stepwise call
      type(delwaq_data), target   :: dlwqd      !< data structure stepwize call
      type(gridpointercoll)       :: gridps     !< collection off all grid definitions

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

!     Common to define external communications in SOBEK
!     olcfwq             Flag indicating ONLINE running of CF and WQ
!     srwact             Flag indicating active data exchange with SRW
!     rtcact             Flag indicating output for RTC

      logical            olcfwq, srwact, rtcact
      common /commun/    olcfwq, srwact, rtcact

!     Local declarations

      real            rdummy(1)
      logical         imflag , idflag , ihflag
      logical         updatr , update , lstrec , lrewin
      logical         litrep , ldummy, timon_old
      real(8)         tol
      integer         laatst

      integer, save :: ithandl
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

      integer         ibnd

      integer         noth
      integer         ith

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

C     SPECIAL REMARKS    : MASS-ARRAY IS USED FOR RHS VECTOR!!
C
C     This option is a mix of option 1 (discretization of transport
C     in space) and option 6 (matrix inversion to perform implicit
C     integration in time.
C     The processes part is integrated EXPLICITLY, in order to allow
C     for any complexity of the processes.
C     Strictly speaking, a loop over substances should be added
C     To anticipate this, the method uses an
C     extra VOLUME-array (IVOL2), and uses the AMASS-array (IMASS)
C     for the rhs-matrix, instead of the DERIV-array as in method 6.
C     (JvG, May 8 1992)
C
C     This option implements:
C     1) Euler backward time integration
C     2) upwind differences for advection
C     3) central differences for diffusion
C     The resulting systems of equations are solved by an iterative
C     solution method (GMRES).
C     With such an iterative method, systems with multiple rhs cannot be solved
C     (simultaneously). So we loop over the substances and solve each system
C     individually. So RHS can be reduced to an REAL array of size NOSEG+NOBND.
C
C     possible improvements:
C
C     - Use FGMRES instead of GMRES for solving system of equations.
C       This makes it possible to keep search directions which have already
C       been computed in previous FGMRES calls and hence find the solution
C       of new systems at lower costs!
C
C     - Tune the preconditioner to speed up the iteration process.
C       Only Gaus-Seidel, and SSOR preconditioning has been implemented yet.
C
C     - Integrate processes in an implicit way as well. Enables users to
C       potentially take larger time steps (better stability properties)
C       or even compute steady states in "one time step" (the latter subject
C       to constraint that proces formulation is time independent).
C       Implicit time integration of processes requires the Inexact Newton
C       solution method described in:
C
C       "DELWAQ FASTSOLVER II"
C       Newton-Krylov methods for solving linear and non-linear equations
C       report T1596, January 1996, Deltares
C                                                              (KHT, 13/11/96)

      if ( action == ACTION_FINALISATION ) then
          include 'dlwqdata_restore.inc'
          goto 50
      endif

      if ( action == ACTION_INITIALISATION  .or.
     &     action == ACTION_FULLCOMPUTATION        ) then

C
C        some initialisation
C        IOPTPC = preconditioner switch [0 = none, 1 = GS (L), 2 = GS (U),
C        3 = SSOR], ITER = maximum number of iterations [ > 0],
C        TOL = relative tolerance [10^-3, 10^-10], ISCALE = row scaling
C        of system of equations [0 = no, 1 =yes], KLAT = number of
C        layers in preconditioner [1,KMAX]
C
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
          lstrec = icflag .eq. 1
          nosss  = noseg + nseg2
          noqtt  = noq + noq4
          inwtyp = intyp + nobnd

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )

!          initialize second volume array with the first one

          call move   ( a(ivol ), a(ivol2) , nosss   )

!      Initialize pointer matices for fast solvers

          call dlwqf1 ( noseg   , nobnd   , noq     , noq1    , noq2    ,
     &                  nomat   , j(ixpnt), j(iwrk) , j(imat) , rowpnt  ,
     &                  fmat    , tmat    )
      endif

C
C     Save/restore the local persistent variables,
C     if the computation is split up in steps
C
C     Note: the handle to the timer (ithandl) needs to be
C     properly initialised and restored
C
      if ( action == ACTION_INITIALISATION ) then
          if ( timon ) call timstrt ( "dlwqnf", ithandl )
          include 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          return
      endif

      if ( action == ACTION_SINGLESTEP ) then
          include 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      endif

      iexseg = 1     !  There is nothing to mask. This array is meant for method 21

      if ( timon ) call timstrt ( "dlwqnf", ithandl )

!======================= simulation loop ============================
   10 continue

!        Determine the volumes and areas that ran dry at start of time step

         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , nopa     ,
     &                 c(ipnam) , a(iparm) , nosfun   , c(isfna) , a(isfun) ,
     &                 j(iknmr) , iknmkv   )

!     mt3d coupling

         call dlwq_mt3d   ( lun  (19), itime   , idt     , itstop  , notot   ,
     &                      nosys    , nosss   , nobnd   , c(isnam), c(ibnid),
     &                      j(ibpnt) , a(iconc), a(ibset), noqtt   , j(ixpnt),
     &                      a(iflow) , ndmpq   , j(iqdmp), a(idmpq))

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
!     Temporary ? set the variables grid-setting for the DELWAQ variables
         call setset ( lun(19), nocons, nopa  , nofun   , nosfun,
     &                 nosys  , notot , nodisp, novelo  , nodef ,
     &                 noloc  , ndspx , nvelx , nlocx   , nflux ,
     &                 nopred , novar , nogrid, j(ivset))

! jvb
!     call PROCES subsystem
         call hsurf  ( nosys   , notot   , noseg   , nopa    , c(ipnam),
     +                 a(iparm), nosfun  , c(isfna), a(isfun), surface ,
     +                 lun(19) )
         call proces ( notot   , nosss   , a(iconc), a(ivol) , itime   ,
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


!          communicate with flow

      call waq2flow(nrvart  , c(ionam), j(iiopo), nocons  , nopa    ,
     +              nofun   , nosfun  , notot   , a(iconc), a(isfun),
     +              a(ifunc), a(iparm), a(icons), idt     , itime   ,
     +              a(ivol) , noseg   , nosys   , nodump  , j(idump),
     +              nx      , ny      , j(igrid), a(iboun), noloc ,
     +              a(iploc), nodef   , a(idefa), lun(19) )


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
     &                                    A(ibset+(ibnd-1)*nosys + isys-1))
                      enddo
                  enddo
              endif
             call dlwq17 ( a(ibset), a(ibsav), j(ibpnt), nobnd   , nosys   ,
     &                     notot   , idt     , a(iconc), a(iflow), a(iboun))
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

!     zero cumulative arrays
         if ( imflag .or. ( ihflag .and. noraai .gt. 0 ) ) then
            call zercum ( notot   , nosys   , nflux   , ndmpar  , ndmpq   ,
     &                    ndmps   , a(ismas), a(iflxi), a(imas2), a(iflxd),
     &                    a(idmpq), a(idmps), noraai  , imflag  , ihflag  ,
     &                    a(itrra), ibflag  , nowst   , a(iwdmp))
         endif

!     progress file
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
         call dlwq14 ( a(iderv), notot   , noseg   , itfact  , a(imas2),
     &                 idt     , iaflag  , a(idmps), intopt  , j(isdmp),
     &                 j(iowns), mypart )

!     get new volumes
         itimel = itime
         itime  = itime + idt
         if ( ivflag .eq. 1 ) then     !     computation of volumes for computed volumes only
            call move   ( a(ivol) , a(ivol2), noseg   )
            call dlwqb3 ( a(iarea), a(iflow), a(ivnew), j(ixpnt), notot   ,
     &                    noq     , nvdim   , j(ivpnw), a(ivol2), intopt  ,
     &                    a(imas2), idt     , iaflag  , nosys   , a(idmpq),
     &                    ndmpq   , j(iqdmp))
            updatr = .true.
         else                          !     read new volumes from files
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

!     add the waste loads
         inwtyp = intyp + nobnd
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
!          In future we need a smart switch between DELMAT and FS at this place
!          For now always do FS!
!                                                               (KHT, 11/11/96)

         call dlwqm7 ( noq     , noq1    , noq2    , a(iarea), a(iflow),
     &                 a(ileng), ilflag  , intopt  , j(ixpnt), mixlen  ,
     &                 iknmkv  )

         if ( timon ) call timstrt ( "ADE solver", ithand1 )
         timon_old = timon
         noth = OMP_GET_NUM_THREADS()
         if ( noth .gt. 1 ) timon = .false.
         call OMP_SET_NUM_THREADS(noth) ! AM: this seems necessary!

!$OMP PARALLEL
!$OMP DO PRIVATE(ith) SCHEDULE(DYNAMIC)
! start of loop over substances

      do 40 isys = 1, nosys

         ith = OMP_GET_THREAD_NUM()+1

!     make flow and dispersion arrays
         call dlwqm0 ( isys          , nosys         , noq           , noq1          , noq2          ,
     &                 a(iarea)      , a(iflow)      , flowtot(1,ith), nvdim         , j(ivpnw)      ,
     &                 a(ivnew)      , a(idisp)      , disptot(1,ith), nddim         , j(idpnw)      ,
     &                 a(idnew)      , mixlen        )

!     do the transport itself, fill matrix, scale diagonal
         call dlwqf3 ( idt           , noseg         , a(ivol2)      , nobnd         , noq           ,
     &                 j(ixpnt)      , flowtot(1,ith), disptot(1,ith), gm_diag(1,ith), iscale        ,
     &                 gm_diac(1,ith), nomat         , gm_amat(1,ith), rowpnt        , fmat          ,
     &                 tmat          )

!     compute RHS (substance after substance)
         call dlwqf4 ( noseg         , nobnd         , nosys         , notot         , isys          ,
     &                 idt           , a(iconc)      , a(iderv)      , a(ivol)       , a(iboun)      ,
     &                 gm_rhs(1,ith) , gm_diac(1,ith), gm_sol(1,ith) )

!     solve linear system of equations
         call sgmres ( noseg+nobnd   , gm_rhs (1,ith), gm_sol (1,ith), novec         , gm_work(1,ith),
     &                 noseg+nobnd   , gm_hess(1,ith), novec+1       , iter          , tol           ,
     &                 nomat         , gm_amat(1,ith), j(imat)       , gm_diag(1,ith), rowpnt        ,
     &                 nolay         , ioptpc        , nobnd         , gm_trid(1,ith), iexseg (1,ith),
     &                 lun(19)       , litrep        )

!     mass balance of transport and copy of solution in the concentration array
         call dlwqf7 ( isys          , nosys         , notot         , noseg         , a(iconc)      ,
     &                 gm_sol (1,ith), nobnd         , a(iboun)      , noq           , j(ixpnt)      ,
     &                 flowtot(1,ith), disptot(1,ith), a(imas2)      , ndmpq         , j(iqdmp)      ,
     &                 a(idmpq)      , iknmkv        , idt           )

!        end loop over the substances

   40 continue

!$OMP ENDDO
!$OMP ENDPARALLEL
      if ( noth .gt. 1 ) timon = timon_old

      if ( timon ) call timstop ( ithand1 )

!          update mass array, explicit step for passive substances

         call dlwqb4 ( nosys   , notot   , noseg   , a(ivol2), a(imass),
     &                 a(iconc), a(iderv), nopa    , c(ipnam), a(iparm),
     &                 nosfun  , c(isfna), a(isfun), idt     )

!     replace old by new volumes
         call move   ( a(ivol2), a(ivol) , noseg   )

!     calculate closure error
         if ( lrewin .and. lstrec ) then
            call dlwqce ( a(imass), a(ivoll), a(ivol2), nosys , notot ,
     &                    noseg   , lun(19) )
            call move   ( a(ivoll), a(ivol) , noseg   )
         endif

!     integrate the fluxes at dump segments fill asmass with mass
         if ( ibflag .gt. 0 ) then
            call proint ( nflux   , ndmpar  , idt     , itfact  , a(iflxd),
     &                    a(iflxi), j(isdmp), j(ipdmp), ntdmpq  )
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

!     close files, except monitor file
         call CloseHydroFiles( dlwqd%collcoll )
         call close_files( lun )

!     write restart file
         call dlwq13 ( lun      , lchar , a(iconc) , itime , c(imnam) ,
     *                 c(isnam) , notot , noseg    )
      endif

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      return

      end subroutine dlwqnf
