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

      subroutine dlwqng ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Upwind horizontally, central vertically, GMRES solver (16)
!>
!>                         Performs time dependen integration. Upwind horizontally,
!>                         central vertically, implicit in time. Uses the GMRES
!>                         iterative solver with Krilov sub-spaces.\n
!>                         Forester filter is optional to ensure monotoneous behaviour
!>                         in the vertical.

C     CREATED            : april 1992 by J.v.Gils
C                          Fast solvers enhancements by L.P, R.V, KHT
C                                 sept-nov. 1996
C                              Central discretization vertically
C
C     LAST MODIFIED      : 6 feb 1997
C
C     LOGICAL UNITS      : LUN(19) , output, monitoring file
C                          LUN(20) , output, formatted dump file
C                          LUN(21) , output, unformatted hist. file
C                          LUN(22) , output, unformatted dump file
C                          LUN(23) , output, unformatted dump file
C
C     SUBROUTINES CALLED : DLWQTR, user transport routine
C                          DLWQWQ, user waterquality routine
C                          PROCES, DELWAQ proces system
C                          DLWQO2, DELWAQ output system
C                          DLWQPP, user postprocessing routine
C                          DLWQ13, system postpro-dump routine
C                          DLWQ14, scales waterquality
C                          DLWQ15, wasteload routine
C                          DLWQ17, boundary routine
C                          DLWQ41, updates volumes
C                          DLWQT0, updates other time dependent items
C                          DLWQ62, adds transport to matrix and rhs
C                          DLWQB3, computes volumes
C                          DLWQB4, computation of mass array
C                          DLWQB5, performs mass balance computation
C                          DLWQB6, updates right hand side
C                          DLWQB7, adds open boundaries to deriv
C                          DLWQB8, restores conc array
C                          DLWQF1, initializes matrix pointer administration
C                          DLWQF2, sets diagonal of system of equations
C                          DLWQG3, fills matrix for vertical central discretizat
C                          DLWQF4, sets (scaled) rhs of system of equations
C                          DLWQF6, checks matrix
C                          MOVE,   copies one array to another
C                          PROINT, integration of fluxes
C                          DHOPNF, opens files
C                          SGMRES, solves (iteratively) system of equations
C                          ZERCUM, zero's the cummulative array's
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
C     ---------------------------------------------------------
C     A       REAL       *      LOCAL  real      workspace array
C     J       INTEGER    *      LOCAL  integer   workspace array
C     C       CHARACTER  *      LOCAL  character workspace array
C     LUN     INTEGER    *      INPUT  array with unit numbers
C     LCHAR   CHAR*(*)   *      INPUT  filenames
C
      use grids
      use timers
      use m_timers_waq
      use waqmem                         ! Global memory with allocatable GMRES arrays
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress

      implicit none

      include 'actions.inc'
C
C     Declaration of arguments
C
      REAL, DIMENSION(*)          :: A
      INTEGER, DIMENSION(*)       :: J
      INTEGER, DIMENSION(*)       :: LUN
      CHARACTER*(*), DIMENSION(*) :: C
      CHARACTER*(*), DIMENSION(*) :: LCHAR
      INTEGER                     :: ACTION
      TYPE(DELWAQ_DATA), TARGET   :: DLWQD
      type(GridPointerColl)       :: GridPs               ! collection of all grid definitions

!$    include "omp_lib.h"

C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI  /    Timer characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSA   /   Pointers in real array workspace
C
      INCLUDE 'sysa.inc'
C
C     COMMON  /  SYSJ   /   Pointers in integer array workspace
C
      INCLUDE 'sysj.inc'
C
C     COMMON  /  SYSC   /   Pointers in character array workspace
C
      INCLUDE 'sysc.inc'

C
C     Local declarations
C
      REAL                   :: RDUMMY(1)
      LOGICAL                :: IMFLAG , IDFLAG , IHFLAG
      LOGICAL                :: UPDATR , UPDATE , LSTREC , LREWIN
      LOGICAL                :: LITREP , LDUMMY, timon_old
      real(kind=kind(1.0d0)) :: tol
      LOGICAL                :: FORESTER
      INTEGER                :: ITIME
      INTEGER                :: ITIMEL
      INTEGER                :: IFFLAG
      INTEGER                :: IAFLAG
      INTEGER                :: IBFLAG
      INTEGER                :: NDDIM
      INTEGER                :: NVDIM
      INTEGER                :: NOQT
      INTEGER                :: NOWARN
      INTEGER                :: NOPRED
      INTEGER                :: INWTYP
      INTEGER                :: ISYS
      INTEGER                :: NSTEP

      integer                :: ithandl
      integer, save          :: ithand1 = 0 ! Make this one "global"
      integer                :: noth
      integer                :: ith

      integer                :: ibnd

      !
      ! Variables specific to this method: leave them SAVEd
      !
      integer, save          :: ioptpc
      integer, save          :: iter
      integer, save          :: iscale

      !
      ! Dummy variables - used in DLWQD
      !
      integer                :: lleng
      integer                :: ioptzb
      integer                :: nosss
      integer                :: noqtt

      INCLUDE 'state_data.inc'


C
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

      if ( action == action_finalisation ) then
          include 'dlwqdata_restore.inc'
          goto 50
      endif

      IF ( ACTION == ACTION_INITIALISATION  .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

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
          lstrec   = icflag .eq. 1
          nosss    = noseg + nseg2
          NOQTT    = NOQ + NOQ4
          inwtyp   = intyp + nobnd
          noqt     = noq1+noq2
          lleng    = ileng+noqt*2
          forester = btest(intopt,6)
          nowarn   = 0

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )
C
C          initialize second volume array with the first one
C
          CALL MOVE   ( A(IVOL ), A(IVOL2) , NOSEG   )
C
C      Initialize pointer matices for fast solvers
C
          call dlwqf1 ( noseg   , nobnd   , noq     , noq1    , noq2    ,
     &                  nomat   , j(ixpnt), j(iwrk) , j(imat) , rowpnt  ,
     &                  fmat    , tmat    )

      ENDIF

C
C     Save/restore the local persistent variables,
C     if the computation is split up in steps
C
C     Note: the handle to the timer (ithandl) needs to be
C     properly initialised and restored
C
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqng", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

      if ( timon ) call timstrt ( "dlwqng", ithandl )

      iexseg = 1     !  There is nothing to mask. This array is meant for method 21
C
C======================= simulation loop ============================
   10 CONTINUE

!        Determine the volumes and areas that ran dry at start of time step

         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , nopa     ,
     &                 c(ipnam) , a(iparm) , nosfun   , c(isfna) , a(isfun) ,
     &                 j(iknmr) , iknmkv   )
C
C          user transport processes
C
      call timer_start(timer_user)
      UPDATE = UPDATR
      CALL DLWQTR ( NOTOT   , NOSYS   , NOSEG   , NOQ     , NOQ1    ,
     *              NOQ2    , NOQ3    , NOPA    , NOSFUN  , NODISP  ,
     *              NOVELO  , J(IXPNT), A(IVOL) , A(IAREA), A(IFLOW),
     *              A(ILENG), A(ICONC), A(IDISP), A(ICONS), A(IPARM),
     *              A(IFUNC), A(ISFUN), A(IDIFF), A(IVELO), ITIME   ,
     *              IDT     , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), UPDATE  , ILFLAG  ,
     *              NPARTp  )
      IF ( UPDATE ) UPDATR = .TRUE.
      call timer_stop(timer_user)
Cjvb
C     Temporary ? set the variables grid-setting for the DELWAQ variables
C
      CALL SETSET ( LUN(19), NOCONS, NOPA  , NOFUN   , NOSFUN,
     +              NOSYS  , NOTOT , NODISP, NOVELO  , NODEF ,
     +              NOLOC  , NDSPX , NVELX , NLOCX   , NFLUX ,
     +              NOPRED , NOVAR , NOGRID, J(IVSET))
Cjvb
C
C          call PROCES subsystem
C
      call hsurf  ( nosys   , notot   , noseg   , nopa    , c(ipnam),
     +              a(iparm), nosfun  , c(isfna), a(isfun), surface ,
     +              lun(19) )
      CALL PROCES ( NOTOT   , NOSEG   , A(ICONC), A(IVOL) , ITIME   ,
     +              IDT     , A(IDERV), NDMPAR  , NPROC   , NFLUX   ,
     +              J(IIPMS), J(INSVA), J(IIMOD), J(IIFLU), J(IIPSS),
     +              A(IFLUX), A(IFLXD), A(ISTOC), IBFLAG  , IPBLOO  ,
     +              IPCHAR  , IOFFBL  , IOFFCH  , A(IMASS), NOSYS   ,
     +              ITFACT  , A(IMAS2), IAFLAG  , INTOPT  , A(IFLXI),
     +              J(IXPNT), iknmkv  , NOQ1    , NOQ2    , NOQ3    ,
     +              NOQ4    , NDSPN   , J(IDPNW), A(IDNEW), NODISP  ,
     +              J(IDPNT), A(IDIFF), NDSPX   , A(IDSPX), A(IDSTO),
     +              NVELN   , J(IVPNW), A(IVNEW), NOVELO  , J(IVPNT),
     +              A(IVELO), NVELX   , A(IVELX), A(IVSTO), A(IDMPS),
     +              J(ISDMP), J(IPDMP), NTDMPQ  , A(IDEFA), J(IPNDT),
     +              J(IPGRD), J(IPVAR), J(IPTYP), J(IVARR), J(IVIDX),
     +              J(IVTDA), J(IVDAG), J(IVTAG), J(IVAGG), J(IAPOI),
     +              J(IAKND), J(IADM1), J(IADM2), J(IVSET), J(IGNOS),
     +              J(IGSEG), NOVAR   , A       , NOGRID  , NDMPS   ,
     +              C(IPRNA), INTSRT  , J(IOWNS), J(IOWNQ), MYPART  ,
     +              j(iprvpt), j(iprdon), nrref , j(ipror), nodef   ,
     +              surface  ,lun(19) )
C
C          communicate boundaries
C
      call timer_start(timer_bound)
      CALL DLWQ_BOUNDIO( LUN(19)  , NOTOT    ,
     +                   NOSYS    , NOSEG    ,
     +                   NOBND    , C(ISNAM) ,
     +                   C(IBNID) , J(IBPNT) ,
     +                   A(ICONC) , A(IBSET) ,
     +                   LCHAR(19))
C
C          set new boundaries
C
      IF ( ITIME .GE. 0   ) THEN
          ! first: adjust boundaries by OpenDA
          if ( dlwqd%inopenda ) then
              do ibnd = 1,nobnd
                  do isys = 1,nosys
                      call get_openda_buffer(isys,ibnd, 1,1,
     &                                  A(ibset+(ibnd-1)*nosys + isys-1))
                  enddo
               enddo
          endif

          CALL DLWQ17 ( A(IBSET), A(IBSAV), J(IBPNT), NOBND   , NOSYS   ,
     *                  NOTOT   , IDT     , A(ICONC), A(IFLOW), A(IBOUN))
      ENDIF
      call timer_stop(timer_bound)
C
C     Call OUTPUT system
C
      call timer_start(timer_output)
      CALL DLWQO2 ( NOTOT   , NOSEG   , NOPA    , NOSFUN  , ITIME   ,
     +              C(IMNAM), C(ISNAM), C(IDNAM), J(IDUMP), NODUMP  ,
     +              A(ICONC), A(ICONS), A(IPARM), A(IFUNC), A(ISFUN),
     +              A(IVOL) , NOCONS  , NOFUN   , IDT     , NOUTP   ,
     +              LCHAR   , LUN     , J(IIOUT), J(IIOPO), A(IRIOB),
     +              C(IONAM), NX      , NY      , J(IGRID), C(IEDIT),
     +              NOSYS   , A(IBOUN), J(ILP)  , A(IMASS), A(IMAS2),
     +              A(ISMAS), NFLUX   , A(IFLXI), ISFLAG  , IAFLAG  ,
     +              IBFLAG  , IMSTRT  , IMSTOP  , IMSTEP  , IDSTRT  ,
     +              IDSTOP  , IDSTEP  , IHSTRT  , IHSTOP  , IHSTEP  ,
     +              IMFLAG  , IDFLAG  , IHFLAG  , NOLOC   , A(IPLOC),
     +              NODEF   , A(IDEFA), ITSTRT  , ITSTOP  , NDMPAR  ,
     +              C(IDANA), NDMPQ   , NDMPS   , J(IQDMP), J(ISDMP),
     +              J(IPDMP), A(IDMPQ), A(IDMPS), A(IFLXD), NTDMPQ  ,
     +              C(ICBUF), NORAAI  , NTRAAQ  , J(IORAA), J(NQRAA),
     +              J(IQRAA), A(ITRRA), C(IRNAM), A(ISTOC), NOGRID  ,
     +              NOVAR   , J(IVARR), J(IVIDX), J(IVTDA), J(IVDAG),
     +              J(IAKND), J(IAPOI), J(IADM1), J(IADM2), J(IVSET),
     +              J(IGNOS), J(IGSEG), A       , NOBND   , NOBTYP  ,
     +              C(IBTYP), J(INTYP), C(ICNAM), NOQ     , J(IXPNT),
     +              INTOPT  , C(IPNAM), C(IFNAM), C(ISFNA), J(IDMPB),
     +              NOWST   , NOWTYP  , C(IWTYP), J(IWAST), J(INWTYP),
     +              A(IWDMP), iknmkv  , J(IOWNS), MYPART  )
      call timer_stop(timer_output)
C
C          zero cummulative array's
C
      call timer_start(timer_output)
      IF ( IMFLAG .OR. ( IHFLAG .AND. NORAAI .GT. 0 ) ) THEN
         CALL ZERCUM ( NOTOT   , NOSYS   , NFLUX   , NDMPAR  , NDMPQ   ,
     +                 NDMPS   , A(ISMAS), A(IFLXI), A(IMAS2), A(IFLXD),
     +                 A(IDMPQ), A(IDMPS), NORAAI  , IMFLAG  , IHFLAG  ,
     +                 A(ITRRA), IBFLAG  , NOWST   , A(IWDMP))
      ENDIF

      ! progress file
      call write_progress( dlwqd%progress )
      call timer_stop(timer_output)
C
C          simulation done ?
C
      IF ( ITIME .LT. 0      ) goto 9999
      IF ( ITIME .GE. ITSTOP ) GOTO 50

         call delpar01 ( itime   , noseg   , noq     , a(ivol) , a(iflow),
     &                   nosfun  , c(isfna), a(isfun))

!          restore conc-array from mass array

      call timer_start(timer_transport)
      call dlwqb8 ( nosys   , notot   , noseg   , a(ivol ), a(imass),
     &              a(iconc), nopa    , c(ipnam), a(iparm), nosfun  ,
     &              c(isfna), a(isfun))
C
C          add processes
C
      CALL DLWQ14 ( A(IDERV), NOTOT   , NOSEG   , ITFACT  , A(IMAS2),
     *              IDT     , IAFLAG  , A(IDMPS), INTOPT  , J(ISDMP),
     *              J(IOWNS), MYPART )
C
C          get new volumes
C
      ITIMEL = ITIME
      ITIME  = ITIME + IDT
      call timer_stop(timer_transport)
C
      call timer_start(timer_readdata)
      IF ( IVFLAG .EQ. 1 ) THEN
C
C          computation of volumes for computed volumes only
C
         CALL MOVE   ( A(IVOL) , A(IVOL2), NOSEG   )
         CALL DLWQB3 ( A(IAREA), A(IFLOW), A(IVNEW), J(IXPNT), NOTOT   ,
     *                 NOQ     , NVDIM   , J(IVPNW), A(IVOL2), INTOPT  ,
     *                 A(IMAS2), IDT     , IAFLAG  , NOSYS   , A(IDMPQ),
     *                 NDMPQ   , J(IQDMP))
         UPDATR = .TRUE.
      ELSE
C
C          read new volumes from files
C
         CALL DLWQ41 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *                 J(INRHA), J(INRH2), J(INRFT), NOSEG   , A(IVOL2),
     *                 J(IBULK), LCHAR   , ftype   , ISFLAG  , IVFLAG  ,
     *                 UPDATE  , J(INISP), A(INRSP), J(INTYP), J(IWORK),
     *                 LSTREC  , LREWIN  , A(IVOLL), MYPART  , dlwqd   )
         IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF

!        update the info on dry volumes with the new volumes

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , nopa     , c(ipnam) , a(iparm) ,
     &                 nosfun   , c(isfna) , a(isfun) , j(iknmr) , iknmkv   )
      call timer_stop(timer_readdata)
C
C          add the waste loads
C

      call timer_start(timer_wastes)
      call dlwq15 ( nosys     , notot    , noseg    , noq      , nowst    ,
     &              nowtyp    , ndmps    , intopt   , idt      , itime    ,
     &              iaflag    , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &              a(iflow ) , j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &              j(inwtyp) , j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &              iknmkv    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &              c(isfna ) , a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &              a(iwdmp)  , 1        , notot    , j(iowns ), mypart   )

      call timer_stop(timer_wastes)
C
C          Here we implement a loop that inverts the same matrix
C          for series of subsequent substances having the same
C          additional VELO and DISPER array. (JvG, April 24, 1993).
C
C          In solving equations with multiple rhs, the *FULL* fast (or should
C          I say slow?) solver algorithm needs to be applied to each rhs vector
C          So DELMAT may outperform FS when we deal with a large number of
C          substances with the same additional velocity and dispersion field
C          In future we need a smart switch between DELMAT and FS at this place
C          For now always do FS!
C                                                               (KHT, 11/11/96)


      call timer_start(timer_transport)
      if (timon) call timstrt ( "ADE solver", ithand1 )
      timon_old = timon
      noth = OMP_GET_NUM_THREADS()
      if ( noth .gt. 1 ) timon = .false.

!$OMP PARALLEL
!$OMP DO PRIVATE(ith) SCHEDULE(DYNAMIC)
! start of loop over substances

      do 40 isys = 1, nosys
!        ith = 1             !  number of threads for parallel processing
         ith = OMP_GET_THREAD_NUM()+1

!          initialize diagonal

         call dlwqf2 ( noseg         , nobnd         , idt           , a(ivol2)      , gm_diag(1,ith))

!          do the transport itself, fill matrix, scale diagonal

         call dlwqg3 ( noseg         , nobnd         , noq1          , noq2          , noq           ,
     &                 j(ixpnt)      , nddim         , nvdim         , j(idpnw)      , j(ivpnw)      ,
     &                 a(iarea)      , a(iflow)      , a(ileng)      , a(idisp)      , a(idnew)      ,
     &                 a(ivnew)      , isys          , intopt        , ilflag        , nomat         ,
     &                 gm_amat(1,ith), j(imat)       , rowpnt        , gm_diag(1,ith),gm_diac(1,ith),
     &                 iscale        , fmat          , tmat          , iknmkv        )

!          compute RHS (substance after substance)

         call dlwqf4 ( noseg        , nobnd         , nosys         , notot         , isys          ,
     &                 idt          , a(iconc)      , a(iderv)      , a(ivol)       , a(iboun)      ,
     &                 gm_rhs(1,ith), gm_diac(1,ith), gm_sol(1,ith) )

!          solve linear system of equations

         call sgmres ( noseg+nobnd  , gm_rhs (1,ith), gm_sol (1,ith), novec         , gm_work(1,ith),
     &                 noseg+nobnd  , gm_hess(1,ith), novec+1       , iter          , tol           ,
     &                 nomat        , gm_amat(1,ith), j(imat)       , gm_diag(1,ith), rowpnt        ,
     &                 nolay        , ioptpc        , nobnd         , gm_trid(1,ith), iexseg (1,ith),
     &                 lun(19)      , litrep        )

!           copy solution for this substance into concentration array

         call dlwqf6 ( noseg        , notot         , isys          , 1             , gm_sol(1,ith) ,
     &                 a(iconc)     , iknmkv        )

!        end loop over the substances

   40 continue

!$OMP ENDDO
!$OMP ENDPARALLEL
      if ( noth .gt. 1 ) timon = timon_old

      if ( timon ) call timstop ( ithand1 )

!          mass balance of transport

      CALL DLWQB5 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *              A(IVNEW), A(ICONC), A(IBOUN), J(IXPNT), NOSYS   ,
     *              NOTOT   , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *              NVDIM   , J(IDPNW), J(IVPNW), INTOPT  , A(IMAS2),
     *              ILFLAG  , A(IDMPQ), NDMPQ   , IDT     , J(IQDMP))

!          update mass array, explicit step for passive substances

      call dlwqb4 ( nosys   , notot   , noseg   , a(ivol2), a(imass),
     &              a(iconc), a(iderv), nopa    , c(ipnam), a(iparm),
     &              nosfun  , c(isfna), a(isfun), idt     )

!       Forester filter on the vertical

      IF ( FORESTER ) THEN
         CALL DLWQD2 ( LUN(19) , NOSYS   , NOTOT   , NOSEG   , NOQ3    ,
     *                 KMAX    , A(ICONC), A(LLENG), NOWARN  , J(IOWNS),
     *                 MYPART )
      ENDIF

!          replace old by new volumes

      CALL MOVE   ( A(IVOL2), A(IVOL) , NOSEG   )

!          calculate closure error

      IF ( LREWIN .AND. LSTREC ) THEN
         CALL DLWQCE ( A(IMASS), A(IVOLL), A(IVOL2), NOSYS , NOTOT ,
     +                 NOSEG   , LUN(19) )
         CALL MOVE   ( A(IVOLL), A(IVOL) , NOSEG   )
      ENDIF

!          integrate the fluxes at dump segments fill ASMASS with mass

      IF ( IBFLAG .GT. 0 ) THEN
         CALL PROINT ( NFLUX   , NDMPAR  , IDT     , ITFACT  , A(IFLXD),
     +                 A(IFLXI), J(ISDMP), J(IPDMP), NTDMPQ  )
      ENDIF
      call timer_stop(timer_transport)

!          new time values, volumes excluded

      call timer_start(timer_readdata)
      CALL DLWQT0 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *              J(INRHA), J(INRH2), J(INRFT), IDT     , A(IVOL) ,
     *              A(IDIFF), A(IAREA), A(IFLOW), A(IVELO), A(ILENG),
     *              A(IWSTE), A(IBSET), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), J(IBULK), LCHAR   , C(ILUNT), ftype   ,
     *              INTSRT  , ISFLAG  , IFFLAG  , IVFLAG  , ILFLAG  ,
     *              UPDATE  , J(IKTIM), J(IKNMR), J(INISP), A(INRSP),
     *              J(INTYP), J(IWORK), .FALSE. , LDUMMY  , RDUMMY  ,
     &              .FALSE. , GridPs  , dlwqd   )
      IF ( UPDATE ) UPDATR = .TRUE.
      call timer_stop(timer_readdata)

!          end of time loop

      IF ( ACTION == ACTION_FULLCOMPUTATION ) THEN
          GOTO 10
      ENDIF
C
   50 CONTINUE

      IF ( ACTION == ACTION_FINALISATION    .OR.
     &     ACTION == ACTION_FULLCOMPUTATION      ) THEN


!          close files, except monitor file

          call timer_start(timer_close)
          call CloseHydroFiles( dlwqd%collcoll )
          call close_files( lun )

!          write restart file

          CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITIME , C(IMNAM) ,
     *                  C(ISNAM) , NOTOT , NOSEG    )
          call timer_stop(timer_close)
      ENDIF

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      RETURN
      END
