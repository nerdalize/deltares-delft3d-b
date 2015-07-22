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

      subroutine dlwqnb ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         1st order upwind, fully implicit direct method (10)
!>
!>                         Performs time dependent integration. Upwind 1st order
!>                         in space. Fully implicit with a direct method in time.\n
!>                         Matrices become very large in 3D and method unworkable. In 2D
!>                         the method can be used. In 1D the method outperforms the
!>                         iterative methods.

C     CREATED            : april 1992 by J.v.Gils
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
C                          DELMAT, inverts the matrix
C                          DLWQB1, initializes matrix and rhs
C                          DLWQB2, checks matrix
C                          DLWQB3, computes volumes
C                          DLWQB4, computation of mass array
C                          DLWQB5, performs mass balance computation
C                          DLWQB6, updates right hand side
C                          DLWQB7, adds open boundaries to deriv
C                          DLWQB8, restores conc array
C                          MOVE,   copies one array to another
C                          PROINT, integration of fluxes
C                          DHOPNF, opens files
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
C     Declaration of arguments
C
      use grids
      use timers
      use m_timers_waq
      use m_couplib
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress
      use waqmem          ! module with the more recently added arrays

      implicit none

      include 'actions.inc'
C
C     Declaration of arguments
C
      REAL, DIMENSION(*)             :: A
      INTEGER, DIMENSION(*)          :: J
      INTEGER, DIMENSION(*)          :: LUN
      CHARACTER*(*), DIMENSION(*)    :: C
      CHARACTER*(*), DIMENSION(*)    :: LCHAR
      INTEGER                        :: ACTION
      TYPE(DELWAQ_DATA), TARGET      :: DLWQD
      type(GridPointerColl)          :: GridPs               ! collection of all grid definitions

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

c     Common to define external communications in SOBEK
c     OLCFWQ             Flag indicating ONLINE running of CF and WQ
c     SRWACT             Flag indicating active data exchange with SRW
c     RTCACT             Flag indicating output for RTC

      LOGICAL            OLCFWQ, SRWACT, RTCACT
      COMMON /COMMUN/    OLCFWQ, SRWACT, RTCACT
C
C     Local declarations
C
      LOGICAL         IMFLAG , IDFLAG , IHFLAG , LDUMMY
      LOGICAL         UPDATR , UPDATE , LSTREC , LREWIN
      INTEGER         ITIME
      INTEGER         NSTEP
      INTEGER         IFFLAG
      INTEGER         IAFLAG
      INTEGER         IBFLAG
      INTEGER         NDDIM
      INTEGER         NVDIM
      INTEGER         INWTYP
      INTEGER         I
      INTEGER         IBND
      INTEGER         ISYS
      INTEGER         NSYS
      INTEGER         IDDEF
      INTEGER         IVDEF
      REAL            RDUMMY(1)
      INTEGER         LAATST

      integer          :: ithandl


      !
      ! Dummy variables - used in DLWQD
      !
      integer          :: ioptzb
      integer          :: nowarn
      integer          :: nosss
      integer          :: noqtt
      integer          :: noqt
      integer          :: nopred
      integer          :: itimel
      integer          :: lleng
      logical          :: forester
      real(kind=kind(1.0d0)) :: tol

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
C     (see below, DLWQB1). To anticipate this, the method uses an
C     extra VOLUME-array (IVOL2), and uses the AMASS-array (IMASS)
C     for the rhs-matrix, in stead of the DERIV-array as in method 6.
C     (JvG, May 8 1992)

      if ( action == action_finalisation ) then
          include 'dlwqdata_restore.inc'
          goto 50
      endif

      IF ( ACTION == ACTION_INITIALISATION  .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

C
C          some initialisation
C
          ithandl = 0
          ITIME   = ITSTRT
          NSTEP   = (ITSTOP-ITSTRT)/IDT
          IFFLAG  = 0
          IAFLAG  = 0
          IBFLAG  = 0
          IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1
          IF ( NDSPN .EQ. 0 ) THEN
             NDDIM = NODISP
          ELSE
             NDDIM = NDSPN
          ENDIF
          IF ( NVELN .EQ. 0 ) THEN
             NVDIM = NOVELO
          ELSE
             NVDIM = NVELN
          ENDIF
          LSTREC = ICFLAG .EQ. 1
          nosss  = noseg + nseg2
          NOQTT  = NOQ + NOQ4
          inwtyp = intyp + nobnd

          UPDATR = .TRUE.

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )
C
C          initialize second volume array with the first one
C
          CALL MOVE   ( A(IVOL ), A(IVOL2) , NOSEG   )
      ENDIF

C
C     Save/restore the local persistent variables,
C     if the computation is split up in steps
C
C     Note: the handle to the timer (ithandl) needs to be
C     properly initialised and restored
C
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqnb", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

      if ( timon ) call timstrt ( "dlwqnb", ithandl )
C
C======================= simulation loop ============================
   10 CONTINUE

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
C
C          user transport processes
C
      UPDATE = UPDATR
      CALL DLWQTR ( NOTOT   , NOSYS   , NOSEG   , NOQ     , NOQ1    ,
     *              NOQ2    , NOQ3    , NOPA    , NOSFUN  , NODISP  ,
     *              NOVELO  , J(IXPNT), A(IVOL) , A(IAREA), A(IFLOW),
     *              A(ILENG), A(ICONC), A(IDISP), A(ICONS), A(IPARM),
     *              A(IFUNC), A(ISFUN), A(IDIFF), A(IVELO), ITIME   ,
     *              IDT     , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), UPDATE  , ILFLAG  ,
     *              NPARTp   )
      IF ( UPDATE ) UPDATR = .TRUE.
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
     &              j(iprvpt), j(iprdon), nrref , j(ipror), nodef   ,
     &              surface  ,lun(19) )
C
C          communicate boundaries
C
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
     &                                A(ibset+(ibnd-1)*nosys + isys-1))
                  enddo
              enddo
          endif

          CALL DLWQ17 ( A(IBSET), A(IBSAV), J(IBPNT), NOBND   , NOSYS   ,
     *                  NOTOT   , IDT     , A(ICONC), A(IFLOW), A(IBOUN))
      ENDIF
C
C     Call OUTPUT system
C
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
C
C          zero cummulative array's
C
      IF ( IMFLAG .OR. ( IHFLAG .AND. NORAAI .GT. 0 ) ) THEN
         CALL ZERCUM ( NOTOT   , NOSYS   , NFLUX   , NDMPAR  , NDMPQ   ,
     +                 NDMPS   , A(ISMAS), A(IFLXI), A(IMAS2), A(IFLXD),
     +                 A(IDMPQ), A(IDMPS), NORAAI  , IMFLAG  , IHFLAG  ,
     +                 A(ITRRA), IBFLAG  , NOWST   , A(IWDMP))
      ENDIF

      ! progress file
      call write_progress( dlwqd%progress )
C
C          simulation done ?
C
      IF ( ITIME .LT. 0      ) goto 9999
      IF ( ITIME .GE. ITSTOP ) GOTO 50

         call delpar01 ( itime   , noseg   , noq     , a(ivol) , a(iflow),
     &                   nosfun  , c(isfna), a(isfun))

!          restore conc-array from mass array

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

!          add the waste loads

      call dlwq15 ( nosys     , notot    , noseg    , noq      , nowst    ,
     &              nowtyp    , ndmps    , intopt   , idt      , itime    ,
     &              iaflag    , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &              a(iflow ) , j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &              j(inwtyp) , j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &              iknmkv    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &              c(isfna ) , a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &              a(iwdmp)  , 1        , notot    , j(iowns ), mypart   )
C
C          Here we implement a loop that inverts the same matrix
C          for series of subsequent substances having the same
C          additional VELO and DISPER array. (JvG, April 24, 1993).
C
C          Start of loop

      ISYS = 0
      NSYS = 1
   20 CONTINUE
C
C          Compute range of substances for present cycle
C
      ISYS = ISYS + NSYS
      IDDEF = J(IDPNW+ISYS-1)
      IVDEF = J(IVPNW+ISYS-1)
      DO 30 I = ISYS+1,NOSYS
   30 IF (J(IDPNW+I-1).NE.IDDEF.OR.J(IVPNW+I-1).NE.IVDEF) GOTO 40
      I = NOSYS + 1
   40 NSYS = I - ISYS
      IF ( .NOT.( ISYS .EQ. 1 .AND. NSYS .EQ. NOSYS ) ) UPDATR = .TRUE.
C
      IF ( UPDATR ) THEN
         UPDATR = .FALSE.

!          zero the matrix, initialize diagonal and compute RHS

         call dlwqb1 ( notot   , noseg   , a(ivol) , a(ivol2), a(iconc),
     &                 a(iderv), isys    , nsys    , jtrack  , a(itimr),
     &                 rhs     , idt     )
C
C          do the transport itself (Attn.: NOTOT replaced by NOSYS)
C
         CALL DLWQB9 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *                 A(IVNEW), A(IBOUN), J(IXPNT), NOSYS   , ISYS    ,
     *                 NSYS    , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *                 NVDIM   , J(IDPNW), J(IVPNW), rhs     , A(ITIMR),
     *                 JTRACK  , INTOPT  , ILFLAG  )
C
C          invert the matrix
C
         CALL DELMAT ( NOSEG   , JTRACK  , JTRACK  , NSYS    , A(ITIMR),
     *                                               rhs     ,    0    )
      ELSE
C          compute RHS
         CALL DLWQB6 ( A(ICONC), A(IDERV), NOSEG   , NOTOT   , A(IVOL) ,
     *                           IDT     , 1       , rhs     , NOSYS   )
C
C          do the transport itself, only accross boundaries
C
         CALL DLWQB7 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *                 A(IVNEW), A(IBOUN), J(IXPNT), NOSYS   , 1       ,
     *                 NOSYS   , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *                 NVDIM   , J(IDPNW), J(IVPNW), rhs     , INTOPT  ,
     *                                                         ILFLAG  )
C
C          calculate the concentration with known matrix
C
         CALL DELMAT ( NOSEG   , JTRACK  , JTRACK  , NOSYS   , A(ITIMR),
     *                                               rhs     ,    2    )
      ENDIF
C
C          store results from RHS in concentration matrix
C
      CALL DLWQB2 ( A(ICONC), rhs     , NOSEG   , NOTOT   , ISYS    ,
     *              NSYS    )
C
C          Back for new cycle if last substance does not equal NOSYS
C
      IF ( (ISYS+NSYS-1) .NE. NOSYS ) GOTO 20
C
C          mass balance of transport
C
      CALL DLWQB5 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *              A(IVNEW), A(ICONC), A(IBOUN), J(IXPNT), NOSYS   ,
     *              NOTOT   , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *              NVDIM   , J(IDPNW), J(IVPNW), INTOPT  , A(IMAS2),
     *              ILFLAG  , A(IDMPQ), NDMPQ   , IDT     , J(IQDMP))

!          update mass array, explicit step for passive substances

      call dlwqb4 ( nosys   , notot   , noseg   , a(ivol2), a(imass),
     &              a(iconc), a(iderv), nopa    , c(ipnam), a(iparm),
     &              nosfun  , c(isfna), a(isfun), idt     )
C
C          replace old by new volumes
C
      CALL MOVE   ( A(IVOL2), A(IVOL) , NOSEG   )
C
C          calculate closure error
C
      IF ( LREWIN .AND. LSTREC ) THEN
         CALL DLWQCE ( A(IMASS), A(IVOLL), A(IVOL2), NOSYS , NOTOT ,
     +                 NOSEG   , LUN(19) )
         CALL MOVE   ( A(IVOLL), A(IVOL) , NOSEG   )
      ENDIF
C
C          integrate the fluxes at dump segments fill ASMASS with mass
C
      IF ( IBFLAG .GT. 0 ) THEN
         CALL PROINT ( NFLUX   , NDMPAR  , IDT     , ITFACT  , A(IFLXD),
     +                 A(IFLXI), J(ISDMP), J(IPDMP), NTDMPQ  )
      ENDIF

      IF ( RTCACT )
c     Interface to RTC (i)
     Jcall RTCSHL (ITIME, A, J, C)

      IF ( SRWACT )
C     Interface to SRW (i)
     JCALL SRWSHL (ITIME, A, J, C)

      IF ( OLCFWQ ) THEN
c     Synchronizing with CF(i) for on-line mode outside SRW only
c         write (*,*) ' Stop WQ i=',TELLER,' '
c          read  (*,*)
c         write (*,*) ' PUTPER WQtoCF'
          call putpcf('WQtoCF','DataWQtoCF')
c         write (*,*) ' DONE'
c     Synchronizing with CF(i+1) for on-line mode outside SRW only
c     ONLY if this is NOT the last time step!!!!!!!!!!!!!
          IF ( ITIME+IDT .LT. ITSTOP ) then
c             write (*,*) ' GETPER CFtoWQ'
              call getpcf('CFtoWQ','DataCFtoWQ')
c             write (*,*) ' DONE'
c             write (*,*) ' Start WQ i=',TELLER+1,' '
c              read  (*,*)
              LAATST = 0
          ELSE
              LAATST = -1
          ENDIF
      ENDIF
C
C          new time values, volumes excluded
c
      IF ( OLCFWQ .OR. SRWACT ) THEN
c     Note: time step (i+1) of WQINT!
c         write (*,*) ' Start WQI i=',TELLER+1,' '
c          read  (*,*)
c         write (*,*) ' PUTPEV WQtoWQI'
          call putpev ('WQtoWQI','DataWQtoWQI',LAATST)
c         write (*,*) ' DONE '
c         write (*,*) ' GETPER WQItoWQ'
          call GETPER ('WQItoWQ','DataWQItoWQ')
c         write (*,*) ' DONE '
c         write (*,*) ' Stop WQI i=',TELLER+1,' '
c          read  (*,*)
      ENDIF
c
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
C
C          end of time loop
C
      IF ( ACTION == ACTION_FULLCOMPUTATION ) THEN
          GOTO 10
      ENDIF
C
   50 CONTINUE

      IF ( ACTION == ACTION_FINALISATION    .OR.
     &     ACTION == ACTION_FULLCOMPUTATION      ) THEN
C
C
C          close files, except monitor file
C
          call CloseHydroFiles( dlwqd%collcoll )
          call close_files( lun )
C
C          write restart file
C
          CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITIME , C(IMNAM) ,
     *                  C(ISNAM) , NOTOT , NOSEG    )
      ENDIF

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      RETURN
      END SUBROUTINE
