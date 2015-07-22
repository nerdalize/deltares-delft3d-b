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

      subroutine dlwqne ( a     , j     , c     , lun   , lchar  ,
     +                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         First order upwind in space, upwind implicit vertical (13)
!>
!>                         Performs time dependent integration. Upwind advection, explicit
!>                         horizontally, implicit vertically.

C     CREATED            : june 1988 by L. Postma
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
C                          DLWQ16, explicit transport
C                          DLWQ17, boundary routine
C                          DLWQ40, explicit derivative
C                          DLWQ41, update volumes
C                          DLWQ42, set explicit step
C                          DLWQE1, implicit step
C                          DLWQ44, update arrays
C                          DLWQT0, update other time functions
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
      use grids
      use timers
      use m_couplib
      use m_timers_waq
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress
      use waqmem          ! module with the more recently added arrays

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
      LOGICAL         IMFLAG , IDFLAG , IHFLAG
      LOGICAL         LDUMMY , LSTREC , LREWIN , LDUMM2
      LOGICAL         FORESTER
      REAL            RDUMMY(1)
      INTEGER         IFFLAG
      INTEGER         IAFLAG
      INTEGER         IBFLAG
      INTEGER         NDDIM
      INTEGER         NVDIM
      INTEGER         INWTYP
      INTEGER         ITIME
      INTEGER         NSTEP
      INTEGER         NOWARN
      INTEGER         IBND
      INTEGER         ISYS

      INTEGER         NOQT
      INTEGER         LAREA
      INTEGER         LDISP
      INTEGER         LDIFF
      INTEGER         LFLOW
      INTEGER         LLENG
      INTEGER         LNOQ
      INTEGER         LQDMP
      INTEGER         LVELO
      INTEGER         LXPNT

      integer          :: ithandl
      !
      ! Dummy variables - used in DLWQD
      !
      integer          :: ioptzb
      integer          :: nosss
      integer          :: noqtt
      integer          :: nopred
      integer          :: itimel
      logical          :: updatr
      real(kind=kind(1.0d0)) :: tol

      INCLUDE 'state_data.inc'

      if ( action == action_finalisation ) then
          include 'dlwqdata_restore.inc'
          goto 20
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
          LDUMMY = .FALSE.
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
          FORESTER = BTEST(INTOPT,6)
          NOWARN   = 0
          IF ( ILFLAG .EQ. 0 ) LLENG = ILENG+2

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )
C
C          initialize second volume array with the first one
C
          nosss  = noseg + nseg2
          CALL MOVE   ( A(IVOL ), A(IVOL2) , nosss   )
      ENDIF

C
C     Save/restore the local persistent variables,
C     if the computation is split up in steps
C
C     Note: the handle to the timer (ithandl) needs to be
C     properly initialised and restored
C
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqne", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

!          adaptations for layered bottom 08-03-2007  lp

      nosss  = noseg + nseg2
      NOQTT  = NOQ + NOQ4
      inwtyp = intyp + nobnd
C
C     Set alternating set of pointers
C
      NOQT  = NOQ1+NOQ2
      lnoq  = noqtt - noqt
      LDISP = IDISP+2
      LDIFF = IDNEW+NDDIM*NOQT
      LAREA = IAREA+NOQT
      LFLOW = IFLOW+NOQT
      LLENG = ILENG+NOQT*2
      LVELO = IVNEW+NVDIM*NOQT
      LXPNT = IXPNT+NOQT*4
      LQDMP = IQDMP+NOQT

      if ( timon ) call timstrt ( "dlwqne", ithandl )
C
C======================= simulation loop ============================
C
   10 CONTINUE

!        Determine the volumes and areas that ran dry at start of time step

         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , nopa     ,
     &                 c(ipnam) , a(iparm) , nosfun   , c(isfna) , a(isfun) ,
     &                 j(iknmr) , iknmkv   )
C
C          user transport processes
C
      CALL DLWQTR ( NOTOT   , NOSYS   , nosss   , NOQ     , NOQ1    ,
     *              NOQ2    , NOQ3    , NOPA    , NOSFUN  , NODISP  ,
     *              NOVELO  , J(IXPNT), A(IVOL) , A(IAREA), A(IFLOW),
     *              A(ILENG), A(ICONC), A(IDISP), A(ICONS), A(IPARM),
     *              A(IFUNC), A(ISFUN), A(IDIFF), A(IVELO), ITIME   ,
     *              IDT     , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), LDUMMY  , ILFLAG  ,
     *              NPARTp  )
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
      CALL PROCES ( NOTOT   , nosss   , A(ICONC), A(IVOL) , ITIME   ,
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
      CALL DLWQ_BOUNDIO( LUN(19)  , NOTOT    ,
     +                   NOSYS    , nosss    ,
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
C
C     Call OUTPUT system
C
      CALL DLWQO2 ( NOTOT   , nosss   , NOPA    , NOSFUN  , ITIME   ,
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
     +              C(IBTYP), J(INTYP), C(ICNAM), noqtt   , J(IXPNT),
     +              INTOPT  , C(IPNAM), C(IFNAM), C(ISFNA), J(IDMPB),
     +              NOWST   , NOWTYP  , C(IWTYP), J(IWAST), J(INWTYP),
     +              A(IWDMP), iknmkv  , J(IOWNS), MYPART  )
C
C          zero cumulative arrays
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
      IF ( ITIME .GE. ITSTOP ) GOTO 20

         call delpar01 ( itime   , noseg   , noq     , a(ivol) , a(iflow),
     &                   nosfun  , c(isfna), a(isfun))

!          add processes

      CALL DLWQ14 ( A(IDERV), NOTOT   , nosss   , ITFACT  , A(IMAS2),
     *              IDT     , IAFLAG  , A(IDMPS), INTOPT  , J(ISDMP),
     *              J(IOWNS), MYPART )
C
C          get new volumes
C
      ITIMEL = ITIME
      ITIME  = ITIME + IDT
      CALL DLWQ41 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *              J(INRHA), J(INRH2), J(INRFT), NOSEG   , A(IVOL2),
     *              J(IBULK), LCHAR   , ftype   , ISFLAG  , IVFLAG  ,
     *              LDUMMY  , J(INISP), A(INRSP), J(INTYP), J(IWORK),
     *              LSTREC  , LREWIN  , A(IVOLL), MYPART  , dlwqd   )

!        update the info on dry volumes with the new volumes

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , nopa     , c(ipnam) , a(iparm) ,
     &                 nosfun   , c(isfna) , a(isfun) , j(iknmr) , iknmkv   )
C
C          add the waste loads
C
      call dlwq15 ( nosys     , notot    , nosss    , noq      , nowst    ,
     &              nowtyp    , ndmps    , intopt   , idt      , itime    ,
     &              iaflag    , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &              a(iflow ) , j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &              j(inwtyp) , j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &              iknmkv    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &              c(isfna ) , a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &              a(iwdmp)  , 1        , notot    , j(iowns ), mypart   )
C
C          explicit part of the transport step, derivative
C
      call dlwq16 ( nosys   , notot   , nosss   , noq1    , noq2    ,
     &              noq3    , noqt    , nddim   , nvdim   , a(idisp),
     &              a(idnew), a(ivnew), a(iarea), a(iflow), a(ileng),
     &              j(ixpnt), iknmkv  , j(idpnw), j(ivpnw), a(iconc),
     &              a(iboun), intopt  , ilflag  , idt     , a(iderv),
     &              iaflag  , a(imas2), ndmpq   , j(iqdmp), a(idmpq),
     &              j(iowns), mypart  )

!          explicit part of transport done, volumes on diagonal

      call dlwq42 ( nosys   , notot   , nosss   , a(ivol2), a(imass),
     &              a(iconc), a(iderv), nopa    , c(ipnam), a(iparm),
     &              nosfun  , c(isfna), a(isfun), idt     , ivflag  ,
     &              lun(19) , j(iowns), mypart  )
C
C          user water quality processes implicit part
C
      CALL DLWQWX ( NOTOT   , NOSYS   , nosss   , NOPA    , NOSFUN  ,
     *              A(IVOL2), A(IDERV), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), A(ICONC), ITIME   , IDT     , A(ISMAS),
     *              IBFLAG  , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), NODUMP  , J(IDUMP))

!          performs the implicit part of the transport step

      call dlwqe1 ( nosys   , notot   , nosss   , noq3    , lnoq    ,
     &              nddim   , nvdim   , a(ldisp), a(ldiff), a(lvelo),
     &              a(larea), a(lflow), a(lleng), j(lxpnt), iknmkv  ,
     &              j(idpnw), j(ivpnw), a(iconc), a(iboun), intopt  ,
     &              ilflag  , idt     , a(iderv), iaflag  , a(imas2),
     &              j(iowns), mypart  , lun(19) , ndmpq   , j(lqdmp),
     &              a(idmpq), arhs    , adiag   , acodia  , bcodia  )
C
C          user water quality processes implicit part
C
      CALL DLWQWY ( NOTOT   , NOSYS   , nosss   , NOPA    , NOSFUN  ,
     *              A(IVOL2), A(IDERV), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), A(ICONC), ITIME   , IDT     , A(ISMAS),
     *              IBFLAG  , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), NODUMP  , J(IDUMP))
C
C          update the nescessary arrays
C
      call dlwq44 ( nosys   , notot   , nosss   , a(ivol2), a(imass),
     &              a(iconc), a(iderv), j(iowns), mypart  )
      CALL MOVE   ( A(IVOL2), A(IVOL), NOSEG )
C
C          calculate closure error
C
      IF ( LREWIN .AND. LSTREC ) THEN
         CALL DLWQCE ( A(IMASS), A(IVOLL), A(IVOL2), NOSYS , NOTOT ,
     +                 NOSEG   , LUN(19) )
         CALL MOVE   ( A(IVOLL), A(IVOL ), NOSEG )
      ENDIF
C
C          integrate the fluxes at dump segments fill ASMASS with mass
C
      IF ( IBFLAG .GT. 0 ) THEN
         CALL PROINT ( NFLUX   , NDMPAR  , IDT     , ITFACT  , A(IFLXD),
     +                 A(IFLXI), J(ISDMP), J(IPDMP),  NTDMPQ )
      ENDIF
C
C          new time values, volumes excluded
C
      CALL DLWQT0 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *              J(INRHA), J(INRH2), J(INRFT), IDT     , A(IVOL) ,
     *              A(IDIFF), A(IAREA), A(IFLOW), A(IVELO), A(ILENG),
     *              A(IWSTE), A(IBSET), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), J(IBULK), LCHAR   , C(ILUNT), ftype   ,
     *              INTSRT  , ISFLAG  , IFFLAG  , IVFLAG  , ILFLAG  ,
     *              LDUMM2  , J(IKTIM), J(IKNMR), J(INISP), A(INRSP),
     *              J(INTYP), J(IWORK), .FALSE. , LDUMMY  , RDUMMY  ,
     &              .FALSE. , GridPs  , dlwqd   )
C
C          end of loop
C
      IF ( ACTION == ACTION_FULLCOMPUTATION ) THEN
          GOTO 10
      ENDIF
C
   20 CONTINUE

      IF ( ACTION == ACTION_FINALISATION    .OR.
     &     ACTION == ACTION_FULLCOMPUTATION      ) THEN

C
C          close files, except monitor file
C
          call CloseHydroFiles( dlwqd%collcoll )
          call close_files( lun )
C
C          write restart file
C
          CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITIME , C(IMNAM) ,
     *                  C(ISNAM) , NOTOT , nosss    )
      ENDIF

 9999 if ( timon ) call timstop ( ithandl )
      dlwqd%itime = itime
      RETURN
C
      END
