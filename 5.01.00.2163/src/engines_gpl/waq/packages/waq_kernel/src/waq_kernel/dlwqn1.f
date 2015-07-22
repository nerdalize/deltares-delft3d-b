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

      subroutine dlwqn1 ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         First order upwind in space and time (1)
!>
!>                         Performs first order explicit time integration using
!>                         upwind discretization in space. The method is explict
!>                         and thus has a time step stability constraint.

!     CREATED            : june 1988 by L. Postma

!     LOGICAL UNITS      : LUN(19) , output, monitoring file
!                          LUN(20) , output, formatted dump file
!                          LUN(21) , output, unformatted hist. file
!                          LUN(22) , output, unformatted dump file
!                          LUN(23) , output, unformatted dump file

!     SUBROUTINES CALLED : DLWQTR          , user transport routine
!                          PROCES          , DELWAQ proces system
!                          DLWQO2          , DELWAQ output system
!                          DLWQT0          , sets time functions
!                          DLWQ13          , system postpro-dump routine
!                          DLWQ14          , scales waterquality
!                          DLWQ15          , wasteload routine
!                          DLWQ16          , transport routine
!                          DLWQ17          , boundary routine
!                          DLWQ18          , integration step
!                          DLWQ41          , updates volumes to new time level
!                          DLWQCE          , closure error correction
!                          dryfld          , detect drying and flooding from volumes
!                          dryfle          , detect drying and flooding from flows
!                          MOVE            , moves one array to another
!                          PROINT          , integration of fluxes
!                          SETSET          , variable grid settings
!                          ZERCUM          , zero's the cummulative array's
!                          BOUNDIO         , hand to external boundary resolve
!               Delwaq system timer:
!                          timstrt         , start of Delwaq system timer
!                          timstop         , stop of Delwas system timer
!               Some timer by someone:
!                          CPU_TIME, Fortran timer routine
!               VORTECH parallel timer and communication routines
!                          timer_start     , VORTECH parallel timer start
!                          timer_stop      , VORTECH parallel timer stop
!                          update_rdata    , VORTECH parallel data updat
!                          collect_rdata   , VORTECH parallel data collect
!                          distribute_rdata, VORTECH parallel data distribution
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

!     Declaration of arguments

      real                 a     ( * )
      integer              j     ( * )
      integer              lun   ( * )
      character*(*)        c     ( * )
      character*(*)        lchar ( * )
      integer              action
      type(delwaq_data), target      :: dlwqd
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
C
C     Local declarations
C
      LOGICAL         IMFLAG , IDFLAG , IHFLAG
      LOGICAL         LDUMMY , LSTREC , LREWIN
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
      INTEGER         IERROR

      INTEGER         NOQT
      INTEGER         LLENG
      INTEGER         IDTOLD

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
          NOWARN   = 0
          IF ( ILFLAG .EQ. 0 ) LLENG = ILENG+2

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )
C
C          Initialize second volume array with the first one
C
          nosss  = noseg + nseg2
          CALL MOVE   ( A(IVOL ), A(IVOL2) , NOSSS   )


      ENDIF

C
C     Save/restore the local persistent variables,
C     if the computation is split up in steps
C
C     Note: the handle to the timer (ithandl) needs to be
C     properly initialised and restored
C
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqn1", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

      nosss  = noseg + nseg2
      noqtt  = noq + noq4
      inwtyp = intyp + nobnd

      if ( timon ) call timstrt ( "dlwqn1", ithandl )

C
C======================= simulation loop ============================
C
   10 CONTINUE
!        Determine the volumes and areas that ran dry,
!        They cannot have explicit processes during this time step

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
!*****if (mypart.eq.1) then
C
         call timer_start(timer_user)
         CALL DLWQTR ( NOTOT   , NOSYS   , NOSSS   , NOQ     , NOQ1    ,
     *                 NOQ2    , NOQ3    , NOPA    , NOSFUN  , NODISP  ,
     *                 NOVELO  , J(IXPNT), A(IVOL) , A(IAREA), A(IFLOW),
     *                 A(ILENG), A(ICONC), A(IDISP), A(ICONS), A(IPARM),
     *                 A(IFUNC), A(ISFUN), A(IDIFF), A(IVELO), ITIME   ,
     *                 IDT     , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *                 C(IPNAM), C(IFNAM), C(ISFNA), LDUMMY  , ILFLAG  ,
     *                 NPARTp  )
         call timer_stop(timer_user)
Cjvb
C     Temporary ? set the variables grid-setting for the DELWAQ variables
C
         CALL SETSET ( LUN(19), NOCONS, NOPA  , NOFUN   , NOSFUN,
     +                 NOSYS  , NOTOT , NODISP, NOVELO  , NODEF ,
     +                 NOLOC  , NDSPX , NVELX , NLOCX   , NFLUX ,
     +                 NOPRED , NOVAR , NOGRID, J(IVSET))
Cjvb
!*****end if !(mypart.eq.1)
C
C          call PROCES subsystem
C
      call hsurf  ( nosys   , notot   , noseg   , nopa    , c(ipnam),
     +              a(iparm), nosfun  , c(isfna), a(isfun), surface ,
     +              lun(19) )
      CALL PROCES ( NOTOT    , NOSSS    , A(ICONC), A(IVOL) , ITIME   ,
     +              IDT      , A(IDERV) , NDMPAR  , NPROC   , NFLUX   ,
     +              J(IIPMS) , J(INSVA) , J(IIMOD), J(IIFLU), J(IIPSS),
     +              A(IFLUX) , A(IFLXD) , A(ISTOC), IBFLAG  , IPBLOO  ,
     +              IPCHAR   , IOFFBL   , IOFFCH  , A(IMASS), NOSYS   ,
     +              ITFACT   , A(IMAS2) , IAFLAG  , INTOPT  , A(IFLXI),
     +              J(IXPNT) , iknmkv   , NOQ1    , NOQ2    , NOQ3    ,
     +              NOQ4     , NDSPN    , J(IDPNW), A(IDNEW), NODISP  ,
     +              J(IDPNT) , A(IDIFF) , NDSPX   , A(IDSPX), A(IDSTO),
     +              NVELN    , J(IVPNW) , A(IVNEW), NOVELO  , J(IVPNT),
     +              A(IVELO) , NVELX    , A(IVELX), A(IVSTO), A(IDMPS),
     +              J(ISDMP) , J(IPDMP) , NTDMPQ  , A(IDEFA), J(IPNDT),
     +              J(IPGRD) , J(IPVAR) , J(IPTYP), J(IVARR), J(IVIDX),
     +              J(IVTDA) , J(IVDAG) , J(IVTAG), J(IVAGG), J(IAPOI),
     +              J(IAKND) , J(IADM1) , J(IADM2), J(IVSET), J(IGNOS),
     +              J(IGSEG) , NOVAR    , A       , NOGRID  , NDMPS   ,
     +              C(IPRNA) , INTSRT   , J(IOWNS), J(IOWNQ), MYPART  ,
     &              j(iprvpt), j(iprdon), nrref   , j(ipror), nodef   ,
     &              surface  ,lun(19) )
C
C          communicate boundaries
C
c rvw voor domeindecompositie
         CALL DLWQ_BOUNDIO( LUN(19)  , NOTOT    ,
     +                      NOSYS    , nosss    ,
     +                      NOBND    , C(ISNAM) ,
     +                      C(IBNID) , J(IBPNT) ,
     +                      A(ICONC) , A(IBSET) ,
     +                      LCHAR(19))
!*****endif ! mypart.eq.1
C
C          set new boundaries
C
      call timer_start(timer_bound)

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
      call timer_stop(timer_bound)
C
C     Call OUTPUT system
C
      call timer_start(timer_output)
      CALL DLWQO2 ( NOTOT   , NOSSS   , NOPA    , NOSFUN  , ITIME   ,
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

      if (mypart.eq.1) then
          CALL WRITE_PROGRESS( DLWQD%PROGRESS )
      end if !(mypart.eq.1)
C
      call timer_stop(timer_output)
C
C          simulation done ?
C
      IF ( ITIME .LT. 0      ) goto 9999
      IF ( ITIME .GE. ITSTOP ) GOTO 20

         call delpar01 ( itime   , noseg   , noq     , a(ivol) , a(iflow),
     &                   nosfun  , c(isfna), a(isfun))

!          add processes

      call timer_start(timer_transport)
      CALL DLWQ14 ( A(IDERV), NOTOT   , NOSSS   , ITFACT  , A(IMAS2),
     *              IDT     , IAFLAG  , A(IDMPS), INTOPT  , J(ISDMP),
     *              J(IOWNS), MYPART )
      call timer_stop(timer_transport)
C
C          get new volumes
C
      ITIMEL = ITIME
      ITIME  = ITIME + IDT
C
      call timer_start(timer_readdata)
      CALL DLWQ41 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *              J(INRHA), J(INRH2), J(INRFT), NOSEG   , A(IVOL2),
     *              J(IBULK), LCHAR   , ftype   , ISFLAG  , IVFLAG  ,
     *              LDUMMY  , J(INISP), A(INRSP), J(INTYP), J(IWORK),
     *              LSTREC  , LREWIN  , A(IVOLL), MYPART  , dlwqd   )
      call timer_stop(timer_readdata)

!        update the info on dry volumes with the new volumes

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , nopa     , c(ipnam) , a(iparm) ,
     &                 nosfun   , c(isfna) , a(isfun) , j(iknmr) , iknmkv   )
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
C          do the transport itself
C
      call timer_start(timer_transport)
      call dlwq16 ( nosys   , notot   , nosss   , noq1    , noq2    ,
     &              noq3    , noqtt   , nddim   , nvdim   , a(idisp),
     &              a(idnew), a(ivnew), a(iarea), a(iflow), a(ileng),
     &              j(ixpnt), iknmkv  , j(idpnw), j(ivpnw), a(iconc),
     &              a(iboun), intopt  , ilflag  , idt     , a(iderv),
     &              iaflag  , a(imas2), ndmpq   , j(iqdmp), a(idmpq),
     &              j(iowns), mypart  )
      call timer_stop(timer_transport)
C
C          new time values, volumes excluded
C
      call timer_start(timer_readdata)
      IDTOLD = IDT
      CALL DLWQT0 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *              J(INRHA), J(INRH2), J(INRFT), IDT     , A(IVOL) ,
     *              A(IDIFF), A(IAREA), A(IFLOW), A(IVELO), A(ILENG),
     *              A(IWSTE), A(IBSET), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), J(IBULK), LCHAR   , C(ILUNT), ftype   ,
     *              INTSRT  , ISFLAG  , IFFLAG  , IVFLAG  , ILFLAG  ,
     *              UPDATR  , J(IKTIM), J(IKNMR), J(INISP), A(INRSP),
     *              J(INTYP), J(IWORK), .FALSE. , LDUMMY  , RDUMMY  ,
     &              .FALSE. , GridPs  , dlwqd   )
      call timer_stop(timer_readdata)
C
C          set a time step
C
      call timer_start(timer_transport)
      call dlwq18 ( nosys   , notot   , nosss   , a(ivol2), a(imass),
     &              a(iconc), a(iderv), nopa    , c(ipnam), a(iparm),
     &              nosfun  , c(isfna), a(isfun), idtold  , ivflag  ,
     &              lun(19) , j(iowns), mypart  )
      CALL MOVE   ( A(IVOL2), A(IVOL), NOSEG )
C
C          update new concentrations for subdomain boundaries
C
      call update_rdata(A(imass), notot, 'noseg', 1, 'stc1', ierror)
      call update_rdata(A(iconc), notot, 'noseg', 1, 'stc1', ierror)

      if (.false. .and. itime.ge.itstop) then
         call collect_rdata(mypart, A(iconc), notot,'noseg',1, ierror)
         call collect_rdata(mypart, A(imass), notot,'noseg',1, ierror)
!        do isys = 1, 1
!          call dumpconc(lun(19),'na dlwq18, conc',A(iconc), notot, noseg, isys)
!          call dumpconc(lun(19),'na dlwq18, mass',A(imass), notot, noseg, isys)
!        enddo
      endif
C
C          calculate closure error
C
      IF ( LREWIN .AND. LSTREC ) THEN
c collect information on master for computation of closure error before rewind
         call collect_rdata(mypart,A(IMASS), notot, 'noseg', 1, ierror)
         if (mypart.eq.1) then
            CALL DLWQCE ( A(IMASS), A(IVOLL), A(IVOL2), NOSYS , NOTOT ,
     +                    NOSEG   , LUN(19) )
         endif
         call distribute_rdata(mypart,A(IMASS),notot,'noseg',1,'distrib_itf', ierror)
         CALL MOVE   ( A(IVOLL), A(IVOL ), NOSEG )
      ENDIF
C
C          integrate the fluxes at dump segments fill ASMASS with mass
C
      IF ( IBFLAG .GT. 0 ) THEN
         CALL PROINT ( NFLUX   , NDMPAR  , IDTOLD  , ITFACT  , A(IFLXD),
     +                 A(IFLXI), J(ISDMP), J(IPDMP), NTDMPQ  )
      ENDIF
      call timer_stop(timer_transport)
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
          if (mypart.eq.1) then
C
C          close files, except monitor file
C
              call timer_start(timer_close)
              call CloseHydroFiles( dlwqd%collcoll )
              call close_files( lun )
C
C          write restart file
C
              CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITIME , C(IMNAM) ,
     &                      C(ISNAM) , NOTOT , NOSSS    )
              call timer_stop(timer_close)
          end if ! mypart.eq.1

      ENDIF
C
 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      RETURN
      END SUBROUTINE
