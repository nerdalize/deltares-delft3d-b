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

      subroutine dlwqn5 ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Flux corrected transport according to Boris and Book (FCT) (5)
!>
!>                         3D flux corrected transport according to Boris and Book.
!>                         Explicit in time. Very fast and accurate solution. Time
!>                         step size is limited by a stability criterion.

!     CREATED            : june 1988 by L. Postma

!     LOGICAL UNITS      : LUN(19) , output, monitoring file
!                          LUN(20) , output, formatted dump file
!                          LUN(21) , output, unformatted hist. file
!                          LUN(22) , output, unformatted dump file
!                          LUN(23) , restart file

!     SUBROUTINES CALLED : DLWQTR, user transport routine
!                          DLWQWQ, user waterquality routine
!                          PROCES, DELWAQ proces system
!                          DLWQO2, DELWAQ output system
!                          DLWQPP, user postprocessing routine
!                          DLWQ13, system postpro-dump routine
!                          DLWQ14, scales waterquality
!                          DLWQ15, wasteload routine
!                          DLWQ17, boundary routine
!                          DLWQ18, integration step
!                          DLWQ41, update volumes
!                          DLWQT0, update other time functions
!                          DLWQ50, transport
!                          DLWQ51, flux correction
!                          DLWQ52, makes masses and concentrations
!                          PROINT, integration of fluxes
!                          DHOPNF, opens files
!                          ZERCUM, zero's the cummulative array's
!
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

!     Arguments           :
!     type     kind  function         name                      description

      real      (4), intent(inout)   :: a      (*)              !< flat real array space
      integer   (4), intent(inout)   :: j      (*)              !< flat integer array space with indices
      character (*), intent(in   )   :: c      (*)              !< flat character array spave with names
      integer   (4), intent(in   )   :: lun    (*)              !< array with unit numbers
      character (*), intent(in   )   :: lchar  (*)              !< array with file names
      integer      , intent(in   )   :: action                  !< mode of operation
      type(delwaq_data), target      :: dlwqd                   !< data area stepwise processing
      type(GridPointerColl)          :: GridPs                  !< collection of all grid definitions

!     COMMON  /  SYSN   /   System characteristics

      INCLUDE 'sysn.inc'

!     COMMON  /  SYSI  /    Timer characteristics

      INCLUDE 'sysi.inc'

!     COMMON  /  SYSA   /   Pointers in real array workspace

      INCLUDE 'sysa.inc'

!     COMMON  /  SYSJ   /   Pointers in integer array workspace

      INCLUDE 'sysj.inc'

!     COMMON  /  SYSC   /   Pointers in character array workspace

      INCLUDE 'sysc.inc'

!     Local declarations

      logical   imflag     !  true if monitoring took place, set in dlwqo2, used in zercum
      logical   ihflag     !  true if history    took place, set in dlwqo2, used in zercum
      logical   idflag     !  true if dump       took place, set in dlwqo2, not used
      logical   lrewin     !  true if rewind     took place, set in dlwq41, used for closure error corr.
      logical   lstrec     !  true if last record at rewind wanted. set if closure error flag icflag .eq. 1
      logical   ldumm2     !  dummy logical, parameter in dlwqt0
      logical   ldummy     !  dummy logical, parameter in dlwqtr, dlwq41, dlwqt0, initialized .false.
      real      rdummy(1)  !  dummy real   , parameter in dlwqt0
      integer   iaflag     !  set 1 by dlwqo2 if accumulation, used in proces, dlwq14, dlwq15, dlwq50, dlwq51
      integer   ibflag     !  set 1 if bit 3 in intopt set, indicates that balances are required
      integer   nddim      !  eather nodisp or ndspn (last if disps are added by the proc_lib)
      integer   nvdim      !  eather novelo of nveln (last if velos are added by the proc_lib)
      integer   nopred     !  not used, in parameter list of setset
      integer   itimel     !  previous time level, used in dlwqt0 and dlwq41 for interpolation between records
      integer   itime      !  this time level
      integer   nstep      !  number of time steps (does not work if idt is time varying !)
      integer   inwtyp     !  index in the j array where wasteload types are found (not elegant)
      integer   ierr       !  error variable
      integer   ibnd       !  loop counter boundaries (loop should be in a called subroutine !)
      integer   isys       !  loop counter substances (loop should be in a called subroutine !)
      integer   ifflag     !  first-flag for dlwqt0, is zero in this routine

      integer   ithandl    !  timer handle for this routine


!     Dummy variables - used in DLWQD

      integer          :: nosss
      integer          :: noqtt
      integer          :: noqt
      integer          :: ioptzb
      integer          :: lleng
      integer          :: nowarn
      logical          :: forester
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
          ibflag  = 0
          if ( btest(intopt,3) ) ibflag = 1
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
          nosss  = noseg + nseg2
          NOQTT  = NOQ + NOQ4
          inwtyp = intyp + nobnd

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )
C
C          initialize second volume array with the first one
C
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
          if ( timon ) call timstrt ( "dlwqn5", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

      if ( timon ) call timstrt ( "dlwqn5", ithandl )
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
!*****if (mypart.eq.1) then

         call timer_start(timer_user)
         CALL DLWQTR ( NOTOT   , NOSYS   , nosss   , NOQ     , NOQ1    ,
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
     &              j(iprvpt), j(iprdon), nrref , j(ipror), nodef   ,
     &              surface  , lun(19) )
C
C          communicate boundaries
C
         CALL DLWQ_BOUNDIO( LUN(19)  , NOTOT    ,
     +                      NOSYS    , nosss    ,
     +                      NOBND    , C(ISNAM) ,
     +                      C(IBNID) , J(IBPNT) ,
     +                      A(ICONC) , A(IBSET) ,
     +                      LCHAR(19))
!*****endif
C
C          set new boundaries
C
      call timer_start(timer_bound)
      IF ( ITIME .GE. 0   ) THEN
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
      if (mypart.eq.1) then
         call write_progress( dlwqd%progress )
      end if ! (mypart.eq.1)
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
      CALL DLWQ14 ( A(IDERV), NOTOT   , nosss   , ITFACT  , A(IMAS2),
     *              IDT     , IAFLAG  , A(IDMPS), INTOPT  , J(ISDMP),
     *              J(IOWNS), MYPART )
      call timer_stop(timer_transport)
C
C          get new volumes
C
      ITIMEL = ITIME
      ITIME  = ITIME + IDT

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

!          do the transport itself

      call timer_start(timer_transport)
      call dlwq50 ( nosys   , notot   , nosss   , noqtt   , nvdim   ,
     &              a(ivnew), a(iarea), a(iflow), j(ixpnt), j(ivpnw),
     &              a(iconc), a(iboun), idt     , a(iderv), iaflag  ,
     &              a(imas2), j(iowns), mypart  )
C
C          set the first guess in array CONC2 == ITIMR
C
      call dlwq18 ( nosys   , notot   , nosss   , a(ivol2), a(imass),
     &              a(itimr), a(iderv), nopa    , c(ipnam), a(iparm),
     &              nosfun  , c(isfna), a(isfun), idt     , ivflag  ,
     &              lun(19) , j(iowns), mypart  )
C
C          exchange concentrations among neighbouring subdomains
C
      call update_rdata(A(itimr), notot,'noseg',1, 'stc2', ierr)

!          perform the flux correction on conc2 == a(itimr)

      call dlwq51 ( nosys   , notot   , nosss   , noq1    , noq2    ,
     &              noq3    , noqtt   , nddim   , nvdim   , a(idisp),
     &              a(idnew), a(ivnew), a(ivol2), a(iarea), a(iflow),
     &              a(ileng), j(ixpnt), iknmkv  , j(idpnw), j(ivpnw),
     &              a(iconc), a(itimr), a(iboun), intopt  , ilflag  ,
     &              idt     , iaflag  , a(imas2), ndmpq   , j(iqdmp),
     &              a(idmpq), j(iowns), mypart  )
      call dlwq52 ( nosys   , notot   , nosss   , a(ivol2), a(imass),
     *              a(itimr), a(iconc), j(iowns), mypart  )

      CALL MOVE   ( A(IVOL2), A(IVOL), nosss )
C
C          update new concentrations for subdomain boundaries
C
      call update_rdata(A(imass), notot,'noseg',1, 'stc1', ierr)
      call update_rdata(A(iconc), notot,'noseg',1, 'stc1', ierr)

      if (itime.ge.itstop) then
         call collect_rdata(mypart, A(iconc), notot,'noseg',1, ierr)
         call collect_rdata(mypart, A(imass), notot,'noseg',1, ierr)
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
         call collect_rdata(mypart,A(imass), notot,'noseg',1, ierr)
         if (mypart.eq.1) then
            CALL DLWQCE ( A(IMASS), A(IVOLL), A(IVOL2), NOSYS , NOTOT ,
     +                    NOSEG   , LUN(19) )
         endif
         call distribute_rdata(mypart,A(IMASS),notot,'noseg',1,'distrib_itf', ierr)
         CALL MOVE   ( A(IVOLL), A(IVOL ), NOSEG )
      ENDIF
C
C          integrate the fluxes at dump segments fill ASMASS with mass
C
      IF ( IBFLAG .GT. 0 ) THEN
         CALL PROINT ( NFLUX   , NDMPAR  , IDT     , ITFACT  , A(IFLXD),
     +                 A(IFLXI), J(ISDMP), J(IPDMP), NTDMPQ  )
      ENDIF
      call timer_stop(timer_transport)
C
C          new time values, volumes excluded
C
      call timer_start(timer_readdata)
      CALL DLWQT0 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *              J(INRHA), J(INRH2), J(INRFT), IDT     , A(IVOL) ,
     *              A(IDIFF), A(IAREA), A(IFLOW), A(IVELO), A(ILENG),
     *              A(IWSTE), A(IBSET), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), J(IBULK), LCHAR   , C(ILUNT), ftype   ,
     *              INTSRT  , ISFLAG  , IFFLAG  , IVFLAG  , ILFLAG  ,
     *              LDUMM2  , J(IKTIM), J(IKNMR), J(INISP), A(INRSP),
     *              J(INTYP), J(IWORK), .FALSE. , LDUMMY  , RDUMMY  ,
     &              .FALSE. , GridPs  , dlwqd   )
      call timer_stop(timer_readdata)
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
          call collect_rdata(mypart, A(ICONC), notot, 'noseg', 1, ierr)
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
     *                      C(ISNAM) , NOTOT , nosss    )
              call timer_stop(timer_close)
C
          endif !(mypart.eq.1)
      ENDIF
C
 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      RETURN
      END SUBROUTINE
