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

      subroutine dlwqnj ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         ADI solver of Delft3D-FLOW method (19 & 20)
!>
!>                         Performs time dependent integration according to
!>                         the Alternate Direction Implicit method of
!>                         Delft3D-FLOW (difu.f90).
!>                         Is implemented as:
!>                            - method 19 upwind discretisation of the vertical
!>                            - method 20 central discretisation of the vertical
!>                            .
!>                         Method 20 allows for the use of a Forester filter to
!>                         warantee monotoneous behaviour.

C     CREATED            : december 1995 by E. de Goede
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
C                          DLWQ41, update volumes
C                          DLWQT0, update other time functions
C                          PROINT, integration of fluxes
C                          DHOPNF, opens files
C                          SRSTOP, stops execution
C
C ROUTINES MBT TRISULA-TRANSPORTSCHEMA:
C                          DLBACK, back conversion to DELWAQ arrays
C                          DLCONV, conversion to TRISULA arrays
C                          DLDIFU, performs time step
C                          DLFLUX, computes fluxes for mass balance
C                          DLFORF, applies Forester filter
C                          DLINIT, initializes TRISULA arrays
C                          DLMASB, updates mass balance
C                          DLWSOL, print concentrations at end of simulation
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
C     ---------------------------------------------------------
C     A       REAL       *      LOCAL  real      workspace array
C     J       INTEGER    *      LOCAL  integer   workspace array
C     C       CHARACTER  *      LOCAL  character workspace array
C     LUN     INTEGER    *      INPUT  array with unit numbers
C     LCHAR   CHARACTER  *      INPUT  filenames
C
C     Declaration of arguments
C
      use grids
      use timers
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

!     Common to define external communications in SOBEK
!     olcfwq             Flag indicating ONLINE running of CF and WQ
!     srwact             Flag indicating active data exchange with SRW
!     rtcact             Flag indicating output for RTC

      logical            olcfwq, srwact, rtcact
      common /commun/    olcfwq, srwact, rtcact
      integer                     :: laatst               ! detect latest step for communication

C
C     Local declarations
C
      REAL             RDUMMY(1)
      LOGICAL          IMFLAG , IDFLAG , IHFLAG
      LOGICAL          LDUMMY , LSTREC , LREWIN , LDUMM2
      LOGICAL          FORESTER
      INTEGER          ITIME
      INTEGER          NSTEP
      INTEGER          IFFLAG
      INTEGER          IAFLAG
      INTEGER          IBFLAG
      INTEGER          NDDIM
      INTEGER          NVDIM
      INTEGER          NOWARN
      INTEGER          NOPRED
      INTEGER          ITIMEL
      INTEGER          INWTYP
      INTEGER          LLENG
      INTEGER          ICREEP
      INTEGER          ICENTR
      INTEGER          IZ
      INTEGER          IDUMMY
      REAL             RDT

      INTEGER         IBND
      INTEGER         ISYS

      real             dsdksi(1,1), dsdeta(1,1), dtdksi(1,1), dtdeta(1,1)
      real             rbnd
      real             ws
      real             adummy
      integer          kmxsed
      logical          eqmbc
      character*4      sedtyp(2)
      integer       :: ithandl

      !
      ! Variables local to this method
      !
      integer       :: i
      logical, save :: ifirst = .true.
      integer, save :: jstart
      integer, save :: nmmaxj
      real, save    :: vicmol
      real, save    :: eps

      !
      ! Dumy variables - used in DLWQD
      !
      integer       :: nosss
      integer       :: noqtt
      integer       :: noqt
      integer       :: ioptzb
      logical       :: updatr
      real(kind=kind(1.0d0)) :: tol

      include 'state_data.inc'

C ====================================================================
C SOME REMARKS:
C
C IN TRISULA QXK=FLUX IN X-DIRECTION, QYK=FLUX IN Y-DIRECTION
C IN DELWAQ FIRST DIRECTION=Y-DIRECTION; SECOND DIRECTION=X-DIRECTION
C
C IN TRISULA Z-DIRECTION IS POSITIVE UPWARDS; IN DELWAQ POSITIVE DOWNWARDS
C ====================================================================
C
      if ( action == action_finalisation ) then
          include 'dlwqdata_restore.inc'
          goto 20
      endif

      IF ( ACTION == ACTION_INITIALISATION  .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

!          some initialisation

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
         LSTREC   = ICFLAG .EQ. 1
         nosss    = noseg + nseg2
         noqtt    = noq + noq4
         inwtyp   = intyp + nobnd
         LLENG    = ILENG+NOQ1+NOQ2
         FORESTER = BTEST(INTOPT,6)
         NOWARN   = 0

         call initialise_progress( dlwqd%progress, nstep, lchar(44) )

!          initialize second volume array with the first one

!        This statement caused a stack overflow with a very large model
!        Use an explicit loop instead
!         a( ivol2 : ivol2+noseg ) = a( ivol : ivol+noseg )
         do i = 0,noseg-1
             a(ivol2+i) = a(ivol+i)
         enddo

!          initialize constant arrays for D3D-FLOW solver

         call dlinit ( lun(19)  , noseg    , noq1     , noq2     , noq      ,
     &                 a(ivol)  , nopa     , c(ipnam) , a(iparm) , ilflag   ,
     &                 a(ileng) , flowpnt  , nmax     , mmax     , kmax     ,
     &                 j(ilgra) , gsqs     , a(iguv)  , a(igvu)  , cell_x   ,
     &                 cell_y   , guu      , gvv      , j(ikcs)  , thick    ,
     &                 sig      )
         hu = 0.0
         hv = 0.0
      ENDIF


C
C     Save/restore the local persistent variables,
C     if the computation is split up in steps
C
C     Note: the handle to the timer (ithandl) needs to be
C     properly initialised and restored
C
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqnj", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

      ICREEP   = 0
      IF ( BTEST(INTOPT,7) ) ICREEP = 1
      ICENTR   = 0
      IF ( INTSRT .NE. 19 ) ICENTR = 1

      if ( timon ) call timstrt ( "dlwqnj", ithandl )
C
C======================= simulation loop ============================
C
C          set alternating set of pointers
C
   10 continue

!        Determine the volumes and areas that ran dry at start of time step

         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , nopa     ,
     &                 c(ipnam) , a(iparm) , nosfun   , c(isfna) , a(isfun) ,
     &                 j(iknmr) , iknmkv   )

C          user transport processes allowed in this version
C
      CALL DLWQTR ( NOTOT   , NOSYS   , NOSEG   , NOQ     , NOQ1    ,
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
C          PROCES subsystem allowed in this version
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

!          communicate with flow

      call waq2flow(nrvart  , c(ionam), j(iiopo), nocons  , nopa    ,
     +              nofun   , nosfun  , notot   , a(iconc), a(isfun),
     +              a(ifunc), a(iparm), a(icons), idt     , itime   ,
     +              a(ivol) , noseg   , nosys   , nodump  , j(idump),
     +              nx      , ny      , j(igrid), a(iboun), noloc ,
     +              a(iploc), nodef   , a(idefa), lun(19) )
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
      IF ( ITIME .GE. ITSTOP ) GOTO 20

      write (lun(19),*) '  '
      write (lun(19),*) '==========================================='
      iz = (itime - itstrt) / idt + 1
      write (lun(19),*) ' time step no. :',iz
C
C     INITIALIZATION OF TRISULA ARRAYS
C
      IF ( IFIRST ) THEN
         jstart = 1 - 2 * NMAX
         nmmaxj = ( 2 + MMAX ) * NMAX
         kadu   = 1
         kadv   = 1
         kcu    = 0
         r11    = 0.0
         sour   = 0.0
         sink   = 0.0
         s0     = 0.0
         s1     = 0.0
         sigdif = 1.0
         sigmol = 1.0
         vicmol = 0.0
         dicuv  = 0.0
         dicww  = 0.0
         IFIRST = .FALSE.
         eqmbc  = .false.
         eps    = 1.0e-20
      ENDIF

!          add processes

      CALL DLWQ14 ( A(IDERV), NOTOT   , NOSEG   , ITFACT  , A(IMAS2),
     *              IDT     , IAFLAG  , A(IDMPS), INTOPT  , J(ISDMP),
     *              J(IOWNS), MYPART )

!          get new volumes

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

!        add the waste loads

      call dlwq15 ( nosys    , notot    , noseg    , noq      , nowst    ,
     &              nowtyp   , ndmps    , intopt   , idt      , itime    ,
     &              iaflag   , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &              a(iflow ), j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &              j(inwtyp), j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &              iknmkv   , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &              c(isfna ), a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &              a(iwdmp) , 1        , notot    , j(iowns ), mypart   )

!        conversion of Delwaq arrays to FLOW arrays

      call dlconv ( noseg    , noq1     , noq2     , noq3     , noq      ,
     &              nobnd    , nosys    , notot    , intsrt   , intopt   ,
     &              ilflag   , nvdim    , nddim    , j(ivpnw) , j(idpnw) ,
     &              a(ivol)  , a(ivol2) , a(iarea) , a(iflow) , a(ileng) ,
     &              a(ivnew) , a(idisp) , a(idnew) , a(iconc) , a(iboun) ,
     &              a(iderv) , cellpnt  , flowpnt  , nmax     , mmax     ,
     &              kmax     , j(ilgra) , gsqs     , a(iguv)  , a(igvu)  ,
     &              thick    , a(ivola) , a(ivolb) , dps      , s1       ,
     &              a(iddkl) , a(ir1)   , a(iqxk)  , a(iqyk)  , a(iqzk)  ,
     &              a(idifx) , a(idify) , a(idifz) , dicuv    , dicww    ,
     &              areau    , areav    , a(iaakl) , a(ibbkl) , a(icckl) ,
     &              j(ikcs)  , j(ikfu)  , j(ikfv)  , j(ikfs)  )

      rdt = idt * 1.0
      if ( mod((itime-itstrt)/idt,2) .eq. 1 ) then

         write ( lun(19), * ) 'implicit in x-direction'
         call dldifu ( icreep   , rdt      , lun(19)  , 0        , nmax     ,
     &                 1        , jstart   , nmmaxj   , nmax*mmax, kmax     ,
     &                 nosys    , nosys    , 0        , 0        , 0        ,
     &                 0        , 0        , 0        , 0        , idummy   ,
     &                 j(ikcs)  , kcu      , j(ikfs)  , j(ikfu)  , j(ikfv)  ,
     &                 kadu     , kadv     , s0       , s1       , hu       ,
     &                 hv       , dps      , a(iqxk)  , a(iqyk)  , a(iqzk)  ,
     &                 guu      , gvv      , a(iguv)  , a(igvu)  , gsqs     ,
     &                 rbnd     , sigdif   , sigmol   , a(ir1)   , r11      ,
     &                 sour     , sink     , ws       , sedtyp   , thick    ,
     &                 sig      , dicuv    , dicww    , dsdksi   , dsdeta   ,
     &                 dtdksi   , dtdeta   , a(iaak)  , a(ibbk)  , a(icck)  ,
     &                 a(ibd3x) , a(ibddx) , a(ibdx)  , a(ibux)  , a(ibuux) ,
     &                 a(ibu3x) , a(iwrk1) , a(iwrk2) , areau    , areav    ,
     &                 a(iaakl) , a(ibbkl) , a(icckl) , a(iddkl) , kmxsed   ,
     &                 eqmbc    , eqmbc    , adummy   , a(ivola) , a(ivolb) ,
     &                 rscale   , adummy   , eps      , vicmol   , a(idifx) ,
     &                 a(idify) , icentr   , dfluxx   , dfluxy   )

!        Applies forester filter

!             CALL DLFORF (
!     *              LUN(19) ,
!     *              NMAX    , 1       , JSTART  , NMMAXJ  ,
!     *              NMAX*MMAX,KMAX    ,
!     *              J(IKCS) , J(IKFS) , J(IKFU) , J(IKFV) ,
!     *              r11     ,
!     *              A(IWRK1), A(IWRK2),
!     *              NOSYS   , NOTOT   , A(IVOLB)          )

!        Computes fluxes for mass balance

         call dlflux ( jstart   , nmmaxj   , nmax*mmax, kmax     , nosys    ,
     &                 notot    , nmax     , 1        , intsrt   , icreep   ,
     &                 j(ikfu)  , j(ikfv)  , j(ikfs)  , j(ikcs)  , kadu     ,
     &                 kadv     , a(iqxk)  , a(iqyk)  , a(iqzk)  , a(idifx) ,
     &                 a(idify) , a(idifz) , a(ir1)   , r11      , a(iguv)  ,
     &                 a(igvu)  , dicww    , areau    , areav    , gsqs     ,
     &                 s0       , dps      , thick    , sigdif   , sigmol   ,
     &                 vicmol   , dfluxx   , dfluxy   , a(iaakl) , a(ibbkl) ,
     &                 a(icckl) )

      else

         write ( lun(19), * ) 'implicit in y-direction'
         call dldifu ( icreep   , rdt      , lun(19)  , 0        , 1        ,
     &                 nmax     , jstart   , nmmaxj   , nmax*mmax, kmax     ,
     &                 nosys    , nosys    , 0        , 0        , 0        ,
     &                 0        , 0        , 0        , 0        , idummy   ,
     &                 j(ikcs)  , kcu      , j(ikfs)  , j(ikfv)  , j(ikfu)  ,
     &                 kadv     , kadu     , s0       , s1       , hv       ,
     &                 hu       , dps      , a(iqyk)  , a(iqxk)  , a(iqzk)  ,
     &                 gvv      , guu      , a(igvu)  , a(iguv)  , gsqs     ,
     &                 rbnd     , sigdif   , sigmol   , a(ir1)   , r11      ,
     &                 sour     , sink     , ws       , sedtyp   , thick    ,
     &                 sig      , dicuv    , dicww    , dsdksi   , dsdeta   ,
     &                 dtdksi   , dtdeta   , a(iaak)  , a(ibbk)  , a(icck)  ,
     &                 a(ibd3x) , a(ibddx) , a(ibdx)  , a(ibux)  , a(ibuux) ,
     &                 a(ibu3x) , a(iwrk1) , a(iwrk2) , areav    , areau    ,
     &                 a(iaakl) , a(ibbkl) , a(icckl) , a(iddkl) , kmxsed   ,
     &                 eqmbc    , eqmbc    , adummy   , a(ivola) , a(ivolb) ,
     &                 rscale   , adummy   , eps      , vicmol   , a(idify) ,
     &                 a(idifx) , icentr   , dfluxx   , dfluxy              )
C
C APPLIES FORESTER FILTER
C
!             CALL DLFORF (
!     *              LUN(19) ,
!     *              NMAX    , 1       , JSTART  , NMMAXJ  ,
!     *              NMAX*MMAX,KMAX    ,
!     *              J(IKCS) , J(IKFS) , J(IKFU) , J(IKFV) ,
!     *              r11     ,
!     *              A(IWRK1), A(IWRK2),
!     *              NOSYS   , NOTOT   , A(IVOLB)          )

!        Computes fluxes for mass balance

         call dlflux ( jstart   , nmmaxj   , nmax*mmax, kmax     , nosys    ,
     &                 notot    , 1        , nmax     , intsrt   , icreep   ,
     &                 j(ikfv)  , j(ikfu)  , j(ikfs)  , j(ikcs)  , kadv     ,
     &                 kadu     , a(iqyk)  , a(iqxk)  , a(iqzk)  , a(idify) ,
     &                 a(idifx) , a(idifz) , a(ir1)   , r11      , a(igvu)  ,
     &                 a(iguv)  , dicww    , areav    , areau    , gsqs     ,
     &                 s0       , dps      , thick    , sigdif   , sigmol   ,
     &                 vicmol   , dfluxy   , dfluxx   , a(ibbkl) , a(iaakl) ,
     &                 a(icckl) )

      endif

!        Computations for mass balance

      call dlmasb ( rdt      , nmax     , mmax     , kmax     , noq1     ,
     &              noq2     , noq3     , noq      , nosys    , notot    ,
     &              j(ilgra) , flowpnt  , a(iaakl) , a(ibbkl) , a(icckl) ,
     &              a(iarea) , a(iguv)  , a(igvu)  , dps      , thick    ,
     &              a(ir1)   , r11      , nvdim    , j(ivpnw) , a(ivnew) ,
     &              nddim    , j(idpnw) , a(idnew) , iaflag   , a(imas2) ,
     &              ndmpq    , j(iqdmp) , a(idmpq) )

!        Back conversion

      call dlback ( rdt      , nmax     , mmax     , kmax     , j(ilgra) ,
     &              noseg    , nosys    , notot    , gsqs     , r11      ,
     &              a(ivol)  , a(ivol2) , a(iconc) , a(imass) , a(iderv) )
C
C       Forester filter on the vertical
C
      IF ( FORESTER .AND. INTSRT .EQ. 19 ) THEN
         CALL DLWQD2 ( LUN(19) , NOSYS   , NOTOT   , NOSEG   , NOQ3    ,
     *                 KMAX    , A(ICONC), A(LLENG), NOWARN  , J(IOWNS),
     *                 MYPART )
      ENDIF
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
     +                 A(IFLXI), J(ISDMP), J(IPDMP), NTDMPQ  )
      ENDIF

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

      if ( olcfwq .or. srwact ) then
         call putpev ( 'WQtoWQI', 'DataWQtoWQI', laatst )
         call getper ( 'WQItoWQ', 'DataWQItoWQ' )
      endif
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
     *                  C(ISNAM) , NOTOT , NOSEG    )
      ENDIF
 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      RETURN
C
      END
