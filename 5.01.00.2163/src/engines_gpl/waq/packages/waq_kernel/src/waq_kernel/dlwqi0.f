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

      module dlwqi0_mod
      contains
      subroutine dlwqi0 ( nlun   , a      , j      , c      , imaxa  ,
     &                    imaxi  , imaxc  , ipage  , lun    , lchar  ,
     &                    filtype, gridps , dlwqd  , ierr   )

!          Deltares Software Centre

!>\File
!>                          Initializes all start conditions for simulation
!>
!>                          Performs:
!>                             - calls SPACE to allocate space for all arrays
!>                             - calls DLWQI2 to initialize all fixed conditions
!>                             - calls DLWQIP to initialize all processes subsystem
!>                             - calls DLWQIV for unclear reasons
!>                             - calls DLWQIO to initialize the output system
!>                             - imports all grid informations and exchange tables
!>                             - calls DLWQT0 to initialize all time dependent variables
!>                             - calls DLWQTD to initialize the water bed layers
!>                             - imports initial conditions

C     CREATED: may -1988 by L. Postma
C
C     LOGICAL UNITNUMBERS : LUN( 2) - system intermediate file
C                           LUN(19) - monitoring output file
C
C     SUBROUTINES CALLED  : SPACE , initialises common blocks
C                           DLWQI2, initialises fixed conditions
C                           DLWQIP, initialises proces system
C                           DLWQIO, initialises output system
C                           DLWQT0, sets time functions
C                           DHOPNF, opens files
C                           MOVE  , copy's arrays
C                           ZERO  , zeros an real arrays
C
      use grids
      use waqmem
      use delwaq2_data
      use timers
      use m_couplib
      use workspace

!     Parameters          :

!     kind           function         name            description

      integer              , intent(in   ) :: nlun          !< Number of files
      real                 , pointer       :: a(:)          !< Real      model workspace
      integer              , pointer       :: j(:)          !< Integer   model workspace
      character(*)         , pointer       :: c(:)          !< Character model workspace
      integer              , intent(inout) :: imaxa         !< dimension   A-array
      integer              , intent(inout) :: imaxi         !< dimension   J-array
      integer              , intent(inout) :: imaxc         !< dimension   C-array
      integer              , intent(in   ) :: ipage         !< pagelength of the output file
      integer              , intent(in   ) :: lun    (nlun) !< array with unit numbers
      character(*)         , intent(in   ) :: lchar  (nlun) !< filenames
      integer              , intent(in   ) :: filtype(nlun) !< type of file
      type(gridpointercoll), intent(out)   :: gridps        !< collection off all grid definitions
      type(delwaq_data)    , intent(inout) :: dlwqd         !< derived type for persistent storage
      integer              , intent(inout) :: ierr          !< error count

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
C     Local declaration
C
      REAL          RDUMMY(1)
      LOGICAL       LDUMMY    , UPDATR
      CHARACTER*200 FINAM
      INTEGER       SENDBUF(3)

      INTEGER       IERRIO

      logical, save :: init_ixset = .true.

c     Common to define external communications in SOBEK
c     OLCFWQ             Flag indicating ONLINE running of CF and WQ
c     SRWACT             Flag indicating active data exchange with SRW
c     RTCACT             Flag indicating output for RTC
c     DDWAQ              Flag indicating parallel computation

      LOGICAL            OLCFWQ, SRWACT, RTCACT, DDWAQ, propor
      COMMON /COMMUN/    OLCFWQ, SRWACT, RTCACT, DDWAQ
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqi0", ithandl )

!         initialise the system

      ftype = filtype
      CALL SPACE  ( LUN(19) , .TRUE.  , A       , J       , C       ,
     +              IMAXA   , IMAXI   , IMAXC   )
C
C     copy common to (possible) shared array to share these values with
C     other processes (domain decomposition)
C
      CALL DHISYS ( J(ISYSI), J(ISYSN) )
C
      J(ILP  ) = IPAGE
      J(ILP+1) =    10
      J(ILP+4) = IPAGE
      J(ILP+5) =    20

!     number of all segments - including segments at the bed

      nosss = noseg + nseg2                ! nseg2 are bed-volumes
      noqtt = noq + noq4
C
C         initialisation of info from the system file
C
      CALL DHOPNF ( LUN(2) , LCHAR(2) , 2    , 2    , IERRD  )
      CALL DLWQI2 ( LUN     , C(IMNAM), C(ISNAM), J(IDUMP), C(IDNAM),
     *              J(IDPNT), J(IVPNT), A(IDISP), J(IBPNT), C(IBNID),
     *              C(IBNAM), C(IBTYP), J(INTYP), J(IWAST), iwstkind,
     *              C(IWSID), C(IWNAM), C(IWTYP), A(ILENG), A(ICONS),
     *              A(IPARM), J(INRFT), J(INRH2), C(ICNAM), C(IPNAM),
     *              C(IFNAM), C(ISFNA), C(IDINA), C(IVNAM), J(IKNMR),
     *              C(IDANA), J(IPDMP), J(IQDMP), J(ISDMP), C(IRNAM),
     *              J(IORAA), J(NQRAA), J(IQRAA), J(IGNOS), J(IGREF),
     *              J(IGSEG), gridps  , J(IDMPB), dlwqd )
      CLOSE ( LUN(2) )

      IF ( OLCFWQ ) THEN
c     Synchronizing with CF(0) for on-line mode outside SRW only
c     This step is lacking in the original SRW version of SOBEK-RE
c     ERRONEOUSLY!!

c         write (*,*) ' GETPER CFtoWQ'
          call getpcf('CFtoWQ','DataCFtoWQ')
c         write (*,*) ' DONE '
c         write (*,*) ' Received permission to Start WQ i=0 '
      ENDIF

      IF ( OLCFWQ .OR. SRWACT ) THEN
c         Pass the stick to WQInt and wait for it to come back! (0)
c         write (*,*) ' Start WQI i=0'
c          read  (*,*)
c         write (*,*) ' PUTPEV WQtoWQI'
          call putpev ('WQtoWQI','DataWQtoWQI',0)
c         write (*,*) ' DONE '
c         write (*,*) ' GETPER WQItoWQ'
          call GETPER ('WQItoWQ','DataWQItoWQ')
c         write (*,*) ' DONE '
c         write (*,*) ' Stop WQI i=0 '
c          read  (*,*)
      ENDIF
C
C     open binary system files for new input processing, if any
C
      CALL DHOPNF ( LUN(41) , LCHAR(41) , 41   ,  1   , IERRD  )
      IF ( IERRD .EQ. 0 ) THEN
         DO I = 1 , NUFIL
            READ ( LUN(41) , * ) iftyp, FINAM
            CALL DHOPNF ( 800+I , FINAM , 3 , 2+iftyp , IOERR )
            IF ( IOERR .NE. 0 ) THEN
               WRITE ( LUN(19) , '(A,I3,A,A)' )
     *         ' ERROR opening file on unit: ',800+I,' filename: ',FINAM
               CALL SRSTOP(1)
            ENDIF
            ICLEN = LEN(FINAM)
            DO IC = 1 , MIN(ICLEN,200)
               C(ILUNT+(I-1)*200+IC-1) = FINAM(IC:IC)
            ENDDO
         ENDDO
         CLOSE( LUN(41) )
      ENDIF
C
C     initialisation of PROCES subsytem
C
      IF ( NPROC .GT. 0 ) THEN
         CALL DHOPNF (LUN(24) , LCHAR(24), 24      , 2        , IERRD   )
         CALL DLWQIP (LUN(24) , LCHAR(24), LUN(19) , NOTOT    , NIPMSA  ,
     +                NPROC   , NOLOC    , NFLUX   , NODEF    , J(INSVA),
     +                J(IIFLU), J(IPVAR) , J(IPTYP), A(IDEFA) , A(ISTOC),
     +                C(IPRNA), J(IIMOD) , IERR    , IPBLOO   , IPCHAR  ,
     +                IOFFBL  , IOFFCH   , NOSYS   , NDSPX    , NVELX   ,
     +                A(IDSTO), A(IVSTO) , NDSPN   , J(IDPNW) , NVELN   ,
     +                J(IVPNW), NLOCX    , J(IPGRD), J(IPNDT) , NOVAR   ,
     +                J(IVARR), J(IVIDX) , J(IVTDA), J(IVDAG) , J(IVTAG),
     +                J(IVAGG), nrref    , J(ipror), j(iprvpt))
         CLOSE ( LUN(24) )
      ENDIF
C
C     Set variable "structure"
C
      CALL DLWQIV ( LUN(19) , NOCONS  , NOPA    , NOFUN   , NOSFUN  ,
     +              NOSYS   , NOTOT   , NODISP  , NOVELO  , NODEF   ,
     +              NOLOC   , NDSPX   , NVELX   , NLOCX   , NFLUX   ,
     +              NOPRED  , NOVAR   , J(IVARR), J(IVIDX), J(IVTDA),
     +              J(IVDAG), J(IVTAG), J(IVAGG), NOGRID  , J(IVSET))
C
C     initialisation of OUTPUT subsytem
C

      IF ( NOUTP .GT. 0 ) THEN
         CALL DHOPNF ( LUN(25) , LCHAR(25), 25      , 2      , IERRD   )
         CALL DLWQIO ( LUN(25) , LCHAR(25), LUN(19) , NOUTP   , NRVART,
     +                 NBUFMX  , J(IIOUT) , J(IIOPO), C(IONAM), LUN   ,
     +                 LCHAR   , MYPART   , IERR    )
         CLOSE ( LUN(25) )
      ENDIF
C
C         initialisation of the grid layout
C
      IF ( NX*NY .GT. 0 ) THEN
         CALL DHOPNF ( LUN(6) , LCHAR(6) , 6    , 2    , IERRD  )
         READ  ( LUN( 6) ) (J(K),K=IGRID,IGRID+NX*NY-1)
         CLOSE ( LUN( 6) )
      ENDIF
C
C         initialisation of exchange pointers
C
      CALL DHOPNF ( LUN(8) , LCHAR(8) , 8    , 2+ftype(8), IERRD  )

      if ( intsrt .eq. 19 .or. intsrt .eq. 20 .or. nmax*mmax .gt. 0 ) then

!        read grid, make pointer table

         i1 = ilgra-1
         read  ( lun( 8) ) nmax2,mmax2,noseg2,kmax2,noq1d,noq2d,noq3d
         read  ( lun( 8) ) ( j(i1+k), k=1,mmax*nmax )
         i2 = ikbnd-1
         if ( intsrt .eq. 19 .or. intsrt .eq. 20 ) then
            do k =1 , mmax*nmax
               if ( j(i1+k) .lt. 0 ) then
                  i3 = -j(i1+k)
                  j(i2+i3) = k
               endif
            enddo
         endif
         call makpnt( nmax    , mmax    , kmax    , noseg   , nobnd   ,
     &                noq     , noq1    , noq2    , j(ilgra), j(ixpnt),
     &                cellpnt , flowpnt )
         finam = lchar(8)(1:index(lchar(8),'.',.true.))//'cco'
         call dhopnf ( lun(8), finam, 8, 2+ftype(8), ierrd )
         read ( lun(8) )
         read ( lun(8) ) mmax2, nmax2, x0, y0, beta, np2, nlay
         do i = 1, 2*np2 + 9
            read( lun(8) ) dummy
         enddo
         read ( lun(8) ) cell_x
         read ( lun(8) ) cell_y
      else

!        Read pointer table with first index 4

         I1 = IXPNT-1
         do i = 1, noqtt
            READ ( LUN( 8 ) ) ( J(I1+K+(i-1)*4),K=1,4 )
         enddo
      ENDIF
      CLOSE ( LUN( 8) )
C
C     determine mesh/grid partitioning for parallel computing
C
      CALL PARTIT ( LUN(19) , NOSSS   , NOLAY   , NOQTT   , J(IXPNT),
     +              MYPART  , NPARTp  , J(IOWNS), J(IOWNQ), INTSRT  )

      IBFLAG = 0
      IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1

      if ( init_ixset ) then
          init_ixset = .false.
          CALL IXSETS ( LUN(19) , MYPART  , NOTOT   , NOSYS   ,
     +                  NOSEG   , NOQ     , J(IXPNT), J(IOWNS),
     +                  J(IOWNQ), NDMPAR  , NDMPS   , NTDMPQ  ,
     +                  NDMPQ   , IBFLAG  , J(ISDMP), J(IPDMP),
     +                  J(IQDMP) )
      endif

C
C     locally/per processor adapt the feature array:
C        feature 1 == segment is active segment of own subdomain or not
C        feature 2 == position w.r.t. the vertical direction
C        feature 3 == segment is active segment of global domain or not
C        feature 4 == segment belongs to own processor
C

      CALL CHKNMR ( LUN(19) , MYPART , nosss  , J(IOWNS) , J(IKNMR) )

      IF ( RTCACT )
c     Interface to RTC (0)
     Jcall RTCSHL (ITSTRT, A, J, C)

      IF ( SRWACT )
C     Interface to SRW (0)
     JCALL SRWSHL (ITSTRT, A, J, C)

      IF ( OLCFWQ ) THEN
c     Synchronizing with CF(0) for on-line mode outside SRW only
c         write (*,*) ' Stop WQ i=0 '
c          read  (*,*)
c         write (*,*) ' PUTPER WQtoCF'
          call putpcf('WQtoCF','DataWQtoCF')
c         write (*,*) ' DONE '
c     Synchronizing with CF(1) for on-line mode outside SRW only
c         write (*,*) ' GETPER CFtoWQ'
          call getpcf('CFtoWQ','DataCFtoWQ')
c         write (*,*) ' DONE '
c         write (*,*) ' Start WQ i=1 '
c          read  (*,*)
      ENDIF

C         first read of relevant time varying arrays
C
      IFFLAG = 1

      IF ( SRWACT .OR. OLCFWQ ) THEN
c     Pass the stick to WQInt and wait for it to come back! (1)
c         write (*,*) ' Start WQI i=1'
c          read  (*,*)
c         write (*,*) ' PUTPEV WQtoWQI'
          call putpev ('WQtoWQI','DataWQtoWQI',0)
c         write (*,*) ' DONE '
c         write (*,*) ' GETPER WQItoWQ'
          call GETPER ('WQItoWQ','DataWQItoWQ')
c         write (*,*) ' DONE '
c         write (*,*) ' Stop WQI i=1 '
c          read  (*,*)
      ENDIF

      CALL DLWQT0 ( LUN     , ITSTRT  , ITIMEL  , A(IHARM), A(IFARR),
     *              J(INRHA), J(INRH2), J(INRFT), IDT     , A(IVOL) ,
     *              A(IDIFF), A(IAREA), A(IFLOW), A(IVELO), A(ILENG),
     *              A(IWSTE), A(IBSET), A(ICONS), A(IPARM), A(IFUNC),
     *              A(ISFUN), J(IBULK), LCHAR   , C(ILUNT), ftype   ,
     *              INTSRT  , ISFLAG  , IFFLAG  , IVFLAG  , ILFLAG  ,
     *              UPDATR  , J(IKTIM), J(IKNMR), J(INISP), A(INRSP),
     *              J(INTYP), J(IWORK), .FALSE. , LDUMMY  , RDUMMY  ,
     &              .TRUE.  , gridps  , DLWQD   )

!         Particle tracking

      call delpar00 ( lchar(45), noseg  , noq    , a(ivol), a(iflow))

      if (mypart .eq.1) then
C
C     New bottomlayer processing
C
         IF ( NOQ4 .GT. 0 )
     *        CALL DLWQTD ( LUN     , NOSEG   , NSEG2   , NOLAY   , NOGRID  ,
     *                      NOQ     , NOQ4    , J(IGREF), J(IGSEG), NOCONS  ,
     *                      NOPA    , NOFUN   , NOSFUN  , A(ICONS), C(ICNAM),
     *                      A(IPARM), C(IPNAM), A(IFUNC), C(IFNAM), A(ISFUN),
     *                      C(ISFNA), J(IXPNT), A(IVOL ), A(IAREA), A(IFLOW),
     *                      A(ILENG))
C
      end if

      IF ( INTSRT .EQ. 6 .OR. INTSRT .EQ. 7 ) THEN
         NOSUBz = NOTOT
      ELSE
         NOSUBz = NOSYS
      ENDIF
      CALL MOVE   ( A(IBSET), A(IBOUN), NOBND*NOSUBz )
      CALL MOVE   ( A(IBSET), A(IBSAV), NOBND*NOSUBz )
      CALL ZERO   ( A(IDERV), NOTOT*NOSSS )
      CALL ZERO   ( A(IMAS2), NOTOT*5     )
      CALL ZERO   ( A(IWDMP), NOTOT*NOWST*2  )
      IF ( MOD(INTOPT,16) .GT. 7 ) THEN
         CALL ZERO( A(IDMPQ), NOSYS*NDMPQ*2  )
         CALL ZERO( A(IDMPS), NOTOT*NDMPS*3  )
         CALL ZERO( A(ISMAS), NOTOT*NDMPAR*6 )
         CALL ZERO( A(IFLXI), NDMPAR*NFLUX   )
         CALL ZERO( A(IFLXD), NDMPS*NFLUX    )
         CALL ZERO( A(ITRRA), NOSYS*NORAAI   )
      ENDIF

!         initial conditions

      propor = .false.
      call dhopnf ( lun(18) , lchar(18) , 18    , 2    , ierrd  )
      ig = scan ( lchar(18), '.', back = .true. )                ! look fo rthe file type
      if ( lchar(18)(ig:ig+3) .eq. '.map' ) then                 ! if .map, it is a map-file
         read ( lun(18), iostat=ierrio ) finam(1:160)            ! read title of simulation
         if ( ierrio .ne. 0 ) goto 50
         if ( finam(114:120) .eq. 'mass/m2' ) propor = .true.    !  at end of third line ...
         read ( lun(18) ) idummy                                 ! should be nr. of substance
         if ( idummy .ne. notot ) then
            write ( lun(19), '(a,a,/,a,i10)' )
     &        ' ERROR reading initial conditions - filename: ', lchar(18),
     &        ' Number of substances does not match : ', idummy
            call srstop(1)
         endif
         read ( lun(18) ) idummy                                 ! should be nr. of comp. volumes
         if ( idummy .ne. nosss ) then
            write ( lun(19), '(a,a,/,a,i10)' )
     &        ' ERROR reading initial conditions - filename: ', lchar(18),
     &        ' Number of computational volumes does not match : ', idummy
            call srstop(1)
         endif
         do i = 1, notot
            read ( lun(18) ) finam(1:20)
         enddo
      endif
      read  ( lun(18) , iostat = ierrio )                  ! like the .ini, the .res and .wrk file
     *       idummy , ( a(k) , k=iconc,iconc+notot*nosss-1 )
   50 if ( ierrio .ne. 0 ) then
          write ( lun(19) , '(a,a)' )
     *        ' ERROR reading initial conditions - filename: ',
     *        lchar(18),
     *        ' Too few data - file contents does not match current '//
     *        'model'
          call srstop(1)
      else
          read  ( lun(18) , iostat = ierrio ) idummy
          if ( ierrio .eq. 0 ) then
              write ( lun(19) , '(a,a)' )
     *            ' ERROR reading initial conditions - filename: ',
     *            lchar(18),
     *            ' Too many data - file contents does not match ' //
     *            'current model'
              call srstop(1)
          endif
      endif
      close ( lun(18) )

!         make start masses for dynamic and iterative computation

      if ( intsrt .eq.  6 .or. intsrt .eq.  7 .or.
     &     intsrt .eq. 17 .or. intsrt .eq. 18 ) goto 40

!         initial conditions coflowing substances

      do iseg = 0 , nosss-1
         volume   = a(ivol +iseg)
         do i1 = iseg*notot   , iseg*notot+nosys-1
            a(imass+i1) = a(iconc+i1) * volume
         enddo
      enddo

!         initial conditions passive substances

      if ( nosys .ne. notot ) then                         ! if there are bed-substances
         call zoek20 ( 'SURF      ',nopa  ,c(ipnam),10,indx )
         if ( indx .gt. 0 ) then                           ! and if SURF is found
            call inact ( nosss   , nosys   , notot    , a(iconc:)   , a(imass:),
     &                   nopa    , indx    , a(iparm:), c(imnam+113), propor   ,
     &                   .true.  )
         else                                     ! routine inact is at end of this file !
            call zoek20 ( 'SURF      ', nosfun, c(isfna), 10, indx )
            if ( indx .gt. 0 ) then                        ! and if SURF is found
               call inact ( nosss   , nosys   , notot    , a(iconc:)   , a(imass:),
     &                      nosfun  , indx    , a(isfun:), c(imnam+113), propor   ,
     &                      .false. )
            else
               write ( lun(19) , '(a,a)' )              !   not found
     &             ' Error reading initial conditions: ',
     &             ' horizontal surface area not found! '
               call srstop(1)
            endif
         endif
      endif

!         deal with z-layers (inactive cells at the bottom side of the water column

!     open  ( 336, file='kenmerk-test1.out', recl=8575 )
!     write ( 336, '(2143i4)' ) (mod(j(iknmr+k),1000),k=0,noseg-1)
!     open  ( 337, file='frmto-test1.out', recl=8575 )
!     write ( 337, '(2143i4)' ) (mod(j(ixpnt+k),1000),k=(noq1+noq2)*4  ,noq*4-1,4)
!     write ( 337, * )
!     write ( 337, '(2143i4)' ) (mod(j(ixpnt+k),1000),k=(noq1+noq2)*4+1,noq*4-1,4)
      call zlayer ( noseg    , nosss    , nosys    , notot    , nolay    ,
     &              a(ivol)  , noq1+noq2, noq      , a(iarea) , nocons   ,
     &              c(icnam) , a(icons) , nopa     , c(ipnam) , a(iparm) ,
     &              nosfun   , c(isfna) , a(isfun) , a(iconc) , a(imass) ,
     &              j(iknmr) , iknmkv   , j(ixpnt) )
!     open  ( 339, file='kenmerk-test2.out', recl=8575 )
!     write ( 339, '(2143i4)' ) (mod(iknmkv(k,1),1000),k=1,noseg)
!     open  ( 340, file='frmto-test2.out', recl=8575 )
!     write ( 340, '(2143i4)' ) (mod(j(ixpnt+k),1000),k=(noq1+noq2)*4  ,noq*4-1,4)
!     write ( 340, * )
!     write ( 340, '(2143i4)' ) (mod(j(ixpnt+k),1000),k=(noq1+noq2)*4+1,noq*4-1,4)

!     temporary for closure error

   40 CALL ZOEK20 ( 'CLOSE_ERR ',NOCONS,C(ICNAM),10,INDX )
      IF ( INDX .GT. 0 ) THEN
         ICFLAG = 1
         WRITE(LUN(19),*) ' Closure error correction enabled'
      ELSE
         ICFLAG = 0
         WRITE(LUN(19),*) ' Closure error correction disabled'
      ENDIF

      if ( timon ) call timstop ( ithandl )
      RETURN
      END subroutine

      subroutine inact ( noseg  , nosys  , notot  , conc   , amass  ,
     &                   nopa   , iparm  , parm   , string , propor ,
     &                   direct )
!>\File
!>         Makes mass/gridcell from mass/m2 for the passive substances

      implicit none

      integer  (4), intent(in   ) :: noseg              !< number of computational volumes
      integer  (4), intent(in   ) :: nosys              !< number of transported substances
      integer  (4), intent(in   ) :: notot              !< total number of substances
      real     (4), intent(inout) :: conc (notot,noseg) !< the concentration values
      real     (4), intent(  out) :: amass(notot,noseg) !< the mass values
      integer  (4), intent(in   ) :: nopa               !< number of parameters or segment functions
      integer  (4), intent(in   ) :: iparm              !< selected parameter
      real     (4), intent(in   ) :: parm (nopa *noseg) !< parameter or segment function array
      character(7), intent(  out) :: string             !< model docu substring
      logical     , intent(in   ) :: propor             !< if .true. then /m2 in the input
      logical     , intent(in   ) :: direct             !< if .false. segments is first index

      integer iseg   ! loop counter computational volumes
      integer isys   ! loop counter modelled substances
      real(4) surf   ! help variable
      integer indx   ! index

      string = 'mass/m2'                                ! always in the output
      if ( direct ) then
         indx =  iparm                    ! parameter
      else
         indx = (iparm-1)*noseg + 1       ! segment function
      endif
      do iseg = 1, noseg
         surf = parm( indx )
         do isys = nosys+1, notot
            if ( propor ) then                                ! input / m2
               amass(isys,iseg) = conc(isys,iseg) * surf
            else                                              ! input / gridcell
               amass(isys,iseg) = conc(isys,iseg)
               conc (isys,iseg) = conc(isys,iseg) / surf      ! conc  / m2
            endif
         enddo
         if ( direct ) then
            indx =  indx + nopa
         else
            indx =  indx + 1
         endif
      enddo

      return
      end subroutine
      end module dlwqi0_mod
