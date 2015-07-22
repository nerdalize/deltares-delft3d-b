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

      PROGRAM DLWQ00

!     Deltares Software Centre

!>\file
!>                    DELWAQ - INPUT PROGRAMME
!>
!>                    Reads the DELWAQ inputfiles and generates
!>                    a consistent set of binairy intermediate files.

C     INFORMATION   : Deltares
C                     L. Postma,
C                     Rotterdamse weg 185,
C                     P.O. Box 177,
C                     2600 MH Delft,
C                     Netherlands.
C                     telephone (31) 15-569353
C                     telefax   (31) 15-619674
C
C     LOGICAL UNITS : LUN(29), output, formatted report file
C                     LUN( 1), output, binary common-block file
C                     LUN( 2), output, binary system file
C
C     SUBROUTINES CALLED :*UNLOCK, unlocks user dependent data
C                         *UNISET, reads input filename
C                          DLWQ01, reads block 1 of user data
C                          DLWQ02, reads block 2 of user data
C                          DLWQ03, reads block 3 of user data
C                          DLWQ04, reads block 4 of user data
C                          DLWQ05, reads block 5 of user data
C                          DLWQ06, reads block 6 of user data
C                          DLWQ07, reads block 7 of user data
C                          DLWQ7A, reads block 7 of user data new style
C                          DLWQ08, reads block 8 of user data
C                          DLWQ09, reads block 9 of user data
C                          DLWQS1, reads block 10 , statistical definition
C                          DLWQP1, proces pre-processor
C                          SPACE , computes space needed
C                          DLWQDI, writes dimensions of arrays for DELWAQ2
C                         *DHOPNF, opens files ( if neccesary )
C                         *SRSTOP, stops execution
C
C                         *, this routines can contain sytem dependencies
C
C
      USE Grids        !   for the storage of contraction grids
      use dlwq_data    !   for definition and storage of data
      USE Output       !   for the output names and pointers
      use timers       !   performance timers
      USE DHCOMMAND
C
      USE     D00SUB
      USE     ProcesSet
      USE     Workspace
      USE     Rd_token
C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI  /    Timer characteristics
C
      INCLUDE 'sysi.inc'
      INCLUDE 'sysa.inc'
      INCLUDE 'sysj.inc'
      INCLUDE 'sysc.inc'
C
C     output structure common blocks
C
      INTEGER             IN(INSIZE)       , II(IISIZE)         ! arrays to write common block to file
      EQUIVALENCE       ( IN(1)  , NOSEG ) , ( II(1), ITSTRT  ) ! equivalence output array with common block
C
C     work arrays
C
      INTEGER, PARAMETER             :: IIMAX = 2500000         ! default size integer work array
      INTEGER, PARAMETER             :: IRMAX =10000000         ! default size real work array
      INTEGER, PARAMETER             :: ICMAX = 1000000         ! default size character work array
      INTEGER                        :: IMAX                    ! dynamic size integer work array
      INTEGER                        :: RMAX                    ! dynamic size real work array
      INTEGER                        :: CMAX                    ! dynamic size character work array
      INTEGER          , ALLOCATABLE :: IAR(:)                  ! integer work array
      REAL             , ALLOCATABLE :: RAR(:)                  ! real work array
      CHARACTER(LEN=20), ALLOCATABLE :: CAR(:)                  ! character work array

      REAL,              DIMENSION(:), POINTER :: ABUF  => NULL()
      INTEGER,           DIMENSION(:), POINTER :: IBUF  => NULL()
      CHARACTER(LEN=20), DIMENSION(:), POINTER :: CHBUF => NULL()

C     files, unit numbers, include file stack, input file settings
C
      INTEGER, PARAMETER             :: NLUN   = 45              ! number of input / output files
!     INTEGER, PARAMETER             :: LSTACK = 4               ! size include files stack
!     INTEGER, PARAMETER             :: LCHMAX = 255             ! sring length file name variables
      INTEGER                        :: LUN(NLUN)                ! unit numbers input / output files
      integer                           filtype(nlun)
      CHARACTER(LEN=LCHMAX)          :: RUNID                    ! runid
      CHARACTER(LEN=LCHMAX)          :: LCHAR(NLUN)              ! file names input / output files
!     CHARACTER(LEN=LCHMAX)          :: LCH(LSTACK)              ! file names include files stack
!     INTEGER                        :: ILUN(LSTACK)             ! unit numbers include files stack
!     CHARACTER                      :: CCHAR                    ! comment character
      LOGICAL                        :: DTFLG1                   ! first flag concerning time formats
      LOGICAL                        :: DTFLG2                   ! second flag concerning time formats
      LOGICAL                        :: DTFLG3                   ! third flag concerning time formats
      type(inputfilestack)           :: inpfil                   ! input file strucure with include stack and flags
C
C     variaous input-output structures
C
      INTEGER, PARAMETER             :: NOITM  = 11              ! number of items with time-functions
      INTEGER, PARAMETER             :: NOINT  = 184             ! number of integration options implemented
      INTEGER, PARAMETER             :: NOOUTP = 9               ! number of output files
      INTEGER                        :: NRFTOT(NOITM)            ! number of function per item
      INTEGER                        :: NRHARM(NOITM)            ! number of harmoncs per item
      INTEGER                        :: IOPT(NOINT)              ! integration option list
      INTEGER                        :: IOUTPS(7,NOOUTP)         ! output file defintion structure
      CHARACTER(LEN=20), POINTER     :: PSYNAM(:)                ! substance names read buffer copies into SYNAME
      integer( 4)      , pointer     :: multp(:,:)               ! multiplication substances pointer copies into imultp
      CHARACTER(LEN=20), ALLOCATABLE :: SYNAME(:)                ! substance names final array
      integer( 4)      , ALLOCATABLE :: imultp(:,:)              ! multiplication substances pointer
      INTEGER           ,POINTER     :: NSEGDMP(:)               ! number of monitored segments
      INTEGER           ,POINTER     :: ISEGDMP(:)               ! segment numbers of monitored segments
      INTEGER           ,POINTER     :: NEXCRAAI(:)              ! number of exchanges used in transects
      INTEGER           ,POINTER     :: IEXCRAAI(:)              ! exchange numbers used in transects
      INTEGER           ,POINTER     :: IOPTRAAI(:)              ! option number for transects
      type(ProcesPropColl)           :: StatProcesDef            ! the statistical proces definition
      type(ItemPropColl)             :: AllItems                 ! all items of the proces system
      type(t_dlwq_item)              :: constants                ! delwaq constants list
C
C     help variables
C
      LOGICAL                        :: NOLIC                    ! No valid license?
      LOGICAL                        :: LFOUND                   ! help varaiable indicating if command line argument is found
      CHARACTER(LEN=20)              :: RUNDAT                   ! execution date-time string
      CHARACTER                      :: CDUMMY
      REAL                           :: RDUMMY
      integer                           ierr                     ! cumulative number of errors
      integer                           iwar                     ! cumulative number of warnings
      type(GridPointerColl) GridPs
      type(OutputColl     ) Outputs
      integer                           narg        ! nr of command line arguments
      character(lchmax)                 arg         ! a command line argument
C
C       initialisations
C
      DATA      LUN / 14 , 15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 ,
     *                24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 ,
     *                34 , 35 , 36 , 37 , 38 , 39 , 40 , 41 , 42 , 43 ,
     *                44 , 45 , 46 , 47 , 48 , 49 , 50 , 51 , 52 , 53 ,
     *                54 , 55 , 56 , 57 , 58 /
      DATA      LCHAR  / '-delwaq03.wrk' , '-delwaq04.wrk' ,
     *                   '-harmonic.wrk' , '-pointers.wrk' ,
     *                   '-timestep.wrk' , '-gridding.wrk' ,
     *                   '-volumes.wrk'  , '-to_from.wrk ' ,
     *                   '-dispersi.wrk' , '-areas.wrk'    ,
     *                   '-flows.wrk'    , '-velocity.wrk' ,
     *                   '-lengthes.wrk' , '-boundary.wrk' ,
     *                   '-wastload.wrk' , '-function.wrk' ,
     *                   '-segfunc.wrk'  , '-initials.wrk' ,
     *                   '.mon'          , '.dmp'          ,
     *                   '.his'          , '.map'          ,
     *                   '.res'          , '-proces.wrk'   ,
     *                   '-output.wrk'   , '.inp'          ,
     *                   ' '             , '-delwaq02.wrk' ,
     *                   '.lst'          , '-dlwqstrt.inc' ,
     *                   '-scratch1opt3' , '-scratch2opt3' ,
     *                   '-auxfileop1'   , '-proces.def'   ,
     *                   '.lsp'          , '-stochi.inp'   ,
     *                   '-bal.his'      , '.hdf'          ,
     *                   '.adf'          , '-kenmerk.wrk'  ,
     *                   '-filenaam.wrk' , '-stat.map'     ,
     *                   '-stat.mon'     , ' '             ,
     *                   ' '             /
      DATA    IOPT / 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 ,
     *               20 , 21 , 22 , 23 , 24 , 25 , 26 , 27 ,
     *               30 , 31 , 32 , 33 , 34 , 35 , 36 , 37 ,
     *               40 , 41 , 42 , 43 , 44 , 45 , 46 , 47 ,
     *               50 , 51 , 52 , 53 , 54 , 55 , 56 , 57 ,
     *               60 , 61 , 62 , 63 , 64 , 65 , 66 , 67 ,
     *               70 , 71 , 72 , 73 , 74 , 75 , 76 , 77 ,
     *               80 , 81 , 82 , 83 , 84 , 85 , 86 , 87 ,
     *               90 , 91 , 92 , 93 , 94 , 95 , 96 , 97 ,
     *              100 ,101 ,102 ,103 ,104 ,105 ,106 ,107 ,
     *              110 ,111 ,112 ,113 ,114 ,115 ,116 ,117 ,
     *              120 ,121 ,122 ,123 ,124 ,125 ,126 ,127 ,
     *              130 ,131 ,132 ,133 ,134 ,135 ,136 ,137 ,
     *              140 ,141 ,142 ,143 ,144 ,145 ,146 ,147 ,
     *              150 ,151 ,152 ,153 ,154 ,155 ,156 ,157 ,
     *              160 ,161 ,162 ,163 ,164 ,165 ,166 ,167 ,
     *              170 ,171 ,172 ,173 ,174 ,175 ,176 ,177 ,
     *              180 ,181 ,182 ,183 ,184 ,185 ,186 ,187 ,
     *              190 ,191 ,192 ,193 ,194 ,195 ,196 ,197 ,
     *              200 ,201 ,202 ,203 ,204 ,205 ,206 ,207 ,
     *              210 ,211 ,212 ,213 ,214 ,215 ,216 ,217 ,
     *              220 ,221 ,222 ,223 ,224 ,225 ,226 ,227 ,
     *              230 ,231 ,232 ,233 ,234 ,235 ,236 ,237 /

!     Special system init

      integer(4), save         :: ithndl = 0
      call timini ( )                          ! initializes timer
      narg = dhstored_number_args()            ! but timer is switched 'off' by default
      if ( narg .eq. 0 ) narg = nargs()
      do ierr = 1, narg
         call dhgarg ( ierr, arg )
         if ( arg .eq. "timer" .or. arg .eq. "TIMER" ) then
            timon = .true.                     ! optionally switch it 'on'
            exit
         endif
      enddo
      if (timon) call timstrt( "delwaq1", ithndl )
      call avundf

!        initialise values

      ierr   = 0
      iwar   = 0
      LUNREP = LUN(29)
      NOLUN  = NLUN
      filtype = 0
      NOITEM = NOITM
      NOUTP  = NOOUTP
      NOINFO = 0
      NHARMS = 0
      NIHARM = 0
      NLINES = 0
      NPOINS = 0
      NEWRSP = 0
      NEWISP = 0
      IVFLAG = 0
      ITFLAG = 0
      NCBUFM = 0
      NOVAR  = 0
      NOARR  = IASIZE + IJSIZE + ICSIZE
      NUFIL  = 0
      DO 10 I=1, NOITEM
        NRFTOT(I) = 0
        NRHARM(I) = 0
   10 CONTINUE
      StatProcesDef%maxsize = 0
      StatProcesDef%cursize = 0
      AllItems%maxsize = 0
      AllItems%cursize = 0
      GridPs%cursize=0
      GridPs%maxsize=0
C
      CALL UNISET ( LUN    , LCHAR , NOLUN , RUNID )
C
C     UNSCRAMBLE NAME USER
C
      CALL UNLOCK (LUNREP,.FALSE.,NOLIC)
      WRITE(*,*)
      WRITE(*,*) ' runid : ',TRIM(RUNID)
      WRITE(*,*)
C
C     allocate workspace
C
      CALL GETCOM ( '-IMAX', 1 , LFOUND, IMAX  , RDUMMY, CDUMMY, IERR )
      IF ( LFOUND ) THEN
         IF ( IERR .EQ. 0 ) THEN
            WRITE(LUNREP,2010) IMAX
         ELSE
            WRITE(LUNREP,2020)
            ierr = 1
            GOTO 900
         ENDIF
      ELSE
         IMAX = IIMAX
      ENDIF
      CALL GETCOM ( '-RMAX', 1 , LFOUND, RMAX  , RDUMMY, CDUMMY, IERR )
      IF ( LFOUND ) THEN
         IF ( IERR .EQ. 0 ) THEN
            WRITE(LUNREP,2030) RMAX
         ELSE
            WRITE(LUNREP,2040)
            ierr = 1
            GOTO 900
         ENDIF
      ELSE
         RMAX = IRMAX
      ENDIF
      CALL GETCOM ( '-CMAX', 1 , LFOUND, CMAX  , RDUMMY, CDUMMY, IERR )
      IF ( LFOUND ) THEN
         IF ( IERR .EQ. 0 ) THEN
            WRITE(LUNREP,2050) CMAX
         ELSE
            WRITE(LUNREP,2060)
            ierr = 1
            GOTO 900
         ENDIF
      ELSE
         CMAX = ICMAX
      ENDIF
      ALLOCATE(IAR(IMAX),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE ( LUNREP , 2070 ) IERR_ALLOC,IMAX
         ierr = 1
         GOTO 900
      ENDIF
      ALLOCATE(RAR(RMAX),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE ( LUNREP , 2080 ) IERR_ALLOC,RMAX
         ierr = 1
         GOTO 900
      ENDIF
      ALLOCATE(CAR(CMAX),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE ( LUNREP , 2090 ) IERR_ALLOC,CMAX
         ierr = 1
         GOTO 900
      ENDIF
C
      CCHAR   = ' '
      ILUN    = 0
      ilun(1) = lun  (26)
      lch (1) = lchar(26)
      lunut   = lun(29)
      call dlwq01 ( lun     , psynam  , nosys   , notot   , nomult  ,
     &              multp   , iwidth  , otime   , isfact  , vrsion  ,
     &              ioutpt  , ierr    , iwar    )
      ALLOCATE(SYNAME(NOTOT+nomult),STAT=IERR_ALLOC)
      ALLOCATE(imultp( 2 ,  nomult),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE ( LUNREP , 2000 ) IERR_ALLOC
         ierr = ierr + 1
         GOTO 900
      ENDIF
      SYNAME = PSYNAM
      imultp = multp
      DEALLOCATE(PSYNAM)
      DEALLOCATE(multp )
      DELTIM = OTIME
      CAR(1) = ' '
      K = 2
      ICMAK = CMAX   - 1

      NULLIFY(NSEGDMP)
      NULLIFY(ISEGDMP)
      NULLIFY(NEXCRAAI)
      NULLIFY(IEXCRAAI)
      NULLIFY(IOPTRAAI)
      call dlwq02 ( lun     , lchar   , filtype , nrftot  , nlines  ,
     &              npoins  , dtflg1  , dtflg2  , nodump  , iopt    ,
     &              noint   , iwidth  , dtflg3  , ndmpar  , ntdmps  ,
     &              noraai  , ntraaq  , nosys   , notot   , nototp  ,
     &              vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai,
     &              iexcraai, ioptraai, ierr    , iwar    )

      IF ( MOD(INTOPT,16) .GT. 7 ) THEN
         IBFLAG = 1
      ELSE
         IBFLAG = 0
      ENDIF

      call dlwq03 ( lun     , lchar   , filtype , nrftot  , nrharm  ,
     &              ivflag  , dtflg1  , iwidth  , dtflg3  , vrsion  ,
     &              ioutpt  , gridps  , syname  , ierr    , iwar    )

      if ( nolic .and. noseg > 150 ) then
         write(*,'(//a)') 'Error: Authorisation problem'
         write(*,'(a)')   '       No valid license, so the number of se
     &gments is limited to 150'
         call srstop(1)
      endif

      IF ( .NOT. ASSOCIATED(NSEGDMP)  ) ALLOCATE(NSEGDMP(1))
      IF ( .NOT. ASSOCIATED(ISEGDMP)  ) ALLOCATE(ISEGDMP(1))
      IF ( .NOT. ASSOCIATED(NEXCRAAI) ) ALLOCATE(NEXCRAAI(1))
      IF ( .NOT. ASSOCIATED(IEXCRAAI) ) ALLOCATE(IEXCRAAI(1))
      IF ( .NOT. ASSOCIATED(IOPTRAAI) ) ALLOCATE(IOPTRAAI(1))
      call dlwq04 ( lun     , lchar   , filtype , nrftot  , nrharm  ,
     &              ilflag  , dtflg1  , iwidth  , intsrt  , dtflg3  ,
     &              vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai,
     &              iexcraai, ioptraai, gridps  , ierr    , iwar    )
      IF ( ASSOCIATED(NSEGDMP)  ) DEALLOCATE(NSEGDMP)
      IF ( ASSOCIATED(ISEGDMP)  ) DEALLOCATE(ISEGDMP)
      IF ( ASSOCIATED(NEXCRAAI) ) DEALLOCATE(NEXCRAAI)
      IF ( ASSOCIATED(IEXCRAAI) ) DEALLOCATE(IEXCRAAI)
      IF ( ASSOCIATED(IOPTRAAI) ) DEALLOCATE(IOPTRAAI)

      DELTIM = OTIME
      CALL DLWQ05 ( LUN    , LCHAR  , filtype, CAR(K) , IAR    ,
     *              RAR    , NRFTOT , NRHARM , NOBND  , NOSYS  ,
     *              NOTOT  , NOBTYP , RMAX   , IMAX   , DTFLG1 ,
     *              IWIDTH , INTSRT , IERR   , DTFLG3 , SYNAME ,
     *              ICMAK  , VRSION , IOUTPT , iwar   )
C
      DELTIM = OTIME

      nosss = noseg + nseg2     ! increase with bottom segments
      call dlwq06 ( lun    , lchar  , filtype, icmak  , car(k) ,
     &              imax   , iar    , rmax   , rar    , notot  ,
     &              nosss  , syname , nowst  , nowtyp , nrftot ,
     &              nrharm , dtflg1 , dtflg3 , iwidth , vrsion ,
     &              ioutpt , ierr   , iwar   )
C
      NOVEC = 50
      inpfil%dtflg1 = dtflg1
      inpfil%dtflg2 = dtflg2
      inpfil%dtflg3 = dtflg3
      inpfil%itfact = itfact
      inpfil%vrsion = vrsion
      IF ( VRSION .LE. 4.90 ) THEN
         nrharm(10) = 0
         call dlwq07 ( lun    , lchar    , filtype, noseg  , nocons ,
     &                 nopa   , nofun    , nosfun , itfact , dtflg2 ,
     &                 dtflg3 , iwidth   , novec  , vrsion , ioutpt ,
     &                 nothrd , constants, ierr   , iwar   )
      ELSE
         nrharm(10) = 0
         deltim     = otime
         call dlwq7a ( lun    , lchar  , filtype, inpfil   , syname ,
     &                 iwidth , ioutpt , gridps , constants, ierr   ,
     &                 iwar   )
      ENDIF
C
C     Finish and close system file ( DLWQ09 can re-read it )
C
      WRITE ( LUN(2) ) ( NRFTOT(I) , I = 1,NOITEM )
      WRITE ( LUN(2) ) ( NRHARM(I) , I = 1,NOITEM )
      CLOSE ( LUN(2) )

      call dlwq08 ( lun    , lchar  , filtype, nosss  , notot  ,
     &              syname , iwidth , vrsion , ioutpt , inpfil ,
     &              gridps , ierr   , iwar   )

      CALL DLWQ09 ( LUN    , LCHAR  , filtype, CAR    , IAR    ,
     +              icmak  , IIMAX  , IWIDTH , IBFLAG , VRSION ,
     +              IOUTPT , IOUTPS , Outputs, IERR   , iwar   )
C
      CALL DLWQS1 ( LUNREP       , NPOS         ,
     +              CCHAR        , VRSION       ,
     +              ILUN         , LCH          ,
     +              LSTACK       , IOUTPT       ,
     +              DTFLG1       , DTFLG3       ,
     +              StatProcesDef, AllItems     ,
     +              NOINFO       , iwar         ,
     +              IERR         )
      WRITE ( LUNREP,'(//'' Messages presented in this .lst file:'')')
!jvb  WRITE ( LUNREP,'( /'' Number of INFOrmative messages:'',I3)') NOINFO
      WRITE ( LUNREP,'( /'' Number of WARNINGS            :'',I3)') iwar
      WRITE ( LUNREP,'(  '' Number of ERRORS during input :'',I3)') IERR
      WRITE ( LUNREP,'(  '' '')')
C
      call dlwqp1 ( lun          , lchar        ,
     +              statprocesdef, allitems     ,
     +              ioutps       , outputs      ,
     +              nomult       , imultp       ,
     +              constants    , noinfo       ,
     +              iwar         , ierr         )

      deallocate(syname)
      deallocate(imultp)
C
  900 CONTINUE
      WRITE ( LUNREP,'(//'' Messages presented including .lsp file:'')')
!jvb  WRITE ( LUNREP,'( /'' Number of INFOrmative messages:'',I3)') NOINFO
      WRITE ( LUNREP,'(  '' Number of WARNINGS            :'',I6)') iwar
      WRITE ( LUNREP,'( /'' Number of ERRORS during input :'',I6)') IERR
      WRITE (   *   ,'(  '' Number of WARNINGS            :'',I3)') iwar
      WRITE (   *   ,'(  '' Number of ERRORS during input :'',I3)') IERR
      WRITE (   *   ,'(  '' '')')
C
      IF ( IERR .EQ. 0 ) THEN
         NOVEC = MIN(NOVEC,(NOSSS+NOBND-1))
         ITOTA = 0
         ITOTI = 0
         ITOTC = 0
         CALL SPACE  ( LUNREP, .FALSE., ABUF   , IBUF   , CHBUF  ,
     +                 ITOTA , ITOTI  , ITOTC  )
C
         CALL DHOPNF  ( LUN(1) , LCHAR(1) , 1     , 1     , IOERR )
         WRITE ( LUN(1) )   IN
         WRITE ( LUN(1) )   II
         WRITE ( LUN(1) )   ITOTA , ITOTI , ITOTC
         WRITE ( LUN(1) ) ( LUN    (K) , K = 1,NOLUN  )
         WRITE ( LUN(1) ) ( LCHAR  (K) , K = 1,NOLUN  )
         WRITE ( LUN(1) ) ( filtype(K) , K = 1,NOLUN  )
      ELSE
         WRITE ( LUNREP , '(  '' SIMULATION PROHIBITED !!!!!!!!'')' )
         CALL DHOPNF  ( LUN(1) , LCHAR(1) , 1     , 3     , IOERR )
         CALL SRSTOP ( 1 )
      ENDIF
C
      CALL DATTIM(RUNDAT)
      WRITE (LUNREP,'(2A)') ' Execution stop : ',RUNDAT
      close ( lunrep )
      if ( timon ) then
         call timstop ( ithndl )
         call timdump ( TRIM(RUNID)//'-delwaq1-timers.out' )
      endif
      CALL SRSTOP ( 0 )
 2000 FORMAT (  /,' ERROR: allocating memory for system names:',I6)
 2010 FORMAT (  /,' Command line argument -IMAX, size of integer work array:',I12)
 2020 FORMAT (  /,' ERROR: interpreting command line argument -IMAX, size of integer work array:')
 2030 FORMAT (  /,' Command line argument -RMAX, size of real work array:',I12)
 2040 FORMAT (  /,' ERROR: interpreting command line argument -RMAX, size of real work array:')
 2050 FORMAT (  /,' Command line argument -CMAX, size of character work array:',I12)
 2060 FORMAT (  /,' ERROR: interpreting command line argument -CMAX, size of character work array:')
 2070 FORMAT (  /,' ERROR: allocating integer work array:',I6,' with length:',I12)
 2080 FORMAT (  /,' ERROR: allocating real work array:',I6,' with length:',I12)
 2090 FORMAT (  /,' ERROR: allocating character work array:',I6,' with length:',I12)
      END
