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

      subroutine dlwq5a ( lun    , lchar  , iu     , iwidth , icmax  ,
     &                    car    , iimax  , iar    , irmax  , rar    ,
     &                    sname  , aname  , atype  , ntitm  , ntdim  ,
     &                    nttype , drar   , dtflg1 , dtflg3 , vrsion ,
     &                    ioutpt , ierr2  , ierr   , iwar   )

!       Deltares Software Centre

!>\file
!>                          boundary and waste data new style
!>
!>                          This routine reads blocks of input of the kind:
!>                             - ITEM
!>                             - bnd/wst item-IDs, nrs or type nrs
!>                             - CONCEN
!>                             - substance IDs or nrs (or FLOW or 0 for wastes)
!>                             - DATA
!>                             - the associated data
!>                          Reading proceeds untill group end #5 or #6\n
!>                          At run time the arrays are filled by wandering
!>                          through the blocks and picking the values at the
!>                          right time.\n
!>                          At multiple definitions the last one counts.
!>                          Writing starts with defaults that are all zero.
!>                             - many keywords apply
!>                             - ITEM and CONCEN sections may be interchanged
!>                             - the last section runs fastest in the matrix
!>                             - USEFOR with simple computational rules apply
!>                             - the data block may have column headers
!>                             - time indicator is absolute time string or integer
!>                             - ODS files are read here and data placed in the blocks
!>                             - BINARY files are resolved at run time

!     CREATED            : May '96  by L. Postma

!     MODIFIED           : March 2000 by L. Postma
!                               Introduction DLWQ5G.F for column headers

!     SUBROUTINES CALLED : RDTOK1 - tokenized input
!                          DHOPNF - opens a file
!                          DLWQ5B - gets names of items/concentrations
!                          DLWQ5C - gets ODS data
!                          DLWQ5D - gets block of breakpoint data
!                          DLWQ5E - performs computations where needed
!                          read_time_delay - reads time delay variables
!                          DLWQ5G - reads optional column headers
!                          DLWQJ3 - writes a data block
!                          CHECK  - checks completion of data group

!     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
!                          LUN(29) = unit formatted output file
!                          LUN( 2) = unit intermediate file (system)
!                          LUN(14) = unit intermediate file (boundaries)
!                          LUN(15) = unit intermediate file (wastes)

      use rd_token
      use timers       !   performance timers

      integer  ( 4), intent(in   ) :: lun  (*)      !< array with unit numbers
      character( *), intent(inout) :: lchar(*)      !< filenames
      integer  ( 4), intent(in   ) :: iu            !< index in LUN array of workfile
      integer  ( 4), intent(in   ) :: iwidth        !< width of the output file
      integer  ( 4), intent(in   ) :: icmax         !< maximum size of character workspace
      character( *), intent(inout) :: car  (*)      !< character workspace
      integer  ( 4), intent(in   ) :: iimax         !< maximum size of integer workspace
      integer  ( 4), intent(inout) :: iar  (iimax)  !< integer workspace
      integer  ( 4), intent(in   ) :: irmax         !< maximum size of real workspace
      real     ( 4), intent(inout) :: rar  (irmax)  !< real workspace
      character( *), intent(in   ) :: sname(*)      !< substances names
      character( *), intent(in   ) :: aname(*)      !< ID's of the boundaries/wastes
      character( *), intent(in   ) :: atype(*)      !< Types of the boundaries/wastes
      integer  ( 4), intent(in   ) :: ntitm         !< number of bounds/wastes
      integer  ( 4), intent(in   ) :: ntdim         !< number of substances
      integer  ( 4), intent(in   ) :: nttype        !< number of boundary/waste types
      real     ( 8), intent(inout) :: drar (*)      !< Double precision workspace
      logical      , intent(in   ) :: dtflg1        !< 'date'-format 1st time scale
      logical      , intent(in   ) :: dtflg3        !< 'date'-format (F;ddmmhhss,T;yydddhh)
      real     ( 4), intent(in   ) :: vrsion        !< version of input
      integer  ( 4), intent(in   ) :: ioutpt        !< how extensive will the output be
      integer  ( 4), intent(  out) :: ierr2         !< return code of this routine
      integer  ( 4), intent(inout) :: ierr          !< cumulative error   count
      integer  ( 4), intent(inout) :: iwar          !< cumulative warning count

!     IN THE COMMON BLOCK:

!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     block sysi.inc
!     ITSTRT  INTEGER    1         INPUT   Simulation start time ( scu )
!     ITSTOP  INTEGER    1         INPUT   Simulation stop time ( scu )
!     ISFACT  INTEGER    1         INPUT   system clock in seconds
!     OTIME   REAL*8     1         INPUT   Julian offset of the real time
!     block sysn.inc
!     NEWRSP  INTEGER    1         IN/OUT  Real array space new bounds
!     NEWISP  INTEGER    1         IN/OUT  Integer array space new bounds

!*****NB for memory map see end of routine

!     System common blocks

      INCLUDE 'sysi.inc'       !     COMMON  /  SYSI   /   System timers
      INCLUDE 'sysn.inc'       !     COMMON  /  SYSN   /   System characteristics

!     Local declarations

      CHARACTER     CALLR*10, CALIT*10, STRNG1*10, STRNG2*10,
     *              STRNG3*10
      INTEGER       IORDER   , NOITM , NODIM , IFLAG  , ITYPE ,
     +              ITTIM    , CHKFLG
      CHARACTER     CHULP*255
      LOGICAL       NEWREC   , SCALE , ODS   , BINFIL , FIRST , TDELAY
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq5a", ithndl )
C
C     Initialise a number of variables
C
      LUNUT  = LUN(29)
      LUNWR2 = LUN(IU)
      IFILSZ = 0
      JFILSZ = 0
      FIRST  = .TRUE.
      CALLR  = 'CONCENTR. '
      STRNG2 = 'Substance'
      IPRO   = 0
      ITFACW = 1
      DELTIM = OTIME
      AMISS  = -999.0
C
C          Initialise new data block
C
C     IORDER is the binary input flag,
C                    0 = not set, 1 = items , 2 = concentr.
C     IFLAG  is the ASCII input flag,
C                    0 = not set, 1 = items , 2 = concentr.  3 = data
C                    4 = scales
C     ITTIM  is the Time function flag
C                    0 = constant, 1 = time function
C     NOBRK  is the number of breakpoints
C     SCALE  is the Scale values flag .TRUE. is present
C     USEFOR is the Alias flag, .TRUE. is alias string expected
C     ODS    is the ODS   flag, .TRUE. ODS datafile expected
C     NEWREC is the flag for new records
C     IOPT   is option flag 1 = block function, 2 = linear
C                           3 = harmonics     , 4 = fourier
C     IOFF   is offset in the array of integers and strings
C
      IF ( IOUTPT .LT. 3 ) WRITE ( LUNUT , 1340 )
      IF ( IOUTPT .LT. 4 ) WRITE ( LUNUT , 1350 )
      IORDER = 0
      IFLAG  = 0
      IOPT   = 1
      ITTIM  = 0
      NOBRK  = 0
      ITEL   = 0
      SCALE  = .FALSE.
      ODS    = .FALSE.
      BINFIL = .FALSE.
      NEWREC = .FALSE.
C
C          Get a token string (and return if something else was found)
C
   10 IF ( IFLAG .EQ. 0 ) ITYPE = 0
      IF ( IFLAG .EQ. 1 .OR. IFLAG .EQ. 2 ) ITYPE = -3
      IF ( IFLAG .EQ. 3 ) THEN
         IF ( NEWREC ) THEN
            ITYPE = -3
         ELSE
            ITYPE = 3
         ENDIF
      ENDIF
      IF ( IFLAG .EQ. 4 ) ITYPE = 3
   20 CALL RDTOK1 ( LUNUT  , ILUN   , LCH    , LSTACK , CCHAR  ,
     *              IPOSR  , NPOS   , CHULP  , IHULP  , RHULP  ,
     *                                         ITYPE  , IERR2  )
C        End of block detected
      IF ( IERR2 .EQ. 2 ) THEN
         IF( ITYPE .GT. 0 ) IERR = IERR + 1
         GOTO 530
      ENDIF
      IF ( IERR2 .NE. 0 ) GOTO 510
C
C          All the following has the old file structure
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. FIRST .AND.
     *                     CHULP .EQ. 'OLD-FILE-STRUCTURE' ) THEN
         WRITE ( LUNUT , 1000 )
         iwar = iwar + 1
         IERR2 = -1
         goto 540
      ENDIF
      IF ( FIRST ) THEN
C
C     Open the binary work file and privide a zero overall default
C
         CALL DHOPNF ( LUN(IU) , LCHAR(IU) , IU    , 1     , IOERR )
         IF ( IU .EQ. 14 ) THEN
            WRITE ( LUNWR2 ) ' 4.900BOUND '
            CALIT  = 'BOUNDARIES'
            STRNG1 = 'boundary'
            IBLOCK = 5
         ELSE
            WRITE ( LUNWR2 ) ' 4.900WASTE '
            CALIT  = 'WASTELOADS'
            STRNG1 = 'wasteload'
            IBLOCK = 6
         ENDIF
         WRITE ( LUNWR2 ) NTITM, NTDIM
         WRITE ( LUNWR2 ) 1, 0, NTDIM, (K,K=1,NTDIM), 1, 0
         WRITE ( LUNWR2 ) 1
         WRITE ( LUNWR2 ) 0, ( 0.0 , I=1,NTDIM )
         IFILSZ = IFILSZ + 2 + 3 + NTDIM + 3 + 1
         JFILSZ = JFILSZ + NTDIM
         FIRST = .FALSE.
      ENDIF
C
C          A local redirection of the name of an item or substance
C                                                   is not valid here
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP .EQ. 'USEFOR') THEN
         WRITE ( LUNUT , 1010 )
         IERR2 = 1
         GOTO 510
      ENDIF
C
C          Time delay for ODS files
C
   30 IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:10) .EQ. 'TIME_DELAY' ) THEN
         call read_time_delay ( ierr2 )
         IF ( IERR2 .NE. 0 ) GOTO 510
         GOTO 10
      ENDIF
C
C          Time interpolation instead of block function
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:6) .EQ. 'LINEAR' ) THEN
         IF ( IOUTPT .GE. 3 ) WRITE ( LUNUT , 1005 )
         IOPT  = 2
         GOTO 10
      ENDIF
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:5) .EQ. 'BLOCK'  ) THEN
         IOPT  = 1
         GOTO 10
      ENDIF
C
C          Items
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP .EQ. 'ITEM' ) THEN
         IF ( IORDER .EQ. 0 ) THEN
            IF ( IOUTPT .GE. 3 ) WRITE ( LUNUT , 1020 )
            IORDER = 1
            IOFF   = 1
         ELSEIF ( IORDER .EQ. 1 ) THEN
            WRITE ( LUNUT , 1030 )
            IERR2 = 1
            GOTO 510
         ELSE
            IF ( IOUTPT .GE. 3 ) WRITE ( LUNUT , 1040 )
            IOFF  = NODIM + IDMNR + 1
         ENDIF
         CHKFLG = 1
         ICM    = ICMAX - IOFF
         IIM    = IIMAX - IOFF
         CALL DLWQ5B ( LUNUT    , IPOSR , NPOS  , CCHAR , CAR(IOFF),
     *                 IAR(IOFF), ICM   , IIM   , ANAME , ATYPE    ,
     *                 NTITM    , NTTYPE, NOITM , NOITS , CHKFLG   ,
     *                 CALIT    , ILUN  , LCH   , LSTACK, VRSION   ,
     *                 ITYPE    , RAR   , NCONST, ITMNR , CHULP    ,
     *                                    IOUTPT, IERR2 , iwar     )
         NOCOL = NOITS
         IF ( IERR2 .NE. 0 ) GOTO 510
         GOTO 30
      ENDIF
C
C          Concentrations
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:6) .EQ. 'CONCEN' ) THEN
         IF ( IORDER .EQ. 0 ) THEN
            IF ( IOUTPT .GE. 3 ) WRITE ( LUNUT , 1050 )
            IORDER = 2
            IOFF   = 1
         ELSEIF ( IORDER .EQ. 1 ) THEN
            IF ( IOUTPT .GE. 3 ) WRITE ( LUNUT , 1060 )
            IOFF  = NOITM + ITMNR + 1
         ELSE
            WRITE ( LUNUT , 1070 )
            IERR2 = 1
            GOTO 510
         ENDIF
         CHKFLG = 1
         ICM    = ICMAX - IOFF
         IIM    = IIMAX - IOFF
         CALL DLWQ5B ( LUNUT    , IPOSR , NPOS  , CCHAR , CAR(IOFF),
     *                 IAR(IOFF), ICM   , IIM   , SNAME , ATYPE    ,
     *                 NTDIM    ,   0   , NODIM , NODIS , CHKFLG   ,
     *                 CALLR    , ILUN  , LCH   , LSTACK, VRSION   ,
     *                 ITYPE    , RAR   , NCONST, IDMNR , CHULP    ,
     *                                    IOUTPT, IERR2 , iwar     )
         NOCOL = NODIS
         IF ( IERR2 .NE. 0 ) GOTO 510
         GOTO 30
      ENDIF
C
C          Data
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:6) .EQ. 'DATA' ) THEN
         IF ( NOITM*NODIM .EQ. 0 ) THEN
            WRITE ( LUNUT , 1080 ) NOITM, NODIM
            IERR2 = 1
            GOTO 510
         ENDIF
C          Checks if an inner loop collumn header exists for the data matrix
         CALL DLWQ5G ( LUNUT  , IAR    , ITMNR  , NOITM  , IDMNR  ,
     *                 NODIM  , IORDER , IIMAX  , CAR    , IPOSR  ,
     *                 NPOS   , ILUN   , LCH    , LSTACK , CCHAR  ,
     *                 CHULP  , NOCOL  , DTFLG1 , DTFLG3 , ITFACW ,
     *                 ITYPE  , IHULP  , RHULP  , IERR2  , iwar   )
         IF ( IERR2 .GT. 1 ) GOTO 510
C          Reads blocks of data
         IF ( IORDER .EQ. 2 ) THEN
            NITM  = NOITM
         ELSE
            NITM  = NODIM
         ENDIF
         NTI   = NOITM + NODIM + ITMNR + IDMNR + 1
         NTI2  = NTI + NITM
         IIM   = IIMAX - NTI2
         NTR   = NCONST + ITEL + 1
         IIM   = IIMAX - NTI2
         IRM   = IRMAX - NTR
         IF ( IORDER .EQ. 2 ) THEN
            NOTTT = NODIM*NOCOL
         ELSE
            NOTTT = NOITM*NOCOL
         ENDIF
         IF ( IOPT .EQ. 3 .OR. IOPT .EQ. 4 ) NOTTT = NOTTT + 1
         CALL DLWQ5D ( LUNUT  , IAR(NTI2), RAR(NTR), IIM   , IRM    ,
     *                 IPOSR  , NPOS     , ILUN    , LCH   , LSTACK ,
     *                 CCHAR  , CHULP    , NOTTT   , ITTIM , NOBRK  ,
     *                 IOPT   , DTFLG1   , DTFLG3  , ITFACW, ITYPE  ,
     *                          IHULP    , RHULP   , IERR2 , ierr3  )
         ierr = ierr + ierr3
         IF ( IERR2 .EQ. 1 .OR. IERR2 .EQ. 4 ) GOTO 510
         IF ( NOBRK .EQ. 0 .and. ittim .eq. 0 ) THEN
            WRITE(LUNUT,1360)
            IERR = IERR + 1
         ENDIF
C          Assigns according to computational rules
         NR2 = NTR + NOTTT*NOBRK
         CALL DLWQ5E ( LUNUT, IAR   , NOITM, ITMNR   , NODIM   ,
     *                 IDMNR, IORDER, RAR  , IOPT    , RAR(NTR),
     *                 NOCOL, NOBRK , AMISS, IAR(NTI), RAR(NR2))
         STRNG3 = 'breakpoint'
C          Writes to the binary intermediate file
         NTS   = NCONST + 1
         NTC   = NTI
         ICM   = ICMAX - NTC
         CALL DLWQJ3 ( LUNWR2 , LUNUT   , IWIDTH , NOBRK  , IAR    ,
     *                 RAR(NTS),RAR(NR2), ITMNR  , IDMNR  , IORDER ,
     *                 SCALE  , .TRUE.  , BINFIL , IOPT   , IPRO   ,
     *                 ITFACW , DTFLG1  , DTFLG3 , IFILSZ , JFILSZ ,
     *                 SNAME  , STRNG1  , STRNG2 , STRNG3 , IOUTPT )
         IF ( IERR2 .EQ. 2 ) GOTO 530
         IF ( IERR2 .EQ. 3 ) GOTO 510
         IORDER = 0
         IFLAG  = 0
         IOPT   = 1
         AMISS  = -999.0
         ITTIM  = 0
         NOBRK  = 0
         ITEL   = 0
         SCALE  = .FALSE.
         ODS    = .FALSE.
         BINFIL = .FALSE.
         NEWREC = .FALSE.
         IF ( ITYPE .EQ. 1 ) GOTO  30
         GOTO 10
      ENDIF

!          binary-file option selected

!     if ( iabs(itype) .eq. 1 .and.  chulp(1:11) .eq. 'BINARY_FILE' ) then
!        binfil = .true.
!        if ( gettoken( chulp, ierr2 ) .gt. 0 ) goto 510
!        write ( lun(41) , '(a240)' ) chulp
!        write ( lunut   ,   2220   ) chulp
!        call check_filesize( lunut, chulp, nosss*noitm, ierr )
!        nufil = nufil + 1
!        goto 10
!     endif
C
C          ODS-file option selected
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:8) .EQ. 'ODS_FILE' ) THEN
         IF ( NOITM*NODIM .EQ. 0 ) THEN
            WRITE ( LUNUT , 1080 ) NOITM, NODIM
            IERR2 = 1
            GOTO 510
         ENDIF
         ODS   = .TRUE.
         ITYPE = 1
         GOTO 20
      ENDIF
C
C          ODS-file-data retrieval
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. ODS ) THEN
         NTI  = NOITM + NODIM + ITMNR + IDMNR + 1
         NTR  = ITEL  + NCONST + 1
         NTD  = (NTR+1)/2 + 1
         NTS  = NCONST + 1
         IIM  = IIMAX - NTI
         IRM  = IRMAX - NTR
         CALL DLWQ5C ( CHULP , LUNUT  , CAR   , IAR      , RAR(NTR),
     *                 ICMAX , IIMAX  , IRMAX , DRAR     , NOITM   ,
     *                 NODIM , IORDER , SCALE , ITMNR    , IDMNR   ,
     *                         AMISS  , NOBRK , IERR2    , iwar    )
         IF ( IERR2 .NE. 0 ) GOTO 510
         NR2 = NTR + NOITM*NODIM*NOBRK
         CALL DLWQ5E ( LUNUT , IAR    , NOITM , ITMNR    , NODIM   ,
     *                 IDMNR , IORDER , RAR   , IOPT     , RAR(NTR),
     *                 NODIM , NOBRK  , AMISS , IAR(NTI) , RAR(NR2))
         STRNG3 = 'breakpoint'
         CALL DLWQJ3 ( LUNWR2 , LUNUT   , IWIDTH , NOBRK  , IAR    ,
     *                 RAR(NTS),RAR(NR2), ITMNR  , IDMNR  , IORDER ,
     *                 SCALE  , ODS     , BINFIL , IOPT   , IPRO   ,
     *                 ITFACW , DTFLG1  , DTFLG3 , IFILSZ , JFILSZ ,
     *                 SNAME  , STRNG1  , STRNG2 , STRNG3 , IOUTPT )
         IF ( IERR2 .EQ. 2 ) THEN
            IERR2 = -2
            GOTO 530
         ENDIF
         IF ( IERR2 .EQ. 3 ) GOTO 510
         IORDER = 0
         IFLAG  = 0
         IOFF   = 0
         IOPT   = 1
         AMISS  = -999.0
         ITTIM  = 0
         NOBRK  = 0
         ITEL   = 0
         SCALE  = .FALSE.
         ODS    = .FALSE.
         BINFIL = .FALSE.
         NEWREC = .FALSE.
         GOTO  10
      ENDIF
C
C          Absolute or relative timers
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:8) .EQ. 'ABSOLUTE') THEN
         WRITE ( LUNUT , 1135 )
         CHULP = 'TIME'
      ENDIF
C
C          Say it is a time function
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP(1:4) .EQ. 'TIME' ) THEN
         ITTIM = 1
         IOPT  = 1
         GOTO 10
      ENDIF
C
C          Scale factors begin
C
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP .EQ. 'SCALE' ) THEN
         IF ( NODIM .EQ. 0 ) THEN
            WRITE ( LUNUT , 1180 )
            IERR2 = 1
            GOTO 510
         ENDIF
         IF ( ITEL .NE. 0 ) THEN
            WRITE ( LUNUT , 1190 )
            IERR2 = 1
            GOTO 510
         ENDIF
         IFLAG = 4
         SCALE = .TRUE.
         GOTO 10
      ENDIF
C          Getting the scale factors
      IF ( IFLAG .EQ. 4 ) THEN
         ITEL = ITEL + 1
         RAR(ITEL+NCONST) = RHULP
         IF ( ITEL .EQ. IDMNR ) IFLAG = 0
         GOTO 10
      ENDIF
C
      WRITE ( LUNUT , 1320  ) CHULP
      WRITE ( LUNUT , '(A)' )
     *   ' Expected character string should be a valid level 2 keyword'
      IERR2 = 1
  510 CLOSE ( LUNWR2 )
      DO 520 I = 2 , LSTACK
         IF ( LCH(I) .NE. ' ' ) THEN
            CLOSE ( ILUN(I) )
            LCH (I) = ' '
            ILUN(I) =  0
         ENDIF
  520 CONTINUE
  530 NEWRSP = NEWRSP + JFILSZ
      NEWISP = NEWISP + IFILSZ
      call check  ( chulp  , iwidth , iblock , ierr2  , ierr   )
  540 if ( timon ) call timstop( ithndl )
      RETURN
C
 1000 FORMAT ( /' WARNING: Old file structure is used for this data !' )
 1002 FORMAT (  ' Delay integers: IDATE = ',I6,', ITIME = ',I6,'.' )
 1003 FORMAT (  ' New reference time is day: ',I2,'-',I2,'-',I4,
     *          ' / ',I2,'H-',I2,'M-',I2,'S.' )
 1004 FORMAT ( /' ERROR: you specified the TIME_DELAY keyword without',
     *          ' a valid value string for the delay !'/' 2 integers',
     *          ' are expected in YYMMDD and HHMMSS format !')
 1005 FORMAT ( /' Linear interpolation is selected !' )
 1010 FORMAT (  ' ERROR: USEFOR is not alowed here. Specify',
     *                 ' ITEM or CONCENTRATION first !' )
 1020 FORMAT ( /' BLOCKED per ITEM:'   )
 1030 FORMAT ( /' ERROR: Second time ITEMs keyword in this block !' )
 1040 FORMAT ( /' ITEMs within the concentration blocks:'   )
 1050 FORMAT ( /' BLOCKED per CONCENTRATION:'   )
 1060 FORMAT ( /' CONCENTRATIONs within the item blocks:'   )
 1070 FORMAT ( /' ERROR: Second time CONCENs keyword in this block !' )
 1080 FORMAT ( /' ERROR: Nr of ITEMS (',I5,') or nr of concentrations',
     *          ' (',I5,') is zero for this DATA block !' )
 1110 FORMAT (  ' DATA will be retrieved from ODS-file: ',A )
 1120 FORMAT (  ' ERROR: Insufficient memory ! Available:',I10,
     *                                            ', needed:',I10,' !' )
 1130 FORMAT (  ' This block consists of a time function.' )
 1135 FORMAT (  ' Absolute times (YYYY/MM/DD;HH:MM:SS) expected in next'
     *         ,' time function block.' )
 1160 FORMAT (  ' Number of valid time steps found: ',I6 )
 1170 FORMAT (  ' This block consists of constant data.' )
 1180 FORMAT (  ' ERROR: number of substances is zero !' )
 1190 FORMAT (  ' ERROR: data has already been entered !' )
 1320 FORMAT (  ' ERROR: token found on input file: ',A )
 1330 FORMAT (/1X, 59('*'),' B L O C K -',I2,' ',5('*')/)
 1340 FORMAT (  ' Output on administration only writen for output',
     *          ' option 3 and higher !' )
 1350 FORMAT (  ' Output of the data only writen for output',
     *          ' option 4 and higher !' )
 1360 FORMAT (  ' ERROR: no (valid) DATA record available !' )
 2220 FORMAT (  ' Input comes from binary file: ',A      )
C
      END
C
C     Additional documentation on the memory lay out
C
C     After ITEM, 5B reads Item names. It produces:
C     a) in CAR: a series of that item name and then a second series
C        with the same name, or the substituted name after USEFOR
C        If ' ' is specified, 'Item-nnn' will appear.
C        If a positive integer is specified, the name of the
C        corresponding item is given. For negative numbers the name
C        of the corresponding type is provided
C        FLOW is a valid item name if concentrations are expected for
C        wasteloads.
C        If a segment number is expected, 'Segment mmmmmmmm' is produced.
C        ITMNR counts the first series, NOITM the second series.
C     b) in IAR: a series of the number of the item from the items list
C                the same series again
C                a series with sequence numbers
C        ITMNR counts the first series, NOITM the second and third series
C        For types a negative number is used
C        If FLOW is allowed, it has number 0
C     If a number is used after a USEFOR, it is stored in RAR. In the
C        CAR location now the word "&$&$SYSTEM_NAME&$&$!" is placed.
C        In the second series location in IAR, the negative value of the
C        location in RAR is placed. The sequence number is decremented
C     If a computational sign is (*,/,+,-,MIN,MAX) is encountered:
C        The second series location in IAR contains a large negative
C           number depending on the computational sign. Sequence number
C           and item number are incremented
C        If now the name of a previously used item is given, the first
C           series location of IAR contains this item number, the second
C           series location in CAR contains "&$&$SYSTEM_NAME&$&$!" and
C           the sequence number is decremented
C        If a new name was provided, the 3rd series of IAR is set at
C           the corresponding sequence number and the second series
C           location in CAR contains that name
C     At first invocation, this all starts at the beginning of the arrays
C     ITMNR contains the number of items processed. It is the dimension
C        of the first series in IAR and CAR
C     NOITS contains the sequence number. That many things are waiting to
C        be resolved and this number was used as dimension for SCALE
C     NOITM contains the second and third series dimension in IAR and CAR
C     NCONST is the number of constants that was put in RAR
C     ITYPE and CHULP are the type and content of the token at exit
C
C     After CONCENTRATION the same happens. Depending on who is first,
C        the arrays are first filled with Items (IORDER = 1) or with
C        Concentrations (IORDER = 2).
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
