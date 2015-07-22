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

      SUBROUTINE RDSTAT ( LUNREP , IPOSR  , NPOS   , CCHAR  , VRSION ,
     +                    ILUN   , LCH    , LSTACK , IOUTPT , DTFLG1 ,
     +                    DTFLG3 , IERR   , NOSTAT , NKEY   , NOKEY  ,
     +                    KEYNAM , KEYVAL , NPERIOD, PERNAM , PERSFX ,
     +                    PSTART , PSTOP  )
C
C
C     Deltares
C
C     CREATED            : Feb 2002 by Jan van Beek
C
C     FUNCTION           : Reads statistical output spec. block 10
C
C     SUBROUTINES CALLED : RDTOK1 tokenized data file reading
C
C     LOGICAL UNITS      : LUNIN = unit formatted inputfile
C                          LUNREP= unit formatted outputfile
C
C     PARAMETERS         :
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     IPOSR   INTEGER  1           IN/OUT  position on input record
C     NPOS    INTEGER  1           INPUT   length of input record
C     CCHAR   CHAR*1   1           INPUT   comment character
C     VERSION REAL     1           INPUT   program version number
C     ILUN    INTEGER  LSTACK      IN/OUT  unit number stack
C     LCH     CHAR*(*) LSTACK      IN/OUT  Filename stack
C     LSTACK  INTEGER  1           INPUT   size of the stack
C     IOUTPT  INTEGER  1           INPUT   output file option
C     DTFLG1  LOGICAL  1           INPUT   'date'-format 1st timescale
C     DTFLG3  LOGICAL  1           INPUT   'date'-format (F;ddmmhhss,T;yydddhh)
C     IERR    INTEGER  1           IN/OUT  Cumulative error count
C     NOSTAT  INTEGER  1           OUTPUT  number of statistical processes
C     NKEY    INTEGER  1           OUTPUT  total number of keywords
C     NOKEY   INTEGER  NOSTAT      OUTPUT  number of keywords per stat. proc.
C     KEYNAM  CHAR*20  NKEY        OUTPUT  names of the keywords read
C     KEYVAL  CHAR*20  NKEY        OUTPUT  values of the keywords
C     NPERIOD INTEGER  1           OUTPUT  number of periods
C     PERNAM  CHAR*20  NPERIOD     OUTPUT  period name
C     PERSFX  CHAR*20  NPERIOD     OUTPUT  period suffix
C     PSTART  INTEGER  NPERIOD     OUTPUT  period start
C     PSTOP   INTEGER  NPERIOD     OUTPUT  period stop
C
      use timers       !   performance timers
      USE      DHRALLOC

      IMPLICIT NONE
C
      INTEGER       LUNREP , IPOSR  , NPOS   , LSTACK , IOUTPT ,
     +              IERR   , NOSTAT , NKEY
      LOGICAL       DTFLG1 , DTFLG3
      REAL          VRSION
      INTEGER       ILUN(*)
      CHARACTER*(*) LCH  (*)
      CHARACTER*1   CCHAR
      CHARACTER*20, POINTER :: KEYNAM(:)
      CHARACTER*20, POINTER :: KEYVAL(:)
      INTEGER     , POINTER :: NOKEY(:)
      INTEGER       NPERIOD
      CHARACTER*20, POINTER :: PERNAM(:)
      CHARACTER*20, POINTER :: PERSFX(:)
      INTEGER     , POINTER :: PSTART(:)
      INTEGER     , POINTER :: PSTOP(:)
C
C     Local
C
      INTEGER       NPKEY  , NKEYPER, NKEYPAR, IPAR
      PARAMETER   ( NPKEY = 4 )
      PARAMETER   ( NKEYPER = 4 )
      PARAMETER   ( NKEYPAR = 3 )
      CHARACTER*20  KEY    , KEYS(NPKEY)
      CHARACTER*20  KEYPER(NKEYPER)
      CHARACTER*20  KEYPAR(NKEYPAR)
      CHARACTER*20  KNAM   , CDUMMY
      CHARACTER*20  KVAL
      REAL          ADUMMY
      INTEGER       IDUMMY , IERR2  , IKEY  , ITYPE  , MAXKEY ,
     +              MAXSTAT, VERSTAT, MINSTAT, IKEY2 , ITSTRT ,
     +              ITSTOP , MPERIOD, IKEY3
      integer       istart, istop
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "rdstat", ithndl )
C
      NOSTAT = 0
      NKEY   = 0
      MAXSTAT = 10
      MAXKEY  = 50
      ALLOCATE(NOKEY(MAXSTAT))
      ALLOCATE(KEYNAM(MAXKEY))
      ALLOCATE(KEYVAL(MAXKEY))
      NPERIOD = 0
      MPERIOD = 2
      ALLOCATE(PERNAM(MPERIOD))
      ALLOCATE(PERSFX(MPERIOD))
      ALLOCATE(PSTART(MPERIOD))
      ALLOCATE(PSTOP (MPERIOD))
C
      KEYS(1) = 'VERSION'
      KEYS(2) = 'MINOR'
      KEYS(3) = 'PERIOD'
      KEYS(4) = 'OUTPUT-OPERATION'
C
      KEYPAR(1) = 'real-parameter'
      KEYPAR(2) = 'time-parameter'
      KEYPAR(3) = 'logical-parameter'
C
  100 CONTINUE
         ITYPE = 0
         CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR  ,
     +                 IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY ,
     +                                            ITYPE  , IERR2  )

         IF ( IERR2 .EQ. 2 ) GOTO 500
         IF ( IERR2 .EQ. 3 .AND. NOSTAT .EQ. 0 ) GOTO 500
         IF ( IERR2 .EQ. 3 ) THEN
            WRITE(LUNREP,*)
     +       'ERROR : closing delimiter block 10 not found'
            IERR = IERR + 1
            GOTO 500
         ENDIF
         IF ( IERR2 .NE. 0 ) THEN
            WRITE(LUNREP,*)
     +       'ERROR : reading block 10'
            IERR = IERR + 1
            GOTO 500
         ENDIF

         CALL ZOEK (KNAM,NPKEY,KEYS,20,IKEY)
         IF ( IKEY .LE. 0 ) THEN
            WRITE(LUNREP,*) 'ERROR : unexpected keyword found'
            WRITE(LUNREP,*) 'found    :',KNAM
            WRITE(LUNREP,*) 'expected : OUTPUT-OPERATION'
            IERR = IERR + 1
            GOTO 100
         ELSEIF ( IKEY .EQ. 1 ) THEN
C
C           version
C
            ITYPE = 2
            CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR  ,
     +                    IPOSR  , NPOS   , CDUMMY , VERSTAT, ADUMMY ,
     +                                               ITYPE  , IERR2  )
            IF ( IERR2 .NE. 0 ) GOTO 900
         ELSEIF ( IKEY .EQ. 2 ) THEN
C
C           minor
C
            ITYPE = 2
            CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR  ,
     +                    IPOSR  , NPOS   , CDUMMY , MINSTAT, ADUMMY ,
     +                                               ITYPE  , IERR2  )
            IF ( IERR2 .NE. 0 ) GOTO 900
         ELSEIF ( IKEY .EQ. 3 ) THEN
C
C           period
C
            NPERIOD = NPERIOD + 1
            IF ( NPERIOD .GT. MPERIOD ) THEN
               MPERIOD = 2*MPERIOD
               CALL DHRALLOC_CH20(PERNAM,MPERIOD,NPERIOD-1)
               CALL DHRALLOC_CH20(PERSFX,MPERIOD,NPERIOD-1)
               CALL DHRALLOC_INT(PSTART,MPERIOD,NPERIOD-1)
               CALL DHRALLOC_INT(PSTOP ,MPERIOD,NPERIOD-1)
            ENDIF
            ITYPE = 0
            CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR  ,
     +                    IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY ,
     +                                               ITYPE  , IERR2  )
            IF ( IERR2 .NE. 0 ) GOTO 900
            PERNAM(NPERIOD) = KNAM
            KEY = 'START'
            CALL DLWQ0T ( KEY, istart, .FALSE., .FALSE., IERR2 )
            PSTART(NPERIOD) = istart
            KEY = 'STOP'
            CALL DLWQ0T ( KEY, istop, .FALSE., .FALSE., IERR2 )
            PSTOP (NPERIOD) = istop
            WRITE(PERSFX(NPERIOD),'(''period'',i2.2)') NPERIOD
C
C           suffix,start, stop, more ?
C
  200       CONTINUE
            ITYPE = 0
            CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR  ,
     +                    IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY ,
     +                                               ITYPE  , IERR2  )
            IF ( IERR2 .NE. 0 ) GOTO 900
C
            KEYPER(1) = 'SUFFIX'
            KEYPER(2) = 'START-TIME'
            KEYPER(3) = 'STOP-TIME'
            KEYPER(4) = 'END-PERIOD'
            CALL ZOEK (KNAM,NKEYPER,KEYPER,20,IKEY2)
            IF ( IKEY2 .LE. 0 ) THEN
               WRITE(LUNREP,*) 'ERROR : unexpected keyword found'
               WRITE(LUNREP,*) 'found    :',KNAM
               IERR = IERR + 1
               GOTO 200
            ELSEIF ( IKEY2 .EQ. 1 ) THEN
C
C              SUFFIX
C
               ITYPE = 0
               CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR ,
     +                       IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY,
     +                                                  ITYPE  , IERR2 )
               IF ( IERR2 .NE. 0 ) GOTO 900
               PERSFX(NPERIOD) = KNAM
C
            ELSEIF ( IKEY2 .EQ. 2 ) THEN
C
C              START-TIME
C
               ITYPE = -3
               CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR ,
     +                       IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY,
     +                                                  ITYPE  , IERR2 )
               istart = IDUMMY
               IF ( IERR2 .NE. 0 ) GOTO 900
               IF ( ITYPE .EQ. 1 ) THEN
                  CALL DLWQ0T ( KNAM, istart, .FALSE., .FALSE., IERR2 )
                  IF ( IERR2 .NE. 0 ) THEN
                     WRITE(LUNREP,*)'ERROR interpreting start time:',
     +                              KNAM
                     IERR = IERR + 1
                  ENDIF
               ELSE
                  CALL CNVTIM ( istart, 1     , DTFLG1 , DTFLG3 )
               ENDIF
               PSTART(NPERIOD) = istart
C
            ELSEIF ( IKEY2 .EQ. 3 ) THEN
C
C              STOP-TIME
C
               ITYPE = -3
               CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR ,
     +                       IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY,
     +                                                  ITYPE  , IERR2 )
               istop = IDUMMY
               IF ( IERR2 .NE. 0 ) GOTO 900
               IF ( ITYPE .EQ. 1 ) THEN
                  CALL DLWQ0T ( KNAM, istop , .FALSE., .FALSE., IERR2 )
                  IF ( IERR2 .NE. 0 ) THEN
                     WRITE(LUNREP,*)'ERROR interpreting stop time:',KNAM
                     IERR = IERR + 1
                  ENDIF
               ELSE
                  CALL CNVTIM ( istop , 1     , DTFLG1 , DTFLG3 )
               ENDIF
               PSTOP(NPERIOD) = istop
C
            ELSEIF ( IKEY2 .EQ. 4 ) THEN
C
C              END-PERIOD
C
               GOTO 100
C
            ENDIF
C
            GOTO 200
C
         ELSEIF ( IKEY .EQ. 4 ) THEN
C
C           statistical operation
C
            NOSTAT = NOSTAT + 1
            IF ( NOSTAT .GT. MAXSTAT ) THEN
               MAXSTAT = 2*MAXSTAT
               CALL DHRALLOC_INT(NOKEY,MAXSTAT,NOSTAT-1)
            ENDIF
            NOKEY(NOSTAT) = 0
C
  300       CONTINUE
C
C           check if it a parameter with extra key word real-parameter, time-parameter, logical-parameter, ?integer-parameter
C
            CALL ZOEK (KNAM,3,KEYPAR,20,IPAR)
            IF ( IPAR .GT. 0 ) THEN
C
C              get real KNAM
C
               ITYPE = 0
               CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR ,
     +                       IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY,
     +                                                  ITYPE  , IERR2 )
               IF ( IERR2 .NE. 0 ) GOTO 900
C
            ENDIF

C
            ITYPE = 0
            CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR  ,
     +                    IPOSR  , NPOS   , KVAL   , IDUMMY , ADUMMY ,
     +                                               ITYPE  , IERR2  )
            IF ( IERR2 .NE. 0 ) GOTO 900
C
            NOKEY(NOSTAT)= NOKEY(NOSTAT) + 1
            NKEY         = NKEY + 1
            IF ( NKEY .GT. MAXKEY ) THEN
               MAXKEY = 2*MAXKEY
               CALL DHRALLOC_CH20(KEYNAM,MAXKEY,NKEY-1)
               CALL DHRALLOC_CH20(KEYVAL,MAXKEY,NKEY-1)
            ENDIF
            KEYNAM(NKEY) = KNAM
            KEYVAL(NKEY) = KVAL
C
            ITYPE = 0
            CALL RDTOK1 ( LUNREP , ILUN   , LCH    , LSTACK , CCHAR  ,
     +                    IPOSR  , NPOS   , KNAM   , IDUMMY , ADUMMY ,
     +                                               ITYPE  , IERR2  )
            IF ( IERR2 .NE. 0 ) GOTO 900
C
            KEY = 'END-OUTPUT-OPERATION'
            CALL ZOEK (KNAM,1,KEY,20,IKEY)
            IF ( IKEY .LE. 0 ) THEN
               GOTO 300
            ENDIF
         ENDIF
C
C        next keyword
C
         GOTO 100
C
  500 CONTINUE
C
      if (timon) call timstop( ithndl )
      RETURN
C
  900 CONTINUE
      IF ( IERR2 .EQ. 3 ) THEN
         WRITE(LUNREP,*) 'ERROR : unexpected end of input file'
      ELSEIF ( IERR2 .EQ. 2 ) THEN
         WRITE(LUNREP,*) 'ERROR : unexpected end of block 10'
      ELSE
         WRITE(LUNREP,*) 'ERROR : reading block 10'
      ENDIF
      IERR = IERR + 1
      if (timon) call timstop( ithndl )
      RETURN
C
      END
