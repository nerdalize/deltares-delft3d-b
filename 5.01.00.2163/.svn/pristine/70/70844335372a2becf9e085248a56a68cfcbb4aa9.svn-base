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

      SUBROUTINE DLWQKB ( LUNIN  , LUNOUT , ITIME  , IDTIME , ITIME1 ,
     +                    ITIME2 , IARRA1 , IARRA2 , NFTOT  , LUNTXT ,
     +                    ISFLAG , IFFLAG )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : december 1994 by Jan van Beek
C
C     FUNCTION            : Steps along in a time variable database
C                           for integer block functions
C
C     LOGICAL UNITNUMBERS : LUNIN  - input unit intermediate file
C                           LUNOUT - monitor file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUNIN   INTEGER       1     INPUT   unit number intermediate file
C     LUNOUT  INTEGER       1     INPUT   unit number monitor file
C     ITIME   INTEGER       1     INPUT   Model timer
C     IDTIME  INTEGER       1     IN/OUT  Delta for this function
C     ITIME1  INTEGER       1     IN/OUT  Lower time in file
C     ITIME2  INTEGER       1     IN/OUT  Higher time in file
C     IARRA1  REAL       NFTOT    IN/OUT  record at lower time
C     IARRA2  REAL       NFTOT    IN/OUT  record at higher time
C     NFTOT   INTEGER       1     INPUT   record length
C     LUNTXT  CHAR*(*)      1     INPUT   text with the unit number
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
C
C     DECLARATIONS        :
C
      use timers
      INTEGER       LUNIN  , LUNOUT , ITIME  , IDTIME , ITIME1 ,
     +              ITIME2 , NFTOT  , ISFLAG , IFFLAG
      INTEGER       IARRA1(*), IARRA2(*)
      CHARACTER*(*) LUNTXT
C
C     Local
C
      CHARACTER*16  MSGTXT(3)
      DATA          MSGTXT / ' REWIND ON      ' , ' WARNING READING' ,
     +                       ' REWIND ERROR   ' /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqkb", ithandl )
C
      MESSGE = 0
      IF ( NFTOT  .EQ. 0 ) goto 9999
      IF ( IFFLAG .EQ. 0 ) GOTO 10
C
C         This is the first time, so read.
C
      READ ( LUNIN , END=80 , ERR=80 ) ITIME1 , (IARRA1(K),K=1,NFTOT)
      READ ( LUNIN , END=80 , ERR=80 ) ITIME2 , (IARRA2(K),K=1,NFTOT)
      IDTIME = 0
C
C         Check for start time simulation before start time file
C
      IF ( ITIME .LT. ITIME1 ) MESSGE = 2
C
C         a new record required?
C
   10 IF ( ITIME-IDTIME .LT. ITIME2 ) GOTO 100
      CALL DHIMOV ( IARRA2 , IARRA1 , NFTOT )
      ITIME1 = ITIME2
      READ ( LUNIN , END=60 , ERR=80 ) ITIME2 , (IARRA2(K),K=1,NFTOT)
      GOTO 10
C
C         normal rewind.
C
   60 MESSGE = 1
      REWIND LUNIN
      IDTIME = IDTIME + ITIME1
      READ ( LUNIN , END=80 , ERR=80 ) ITIME1 , (IARRA1(K),K=1,NFTOT)
      READ ( LUNIN , END=80 , ERR=80 ) ITIME2 , (IARRA2(K),K=1,NFTOT)
      IDTIME = IDTIME - ITIME1
      GOTO 100
C
C         error, reading the unit
C
   80 MESSGE = 3
      GOTO 100
C
C         write the messages
C
  100 IF ( MESSGE .EQ. 0 ) goto 9999
      IF ( ISFLAG .NE. 1 ) THEN
           WRITE(LUNOUT,2000) MSGTXT(MESSGE), LUNIN, LUNTXT,
     *                        ITIME, ITIME1
      ELSE
           WRITE(LUNOUT,2010) MSGTXT(MESSGE), LUNIN, LUNTXT,
     *                        ITIME /86400, MOD(ITIME ,86400)/3600 ,
     *                        MOD(ITIME ,3600)/60, MOD(ITIME ,60)  ,
     *                        ITIME1/86400, MOD(ITIME1,86400)/3600 ,
     *                        MOD(ITIME1,3600)/60, MOD(ITIME1,60)
      ENDIF
      IF ( MESSGE .EQ. 1 ) THEN
           MESSGE = 0
           GOTO 10
      ENDIF
      IF ( MESSGE .EQ. 2 ) goto 9999
      CALL SRSTOP ( 1 )
 9999 if ( timon ) call timstop ( ithandl )

C
 2000 FORMAT (   A16          ,' UNIT: ',I3,', READING: ',A20,/
     *         ' AT SIMULATION TIME:',I12,' !',/,
     *         ' TIME IN FILE:      ',I12,' !')
 2010 FORMAT (   A16          ,' UNIT: ',I3,', READING: ',A20,/
     *         ' AT SIMULATION TIME:',I5,'D ',I2,'H ',I2,'M ',I2,'S !',/
     *         ' TIME IN FILE:      ',I5,'D ',I2,'H ',I2,'M ',I2,'S !')
C
      END
