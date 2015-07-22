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

      SUBROUTINE DLWQT2 ( LUNIN  , LUNOUT , ITIME  , RESULT , NTOTAL ,
     *                    LUNTXT , ISFLAG , IFFLAG , ONLINE  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:    march 1988 by L.Postma
C
C     FUNCTION            : Makes values at ITIME for user supplied
C                                         binary intermediate files
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
C     RESULT  REAL     NTOTAL     OUTPUT  result array at time ITIME
C     NTOTAL  INTEGER       1     INPUT   number of items to be filled
C     LUNTXT  CHAR*(*)      1     INPUT   text concerning unit numbers
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
C
C     DECLARATIONS        :
C
      use timers

      DIMENSION     RESULT(NTOTAL)
      CHARACTER*10  MSGTXT(3)
      CHARACTER*(*) LUNTXT
      LOGICAL       ONLINE
      DATA          MSGTXT /' REWIND   ' , ' CONSTANT ' , ' ERROR    '/
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt2", ithandl )

      IF ( ONLINE ) THEN
         if ( lunin .eq. 20 ) write (*,*) ' Read VOLUME record'
         if ( lunin .eq. 24 ) write (*,*) ' Read FLOW   record'
      ENDIF
C
C         is this the first time?
C         BYPASS FOR ONLINE MODE, TO AVOID APPARENT CONSTANT FUNCTION
C
      MESSGE = 0
      IF ( IFFLAG .EQ. 1 .AND. .NOT. ONLINE ) GOTO 20
C
C         normal time varying read
C
      READ  ( LUNIN , END=10 , ERR=40 ) ITIME1 , RESULT
      goto 9999  !   RETURN
C
C         normal rewind.
C
   10 MESSGE = 1
      IF ( ONLINE ) STOP 'REWIND NOT POSSIBLE IN ON-LINE MODE'
      REWIND  LUNIN
      READ  ( LUNIN , END=40 , ERR=40 ) ITIME1 , RESULT
      GOTO 50
C
C         This is the first time, check only for nr of records.
C
   20 CONTINUE
      READ  ( LUNIN , END=40 , ERR=40 ) ITIME1 , RESULT
      READ  ( LUNIN , END=30 , ERR=40 ) ITIME1 , RESULT
      REWIND  LUNIN
      READ  ( LUNIN , END=30 , ERR=40 ) ITIME1 , RESULT
      goto 9999  !   RETURN
C
C         file has only one record, array is constant
C
   30 MESSGE =  2
      REWIND  LUNIN
      READ  ( LUNIN , END=40 , ERR=40 ) ITIME1 , RESULT
      IFFLAG = -1
      GOTO 50
C
C         error, during read
C
   40 MESSGE = 3
   50 IF ( ISFLAG .EQ. 1 ) THEN
           WRITE(LUNOUT,2010) MSGTXT(MESSGE), LUNIN, LUNTXT ,
     *                        ITIME /86400, MOD(ITIME ,86400)/3600,
     *                        MOD(ITIME ,3600)/60, MOD(ITIME ,60) ,
     *                        ITIME1/86400, MOD(ITIME1,86400)/3600,
     *                        MOD(ITIME1,3600)/60, MOD(ITIME1,60)
      ELSEIF ( ISFLAG .EQ. 2 ) THEN
           WRITE(LUNOUT,2020) MSGTXT(MESSGE), LUNIN, LUNTXT ,
     *                            ITIME /31536000           ,
     *                        MOD(ITIME ,31536000)/86400    ,
     *                        MOD(ITIME ,86400)/3600        ,
     *                        MOD(ITIME ,3600)/60           ,
     *                        MOD(ITIME ,60)                ,
     *                            ITIME1/31536000           ,
     *                        MOD(ITIME1,31536000)/86400    ,
     *                        MOD(ITIME1,86400)/3600        ,
     *                        MOD(ITIME1,3600)/60           ,
     *                        MOD(ITIME1,60)
      ELSE
           WRITE(LUNOUT,2000) MSGTXT(MESSGE), LUNIN, LUNTXT ,
     *                        ITIME, ITIME1
      ENDIF
      IF ( MESSGE .LT. 3 ) goto 9999  !   RETURN
      CALL SRSTOP ( 1 )
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
 2000 FORMAT (   A10  ,  'ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME :',I10,' !  TIME IN FILE: ',I10,' !')
 2010 FORMAT (   A10  ,  'ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME :',I5,'D ',I2,'H ',I2,'M ',I2,'S ! ',/
     *         ' TIME IN FILE    :',I5,'D ',I2,'H ',I2,'M ',I2,'S ! ')
 2020 FORMAT (   A10  ,  'ON UNIT:',I10,', READING: ',A,/
     *   ' SIMULATION TIME :',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .',/
     *   ' TIME IN FILE    :',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
C
      END
