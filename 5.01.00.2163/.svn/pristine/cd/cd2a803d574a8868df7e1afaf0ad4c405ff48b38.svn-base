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

      SUBROUTINE DLWQT5 ( LUNIN , LUNOUT , ITIME  , ARRAY  , RESULT ,
     *                    IP1   , NOP1   , IP2    , NOP2   , NOITM  ,
     *                    NTOT  , LTXT   , ISFLAG )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: december 2000 by L.Postma
C     UPDATED:
C
C     FUNCTION            : Reads constants and parameters from binary files
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
C     ARRAY   REAL       NFTOT    LOCAL   record of the file
C     RESULT  REAL     NOITM,*    IN/OUT  result array
C     IP1     INTEGER     NOP1    INPUT   first pointer to result array
C     NOP1    INTEGER       1     INPUT   dimension of IP1
C     IP2     INTEGER     NOP2    INPUT   second pointer to result array
C     NOP2    INTEGER       1     INPUT   dimension of IP2
C     NTOT    INTEGER       1     INPUT   record length
C     LTXT    CHAR*(*)      1     INPUT   text with the unit number
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C
C     DECLARATIONS        :
C
      use timers

      DIMENSION     ARRAY(NTOT) , RESULT(NOITM,*) ,
     *              IP1  (NOP1) , IP2   (NOP2)
      CHARACTER*(*) LTXT
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt5", ithandl )
C
      IF ( NTOT  .EQ. 0 ) goto 9999  !   RETURN
      READ ( LUNIN , END=80 , ERR=80 ) ITIME1 , (ARRAY(K),K=1,NTOT)
C
C         if we run with multiple instances, each must be able to
C         read the file, so rewind it
C
      REWIND( LUNIN )
C
      ISET = 1
      DO 20 I2 = 1 , NOP2
         DO 10 I1 = 1 , NOP1
            RESULT(IP1(I1),IP2(I2)) = ARRAY(ISET)
            ISET = ISET + 1
   10    CONTINUE
   20 CONTINUE
C
      goto 9999  !   RETURN
C
C         write the messages
C
   80 IF ( ISFLAG .EQ. 1 ) THEN
           WRITE(LUNOUT,2010) LUNIN, LTXT,
     *                        ITIME /86400, MOD(ITIME ,86400)/3600 ,
     *                        MOD(ITIME ,3600)/60, MOD(ITIME ,60)  ,
     *                        ITIME1/86400, MOD(ITIME1,86400)/3600 ,
     *                        MOD(ITIME1,3600)/60, MOD(ITIME1,60)
      ELSEIF ( ISFLAG .EQ. 2 ) THEN
           WRITE(LUNOUT,2020) LUNIN, LTXT ,
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
      ENDIF
      CALL SRSTOP ( 1 )
 9999 if ( timon ) call timstop ( ithandl )
      return
C
 2010 FORMAT ( ' ERROR file on UNIT: ',I3,', READING: ',A,/
     *         ' AT SIMULATION TIME:',I5,'D ',I2,'H ',I2,'M ',I2,'S !',/
     *         ' TIME IN FILE:      ',I5,'D ',I2,'H ',I2,'M ',I2,'S !')
 2020 FORMAT ( ' ERROR file on UNIT:',I10,', READING: ',A,/
     *   ' SIMULATION TIME :',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .',/
     *   ' TIME IN FILE    :',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
C
      END
