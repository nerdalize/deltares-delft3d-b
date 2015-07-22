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

      SUBROUTINE DLWQT3 ( ITIME  , IPERIO , APHASE , AVALUE , NRHARM ,
     *                    NOSUB  , NOSPAC , IPOINT , NPOINT , RESULT ,
     *                    LUNTXT , LUNIN  , LUNOUT , ISFLAG , IFFLAG ,
     *                    UPDATE )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april- 8-1988 by L.Postma
C
C     FUNCTION            : Makes harmonic function values.
C
C     LOGICAL UNITNUMBERS : LUNIN file for initialisation of harmonics
C                           LUNOUT - monitor file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ITIME   INTEGER       1     INPUT   Model timer
C     IPERIO  INTEGER   NRHARM    IN/OUT  harmonic periods
C     APHASE  REAL      NRHARM    IN/OUT  harmonic phases
C     AVALUE  REAL   NOSUB*NRHARM IN/OUT  amplitudes for NOSUB values
C     NRHARM  INTEGER       1     INPUT   number of harmonic records
C     NOSUB   INTEGER       1     INPUT   number of values in an item
C     NOSPAC  INTEGER       1     OUTPUT  space occupied by harmonics
C     IPOINT  INTEGER       ?     INPUT   pointer to output array
C     NPOINT  INTEGER       1     OUTPUT  last pointer in IPOINT
C     RESULT  REAL     NOSUB*?    OUTPUT  function values at ITIME
C     LUNTXT  CHAR*(*)      1     INPUT   text with unitnumber
C     LUNIN   INTEGER       1     INPUT   unit number intermediate file
C     LUNOUT  INTEGER       1     INPUT   unit number monitor file
C     ISFLAG  INTEGER       1     INPUT   = 1, 'DDHHMMSS' format
C     IFFLAG  INTEGER       1     INPUT   = 1, first invocation
C     UPDATE  LOGICAL       1     OUTPUT  set to T if function is updated
C                                         else set to F
C
C     Internal file structure:
C     IPERIO    APHASE     AVALUE --->
C     npoints1  n1-harmos  nosub averages             .
C     period1   phase1     nosub amplitudes           .
C        .         .                                  .
C     period-n1 phase-n1   nosub amplitudes           .
C     npoints2  n2-harmos  nosub averages             .
C     period1   phase1     nosub amplitudes           .
C        .         .                                  .
C     period-n2 phase-n2   nosub amplitudes   ---- NRHARMth line
C     IPOINT:
C     -npoints1--->---npoints2----->.....NPOINT     pointers
C
C     DECLARATIONS        :
C
      use timers

      PARAMETER     ( TWOPI = 6.28319 )
      DIMENSION     IPERIO(*) , APHASE(*) , AVALUE(*) , IPOINT(*) ,
     *              RESULT(NOSUB,*)
      CHARACTER*(*) LUNTXT
      LOGICAL       UPDATE
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqt3", ithandl )
C
C         are there harmonics? is this the initialisation phase?
C
      NOSPAC = 0
      NPOINT = 0
      UPDATE = .FALSE.
      IF ( NRHARM .EQ. 0 ) goto 9999  !   RETURN
      UPDATE = .TRUE.
      IREC   = 1
      ITEL   = 1
      IF ( IFFLAG .EQ. 0 ) GOTO 40
C
C         at first time, initialise arrays
C         loop over the blocks of harmonics ( must be less than NRHARM )
C
      DO 20 IB = 1 , NRHARM+1
      IF ( IREC .GT. NRHARM ) GOTO 30
C
C         loop over the number of harmonics
C
      READ ( LUNIN , END=80 , ERR=80 )   NOTOT      , APHASE(IREC) ,
     *                                 ( AVALUE(K+NOSPAC) , K=1,NOTOT )
      NOSPAC       = NOSPAC + NOTOT
      IPERIO(IREC) = NOTOT
      IHSTOP       = APHASE(IREC) + 0.5
      IREC         = IREC + 1
      DO 10 IH = 1 , IHSTOP
      READ ( LUNIN , END=80 , ERR=80 ) IPERIO(IREC) , APHASE(IREC) ,
     *                                 ( AVALUE(K+NOSPAC) , K=1,NOTOT )
      NOSPAC = NOSPAC + NOTOT
      IREC = IREC + 1
   10 CONTINUE
C
C         return only by IREC > NRHARM
C
   20 CONTINUE
   30 NOSPAC = 0
      NPOINT = 0
      IREC   = 1
      ITEL   = 1
C
C         loop over the blocks of harmonics ( must be less than NRHARM )
C
   40 DO 70 IB = 1 , NRHARM+1
      IF ( IREC .GT. NRHARM ) goto 9999  !   RETURN
C
C         loop over the number of harmonics
C
      NOTOT  = IPERIO(IREC)
      IHSTOP = APHASE(IREC) + 0.5
      ISTART = NPOINT + 1
      NPOINT = NPOINT + NOTOT/NOSUB
      DO 60 IH = 1 , IHSTOP + 1
C
C         harmonic function
C
      IF ( IH .EQ. 1 ) THEN
       FUNC = 1.0
      ELSE
       FUNC = SIN( ( FLOAT(ITIME)/IPERIO(IREC) - APHASE(IREC) )*TWOPI )
      ENDIF
C
C         loop over the pointers and the values
C
      DO 50 I1 = ISTART , NPOINT
      IV = IPOINT(I1)
      DO 50 I2 = 1,NOSUB
      RESULT(I2,IV) = RESULT(I2,IV) + FUNC*AVALUE(ITEL)
      ITEL = ITEL+1
   50 CONTINUE
C
C         increase the record counter
C
      IREC   = IREC + 1
      NOSPAC = NOSPAC + NOTOT
   60 CONTINUE
C
C         return only by IREC > NRHARM
C
   70 CONTINUE
C
C         errors during read
C
   80 IF ( ISFLAG .EQ. 1 ) THEN
           WRITE(LUNOUT,2020) LUNIN, LUNTXT,
     *                        ITIME/86400, MOD(ITIME,86400/3600),
     *                        MOD(ITIME ,3600)/60, MOD(ITIME ,60)
      ELSEIF ( ISFLAG .EQ. 2 ) THEN
           WRITE(LUNOUT,2030) LUNIN, LUNTXT,
     *                            ITIME /31536000           ,
     *                        MOD(ITIME ,31536000)/86400    ,
     *                        MOD(ITIME ,86400)/3600        ,
     *                        MOD(ITIME ,3600)/60           ,
     *                        MOD(ITIME ,60)
      ELSE
           WRITE(LUNOUT,2010) LUNIN, LUNTXT, ITIME
      ENDIF
      CALL SRSTOP(1)
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
 2010 FORMAT ( ' ERROR   ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME:',I10,' !')
 2020 FORMAT ( ' ERROR   ON UNIT:',I10,', READING: ',A,/
     *         ' SIMULATION TIME:',I5,'D ',I2,'H ',I2,'M ',I2,'S !')
 2030 FORMAT ( ' ERROR   ON UNIT:',I10,', READING: ',A,/
     *    ' SIMULATION TIME:',I2,'Y ',I3,'D ',I2,'H ',I2,'M ',I2,'S .')
C
      END
