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

C    Module to read BLOOM input files
C
C    Called by: BLOOMC
C    Calls    : INPUT2, OPTION, CHANGE, VIDEO
C
      SUBROUTINE BLINPU (NTYP_M,NTYP_A,NGRO_A,ALGTYP)

C     Arguments
C
C     971217  Marnix vd Vat   MrtExAlg added
C
C     Name    Type  Length   I/O  Description
C
C     NTYP_A  I     1        O    Actual number of types
C     NGRO_A  I     1        O    Actual number of groups
C     ALGTYP  R   0:20,*      O    Characteristics per algae type

      INTEGER         NTYP_A, NGRO_A
C
C     Common block variables used
C
C     Name    Type  Length   I/O  Inc-file  Description
C
C     IOU     I     99       I    ioblck    Logical unit numbers
C     INUNI   I     1        I    ioblck    Logical unit number
C     OUUNI   I     1        I    ioblck    Logical unit number
C     IYEAR   I     1        O    putin1
C     CASE    C*8   13       O    putin1
C     COM     C*8   18       O    putin1
C     NREP    I     1        O    phyt2
C     NPRINT  I     1        O    sumout
C     NPRODU  I     1        O    size
C     LPRINT  I     1        O    sumout
C     LDYN    I     1        O    dynam
C     MI      I     1        O    putin1
C     NPER    I     10,3     I    putin1
C     IMU     I     1        O    putin1
C     NUSPEC  I     1        I    phyt2
C     NUECOG  I     1        I    phyt2
C     MT      I     1        I    blmdim
C     LRUN    I     1        I    putin1

      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'putin2.inc'
      INCLUDE 'size.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'dynam.inc'
C
C     Local variables
C
C     Name    Type  Length   I/O  Description
C
C     NDEC    I     1             Dummy in coupled version
C     LPARAM  I     1
C     I       I     1

      INTEGER      NDEC  , LPARAM, I
      REAL         ALGTYP(0:20,NTYP_M),AUTOFR
      LOGICAL      LMIXO,LFIXN
C
C  Read title lines of BLOOM II input file.
C  Note: In the standalone BLOOM II version these comments are read by
C  INPUT1.
C
      READ (INUNI, '(I4,1X,9A8)') IYEAR, (CASE (I), I = 1,9)
      READ (INUNI, '(9A8,8X)') COM
      DO I = 0 , MG
         DO J = 1 , 52
            ZOOD(J,I) = 0.0D0
         ENDDO
      ENDDO

C  DETERMINE NUSPEC AND NUECOG
      IS = 0
      NUECOG = 0
   60   IS = IS + 1
        IF ((ALGTYP(0,IS).GT.-100.).AND.(IS.LE.NTYP_M)) THEN
          IF (IS.EQ.1) THEN
            J=1
            IT2(1,1)=1
          ELSEIF (IS.EQ.NTYP_M) THEN
            IT2(J,2) = NTYP_M
          ELSEIF (NINT(ALGTYP(1,IS)).NE.NINT(ALGTYP(1,IS-1))) THEN
            IT2(J,2) = IS-1
            J = J + 1
            IT2(J,1) = IS
          ENDIF
          IT2(J,2) = IS
          NUECOG = MAX(NUECOG,NINT(ALGTYP(1,IS)))
          GOTO 60
        ENDIF

      NUECOG = J
      NUSPEC = IS - 1
      IF ((IS.EQ.NTYP_M).AND.(ALGTYP(0,NTYP_M).GT.-100.)) NUSPEC =NTYP_M

C  SET THE ALGAE CHARACTERISTICS
      LMIXO = .FALSE.
      LFIXN = .FALSE.
      NUNUCO = 3
      DO 70 J=1,NUECOG
        GRNAME(J)(1:1) = CHAR(ICHAR('A')+J-1)
        K = 0
        DO 80 I=IT2(J,1),IT2(J,2)
          K = K + 1
          SPNAME(I)(1:1) = CHAR(ICHAR('A')+J-1)
          WRITE(SPNAME(I)(3:3),'(I1)') K
          CTODRY(I) = ALGTYP(3,I)
          EKX(I)    = ALGTYP(2,I) * 0.001 / CTODRY(I)
          AA(1,I)   = ALGTYP(4,I) / CTODRY(I)
          AA(2,I)   = ALGTYP(5,I) / CTODRY(I)
          AA(3,I)   = ALGTYP(6,I) / CTODRY(I)
          IF (ALGTYP(16,I).GT.0.0) LMIXO = .TRUE.
          IF (ALGTYP(17,I).GT.0.0) LMIXO = .TRUE.
          IF (ALGTYP(18,I).GT.0.0) LFIXN = .TRUE.
          CHLTOC(I) = 1./ ALGTYP(7,I)
          CHLR(I)   = CHLTOC(I)*CTODRY(I)
          PMAX1(I)  = ALGTYP(8,I)
          PMAX2(I)  = ALGTYP(9,I)
          IF (NINT(ALGTYP(10,I)).EQ.0) THEN
            LPMAX(I) = 1
          ELSE
            LPMAX(I) = 0
          ENDIF
          RMORT1(I) = ALGTYP(11,I)
          RMORT2(I) = ALGTYP(12,I)
          RMORT3(I) = ALGTYP(20,I)
          RES1(I)   = ALGTYP(13,I)
          RES2(I)   = ALGTYP(14,I)
          SDMIX(I)  = ALGTYP(19,I)
          IF (I.EQ.1) THEN
            AUTOFR  = ALGTYP(15,I)
            AVAILN=DBLE(1.D0 - AUTOFR)
          ELSE
            IF (ABS(ALGTYP(15,I)-ALGTYP(15,1)).GT.1.0E-6) THEN
              WRITE(*,*) 'Fraction autolyse must be the same for all ',
     1                   'BLOOM algae types'
              CALL SRSTOP(1)
            ENDIF
          ENDIF
   80   CONTINUE
   70 CONTINUE

C     Mixotrophic and/or nitrogen fixing algae
      IF (LMIXO.AND.LFIXN) THEN
        NUNUCO = 6
        DO 90 I=1,NUSPEC
          IF (ALGTYP(16,I).GT.0.0) THEN
            AA(4,I)   = ALGTYP(16,I) / CTODRY(I)
          ELSE
            AA(4,I)   = 0.0
          ENDIF
          IF (ALGTYP(17,I).GT.0.0) THEN
            AA(5,I)   = ALGTYP(17,I) / CTODRY(I)
          ELSE
            AA(5,I)   = 0.0
          ENDIF
          IF (ALGTYP(18,I).GT.0.0) THEN
            AA(6,I)   = ALGTYP(18,I) / CTODRY(I)
          ELSE
            AA(6,I)   = 0.0
          ENDIF
   90   CONTINUE
        CSTRA(4) = 'N-Detr'
        LIMNAM(4) = 'N-D'
        CSTRA(5) = 'P-Detr'
        LIMNAM(5) = 'P-D'
        CSTRA(6) = 'N-Fix'
        LIMNAM(6) = 'N-F'
      ELSEIF (LMIXO) THEN
        NUNUCO = 5
        DO 100 I=1,NUSPEC
          IF (ALGTYP(16,I).GT.0.0) THEN
            AA(4,I)   = ALGTYP(16,I) / CTODRY(I)
          ELSE
            AA(4,I)   = 0.0
          ENDIF
          IF (ALGTYP(17,I).GT.0.0) THEN
            AA(5,I)   = ALGTYP(17,I) / CTODRY(I)
          ELSE
            AA(5,I)   = 0.0
          ENDIF
  100   CONTINUE
        CSTRA(4) = 'N-Detr'
        LIMNAM(4) = 'N-D'
        CSTRA(5) = 'P-Detr'
        LIMNAM(5) = 'P-D'
      ELSEIF (LFIXN) THEN
        NUNUCO = 4
        DO 110 I=1,NUSPEC
          IF (ALGTYP(18,I).GT.0.0) THEN
            AA(4,I)   = ALGTYP(18,I) / CTODRY(I)
          ELSE
            AA(4,I)   = 0.0
          ENDIF
  110   CONTINUE
        CSTRA(4) = 'N-Fix'
        LIMNAM(4) = 'N-F'
      ENDIF

C
C  Call subroutine INPUT2 to read BLOOM specific data for
C  species, constraints, stochiometry etc.
C
      NDEC = 0
      CALL INPUT2 (NDEC,INUNI,IOU(12),1)
C
C  Close the efficiency file.
C
      CLOSE (IOU(12))
C
C  Set various counters used in several routines of BLOOM II.
C  NREP   = counter for number of calls to all main BLOOM II routines.
C  NPRINT = counter for print routines.
C  NPRODU = counter for BLOOM II production routines (which are NOT
C           used here).
C  LPRINT = flag indicating whether normal BLOOM II output routines
C           are called (LPRINT = 1) or not (LPRINT = 0).
C  LDYN   = flag indicating whether BLOOM II runs in full dynamic mode
C           (LDYN = 1)
C  MI     = number of time periods considered in one computation step of
C           BLOOM II.
C
      NREP   = 0
      NPRINT = 0
      NPRODU = 0
      LPRINT = 1
      LDYN   = 1
      MI     = NPER (1,3)
      IMU    = 1
C
C  Call subroutine "OPTION" to read options for program control.
C  If "RUN" was not specified or if the program has detected
C  an error, it will terminate.
C
      CALL OPTION (0,LPARAM)
      IF (LPARAM. EQ. 1) CALL CHANGE(1)
      CLOSE (IOU(9))
      IF (NUSPEC .GT. MT) GOTO 901
      IF (NUNUCO .GT. MN) GOTO 901
      IF (LRUN .EQ. 0) THEN
         WRITE (OUUNI,40)
   40    FORMAT (1X,'No "RUN" command or a fatal error was detected; ',
     1           'execution terminates',/)
C$ Dit moet waarschijnlijk gewoon weg, is alleen maar scherm-actie??
         CALL VIDEO (0)
         CALL VIDEO (3744)
         GOTO 902
      ENDIF

C     Pass actual number of groups and species to main program

      NTYP_A = NUSPEC
      NGRO_A = NUECOG

   50 CONTINUE

c      write(*,*)'Arjen: BLINPU: nunuco=',nunuco

      RETURN

C     $ Handel deze foutmeldingen netjes af
C     Maximum number permitted species exceeded
C     Present program version can only handle MT phytoplankton species.

  901 STOP 'Fatal error 901 in BLINPU'

C     No "RUN" command or a fatal error was detected,
C     execution terminates

  902 STOP 'Fatal error 902 in BLINPU'

      END
