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

C
C  *********************************************************************
C  *     SUBROUTINE TO PRINT HEADINGS FOR SEVERAL OUTPUT FILES         *
C  *********************************************************************
C
      SUBROUTINE HEADIN(NZOUT,WORDS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'ioblck.inc'
      INTEGER NUMTYP(MT)
      CHARACTER*4 WORDS2(12),WORDS3(20),WORDS4(20)
      CHARACTER*8 WORDS(*),WWORDS(MG)
C
C  Print heading for output on units OUUNI, IOU(14), NZOUT, IOU(21),
C  IOU(24) and IOU(25).
C
C  Set print-array indices.
C
      NUNU2 = NUNUCO * 2
      NTS6=NUABCO+3
      NTS7=NTS6+1
      NTS14=NUECOG+NTS7+1
C
C Exit if LPRINT <= 1: nothing more to be done here.
C
      IF (LPRINT .LE. 1) RETURN
C
C  Write heading for (optional) zooplankton output NZOUT.
C
      IF (NUGRAZ.GT.1) THEN
        DO 11 IG=1,NUGRAZ
          WRITE(WWORDS(IG),'(A4,1X,I2)') WORDS(5),IG
   11   CONTINUE
      ENDIF
      IF (IPERM .GT. 1) THEN
         WRITE(NZOUT,10)
   10    FORMAT(2X,'Summary of all solutions for this run:',/,' ')
         IF (NUGRAZ.GT.1) THEN
           WRITE(NZOUT,21) (WORDS(K),K=1,4),(WWORDS(IG),IG=1,NUGRAZ),
     1                  (GRNAME(K),K=1,NUECOG),(WORDS(K),K=6,7)
   21      FORMAT(2X,A4,2X,2(A8,1X),7X,(A4,1X),32(A8,1X))
         ELSE
           WRITE(NZOUT,20) (WORDS(K),K=1,5),(GRNAME(K),K=1,NUECOG),
     1                  (WORDS(K),K=6,7)
   20      FORMAT(2X,A4,2X,2(A8,1X),7X,2(A4,1X),1X,12(A8,1X))
         ENDIF
         WRITE (NZOUT,150)
      END IF
C
C  Determine main active program options and store them in WORDS4.
C
      J = 1
      IF (LOBFUN .EQ. 1) THEN
         WORDS4(J) = 'Grow'
         J = J + 1
         WORDS4(J) = 'th. '
      ELSE
         WORDS4(J) = 'Biom'
         J = J + 1
         WORDS4(J) = 'ass.'
      END IF
      IF (LGROCH .EQ. 1) THEN
         J = J + 1
         WORDS4(J) = 'Groc'
         J = J + 1
         WORDS4(J) = 'heck'
      END IF
      IF (LMORCH .EQ. 1) THEN
         J = J + 1
         WORDS4(J) = 'Morc'
         J = J + 1
         WORDS4(J) = 'heck'
      END IF
      IF (LDAYEU .EQ. 1) THEN
         J = J + 1
         WORDS4(J) = 'Daye'
         J = J + 1
         WORDS4(J) = 'upho'
      END IF
      IF (LPMORT .EQ. 1) THEN
         J = J + 1
         WORDS4(J) = 'Pmax'
         J = J + 1
         WORDS4(J) = '+Mor'
      END IF
C
C  Write heading for standard output file (OUUNI).
C
      WRITE(OUUNI,25) (WORDS4(I),I=1,J)
      IF (IOFLAG .EQ. 1) WRITE(IOU(21),25) (WORDS4(I),I=1,J)
      WRITE(OUUNI,30)
   25 FORMAT(4X,'Model objective: Maximize ',2A4,/,
     1       4X,'Main active program options: ',9(A4,A4,'; '))
   30 FORMAT(2(' ',/),14X,'Summary of solutions for this run:',/,' ')
C
C  Write heading unit IOU(24). This file contains biomasses of all
C  types.
C
      CALL FORMFE (IOU(24))
      WRITE(IOU(24),40)
   40 FORMAT(8X,'Dry weight biomasses of phytoplankton types:',2(/,' '))
C
C  Split species names in two parts and store truncated second parts
C  of names in "WORDS2".
C
      DO 50 K=1,NUECOG
      WORDS2(K) = GRNAME(K) (5:8)
   50 CONTINUE
      WORDS2(NUECOG+1) = WORDS(6) (5:8)
      WORDS2(NUECOG+2) = WORDS(7) (5:8)
      NUECO2=NUECOG+2
C
C Get names and relative numbers of types for heading unit IOU(24).
C Store first parts of names in WORDS3, get second parts from WORDS2.
C
      KK = 0
      DO 70 I = 1,NUECOG
      K = 0
      DO 60 J = IT2(I,1),IT2(I,2)
         KK =KK + 1
         K = K + 1
         NUMTYP(KK) = K
         WORDS3(KK) = GRNAME (I) (1:4)
         WORDS4(KK) = WORDS2(I)
   60 CONTINUE
   70 CONTINUE
C
C Print names and relative numbers of types to unit IOU(24).
C
      WRITE(IOU(24),80) WORDS(1),(WORDS3(K),K=1,NUSPEC)
      WRITE(IOU(24),90) (WORDS4(K),K=1,NUSPEC)
   80 FORMAT (1X,A4,1X,2X,20(A4,'-',1X))
   90 FORMAT (9X,20(A4,2X))
      WRITE(IOU(24),100) (NUMTYP(K),K=1,NUSPEC)
  100 FORMAT (7X,20(I4,2X))
C
C Write heading for unit OUUNI. This heading differs for interactive
C and batch runs.
C
      IF (IOFLAG .EQ. 0) THEN
        IF (NUGRAZ.GT.1) THEN
          WRITE(OUUNI,141) (WORDS(K),K=1,3),(WWORDS(IG),IG=1,NUGRAZ),
     1                     (GRNAME(K),K=1,NUECOG),(WORDS(K),K=6,7)
        ELSE
          WRITE(OUUNI,140) (WORDS(K),K=1,3),WORDS(5),
     1                     (GRNAME(K),K=1,NUECOG),(WORDS(K),K=6,7)
        ENDIF
      ELSE

         WRITE(OUUNI,110) (WORDS(K),K=1,3),(GRNAME(K),K=1,NUECOG),
     1                    (WORDS(K),K=6,7)
  110    FORMAT(1X,A4,1X,A5,4X,A4,3X,11(A4,'-',1X))
         WRITE(OUUNI,120) (WORDS2(K),K=1,NUECO2)
  120    FORMAT(24X,11(A4,2X))
C
C  Write heading for output IOU(21). This is the same heading written to
C  unit OUUNI in a batch job.
C
         WRITE(IOU(21),30)
         IF (NUGRAZ.GT.1) THEN
           WRITE(IOU(21),141) (WORDS(K),K=1,3),(WWORDS(IG),IG=1,NUGRAZ),
     1                        (GRNAME(K),K=1,NUECOG),(WORDS(K),K=6,7)
         ELSE
           WRITE(IOU(21),140) (WORDS(K),K=1,3),WORDS(5),
     1                        (GRNAME(K),K=1,NUECOG),(WORDS(K),K=6,7)
         ENDIF
  140    FORMAT(2X,A4,2X,2(A8,1X),8X,A4,5X,12(A8,1X))
  141    FORMAT(2X,A4,2X,2(A8,1X),8X,32(A8,1X))
         WRITE (IOU(21),150)

      END IF
      WRITE (OUUNI,150)
  150 FORMAT(' ',/,' ')
C
C  Write heading unit IOU(25). This unit contains forcing function
C  values. Overwrite (!) WORDS3, which is no longer needed for species
C  names.
C
      CALL FORMFE (IOU(25))
      WRITE (IOU(25),160)
  160 FORMAT (10X,'Summary of forcing functions used this run.',
     1        /,' ',/,' ')
      WORDS3(1) = 'Date'
      WORDS3(2) = 'Temp'
      WORDS3(3) = 'Sola'
      WORDS3(4) = 'Chl '
      DO 170 I = 1,NUNUCO
      WORDS3(I+4) = CSTRA (I) (1:4)
  170 CONTINUE
      WORDS3(NUNUCO+5) = 'Kb  '
      WORDS3(NUNUCO+6) = 'Dayl'
      WORDS3(NUNUCO+7) = 'Mort'
      WORDS3(NUNUCO+8) = 'Zood'
      WORDS3(NUNUCO+9) = 'Dept'
      WRITE(IOU(25),180) (WORDS3(I),I=1,NUNUCO+9)
  180 FORMAT(1X,15(A4,4X))
C
C  Write heading for output IOU(14).
C
      WRITE (IOU(14),200)
  200 FORMAT (2X,'Particulate organic and dissolved nutrient concentra',
     1        'tions at equilibrium in mg / m3',/,'  Comparison of ',
     2        'calculated maximum steady state values of chlorophyll',
     3        ' to observations',3(/,' '))
      WRITE (IOU(14),220) WORDS(1),(WORDS(9),WORDS(10),K=1,NUNUCO),
     1                    WORDS(12),WORDS(7),WORDS(11)
  220 FORMAT (2X,A4,6X,12(A8,4X))
      WRITE (IOU(14),240) ((CSTRA(K),I=1,2),K=1,NUNUCO)
  240 FORMAT (10X,10(A8,4X))
C
C Future heading for output IOU(26). This file contains the limiting
C factors.
C
C     WRITE (IOU(26),250) (LIMNAM(I),I=1,NUNUCO+3)
C 250 FORMAT (1X,8(A3,1X))
      RETURN
      END
