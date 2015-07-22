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
C  *     SUBROUTINE TO CALCULATE PRODUCTION,RESPIRATION,MORTALITY,     *
C  *            FLUSHING AND GRAZING RATES OF THE BLOOM                *
C  *********************************************************************
C
      SUBROUTINE PRODUC(XDEF,GRAMOR,DEATH,CDATE,DAY,TEMP,DEP,LFIN)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'ioblck.inc'
      DIMENSION XDEF(*),GRAMOR(*),PRODRA(MT),RESPRA(MT),DEATRA(MT),
     1          FLUSRA(MT),GRAZRA(MT),PRODGR(MS),RESPGR(MS),DEATGR(MS),
     2          FLUSGR(MS),GRAZGR(MS)
      CHARACTER*8 WORDS(3),CDATE
      DATA WORDS /'Date    ','Total-C ','Total-O2'/
C
C  Calculate production, respiration, mortality, flushing and
C  grazing rates in the bloom at equilibrium.
C  Units: mg C / m2 / day for the species and total-C, and
C  mg O2 /m2 / day for total-O2.
C
C  Notice that non-unique solutions, no matter how they occur,
C  should all have the same amount of edible algae with the
C  same grazing rate; if not, the solution with the lowest grazing
C  grazing rate would have been unique.
C
C  Use the conversion array CTODRY to convert dry weights to C.
C  Respiratory quotient RQ=1.00
C  PhotosynthetiC quotient PQ=1.20
C  Print heading for output on tape IOU(17)
C
      IF (LFIN .EQ. 1) GOTO 130
      NPRODU=NPRODU+1
      IF (NPRODU .GT. 1) GO TO 30
      CALL FORMFE (IOU(17))
      WRITE(IOU(17),10)
   10 FORMAT('  Production, respiration, mortality, flushing and',
     1       ' grazing rates at equilibrium.',/,'    Units are',
     2       ' mg C / m2 day or mg O2 / m2 / day.',///)
      WRITE(IOU(17),20) WORDS(1),(GRNAME(K),K=1,NUECOG),(WORDS(K),K=2,3)
   20 FORMAT(4X,A4,15X,22(A8,1X))
      LINEPA = 0
      PQ=1.20
      RQ=1.00
      COXRAT=32./12.
      YEARPR = 0.0
      YEAROX = 0.0
   30 CONTINUE
      SUMPRO=0.
      SUMRES=0.
      SUMDEA=0.
      SUMFLU=0.
      SUMGRA=0.
      ALIVE=0.
      K1 = NUROWS
      DO 40 K=1,NUSPEC
      K1 = K1 + 1
      PRODRA(K)=0.
      RESPRA(K)=0.
      DEATRA(K)=0.
      FLUSRA(K)=0.
      GRAZRA(K)=0.
      IF (XDEF(K1) .LT. 1.D-6) GO TO 40
      RESPK=RESP(K)
      GRAMK=GRAMOR(K)
      DEATK=RMORT(K)
      XTOC=XDEF(K1)*DEP/CTODRY(K)
      ALIVE=ALIVE+XTOC
      PRODRA(K)=(RESPK+GRAMK+DEATK+FLUSH)*XTOC
      RESPRA(K)=RESPK*XTOC
      DEATRA(K)=DEATK*XTOC
      FLUSRA(K)=FLUSH*XTOC
      GRAZRA(K)=GRAMK*XTOC
      SUMPRO=SUMPRO+PRODRA(K)
      SUMRES=SUMRES+RESPRA(K)
      SUMDEA=SUMDEA+DEATRA(K)
      SUMFLU=SUMFLU+FLUSRA(K)
      SUMGRA=SUMGRA+GRAZRA(K)
   40 CONTINUE
C
C  Get totals for groups from totals per species.
C
      DO 45 I=1,NUECOG
      PRODGR(I) = 0.0
      RESPGR(I) = 0.0
      DEATGR(I) = 0.0
      FLUSGR(I) = 0.0
      GRAZGR(I) = 0.0
      L1 = IT2(I,1)
      L2 = IT2(I,2)
      DO 44 L=L1,L2
      PRODGR(I) = PRODGR(I) + PRODRA(L)
      RESPGR(I) = RESPGR(I) + RESPRA(L)
      DEATGR(I) = DEATGR(I) + DEATRA(L)
      FLUSGR(I) = FLUSGR(I) + FLUSRA(L)
      GRAZGR(I) = GRAZGR(I) + GRAZRA(L)
   44 CONTINUE
   45 CONTINUE
C
C  Convert production, respiration, mortality and grazing rates
C  to oxygen units.
C
      COXPQ=COXRAT*PQ
      SUMPOX=SUMPRO*COXPQ
      SUMROX=SUMRES*COXRAT/RQ
      SUMDOX=SUMDEA*COXPQ
      SUMFOX=SUMFLU*COXPQ
      SUMGOX=SUMGRA*COXPQ
C
C  Increase YEARPR and YEAROX to compute average production.
C
      YEARPR = YEARPR + SUMPRO
      YEAROX = YEAROX + SUMPOX
C
C  Print heading for new page.
C
      LINEPA = LINEPA + 1
      IF (LINEPA .LE. 8) GO TO 50
      CALL FORMFE (IOU(17))
      WRITE(IOU(17),10)
      WRITE(IOU(17),20) WORDS(1),(GRNAME(K),K=1,NUECOG),(WORDS(K),K=2,3)
      LINEPA = 1
C
C  Print production, respiration, mortality flushing and grazing rates.
C
   50 CONTINUE
      WRITE(IOU(17),60) CDATE,(PRODGR(K),K=1,NUECOG),SUMPRO,SUMPOX
   60 FORMAT(/,4X,A4,2X,'Production',1X,12(F8.1,1X))
      WRITE(IOU(17),70) (RESPGR(K),K=1,NUECOG),SUMRES,SUMROX
   70 FORMAT(10X,'Respiration',12(F8.1,1X))
      WRITE(IOU(17),80) (DEATGR(K),K=1,NUECOG),SUMDEA,SUMDOX
   80 FORMAT(10X,'Mortality',2X,12(F8.1,1X))
      WRITE(IOU(17),85) (FLUSGR(K),K=1,NUECOG),SUMFLU,SUMFOX
   85 FORMAT(10X,'Flushing',3X,12(F8.1,1X))
      WRITE(IOU(17),100) (GRAZGR(K),K=1,NUECOG),SUMGRA,SUMGOX
  100 FORMAT(10X,'Grazing',4X,12(F8.1,1X))
C
C  Call subroutine to distribute the production and respiration rates
C  during the day.
C
      IF (LDIEL .NE. 1) GO TO 110
      CALL DIEL(SUMPOX,SUMROX,DAY,CDATE)
  110 CONTINUE
C
C  Call subroutine to calculate the concentrations of living and dead
C  algae and the sedimentation rate of dead algae.
C
      IF (LPOOLS .NE. 1) GO TO 120
      CALL POOLS(CDATE,DEATH,ALIVE,TEMP)
  120 CONTINUE
      RETURN
  130 CONTINUE
C
C Compute and print average production rate.
C
      CAV = YEARPR / (1000. * NREP)
      OAV = YEAROX / (1000. * NREP)
      WRITE (IOU(17),140) CAV,OAV
  140 FORMAT (///,'   Average gross production rate =  ',F4.2,
     1        ' grams C  per m2 per day.',/,
     2        '   Average gross production rate = ',F5.2,
     3        ' grams O2 per m2 per day.')
      RETURN
      END
