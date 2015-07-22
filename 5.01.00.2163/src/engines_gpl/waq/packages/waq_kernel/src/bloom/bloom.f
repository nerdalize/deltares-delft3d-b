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

C    Date:       31 Dec 1999
C    Time:       11:52
C    Program:    BLOOM.FOR
C    Version:    1.82
C    Programmer: Hans Los
C    Previous version(s):
C    1.82 -- 30 Dec 1999 -- 08:36 -- Operating System: DOS
C    1.81 -- 12 Feb 1993 -- 08:33 -- Operating System: DOS
C    1.8 -- 11 Feb 1993 -- 14:08 -- Operating System: DOS
C    1.7 -- 4 Nov 1992 -- 14:17 -- Operating System: DOS
C    1.6 -- 26 Feb 1992 -- 19:09 -- Operating System: DOS
C    1.5 -- 26 Dec 1991 -- 19:57 -- Operating System: DOS
C    1.4 -- 7 May 1991 -- 13:30 -- Operating System: DOS
C    1.3 -- 21 Jun 1990 -- 14:14 -- Operating System: DOS
C    1.2 -- 9 Jan 1990 -- 08:33 -- Operating System: DOS
C    1.1 -- 9 Jan 1990 -- 08:08 -- Operating System: DOS
C    1.0 -- 2 Jan 1990 -- 08:05 -- Operating System: DOS
C    BLOOM FORTRAN -- 20 Dec 1989 -- 07:47 -- Operating System: DOS
C    BLOOM.FOR  -- 22 Nov 1989 -- 13:03 -- Operating System: CMS
C    BLOOM FORTRAN -- 20 Nov 1989 -- 14:57 -- Operating System: DOS
C    BLOOM FORTRAN -- 26 Oct 1989 -- 10:20
C    1.0 -- 26 Oct 1989 -- 07:52
C    0.0 -- 26 Oct 1989 --  7:52
C    and many, many others!
C
C    Update 1.82: Added LCOUPL flag to if statement before call to
C                 MAXMOR:this makes it possible to maitain a single
C                 version on all platforms!
C                 Added conditional write statements for N fixation
C                 and or mixotrophy (NUNUCO > 3). Although not very
C                 elegant, the same code can be used for all possible
C                 cases.
C
C    0895 MvdV extended for multiple grazer types, subroutine CONSBL added
C              CONSBL is called if NUGRAZ > 0
C              extension of the dimension of ZOOD for multiple grazers
C              addition of ZOOC, GRADET, GDTOLD
C
C    Update 1.81: Solved the actual cause of problem at 1.8. Error in
C                 PRINT6. This module needs both INOW and J so changed
C                 the header.
C
C    Update 1.8: Use logical LSOLU to determine if at least one feasible
C                solution exists. Previous criterium SOMETIMES failed
C                in a situation of photo-inhibition.
C
C    Update 1.7: Added DEP to Calls of SETABC, MAXGRO and MAXGRA.
C
C    Update 1.6: Changed call to FIXINF.

C    Update 1.5: Alternative optimization scheme.
C       All types of a species with the same KMAX, but with different
C       objective function.
C       Modified modules:
C       BLOOM.FOR
C       MAXGRO.FOR
C    Update 1.4: Distinguish LPRINT = 0, 1, or 2 (See RUN).
C    Update 1.3: Exit as soon as possible if solar radiation is below
C    100 J/m2/week -- speed of ECOLUMN - BLOOM II computations for
C    for segments 2 and 3.
C    Update 1.2: INT initialized at 0; initialization of NIN moved to
C    section included EACH run through the subroutine.
C    Update 1.1: CSOL = DSOL/10000 * DAY set upon exit of the routine to
C    enable calling programs using the same, corrected solar intensity
C    level as BLOOM II.
C
C  *********************************************************************
C  *    SUBROUTINE FOR SETTING UP AND SOLVING BLOOM MODEL PROBLEM      *
C  *********************************************************************
C
C
      SUBROUTINE BLOOM(CDATE,ID,MI,T,CSOL,PHYT,EXTB,DAY,DEATH,ZOOD,
     1           DEP,XINIT,XDEF,XECO,TOTAL,EXTTOT,EXTLIM,NSET,INFEAS,
     2           NONUN,NUMUN,LCOUPL)

      USE DATA_3DL

      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'dynam.inc'
      INCLUDE 'ioblck.inc'
      INTEGER IRS3
      INTEGER NONUNI(MT),NONUN(MT),IRS(3),LIB(MX),JKMAX(MS)
      SAVE    IRS, IRS3
      DIMENSION X(MX),XINIT(*),XDEF(*),BIO(2),GROOT(2),
     1          OROOT(2*MT),ROOT(2),GRAMOR(MT),EMIN(MT),
     2          OUT15(20+MG),OUTST(20+MG),XECO(*),XECOST(MS),ZOOD(0:MG),
     3          ZOOC(MG),GRADET(MN),GDTOLD(MN)
      CHARACTER*8 CDATE
*     CHARACTER*4 COUT(10),COUTST(10)
      CHARACTER*4 COUT(MN+5),COUTST(MN+5)
      CHARACTER*1 ERRIND
      LOGICAL LSOLU
      PARAMETER (SOLMIN=100.0)

C  Save the amount of grazer biomass for the first timestep
      IF ((NREP.EQ.1).AND.(NUGRAZ.GT.0)) THEN
C     No grazing coupled version
        IF (LCOUPL.NE.0) THEN
          NUGRAZ = 0
        ELSE
          DO 620 J=1,NUGRAZ
            ZOOC(J) = ZOOD(J)
  620     CONTINUE
        ENDIF
      ENDIF

C
C
C  Calculate maximum primary production and respiration rates.
C
      CALL MAXPRD(T)
C
C  In the stand-alone version of BLOOM II
C  compute the total extinction (EXTTOT) as:
C
C   EXTTOT = EXTB + EXLIVE + EXDEAD
C
C  to account for eventual variations in the background extinction.
C  Set A-matrix and B and C vectors; inititate detritus pools (optional)
C  In coupled model versions EXTTOT is updated by the calling program.
C
C
      IF (NREP .NE. 1 .AND. LCOUPL .EQ. 0) EXTTOT = EXTB+EXLIVE+EXDEAD
C
C  Update 900109:
C  Make a copy of the (unconverted) solar radiation level. PRINUN needs
C  this value to replicate the boundary conditions.
C  Update 28 oct 92: added DEP to argument list.
C
      USOL = CSOL
      CALL SETABC(XINIT,EXTB,EXTTOT,ZOOD,CSOL,DSOL,T,DEP,ID,NSET,LCOUPL)
C
C   Test for (in)feasibility of the nutrient constraints in
C   a run with a dynamic detritus computation.
C
      IF (LDYDEA .EQ. 0) GO TO 10
      CALL NUTFEA (INFEAS)
      IF (INFEAS .EQ. 1) THEN
         IRERUN = 3
         GO TO 270
      END IF
   10 CONTINUE
C
C  Calculate minimum efficiency requirements for groups
C  with initial grazing rate gramo1.
C
C  0895 MvdV Calculate initial grazing pressure for multiple grazers
C            and correct the amount of available nutrients
      DO 580 I=1,NUNUCO
        GRADET(I) = 0.0D0
  580 CONTINUE
      DO 20 K=1,NUSPEC
        IF (NUGRAZ.GT.0) THEN
          GRAMOR(K) = 0.0
          DO 500 J=1,NUGRAZ
            GRAMOR(K)=GRAMOR(K) + GRAMO1*ZOOPR(K,J)
            DO 590 I=1,NUNUCO
              GRADET(I) = GRADET(I) - GRAMO1*ZOOPR(K,J)*GFECFR(K,J) *
     1                    AA(I,K)*CTODRY(K)
  590       CONTINUE
  500     CONTINUE
        ELSE
          GRAMOR(K)=GRAMO1*ZOOPR(K,0)
        ENDIF
   20 CONTINUE
      IF (NUGRAZ.GT.0) THEN
        DO 510 J=1,NUGRAZ
          DO 600 I=1,NUNUCO
            GRADET(I) = GRADET(I) + GRAMO1 * GDETPR(J) *
     1                  (1.0 - GDETFF(J))
  600     CONTINUE
  510   CONTINUE
      ENDIF
C     Correct detritus and available nutrients for the grazing rate
      IF ((LCOUPL.EQ.0).AND.(NUGRAZ.GT.0).AND.(LDYDEA.EQ.1).AND.
     1    (REMEXP(1).GT.1.0D-20)) THEN
        DO 520 J=1,NUNUCO
          REMINU(J)= REMINU(J) + GRADET(J)
          REMEXP(J) = DEXP(-REMINU(J)*TSTEP*MI)
          DO 530 K=1,NUSPEC
            A(J,K)=AA(J,K)*(AVAILN*RMORT(K)*(1.0-REMEXP(J))+REMINU(J))/
     1             REMINU(J)
  530     CONTINUE
          B(J) = CONCEN(J) - DETRIT(J)*REMEXP(J)
          DO 540 K=1,NUGRAZ
            B(J)=B(J)-ZOONUT(J,K)*ZOOD(K)
  540     CONTINUE
  520   CONTINUE
      ENDIF

      ITNUM=0
      IF (IPERM .LE. 1) GO TO 70
      BIOFOR=0.

C
C  Start grazing loop of the program; the loops ends at label 300
C
   40 CONTINUE
      ITNUM=ITNUM+1
      IF (ITNUM .EQ. 1) GO TO 50
      BIOFOR=BIO(2)
   50 CONTINUE
      IF (IDUMP .EQ. 0) GO TO 70
      WRITE (IOU(6),60) ITNUM
   60 FORMAT (2X,'Zooplankton iteration: ',I2)
   70 CONTINUE
C
C Compute the euphotic depth and the euphotic day lenght if option
C "DAYEUPHO" is on.
C
      IF (LDAYEU .EQ. 1) THEN
         CALL DAYEU (DAY,DAYEUF,EXTTOT,DEP,DEPEUF,DSOL,EULIGH,IDUMP)
         DO 75 I = 1,NUSPEC
         DMIX(I) = DABS(SDMIX(I)) * DEPEUF
   75    CONTINUE
      ELSE
         DAYEUF = DAY
         DEPEUF = DEP
      END IF
C
C If the light intensity is very low, declare the problem 'infeasible',
C don't rerun, only write output. But do not do this for 3DL approach the
C efficiency can be obtained in other layers with light.
C
      IF (CSOL .LT. SOLMIN .AND. .NOT. ACTIVE_3DL) THEN
         IRERUN = 0
         ERRIND = ' '
         NI = 0
         GO TO 205
      END IF
C
C Determine indices for interpolation of day length correction factors.
C
      DO 80 INDEX2 = 1,24
      IF (DL(INDEX2) .GE. DAYEUF) GOTO 90
   80 CONTINUE
   90 CONTINUE
      INDEX1 = INDEX2 - 1
C
C  Compute minimum efficiency requirements of species.
C
      DO 110 J=1,NUECOG
      DAYM = DAYMUL(INDEX1,J) + ( DAYMUL(INDEX2,J) - DAYMUL(INDEX1,J) )
     1       * ( DAYEUF - DL(INDEX1) ) / ( DL(INDEX2) - DL(INDEX1) )
      DO 100 K=IT2(J,1),IT2(J,2)
      EMIN(K)=(RESP(K)+RMORT(K)+FLUSH+GRAMOR(K)) / (PMAX(K)*DAYM)
  100 CONTINUE
  110 CONTINUE
C
C  Tranform sunlight to per hour units.
C
      IF (ITNUM .GT. 1) GOTO 130
      DO 120 J=1,NUSPEC
  120 SURF(J)=SURF(J)/DAY
  130 CONTINUE
C
C  Average EMIN in time and find range of extinction coefficient for
C  each species.
C
      DO 170 J=1,NUECOG
      L1 = IT2(J,1)
      L2 = IT2(J,2)
      GROOT (2) = -1.0
      ISKMAX  = L1
      DO 160 K=L1,L2
      IF ( .NOT. ACTIVE_3DL ) THEN
         CALL CONSTR(SURF(K),DEPEUF,EMIN(K),ROOT,J)
      ELSE
         IF ( IFIX_3DL(K) .LT. 0 ) THEN
            EFFI = EFFIC_3DL(K,ISEG_3DL)
         ELSE
            CALL EFFI_3DL(EFFI,K)
         ENDIF
         ROOT(1) = 0.0
         ROOT(2) = EFFI*EXTTOT/EMIN(K)
      ENDIF
      AROOT(2*K-1)=ROOT(1)
      AROOT(2*K)=ROOT(2)
      IF (ROOT(2) .LE. GROOT(2)) GO TO 140
      GROOT(2) = ROOT(2)
      ISKMAX = K
  140 CONTINUE
C
C  Calculate the maximum allowable grazing rate constant
C  if option MAXGRA is chosen.
C
      IF (LGRAMO .EQ. 0 .OR. ITNUM .GT. 1) GO TO 160
      EADJ=EMIN(K)
      IF (GRAMO1 .LT. 1.0D-6) GO TO 150
      EADJ=EMIN(K)-(GRAMOR(K)/PMAX(K))
  150 CONTINUE
C
C  Update nov 4 1992: added DEP to parameter list.
C  0895 MvdV GRAMX added to parameter list for output of max. grazing rate
C            input of old grazing pressure (ZOOD(0))
C
      CALL MAXGRA(ROOT,EXTB,DAYEUF,SURF(K),EADJ,PMAX(K),CDATE,ZOOD(0),J,
     1            K,DEP,GRAMX)
  160 CONTINUE
C
C  New section starts here.
C  We now know the extinction roots all types of species J.
C  Replace them by the MAXIMUM, but only for types, whose KMAX is
C  positive. Do nothing if KMAX is negative.
C
      DO 165 K = IT2(J,1),IT2(J,2)
         IF (K .EQ. ISKMAX) GO TO 165
         IF (AROOT(2*K) .LE. 0.0) GO TO 165
         AROOT(2*K-1)=AROOT(2*ISKMAX-1)
         AROOT(2*K)=AROOT(2*ISKMAX)
 165  CONTINUE

C
C  Calculate the maximum growth rate and the right hand side for the
C  growth constraints of each species, if option "GROCHECK" is on.
C  Copy the growth constraints to BGRO to deal with infeasible
C  solutions, which are recomputed without mortality constraints.
C  Store the type number of each group with the highest KMAX value
C  in JKMAX.
C
C
C  Update nov 4 1992: added DEP to parameter list.
C
C
      IF (LGROCH .EQ. 1 .OR. LOBFUN .EQ. 1) THEN
         CALL MAXGRO(XINIT,GROOT,EXTTOT,EMIN(ISKMAX),GRAMOR(ISKMAX),J,
     *               ISKMAX,DEP)
         BGRO(J) = B(NUEXRO + J)
      END IF
      JKMAX(J) = ISKMAX
  170 CONTINUE
C
C  Compute values for mortality constraints if option "MORCHECK" is on.
C  MvdV 961014 moved from RUN subroutine to iteration loop grazers
C  Hans Los 991231: added LCOUPL
C
      IF (LCOUPL .EQ. 0) THEN
         IF (LMORCH .EQ. 1) THEN
            CALL MAXMOR (XDEF,MI,EXTLIM,INFEAS,GRAMOR)
         ELSE
            EXTLIM = 0.0
         END IF
      END IF
C
C Print KMIN and KMAX roots of types if option "DUMP" is on.
C
      IF (IDUMP .EQ. 1) THEN
         WRITE (IOU(6),180) (AROOT(I),I=1,2*NUSPEC,2)
  180    FORMAT (' KMIN: ',20(F7.2,1X))
         WRITE (IOU(6),190) (AROOT(I),I=2,2*NUSPEC,2)
  190    FORMAT (' KMAX: ',20(F7.2,1X))
      END IF
C
C  Initialize RERUN flag to 0: do not rerun the problem.
C  Set error indicator to blank.
C
      IRERUN = 0
      ERRIND = ' '
C
C  Order extinction values and determine species in intervals.
C  Initialize counter infeasible intervals and number maximum interval
C  at 0
C
  200 CALL SPCSD(AROOT,OROOT,ACO,EXTLIM,EXTB,NI)
  205 NIN=0
      INT=0
      LSOLU = .FALSE.
      IF (NI .NE. 0) GO TO 220
      IF (IDUMP .EQ. 1) WRITE (IOU(6),210)
  210 FORMAT (5X,'No species permitted in any interval')
      INFEAS=1
      INHIB = 0
      GO TO 270
C
C  Begin solving linear programs for each interval.
C  Set initial values.
C
  220 CONTINUE
      BIO(1)=0.0
      BIO(2)=-1.0
C
C----------------------------------------------------------------------
C            Solve program for biomass in each interval
C----------------------------------------------------------------------
C
      IRS(2)=0
      INOW = 0
      INHIB = 0
      DO 250 J=1,NI
      INOW = INOW + 1
C
C  Set "B" values for extinction coefficient rows.
C
      LINF=0
      B(NUFILI)=OROOT(J)
      B(NUABCO)=OROOT(J+1)
C
C  Determine allowable species for feasibility interval J.
C
      CALL EXCLUD (J,LINF,IRS)
      IF (LINF .EQ. 0) GO TO 230
      INOW = INOW - 1
      IF (LINF .EQ. 1) GO TO 240
C
C  LINF = 2: photo inhibition.
C
      IF (IDUMP .EQ. 1) WRITE (IOU(6),225) J
  225 FORMAT(5X,'No species in interval ',I2,' due to photo inhibition')
      NIN = NIN + 1
      INHIB = 1
      GO TO 250
C
C  Solve, test for feasibility, and find total biomass.
C
  230 CONTINUE
      CALL SOLVLP(INOW,X,BIOMAX,IER,IRS,NONUNI,NUMUNI,LIB)
      IF (IER .NE. 0 .AND. INOW .EQ. 1) IRS3 = IRS(3)
      LINF=IER
      IF (IER .EQ. 0) LSOLU = .TRUE.
  240 CALL PRINT6(BIO,BIOMAX,X,XDEF,INOW,J,LINF,IRS,INT,NIN,NONUNI,
     1            NONUN,NUMUNI,NUMUN,LIB)
  250 CONTINUE
C
C  Check to determine if there has been any feasible solution;
C  if not, INFEAS = 1
C
      INFEAS=0
C     IF (NIN .LT. NI) GO TO 280
      IF (LSOLU) GO TO 280
      INFEAS=1
      IF (IDUMP .EQ. 1) WRITE (IOU(6),260)
  260 FORMAT (5X,'No solution--all intervals are infeasible')
C
C  Infeasible solution. Call FIXINF to deal with this problem.
C
  270 CONTINUE
      IRS(3) = IRS3
      CALL FIXINF(XDEF,BIO,EXTTOT,EXTB,INHIB,NI,IRERUN,IRS,INFEAS,
     1            ERRIND,JKMAX,AROOT,CDATE,LCOUPL)
      IF (IRERUN .NE. 0) GO TO 200
C
C----------------------------------------------------------------------
C              START SECOND PART OF THE SUBROUTINE
C----------------------------------------------------------------------
C
C  The program will now:
C    1. Print output according to the options specified and the
C       solution obtained.
C    2. Determine grazing rate constant and re-run for this timeperiod
C       if grazing is substantial.
C    3. Print a message for non-unique solutions of there may have been
C       any.
C
C
C  Print maximum solution(s) on unit 6 if option DUMP was selected.
C  Print all summarized solutions for all zooplankton iterations
C  on unit 15.
C
  280 CONTINUE
      CALL PRINSU(XDEF,XECO,BIO(2),TOTAL,ZOOD,COUT,OUT15,NTSTOT,
     1     ITNUM,15)
      IF (IDUMP .NE. 0) CALL PRINMA(XDEF,BIO(2),TOTAL,NI,NIN,INT)
C
C  If two intervals have the same maximum biomass, print both of them
C  in the complete and summarized output.
C
      IF (LST .EQ. 1) THEN
        CALL PRINSU(XST,XECOST,BIOST,TOTST,ZOOD,COUTST,OUTST,
     1       NTSTOT,ITNUM,15)
        NTSST = NTSTOT
        IF (IDUMP .NE. 0) CALL PRINMA(XST,BIOST,TOTST,NI,NIN,INTST)
      END IF
C
C  Calculate grazing rate constant and recalculate bloom.
C  Further iterations are not necessary if:
C  1. The amount of food for the grazers reaches zero
C  2. The maximum biomass of iteration I is the same as in
C     interation I-1.
C  3. The maximum iteration number is achieved.
C  4. The difference in grazing rates GRADET en GRAMOR between
C     two steps is less than 1.0D-06
C
      IF (IPERM .LE. 1 ) GO TO 300
      IF (ITNUM .EQ. 1 ) GO TO 290
      BISTOP=DABS(BIOFOR-BIO(2))
      IF (BISTOP .LT. 1.D-6) GO TO 300
C     0895 MvdV Start CONSBL if NUGRAZ > 0
C               First save the grazing rate of detritus
  290 IF (NUGRAZ.GT.0) THEN
        DO 610 I=1,NUNUCO
          GDTOLD(I) = GRADET(I)
  610   CONTINUE
        CALL CONSB2(XDEF,GRAMOR,GRADET,ZOOD,ZOOC,ITNUM,LGRAZ,T,
     1              TSTEP*MI,GRAMX,LCOUPL,ZMAX)
C       Correct detritus and available nutrients for the grazing rate
        IF ((LCOUPL.EQ.0).AND.(LDYDEA.EQ.1)) THEN
          DO 550 J=1,NUNUCO
            REMINU(J)= REMINU(J) + GRADET(J) - GDTOLD(J)
            REMEXP(J) = DEXP(-REMINU(J)*TSTEP*MI)
            DO 560 K=1,NUSPEC
              A(J,K)=AA(J,K)*(AVAILN*RMORT(K)*(1.0-REMEXP(J))+
     1               REMINU(J))/REMINU(J)
  560       CONTINUE
            B(J) = CONCEN(J) - DETRIT(J)*REMEXP(J)
            DO 570 K=1,NUGRAZ
              B(J)=B(J)-ZOONUT(J,K)*ZOOD(K)
  570       CONTINUE
  550     CONTINUE
        ENDIF
      ELSE
C  Further iterations are not necessary if:
C  1. The grazing rate constant = 0.0.
C  2. The maximum biomass of iteration I is the same as in
C     interation I-1.
C  3. The maximum iteration number is achieved.
C
        CALL GRAZIN(XDEF,GRAMOR,ZOOD(0),ITNUM,LGRAZ)
      ENDIF
      IF (LGRAZ .EQ. 1) GO TO 40
  300 CONTINUE

C  Save the amount of grazer biomass
      IF (NUGRAZ.GT.0) THEN
        DO 630 J=1,NUGRAZ
          ZOOC(J) = ZOOD(J)
  630   CONTINUE
      ENDIF

C     Extra output
C     WRITE(243,'(4(1PE12.4))') (DETRIT(J),J=1,3),ZOOD(1)
C
C  Compute the total extinction
C  Update nov 4 1992: don't divide by SDMIX. Questionable for species
C  with buoyancy regulation; definitaly incorrect for species at the
C  bottom.
C
      EXDEAD = 0.0
      EXLIVE = 0.0
      K1 = NUROWS
      DO 320 K = 1,NUSPEC
      K1 = K1 + 1
      IF (XDEF(K1) .LT. 1.D-6) GO TO 310
      EKXI = EKX (K) * XDEF (K1)
      EXDEAD = EXDEAD + QMREM * RMORT(K) * EKXI
*     EXLIVE = EXLIVE + EKXI/SDMIX(K)
      EXLIVE = EXLIVE + EKXI
  310 CONTINUE
  320 CONTINUE
      EXTTOT = EXDEAD + EXLIVE + EXTB
C
C  Print summary of solution(s) on unit OUUNI and unit 21.
C
C  0895 MvdV add extra grazers to the output
      IF (NUGRAZ.GT.0) THEN
        NTS8 = NTS7 + NUGRAZ
      ELSE
        NTS8=NTS7+1
      ENDIF
      IF (IOFLAG .EQ. 0) GO TO 360
*     IF ( LPRINT .EQ. 2)  WRITE(IOU(21),330) ERRIND,CDATE,
*    1                     (COUT(J),J=2,NTS6),(OUT15(K),K=NTS7,NTSTOT)
* 330 FORMAT(1X,A1,A5,1X,6(A3,1X),A2,F5.0,2X,14(F8.1,1X))
      IF ( LPRINT .EQ. 2) THEN
        IF (NUNUCO .EQ. 3) WRITE(IOU(21),330) ERRIND,CDATE,
     1                     (COUT(J),J=2,NTS6),(OUT15(K),K=NTS7,NTSTOT)
        IF (NUNUCO .EQ. 4) WRITE(IOU(21),331) ERRIND,CDATE,
     1                     (COUT(J),J=2,NTS6),(OUT15(K),K=NTS7,NTSTOT)
        IF (NUNUCO .EQ. 5) WRITE(IOU(21),332) ERRIND,CDATE,
     1                     (COUT(J),J=2,NTS6),(OUT15(K),K=NTS7,NTSTOT)
        IF (NUNUCO .EQ. 6) WRITE(IOU(21),333) ERRIND,CDATE,
     1                     (COUT(J),J=2,NTS6),(OUT15(K),K=NTS7,NTSTOT)
      END IF
  330 FORMAT(1X,A1,A5,1X,6(A3,1X),A2,F5.0,2X,14(F8.1,1X))
  331 FORMAT(1X,A1,A5,1X,7(A3,1X),A2,F5.0,2X,14(F8.1,1X))
  332 FORMAT(1X,A1,A5,1X,8(A3,1X),A2,F5.0,2X,14(F8.1,1X))
  333 FORMAT(1X,A1,A5,1X,9(A3,1X),A2,F5.0,2X,14(F8.1,1X))
C
C  Convert output to compact format to fit on to terminal screen.
C
      IF (LPRINT .NE. 2) GO TO 387
      DO 340 K=1,NUECOG
      KK=NTS8+K-1
  340 OUT15(KK)=OUT15(KK)/1000.
      OUT15(NTS14)=OUT15(NTS14)/1000.
      IF (NUNUCO .EQ. 3) WRITE(OUUNI,350) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                  (OUT15(K),K=NTS8,NTSTOT)
      IF (NUNUCO .EQ. 4) WRITE(OUUNI,351) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                  (OUT15(K),K=NTS8,NTSTOT)
      IF (NUNUCO .EQ. 5) WRITE(OUUNI,352) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                  (OUT15(K),K=NTS8,NTSTOT)
      IF (NUNUCO .EQ. 6) WRITE(OUUNI,353) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                  (OUT15(K),K=NTS8,NTSTOT)
  350 FORMAT (1X,A1,I2,3X,7(A1,1X),1X,14(F5.1,1X))
  351 FORMAT (1X,A1,I2,3X,8(A1,1X),1X,14(F5.1,1X))
  352 FORMAT (1X,A1,I2,3X,9(A1,1X),1X,14(F5.1,1X))
  353 FORMAT (1X,A1,I2,3X,10(A1,1X),1X,14(F5.1,1X))
      GO TO 370
  360 CONTINUE
      IF (LPRINT .NE. 2) GO TO 387
      IF (NUNUCO.EQ.3) WRITE(OUUNI,330) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
      IF (NUNUCO.EQ.4) WRITE(OUUNI,331) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
      IF (NUNUCO.EQ.5) WRITE(OUUNI,332) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
      IF (NUNUCO.EQ.6) WRITE(OUUNI,333) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
C
C  Print biomasses of phytoplankton types.
C
  370 CONTINUE
      WRITE (IOU(24),375) CDATE,(XDEF(J+NUROWS)/1000,J=1,NUSPEC)
  375 FORMAT (1X,A5,1X,20(F5.1,1X))
C
C  Print particulate organic and dissolved nutrient concentrations
C  on unit 14.
C  Update 1.82: never write more than 3 nutrients to plot files!
C
      NUNU3 = NUNU2 - 2*(NUNUCO-3)
      WRITE (IOU(14),380) CDATE,(PARDIS(K),K=1,NUNU2),EXTTOT,TOTAL,PHYT
      IF ((LPLOT .EQ. 1 .OR. LSCR .EQ. 1) .AND. LCOUPL .NE. 1)
     1  WRITE (IPL3,381) CDATE,(PARDIS(K),K=1,NUNU3),EXTTOT,TOTAL,PHYT
  380 FORMAT (2X,A5,4X,15(F7.2,5X))
  381 FORMAT (2X,A5,4X,15(F11.2,1X))
C
C
C   Echo the input to unit 25.
C
      IF (NUGRAZ.GT.0.) THEN
      WRITE (IOU(25),385) CDATE,T,CSOL,PHYT,(B(I),I=1,NUNUCO),EXTB,DAY,
     1             DEATH,(ZOOD(IG)*1000.*GCTDRY(IG),IG=1,NUGRAZ),DEP
      ELSE
      WRITE (IOU(25),385) CDATE,T,CSOL,PHYT,(B(I),I=1,NUNUCO),EXTB,DAY,
     1             DEATH,ZOOD(0),DEP
      ENDIF
  385 FORMAT (1X,A5,1X,F7.1,1X,2(F7.0,1X),31(F7.2,1X))
C
C  Calculate production and respiration rates if option PRODUC was
C  selected.
C
  387 IF (LPRODU .NE. 1) GO TO 390
      CALL PRODUC(XDEF,GRAMOR,DEATH,CDATE,DAY,T,DEPEUF,0)
  390 CONTINUE
      IF (LST .EQ. 0) GO TO 440
      IF (LPRINT .NE. 2) GO TO 440
      DO 400 K=1,NTSST
  400 OUT15(K)=OUTST(K)
C
C  Print biomasses of phytoplankton types.
C
      WRITE (IOU(24),375) CDATE,(XST(J+NUROWS)/1000,J=1,NUSPEC)
      IF (IOFLAG .EQ. 0) GO TO 420
      IF(NUNUCO.EQ.3)
     1             WRITE(IOU(21),330) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     2             (OUT15(K),K=NTS7,NTSTOT)
      IF(NUNUCO.EQ.4)
     1             WRITE(IOU(21),331) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     2             (OUT15(K),K=NTS7,NTSTOT)
      IF(NUNUCO.EQ.5)
     1             WRITE(IOU(21),332) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     2             (OUT15(K),K=NTS7,NTSTOT)
      IF(NUNUCO.EQ.6)
     1             WRITE(IOU(21),333) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     2             (OUT15(K),K=NTS7,NTSTOT)
      DO 410 K=1,NUECOG
      KK=NTS8+K-1
  410 OUT15(KK)=OUTST(KK)/1000.
      OUT15(NTS14)=OUTST(NTS14)/1000.
      IF(NUNUCO.EQ.3)  WRITE(OUUNI,350) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                 (OUT15(K),K=NTS8,NTSST)
      IF(NUNUCO.EQ.4)  WRITE(OUUNI,351) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                 (OUT15(K),K=NTS8,NTSST)
      IF(NUNUCO.EQ.5)  WRITE(OUUNI,352) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                 (OUT15(K),K=NTS8,NTSST)
      IF(NUNUCO.EQ.6)  WRITE(OUUNI,353) ERRIND,ID,(COUT(K),K=2,NTS6),
     1                 (OUT15(K),K=NTS8,NTSST)
      GO TO 430
  420 CONTINUE
      IF(NUNUCO.EQ.3)  WRITE(OUUNI,330) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
      IF(NUNUCO.EQ.4)  WRITE(OUUNI,331) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
      IF(NUNUCO.EQ.5)  WRITE(OUUNI,332) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
      IF(NUNUCO.EQ.6)  WRITE(OUUNI,333) ERRIND,CDATE,(COUT(J),J=2,NTS6),
     1                 (OUT15(K),K=NTS7,NTSTOT)
  430 IF (LPRODU .NE. 1) GO TO 440
      CALL PRODUC(XST,GRAMOR,DEATH,CDATE,DAY,T,DEPEUF,0)
  440 CONTINUE
C
C  Print a warning message if potential non-unique solutions have been
C  determined by subroutine SOLVLP.
C
      IF (IDUMP. EQ. 0 .OR. NUMUN .EQ. 0 .OR.BIO(2) .LT. 0.0) GO TO 460
      WRITE (IOU(6),450) (NONUN(I),I=1,NUMUN)
  450 FORMAT ('  The following species have minimum reduced cost =',
     1        ' 0.0 and might replace',/'  one of the species in the ',
     2        'bloom:',1X,20I3)
  460 CONTINUE
C
C  Print Relevant information.
C  Update 11-10-90:
C  Call prinun, when  LPRINT >= 1
C
      IF (LPRINT .GE. 1)
     1    CALL PRINUN (CDATE, TOTAL, PHYT, EXTTOT, EXLIVE, EXDEAD,
     2                 EXTB, T, USOL, DAY, DEP , ZOOD,ZMAX,GRAMX)
C
C  Return the converted and corrected solar radiation level as
C  CSOL in Joules / cm2 / hour.
C
      CSOL = DSOL * 1.0D-4 / DAY

      RETURN
      END
