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
C  *      SUBROUTINE CSPSTO TO MODIFY SPECIES STOCHIOMETRY             *
C  *********************************************************************
C
      SUBROUTINE CSPSTO (LERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'ioblck.inc'
      PARAMETER (NSPE = 8)
      INTEGER STOS, MATCH
      CHARACTER*8 WSPE(NSPE),FNAME,PROFIL,WORD1,WORD2,WORD3
      CHARACTER*3 NEW, OLD
      DATA FNAME /'BLOOMED '/
      DATA PROFIL /'PROFBLM '/
      DATA NEW    /'New'/
      DATA OLD    /'Old'/
      DATA WSPE  /'?       ','END     ','EDIT    ','CARTODRY',
     1            'CHLTOCAR','NUTCOEFF','PRINT   ','SPECEXT '/
C
C  General comment: in case an error is detected in this subroutine
C  -- for instance a missing or misspelled control word --
C  LERR will be put to 1 and subroutine CHANGE will set LRUN = 0.
C  Hence a batch job will be terminated,
C  but re-entry is possible in an interactive run.
C
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      LERR=0
   10 IF (IOFLAG .EQ. 1) CALL BLSELECT (WSPE, NSPE, 1037)
      I=INPTDT(1037,WORD1,LENWRD)
      IF (MATCH(WSPE,NSPE,8,WORD1,LENWRD,0,NUMCA) .NE. 1) NUMCA=NSPE+1
      GO TO (   100,  2000,   400,    500,   500,    500,   300,
     1          500,    20), NUMCA
C
C  Error in input.
C
   20 WRITE (OUUNI,99990) WORD1
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 10
   30 WRITE (OUUNI,99980)
      LERR=1
      RETURN
C
C  Print characteristics which may be modified.
C
  100 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99970)
      CALL VIDEO (1)
      WRITE (OUUNI,99965) (WSPE(I),I=3,NSPE)
      CALL VIDEO (0)
      WRITE (OUUNI,99960)
      CALL VIDEO (1)
      WRITE (OUUNI,99955) (GRNAME(I),I=1,NUECOG)
      CALL VIDEO (0)
      WRITE (OUUNI,99950)
      WRITE (OUUNI,99940)
      GO TO 10
C
C  Print present set of stochiometric coefficients.
C
  300 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99920) (CSTRA(I),I=1,NUNUCO),(WSPE(I),I=5,4,-1)
      DO 310 I=1,NUSPEC
      WRITE(OUUNI,99910) SPNAME(I),EKX(I),(AA(K,I),K=1,NUNUCO),
     1                   CHLTOC(I),CTODRY(I)
  310 CONTINUE
      IF (IOFLAG .EQ. 0) GO TO 10
      WRITE (IOU(21),99920) (CSTRA(I),I=1,NUNUCO),(WSPE(I),I=5,4,-1)
      DO 320 I=1,NUSPEC
      WRITE(IOU(21),99910) SPNAME(I),EKX(I),(AA(K,I),K=1,NUNUCO),
     1                CHLTOC(I),CTODRY(I)
  320 CONTINUE
      GO TO 10
C
C  Make a global change of many characteristics using the system editor.
C
  400 CONTINUE
      IF (IOFLAG .EQ. 1) GO TO 410
      WRITE (OUUNI,99900)
      GO TO 10
  410 CONTINUE
      OPEN (IOU(29), FILE = FNAME)
      WRITE (IOU(29),99920) (CSTRA(I),I=1,NUNUCO),(WSPE(I),I=5,4,-1)
      DO 420 I=1,NUSPEC
      WRITE(IOU(29),99910) SPNAME(I),EKX(I),(AA(K,I),K=1,NUNUCO),
     1                     CHLTOC(I),CTODRY(I)
  420 CONTINUE
      CALL EDIT (FNAME,PROFIL,IRC)
      IF (IRC .NE. -3) GO TO 425
      WRITE (OUUNI,99790) IRC
      CLOSE (IOU(29), STATUS = 'DELETE')
      GO TO 10
  425 REWIND IOU(29)
      READ (IOU(29),99890)
      DO 430 I=1,NUSPEC
      READ (IOU(29),99910) SPNAME(I),EKX(I),(AA(K,I),K=1,NUNUCO),
     1                     CHLTOC(I),CTODRY(I)
      CHLR(I) = CHLTOC(I) * CTODRY(I)
  430 CONTINUE
      CLOSE (IOU(29), STATUS = 'DELETE')
      GO TO 10
C
C  Read species name and find species and type number.
C
  500 NSGR=0
      NSGR2 = 1
      IF (IOFLAG .EQ. 1) CALL BLSELECT (GRNAME, NUECOG, 1032)
      I=INPTDT(1032,WORD2,LENWRD)
      IF (MATCH(GRNAME,NUECOG,8,WORD2,LENWRD,0,NUMGR) .EQ. 1) GO TO 510
C
C  Incorrect species name.
C
      WRITE (OUUNI,99880) WORD2
      WRITE (OUUNI,99870) (GRNAME(J),J=1,NUECOG)
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 500
  510 CONTINUE
      IGRO=IT2(NUMGR,2)-IT2(NUMGR,1)+1
      IF (IGRO .EQ. 1) GO TO 540
      IF (IOFLAG .EQ. 0) GO TO 520
      CALL BLPROMPT (1,IGRO)
      IRC = STOS(GRNAME(NUMGR),1,8,STRING,LENSTR)
  520 I=INPTNM(1033,0.0D0,NSGR,2)
      IF (NSGR .NE. 0 .AND. NSGR .LE. IGRO) GO TO 530
      WRITE(OUUNI,99850) NSGR,GRNAME(NUMGR)
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 520
  530 NSGR2=NSGR
      NSGR=NSGR-1
  540 CONTINUE
      NUMSP=IT2(NUMGR,1)+NSGR
      GO TO (100,2000,  400, 900,  800, 700, 300, 600), NUMCA
C
C  Modify specific extinction coefficient.
C
  600 CONTINUE
      IF (IOFLAG .EQ. 1)
     1    WRITE (OUUNI,99840) OLD,WSPE(NUMCA),NSGR2,SPNAME(NUMSP),
     2                        EKX(NUMSP)
      I=INPTNM(1034,EKX(NUMSP),0,1)
      WRITE (OUUNI,99840) NEW,WSPE(NUMCA),NSGR2,SPNAME(NUMSP),
     1                    EKX(NUMSP)
      GO TO 10
C
C  Modify nutrient coefficients.
C
  700 CONTINUE
      IF (IOFLAG .EQ. 1) CALL BLSELECT (CSTRA, NUNUCO, 1035)
      I = INPTDT(1035,WORD3,LENWRD)
      IF (MATCH(CSTRA,NUNUCO,8,WORD3,LENWRD,0,NUMCO).EQ.1) GOTO 710
      WRITE (OUUNI,99830) WORD3
      WRITE (OUUNI,99820) (CSTRA(I),I=1,NUNUCO)
      POSIT=0
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 700
  710 CONTINUE
      IF (IOFLAG .EQ. 1)
     1    WRITE (OUUNI,99810) OLD,CSTRA(NUMCO),NSGR2,SPNAME(NUMSP),
     2                        AA(NUMCO,NUMSP)
      I=INPTNM(1036,AA(NUMCO,NUMSP),0,1)
      WRITE (OUUNI,99810) NEW,CSTRA(NUMCO),NSGR2,SPNAME(NUMSP),
     1                    AA(NUMCO,NUMSP)
      GO TO 10
C
C  Modify chlorophyll to carbon ratio.
C
  800 CONTINUE
      IF (IOFLAG .EQ. 1)
     1   WRITE (OUUNI,99810) OLD,WSPE(NUMCA),NSGR2,SPNAME(NUMSP),
     2                       CHLTOC(NUMSP)
      I=INPTNM(1038,CHLTOC(NUMSP),0,1)
      WRITE (OUUNI,99810) NEW,WSPE(NUMCA),NSGR2,SPNAME(NUMSP),
     1                    CHLTOC(NUMSP)
C
C  Calculate CHLR--the conversion from chlorophyll to dry weight.
C
  810 CHLR(NUMSP)=CHLTOC(NUMSP)*CTODRY(NUMSP)
      WRITE (OUUNI,99800) NSGR2,SPNAME(NUMSP),CHLR(NUMSP)
      GO TO 10
C
C  Modify carbon to dry weight ratio.
C
  900 CONTINUE
      IF (IOFLAG .EQ. 1)
     1 WRITE (OUUNI,99810) OLD,WSPE(NUMCA),NSGR2,SPNAME(NUMSP),
     2                     CTODRY(NUMSP)
      I=INPTNM(1039,CTODRY(NUMSP),0,1)
      WRITE (OUUNI,99810) NEW,WSPE(NUMCA),NSGR2,SPNAME(NUMSP),
     1                    CTODRY(NUMSP)
      GO TO 810
C
C  Exit
C
 2000 CONTINUE
C
C  Formats for this subroutine.
C
99990 FORMAT (1X,'Invalid Stochiometry command',2X,A8)
99980 FORMAT (//,1X,'Excecution terminates after an error in',
     1       ' Subroutine "CSPSTO"',//)
99970 FORMAT (1X,'You are in Stochiometry command mode.',/,' Select',
     1        ' one of the following specific inputs:')
99965 FORMAT (9(1X,A8))
99960 FORMAT (1X,'Then specify one of the following species names:')
99955 FORMAT (9(1X,A8))
99950 FORMAT (1X,'If several types are in the same species,',/,
     1        ' add the relative type number.',/)
99940 FORMAT (1X,'Or exit to parameter command mode entering "END".',/,
     1        1X,'Use "PRINT" command to show present dataset.',/,
     2        1X,'A global change using the editor can be made',/,
     3        1X,'with the "EDIT" command.',/)
99920 FORMAT (1X,'Present species stochiometry: ',/,
     1        1X,'Spec.name',3X,'Spec ext',3X,8(A8,2X))
99910 FORMAT (1X,A8,2X,D10.3,3F10.5,F10.4,4F10.5)
99900 FORMAT (1X,'Message from subroutine "CSPSTO":',/,
     1        ' You cannot use option "EDIT" in a batch run,',/,
     2        ' as this option invokes the system editor.')
99890 FORMAT (/)
99880 FORMAT (1X,'Invalid species name',2X,A8)
99870 FORMAT (' Valid species names are: ',/,1X,10(1X,A9))
99850 FORMAT(1X,'Incorrect type number ',I1,' for species',1X,A8)
99840 FORMAT (1X,A3,1X,A8,1X,'for type ',I2,' in species',
     1        1X,A8,1X,'=',1X,D10.3)
99830 FORMAT (1X,'Incorrect nutrient name ',A8)
99820 FORMAT (1X,'Valid nutrients are: ',6A9)
99810 FORMAT (1X,A3,1X,A8,1X,'coefficient for type ',I2,
     1        ' in species',1X,A8,1X,'=',F10.5)
99800 FORMAT (1X,'New dry weight to chlorophyll ratio for ',
     1        'type ',I2,' of species',1X,A8,1X,'=',F10.5)
99790 FORMAT (' Return code from edit: ',I3,'. Command not executed')
      END
