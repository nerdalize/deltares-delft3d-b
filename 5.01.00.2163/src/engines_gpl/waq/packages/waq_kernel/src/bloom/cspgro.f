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
C  *      SUBROUTINE CSPGRO TO MODIFY SPECIES GROWTH COEFFICIENTS      *
C  *********************************************************************
C
C  0895 MvdV adaptation dimension ZOOPR for more than one grazer type
      SUBROUTINE CSPGRO(LERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'ioblck.inc'
      PARAMETER (NSPE = 9)
      CHARACTER*8 PWORDS(MT),WORD
      INTEGER GETS,UPRCAS,STOS,MATCH
      CHARACTER*8 WSPE(NSPE),FNAME,PROFIL,PWORD,WORD2,PWORD1
      CHARACTER*3 NEW,OLD
      DATA FNAME /'BLOOMED '/
      DATA PROFIL /'PROFBLM '/
      DATA NEW    /'New'/
      DATA Old    /'Old'/
      DATA WSPE  /'?       ','END     ','EDIT    ','MORTAL  ',
     1            'PMAX    ','PRINT   ','RELMIX  ','RESPIRAT',
     2            'ZOOPREF '/
C
C  General comment: in case an error is detected in this subroutine
C  -- for instance a missing or misspelled control word --
C  LERR will be put to 1 and subroutine CHANGE will set LRUN = 0.
C  Hence a batch job will be terminated,
C  but re-entry is possible in an interactive run.
C
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      LERR=0
   10 IF (IOFLAG .EQ. 1) CALL BLSELECT (WSPE, NSPE, 1051)
      I=INPTDT(1051,WORD,LENWRD)
      IF (MATCH(WSPE,NSPE,8,WORD,LENWRD,0,NUMCA) .NE. 1) NUMCA=NSPE+1
      GO TO (   100,  2000,   400,   500,   500,
     1          300,   500,   500,   500,   20), NUMCA
C
C  Error detected in input.
C
   20 WRITE (OUUNI,99990) WORD
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
      WRITE (OUUNI,99965) (WSPE(I),I=4,NSPE)
      CALL VIDEO (0)
      WRITE (OUUNI,99960)
      CALL VIDEO (1)
      WRITE (OUUNI,99955) (GRNAME(I),I=1,NUECOG)
      CALL VIDEO (0)
      WRITE (OUUNI,99950)
      WRITE (OUUNI,99940)
      GO TO 10
C
C  Print present growth characteristics.
C
  300 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99920)
      DO 310 I=1,NUSPEC
      K = 7
      IF (LPMAX(I) .EQ. 1) K = 8
      WRITE(OUUNI,99910) SPNAME(I),PMAX1(I),PMAX2(I),CONTRO(K),
     1                   RMORT1(I),RMORT2(I),RES1(I),RES2(I),SDMIX(I),
     2                   ZOOPR(I,0)
  310 CONTINUE
      IF (IOFLAG .EQ. 0) GO TO 10
      WRITE (IOU(21),99920)
      DO 320 I=1,NUSPEC
      K = 7
      IF (LPMAX(I) .EQ. 1) K = 8
      WRITE(IOU(21),99910) SPNAME(I),PMAX1(I),PMAX2(I),CONTRO(K),
     1                RMORT1(I),RMORT2(I),RES1(I),RES2(I),SDMIX(I),
     2                ZOOPR(I,0)
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
      WRITE (IOU(29),99920)
      DO 420 I=1,NUSPEC
      K = 7
      IF (LPMAX(I) .EQ. 1) K = 8
      WRITE(IOU(29),99910) SPNAME(I),PMAX1(I),PMAX2(I),CONTRO(K),
     1                RMORT1(I),RMORT2(I),RES1(I),RES2(I),SDMIX(I),
     2                ZOOPR(I,0)
  420 CONTINUE
      CALL EDIT (FNAME,PROFIL,IRC)
      IF (IRC .NE. -3) GO TO 425
      WRITE (OUUNI,99790) IRC
      CLOSE (IOU(29), STATUS = 'DELETE')
      GO TO 10
  425 REWIND IOU(29)
      READ (IOU(29),99905)
      DO 430 I=1,NUSPEC
      READ (IOU(29),99910) SPNAME(I),PMAX1(I),PMAX2(I),PWORDS(I),
     1                RMORT1(I),RMORT2(I),RES1(I),RES2(I),SDMIX(I),
     2                ZOOPR(I,0)
  430 CONTINUE
      CLOSE (IOU(29), STATUS = 'DELETE')
C
C  Set control values for Pmax function. Check for errors.
C
      DO 460 I=1,NUSPEC
      IPOS = 1
      K = GETS (PWORDS(I),IPOS,8,8,PWORD1,LEN)
      K = UPRCAS (PWORD1,PWORD,8)
  440 IF (MATCH(CONTRO,8,8,PWORD,LEN,0,NUMPMA) .NE. 1) GO TO 450
      LPMAX(I) = 0
      IF (NUMPMA .EQ. 8) LPMAX(I) = 1
      GO TO 460
C
C  Invalid control word for Pmax function.
C
  450 CONTINUE
      WRITE (OUUNI,99890) PWORD,I
      WRITE (OUUNI,99880) CONTRO(7),CONTRO(8)
      K=INPTDT(1054,PWORD,LEN)
      GO TO 440
  460 CONTINUE
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
      WRITE (OUUNI,99870) WORD2
      WRITE (OUUNI,99860) (GRNAME(J),J=1,NUECOG)
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 500
  510 CONTINUE
      IGRO=IT2(NUMGR,2)-IT2(NUMGR,1)+1
      IF (IGRO .EQ. 1) GO TO 540
      CALL BLPROMPT (1,IGRO)
      IRC = STOS(GRNAME(NUMGR),1,8,STRING,LENSTR)
  520 I=INPTNM(1033,0.0D0,NSGR,2)
      IF (NSGR .NE. 0 .AND. NSGR .LE. IGRO) GO TO 530
      WRITE(OUUNI,99840) NSGR,GRNAME(NUMGR)
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 520
  530 NSGR2=NSGR
      NSGR=NSGR-1
  540 CONTINUE
      NUMSP=IT2(NUMGR,1)+NSGR
      GO TO (   100,  2000,   400,   700,   600,  300,  900,
     1          800,  1000), NUMCA
C
C  Modify Pmax.
C
  600 CONTINUE
      IF (IOFLAG .EQ. 1) THEN
        IF (LPMAX(NUMSP) .EQ. 1) THEN
          WRITE (OUUNI,99830) OLD,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                        WSPE(NUMCA),PMAX1(NUMSP),PMAX2(NUMSP)
       ELSE
          WRITE (OUUNI,99820) OLD,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                        WSPE(NUMCA),PMAX1(NUMSP),PMAX2(NUMSP)
        END IF
      END IF
      I=INPTNM(1052,PMAX1(NUMSP),0,1)
      I=INPTNM(1053,PMAX2(NUMSP),0,1)
  610 IF (IOFLAG .EQ. 1) CALL BLSELECT (CONTRO(7)//CONTRO(8), 2, 1054)
      I=INPTDT(1054,PWORD,LEN)
      IF (MATCH(CONTRO,8,8,PWORD,LEN,0,NUMPMA) .NE. 1) GO TO 630
      IF (NUMPMA .EQ. 7) GO TO 620
      LPMAX(NUMSP) = 1
      WRITE (OUUNI,99830) NEW,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                    WSPE(NUMCA),PMAX1(NUMSP),PMAX2(NUMSP)
      GO TO 10
  620 LPMAX(NUMSP) = 0
      WRITE (OUUNI,99820) NEW,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                    WSPE(NUMCA),PMAX1(NUMSP),PMAX2(NUMSP)
      GO TO 10
C
C  Invalid control word for Pmax function.
C
  630 CONTINUE
      WRITE (OUUNI,99810) PWORD
      IF (IOFLAG .EQ. 1) GO TO 640
      WRITE (OUUNI,99980)
      LERR = 1
      RETURN
  640 CONTINUE
      WRITE (OUUNI,99880) CONTRO(7),CONTRO(8)
      GO TO 610
C
C  Modify mortality.
C
  700 CONTINUE
      IF (IOFLAG .EQ. 1)
     1   WRITE (OUUNI,99820) OLD,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     2                       WSPE(NUMCA),RMORT1(NUMSP),RMORT2(NUMSP)
      I=INPTNM(1057,RMORT1(NUMSP),0,1)
      I=INPTNM(1058,RMORT2(NUMSP),0,1)
      WRITE (OUUNI,99820) NEW,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                    WSPE(NUMCA),RMORT1(NUMSP),RMORT2(NUMSP)
      GO TO 10
C
C  Modify respiration.
C
  800 CONTINUE
      IF (IOFLAG .EQ. 1)
     1   WRITE (OUUNI,99820) OLD,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     2                       WSPE(NUMCA),RES1(NUMSP),RES2(NUMSP)
      I=INPTNM(1059,RES1(NUMSP),0,1)
      I=INPTNM(1060,RES2(NUMSP),0,1)
      WRITE (OUUNI,99820) NEW,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                    WSPE(NUMCA),RES1(NUMSP),RES2(NUMSP)
      GO TO 10
C
C  Modify mixing depth multiplier.
C
  900 CONTINUE
      IF (IOFLAG .EQ. 1)
     1    WRITE (OUUNI,99800) OLD,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     2                        SDMIX(NUMSP)
      I=INPTNM(1055,SDMIX(NUMSP),0,1)
      WRITE (OUUNI,99800) NEW,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                    SDMIX(NUMSP)
      GO TO 10
C
C  Modify zooplankton preference rates.
C
 1000 CONTINUE
      IF (IOFLAG .EQ. 1)
     1  WRITE (OUUNI,99800) OLD,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     2                      ZOOPR(NUMSP,0)
      I=INPTNM(1056,ZOOPR(NUMSP,0),0,1)
      WRITE (OUUNI,99800) NEW,WSPE(NUMCA),NSGR2,GRNAME(NUMGR),
     1                    ZOOPR(NUMSP,0)
      GO TO 10
C
C  Exit
C
 2000 CONTINUE
C
C  Formats for this subroutine.
C
99990 FORMAT(1X,'Invalid Growth command',2X,A8)
99980 FORMAT (//,1X,'Excecution terminates following an error in',
     1       ' subroutine "CSPGRO"',//)
99970 FORMAT (1X,'You are in Growth command mode.',/,' Select on  of',
     1        ' the following specific inputs:')
99965 FORMAT (9(1X,A8))
99960 FORMAT (1X,'Then specify one of the following species names:')
99955 FORMAT (9(1X,A8))
99950 FORMAT (1X,'If several types are in the same species,',/,
     1        ' add the relative type number.',/)
99940 FORMAT (1X,'Or exit to parameter command mode entering "END".',/,
     1        1X,'Use "PRINT" command to show present dataset.',/,
     2        1X,'A global change using the editor can be made',/,
     3        1X,'with the "EDIT" command.',/)
99920 FORMAT (1X,'Present species growth coefficients: ',/,' Species ',
     1        'Pmax 1  Pmax 2  P-func. Mort 1  Mort 2  Resp 1  ',
     2        'Resp 2  Relmix  Zoopref')
99910 FORMAT (1X,A8,F6.3,1X,F7.3,1X,A8,6(F7.3,1X))
99905 FORMAT (/)
99900 FORMAT (1X,'Message from subroutine "CSPGRO":',/,
     1        ' You cannot use option "EDIT" in a batch run,',/,
     2        ' as this option invokes the system editor.')
99890 FORMAT ('  Unrecognised control word ',A8,' for Pmax ',
     1        'function of species number ',I2)
99880 FORMAT ('  Valid Pmax function control words are:',/,1X,4(A8,2X))
99870 FORMAT(1X,'Invalid species name',2X,A8)
99860 FORMAT (' Valid species names are: ',/,1X,10(1X,A9))
99840 FORMAT(1X,'Incorrect type number ',I1,' for species',2X,A8)
99830 FORMAT (1X,A3,' value of',1X,A8,1X,'for type ',I2,
     1        ' of species ',A8,':',//,5X,A8,' = ',F8.4,' *  (T - ',
     2        F8.4,')',/)
99820 FORMAT (1X,A3,' value of',1X,A8,1X,'for type ',I2,
     1        ' of species ',A8,':',//,5X,A8,' = ',F8.4,' * ',
     2        F8.4,' ** T',/)
99810 FORMAT ('  Unrecognised control word ',A8,' for Pmax ',
     1        'function.')
99800 FORMAT (1X,A3,1X,A8,1X,'for type ',I2,' of species',
     1        1X,A8,1X,'=',1X,F14.4)
99790 FORMAT (' Return code from edit: ',I3,'. Command not executed')
      RETURN
      END
