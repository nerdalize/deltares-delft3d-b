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
C  *      SUBROUTINE CHANGE TO MODIFY NOMINAL INPUTS TO THE MODEL      *
C  *********************************************************************
C
C  0895 MvdV goto subroutine CCONS instead of CZOOPL if NUGRAZ > 0
      SUBROUTINE CHANGE(LCHA)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'ioblck.inc'
      PARAMETER (NCHANG = 25)
      REAL*4 RESULT
      CHARACTER*8 WCHANG(NCHANG),FNAME,PROFIL
      CHARACTER*8 WORD,WNUNAM,WMORT
      CHARACTER*3 OLD,NEW
      character*8 indepen(3)
      INTEGER CMS,CALC,STOS,MATCH
      DATA FNAME /'BLOOMED '/
      DATA PROFIL /'PROFBLM '/
      DATA NEW /'New'/
      DATA OLD /'Old'/
      DATA WCHANG /'?       ','END     ','PARAM   ','OPTION  ',
     1             'AUTOLYSE','BACKEXT ','CALCULAT','CHL-MINE',
     2             'CMS-DOS ','DAYLNGTH','DEPTH   ','FLUSH   ',
     3             'GROWTH  ','MORTAL  ','NUTRCONC','NUTRMINE',
     4             'ORG-MINE','PERIODS ','PRINT   ','SEDIMENT',
     5             'SOLARRAD','STOCHIOM','TEMPTURE','TITLE   ',
     6             'ZOOPLANK'/
      data indepen /'DEPENDEN','INDEPEND','        '/
C
C
C  General comment: in case an error is detected in this subroutine
C  -- for instance a missing or misspelled control word --
C  a batch job will be terminated putting LRUN = 0,
C  but re-entry is possible during an interactive run
C
      IF (LCHA .EQ. 1) GO TO 50
      CALL FORMFE (OUUNI)
   10 POSIT=0
      IF (IOFLAG .EQ. 1) CALL BLSELECT (WCHANG, 4, 1001)
      I=INPTDT(1001,WORD,LENWRD)
      IF (MATCH(WCHANG,4,8,WORD,LENWRD,0,NUMWRD) .NE. 1) NUMWRD=5
      GO TO (    40,  2400,    100,  2400,    20), NUMWRD
   20 CONTINUE
      WRITE(OUUNI,99860) WORD
      IF (IOFLAG .EQ. 0) GO TO 30
      WRITE(OUUNI,99850)
      GO TO 10
   30 CONTINUE
      WRITE(OUUNI,99840)
      LRUN=0
      RETURN
   40 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE(OUUNI,99830)
      WRITE(OUUNI,99850)
      GO TO 10
C
C     *******************************
C     * START CHANGE PARAMETER MODE *
C     *******************************
C
   50 IF (IOFLAG .EQ. 1) CALL CLRSCR
  100 IF (IOFLAG .EQ. 1) CALL BLSELECT (WCHANG, NCHANG, 1002)
      I=INPTDT(1002,WORD,LENWRD)
      IF (MATCH(WCHANG,NCHANG,8,WORD,LENWRD,0,NUMWRD) .NE. 1)
     1   NUMWRD=NCHANG+1
      GO TO (   130,  2400,   100,  2400,  1600,   400,
     1         2100,  1500,   800,  2200,   500,  1300,
     2         1850,  1000,   600,   700,  1400,   900,
     3         2300,  1200,   300,  1800,   200,  1900,
     4         2350,  110), NUMWRD
C
C  Error detected in input of CHANGE parameter command
C  Terminate in batch mode.
C
  110 CONTINUE
      IF (IOFLAG .EQ. 1) GO TO 120
      WRITE(OUUNI,99820) WORD
      WRITE(OUUNI,99840)
      LRUN=0
      RETURN
C
C  In an interactive run, try if a valid CMS command was entered.
C
  120 IRC = CMS (LINE,80)
      POSIT = 0
      IF (IRC .EQ. 0) GO TO 100
      IF (IRC .LT. 0) GO TO 125
      WRITE (OUUNI,810) IRC
      GO TO 100
  125 WRITE(OUUNI,99820) WORD
      GO TO 100
C
C ? was entered.
C
  130 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE(OUUNI,99810)
      CALL VIDEO (1)
      WRITE(OUUNI,99805) (WCHANG(I),I=5,NCHANG)
      CALL VIDEO (0)
      WRITE(OUUNI,99800)
      GO TO 100
C
C  Modify temperature
C
  200 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99990) OLD,TEMPMU,TEMPAD
      I=INPTNM(1003,TEMPMU,0,1)
      I=INPTNM(1004,TEMPAD,0,1)
      IF (TEMPMU .GT. 2.0) WRITE (OUUNI,99650) TEMPMU,WCHANG(NUMWRD)
      IF (TEMPAD .GT. 20.0) WRITE (OUUNI,99640) TEMPAD,WCHANG(NUMWRD)
      WRITE(OUUNI,99990) NEW,TEMPMU,TEMPAD
      GO TO 100
C
C  Modify solar intensity
C
  300 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99980) OLD,SOLAMU,SOLAAD
      I=INPTNM(1003,SOLAMU,0,1)
      I=INPTNM(1004,SOLAAD,0,1)
      IF (SOLAMU .GT. 2.0) WRITE (OUUNI,99650) SOLAMU,WCHANG(NUMWRD)
      IF (SOLAAD .GT. 5000.0) WRITE (OUUNI,99640) SOLAAD,WCHANG(NUMWRD)
      WRITE(OUUNI,99980) NEW,SOLAMU,SOLAAD
      GO TO 100
C
C  Modify background extinction coefficient
C
  400 CONTINUE
      IF ( IOFLAG .EQ. 1) WRITE(OUUNI,99970) OLD,BACKMU,BACKAD
      I=INPTNM(1003,BACKMU,0,1)
      I=INPTNM(1004,BACKAD,0,1)
      IF (BACKMU .GT. 2.0) WRITE (OUUNI,99650) BACKMU,WCHANG(NUMWRD)
      IF (BACKAD .GT. 5.0) WRITE (OUUNI,99640) BACKAD,WCHANG(NUMWRD)
      WRITE(OUUNI,99970) NEW,BACKMU,BACKAD
      GO TO 100
C
C  Modify depth
C
  500 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99960) OLD,DEPTMU,DEPTAD
      I=INPTNM(1003,DEPTMU,0,1)
      I=INPTNM(1004,DEPTAD,0,1)
      IF (DEPTMU .GT. 5.0) WRITE (OUUNI,99650) DEPTMU,WCHANG(NUMWRD)
      IF (DEPTAD .GT. 5.0) WRITE (OUUNI,99640) DEPTAD,WCHANG(NUMWRD)
      WRITE(OUUNI,99960) NEW, DEPTMU,DEPTAD
      GO TO 100
C
C  Modify nutrient concentrations
C
  600 CONTINUE
      IF (IOFLAG .EQ. 1) CALL BLSELECT (CSTRA, NUNUCO, 1005)
      I = INPTDT(1005,WNUNAM,LENWRD)
      IF (MATCH(CSTRA,NUNUCO,8,WNUNAM,LENWRD,0,NUMNUT).NE.1) GOTO 610
      IF (IOFLAG .EQ. 1)
     1    WRITE(OUUNI,99940) OLD,CSTRA(NUMNUT),BNUT(NUMNUT),DNUT(NUMNUT)
      I=INPTNM(1003,BNUTI,0,1)
      I=INPTNM(1004,DNUTI,0,1)
      DNUTMA = 5.0
      IF (CSTRA(NUMNUT) .EQ. 'PHOSPOR') DNUTMA = 0.5
      IF (BNUTI .GT. 3.0) WRITE (OUUNI,99650) BNUTI,WCHANG(NUMWRD)
      IF (DNUTI .GT. DNUTMA) WRITE (OUUNI,99640) DNUTI,WCHANG(NUMWRD)
      BNUT(NUMNUT)=BNUTI
      DNUT(NUMNUT)=DNUTI
      WRITE(OUUNI,99940) NEW,CSTRA(NUMNUT),BNUTI,DNUTI
      GO TO 100
C
C  Error in nutrient name
C
  610 CONTINUE
      WRITE(OUUNI,99930) WNUNAM
      IF (IOFLAG .EQ. 0) GO TO 630
      WRITE(OUUNI,620) (CSTRA(I),I=1,NUNUCO)
  620 FORMAT(' Valid nutrients are: ', 7A9)
      POSIT=0
      GO TO 600
  630 WRITE(OUUNI,99840)
      LRUN=0
      RETURN
C
C  Modify nutrient mineralization rate constants
C
  700 CONTINUE
      IF (IOFLAG .EQ. 1) CALL BLSELECT (CSTRA, NUNUCO, 1005)
      I = INPTDT(1005,WNUNAM,LENWRD)
      IF (MATCH(CSTRA,NUNUCO,8,WNUNAM,LENWRD,0,NUMNUT).NE.1) GOTO 720
      IF (IOFLAG .EQ. 1) THEN
         IF (RNUT(2,NUMNUT) .GT. 0.5) THEN
             WRITE(OUUNI,99910) OLD,CSTRA(NUMNUT),RNUT(1,NUMNUT)
         ELSE
             WRITE(OUUNI,99920) OLD,CSTRA(NUMNUT),RNUT(1,NUMNUT)
         END IF
      END IF
  702 CONTINUE
      IF (IOFLAG .EQ. 1) CALL BLSELECT ('DEPENDENINDEPEND', 2, 1013)
      I = INPTDT(1013,WORD,LENWRD)
!     IF (MATCH('DEPENDEN',1,8,WORD,LENWRD,0,NUM).EQ.1) THEN
      IF (MATCH(indepen(1:),1,8,WORD,LENWRD,0,NUM).EQ.1) THEN
         RNUT(2,NUMNUT) =  1.0
         GO TO 705
      END IF
!     IF (MATCH('INDEPEND',1,8,WORD,LENWRD,0,NUM).EQ.1) THEN
      IF (MATCH(indepen(2:),1,8,WORD,LENWRD,0,NUM).EQ.1) THEN
         RNUT(2,NUMNUT) =  0.0
         GO TO 710
      END IF
      WRITE (OUUNI,99600) WORD
      IF (IOFLAG .EQ. 0) GO TO 730
      POSIT = 0
      GO TO 702
  705 CONTINUE
      I=INPTNM(1010,RATE,0,1)
      IF (RATE .GT. 0.05) WRITE (OUUNI,99650) RATE,WCHANG(NUMWRD)
      WRITE(OUUNI,99910) NEW,CSTRA(NUMNUT),RATE
      RNUT(1,NUMNUT)=RATE
      GO TO 100
  710 CONTINUE
      I=INPTNM(1011,RATE,0,1)
      IF (RATE .GT. 1.0) WRITE (OUUNI,99630) RATE,WCHANG(NUMWRD)
      WRITE(OUUNI,99920) NEW,CSTRA(NUMNUT),RATE
      RNUT(1,NUMNUT)=RATE
      GO TO 100
C
C  Error in nutrient name
C
  720 CONTINUE
      WRITE(OUUNI,99930) WNUNAM
      IF (IOFLAG .EQ. 0) GO TO 730
      WRITE(OUUNI,620) (CSTRA(I),I=1,NUNUCO)
      POSIT=0
      GO TO 700
  730 WRITE(OUUNI,99840)
      LRUN=0
      RETURN
C
C  Execute CMS or CP command
C
  800 CONTINUE
      IF (IOFLAG .EQ. 1) GO TO 805
      WRITE (OUUNI,99620)
      GOTO 100
  805 CONTINUE
      CALL VIDEO (7)
      IRC=CMS(LINE,80)
      CALL VIDEO (0)
      POSIT = 0
      IF(IRC .NE. 0) WRITE (OUUNI,810) IRC
  810 FORMAT (' Return code from CMS: ',I5,'.')
      GO TO 100
C
C  Modify period for which blooms are to be calculated
C  for NRUN successive runs
C
  900 CONTINUE
      DO 910 J=1,NRUN
      DO 910 K=1,3
  910 NPER(J,K)=0
  920 I = INPTNM(1006,0.0D0,NRUN,2)
      IF (NRUN .GE. 1 .AND. NRUN .LE. 10) GO TO 930
      WRITE(OUUNI,99700) NRUN
      IF (IOFLAG .EQ. 0) GO TO 30
      POSIT=0
      GO TO 920
  930 J=0
  940 J=J+1
      CALL BLPROMPT(1,J)
  950 I = INPTNM(1007,0.0D0,NPER(J,1),2)
      I = INPTNM(1008,0.0D0,NPER(J,2),2)
      IF (NPER(J,1) .LE. NPER(J,2)) GO TO 960
      WRITE(OUUNI,99670) NPER(J,1),NPER(J,2)
      IF (IOFLAG .EQ. 0) GO TO 30
      POSIT=0
      GO TO 950
  960 I = INPTNM(1009,0.0D0,NPER(J,3),2)
      IFIN=NPER(J,1)+NPER(J,3)-1
      IF (NPER(J,2) .GE. IFIN) GO TO 970
      WRITE(OUUNI,99660) NPER(J,1),NPER(J,3),NPER(J,2)
      IF (IOFLAG .EQ. 0) GO TO 30
      POSIT=0
      GO TO 950
  970 IF (J .LT. NRUN) GO TO 940
      WRITE(OUUNI,99690) NRUN
      DO 980 J=1,NRUN
  980 WRITE(OUUNI,99790) NPER(J,1),NPER(J,2),NPER(J,3)
      GO TO 100
C
C  Invalid control word to modify mortality rate constants
C
  990 CONTINUE
      WRITE (OUUNI,99905) WMORT
      IF (IOFLAG .EQ. 1) GO TO 995
      WRITE (OUUNI,99840)
      LRUN = 0
      RETURN
  995 WRITE (OUUNI,99904) CONTRO(2),CONTRO(7)
C
C  Modify mortality rate constants
C
 1000 CONTINUE
      IF (IOFLAG .EQ. 1) CALL BLSELECT (CONTRO(2)//CONTRO(7), 2, 1012)
      I = INPTDT(1012,WMORT,LENWRD)
      IF (MATCH(CONTRO,7,8,WMORT,LENWRD,0,NUMMOR).NE.1) GOTO 990
      GO TO ( 990, 1020,  990, 990,  990, 990, 1040), NUMMOR
 1020 WRITE(OUUNI,99890) FLUSH
      LCAL=1
      DO 1030 K = 1,52
      IF (DEATH(K) .GT. 1.0D-6) GO TO 100
 1030 CONTINUE
      WRITE (OUUNI,99880)
      GO TO 100
 1040 CONTINUE
      WRITE(OUUNI,99870)
      LCAL=4
      GO TO 100
C
C  Modify sedimentation rate constant
C
 1200 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99770) OLD,SEDRAT
      I = INPTNM(1017,SEDRAT,0,1)
      IF (SEDRAT .GT. 0.5) WRITE (OUUNI,99630) SEDRAT,WCHANG(NUMWRD)
      IF (IOFLAG .EQ. 0) GO TO 100
      WRITE(OUUNI,99770) NEW,SEDRAT
      GO TO 100
C
C  Modify flushing rate constant
C
 1300 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99760) OLD,FLUSH
      I = INPTNM(1018,FLUSH,0,1)
      IF (FLUSH .GT. 0.5) WRITE (OUUNI,99630) FLUSH,WCHANG(NUMWRD)
      IF (IOFLAG .EQ. 0) GO TO 100
      WRITE(OUUNI,99760) NEW,FLUSH
      GO TO 100
C
C  Modify mineralization rate detritus
C
 1400 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99750) OLD,REMIOR
      I = INPTNM(1019,REMIOR,0,1)
      IF (REMIOR .GT. 0.05) WRITE (OUUNI,99650) REMIOR,WCHANG(NUMWRD)
      WRITE(OUUNI,99750) NEW,REMIOR
      GO TO 100
C
C  Modify disappearance rate light absorption by dead algae
C
 1500 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99740) OLD,REMILI(1),REMILI(2)
      I = INPTNM(1020,REMILI(1),0,1)
      I = INPTNM(1021,REMILI(2),0,1)
      WRITE (OUUNI,99740) NEW,REMILI(1),REMILI(2)
      GO TO 100
C
C  Modify autolysing fraction
C
 1600 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99730) OLD,1.0-AVAILN
      I = INPTNM(1022,AUTO,0,1)
      AVAILN=1.0-AUTO
      IF (AUTO .GT. 1.0 .OR. AUTO .LT. 0.0)
     1   WRITE (OUUNI,99630) AUTO,WCHANG(NUMWRD)
      WRITE(OUUNI,99730) NEW,AUTO
      GO TO 100
C
C  Modify species stochiometric coefficients
C
 1800 CALL CSPSTO(LERR)
      IF (LERR .EQ. 1) GO TO 1810
      GO TO 50
C
C  Fatal error in subroutines CSPEC, CSPSTO, CSPGRO, OR CZOOPL.
C
 1810 LRUN=0
      RETURN
C
C  Modify species growth coefficients.
C
 1850 CALL CSPGRO(LERR)
      IF (LERR .EQ. 1) GO TO 1810
      GO TO 50
C
C  Modify title lines for outputs; in interactive mode, use system
C  editor, otherwise simply type new title.
C
 1900 CONTINUE
      IF (IOFLAG .EQ. 0) GO TO 1910
      OPEN (IOU(29), FILE = FNAME)
      WRITE (IOU(29),99710) (CASE(I),I=1,9)
      WRITE (IOU(29),99710) COM
      CALL EDIT (FNAME,PROFIL,IRC)
      IF (IRC .EQ. -3) THEN
         CLOSE (IOU(29), STATUS = 'DELETE')
         GO TO 1910
      END IF
      REWIND IOU(29)
      READ (IOU(29),99710) (CASE(I),I=1,9)
      READ (IOU(29),99710) COM
      CLOSE (IOU(29), STATUS = 'DELETE')
      GO TO 100
C
C  Get new title if system editor cannot be used.
C
 1910 I = INPTNM(1023,0.0D0,ILINE,2)
      IF (ILINE .LT. 1 .OR. ILINE .GT. 2) GO TO 100
      IF (ILINE .EQ. 1) THEN
         I1=1
         I2=9
      ELSE
         I1=10
         I2=18
      END IF
      READ (INUNI,99710) (COM(I),I=I1,I2)
      WRITE(OUUNI,99705) COM
      GO TO 1910
C
C  Use calculator routine
C
 2100 IRC = STOS(LINE,POSIT,80,LINE,LEN)
      IRC=CALC(LINE,1,LEN,3,RESULT)
      POSIT = 0
      IF(IRC .EQ. 0) THEN
         WRITE (OUUNI,99680) RESULT
      ELSE
         WRITE (OUUNI,2150) IRC
 2150    FORMAT (' Return code from CALC = ',I4)
      END IF
      GO TO 100
C
C     Modify the daylength
C
 2200 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE(OUUNI,99610) OLD,DLGTMU,DLGTAD
      I=INPTNM(1003,DLGTMU,0,1)
      I=INPTNM(1004,DLGTAD,0,1)
      IF (DLGTMU .GT. 1.5) WRITE (OUUNI,99650) DLGTMU,WCHANG(NUMWRD)
      IF (DLGTAD .GT. 5.0) WRITE (OUUNI,99640) DLGTAD,WCHANG(NUMWRD)
      WRITE(OUUNI,99610) NEW,DLGTMU,DLGTAD
      GO TO 100
C
C  Call subroutine CHHELP to print current parameter settings.
C
 2300 CALL CHHELP (OUUNI)
      IF (IOFLAG .EQ. 1) CALL CHHELP (IOU(21))
      GO TO 100
C
C  Modify zooplankton characteristics
C  0895 MvdV goto subroutine CCONS if NUGRAZ > 0
C
 2350 IF (NUGRAZ.EQ.0) THEN
        CALL CZOOPL(LERR)
      ELSE
        CALL CCONS (LERR)
      ENDIF
      IF (LERR .EQ. 1) GO TO 1810
      GO TO 50
C
C  Call subroutine OPTION to (re)set options for this run
C
 2400 CONTINUE
      CALL OPTION(1,LPARAM)
      IF (LPARAM .EQ. 1) GO TO 50
      RETURN
C
C  Formats for this subroutine
C
99990 FORMAT (1X,A3,' temperature calculated as',1X,F6.2,1X,
     1        ' * nominal Temp + ',1X,F6.2)
99980 FORMAT (1X,A3,' solar radiation calculated as',1X,F6.2,1X,
     1        ' * nominal rad + ',1X,F10.2)
99970 FORMAT (1X,A3,' backgr. ext. calculated as',1X,F6.2,1X,
     1        ' * nominal Kb + ',1X,F6.2)
99960 FORMAT (1X,A3,' depth calculated as',1X,F6.2,1X,
     1        ' * nominal depth + ',1X,F6.2)
99940 FORMAT (1X,A3,' concentration of',1X,A8,1X,' is',
     1        1X,F6.2,1X,' * nominal conc. + ',1X,F6.2)
99930 FORMAT (1X,A8,' Is an invalid name for a nutrient')
99920 FORMAT (1X,A3,' mineralization of',1X,A8,1X,' = ',1X,F6.4)
99910 FORMAT (1X,A3,' mineralization of',1X,A8,1X,' = ',1X,F6.4,1X,
     1       ' * Temperature')
99905 FORMAT ('  Unrecognised control word ',A8,' for mortality',
     1        ' computation.')
99904 FORMAT ('  Valid mortality control words are:',/,1X,4(A8,2X))
99890 FORMAT (1X,A3,' mortality is input + ',F6.4)
99880 FORMAT (' *** WARNING MESSAGE ***',//,
     1        ' All input mortality rates are 0.0.')
99870 FORMAT (1X,'Mortality rate calculated as exponential function ',
     1        'of temperature.',/,' To modify the specific values, ',
     2        'enter "GROWTH" command.')
99860 FORMAT (1X,'You are in parameter command mode and have',
     1        ' specified:',2X,A8)
99850 FORMAT (1X,'To proceed you should first enter',
     1        ' "PARAM" or "OPTION" ')
99840 FORMAT (//,1X,'Excecution terminates following a fatal error',
     1        ' in subroutine "CHANGE"',//)
99830 FORMAT (1X,'You are in parameter command mode')
99820 FORMAT(1X,'Invalid change parameter command',2X,A8)
99810 FORMAT (1X,'You are in parameter command mode; select one of',
     1        ' the following parameters:')
99805 FORMAT (8(2X,A8))
99800 FORMAT (1X,'Or exit to option command mode specifying "OPTION"',
     1        ' command.',/,1X,'For most parameters the modification',
     2        ' formula is of the type:',//,5X,'Multiplier  *  ',
     3        'Present value  +  Increment  =  New value',//,
     4        1X,'To show all current parameter settings, you may',
     5        ' enter "PRINT".',/)
99790 FORMAT (1X,'First period: ',I5,' Last period: ',I5,
     1        ' Increment: ',I5)
99770 FORMAT (1X,A3,' sedimentation rate =',F6.3)
99760 FORMAT (1X,A3,' flushing rate =',F6.3)
99750 FORMAT (1X,A3,' mineralization rate of detritus =',1X,F6.3,1X,
     1        '*  Temperature')
99740 FORMAT (1X,A3,' mineralization rate of chlorophyll is:',/,1X,
     1        'EXP (',1X,F6.4,' * Temperature- ',1X,F6.4,' )')
99730 FORMAT (1X,A3,' fraction which is autolysing =',F6.3)
99710 FORMAT (9A8)
99705 FORMAT (1X,9A8)
99700 FORMAT (1X,'NRUN =',1X,I2,1X,'Probable input error')
99690 FORMAT (1X,'Blooms will be calculated for the following',
     1        1X,I2,1X,'periods:')
99680 FORMAT (1X,'Result = ',F20.4)
99670 FORMAT (' First period ',I2,' exceeds final period ',I2)
99660 FORMAT (' First period ',I2,' plus increment ',I2,' exceed',
     1        ' final period ',I2)
99650 FORMAT (' *** WARNING MESSAGE ***',//,' Unlikely multiplier ',
     1        F10.5,' for computation of ',A8,//)
99640 FORMAT (' *** WARNING MESSAGE ***',//,' Unlikely increment ',
     1        F10.5,' for computation of ',A8,//)
99630 FORMAT (' *** WARNING MESSAGE ***',//,' Unlikely value ',F10.5,
     1        ' for computation of ',A8,//)
99620 FORMAT (1X,'Command CMS not allowed in a batch job')
99610 FORMAT (1X,A3,' daylength calculated as',1X,F6.2,1X,
     1        '* nominal daylength +',1X,F6.2)
99600 FORMAT (1X,'Invalid respons ',A8)
      END
