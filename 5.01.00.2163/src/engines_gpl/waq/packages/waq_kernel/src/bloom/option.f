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
C  *        SUBROUTINE OPTION TO (RE)SET CONTROL OPTIONS               *
C  *                     FOR THE PROGRAM                               *
C  *********************************************************************
C
      SUBROUTINE OPTION(LOMODE,LPARAM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'dynam.inc'
      INCLUDE 'arran.inc'
C
      PARAMETER (NOPT = 32)
      INTEGER CMS,STOS
      INTEGER OFON(NOPT)
      CHARACTER*56 TITLE
      CHARACTER*60 CPLVAR
      CHARACTER*36 CPLNUT(3),CPLCHL
      CHARACTER*11 CHLTIT,PFON,PFOFF
      CHARACTER*8 SUBTIT(6),PRODUC,DIURNA,WBASE(2),WOPTIO(NOPT)
      CHARACTER*8 WORD,WO1,TNAME,WA,OJECT(2)
C
      DATA WOPTIO /'?       ','ALL     ','CARBON  ','CMS-DOS ',
     1             'CONTINUE','DAYEUPHO','DIURNAL ','DOMINANC',
     2             'DUMP    ','DYNADEAD','DYNAEXT ','END     ',
     3             'GROCHECK','HELP    ','INTERACT','MAXGRA  ',
     4             'MORCHECK','NEWS    ','OBJECTIV','OXMODOUT',
     5             'PARAM   ','PFKEYS  ','PMAX+MOR','PRODUC  ',
     6             'PRTPLOT ','RESET   ','RUN     ','SCRPLOT ',
     7             'SEL-DUMP','STOP    ','TEMPLIM ','WHICH?  '/
      DATA WBASE /'CONSTANT','FRACTION'/
      DATA OJECT /'BIOMASS ','GROWTH  '/
      DATA SUBTIT /'        ','Plankton','ic (*) a','nd slack',
     1             ' (+) of ','        '/
      DATA CHLTIT /'CHLOROPHYLL'/
      DATA PFON   /'EXEC   PFON'/
      DATA PFOFF  /'EXEC  PFOFF'/
      DATA PRODUC /'PRODUC  '/
      DATA DIURNA /'DIURNAL '/
      DATA CPLVAR
     1    /'Week  * PN  + DN  * PP  + DP  * PS  + DS  EX    -     +   '/
      DATA CPLCHL /'Week  -     +              0.0   0.0'/
      DATA CPLNUT /'Week  * PN  + DN           0.0   0.0',
     1             'Week  * PP  + DP           0.0   0.0',
     2             'Week  * PS  + DS           0.0   0.0'/
C
C
C  NOTE : RUN and CONTINUE/STOP should be specified for each job.
C  In an interactive job, the first option name should be INTERACT.
C  From that moment on the program will read user respones fronm unit 30
C  and prompt for a re-entry in case of an error.
C
C  If unspecified, the program will put all options "OFF".
C
C
C
C  Initiate default option values (turn them "off"
C
C  If subroutine OPTION is called by CHANGE, options "RUN" and "STOP"
C  are automatically reset.
C
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      IF (LOMODE .EQ. 1) GO TO 60
      DO 20 I = 1,NOPT
   20 OFON(I) = 0
   30 WRITE (OUUNI,99999)
      WRITE (IOU(6),99999)
      MEMORY = OFON(9)
      DO 40 I = 1,NOPT
      IF (OFON(I) .EQ. 1) OFON(I) = -1
   40 CONTINUE
      OFON(9) = MEMORY
      IDUMP=0
      ISDUMP=0
      LGRAMO=0
      LPRODU=0
      LDIEL=0
      LPOOLS=0
      LOXOUT=0
      LDYDEA=0
      LDYEXT=0
      LPLOT=0
      LSCR=0
      LDOM=0
      LPF=0
      LDAYEU=0
      LPMORT=0
      LTLIM=0
      LOBFUN=0
      IPL1 = 41
      IPL2 = 42
      IPL3 = 43
      OPL = 45
      IF (LMORCH .NE. 1) GO TO 50
      LMORCH=0
      IF (LOMODE .EQ. 0) GO TO 50
      NUSPE1=NUSPE1-NUECOG
      NUROWS=NUROWS-NUECOG
      NUCOLS=NUCOLS-NUECOG
   50 IF (LGROCH .NE. 1) GO TO 60
      LGROCH=0
      IF (LOMODE .EQ. 0) GO TO 60
      NUSPE1=NUSPE1-NUECOG
      NUROWS=NUROWS-NUECOG
      NUCOLS=NUCOLS-NUECOG
   60 LRUN=0
      LPARAM=0
      LSTOP=1
C
C----------------------------------------------------------------------
C      Start INPUT option mode
C----------------------------------------------------------------------
C
C  Here is the main loop of the program, where all options are read.
C

C     RETURN

   70 IF (POSIT .LT. 0) POSIT = 0
      if (ioflag .eq. 1) call blselect (woptio, nopt, 1071)
      I=INPTDT(1071,WO1,LENWRD)
      IF (MATCH(WOPTIO,NOPT,8,WO1,LENWRD,0,NUMWRD) .NE. 1)
     1    NUMWRD=NOPT+1
      GO TO (  200,  210,  150,  310,  930,  570,  140,  470,
     1         110,  170,  180,  600,  230,  920,  190,  120,
     2         530,  595,  596,  160,  600,  500,  580,  130,
     3         340,  610,  220,  510,  960,  940,  590,  480,
     4          80), NUMWRD
C
C  Error in input option mode.
C  Terminate in batch mode.
C
   80 CONTINUE
      IF (IOFLAG .EQ. 1) GO TO 90
      WRITE (OUUNI,99990) WO1
      WRITE (OUUNI,99980)
      LRUN=0
      RETURN
C
C  In an interactive run, try if a valid CMS command was entered.
C
   90 CONTINUE
      IRC = CMS (LINE,80)
      POSIT = 0
      IF (IRC .EQ. 0) GO TO 70
      IF (IRC .LT. 0) GO TO 100
      WRITE (OUUNI,330) IRC
      GO TO 70
  100 WRITE (OUUNI,99990) WO1
      GO TO 70
C
C  Set control parameters for selected options.
C
  110 IDUMP=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
  120 LGRAMO=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
  130 LPRODU=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
  140 LDIEL=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      IF (LPRODU .EQ. 1) GO TO 70
      IF (MATCH(WOPTIO,NOPT,8,PRODUC,8,0,NUMWRD) .EQ. 1) GO TO 130
      GO TO 70
  150 LPOOLS=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      IF (LPRODU .EQ. 1) GO TO 70
      IF (MATCH(WOPTIO,NOPT,8,PRODUC,8,0,NUMWRD) .EQ. 1) GO TO 130
      GO TO 70
  160 LOXOUT=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      IF (LPRODU .EQ. 1) GO TO 70
      IF (MATCH(WOPTIO,NOPT,8,DIURNA,8,0,NUMWRD) .EQ. 1) GO TO 140
      GO TO 70
  170 WRITE (OUUNI,99950)
      OFON(NUMWRD)=1
      LDYDEA=1
      GO TO 70
  180 WRITE (OUUNI,99940) WOPTIO(NUMWRD)
      GO TO 70
  190 CONTINUE
      IOFLAG=1
      INUNI=IOU(5)
      OUUNI=IOU(6)
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
  200 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99930)
      CALL VIDEO (1)
      WRITE (OUUNI,99925) WOPTIO(1), (WOPTIO(I),I=3,14),
     1                   (WOPTIO(I),I=16,NOPT)
      CALL VIDEO (0)
      GO TO 70
  210 CONTINUE
      WRITE (OUUNI,99840) WOPTIO(NUMWRD)
      GO TO 70
  220 LRUN=1
      GO TO 70
C
C  Put "GROCHECK" on; check if this option had already been put on
C  and check for erroneous values of BIOBASS.
C
  230 CONTINUE
      IF (LGROCH .NE. 1) GO TO 240
      WRITE (OUUNI,99830) WOPTIO(NUMWRD)
      GO TO 70
  240 LGROCH=1
      OFON(NUMWRD)=1
      LGBASE = 0
  250 if (ioflag .eq. 1) call blselect (wbase, 2, 1075)
      I=INPTDT(1075,WORD,LENWRD)
      IF (MATCH(WBASE,2,8,WORD,LENWRD,0,NUMWRD) .NE. 1)
     1    NUMWRD=3
       GO TO (270,290,260), NUMWRD
C
C  Error in input.
C
  260 IF (IOFLAG .EQ. 0) GO TO 80
      GO TO 250
C
C  Read constant baselevel.
C
  270 I=INPTNM(1072,BIOBAS,0,1)
      IF (BIOBAS .LE. 0.0) GO TO 280
      LGBASE = 1
      GO TO 300
  280 IF (IOFLAG .EQ. 0) GO TO 80
      WRITE (OUUNI,99800) BIOBAS
      GO TO 270
C
C  Read fractional baselevel.
C
  290 I=INPTNM(1076,BIOBAS,0,1)
      IF (BIOBAS .GT. 0.0 .AND. BIOBAS .LE. 1.0) GO TO 300
      IF (IOFLAG .EQ. 0) GO TO 80
      WRITE (OUUNI,99790) BIOBAS
      GO TO 290
C
C  Change array indices for extra constraints.
C
  300 CONTINUE
      NUROWS=NUROWS+NUECOG
      NUSPE1=NUSPE1+NUECOG
      NUCOLS=NUCOLS+NUECOG
      IF (LGBASE .EQ. 1) WRITE (OUUNI,99920) BIOBAS
      IF (LGBASE .EQ. 0) WRITE (OUUNI,99910) BIOBAS
      GO TO 70
C
C  Execute CMS or CP command.
C
  310 CONTINUE
      IF (IOFLAG .EQ. 1) GO TO 320
      WRITE (OUUNI,99780)
      GOTO 70
  320 CONTINUE
      IRC=CMS(LINE,80)
      POSIT = 0
      IF(IRC .NE. 0) WRITE (OUUNI,330) IRC
  330 FORMAT (' Return code from CMS: ',I5,'.')
      GO TO 70
C
C  Create input file for printplot routines.
C  Create title and subtitle; read upper limits for plots.
C  Put unit number for in- and output; rewind plot input units.
C  Note: use ERR = 370 construction for rewind of console. This is
C  because microsoft fortran does not permit the console to be rewinded,
C  where as IBM mainframe requires the console to be rewinded when an
C  empty line is entered (user hits carriage return).
C
  340 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      LPLOT=1
      OFON(NUMWRD)=1
      REWIND IPL1
      REWIND IPL3
  350 IF (IOFLAG .EQ. 1) THEN
         CALL VIDEO (7)
         WRITE(OUUNI,360)
         CALL VIDEO (0)
      END IF
  360 FORMAT(' Enter main title for every plot:')
      IF (IOFLAG .EQ. 1) REWIND (INUNI, ERR = 370, IOSTAT= IRC)
  370 READ(INUNI,380,END=350) TITLE
      WRITE(IPL1,390) CASE(11) (8:), (CASE(I),I=12,13), TITLE
      WRITE(IPL1,400) CPLVAR
      WRITE(IPL1,410) CPLCHL
  380 FORMAT(A56)
  390 FORMAT(2X,A1,2A8,8X,A56)
  400 FORMAT(A60)
  410 FORMAT(A36)
      IRC = STOS(CHLTIT,1,11,STRING,LENSTR)
      IRC = INPTNM(1077,PLLIM,0,1)
      WRITE(IPL1,430) PLLIM
  430 FORMAT(6X,F6.0,63X,'2')
      WRITE(IPL1,440)
  440 FORMAT(12X,'Predicted (-) and observed (+) chlorophyll')
C
C  Read and write nutrient data.
C
      DO 460 K=1,NUNUCO
      TNAME=CSTRA(K)
      IRC = STOS(TNAME,1,8,STRING,LENSTR)
      IRC = INPTNM(1077,PLLIM,0,1)
      WRITE(IPL1,410) CPLNUT(K)
      WRITE(IPL1,430) PLLIM
      IRC = STOS(TNAME,1,8,SUBTIT(6),LENOUT)
      WRITE(IPL1,450) (SUBTIT(J),J=1,6)
  450 FORMAT(12X,6A8)
  460 CONTINUE
      GO TO 70
C
C  Put option "DOMINANCE" on.
C
  470 LDOM = 1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
  480 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      DO 490 K = 1,NOPT
      IF (OFON(K) .LE. 0) GO TO 490
      WRITE (OUUNI,99970) WOPTIO(K)
  490 CONTINUE
      GO TO 70
C
C  Set PF-keys to BLOOM commands.
C
  500 CONTINUE
      IRC=CMS(PFON,11)
      IF (IRC .NE. 0) THEN
         WRITE (OUUNI,99720) WOPTIO(NUMWRD)
         GO TO 70
      END IF
      LPF=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
C
C  Create input file for screenplot routines.
C  Read upper limits for chlorophyll plot.
C  Put unit number for in- and output; rewind plot input units.
C
  510 CONTINUE
      LSCR=1
      OFON(NUMWRD)=1
      REWIND IPL2
      REWIND IPL3
      WRITE(IPL2,515) CASE(11) (8:), (CASE(I),I=12,13), (CASE(I),I=1,8)
  515 FORMAT (1X,A1,2A8,1X,8A8)
      WRITE(IPL2,400) CPLVAR
      WRITE(IPL2,410) CPLCHL
      IRC = STOS(CHLTIT,1,11,STRING,LENSTR)
      IRC = INPTNM(1077,PLLIM,0,1)
      WRITE(IPL2,520) PLLIM
  520 FORMAT(6X,F6.0,13X,'60. 18.',43X,'1')
      WRITE(IPL2,440)
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
C
C  Put "MORCHECK" on; check if this option had already been put on
C  and check for erroneous values of TOPLEV.
C
  530 CONTINUE
      IF (LMORCH .NE. 1) GO TO 540
      WRITE (OUUNI,99830) WOPTIO(NUMWRD)
      GO TO 70
  540 LMORCH=1
      OFON(NUMWRD)=1
C
C  Read toplevel.
C
  550 I=INPTNM(1078,TOPLEV,0,1)
      IF (TOPLEV .GT. 0.0) GO TO 560
      WRITE (OUUNI,99770) TOPLEV
      IF (IOFLAG .EQ. 0) GO TO 80
      GO TO 550
C
C  Change array indices for extra constraints.
C
  560 CONTINUE
      NUROWS=NUROWS+NUECOG
      NUSPE1=NUSPE1+NUECOG
      NUCOLS=NUCOLS+NUECOG
      WRITE (OUUNI,99760) TOPLEV
      IF (LGROCH .EQ. 1) THEN
         GO TO 70
      ELSE
         WRITE (OUUNI,99740) WOPTIO(13)
         NUMWRD = 13
         GO TO 240
      END IF
C
C  Put option "DAYEUPHO" on: compute the euphotic depth and assume that
C  the actual day length observed by the phytoplankton cells equals the
C  time spend in the euphotic zone.
C
  570 LDAYEU=1
      OFON(NUMWRD)=1
      IRC=INPTNM(1079,EULIGH,0,1)
      WRITE (OUUNI,99730) EULIGH
      GO TO 70
C
C Put option "PMAX+MOR" on: add the mortality rate constant of each
C species to the net gross rate in order to compute the gross growth
C rate. This option is included to maintain compatibility with older
C program versions.
C
  580 LPMORT=1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99970) WOPTIO(NUMWRD)
      GO TO 70
C
C Put option "TEMPLIM" on: set a lower temperature limit for the
C the growth and mortality rates of all types.
C
  590 LTLIM=1
      OFON(NUMWRD)=1
      IRC=INPTNM(1080,TEMLIM,0,1)
      IRC=INPTNM(1081,BASMOR,0,1)
      WRITE (OUUNI,99710) BASMOR,TEMLIM
      GO TO 70
C
C Call subroutine NEWS to print release notes.
C
  595 OFON(NUMWRD)=1
      CALL NEWS
      GO TO 70
C
C Determine the objective of the model: maximize biomass or growth.
C
  596 CONTINUE
  597 if (ioflag .eq. 1) call blselect (oject, 2, 1082)
      I=INPTDT(1082,WORD,LENWRD)
      IF (MATCH(OJECT,2,8,WORD,LENWRD,0,NUM) .NE. 1) GO TO 597
       GO TO (598, 599), NUM
  598 LOBFUN = 0
      OFON(NUMWRD)=0
      WRITE (OUUNI,99700)
      GO TO 70
  599 LOBFUN = 1
      OFON(NUMWRD)=1
      WRITE (OUUNI,99690)
      GO TO 70
C
C Activate selective dump option.
C
  960 ISDUMP=1
      OFON(NUMWRD)=1
      IRC = INPTNM(1083,0.0D0,ISDPER(1),2)
      IRC = INPTNM(1084,0.0D0,ISDPER(2),2)
      WRITE (OUUNI,99680) ISDPER(1),ISDPER(2)
      GO TO 70
C
C     Return to parameter mode to modify input values.
C
  600 LPARAM=1
      RETURN
C
C----------------------------------------------------------------------
C       Start RESET option mode
C----------------------------------------------------------------------
C
  610 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
  620 if (ioflag .eq. 1) call blselect (woptio, nopt, 1073)
      I=INPTDT(1073,WA,LENWRD)
      IF (MATCH(WOPTIO,NOPT,8,WA,LENWRD,0,NUM2) .NE. 1)
     1    NUM2=NOPT+1
      GO TO (  740,   30,  690,  800,  770,  890,  680,  820,
     1         650,  710,  720,   70,  780,  760,  720,  660,
     2         870,  620,  915,  700,  770,  850,  900,  670,
     3         810,  620,  770,  860,  970,  770,  910,  830,
     4        630), NUM2
C
C  Error in reset option mode.
C
  630 CONTINUE
      WRITE (OUUNI,99900) WA
      IF (IOFLAG .EQ. 0) GO TO 640
      GO TO 620
  640 CONTINUE
      WRITE (OUUNI,99980)
      LRUN=0
      RETURN
C
C Reset control parameters for selected options.
C
  650 IDUMP=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  660 LGRAMO=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  670 LPRODU=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  680 LDIEL=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  690 LPOOLS=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  700 LOXOUT=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  710 LDYDEA=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  720 LDYEXT=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  740 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
  750 WRITE (OUUNI,99880)
      CALL VIDEO (1)
      WRITE (OUUNI,99925) WOPTIO(3),(WOPTIO(I),I=6,13),
     1      (WOPTIO(I),I=16,17),WOPTIO(19),
     2      (WOPTIO(I),I=22,25), (WOPTIO(I),I=28,29),WOPTIO(31)
      CALL VIDEO (0)
      GO TO 620
  760 CONTINUE
      WRITE (OUUNI,99870)
      GO TO 750
  770 CONTINUE
      WRITE (OUUNI,99860) WOPTIO(NUM2)
      GO TO 620
C
C  Put "GROCHECK" off.
C  Reset array indices to remove extra constraints.
C
  780 CONTINUE
      IF (LGROCH .EQ. 1) GO TO 790
      WRITE (OUUNI,99810) WOPTIO(NUM2)
      GO TO 620
  790 LGROCH=0
      OFON(NUM2)=-1
      NUSPE1=NUSPE1-NUECOG
      NUROWS=NUROWS-NUECOG
      NUCOLS=NUCOLS-NUECOG
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  800 CONTINUE
      WRITE (OUUNI,99850)
      GO TO 620
  810 CONTINUE
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      LPLOT=0
      OFON(NUM2)=-1
      GO TO 620
  820 CONTINUE
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      LDOM=0
      OFON(NUM2)=-1
      GO TO 620
  830 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      DO 840 K = 1,NOPT
      IF (OFON(K) .GE. 0) GO TO 840
      WRITE (OUUNI,99960) WOPTIO(K)
  840 CONTINUE
      GO TO 620
  850 IRC=CMS(PFOFF,11)
      LPF=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
  860 CONTINUE
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      LSCR = 0
      OFON(NUM2)=-1
      GO TO 620
C
C  Put "MORCHECK" off.
C  Reset array indices to remove extra constraints.
C
  870 CONTINUE
      IF (LMORCH .EQ. 1) GO TO 880
      WRITE (OUUNI,99810) WOPTIO(NUM2)
      GO TO 620
  880 LMORCH=0
      OFON(NUM2)=-1
      NUSPE1=NUSPE1-NUECOG
      NUROWS=NUROWS-NUECOG
      NUCOLS=NUCOLS-NUECOG
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      IF (LGROCH .EQ. 0) THEN
         GO TO 620
      ELSE
         NUM2 = 13
         GO TO 790
      END IF
C
C  Reset option "DAYEUPHO": blooms are computed at the surface daylength
C
  890 CONTINUE
      LDAYEU=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
C
  900 LPMORT=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
C
  910 LTLIM=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
C
  915 LOBFUN=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
C
  970 ISDUMP=0
      OFON(NUM2)=-1
      WRITE (OUUNI,99960) WOPTIO(NUM2)
      GO TO 620
C
C----------------------------------------------------------------------
C       Call SUBROUTINE OPHELP for help
C----------------------------------------------------------------------
C
  920 LOPHLP = 0
      CALL OPHELP (WOPTIO,NOPT,LOPHLP)
      IF (LOPHLP .EQ. 0) GO TO 70
      LRUN = 0
      RETURN
C
C  Exit after "STOP"/"CONTINUE" has been set.
C
  930 LSTOP=0
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      GO TO 950
  940 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      IF (LPF .EQ. 1) IRC=CMS(PFOFF,11)
C
C Exit the subroutine. In a dynamic run check whether both
C "GROWTHCHECK" and "MORCHECK" have been put on. Do not leave the
C subroutine until both options have been initialized!
C
  950 CONTINUE
      IF (LRUN .EQ. 0) RETURN
      IF (LDYN .EQ. 0) RETURN
      IF (LMORCH .EQ. 1) THEN
         IF (LGROCH .EQ. 1) THEN
            RETURN
         ELSE
            WRITE (OUUNI,99750) WOPTIO (13)
         END IF
      ELSE
         IF (LGROCH .EQ. 0) WRITE (OUUNI,99750) WOPTIO (13)
         WRITE (OUUNI,99750) WOPTIO (17)
      END IF
      IF (IOFLAG .EQ. 0) THEN
         GO TO 80
      ELSE
         GO TO 70
      END IF
C
C  Formats for this subroutine.
C
99999 FORMAT (' All options have been (re)set to default values',/)
99990 FORMAT (1X,'Invalid option',1X,A8)
99980 FORMAT (//,1X,'Execution terminates',//)
99970 FORMAT (1X,'Option',1X,A8,1X,'has been put on')
99960 FORMAT (1X,'Option',1X,A8,1X,'has been reset')
99950 FORMAT (1X,'Dynamic computation of detritus pools',/)
99940 FORMAT (1X,'Option ',A8,' is no longer supported. You are',
     1        ' recommended',/,' to use one of the more recent dynamic',
     2        ' program options',/,' such as "GROWTHCHECK" or to use',
     3        ' the dynamic version of the model.')
99930 FORMAT (1X,'You are in Option command mode and should select',
     1        ' one of the following options:',/)
99925 FORMAT (8(2X,A8))
99920 FORMAT (1X,'Computation with extra constraints on growth',
     1        ' rates',/,1X,'using a CONSTANT base level of',1X,F8.2,/)
99910 FORMAT (1X,'Computation with extra constraints on growth',
     1        ' rates',/,1X,'using a FRACTION of ',F4.2,' of',
     2        ' the steady state biomass.',/)
99900 FORMAT (1X,'Invalid Reset option command:',1X,A8)
99890 FORMAT (1X,'It is not allowed to reset',1X,A8)
99880 FORMAT (1X,'You are in Reset option mode; to reset all options',
     1        ' enter "ALL".',/,1X,'To return to Option command mode',
     2        ' enter "END".',/,1X,'To get the names of options',
     2        ' which have been put off, enter "WHICH?"',/,1X,
     3        'To reset one particular option',
     4        ' enter one of the following names:')
99870 FORMAT (1X,'"HELP" command is only available in Option cmd.',
     1        ' mode',/,1X,'Essential information about Reset option',
     2        ' mode is given below')
99860 FORMAT (1X,'Option',1X,A8,1X,'is automatically reset by the',
     1        ' program before each run')
99850 FORMAT(' You cannot reset "CMS"; if you want to enter a CMS',
     1       ' command',/,' Return to Option command mode')
99840 FORMAT (1X,'Invalid input option',1X,A8,1X,'ignored')
99830 FORMAT (1X,A8,1X,' has been put on previously; second',
     1        ' specification will be ignored',/,' to avoid',
     2        ' incorrect indexing of the A-matrix and constraints.')
99810 FORMAT (1X,A8,1X,' has been put off previously; reset',
     1        ' will be ignored',/,' to avoid incorrect indexing',
     2        ' of the A-matrix and contraints.')
99800 FORMAT (1X,'You have specified base level =',2X,F6.1,/,
     1        ' This will cause infeasibility of all solutions')
99790 FORMAT (1X,'You have specified a fraction =',2X,F6.1,/,
     1        ' Probable input error')
99780 FORMAT (1X,'Command CMS not allowed in a batch job')
99770 FORMAT (1X,'You have specified top level =',2X,F6.1,/,
     1        ' TOPLEV must be positive.')
99760 FORMAT (1X,'Computation with extra mortality constraints',/,
     1        ' using a top level of',2X,F8.2,/)
99750 FORMAT (1X,'Option ',A8,' has not been turned on, although you ',
     1        'are using',/,' the Dynamic BLOOM version.',
     2        ' You cannot leave the Option command',/,' mode',
     3        ' unless you activate this option.',/)
99740 FORMAT (1X,'Option ',A8,' is forced on.')
99730 FORMAT (1X,'Blooms will be computed assuming a euphotic light',
     1        ' intensity of ',F10.1,/,' ')
99720 FORMAT (1X,'Error setting option',1X,A8,1X,'-- Command NOT',
     1        ' executed.')
99710 FORMAT (1X,'Minimum Pmax and mortality rate of ',F5.3,' for',
     1        ' temperatures less or equal to ',F4.1)
99700 FORMAT (1X,'Ojective function: maximize total biomass.')
99690 FORMAT (1X,'Ojective function: maximize net growth rates.')
99680 FORMAT (1X,'Selective DUMP for periods ',I2,' through ',I2)
      RETURN
      END
