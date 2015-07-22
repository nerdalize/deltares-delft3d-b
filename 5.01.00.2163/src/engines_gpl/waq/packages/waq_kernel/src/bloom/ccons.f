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
C  *    SUBROUTINE CCONS  TO MODIFY CONSUMER CHARACTERISTICS           *
C  *********************************************************************
C
C  0895 MvdV New subroutine for interactive adapatation of the input
C            for the new grazing subroutine CONSBL. This subroutine is
C            called by CHANGE if NUGRAZ > 0.

      SUBROUTINE CCONS (LERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      PARAMETER (NZOO = 26)
      INTEGER STOS
      CHARACTER*8 WZOO(NZOO),WORD,WORD2
      CHARACTER*3 NEW,OLD
      DATA NEW    /'New'/
      DATA Old    /'Old'/
      DATA WZOO  /'?       ','END     ','GRAZINIT','MAXITER ',
     1            'PHYTFEFR','PHYTPREF',
     1            'DETRFEFR','DETRPREF','MAXFILT ','MAXGROW ',
     2            'MAXMORT ','MONODFIL','ROUTRESP','MAXUPT  ',
     3            'STNDRESP','NUTCOEFF','TFILT   ','TMAXGROW',
     4            'TMAXMORT','TROUTRES','TMAXUPT ','TSTNDRES',
     5            'FREXWAT ','CTODRY  ','CONSCONC','PRINT   '/
C
C  General comment: in case an error is detected in this subroutine
C  -- for instance a missing or misspelled control word --
C  LERR will be put to 1 and subroutine CHANGE will set LRUN = 0.
C  Hence a batch job will be terminated,
C  but re-entry is possible in an interactive run.
C
      LERR=0
   10 IF (IOFLAG .EQ. 1) CALL BLSELECT (WZOO, NZOO, 1041)
      LENSTR = 0
      I=INPTDT(1041,WORD,LENWRD)
      IF (MATCH(WZOO,NZOO,8,WORD,LENWRD,0,NUMCA) .NE. 1) NUMCA=NZOO+1
      GO TO (   900,  1000,   500,   600,  3000,
     1         3000,  3000,  3000,  3000,  3000,
     2         3000,  3000,  3000,  3000,  3000,
     3         3000,  3000,  3000,  3000,  3000,
     4         3000,  3000,  3000,  3000,  3000, 2600,    20), NUMCA
C
C  Error detected in input of change zooplankton characteristic
C
   20 WRITE (OUUNI,99999) WORD
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 10
   30 WRITE (OUUNI,99990)
      LERR=1
      RETURN
C
C  Select number for consumer
C
 3000 CONTINUE
      IF (NUGRAZ .EQ. 0) GO TO 30
      IFILSP = 1
      IF (NUGRAZ .EQ. 1) GO TO 3040
      IF (IOFLAG .EQ. 0) GO TO 3020
      CALL BLPROMPT (1,NUGRAZ)
 3020 I=INPTNM(1085,0.0D0,IFILSP,2)
      IF (IFILSP .NE. 0 .AND. IFILSP .LE. NUGRAZ) GO TO 3040
      WRITE(OUUNI,99830) IFILSP
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 3020
 3040 CONTINUE
      GO TO (   900,  1000,   500,   600,  4000,
     1         4000,   300,   400,   700,   800,
     2         1100,  1200,  1300,  1400,  1500,
     3         1600,  1700,  1800,  1900,  2000,
     4         2100,  2200,  2300,  2400,  2500, 2600,  20), NUMCA
C
C  Read species name and find species and type number.
C
 4000 NSGR=0
      NSGR2 = 1
      IF (IOFLAG .EQ. 1) CALL BLSELECT (GRNAME, NUECOG, 1032)
      I=INPTDT(1032,WORD2,LENWRD)
      IF (MATCH(GRNAME,NUECOG,8,WORD2,LENWRD,0,NUMGR) .EQ. 1) GO TO 4010
C
C  Incorrect species name.
C
      WRITE (OUUNI,99820) WORD2
      WRITE (OUUNI,99810) (GRNAME(J),J=1,NUECOG)
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 4000
 4010 CONTINUE
      IGRO=IT2(NUMGR,2)-IT2(NUMGR,1)+1
      IF (IGRO .EQ. 1) GO TO 4040
      IF (IOFLAG .EQ. 0) GO TO 4020
      CALL BLPROMPT (1,IGRO)
      IRC = STOS(GRNAME(NUMGR),1,8,STRING,LENSTR)
 4020 I=INPTNM(1033,0.0D0,NSGR,2)
      IF (NSGR .NE. 0 .AND. NSGR .LE. IGRO) GO TO 4030
      WRITE(OUUNI,99800) NSGR,GRNAME(NUMGR)
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 4020
 4030 NSGR2=NSGR
      NSGR=NSGR-1
 4040 CONTINUE
      NUMSP=IT2(NUMGR,1)+NSGR
      GO TO (   900,  1000,   500,   600,   100,
     1          200,   300,   400,   700,   800,
     2         1100,  1200,  1300,  1400,  1500,
     3         1600,  1700,  1800,  1900,  2000,
     4         2100,  2200,  2300,  2400,  2500, 2600,  20), NUMCA
C
C  Modify feacal fractions for phytoplankton types
C
  100 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                         GFECFR(NUMSP,IFILSP)
      I=INPTNM(1102,GFECFR(NUMSP,IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GFECFR(NUMSP,IFILSP)
      GO TO 10
C
C  Modify preferences for phytoplankton types
C
  200 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                          ZOOPR(NUMSP,IFILSP)
      I=INPTNM(1103,ZOOPR(NUMSP,IFILSP) ,0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),ZOOPR(NUMSP,IFILSP)
      GO TO 10
C
C  Modify feacal fraction detritus
C
  300 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                         GDETFF(IFILSP)
      I=INPTNM(1086,GDETFF(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GDETFF(IFILSP)
      GO TO 10
C
C  Modify preference for detritus
C
  400 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                          GDETPR(IFILSP)
      I=INPTNM(1087,GDETPR(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GDETPR(IFILSP)
      GO TO 10
C
C  Modify initial grazing rate.
C
  500 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),GRAMO1
      I=INPTNM(1047,GRAMO1,0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRAMO1
      GO TO 10
C
C  Modify maximum permissible iteration number.
C
  600 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99940) OLD,WZOO(NUMCA),IPERM
      I=INPTNM(1048,0.0D0,IPERM,2)
      WRITE (OUUNI,99940) NEW,WZOO(NUMCA),IPERM
      GO TO 10
C
C  Modify maximum filtration rate
C
  700 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                         GRZFM(IFILSP)
      I=INPTNM(1088,GRZFM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRZFM(IFILSP)
      GO TO 10
C
C  Modify maximum growth rate
C
  800 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                         GRZGM(IFILSP)
      I=INPTNM(1089,GRZGM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRZGM(IFILSP)
      GO TO 10
C
C  Modify maximum mortality rate
C
 1100 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                         GRZMM(IFILSP)
      I=INPTNM(1090,GRZMM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRZMM(IFILSP)
      GO TO 10
C
C  Modify monod half saturation value of food for grazers for the
C  determination of the grazing rate
C
 1200 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GRZMO(IFILSP)
      I=INPTNM(1091,GRZMO(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRZMO(IFILSP)
      GO TO 10
C
C  Modify routine respiration
C
 1300 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GRZRE(IFILSP)
      I=INPTNM(1092,GRZRE(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRZRE(IFILSP)
      GO TO 10
C
C  Modify maximum uptake
C
 1400 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GRZRM(IFILSP)
      I=INPTNM(1093,GRZRM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRZRM(IFILSP)
      GO TO 10
C
C  Modify standard respiration
C
 1500 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GRZSE(IFILSP)
      I=INPTNM(1094,GRZSE(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRZSE(IFILSP)
      GO TO 10
C
C  Modify nutrient coefficients
C
 1600 CONTINUE
      IF (IOFLAG .EQ. 1) CALL BLSELECT (CSTRA, NUNUCO, 1042)
      I = INPTDT(1042,WORD2,LENWRD)
      IF (MATCH(CSTRA,NUNUCO,8,WORD2,LENWRD,0,NUMCO).EQ.1) GOTO 1610
      WRITE (OUUNI,99980) WORD2
      WRITE (OUUNI,99970) (CSTRA(I),I=1,NUNUCO)
      POSIT=0
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 1600
 1610 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99960) OLD,CSTRA(NUMCO),
     1                   ZOONUT(NUMCO,IFILSP)
      I=INPTNM(1043,ZOONUT(NUMCO,IFILSP),0,1)
      WRITE (OUUNI,99960) NEW,CSTRA(NUMCO),ZOONUT(NUMCO,IFILSP)
      GO TO 10
C
C  Modify temperature coefficient for filter rate
C
 1700 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GTMPFM(IFILSP)
      I=INPTNM(1095,GTMPFM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GTMPFM(IFILSP)
      GO TO 10
C
C  Modify temperature coefficient for maximum growth
C
 1800 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GTMPGM(IFILSP)
      I=INPTNM(1096,GTMPGM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GTMPGM(IFILSP)
      GO TO 10
C
C  Modify temperature coefficient for maximum mortality
C
 1900 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GTMPMM(IFILSP)
      I=INPTNM(1097,GTMPMM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GTMPMM(IFILSP)
      GO TO 10
C
C  Modify temperature coefficient for routine respiration
C
 2000 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GTMPRE(IFILSP)
      I=INPTNM(1098,GTMPRE(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GTMPRE(IFILSP)
      GO TO 10
C
C  Modify temperature coefficient for maximum uptake
C
 2100 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GTMPRM(IFILSP)
      I=INPTNM(1099,GTMPRM(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GTMPRM(IFILSP)
      GO TO 10
C
C  Modify temperature coefficient for standard respiration
C
 2200 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                   GTMPSE(IFILSP)
      I=INPTNM(1100,GTMPSE(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GTMPSE(IFILSP)
      GO TO 10
C
C  Modify fraction detritus excretion to the water column
C
 2300 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                          GTODET(IFILSP)
      I=INPTNM(1101,GTODET(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GTODET(IFILSP)
      GO TO 10
C
C  Modify carbon to dry weight ratio
C
 2400 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),
     1                          GCTDRY(IFILSP)
      I=INPTNM(1104,GCTDRY(IFILSP),0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GCTDRY(IFILSP)
      GO TO 10
C
C  Modify consumer pressure
C
 2500 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99790) OLD,IFILSP,
     1                          BZOOD(IFILSP),DZOOD(IFILSP)
      I=INPTNM(1003,BZOODI,0,1)
      I=INPTNM(1004,DZOODI,0,1)
      BZOOD(IFILSP)=BZOODI
      DZOOD(IFILSP)=DZOODI
      WRITE(OUUNI,99790) NEW,IFILSP,BZOODI,DZOODI
      GO TO 10
C
C  Print present set of zooplankton characteristics.
C
 2600 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99930)
      WRITE (OUUNI,99880) GRAMO1
      WRITE (OUUNI,99870) IPERM
      WRITE (OUUNI,99770)
      DO 2620 I=1,NUSPEC
 2620 WRITE (OUUNI,99760) SPNAME(I),(GFECFR(I,J),J=1,NUGRAZ)
      IF (IOFLAG.EQ.1) CALL MORESC

      WRITE (OUUNI,99750)
      DO 2630 I=1,NUSPEC
 2630 WRITE (OUUNI,99760) SPNAME(I),(ZOOPR(I,J),J=1,NUGRAZ)
      IF (IOFLAG.EQ.1) CALL MORESC

      WRITE (OUUNI,99740) WZOO( 7),(GDETFF(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO( 8),(GDETPR(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO( 9),(GRZFM (J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(10),(GRZGM (J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(11),(GRZMM (J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(12),(GRZMO (J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(13),(GRZRE (J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(14),(GRZRM (J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(15),(GRZSE (J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(17),(GTMPFM(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(18),(GTMPGM(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(19),(GTMPMM(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(20),(GTMPRE(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(21),(GTMPRM(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(22),(GTMPSE(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(23),(GTODET(J),J=1,NUGRAZ)
      WRITE (OUUNI,99740) WZOO(24),(GCTDRY(J),J=1,NUGRAZ)
      IF (IOFLAG.EQ.1) CALL MORESC

      DO 2610 I=1,NUNUCO
 2610 WRITE (OUUNI,99780) CSTRA(I),(ZOONUT(I,J),J=1,NUGRAZ)

      DO 2650 J=1,NUGRAZ
        WRITE (OUUNI,99790) NEW,J,BZOOD(J),DZOOD(J)
 2650 CONTINUE

      GO TO 10
C
C  Print characteristics that may be modified
C
  900 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99850)
      CALL VIDEO (1)
      WRITE (OUUNI,99845) (WZOO(I),I=3,NZOO)
      CALL VIDEO (0)
      WRITE (OUUNI,99840)
      GO TO 10
C
C  Exit
C
 1000 CONTINUE
C
C  Formats for this subroutine
C
99999 FORMAT (1X,'Invalid consumer command',2X,A8)
99990 FORMAT (//,1X,'Excecution terminates following an error in',
     1       ' subroutine "CCONS"',//)
99980 FORMAT (1X,'Incorrect nutrient name ',A8)
99970 FORMAT (1X,'Valid nutrients are: ',6A9)
99960 FORMAT (1X,A3,' nutrient ',A8,' coefficient = ',F14.4)
99950 FORMAT (1X,A3,' value of ',A8,' = ',F14.4)
99940 FORMAT (1X,A3,' value of ',A8,' = ',I2)
99930 FORMAT (1X,'Present consumer characteristics: ')
99920 FORMAT (' Consumer nutrient coefficient: ')
99910 FORMAT (' Half saturation constant ZOOK = ',F6.1)
99900 FORMAT (' Phytoplankton escaping grazing XMIN = ',F6.1)
99890 FORMAT (' Grazing rate constant ZOOGR = ',F8.4,' per day')
99880 FORMAT (' Grazing rate first iteration step = ',F8.4)
99870 FORMAT (' Maximum number of iterations = ',I2)
99850 FORMAT (1X,'You are in consumer command mode.',/,' Select one',
     1        ' of the following characteristics:')
99845 FORMAT (9(1X,A8))
99840 FORMAT (1X,'To exit to parameter command mode enter "END".',/,
     1        1X,'Use "PRINT" command to show present dataset.',/)
99830 FORMAT(1X,'Incorrect type number ',I1,' for consumer.')
99820 FORMAT (1X,'Invalid species name',2X,A8)
99810 FORMAT (' Valid species names are: ',/,1X,10(1X,A9))
99800 FORMAT(1X,'Incorrect type number ',I1,' for species',1X,A8)
99790 FORMAT (1X,A3,' concentration of consumer type',1X,I2,1X,' is',
     1        1X,F6.2,1X,' * nominal conc. + ',1X,F6.2)
99780 FORMAT (' Consumer''s ',A8,'-carbon ratios: ',20(2X,F6.4))
99770 FORMAT (' Consumer''s feacal fraction for phytoplankton species:')
99760 FORMAT (1X,A8,20(2X,F6.4))
99750 FORMAT (' Consumer''s preference for phytoplankton species:')
99740 FORMAT (' Coeff. ',A8,' values: ',F6.4,19(2X,F6.4))
      RETURN
      END
