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
C  *        SUBROUTINE OPHELP TO PRINT INFORMATION ON OPTIONS          *
C  *                     OF THE PROGRAM                                *
C  *********************************************************************
C
      SUBROUTINE OPHELP (WOPTIO,NOPT,LOPHLP)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8 WOPTIO(*),WORD
      INCLUDE 'ioblck.inc'
C
C  Clear the screen.
C  Match option names for which "HELP" is requested.
C
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99900)
      CALL VIDEO (1)
      WRITE (OUUNI,99898) (WOPTIO(I),I=1,NOPT)
   10 CONTINUE
      CALL VIDEO (0)
      IF (IOFLAG .EQ. 1) call blselect (woptio, nopt, 1074)
      I=INPTDT(1074,WORD,LENWRD)
      CALL VIDEO (1)
      IF (MATCH(WOPTIO,NOPT,8,WORD,LENWRD,0,NUM) .NE. 1)
     1    NUM=NOPT+1
      GO TO ( 130,   160,   80,  220,  180,  290,   70,  240,
     1         40,   100,  110,   20,  210,  140,  120,   50,
     2        280,   320,  330,   90,  200,  260,  300,   60,
     3        230,   150,  170,  270,  340,  190,  310,  250,
     4         25), NUM
C
C  Return to input option mode if "END" is entered.
C
   20 CONTINUE
      RETURN
C
C  Error in HELP option mode.
C
   25 WRITE (OUUNI,99890) WORD
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 10
   30 CONTINUE
      WRITE (OUUNI,99970)
      LOPHLP = 1
      RETURN
C
C  Print HELP information.
C
   40 CONTINUE
      WRITE (OUUNI,99880) WOPTIO(NUM)
      WRITE (OUUNI,99875) WOPTIO(NUM)
      GO TO 10
   50 CONTINUE
      WRITE (OUUNI,99870) WOPTIO(NUM)
      WRITE (OUUNI,99873) WOPTIO(NUM)
      GO TO 10
   60 CONTINUE
      WRITE (OUUNI,99860) WOPTIO(NUM)
      WRITE (OUUNI,99873) WOPTIO(NUM)
      GO TO 10
   70 CONTINUE
      WRITE (OUUNI,99850) WOPTIO(NUM)
      WRITE (OUUNI,99873) WOPTIO(NUM)
      GO TO 10
   80 CONTINUE
      WRITE (OUUNI,99840) WOPTIO(NUM)
      WRITE (OUUNI,99873) WOPTIO(NUM)
      GO TO 10
   90 CONTINUE
      WRITE (OUUNI,99830) WOPTIO(NUM)
      WRITE (OUUNI,99873) WOPTIO(NUM)
      GO TO 10
  100 CONTINUE
      WRITE (OUUNI,99820) WOPTIO(NUM)
      GO TO 10
  110 CONTINUE
      WRITE (OUUNI,99810) WOPTIO(NUM)
      GO TO 10
  120 CONTINUE
      WRITE (OUUNI,99800) WOPTIO(NUM)
      GO TO 10
  130 CONTINUE
      WRITE (OUUNI,99790) WOPTIO(NUM)
      GO TO 10
  140 CONTINUE
      WRITE (OUUNI,99780) WOPTIO(NUM)
      WRITE (OUUNI,99895)
      GO TO 10
  150 CONTINUE
      WRITE (OUUNI,99770) WOPTIO(NUM)
      GO TO 10
  160 CONTINUE
      WRITE (OUUNI,99760) WOPTIO(NUM)
      GO TO 10
  170 CONTINUE
      WRITE (OUUNI,99750) WOPTIO(NUM)
      GO TO 10
  180 CONTINUE
      WRITE (OUUNI,99740) WOPTIO(NUM)
      GO TO 10
  190 CONTINUE
      WRITE (OUUNI,99730) WOPTIO(NUM)
      GO TO 10
  200 CONTINUE
      WRITE (OUUNI,99720) WOPTIO(NUM)
      GO TO 10
  210 CONTINUE
      WRITE (OUUNI,99710) WOPTIO(NUM)
      GO TO 10
  220 CONTINUE
      WRITE (OUUNI,99650) WOPTIO(NUM)
      GO TO 10
  230 CONTINUE
      WRITE (OUUNI,99640) WOPTIO(NUM)
      GO TO 10
  240 CONTINUE
      WRITE (OUUNI,99630) WOPTIO(NUM)
      GO TO 10
  250 CONTINUE
      WRITE (OUUNI,99620) WOPTIO(NUM)
      GO TO 10
  260 CONTINUE
      WRITE (OUUNI,99610) WOPTIO(NUM)
      GO TO 10
  270 CONTINUE
      WRITE (OUUNI,99600) WOPTIO(NUM)
      GO TO 10
  280 CONTINUE
      WRITE (OUUNI,99590) WOPTIO(NUM)
      GO TO 10
  290 CONTINUE
      WRITE (OUUNI,99580) WOPTIO(NUM)
      GO TO 10
  300 CONTINUE
      WRITE (OUUNI,99570) WOPTIO(NUM)
      GO TO 10
  310 CONTINUE
      WRITE (OUUNI,99560) WOPTIO(NUM)
      GO TO 10
  320 CONTINUE
      WRITE (OUUNI,99550) WOPTIO(NUM)
      GO TO 10
  330 CONTINUE
      WRITE (OUUNI,99540) WOPTIO(NUM)
      GO TO 10
  340 CONTINUE
      WRITE (OUUNI,99530) WOPTIO(NUM)
      GO TO 10
C
C  Formats for this subroutine.
C
99970 FORMAT (//,1X,'Execution terminates.',//)
99900 FORMAT (1X,'This section of the program gives you information:',
     1        //,5X,'1. How to use its options.',/,5X,'2. What these',
     2        ' options do.',//,1X,'You may request information on a',
     3        ' particular option by entering its name:',/)
99898 FORMAT (8(2X,A8))
99895 FORMAT (' To return to Option cmd. mode enter "END".')
99890 FORMAT (1X,'Invalid control word',1X,A8,1X,'in Help option mode.')
99880 FORMAT (1X,'Option',1X,A8,1X,'writes a detailed output to the ',
     1        'terminal',/,' containing information for all extinction',
     2        ' intervals,',/,1X,'for each iteration and time-step.')
99875 FORMAT (/,1X,'WARNING',1X,A8,1X,'writes many output lines',
     1        ' and should ONLY be used',/,' in an interactive run',
     2        ' for a limited number of periods (about 5 or less).',/,
     3        ' Note: use option "SEL-DUMP" to get a dump for a',
     4        ' limited number of periods only.')
99873 FORMAT (/,1X,'WARNING',1X,A8,1X,'writes its output to a separate',
     1        ' output unit.',/,1X,'You better not send this',
     3        ' output to the terminal.')
99870 FORMAT (1X,'Option',1X,A8,1X,'calculates the highest feasible',
     1        ' grazing pressure',/,' of edible groups',
     2        ' under the prevailing conditions.',/,
     3        ' It finds the grazing rate constant for which the',
     4        ' net-production is zero.')
99860 FORMAT (1X,'Option',1X,A8,1X,'calculates production, respiration',
     1        ' mortality, grazing',/,1X,'and flushing rates for each',
     2        ' phytoplankton group in each period.')
99850 FORMAT (1X,'Option',1X,A8,1X,'calculates the diurnal production',
     1        /,1X,'and respiration rates for each time-period.')
99840 FORMAT (1X,'Option',1X,A8,1X,'calculates the concentrations',
     1        ' of living and dead algae,',/,1X,'and the sedimentation',
     2        ' rate in each time-period.')
99830 FORMAT (1X,'Option',1X,A8,1X,'rewrites the ouptut of "DIURNAL"',
     1        ' into a specially',/,1X,'formatted dataset for the',
     2        ' oxygen model "OXYMOD".')
99820 FORMAT (1X,'Option',1X,A8,1X,'calculates potential phytoplankton',
     1        ' blooms',/,' using a dynamic computation scheme for',
     2        ' the detritus pools.')
99810 FORMAT (1X,'Option ',A8,' is no longer supported. You are',
     1        ' recommended',/,' to use one of the more recent dynamic',
     2        ' program options',/,' such as "GROCHECK" and "MORCHECK',
     3        ' or to use the dynamic version of the model.')
99800 FORMAT (1X,'Option',1X,A8,1X,'tells the program that you want',
     1        ' to make an interactive run.',/,1X,'It is the LAST',
     2        ' option read from unit 5',/,
     3        ' and may not be reset during an interactive session.',/,
     4        ' A copy of the summarized output of the program',/,
     5        ' written to the terminal will be written to unit',
     6        ' 21.',/)
99790 FORMAT (1X,'Option',1X,A8,1X,'gives you some information',/,
     1        ' how the Option mode of the program works.')
99780 FORMAT (1X,'Option',1X,A8,1X,'gives you the detailed information',
     1        /,' you are presently watching.')
99770 FORMAT (1X,'Option',1X,A8,1X,'gives you the possibility to put',
     1        ' one or more options "OFF"',/,' that had been',
     2        ' turned "ON" previously.')
99760 FORMAT (1X,A8,1X,'operates under "RESET" and resets ALL',
     1        ' options',/,1X,'to their initial default values.')
99750 FORMAT (1X,'Option',1X,A8,1X,'tells the program to excecute',
     1        ' and solve your problem.',/,1X,'This command should',
     2        ' ALWAYS be followed by "CONTINUE" OR "STOP".')
99740 FORMAT (1X,'Option',1X,A8,1X,'informs the program that you',
     1        ' want to continue this session',/,1X,'after it has',
     2        ' solved the currently specified bloom problem.')
99730 FORMAT (1X,'Option',1X,A8,1X,'informs the program that you',
     1        ' want to quit',/,1X,'after it has',
     2        ' solved the currently specified bloom problem.')
99720 FORMAT (1X,'Command',1X,A8,1X,'returns the program control',/,
     1        ' to its Parameter command mode to modify input values.')
99710 FORMAT (1X,'Option',1X,A8,1X,'calculates steady state phytoplank',
     1        'ton blooms',/,' with additional constraints on growth',
     2        ' rates,',/,' determined at the start of the time-step.',/
     3        ' The initial value equals the biomass level at the',
     4        ' previous time-step,'/,' or a constant baselevel, or',
     5        ' a fraction of the equilibrium value.')
99650 FORMAT (1X,'Command',1X,A8,1X,'allows you to execute operating',
     1        ' system commands.',/' In the mainframe model version',
     2        ' these are all CMS and CP commands valid',/,' under CMS',
     3        ' SUBSET.',/,' In the PC model version these are all',
     4        ' valid DOS commands.')
99640 FORMAT (1X,'Option',1X,A8,1X,'creates a plot of predicted',
     1        ' and observed chlorophyll',/' and of planktonic and',
     2        ' slack nutrients.',/,' If several subsequent runs are',
     3        ' made, plots will be produced for each of them.',/,
     4        ' However, if you want to change the title, or any of',
     5        ' the upper limits,',/,' you should put this option',
     6        ' "ON" again to enter new title and limits.')
99630 FORMAT (1X,'Option',1X,A8,1X,'creates a table with the total',
     1        ' biomass',/' and the relative species composition.',/,
     2        ' It also computes the average total dry weight',/,
     3        ' and chlorophyll concentration.')
99620 FORMAT (1X,'Option',1X,A8,1X,'gives you the names of all',
     1        ' options',/' which have been put "ON" or "OFF".')
99610 FORMAT (1X,'Option',1X,A8,1X,'resets the normal settings',
     1        ' of the PF-keys',/' to frequently used BLOOM commands.',
     2        /,' This option is NOT supported in the PC program',
     3        ' version.')
99600 FORMAT (1X,'Option',1X,A8,1X,'displays a plot of predicted',
     1        ' and observed chlorophyll',/' when a run is finished.')
99590 FORMAT (1X,'Option',1X,A8,1X,'computes phytoplankton blooms',
     1        ' using mortality constraints.',/,' This means that ',
     2        'species cannot decline faster than permitted by their',/,
     3        ' mortality rate constant.')
99580 FORMAT (1X,'Option',1X,A8,1X,'computes the phytoplankton growth',
     1        ' rates',/,' assuming that the relevant day length',
     2        ' equals the time',/,' spend above the euphotic depth.')
99570 FORMAT (1X,'Option',1X,A8,1X,'computes the gross growth',
     1        ' rates',/,' assuming that the mortality rate should',
     2        ' be added to',/,' the net growth rate. Hence:',/,/,
     3        '       Pgmax = Pnmax + Resp + Mort',/)
99560 FORMAT (1X,'Option',1X,A8,1X,'sets a lower limit for Pmax and',
     1        ' mortality rates',/,' at a specific temperature.')
99550 FORMAT (1X,'Option',1X,A8,1X,'gives you an extensive overview',
     1        ' of the lates news about the',/,' program. Most of',
     2        ' these items are NOT yet documented in manuals or',
     3        ' reports.')
99540 FORMAT (1X,'Option',1X,A8,1X,'determines the objective function',
     1        ' of the model.',/,' You may maximize the total biomass',
     2        ' (default) or the growth rate.')
99530 FORMAT (1X,'Option',1X,A8,1X,'activates "DUMP" for a selected',
     1        ' number of periods.')
      END
