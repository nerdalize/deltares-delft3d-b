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
C  * SUBROUTINE PROMPT TO WRITE PROMPTING MESSAGES FOR AN              *
C  *                     INTERACTIVE RUN                               *
C  *********************************************************************
C
C  0895 MvdV Addition of prompts for input related to grazing subroutine
C            CONSBL starting with label 1085

      SUBROUTINE PROMES (INDEX, ARG, OUTSTR)
      CHARACTER*80 OUTSTR
      INTEGER ARG
      INCLUDE 'ioblck.inc'
C
      IF (INDEX.GT.1000) GOTO 3000
      GOTO (1), INDEX
      RETURN
1     INDATA = ARG
      RETURN
C
3000  NDX = INDEX - 1000
      GOTO (1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,
     X      1011,1012,1013,3001,3001,3001,1017,1018,1019,1020,
     X      1021,1022,1023,3001,3001,3001,3001,3001,3001,3001,
     X      3001,1032,1033,1034,1035,1036,1037,1038,1039,3001,
     X      1041,1042,1043,1044,1045,1046,1047,1048,3001,3001,
     X      1051,1052,1053,1054,1055,1056,1057,1058,1059,1060,
     X      3001,3001,3001,3001,3001,3001,3001,3001,3001,3001,
     X      1071,1072,1073,1074,1075,1076,1077,1078,1079,1080,
     X      1081,1082,1083,1084,1085,1086,1087,1088,1089,1090,
     X      1091,1092,1093,1094,1095,1096,1097,1098,1099,1100,
     X      1101,1102,1103,1104),NDX
3001  RETURN
C
1001   WRITE(OUTSTR,2001)
2001   FORMAT(' Parameter or Option? ')
       RETURN
C
1002   WRITE(OUTSTR,2002)
2002   FORMAT(' Parameter cmd: ')
       RETURN
C
1003   WRITE(OUTSTR,2003)
2003   FORMAT(' Mult. factor: ')
       RETURN
C
1004   WRITE(OUTSTR,2004)
2004   FORMAT(' Add. factor: ')
       RETURN
C
1005   WRITE(OUTSTR,2005)
2005   FORMAT(' Nutrient name: ')
       RETURN
C
1006   WRITE(OUTSTR,2006)
2006   FORMAT(' Enter number of runs: ')
       RETURN
C
1007   WRITE(OUTSTR,2007) INDATA
2007   FORMAT(' Start period for run', I3, ': ')
       RETURN
C
1008   WRITE(OUTSTR,2008) INDATA
2008   FORMAT(' End period for run', I3, ': ')
       RETURN
C
1009   WRITE(OUTSTR,2009) INDATA
2009   FORMAT(' Increment for run', I3, ': ')
       RETURN
C
1010   WRITE(OUTSTR,2010)
2010   FORMAT(' Linear temperature constant: ')
       RETURN
C
1011   WRITE(OUTSTR,2011)
2011   FORMAT(' Mineralization constant: ')
       RETURN
C
1012   WRITE(OUTSTR,2012)
2012   FORMAT(' Enter mortality cmd: ')
       RETURN
C
1013   WRITE(OUTSTR,2013)
2013   FORMAT(' Temperature dependent or independent?')
       RETURN
C
1017   WRITE(OUTSTR,2017)
2017   FORMAT(' Sedimentation rate: ')
       RETURN
C
1018   WRITE(OUTSTR,2018)
2018   FORMAT(' Flushing rate: ')
       RETURN
C
1019   WRITE(OUTSTR,2019)
2019   FORMAT(' Organic min. rate: ')
       RETURN
C
1020   WRITE(OUTSTR,2020)
2020   FORMAT(' Chlorophyll min. temp. coeff: ')
       RETURN
C
1021   WRITE(OUTSTR,2021)
2021   FORMAT(' Chlorophyll min. const. coeff: ')
       RETURN
C
1022   WRITE(OUTSTR,2022)
2022   FORMAT(' Autolyse fraction: ')
       RETURN
C
1023   WRITE(OUTSTR,2023)
2023   FORMAT(' Line number: ')
       RETURN
C
1032   WRITE(OUTSTR,2032)
2032   FORMAT(' Species name: ')
       RETURN
C
1033   WRITE(OUUNI,2033) INDATA,(STRING(I),I=1,LENSTR)
2033   FORMAT (1X,'There are ',I2,' types of species ',48A1)
       WRITE(OUTSTR,20333)
20333  FORMAT (' Enter Type number: ')
       RETURN
C
1034   WRITE(OUTSTR,2034)
2034   FORMAT(' Specific extinction: ')
       RETURN
C
1035   WRITE(OUTSTR,2035)
2035   FORMAT(' Nutrient name: ')
       RETURN
C
1036   WRITE(OUTSTR,2036)
2036   FORMAT(' Stochiometric coeff: ')
       RETURN
C
1037   WRITE(OUTSTR,2037)
2037   FORMAT(' Stochiometry cmd: ')
       RETURN
C
1038   WRITE(OUTSTR,2038)
2038   FORMAT(' Chlorophyll carbon ratio: ')
       RETURN
C
1039   WRITE(OUTSTR,2039)
2039   FORMAT(' Carbon dry weight ratio: ')
       RETURN
C
1041   WRITE(OUTSTR,2041)
2041   FORMAT(' Zooplankton cmd: ')
       RETURN
C
1042   WRITE(OUTSTR,2042)
2042   FORMAT(' Nutrient name: ')
       RETURN
C
1043   WRITE(OUTSTR,2043)
2043   FORMAT(' Stochiometric coeff: ')
       RETURN
C
1044   WRITE(OUTSTR,2044)
2044   FORMAT(' Half saturation constant: ')
       RETURN
C
1045   WRITE(OUTSTR,2045)
2045   FORMAT(' Amount of phytoplankton escaping: ')
       RETURN
C
1046   WRITE(OUTSTR,2046)
2046   FORMAT(' Grazing rate constant: ')
       RETURN
C
1047   WRITE(OUTSTR,2047)
2047   FORMAT(' Initial grazing rate constant: ')
       RETURN
C
1048   WRITE(OUTSTR,2048)
2048   FORMAT(' Maximum iteration number: ')
       RETURN
C
1051   WRITE(OUTSTR,2051)
2051   FORMAT(' Growth cmd: ')
       RETURN
C
1052   WRITE(OUTSTR,2052)
2052   FORMAT(' Pmax. temp. coeff: ')
       RETURN
C
1053   WRITE(OUTSTR,2053)
2053   FORMAT(' Pmax. const. coeff: ')
       RETURN
C
1054   WRITE(OUTSTR,2054)
2054   FORMAT(' Pmax. function: ')
       RETURN
C
1055   WRITE(OUTSTR,2055)
2055   FORMAT(' Rel. mix. depth: ')
       RETURN
C
1056   WRITE(OUTSTR,2056)
2056   FORMAT(' Zoopl. pref: ')
       RETURN
C
1057   WRITE(OUTSTR,2057)
2057   FORMAT(' Mort. temp. coeff: ')
       RETURN
C
1058   WRITE(OUTSTR,2058)
2058   FORMAT(' Mort. const. coeff: ')
       RETURN
C
1059   WRITE(OUTSTR,2059)
2059   FORMAT(' Resp. temp. coeff: ')
       RETURN
C
1060   WRITE(OUTSTR,2060)
2060   FORMAT(' Resp. const. coeff: ')
       RETURN
C
1071   WRITE(OUTSTR,2071)
2071   FORMAT(' Option cmd: ')
       RETURN
C
1072   WRITE(OUTSTR,2072)
2072   FORMAT(' Species base level constant: ')
       RETURN
C
1073   WRITE(OUTSTR,2073)
2073   FORMAT(' Reset Option cmd: ')
       RETURN
C
1074   WRITE(OUTSTR,2074)
2074   FORMAT(' Help Option cmd: ')
       RETURN
C
1075  WRITE(OUUNI,2075)
2075  FORMAT (1X,'Use a constant base-level',
     1        ' for the initial biomass concentrations',/,
     2        ' or a (constant) fraction of the equilibrium value?')
      WRITE(OUTSTR,20755)
20755 FORMAT (' Enter Constant or Fraction: ')
       RETURN
C
1076   WRITE(OUTSTR,2076)
2076   FORMAT(' Species base level fraction: ')
       RETURN
C
1077   WRITE(OUUNI,2077) (STRING(I),I=1,LENSTR)
2077   FORMAT(' Enter upper limit (mg /m3) for plot of ',48A1,/)
       WRITE(OUTSTR,20775)
20775  FORMAT (' Enter zero for autoscaling: ')
       RETURN
C
1078   WRITE(OUTSTR,2078)
2078   FORMAT(' Species top level fraction: ')
       RETURN
C
1079   WRITE(OUTSTR,2079)
2079   FORMAT(' Euphotic light intensity in Joules/m2/day (PAR): ')
       RETURN
C
1080   WRITE(OUTSTR,2080)
2080   FORMAT(' Temperature baselevel for growth: ')
       RETURN
C
1081   WRITE(OUTSTR,2081)
2081   FORMAT(' Minimum value for mortality rate: ')
       RETURN
C
1082   WRITE(OUTSTR,2082)
2082   FORMAT(' Maximize total biomass or growth? ')
       RETURN
C
1083   WRITE(OUTSTR,2083)
2083   FORMAT(' Initial period for selective dump: ')
       RETURN
C
1084   WRITE(OUTSTR,2084)
2084   FORMAT(' Final period for selective dump: ')
C
C      0895 MvdV Start additional prompts for input related to grazing
C                subroutine CONSBL
C
1085   WRITE(OUUNI,2085) INDATA,(STRING(I),I=1,LENSTR)
2085   FORMAT (1X,'There are ',I2,' types of consumers.')
       WRITE(OUTSTR,20085)
20085  FORMAT (' Enter Type number: ')
       RETURN
C
1086   WRITE(OUTSTR,2086)
2086   FORMAT(' Feacal fraction for detritus: ')
       RETURN
C
1087   WRITE(OUTSTR,2087)
2087   FORMAT(' Preference for detritus: ')
       RETURN
C
1088   WRITE(OUTSTR,2088)
2088   FORMAT(' Maximum filtration rate: ')
       RETURN
C
1089   WRITE(OUTSTR,2089)
2089   FORMAT(' Maximum growth rate: ')
       RETURN
C
1090   WRITE(OUTSTR,2090)
2090   FORMAT(' Maximum mortality rate: ')
       RETURN
C
1091   WRITE(OUTSTR,2091)
2091   FORMAT(' Half saturation value for filtering: ')
       RETURN
C
1092   WRITE(OUTSTR,2092)
2092   FORMAT(' Routine respiration: ')
       RETURN
C
1093   WRITE(OUTSTR,2093)
2093   FORMAT(' Maximum uptake: ')
       RETURN
C
1094   WRITE(OUTSTR,2094)
2094   FORMAT(' Standard respiration: ')
       RETURN
C
1095   WRITE(OUTSTR,2095)
2095   FORMAT(' Temp. coeff. filter rate: ')
       RETURN
C
1096   WRITE(OUTSTR,2096)
2096   FORMAT(' Temp. coeff. max. growth: ')
       RETURN
C
1097   WRITE(OUTSTR,2097)
2097   FORMAT(' Temp. coeff. max. mortality: ')
       RETURN
C
1098   WRITE(OUTSTR,2098)
2098   FORMAT(' Temp. coeff. routine respiration: ')
       RETURN
C
1099   WRITE(OUTSTR,2099)
2099   FORMAT(' Temp. coeff. max. uptake: ')
       RETURN
C
1100   WRITE(OUTSTR,2100)
2100   FORMAT(' Temp. coeff. standard respiration: ')
       RETURN
C
1101   WRITE(OUTSTR,2101)
2101   FORMAT(' Fraction excretion to water column: ')
       RETURN
C
1102   WRITE(OUTSTR,2102)
2102   FORMAT(' Feacal fraction: ')
       RETURN
C
1103   WRITE(OUTSTR,2103)
2103   FORMAT(' Preference: ')
       RETURN
C
1104   WRITE(OUTSTR,2104)
2104   FORMAT(' Carbon to dry weight ratio: ')
       RETURN
C
       END
