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

      subroutine nh3fre ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Calculation conc. unionized ammonia

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.80 / T1014
C     Author  : Rik Sonneveldt, Jan van Beek, Pascal Boderie
C     Date    : 960822             Version : 0.01 for pH-delwaq Z2121
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     930702  template = vervlu.for by JvBeek and P. Boderie
C     960822  Rik Sonneveldt   first version
C     981202  Arno Nolte       Dissociation constant according to Millero (1995)
C                              both salinity abd temperature dependent
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Uni
C ----    --- -  -    -------------------                             --
C CNH3    R*4 1 L  molar conc NH3      ,,        ,,              [mol/l]
C CTNH4   R*4 1 I  total molar conc (gN/m3) NH4                  [gN/m3]
C FRNH3   R*4 1 O  fraction NH3 of total NH3 + NH4+              [mol/l]
C LKNH3   R*4 1 L  log equilibrium constant                 [log(mol/l)]
C LRATIO  R*4 1 L  log{(NH3)/(NH4+)} see eq. (e) below          [log(-)]
C M3TOL   R*4 1 L  conversion from 1/m3 to 1/l                    [m3/l]
C MNITRO  R*4 1 L  from gN/m3 NH3 or NH4+ to mol/m3             [gN/mol]
C KRF1A   R*4 1 I  coefficient a of reprofunction 1 KNH3    [log(mol/l)]
C KRF1B   R*4 1 I  coefficient a of reprofunction 1 KNH3 [log(mol/l)/oC]
C NH3     R*4 1 O  unionized ammonia                             [gN/m3]
C INH3SW  I*4 1 I  switch: option number calculation method KNH3     [-]
C PH      R*4 1 I  pH                                         [pH units]
C TEMP    R*4 1 I  temperature                                      [oC]
C TNH4    R*4 1 I  total concentration (gN/m3) NH4               [gN/m3]
C SAL     R*4 1 I  salinity                                       [g/kg]
C
C-----------------------------------------------------------------------
C     Name     Type   Library
C     ------   -----  ------------
      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      PARAMETER ( MNITRO =    14.0    ,
     +            KELVIN =   273.15   ,
     +            M3TOL  =     1.0E-3   )
C
C     set pointers
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
C
C     Loop over de segmenten
C
      DO 9000 ISEG = 1 , NOSEG
C
C     Eerste kenmerk actief of inactief segment
C
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
C
C     Alleen actieve segmenten behandelen ( KENMERK1 = 1 )
C
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
C     Map PMSA on local variables
C
      INH3SW = NINT(PMSA(IP1 ))
      TNH4   = PMSA(IP2 )
      PH     = PMSA(IP3 )
      TEMP   = PMSA(IP4 )
      TEMPK  = TEMP + KELVIN
      KRF1A  = PMSA(IP5 )
      KRF1B  = PMSA(IP6 )
      SAL    = MAX( 0.0 , PMSA(IP7 ) )
C
C     Error messages
C
      IF ( TEMP .LE. -KELVIN) CALL
     &                 ERRSYS ('TEMP in NH3FREE < 0 KELVIN', 1 )
C
C---- Procesformuleringen ---------------------------------------
      NH3   = 0.0
      FRNH3 = 0.0

C     Berekening alleen indien NH4 > 0

      IF (TNH4.GT.1.E-15) THEN
         CTNH4   = M3TOL * TNH4 / MNITRO
C
C        OPTION 1 -- log K berekenen
C
         IF (INH3SW.EQ.1) THEN
            LKNH3 = ( KRF1A + KRF1B*TEMP )
            LRATIO = LKNH3 + PH
            CNH3 = CTNH4 * (10**LRATIO)/(1.+(10**LRATIO))
            NH3 = CNH3 * MNITRO / M3TOL
            FRNH3 = CNH3/CTNH4
C
C        OPTION 2 ACCORDING TO MILLERO
C
         ELSEIF (INH3SW .EQ. 2) THEN
            LKNH3 = -6285.33/TEMPK + 0.0001635*TEMPK - 0.25444 +
     +             (0.46532 -123.7184/TEMPK) * SAL**0.5 +
     +             (-0.01992 + 3.17556/TEMPK) * SAL

            KNH3 = EXP (LKNH3)

C --- Unit of KNH3 [mol/kg solution], so redefine NH4 in same unit
            RHOH2O = (1000. + 0.7 * SAL / (1.-SAL/1000.)
     +            - 0.0061 * (TEMP-4.0) * (TEMP-4.0))/1000

            NH4 = TNH4 * M3TOL / MNITRO / RHOH2O

            NH3 = NH4 / (1. + 10**(-PH) / KNH3)
            NH3 = NH3 * RHOH2O * MNITRO / M3TOL

            FRNH3 = NH3 / TNH4
         ELSE
            CALL ERRSYS ('INH3SW in NH3FRE not 1 or 2', 1 )
         ENDIF

      ENDIF

      PMSA(IP8) = NH3
      PMSA(IP9) = FRNH3
C
      ENDIF
C
C---- Pointers ophogen ( altijd buiten de if's op kenmerk) in de segment loop
C
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
      IP9   = IP9   + INCREM (  9 )
C
 9000 CONTINUE
C
      RETURN
      END
