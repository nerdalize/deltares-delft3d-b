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

      subroutine vervlu ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Atmospheric exchange OMPs (volatilization/intake)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.80 / T1014
C     Author  : Jan van Beek, Pascal Boderie
C     Date    : 930324             Version : 0.01 for T1020
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     930326  Jan Van Beek    Create first version
C     930702  Pascal Boderie  Create version without Kl and Kg calc.
C                             correction calculation of NG
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Uni
C ----    --- -  -    -------------------                             --
C ATMC    R*4 1 I  Concentration OMV in atmosphere                [g.m3]
C CONC    R*4 1 I  Total concentration OMV in water               [g.m3]
C C1      R*4 1 I  Constant in temperature dependance of Henrys
C                  value represents delta S0 (entropy) / R           [-]
C C2      R*4 1 L  Constant in temperature dependence of Henrys
C                  value represents delta H0 (enthalpy) / R          [-]
C DEPTH   R*4 1 I  Depth                                             [m]
C E       R*4 1 LC Natural logaritmic                                [-]
C FDIS    R*4 1 I  Fraction omive free dissolved                     [-]
C FL      R*4 1 O  Calculated volatilizatioin flux              [g/m3/d]
C H0TREF  R*4 1 I  Henrys constant at reference temperature  [Pa.m3\mol]
C H2TREF  R*4 1 L  Dimensionless Henry at Tref
C                  on a basis of moelfraction      [molefracG/molefracL]
C H1TEMP  R*4 1 I  Dimensionless Henry at any TEMP     [mol/m3/(mol.m3)]
C KL      R*4 1 I  Mass transport coefficient liquid phase         [m/d]
C KG      R*4 1 I  Mass transport coefficient gas phase            [m/d]
C KV      R*4 1 O  volatilization rate constant                    [m/d]
C KELVIN  R*4 1 LC absolute temperature reference                    [-]
C NG      R*4 1 L  amount moles in 1m3 gas                     [mole/m3]
C NL      R*4 1 LC amount moles in 1m3 water                   [mole/m3]
C P       R*4 1 LC atmospheric pressure                             [Pa]
C R       R*4 1 LC universal gas constant                  [Pa.m3/mol/K]
C TREF    R*4 1 I  Reference temperature for H0                  [gradC]
C TEMP    R*4 1 I  Temperature                                   [gradC]
C-----------------------------------------------------------------------

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local declarations, constants in source
C
      PARAMETER ( E      =     2.718  ,
     +            KELVIN =   273.15   ,
     +            NL     = 55510.     ,
     +            P      =     1.01E+5,
     +            R      =     8.314    )
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
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.1)) THEN
C
C
C     Map PMSA on local variables
C
      CONC   = MAX ( 0.0, PMSA(IP1 ) )
      ATMC   = PMSA(IP2 )
      KL     = PMSA(IP3 )
      KG     = PMSA(IP4 )
      H0TREF = PMSA(IP5 )
      TREF   = PMSA(IP6 )
      C1     = PMSA(IP7 )
      TEMP   = PMSA(IP8 )
      DEPTH  = PMSA(IP9 )
C
C
C     Error messages
      IF (H0TREF .LT. 1E-30)  CALL ERRSYS ('H0TREF in VERVLU =<0', 1 )
      IF ( TEMP .LE. -KELVIN) CALL
     &                 ERRSYS ('TEMP in VERVLU < 0 DEG KELVIN', 1 )
      IF (KL .LT. 1E-30) CALL ERRSYS ('KL in VERVLU zero', 1 )
      IF (KG .LT. 1E-30) CALL ERRSYS ('KG in VERVLU zero', 1 )
C
C     Calculation of temperarure dependence of Henry
      H2TREF = H0TREF * NL / P
C
      C2     = ( KELVIN + TREF ) * ( LOG(H2TREF) - C1 )
C
      NG     = P / ( R * (KELVIN + TEMP) )
C
      H1TEMP = NG/NL * E**(C2/(KELVIN + TEMP) + C1 )
C
C     Calculation of volatilization rate constant
C
      KV     = 1./(1./KL + 1./(H1TEMP*KG))
C
C     Calculation of volatilization flux
C
      FL (1 + IFLUX) = (CONC - ATMC / H1TEMP ) * KV / DEPTH
C
C     Output
      PMSA(IP10) = KV
      PMSA(IP11) = H1TEMP
C
      ENDIF
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
      IP9   = IP9   + INCREM (  9 )
      IP10  = IP10  + INCREM ( 10 )
      IP11  = IP11  + INCREM ( 11 )
c
 9000 CONTINUE
c
C
      RETURN
      END
