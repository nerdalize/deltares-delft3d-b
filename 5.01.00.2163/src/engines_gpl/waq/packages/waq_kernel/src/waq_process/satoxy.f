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

      subroutine satoxy ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Saturation concentration of oxygen

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : WESTERSCHELDE
C     Author  : Jos van Gils
C     Date    : 950103             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     981222  Jan van Beek    Alternative formulation added
C     981215  Jos van Gils    Compute saturation concentration
C                             for all layers
C     950103  Jos van Gils    Create first version
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        COMPUTATION OF OXYGEN SATURATION CONCENTRATION
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CL      R*4 1 I concentration of chloride                         [g/m3]
C OXSAT   R*4 1 O saturation concentration of dissolved oxygen      [g/m3]
C SAL     R*4 1 I Salinity                                           [ppt]
C SWITCH  I*4 1 I Switch for formulation options                       [-]
C TEMP    R*4 1 I ambient temperature                                 [xC]


C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local declarations
C
      INTEGER  SWITCH, LUNREP, IKMRK1, ISEG  , IP1   ,
     +         IP2   , IP3   , IP4   , IP5
      REAL     CL    , TEMP  , SAL   , TEMP2 , PART1 ,
     +         PART2 , OXSAT , A1    , A2    , A3    ,
     +         A4    , B1    , B2    , B3
      PARAMETER ( A1 = -173.4292   ,
     +            A2 =  249.6339   ,
     +            A3 =  143.3483   ,
     +            A4 =  -21.8492   ,
     +            B1 =   -0.033096 ,
     +            B2 =    0.014259 ,
     +            B3 =   -0.0017   )
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
C
C     Initial calculations
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
!jvb  IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      CL     = PMSA(IP1 )
      TEMP   = PMSA(IP2 )
      SWITCH = NINT(PMSA(IP3 ))
      SAL    = PMSA(IP4 )

      IF ( SWITCH .EQ. 1 ) THEN
C
C        Weiss volgens Gils (WL)
C
         OXSAT  = (    14.652
     &              -  (0.41022  * TEMP)
     &              +  (0.089392 * TEMP)**2
     &              -  (0.042685 * TEMP)**3 )
     &              *  (1. - CL/1E+5 )
      ELSEIF ( SWITCH .EQ. 2 ) THEN
C
C        Weiss volgen Monteiro (CISR)
C        1.428571 = 32.*1000./22400.
C
         TEMP2 = (TEMP+273.)/100.
         PART1 = A1 + A2/TEMP2 + A3*LOG(TEMP2) + A4*TEMP2
         PART2 = SAL*(B1+B2*TEMP2+B3*TEMP2*TEMP2)
         OXSAT = EXP(PART1+PART2)*1.428571
C
      ELSE
          CALL GETMLU(LUNREP)
          WRITE(LUNREP,*) 'ERROR in SATOXY'
          WRITE(LUNREP,*) 'Illegal option for oxygen saturation formula'
          WRITE(LUNREP,*) 'Option in input:',SWITCH
          WRITE(*,*) ' ERROR in SATOXY'
          WRITE(*,*) ' Illegal option for oxygen saturation formula'
          WRITE(*,*) ' Option in input:',SWITCH
          CALL SRSTOP(1)
      ENDIF

C     Output of calculated oxygen saturation

      PMSA (IP5) = OXSAT
C
cjvb  ENDIF
C
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
C
 9000 CONTINUE
C
      RETURN
C
      END
