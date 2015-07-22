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

      subroutine ebuch4 ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Ebullition of methane (new, generic!)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    Water Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : SLIK, ONTW. BODEM-WATER UITWISSELINGSMODULES, Q2935.30
C     Author  : Johannes Smits
C     Date    : 020524            Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     020524  Johannes Smits  New generic process for methane ebullition
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Methane ebullition on the basis of the assumption that
C        all methane in excess of saturation excapes the water system
C        instantaneously. The building up of a gas bubbles stock in
C        in the sediment is ignored. Process is valid for overlying
C        water as well as sediment.
C
C        ----- description of parameters -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CCH4    R*4 1 I concentration of dissolved methane               [gC/m3]
C CCH4S   R*4 1 O saturation concentration of dissolved methane    [gC/m3]
C DCH4    R*4 1 L diff. between actual and saturation methane conc.[gC/m3]
C DELT    R*4 1 I computational time-step                              [d]
C DEPTH   R*4 1 I total depth of the overlying water column            [m]
C FSCALE  R*4 1 I scaling factor methane ebullition                    [-]
C POROS   R*4 1 I porosity                                             [-]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMP20  R*4 1 L stand. temperature (20) minus ambient temperature   [oC]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7
      INTEGER  IFLUX  , ISEG   , IKMRK1, ILUMON
C
      REAL     CCH4   , CCH4S  , DCH4  , FSCALE
      REAL     POROS  , TEMP   , TEMP20, DEPTH  , DELT
C
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST /.TRUE./
C
      CALL GETMLU(ILUMON)
C
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
C
C     -----Warnings-----
C
      IF (FIRST) THEN
      IF (PMSA(IP4) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : Poros',
     +                          ' should be greater than zero'
          ENDIF
          FIRST = .FALSE.
      ENDIF
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF ( IKMRK1 .GT. 0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
            CCH4   = MAX ( 0.0, PMSA(IP1 ) )
            FSCALE = PMSA(IP2 )
            TEMP   = PMSA(IP3 )
            POROS  = PMSA(IP4 )
            DEPTH  = PMSA(IP5 )
            DELT   = PMSA(IP6 )
C
C           Calculate the saturation concentration
C
            TEMP20 = 20 - TEMP
            CCH4S  = 18.76 * (1 + DEPTH/10.0) * (1.024**TEMP20)
            DCH4   = CCH4 / POROS - CCH4S
C
C           Calculate the ebullition flux
C
            IF ( DCH4 .LT. 0.0 ) THEN
                  FL( 1+IFLUX ) = 0.0
            ELSE
                  FL( 1+IFLUX ) = ( FSCALE * DCH4 ) / DELT
            ENDIF
C
C           The saturation concentration is output
C
            PMSA(IP7 ) = CCH4S
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + IN1
      IP2   = IP2   + IN2
      IP3   = IP3   + IN3
      IP4   = IP4   + IN4
      IP5   = IP5   + IN5
      IP6   = IP6   + IN6
      IP7   = IP7   + IN7
C
 9000 CONTINUE
C
      RETURN
C
      END
