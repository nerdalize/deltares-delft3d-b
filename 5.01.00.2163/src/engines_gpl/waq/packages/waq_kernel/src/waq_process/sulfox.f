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

      subroutine sulfox ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Oxidation of dissolved sulphide (0 and 2nd order) (new, generic !)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : SLIK, ONTW. BODEM-WATER UITWISSELINGSMODULES, Q2935.30
C     Author  : Johannes Smits
C     Date    : 020524             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     020524  Johannes Smits  New generic process for sulphide oxidation
C     021205  Johannes Smits  Correction to prevent negative concentr.
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Sulphide oxidation kinetics composed of zeroth order and
C        second order process for dissolved sulphide and oxygen.
C        Process is valid for overlying water as well as sediment.
C
C        ----- description of parameters -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C COX     R*4 1 I concentration of dissolved oxygen                 [g/m3]
C COXC    R*4 1 I critical oxygen conc. for sulphide oxidation      [g/m3]
C CSUD    R*4 1 I concentration of total dissolved sulphide        [gS/m3]
C DELT    R*4 1 I timestep                                             [d]
C FL (1)  R*4 1 O sulphide oxidation flux                        [gS/m3/d]
C FLUXOX  R*4 1 - sulphide oxidation flux                        [gS/m3/d]
C K0OXI   R*4 1 I zeroth order sulphide oxidation rate for DO    [gS/m3/d]
C KOXI    R*4 1 I second order sulphide oxidation rate            [m3.d/g]
C KTOXI   R*4 1 I temperature coefficient for oxidation                [-]
C POROS   R*4 1 I porosity                                             [-]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMPC   R*4 1 L ambient temperature correction function              [-]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
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
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9
      INTEGER  IFLUX  , ISEG   , IKMRK1, ILUMON
C
      REAL     CSUD   , COX
      REAL     K0OXI  , KOXI   , COXC
      REAL     POROS  , KTOXI  , TEMP   , TEMPC  , TEMP20
      REAL     DELT   , FLUXOX
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
      IN8  = INCREM( 8)
      IN9  = INCREM( 9)
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
C     -----Warnings-----
C
      IF (FIRST) THEN
          IF (PMSA(IP8) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : Poros should be',
     +                     ' greater than zero'
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
            CSUD   = MAX ( 0.0, PMSA(IP1 ) )
            COX    = MAX ( 0.0, PMSA(IP2 ) )
            K0OXI  = PMSA(IP3 )
            KOXI   = PMSA(IP4 )
            KTOXI  = PMSA(IP5 )
            COXC   = PMSA(IP6 )
            TEMP   = PMSA(IP7 )
            POROS  = PMSA(IP8 )
            DELT   = PMSA(IP9 )
C
C           Set the rates according to the DO concentration
C
            IF ( COX .LE. 0.0 ) THEN
                  KOXI  = 0.0
            ENDIF
            IF ( COX .GT. (COXC * POROS) ) THEN
                  K0OXI = 0.0
            ENDIF
C
C           Calculate the sulphide oxidation flux
C
            TEMP20 = TEMP - 20.0
            TEMPC  = KTOXI ** TEMP20
C
            FLUXOX = K0OXI + KOXI * TEMPC * CSUD * COX / POROS
            FLUXOX = MIN(FLUXOX,0.9*CSUD/DELT)
            FLUXOX = MIN(FLUXOX,0.5*COX/2.0/DELT)
C
            FL( 1+IFLUX ) = FLUXOX
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
      IP8   = IP8   + IN8
      IP9   = IP9   + IN9
C
 9000 CONTINUE
C
      RETURN
C
      END
