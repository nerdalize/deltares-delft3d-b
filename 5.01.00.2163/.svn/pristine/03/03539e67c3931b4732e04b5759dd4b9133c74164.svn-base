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

      subroutine bacmrt ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Mortality of bacteria depending on UV-light, salinity and temperature

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Pascal Boderie
C     Date    : 921210             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     921210  Pascal Boderie  Create first version
C     980318  Jos van Gils    Dependency on salt input parameter
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        GENERAL ROUTINE FOR THE MORTALITY OF BACTERIA: A FIRST ORDER APPROACH
C        WITH A USER DEFINED RATE CONSTANT CORRECTED FOR TEMPERATURE AND
C        SALINITY. THE MORTALITY RATE IS HIGHTENED BY A LIGHT DEPENDANT PART
C        CONCENTRATION OF BACTERIA EXPRESSED IN SOMETHING/M3
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C BACT    R*4 1 I concentration bacteria                              [gX]
C CFRAD   R*4 1 I conversion factor RAD->mortality                    [m2/W/d]
C CRTEMP  R*4 1 I critical temperature for mortality                      [xC]
C MORT    R*4 1 L overall first order mortality rate                     [1/d]
C MRTRAD  R*4 1 O part of firt order mortality rate from RAD             [1/d]
C DEPTH   R*4 1 I water depth                                          [m]
C EXTUV   R*4 1 I extinction of UV radiation                         [1/m]
C FL (1)  R*4 1 O mortality flux                                      [X/m3/d]
C RAD     R*4 1 I solar radiation at surface (daily averge)         [W/m2]
C RCMRT   R*4 1 I user defined first order mortality rate                [1/d]
C TEMP    R*4 1 I ambient temperature                                 [xC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
C TEMPF   R*4 1 L temperature function                                 [-]
C TCMRT   R*4 1 I temperature coefficient for mortality                  [1/d]
C VOLUME  R*4 1 L DELWAQ volume                                       [m3]
C ZOUT    R*4 1 I chloride concentration                            [g/m3]
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

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
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      BACT   = PMSA( IP1 )
      RCMRT  = PMSA( IP2 )
      TCMRT  = PMSA( IP3 )
      TEMP   = PMSA( IP4 )
      CRTEMP = PMSA( IP5 )
      ZOUT   = PMSA( IP6 )
      RAD    = PMSA( IP7 )
      CFRAD  = PMSA( IP8 )
      DAYL   = PMSA( IP9 )
      FRUV   = PMSA( IP10)
      EXTUV  = PMSA( IP11)
      DEPTH  = PMSA( IP12)
      SPMRTZ = PMSA( IP13)

      IF (EXTUV .LT. 1E-20 )  CALL ERRSYS ('EXTUV in BACMRT zero', 1 )

C***********************************************************************
C**** Processes connected to the MORTALITY OF BACTERIA
C***********************************************************************
C
C
      IF (TEMP .LE. CRTEMP) THEN
C
C        No mortality at all
C
         FL( 1 + IFLUX ) = 0.0
C
      ELSE
C
C        Calculation of mortality flux ( M.L-3.t-1)
C
         TEMP20 = TEMP - 20.0
         TEMPF  = TCMRT ** TEMP20

C        Calculation of the RAD dependent part of the mortality
         MRTRAD = CFRAD*RAD*FRUV*DAYL*(1 - EXP(-EXTUV * DEPTH) )
     &                        / (EXTUV * DEPTH )

C        Calculation of the overall mortality
C        MORT  = ( RCMRT + 1.1*1E-5 * ZOUT ) * TEMPF + MRTRAD
         MORT  = ( RCMRT + SPMRTZ * ZOUT ) * TEMPF + MRTRAD
C
         FL ( 1 + IFLUX  ) = MORT * BACT
C
      ENDIF

      PMSA (IP14 ) = MORT
      PMSA (IP15 ) = MRTRAD
C
      ENDIF
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
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
c
 9000 CONTINUE
c
      RETURN
C
      END
