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

      subroutine caltau ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Calculation of bottom friction

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
C     Date    : 921223             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     9202    Pascal Boderie  Create xxx version for T890 SLIB
C     9207    Jos van Gils    Create xxx version for Djakarta Bay
C     920701  Pascal Boderie  Create first version vor T721.72
C     970701  Johan Boon      Only calculation of tau (z2025)
C     970407  Johan Boon      implementation 3D
C     980317  Jos van Gils    Use TOTDEPTH only, for 2D/3D-schem.
C     030303  Johan Boon      Implement soulsby and Swart, Nelson criteria
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        CALCULATE SHEER STRESS, CAUSED BY FLOW, WIND AND HUMAN ACIVITYS
C        (SHIPS, DREDGING). MODEL IS VERTICALLY AVERAGED.
C
C        THE SUBROUTINE ALSO PRODUCES A CALCULATED STREAM VELOCITY
C        BASED ON TAU-TOTAL FOR COMPARISON WITH CRITICAL STREAM VELOCITIES
C        IN STEAD OF CRITICAL TAUS (DELTA STUDY, T692).
C
C        AVERAGED MODELS
C
C Name    T   L I/O  Description                              Units
C ----    --- -  -   -------------------                      ----
C CHZ     R   1  L   Chezy coefficient                         [sqrt(m)/s]
C DEPTH   R   1  I   Water depth                                       [m]
C TOTDEP  R   1  I   Total water depth                                 [m]
C G       R   1  I   Acceleration of gravity                        [m/s2]
C RHOW    R   1  I   Density of water                              [kg/m3]
C TAUWIN  R   1  O   Shearstress by wind                  [kg/m/s2 = N/m2]
C TAUFLO  R   1  O   Shearstress by flow                            [N/m2]
C TAUSCH  R   1  O   Shearstress by ships and human activity        [N/m2]
C TAU     R   1  O   Total shearstress                              [N/m2]
C TAUVEL  R   1  O   Calculated velocity based on TAU                [m/s]
C VELOC   R   1  I   Velocity                                        [m/s]

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
      PARAMETER ( G      =     9.8    ,
     +            RHOW   =  1000.0    ,
     +            PI     = 3.14159265 ,
     +            KARMAN = 0.41       ,
     +            GRVITY = 9.811      )
      INTEGER     LUNREP
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
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
C
      KARMC1 = SQRT(GRVITY) / KARMAN
      KARMC2 = KARMAN / SQRT(GRVITY)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF (IKMRK2.EQ.0 .OR. IKMRK2 .EQ. 3) THEN
C
      H       = PMSA(IP1 )
      RL      = PMSA(IP2 )
      T       = PMSA(IP3 )
      TAUSCH  = PMSA(IP4 )
      ISWTAUVELOC = NINT(PMSA(IP5 ))
      TAUFLO  = PMSA(IP6 )
      VELOC   = PMSA(IP7 )
      CHZ     = PMSA(IP8 )
      TOTDEP  = PMSA(IP9 )
      ISWTAU  = NINT(PMSA(IP10))
      DEPTH   = PMSA(IP11)
C
C     Nelson criteria
C
      IF (H .GT. 0.55*TOTDEP) H = 0.55*TOTDEP
c
      CHZ=MAX(1.0,CHZ)
      IF ((ISWTAU .NE. 1) .AND. (ISWTAU .NE. 2) .AND. (ISWTAU .NE. 3))
     &     CALL ERRSYS ('Invald switch for tau (ISWTAU) in CALTAU', 1 )
      IF ((ISWTAUVELOC .NE. 1) .AND. (ISWTAUVELOC .NE. 2))
     &     CALL ERRSYS ('Invald switch for tau (ISWTAUVELOC) in CALTAU', 1 )

C     Initialisation
      TAU     = 0.0
      TAUWIN  = 0.0
      TAUVEL  = 0.0

C     Calculate roughness from Chezy
      ROUGH = 12. * TOTDEP / (10**(CHZ/18.))

C     Calculate Chezy 3D
      IF (IKMRK2.EQ.3) THEN
C we are doing a bottom cell --> it must be 3d
         CHZ3D = KARMC1 *   LOG( 1 + ( ( 0.5 * DEPTH )
     +                  / (TOTDEP * EXP(-1.*(1+(KARMC2 * CHZ))))
     +                       )     )
         CHZ3D=MAX(1.0,CHZ3D)
      ELSE
         CHZ3D = CHZ
      ENDIF

C     Shear stress by flow, calculate if wanted otherwise from input

      IF ( ISWTAUVELOC .EQ. 1 ) THEN
         TAUFLO = RHOW * G * VELOC**2 / CHZ3D**2
      ENDIF

C     Shear stress by waves

      IF ( ABS(H) .LT. 1.E-20 .OR. ABS(RL) .LT. 1.E-20 .OR.
     +     ABS(T) .LT. 1.E-20                              ) THEN
         TAUWIN = 0
      ELSE
         A6     = 2.0 * PI*TOTDEP/RL
         IF ( A6 .LE. 10.0 ) THEN
            UBG    = PI  * H/(T * SINH(A6))
            ALM    = UBG * T / ( 2*PI )
            RLF    = ALM/ROUGH
            IF (ISWTAU .EQ. 1) THEN
c           option Tamminga
                FWG    = 0.16* SQRT( ROUGH / ALM )
            ELSEIF (ISWTAU .EQ. 2) THEN
c           option Swart
                IF ( RLF .GT. (PI/2) ) THEN
                  FWG = 0.00251 * EXP ( 5.213 * RLF**(-0.19) )
                ELSE
                  FWG = 0.32
                ENDIF
            ELSEIF (ISWTAU .EQ. 3) THEN
c           option Soulsby
                FWG    = 0.237* RLF**(-0.52)
            ENDIF

            TAUWIN = FWG * 0.25 * RHOW * UBG**2
         ELSE
            TAUWIN = 0.0
         ENDIF
      ENDIF

  150 CONTINUE

C     Total shear stress moet eigenlijk via windrichting opgeteld worden,
C     gebeurt hier niet

C     Als TAUSCH = -1.0 dan wordt tau -1.0
      IF (TAUSCH .EQ. -1.0) THEN
          TAU = -1.0
          TAUVEL = -1.0
      ELSE
         TAU = TAUFLO  + TAUWIN + TAUSCH
CPBO  Re-calculate total sheerstress (TAU) to a total stream velocity
         TAUVEL = SQRT ( TAU * CHZ3D**2 / (RHOW * G) )
      ENDIF

      PMSA (IP12) = TAU
      PMSA (IP13) = TAUFLO
      PMSA (IP14) = TAUWIN
      PMSA (IP15) = TAUVEL
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
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
C
 9000 CONTINUE
C

      RETURN
      END
