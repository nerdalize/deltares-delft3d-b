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

      subroutine nlalg  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Nutrient limiation function for DYNAMO algae

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
C     ......  ..............  ..............................
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C     921229  Pascal Boderie  Add third algae type, nutrient ratio's
C                             per species
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Unit
C ----    --- -  -    -------------------                            ---
C
C DIN     R*4 1 L consumable-dissolved inorganic nitrogen          [gN/m
C FNUT1   R*4 1 L nutrient limitation function                         [
C FNUT2   R*4 1 L nutrient limitation function                         [
C FNUT3   R*4 1 L nutrient limitation function                         [
C KMDIN1  R*4 1 I half-saturation value nitrogen green-algea       [gN/m
C KMP1    R*4 1 I half-saturation value phosphorus green-algea     [gP/m
C KMSI    R*4 1 I half-saturation value silicate diatoms          [gSi/m
C NH4     R*4 1 I concentration of ammonium                        [gN/m
C NO3     R*4 1 I concentration of nitrate                         [gN/m
C PO4     R*4 1 I concentration of ortho phosphorus                [gP/m
C SI      R*4 1 I concentration of dissolved silicate               [g/m

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      AMOPRF    = PMSA( IP1)
      KMDIN     = PMSA( IP2)
      KMP       = PMSA( IP3)
      KMSI      = PMSA( IP4)
      NH4       = PMSA( IP5)
      NO3       = PMSA( IP6)
      PO4       = PMSA( IP7)
      SI        = PMSA( IP8)

      IF (AMOPRF .LT. 1E-20 )  CALL ERRSYS ('AMOPRF in NLALG zero', 1 )

C     Calculation of available dissolved N (NO3 corrected with AMOPRF)
      DIN = NO3 / AMOPRF + NH4
      IF ( (NO3 .LT. 0.0) .OR. (NH4 .LT. 0.0) ) DIN = 0.0

      IF (ABS(DIN+KMDIN).LT.1E-20) CALL ERRSYS
     &                     ('DIN+KMDIN zero in NLALG', 1 )
      IF (ABS(PO4+KMP).LT.1E-20) CALL ERRSYS
     &                     ('PO4+KMP zero in NLALG', 1 )
      IF ( (KMSI .NE. -1.0) .AND. (ABS(SI+KMSI).LT.1E-20) ) CALL ERRSYS
     &                     ('SI+KMSI zero in NLALG', 1 )

C     Nutrient limitation functions (MONOD)
      FN    = DIN / (DIN + KMDIN )

      IF (PO4 .LT. 0.0) THEN
           FP = 0.0
      ELSE
          FP    = PO4 / (PO4 + KMP )
      ENDIF

      IF  (KMSI .EQ. -1.0) THEN
          FS = 1.0
      ELSEIF (SI .LT. 0.0)  THEN
          FS =  0.0
      ELSE
          FS    = SI  / (SI  + KMSI)
      ENDIF

      FNUT = MIN (FN, FP, FS )

C@    Uitvoer limiterende factoren
      PMSA ( IP9)  = FN
      PMSA (IP10)  = FP
      PMSA (IP11)  = FS
      PMSA (IP12)  = FNUT

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
C
 9000 CONTINUE
C
      RETURN

      END
C
