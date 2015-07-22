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

      subroutine tfalg  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Temperature functions for algae growth and mortality

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
C TEMP    R*4 1 I ambient temperature                                 [x
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [x
C TCG1    R*4 1 I temp. coeff. for growth processes diatoms            [
C TCM1    R*4 1 I temp. coeff. for mortality processes green s         [
C TFUNG1  R*4 1 L temp. function for growth processes green            [
C TFUNM1  R*4 1 L temp. function for mortality processes green         [

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      LOGICAL  TMPOPT
C
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
C
      IF ( IN1 .EQ. 0 .AND. IN2 .EQ. 0 .AND. IN3 .EQ. 0 ) THEN
         TEMP   = PMSA(IP1 )
         TCG    = PMSA(IP2 )
         TCM    = PMSA(IP3 )
         TEMP20 = TEMP - 20.
         TFG    = TCG**TEMP20
         TFM    = TCM**TEMP20
         TMPOPT = .FALSE.
      ELSE
         TMPOPT = .TRUE.
      ENDIF
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      IF ( TMPOPT ) THEN
         TEMP   = PMSA(IP1 )
         TCG    = PMSA(IP2 )
         TCM    = PMSA(IP3 )
         TEMP20 = TEMP - 20.
C     Algal temp. functions for growth (G) and mortality (M) processes
         TFG    = TCG**TEMP20
         TFM    = TCM**TEMP20
      ENDIF

C     Uitvoer limiterende factoren
      PMSA(IP4 )  = TFG
      PMSA(IP5 )  = TFM
C
      ENDIF
C
         IP1    = IP1 + IN1
         IP2    = IP2 + IN2
         IP3    = IP3 + IN3
      IP4   = IP4   + IN4
      IP5   = IP5   + IN5
c
 9000 CONTINUE
c
      RETURN
      END
