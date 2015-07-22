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

      subroutine nutupt ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Uptake of nutrients by growth of algae (DYNAMO)

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
C     940629  Pascal Boderie  Prevent divisions by zero
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Unit
C ----    --- -  -    -------------------                            ---
C CONMXN  R*4 1 L conc. beschikbaar N voor opname (positief)       [gN/m3
C CONMXP  R*4 1 L conc. beschikbaar P voor opname (positief)       [gP/m
C CONMXS  R*4 1 L conc. beschikbaar Si voor opname (positief)     [gSi/m
C DELT    R*4 1 I timestep in the computation                         [D]
C FL (1)  R*4 1 O uptake of NH4                                  [gN/m3/
C FL( 2)  R*4 1 O uptake of NO3                                  [gN/m3/
C FL( 3)  R*4 1 O uptake of PO4                                  [gP/m3/
C FL( 4)  R*4 1 O uptake of Si                                  [gSi/m3/
C IAUSYS  R*4 1 I ratio SCU and AUX                                    [
C MXPR1N  R*4 1 L max prod rate green algea based on avail N     [gC/m3/
C MXPR2N  R*4 1 L max prod rate diatoms based on avail N         [gC/m3/
C MXPR1P  R*4 1 L max prod rate green algea based on avail P     [gC/m3/
C MXPR2P  R*4 1 L max prod rate diatoms based on avail P         [gC/m3/
C MXPR2S  R*4 1 L max prod rate diatoms based on avail Si        [gC/m3/
C MXPRD1  R*4 1 L max prod rate green algea based on nutrients   [g/C/m3
C MXPRD2  R*4 1 L max prod rate diatoms based on nutrients      [g/C/m3/
C MXPRD   R*4 1 L max prod rate total algea based on nutrients  [g/C/m3/
C NCRAT1  R*4 1 I Nitrogen-Carbon ratio in green-algea             [gN/g
C NCRAT2  R*4 1 I Nitrogen-Carbon ratio in diatoms                 [gN/g
C NH4D    R*4 1 L fraction ammonium uptake all algea                   [
C NO3D    R*4 1 L fraction nitrate uptake all algea                    [
C NH4KR   R*4 1 I below this NH4 conc. no preference NO3/NH4       [gN/m
C NH4N    R*4 1 L available ammonium conc. (NH4-crit.NH4)          [gN/m
C PCRAT1  R*4 1 I Phosphorus-Carbon ratio in green-algea           [gP/g
C PCRAT2  R*4 1 I Phosphorus-Carbon ratio in diatoms               [gP/g
C FPP1    R*4 1 L total net production of algea1               [gC/m3/d]
C FPP2    R*4 1 L total net production of algea2               [gC/m3/d]
C SICRAT  R*4 1 I Silicate-Carbon ratio in diatoms                [gSi/g
C XNREST  R*4 1 L XNTOT - amount ammonia available for uptake       [g/m
C XNTOT   R*4 1 L total DIN uptake in one timestep                  [g/m

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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      PROD1     = PMSA(IP1)
      NCRAT1    = PMSA(IP2)
      PCRAT1    = PMSA(IP3)
      PROD2     = PMSA(IP4)
      NCRAT2    = PMSA(IP5)
      PCRAT2    = PMSA(IP6)
      SCRAT2    = PMSA(IP7)
      DELT      = PMSA(IP8)
      NH4       = PMSA(IP9)
      NO3       = PMSA(IP10)
      NH4KR     = PMSA(IP11)

C***********************************************************************
C**** Processes connected to the ALGEA model
C***********************************************************************

C      maximum uptake of N in one day (gC/m3)
       XNTOT = (NCRAT1 * PROD1 +
     &          NCRAT2 * PROD2 ) * DELT

C      check if NH4+NO3 available
       IF ( ((NH4 + NO3) .LE. 0.0) .OR. (XNTOT .LE. 0.0) ) THEN
           NH4D = 0.0
           NO3D = 0.0
       ELSE
          IF ( NH4 .GT. NH4KR ) THEN
              NH4N = NH4 - NH4KR
              IF ( XNTOT .LE. NH4N ) THEN
                 NH4D = 1.
                 NO3D = 0.
              ELSE
                  XNREST =  XNTOT -  NH4 + NH4KR
                  FNH4   =  NH4KR / (NO3 + NH4KR  )
                  NH4D   = ( NH4N + FNH4 * XNREST ) / XNTOT
                  NO3D   = 1. - NH4D
              ENDIF
          ELSE
C          below the critical NH4 conentration distribution of
C          NO3 and NH4 uptake based on availability!
                  NH4D = NH4 / ( NO3 + NH4 )
                  NO3D = 1. - NH4D
          ENDIF
       ENDIF
C     uitvoer fraction adsorbed as NH4
      PMSA (IP12 ) = NH4D
      PMSA (IP13 ) = XNTOT

C@    Uptake of NH4
      FL ( 1+IFLUX) = ( NCRAT1 * PROD1 +
     &            NCRAT2 * PROD2 ) * NH4D

C@    Uptake of NO3
      FL ( 2+IFLUX) = ( NCRAT1 * PROD1 +
     &            NCRAT2 * PROD2 ) * NO3D

C@    Uptake of PO4
      FL ( 3+IFLUX) =   PCRAT1 * PROD1 +
     &            PCRAT2 * PROD2

C@    Uptake of Si
      FL ( 4+IFLUX) =   SCRAT2 * PROD2
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM ( 1  )
      IP2   = IP2   + INCREM ( 2  )
      IP3   = IP3   + INCREM ( 3  )
      IP4   = IP4   + INCREM ( 4  )
      IP5   = IP5   + INCREM ( 5  )
      IP6   = IP6   + INCREM ( 6  )
      IP7   = IP7   + INCREM ( 7  )
      IP8   = IP8   + INCREM ( 8  )
      IP9   = IP9   + INCREM ( 9  )
      IP10  = IP10  + INCREM ( 10 )
      IP11  = IP11  + INCREM ( 11 )
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
C
 9000 CONTINUE
C
      RETURN
C
      END
