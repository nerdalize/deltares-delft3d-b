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

      subroutine nutrel ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Release (nutrients/detritus) by of mortality algae DYNAMO

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
C FL( 1)  R*4 1 O autolysis of NN4                               [gN/m3/
C FL( 2)  R*4 1 O production of N-det                            [gN/m3/
C FL( 3)  R*4 1 O autolysis of P                                 [gP/m3/
C FL( 4)  R*4 1 O production of P-det                            [gP/m3/
C FL( 5)  R*4 1 O autolysis of Si                               [gSi/m3/
C FL( 6)  R*4 1 O production of Si-det                         [gSiC/m3/
C MRT1    R*4 1 I fraction of mortality dissolved as nutrients         [
C MRT2    R*4 1 I fraction of mortality dissolved as nutrients         [
C NCRAT1  R*4 1 I Nitrogen-Carbon ratio in green-algea             [gN/g
C NCRAT2  R*4 1 I Nitrogen-Carbon ratio in diatoms                 [gN/g
C PCRAT1  R*4 1 I Phosphorus-Carbon ratio in green-algea           [gP/g
C PCRAT2  R*4 1 I Phosphorus-Carbon ratio in diatoms               [gP/g
C RESP1   R*4 1 L total respiration rate const. green-algea          [1/
C RESP2   R*4 1 L total respiration rate const. diatoms              [1/
C SCRAT3  R*4 1 I Silicate-Carbon ratio in diatoms                [gSi/g

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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C

      MORT1     = PMSA(IP1 )
      NCRAT1    = PMSA(IP2 )
      PCRAT1    = PMSA(IP3 )
      FMRT1A    = PMSA(IP4 )
      FMRT2A    = PMSA(IP5 )
      MORT2     = PMSA(IP6 )
      NCRAT2    = PMSA(IP7 )
      PCRAT2    = PMSA(IP8 )
      SCRAT2    = PMSA(IP9 )
      FMRT1D    = PMSA(IP10)
      FMRT2D    = PMSA(IP11)

C***********************************************************************
C**** Processes connected to the ALGEA model
C***********************************************************************

C     Calculate fractions for carbon (different from nutrient fractions)
C     no part of carbon to autolyse!
       FDCA = 0.0
       FDCD = 0.0
       IF (FMRT1A .LT. 1.0) FDCA = FMRT2A / (1-FMRT1A)
       IF (FMRT1D .LT. 1.0) FDCD = FMRT2D / (1-FMRT1D)

C@    Production of DETC
      FL ( 1 + IFLUX ) = ( MORT1 * FDCA +
     &            MORT2 * FDCD )

C@    Production of OOC
      FL ( 2 + IFLUX ) = ( MORT1 * ( 1.0 - FDCA ) +
     &            MORT2 * ( 1.0 - FDCD ) )

C@    Autolysis of NH4
      FL ( 3 + IFLUX ) = ( MORT1 * NCRAT1 * FMRT1A +
     &            MORT2 * NCRAT2 * FMRT1D )

C@    Production of DETN
      FL ( 4 + IFLUX ) = ( MORT1 * NCRAT1 * FMRT2A +
     &            MORT2 * NCRAT2 * FMRT2D )

C@    Production of OON
      FL ( 5 + IFLUX ) = ( MORT1 * NCRAT1 * ( 1.0 - FMRT1A - FMRT2A ) +
     &            MORT2 * NCRAT2 * ( 1.0 - FMRT1D - FMRT2D ) )

C@    Autolysis of PO4
      FL ( 6 + IFLUX ) = ( MORT1 * PCRAT1 * FMRT1A +
     &            MORT2 * PCRAT2 * FMRT1D )

C@    Production of DETP
      FL ( 7 + IFLUX ) = ( MORT1 * PCRAT1 * FMRT2A +
     &            MORT2 * PCRAT2 * FMRT2D )

C@    Production of OOP
      FL ( 8 + IFLUX ) = ( MORT1 * PCRAT1 * ( 1.0 - FMRT1A - FMRT2A ) +
     &            MORT2 * PCRAT2 * ( 1.0 - FMRT1D - FMRT2D ) )

C@    Autolysis of Si
      FL ( 9 + IFLUX ) =   MORT2 * SCRAT2  * FMRT1D

C@    Production of Si-det
      FL (10 + IFLUX ) =   MORT2 * SCRAT2 * FMRT2D

C@    Production of OOSI
      FL (11 + IFLUX ) =   MORT2 * SCRAT2 * ( 1.0 - FMRT1D - FMRT2D )

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
C
 9000 CONTINUE
C
      RETURN
      END
