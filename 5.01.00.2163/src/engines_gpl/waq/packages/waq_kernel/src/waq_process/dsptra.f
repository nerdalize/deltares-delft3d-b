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

      subroutine dsptra ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Dispersion/diffusion in the sediment

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Impaqt in Delwaq4
C     Author  : Jos van Gils
C     Date    : 951020             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     951020  Jos van Gils    Create first version from BIOTUR
C   20000419  Jan van Beek    Check on dummy exchanges (0->0)
C   20020502  Jos van Gils    First version, from DIFTRA/TURTRA
C   20021203  Jos van Gils    Separate in two advective terms
C                             (necessary to have correct transport
C                              over interface with inhomogeneous Por)
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        BIOTURBATION/BIO-IRRIGATION BETWEEN SEDIMENT LAYERS
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C Coll Struct 1  O    Structure with collection of bottom collumn info
C                  Contains:
C    type(BotColmn), pointer :: set(:)  ! array with info for all bottom collumns
C    integer                 :: maxsize ! maximum size of the current array
C    integer                 :: cursize ! filled up to this size
C BotColm Struct 1   O  Structure with bottom collumn info
C                  Contains:
C    integer :: fstwatsed  ! first water sediment exchange number
C    integer :: lstwatsed  ! last  water sediment exchange number
C    integer :: topsedsed  ! first within collumn exchange number
C    integer :: botsedsed  ! last exchange of collumn to deeper bnd

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
      USE BottomSet     !  Module with derived types and add function

c     type ( BotColmnColl ) :: Coll  <= is defined in the module

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7
      INTEGER  IVAN, INAAR, IK, IQ
      INTEGER  IWA1,IWA2,ITOP,IBOT,IOFFSE
      REAL     TURCOE, DIFCOE, VD_SOL, VU_SOL,
     J         DIFLEN, ACTHS1, ACTHS2, POROS1, POROS2,
     J         XFROM , XTO   , VD_DIS, VU_DIS

c     Include column structure
c     we define a double column structure, one for downward,
c     and one for upward transport

      CALL MAKKO2 ( IEXPNT , IKNMRK , NOQ1   , NOQ2   , NOQ3   ,
     +              NOQ4   )

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
C
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)

c.....Segmentloop om uitvoergrootheden op segmentniveau op 0 te zetten
c     DO 9000 ISEG=1,NOSEG
c9000 CONTINUE
C
c.....Exchangeloop over de horizontale richtingen om op 0 te zetten
c.....en om de pointers te zetten
      DO 8000 IQ=1,NOQ1+NOQ2
c         Uitvoeritems op exchange level
          PMSA(IP6) = 0.0
          PMSA(IP7) = 0.0
          IP6 = IP6 + IN6
          IP7 = IP7 + IN7
 8000 CONTINUE
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
C
c.....Loop over kolommen
      DO 7000 IK = 1 , Coll%cursize

c         Select first column of exchanges for DOWNWARD advection

          IWA1 = Coll%set(IK)%fstwatsed
          IWA2 = Coll%set(IK)%lstwatsed
          ITOP = Coll%set(IK)%topsedsed
          IBOT = Coll%set(IK)%botsedsed

C        Offset to reach second colum for UPWARD advection

         IOFFSE = IBOT - (IWA1-1)

C        Loop over exchanges

         DO IQ = IWA1,IBOT

         IVAN   = IEXPNT(1,IQ)
         INAAR  = IEXPNT(2,IQ)
         TURCOE = PMSA(IP4+(Ivan -1)*IN4)
         DIFCOE = PMSA(IP5+(Inaar-1)*IN5)

         IF ( IQ .LE. IWA2 ) THEN

c.....WATER-SEDIMENT INTERFACE

             DIFLEN = PMSA(IP2+(IVAN -1)*IN2)
             ACTHS2 = PMSA(IP1+(INAAR-1)*IN1)
             POROS1 = PMSA(IP3+(IVAN -1)*IN3)
             POROS2 = PMSA(IP3+(INAAR-1)*IN3)
             XFROM  = DIFLEN
             XTO    = 0.5*ACTHS2
             VD_SOL = 0.0
             VU_SOL = 0.0
             VD_DIS =  DIFCOE*MIN(POROS1,POROS2)/POROS1/(XFROM+XTO)
             VU_DIS = -DIFCOE*MIN(POROS1,POROS2)/POROS2/(XFROM+XTO)

         ELSEIF ( IQ .EQ. IBOT ) THEN

c.....DEEP SEDIMENT BOUNDARY

             ACTHS1 = PMSA(IP1+(IVAN -1)*IN1)
             POROS1 = PMSA(IP3+(IVAN -1)*IN3)
             POROS2 = POROS1
             XFROM  = 0.5*ACTHS1
             XTO    = 0.5*ACTHS1
             VD_SOL =  TURCOE*MIN((1.-POROS1),(1.-POROS2))/(1.-POROS1)/
     J                 (XFROM+XTO)
             VU_SOL = -TURCOE*MIN((1.-POROS1),(1.-POROS2))/(1.-POROS2)/
     J                 (XFROM+XTO)
             VD_DIS = 0.0
             VU_DIS = 0.0

         ELSE

c.....SEDIMENT-SEDIMENT INTERFACE

             ACTHS1 = PMSA(IP1+(IVAN -1)*IN1)
             ACTHS2 = PMSA(IP1+(INAAR-1)*IN1)
             POROS1 = PMSA(IP3+(IVAN -1)*IN3)
             POROS2 = PMSA(IP3+(INAAR-1)*IN3)
             XFROM  = 0.5*ACTHS1
             XTO    = 0.5*ACTHS2
             VD_SOL =  TURCOE*MIN((1.-POROS1),(1.-POROS2))/(1.-POROS1)/
     J                 (XFROM+XTO)
             VU_SOL = -TURCOE*MIN((1.-POROS1),(1.-POROS2))/(1.-POROS2)/
     J                 (XFROM+XTO)
             VD_DIS =  DIFCOE*MIN(POROS1,POROS2)/POROS1/(XFROM+XTO)
             VU_DIS = -DIFCOE*MIN(POROS1,POROS2)/POROS2/(XFROM+XTO)

         ENDIF

         PMSA(IP6+(IQ-1       )*IN6)  = VD_SOL/86400.
         PMSA(IP6+(IQ-1+IOFFSE)*IN6)  = VU_SOL/86400.
         PMSA(IP7+(IQ-1       )*IN7)  = VD_DIS/86400.
         PMSA(IP7+(IQ-1+IOFFSE)*IN7)  = VU_DIS/86400.

         ENDDO

 7000 CONTINUE

      RETURN
      END
