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

      subroutine botmin ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Mineralisation of organic substances and desorption of AAP in the bed (S1,S2) for C, N, P and Si.

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
C     951205  Marnix v.d. Vat Extension for use with SWITCH
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        GENERAL MINERALISATION OF ORGANIC SUBSTANCES IN THE BOTTOM LAYERS
C        FOR CARBON, NITROGEN, PHOSPHORUS AND SILICATE). FORMULATION
C        IS ZERO AND FIRST ORDER AND TEMPERATURE CORRECTED.
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CRTEMP  R*4 1 I critical temperature for mineralisation             [xC]
C DEPTH   R*4 1 I actual depth of a segment                            [m]
C FL (1)  R*4 1 O mineralisation flux mixing layer (x=C,N,P,Si)  [gX/m3/d]
C MINRC   R*4 1 I first order mineralisation rate                    [1/d]
C MINTCR  R*4 1 I temperature coefficient two bottom layers          [1/d]
C ORG     R*4 1 I amount decaying organic material in mixing layer    [gX/m2]
C TEMP    R*4 1 I ambient temperature                                 [xC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
C TEMPC   R*4 1 L temperature coefficient                              [-]
C VOLUME  R*4 1 L volume calculated by DELWAQ                         [m3]
C ZEMIN   R*4 1 I zeroth order mineralisation rate mixing layer  [gX/m2/d]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      LOGICAL  TFACT

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
      IF ( IN3 .EQ. 0 .AND. IN4 .EQ. 0 .AND.
     *     IN5 .EQ. 0 .AND. IN6 .EQ. 0        ) THEN
         TEMP   = PMSA(IP5 )
         CRTEMP = PMSA(IP6 )
         IF ( TEMP .LT. CRTEMP ) THEN
C        Only the zeroth order term
            TEMFAK = 0.0
         ELSE
            MINRC  = PMSA(IP3 )
            MINTC  = PMSA(IP4 )
            TEMP20 = TEMP - 20.0
            TEMFAK = MINRC * MINTC ** TEMP20
         ENDIF
         TFACT  = .FALSE.
      ELSE
         TFACT  = .TRUE.
      ENDIF
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C
      IF ( TFACT ) THEN
         TEMP   = PMSA(IP5 )
         CRTEMP = PMSA(IP6 )
         IF ( TEMP .LT. CRTEMP ) THEN
C        Only the zeroth order term
            TEMFAK = 0.0
         ELSE
            MINRC  = PMSA(IP3 )
            MINTC  = PMSA(IP4 )
            TEMP20 = TEMP - 20.0
            TEMFAK = MINRC * MINTC ** TEMP20
         ENDIF
      ENDIF
C

      ZEMIN   = PMSA(IP1 )
      ORG     = MAX ( 0.0, PMSA(IP2 ) )
      VOLUME  = PMSA(IP7 )
      DEPTH   = PMSA(IP8 )
      SWITCH  = PMSA(IP9 )

C***********************************************************************
C**** Processes connected to the MINERALISATION
C***********************************************************************
C
C
C        Calculation of mineralisation flux ( M.L-3.t-1)
C
      IF (ABS(SWITCH).LT.0.5) THEN
C       NO SWITCH
        FL( 1 + IFLUX ) = ZEMIN/DEPTH + TEMFAK * ORG / DEPTH
        FL( 2 + IFLUX ) = 0.0
      ELSE
C       SWITCH
        FL( 1 + IFLUX ) = 0.0
        FL( 2 + IFLUX ) = ZEMIN/DEPTH + TEMFAK * ORG / DEPTH
      ENDIF
C
      ENDIF
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
c
 9000 CONTINUE
c
      RETURN
C
      END
