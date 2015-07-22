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

      subroutine sedsod ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sedimentation of oxygen demand

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : T1235.35
C     Author  : Pascal Boderie / Marnix van der Vat
C     Date    : 960411             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     960410  Marnix vd Vat   Create first version
C     980428  Jos van Gils    Work with f-fluxes i.s.o. d-fluxes
C     030807  Annette Beems   include third BOD pool
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C ISW     I*4 1 I switch oxygen consumption 0=BOD;1=COD;2=BOD+COD      [-]
C DBOD5   R*4 1 I sedimentation flux of CBOD5                    [gO/m3/d]
C DBOD52  R*4 1 I sedimentation flux of CBOD5_2                  [gO/m3/d]
C DBOD53  R*4 1 I sedimentation flux of CBOD5_3                  [gO/m3/d]
C DBODU   R*4 1 I sedimentation flux of CBODu                    [gO/m3/d]
C DBODU2  R*4 1 I sedimentation flux of CBODu_2                  [gO/m3/d]
C DNBOD5  R*4 1 I sedimentation flux of COD_Cr                   [gO/m3/d]
C DNBODU  R*4 1 I sedimentation flux of COD_Mn                   [gO/m3/d]
C DCODCR  R*4 1 I sedimentation flux of NBOD5                    [gO/m3/d]
C DCODMN  R*4 1 I sedimentation flux of NBODu                    [gO/m3/d]
C DEPTH   R*4 1 I depth of the segment                                 [m]
C DSEDOD  R*4 1 O sedimentation of oxygen demand                 [gO/m3/d]
C FSEDOD  R*4 1 O sedimentation of oxygen demand                 [gO/m2/d]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
      IN8  = INCREM( 8)
      IN9  = INCREM( 9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)
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
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      ISW    = NINT(PMSA(IP1 ))
      DBOD5  = PMSA(IP2 )
      DBOD52 = PMSA(IP3 )
      DBOD53 = PMSA(IP4 )
      DBODU  = PMSA(IP5 )
      DBODU2 = PMSA(IP6 )
      DNBOD5 = PMSA(IP7 )
      DNBODU = PMSA(IP8 )
      DCODCR = PMSA(IP9 )
      DCODMN = PMSA(IP10)
      DEPTH  = PMSA(IP11)
      IF (ISW.EQ.0) THEN
C       BOD
        FSEDOD = DBOD5 + DBOD52 + DBOD53 + DBODU + DBODU2 + DNBOD5 +
     1           DNBODU
      ELSEIF (ISW.EQ.1) THEN
C       COD
        FSEDOD = DCODCR + DCODMN
      ELSEIF (ISW.EQ.2) THEN
C       BOD + COD
        FSEDOD = DBOD5 + DBOD52 + DBOD53 + DBODU + DBODU2 + DNBOD5 +
     1           DNBODU + DCODCR + DCODMN
      ELSE
        WRITE (*,*) 'SEDSOD: Invalid option for SwOXYDem!'
        CALL SRSTOP(1)
      ENDIF

      DSEDOD = 0.0
      IF ( DEPTH .GT. 0.0 ) THEN
          DSEDOD = FSEDOD / DEPTH
      ENDIF

      PMSA(IP12) = FSEDOD

      FL( 1 + IFLUX ) = DSEDOD
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
      IP10  = IP10  + IN10
      IP11  = IP11  + IN11
      IP12  = IP12  + IN12
c
 9000 CONTINUE
c
      RETURN
C
      END
