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

      subroutine ptewor ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Production fluxes for TEWOR+

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : TEWOR M3086.60
C     Author  :
C     Date    : 980506             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     030808  Annette Kuin    Toevoeging FBOD, FBOD3 en FCOD (Q2943)
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Production fluxes for TEWOR
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C FBOD           I    TEWOR production flux of CBOD5            (gO2/m3/d)
C FBOD2          I    TEWOR production flux of CBOD5_2          (gO2/m3/d)
C FBOD3          I    TEWOR production flux of CBOD5_3          (gO2/m3/d)
C FCOD           I    TEWOR production flux of COD_Cr           (gO2/m3/d)
C FOXY           I    TEWOR production flux of OXY              (gO2/m3/d)
C FORGN          I    TEWOR production flux of Org-N             (gN/m3/d)
C FOON           I    TEWOR production flux of OON               (gN/m3/d)
C FNH4           I    TEWOR production flux of NH4               (gN/m3/d)
C FNO3           I    TEWOR production flux of NO3               (gN/m3/d)
C FECOLI         I    TEWOR production flux of EColi            (MPN/ml/d)
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     local declarations
C
      INTEGER  IKMRK1, IFLUX,ISEG,
     +         IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10
      REAL     FBOD, FBOD2, FBOD3, FCOD, FOXY, FORGN, FNH4, FNO3,
     +         FECOLI, FOON

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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
c     CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
c     IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.1)) THEN
C
      FBOD   = PMSA( IP1 )
      FBOD2  = PMSA( IP2 )
      FBOD3  = PMSA( IP3 )
      FCOD   = PMSA( IP4 )
      FOXY   = PMSA( IP5 )
      FORGN  = PMSA( IP6 )
      FNH4   = PMSA( IP7 )
      FNO3   = PMSA( IP8 )
      FOON   = PMSA( IP9 )
      FECOLI = PMSA( IP10)

      FL( 1 + IFLUX ) =   FBOD
      FL( 2 + IFLUX ) =   FBOD2
      FL( 3 + IFLUX ) =   FBOD3
      FL( 4 + IFLUX ) =   FCOD
      FL( 5 + IFLUX ) =   FOXY
      FL( 6 + IFLUX ) =   FORGN
      FL( 7 + IFLUX ) =   FNH4
      FL( 8 + IFLUX ) =   FNO3
      FL( 9 + IFLUX ) =   FOON

c     Conversion from MPN/ml/d to MPN/m3/d
      FL(10 + IFLUX ) =   FECOLI * 1E6

      ENDIF
C     ENDIF
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
C
 9000 CONTINUE
C
      RETURN
C
      END
