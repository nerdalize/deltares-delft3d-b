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

      subroutine calchz ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Calculate chezy coefficient using roughness and depth

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
C     980702  S.Lambrechtsen  Added switch for Chezytype
C                             Added 2d or 3d calculation
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        CALCULATE CHEZY COEFFICIENT USING ROUGHNESS AND DEPTH
C
C        AVERAGED MODELS
C
C Name    T   L I/O  Description                              Units
C ----    --- -  -   -------------------                      ----
C CHZ     R   1  L   Chezy coefficient                         [sqrt(m)/s]
C DEPTH   R   1  I   Water depth                                       [m]
C ROUGH   R   1  I   Bottom roughness                                  [m]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

C     Local declarations, constants in source
C
      REAL     ROUGH  , DEPTH , TOTDEP,    CHZ, ONESIX,
     +         MANCOF
      INTEGER  IP1    , IP2   , IP3   , IKMRK1, IKMRK2,
     +         ICHZTP , IP4   , IP5   , IP6   , ISEG


      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)

      ONESIX = 1.0/6.0
C you need this for maninng

      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C 0-inactive cell  1-active cell
       CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
       IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C place in layers   0-depth integerated (2D) 1-top 2-between 3-bottom
C

        ROUGH   =      PMSA(IP1 )
        MANCOF  =      PMSA(IP2 )
        DEPTH   =      PMSA(IP3 )
        TOTDEP  =      PMSA(IP4 )
        ICHZTP  = NINT(PMSA(IP5 ))

        IF (ICHZTP.EQ.1) THEN
C       Shear stress by flow according to White/Colebrook
         CHZ = 18. * LOG10 ( 12.* TOTDEP / ROUGH  )
        ELSE IF (ICHZTP.EQ.2) THEN
C       Chezy according to Manning
         CHZ = ( TOTDEP ** ONESIX) / MANCOF
        END IF
        CHZ = MAX(CHZ,1.0)

        PMSA (IP6 ) = CHZ
C
       ENDIF
      ENDIF
C
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
C
 9000 CONTINUE

      RETURN
      END
