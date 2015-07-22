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

      subroutine ddepth ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Dynamic calculation of the depth as volume / surf

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Jan van Beek
C     Date    : 921211             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     921210  Jan Van Beek    Create first version
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        DEPTH CALCULATION FROM HORIZONTAL SURFACE AREA OF A SEGMENT
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                             ----
C DEPTH   R*4 1 O depth of the water column                            [m]
C SURF    R*4 1 I surface area of segment                             [m2]
C VOLUME  R*4 1 I volume of segment                                   [m3]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
      character(55) message

      message = 'SURF in DDEPTH zero at segment:'
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1 .OR. IKMRK1.EQ.3) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C

      VOLUME = PMSA(IP1 )
      SURF   = PMSA(IP2 )

      write ( message(32:55) , '(i9,1x,e14.6)' ) iseg, surf
      IF (SURF    .LT. 1E-30) CALL ERRSYS ( message, 1 )

C***********************************************************************
C**** Calculate DEPTH
C***********************************************************************
C
      DEPTH = VOLUME / SURF
C
      PMSA (IP3 ) = DEPTH
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
c
 9000 CONTINUE
c
      RETURN
C
      END
