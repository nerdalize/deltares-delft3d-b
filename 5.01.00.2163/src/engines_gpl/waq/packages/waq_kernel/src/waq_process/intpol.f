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

      subroutine intpol ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Depth where wave is created or wind fetch from wind direction

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    Water Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES IMPAQT M4.70
C     Author  :
C     Date    : 960501             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     030808  Joost Icke      Bugfix, check on number of parameters
C                             Sobek ARS 11826
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        BLOCK INTERPOLATION
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C Y       R*4 8 I     dependent value pairs
C X       R*4 8 I     independent value pairs
C VALUE   R*4 1 I     independent value
C RESULT  R*4 1 I     resulting dependent value
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER   MAXPAR,IP, NUMPAR
      PARAMETER (MAXPAR=8)
      DIMENSION X(MAXPAR),Y(MAXPAR),IP(2*MAXPAR+2)

      DO 10 I=1,2*MAXPAR+2
        IP(I) = IPOINT(I)
   10 CONTINUE
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN

C     fill and count the number of classes

      VALUE = PMSA(IP(1))
      NUMPAR = 1
      DO I=1,MAXPAR
         Y(I) = PMSA(IP(2*I))
         X(I) = PMSA(IP(2*I+1))
         IF (X(I).LT. 0.0) EXIT
         NUMPAR = I
      ENDDO

C*******************************************************************************
C**** RESULT equals the Y corresponding with the interval from the previous
C****        to the current X (assuming the first interval to be 0 - X(1)
C***********************************************************************

      I = 0
   30 I = I + 1
      IF ((VALUE.LT.X(I)).OR.(I.EQ.NUMPAR)) THEN
         RESULT = Y(I)
      ELSE
         GOTO 30
      ENDIF


      PMSA(IP(2*MAXPAR+2)) = RESULT

      ENDIF
C
      DO 40 I=1,2*MAXPAR+2
        IP(I) = IP(I) + INCREM ( I  )
   40 CONTINUE
C
 9000 CONTINUE
C
      RETURN
C
      END
