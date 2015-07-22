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

      subroutine watage ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Age of water through the tracer substances

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.80
C     Author  : Jan van Beek
C     Date    : 930324             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     930324  Jan Van Beek    Create first version
C     000518  Jos van Gils    Robustness increased!!
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                             ----
C AGE     R*4 1 O average age of the water
C CONCWA  R*4 1 I fraction of specific water ( conservative )
C CONCTR  R*4 1 I concentration tracer ( 1st order decay )
C DECAYR  R*4 1 I decay rate tracer
C FDECAY  R*4 1 O flux first order decay on tracer

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  IP1, IP2, IP3, IP4, IFLUX, ISEG, IKMRK1
      REAL     CONCWA, CONCTR, DECAYR, ARGUM, AGE, FDECAY

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      CONCWA = PMSA(IP1 )
      CONCTR = PMSA(IP2 )
      DECAYR = PMSA(IP3 )
C
      IF (DECAYR .LT. 1E-20 ) CALL ERRSYS ('RCDECTR in WATAGE zero', 1 )

C     Calculate age
C
      IF ( CONCWA .LE. 1.0E-20 ) THEN
          AGE = -999.
      ELSEIF ( CONCTR .LE. 1.0E-20 ) THEN
          AGE = -999.
      ELSEIF ( CONCTR .GT. CONCWA ) THEN
          AGE = -999.
      ELSE
          ARGUM =  CONCTR/CONCWA
          IF (ARGUM .LT. 1E-20 ) THEN
              AGE = -999.
          ELSE
              AGE = - LOG(ARGUM) / DECAYR
          ENDIF
      ENDIF
C
C     Calculate decay
C
      FDECAY  = DECAYR * CONCTR
C
C     Output
C
      PMSA(IP4) = AGE
      FL(1 + IFLUX)   = FDECAY
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
c
 9000 CONTINUE
c
      RETURN
      END
