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

      subroutine satch4 ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Methane saturation concentration based on atmospheric methane pressure

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : SLIK, ONTW. BODEM-WATER UITWISSELINGSMODULES, Q2935.30
C     Author  : Johannes Smits
C     Date    : 020524             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     020524  Johannes Smits  New process for methane saturation
C
C***********************************************************************
C
C     Description of the module :
C
C        ----- description of parameters -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C PCH4    R*4 1 I atmospheric methane pressure                       [atm]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMP20  R*4 1 L stand. temperature (20) minus ambient temperature   [oC]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IP1, IP2, IP3, IN1, IN2, IN3
      INTEGER  ISEG   , IKMRK1, IFLUX
C
      REAL     PCH4   , CCH4S
      REAL     TEMP   , TEMP20
C
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF ( IKMRK1 .GT. 0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
            PCH4   = PMSA(IP1 )
            TEMP   = PMSA(IP2 )
C
C           Calculate the saturation concentration
C
            TEMP20 = 20 - TEMP
            CCH4S  = 18.76 * PCH4 * (1.024**TEMP20)
C
C           The saturation concentration is output
C
            PMSA(IP3 ) = CCH4S
C
      ENDIF
C
      IP1   = IP1   + IN1
      IP2   = IP2   + IN2
      IP3   = IP3   + IN3
C
 9000 CONTINUE
C
      RETURN
C
      END
