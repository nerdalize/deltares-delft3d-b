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

      subroutine dissi  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Dissolution of Si in opal 

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |                 MCM                    |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : GEM  (T2087)
C     Author  : Rik Sonneveldt
C     Date    : 970925             Version : 0.1
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     970513  Rik Sonneveldt  first version, based on VIVIAN.FOR
C     970925  Rik Sonneveldt  dDiss ook als uitvoer item
C                             beveiliging tegen poros = 0.
C     970929  Rik Sonneveldt  IKMRK1 LOOP AANGEPAST
C     010711  Johannes Smits  Correction for porosity, change of names
C     120830  Johannes Smits  Addition of 1st order dissolution
C
C***********************************************************************
C
C     Description of the module :
C     dissolution of opal silicate
C
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CSID    R*4 1 I     concentration dissolved silicate            [gSi/m3]
C CSIDE   R*4 1 I     saturation concentration dissolved silicate [gSi/m3]
C FSOL    R*4 1 O     dissolution flux                          [gSi/m3/d]
C KSOL    R*4 1 I     dissolution rate                          [m3/gSi/d]
C OPAL    R*4 1 I     concentration opal silicate                 [gSi/m3]
C POROS   R*4 1 I     porosity                                         [-]
C TC      R*4 1 I     temperature coefficient of dissolution           [-]
C TEMP    R*4 1 I     temperature                                     [oC]
C TEMPC   R*4 1 -     temperature function                             [-]
C SWDISSI R*4 1 I     option: 0.0 2nd order diss., 1.0 1st order diss.  
C
C     Logical Units : -
C
C     Modules called : -
C
C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      REAL     KSOL  , FSOL  , TEMP  , TEMPC , TC , CSID , OPAL ,
     +         CSIDE , POROS , SWDISSI
      INTEGER  LUNREP, NOWARN
      DATA     NOWARN / 0 /
      SAVE     NOWARN
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
      IFLUX = 0
C
      DO 9000 ISEG = 1 , NOSEG
C
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1.OR.IKMRK1.EQ.3) THEN
C
      IF (BTEST(IKNMRK(ISEG),0)) THEN
         CSID    = MAX(PMSA(IP1),0.0)
         OPAL    = MAX(PMSA(IP2),0.0)
         CSIDE   = PMSA(IP3)
         KSOL    = PMSA(IP4)
         TC      = PMSA(IP5)
         TEMP    = PMSA(IP6)
         POROS   = PMSA(IP7)
         SWDISSI = PMSA(IP8)
C
C     Calculation of the dissolution flux
C
         FSOL = 0.0
C
         IF (POROS .GT. 0.05) THEN
C
            TEMPC = TC**(TEMP - 20.0)
C
            IF (NINT(SWDISSi) .EQ. 0) THEN
               FSOL  = KSOL * TEMPC * OPAL * ( CSIDE - CSID / POROS )
            ELSE
               FSOL  = KSOL * TEMPC * OPAL
            ENDIF
C
         ELSE
            FSOL  = 0.0
            NOWARN = NOWARN + 1
            IF ( NOWARN .LE. 25 ) THEN
               CALL GETMLU(LUNREP)
               write (LUNREP,*) 'warning: poros < 0.05 in process DisSi, ISEG=',ISEG,' POROS=',POROS
            ELSEIF ( NOWARN .EQ. 26 ) THEN
               CALL GETMLU(LUNREP)
               write (LUNREP,*) 'number of warnings poros < 0.05 in process DisSi >25 firther messages surpressed'
            ENDIF
         ENDIF
C
C     Output of module
C
         FL(1+IFLUX) = FSOL
         PMSA(IP9)   = FSOL
C
C     End active cells block
C
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
C
 9000 CONTINUE
C
      RETURN
C
      END
