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

      subroutine meteo  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Process meteo from various meteo-stations

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C         Landelijk Warmte model Sobek River
C     Project :
C     Author  :
C     Date    :2004 november           Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     0811004 P.Boderie         Ruimtelijk middelen van 5 meteo stations tbv warmtemodel
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
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
      INTEGER   MAXSTA,MAXVAR, IP , NP
c
C     aantal meteo stations
c
      PARAMETER (MAXSTA=5)
c
C     aantal variabelen per station
c
      PARAMETER (MAXVAR=7)

c
C     aantal ongebonden variabelen
c
      PARAMETER (NP=5)


      DIMENSION RAD(MAXSTA),VWIND(MAXSTA), DIR(MAXSTA), HUM(MAXSTA),
     &          TEMP(MAXSTA), PRES(MAXSTA), SUN(MAXSTA)
      DIMENSION X(MAXSTA), Y(MAXSTA)
      DIMENSION IP((MAXSTA+1)* MAXVAR +  MAXSTA*2 + NP)
      DIMENSION DIST(MAXSTA), WFAC(MAXSTA)

      DO 10 I=1,(MAXSTA+1)* MAXVAR +  MAXSTA*2 + NP
        IP(I) = IPOINT(I)
   10 CONTINUE
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
C     waarden per station
c
      DO 20 I   = 1,MAXSTA
        RAD(I)  = PMSA(IP((I-1)*MAXVAR+1))
        VWIND(I)= PMSA(IP((I-1)*MAXVAR+2))
        DIR(I)  = PMSA(IP((I-1)*MAXVAR+3))
        HUM(I)  = PMSA(IP((I-1)*MAXVAR+4))
        TEMP(I) = PMSA(IP((I-1)*MAXVAR+5))
        PRES(I) = PMSA(IP((I-1)*MAXVAR+6))
        SUN(I) =  PMSA(IP((I-1)*MAXVAR+7))
   20 CONTINUE
C
C     coordinaten van de stations

      DO 21 I = 1, MAXSTA
          X(I) = PMSA(IP(MAXSTA*MAXVAR+(I-1)*2+1))
          Y(I) = PMSA(IP(MAXSTA*MAXVAR+(I-1)*2+2))
   21 CONTINUE
c
C     overige parameters
C
      SCALE  = PMSA(IP(MAXSTA*(MAXVAR+2)+1))
      NOSTAT = PMSA(IP(MAXSTA*(MAXVAR+2)+2))
      ICALCSW= NINT(PMSA(IP(MAXSTA*(MAXVAR+2)+3)))
      XSEG   = PMSA(IP(MAXSTA*(MAXVAR+2)+4))
      YSEG   = PMSA(IP(MAXSTA*(MAXVAR+2)+5))

C*******************************************************************************
C**** RESULT Calculated meteo parameters based on 1-MAXSTA meteo stations.
C*****       Option 1 looks up the nearest meteo station for each segment
C*****       Option 2 calculates the distance weighted average of NoStations
C*****                for each segment
C***********************************************************************

c     Bereken Distance Cell to all stations (in meters)
      MIN = -1.0
      SUM = 0.0
      SUM2 = 0.0
      IF ( NOSTAT .GT. MAXSTA ) THEN
          NOSTAT = MAXSTA
      ENDIF

      IF ( NOSTAT .LT. 1) THEN
          NOSTAT = 1
      ENDIF

c
      DO 30 I = 1, NOSTAT
          DIST(I) = SQRT ( (XSEG - X(I)*SCALE)*(XSEG - X(I)*SCALE) +
     &                     (YSEG - Y(I)*SCALE)*(YSEG - Y(I)*SCALE) )
c
        dist(i) = 1./max(dist(i),1.0)
        SUM  = SUM  + DIST(I)
        SUM2 = SUM2 + DIST(I) * DIST(I)

c
      IF (MIN .LT. 0.0) THEN
           MIN = DIST(I)
           INEAR = I
      ELSEIF (DIST(I) .LT. MIN) THEN
           MIN = DIST(I)
           INEAR = I
      ENDIF
c
   30 CONTINUE


c
c     optie 1:  nearest station
c
      IF  ( ICALCSW .EQ. 1 ) THEN
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +1)) = RAD(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +2)) = VWIND(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +3)) = DIR(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +4)) = HUM(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +5)) = TEMP(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +6)) = PRES(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +7)) = SUN(INEAR)
c
c     optie 2a: dist weighted lineair
c
      ELSE
c         optie 2 lineair inv dist
          IF ( ICALCSW .EQ. 2 ) THEN
              DO 41 I = 1 ,  NOSTAT
                                WFAC(I) = DIST(I) / SUM
   41         CONTINUE
c
c         optie 2b: inv dist kwadratisch
          ELSEIF ( ICALCSW .EQ. 3 ) THEN
              DO 42 I = 1 ,  NOSTAT
                  WFAC(I) = DIST(I)*DIST(I) / SUM2
   42         CONTINUE

          ENDIF

          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 1)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 2)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 4)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 5)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 6)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 7)) = 0.0
          DO 50 I = 1, NOSTAT
              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 1)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 1)) + WFAC(I) * RAD(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 2)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 2)) + WFAC(I) * VWIND(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 4)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 4)) + WFAC(I) * HUM(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 5)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 5)) + WFAC(I) * TEMP(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 6)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 6)) + WFAC(I) * PRES(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 7)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 7)) + WFAC(I) * SUN(I)

   50     CONTINUE
c
c         wind ricthing niet middelen
c
          PMSA(IP(MAXSTA*(MAXVAR+2) + NP +3)) = DIR(INEAR)

      ENDIF

      ENDIF

C
      DO 60 I=1,(MAXSTA+1)* MAXVAR + MAXSTA*2 + NP
          IP(I) = IP(I) + INCREM (I)
   60 CONTINUE

C
 9000 CONTINUE
C
      RETURN
C
      END
