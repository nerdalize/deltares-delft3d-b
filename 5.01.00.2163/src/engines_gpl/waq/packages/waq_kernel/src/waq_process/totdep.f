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

      subroutine totdep ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Total depth water column

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    Water Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : T1519 Clyde river estuary
C     Author  : M. Bokhorst
C     Date    : 13-1-95            Version : 0.1
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     13-1-95 M. Bokhorst     Create first version
C     29-8-97 Jan van Beek    Uitvoer locale diepte toegevoegd
C     17-3-98 Jos van Gils    Define TOTDEPTH also for 2D situation
C     16-6-98 Jos van Gils    Beveiliging voor randen in 3-e richting
C                             Beveiliging voor bodem in 3-e richting
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                  Units
C ----    --- -  -    -------------------                          -----
C
C DEPTH               segment diepte
C TDEPTH              totale diepte ( van surf tot bottom )
C LDEPTH              locale diepte ( = van surf tot onderkant segment )

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      USE BottomSet     !  Module with definition of the waterbottom segments

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      REAL               :: DEPTH              ! 1  in  depth of segment                                     (m)
      REAL               :: SURF               ! 2  in horizontal surface area                              (m2)
      REAL               :: TOTALDEPTH         ! 3  out total depth water column                             (m)
      REAL               :: LOCALDEPTH         ! 4  out depth from water surface to bottom of segment        (m)
      REAL               :: LOCSEDDEPT         ! 5  out Sediment layer depth to bottom of segment            (m)
      REAL               :: TOTSEDDEPT         ! 2  out Sediment layer depth to bottom of sediment column    (m)

      INTEGER  IP1   , IP2   , IP3   , IP4   , IP5
      INTEGER  IN1   , IN2   , IN3   , IN4   , IN5
      INTEGER  IKMRK , IKMRK1, IKMRK2, ISEG  , IFROM , ITO
      INTEGER            :: IK                 ! loop counter bottom columns
      INTEGER            :: IQ                 ! loop counter exchanges
      INTEGER            :: IWA1               ! index first water exchange
      INTEGER            :: IWA2               ! index last water exchange
      INTEGER            :: ITOP               ! index first bottom exhange
      INTEGER            :: IBOT               ! index last bottom exhange
      INTEGER            :: IBODEM             ! segment number bottom segment
      INTEGER            :: IWATER             ! segment number water segment
      REAL               :: CUMTOTDEPTH        ! cummulative in averaging totaldepth
      REAL               :: TOTSURF            ! cummulative surf in averaging totaldepth


C     initialise bottom if necessary

      CALL MAKKO2 ( IEXPNT , IKNMRK , NOQ1   , NOQ2   , NOQ3   ,
     +              NOQ4   )

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)

c.....Zet de totale en lokale diepte initieel op de diepte
c.....voor actieve watersegmenten, anders 0
c.....zet sediment dikte to onderkant segment op 0
      DO 9000 ISEG=1,NOSEG

         PMSA(IP3) = PMSA(IP1)
         PMSA(IP4) = PMSA(IP1)
         PMSA(IP5) = PMSA(IP1)

         IP1 = IP1 + IN1
         IP3 = IP3 + IN3
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5

 9000 CONTINUE

      IP1  = IPOINT( 1)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)

c.....Exchange-loop over de derde richting
      DO 7000 IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3

         IFROM = IEXPNT(1,IQ)
         ITO   = IEXPNT(2,IQ)

         IF ( IFROM.GT.0 .AND. ITO.GT.0 ) THEN

C           CALL DHKMRK(1,IKNMRK(IFROM),IKMRK1)
C           CALL DHKMRK(1,IKNMRK(ITO  ),IKMRK2)
C           IF ( IKMRK1.EQ.1 .AND. IKMRK2.EQ.1 ) THEN

               CALL DHKMRK(2,IKNMRK(IFROM),IKMRK)
               IF ((IKMRK.EQ.0).OR.(IKMRK.EQ.1)) THEN

                  PMSA ( IP3 + (IFROM-1) * IN3 ) =
     +            PMSA ( IP1 + (IFROM-1) * IN1 )
                  PMSA ( IP4 + (IFROM-1) * IN4 ) =
     +            PMSA ( IP1 + (IFROM-1) * IN1 )
                  PMSA ( IP5 + (IFROM-1) * IN5 ) =
     +            PMSA ( IP4 + (IFROM-1) * IN4 )

                  PMSA ( IP3 + (ITO  -1) * IN3 ) =
     +            PMSA ( IP1 + (IFROM-1) * IN1 ) +
     +            PMSA ( IP1 + (ITO  -1) * IN1 )
                  PMSA ( IP4 + (ITO  -1) * IN4 ) =
     +            PMSA ( IP1 + (IFROM-1) * IN1 ) +
     +            PMSA ( IP1 + (ITO  -1) * IN1 )
                  PMSA ( IP5 + (ITO  -1) * IN5 ) =
     +            PMSA ( IP4 + (ITO  -1) * IN4 )

               ELSE

                  PMSA ( IP3 + (ITO  -1) * IN3 ) =
     +            PMSA ( IP3 + (IFROM-1) * IN3 ) +
     +            PMSA ( IP1 + (ITO  -1) * IN1 )
                  PMSA ( IP4 + (ITO  -1) * IN4 ) =
     +            PMSA ( IP4 + (IFROM-1) * IN4 ) +
     +            PMSA ( IP1 + (ITO  -1) * IN1 )
                  PMSA ( IP5 + (ITO  -1) * IN5 ) =
     +            PMSA ( IP4 + (ITO  -1) * IN4 )

               ENDIF
C           ENDIF
         ENDIF

 7000 CONTINUE


c.....Exchange-loop over de derde richting
      DO 8000 IQ =  NOQ1+NOQ2+NOQ3, NOQ1+NOQ2+1,-1

         IFROM = IEXPNT(1,IQ)
         ITO   = IEXPNT(2,IQ)

c........Berekende totale dieptes voor de onderste laag segmenten
c        toekennen aan de bovenliggende segmenten

         IF ( IFROM.GT.0 .AND. ITO.GT.0 )

     +   PMSA ( IP3 + (IFROM-1) * IN3 ) =
     +   PMSA ( IP3 + (ITO  -1) * IN3 )

 8000 CONTINUE

C     loop over the sediment columns, set sediment depth

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)

      DO IK = 1 , Coll%cursize

         IWA1 = Coll%set(IK)%fstwatsed
         IWA2 = Coll%set(IK)%lstwatsed
         ITOP = Coll%set(IK)%topsedsed
         IBOT = Coll%set(IK)%botsedsed

         ! make average totaldepth water

         CUMTOTDEPTH = 0.0
         TOTSURF     = 0.0
         DO IQ = IWA1,IWA2
            IWATER  = IEXPNT(1,IQ)
            TOTALDEPTH  = PMSA(IP3+(IWATER-1)*IN3)
            SURF        = PMSA(IP2+(IWATER-1)*IN2)
            CUMTOTDEPTH = CUMTOTDEPTH + TOTALDEPTH*SURF
            TOTSURF     = TOTSURF + SURF
         ENDDO
         IF ( TOTSURF .GT. 1E-20 ) THEN
            TOTALDEPTH = CUMTOTDEPTH/TOTSURF
         ELSE
            TOTALDEPTH = 0.0
         ENDIF

         ! accumulate depth within bottom

         LOCALDEPTH = TOTALDEPTH
         LOCSEDDEPT = 0.0
         DO IQ = ITOP,IBOT
            IBODEM  = IEXPNT(1,IQ)
            DEPTH   = PMSA(IP1+(IBODEM-1)*IN1)
            LOCALDEPTH = LOCALDEPTH + DEPTH
            LOCSEDDEPT = LOCSEDDEPT + DEPTH
            PMSA(IP4+(IBODEM-1)*IN4) = LOCALDEPTH
            PMSA(IP5+(IBODEM-1)*IN5) = LOCSEDDEPT
         ENDDO

         ! final is total copy back in the column

         TOTSEDDEPT = LOCSEDDEPT
         DO IQ = ITOP,IBOT
            IBODEM  = IEXPNT(1,IQ)
            PMSA(IP3+(IBODEM-1)*IN3) = TOTSEDDEPT
         ENDDO

      ENDDO

      RETURN
      END
