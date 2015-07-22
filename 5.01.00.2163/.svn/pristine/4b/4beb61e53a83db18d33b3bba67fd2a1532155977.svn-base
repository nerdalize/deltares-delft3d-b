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

      SUBROUTINE CLCRAD ( PMSA   , FL     , IPOINT , INCREM , NOSEG  ,
     +                    NOFLUX , IEXPNT , IKNMRK , NOQ1   , NOQ2   ,
     +                    NOQ3   , NOQ4   )
C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     +----------------------------------------+
C***********************************************************************
C
C     Project : KSENOS T1236.03
C     Author  : M. Bokhorst
C     Date    : 21-12-94           Version : 0.1
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     211294  M. Bokhorst     First version
C     111295  M. Bokhorst     Added radiation at the bottom of a segment
C     090497  M. Bokhorst     Update new DELWAQ bottom
C                             Project pilot GEM (T2087)
C                             - evaluation extra first feature
C                               = 2: active bottom segment)
C   20000419  Jan van Beek    Controleren op dummy uitwisselingen ter
C                             voorkoming van array overschrijdingen
C   20031107  Jan van Beek    GEM - delwaq-g en drying flooding
C***********************************************************************
C
C     Function : Calculates the radiation at the surface and at the bottom of the
C                active water segments
C
C***********************************************************************

      USE BottomSet     !  Module with definition of the waterbottom segments

      IMPLICIT NONE

C     arguments

      REAL               :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL               :: FL(*)              ! in/out flux array
      INTEGER            :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
      INTEGER            :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
      INTEGER            :: NOSEG              ! in     number of segments
      INTEGER            :: NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER            :: IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER            :: IKNMRK(*)          ! in     segment features array
      INTEGER            :: NOQ1               ! in     number of exchanges in first direction
      INTEGER            :: NOQ2               ! in     number of exchanges in second direction
      INTEGER            :: NOQ3               ! in     number of exchanges in third direction
      INTEGER            :: NOQ4               ! in     number of exchanges in fourth direction

C     from PMSA array

      REAL               :: EXTVL              ! 1  in  total extinction coefficient visible light   (1/m)
      REAL               :: DEPTH              ! 2  in  depth of segment                               (m)
      REAL               :: RADSURF            ! 3  in  irradiation at the water surface            (W/m2)
      REAL               :: A_ENH              ! 4  in  enhancement factor in radiation calculation    (-)
      REAL               :: SURF               ! 5  in  horizontal surface                            (m2)
      INTEGER            :: SWEMERSION         ! 6  in  switch indicating submersion(0) or emersion (1)(-)
      REAL               :: RADBOT             ! 7  loc/out 9 irradiation at the segment lower-boundary   (W/m2)
      REAL               :: RAD                ! 8  out irradiation at the segment upper-boundary   (W/m2)

C     local decalrations

      INTEGER            :: IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IP9 ! index pointers in PMSA array
      INTEGER            :: IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9 ! increments in PMSA array
      INTEGER            :: ISEG           ! loop counter segment loop
      INTEGER            :: IKMRK1         ! first feature inactive(0)-active(1)-bottom(2) segment
      INTEGER            :: IK1VN          ! first feature inactive(0)-active(1)-bottom(2) VAN segment
      INTEGER            :: IK1NR          ! first feature inactive(0)-active(1)-bottom(2) NAAR segment
      INTEGER            :: IKMRK2         ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) segment
      INTEGER            :: IK2VN          ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) VAN segment
      INTEGER            :: IK2NR          ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) NAAR segment
      INTEGER            :: IK             ! loop counter bottom columns
      INTEGER            :: IQ             ! loop counter exchanges
      INTEGER            :: IVAN           ! segment number from
      INTEGER            :: INAAR          ! segment number to
      INTEGER            :: IWA1           ! index first water exchange
      INTEGER            :: IWA2           ! index last water exchange
      INTEGER            :: ITOP           ! index first bottom exhange
      INTEGER            :: IBOT           ! index last bottom exhange
      INTEGER            :: IWATER         ! segment number water segment
      INTEGER            :: IBODEM         ! segment number bottom segment
      REAL               :: RADTOP         ! radiation at top
      REAL               :: TOTSURF        ! cummulated surface area
      REAL               :: RTMP           ! RTMP


      IP1  = IPOINT(1)
      IP2  = IPOINT(2)
      IP3  = IPOINT(3)
      IP4  = IPOINT(4)
      IP5  = IPOINT(5)
      IP6  = IPOINT(6)
      IP7  = IPOINT(7)
      IP8  = IPOINT(8)
      IP9  = IPOINT(9)

      IN1  = INCREM(1)
      IN2  = INCREM(2)
      IN3  = INCREM(3)
      IN4  = INCREM(4)
      IN5  = INCREM(5)
      IN6  = INCREM(6)
      IN7  = INCREM(7)
      IN8  = INCREM(8)
      IN9  = INCREM(9)

c.....2DH afhandeling

      DO 1000 ISEG=1,NOSEG

         CALL DHKMRK( 1, IKNMRK(ISEG ), IKMRK1 )

c........Segment is inactief
         IF      (IKMRK1 .EQ. 0) THEN

c          RadTop = RadSurf
           PMSA(IP8) = PMSA(IP3)

c          RadBot    = RadSurf
           PMSA(IP9) = PMSA(IP3)

c........Segment is actief watersegment
         ELSE IF (IKMRK1 .EQ. 1) THEN

c          RadTop    = RadSurf
           PMSA(IP8) = PMSA(IP3)

c          RadBot    = RadSurf   * EXP( -ExtVl    *Depth     )
           PMSA(IP9) = PMSA(IP3) * EXP( -PMSA(IP1)*PMSA(IP2) )

c........Segment is actief bodemsegment
         ELSE IF (IKMRK1 .EQ. 3) THEN

c          RadTop    = 0.0
           PMSA(IP8) = 0.0

c          RadBot    = 0.0
           PMSA(IP9) = 0.0

         ENDIF

         IP1  = IP1  + IN1
         IP2  = IP2  + IN2
         IP3  = IP3  + IN3
         IP4  = IP4  + IN4
         IP5  = IP5  + IN5
         IP6  = IP6  + IN6
         IP8  = IP8  + IN8
         IP9  = IP9  + IN9

 1000 CONTINUE


c.....3D afhandeling

c.....Waterkolom

      IP1  = IPOINT(1)
      IP2  = IPOINT(2)
      IP3  = IPOINT(3)
      IP4  = IPOINT(4)
      IP5  = IPOINT(5)
      IP6  = IPOINT(6)
      IP8  = IPOINT(8)
      IP9  = IPOINT(9)

      DO 2000 IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3

         IVAN  = IEXPNT(1,IQ)
         INAAR = IEXPNT(2,IQ)

         IF ( IVAN .GT. 0 .AND. INAAR .GT. 0 ) THEN
            CALL DHKMRK( 1, IKNMRK(IVAN ), IK1VN )
            CALL DHKMRK( 1, IKNMRK(INAAR), IK1NR )
            CALL DHKMRK( 2, IKNMRK(IVAN ), IK2VN )
            CALL DHKMRK( 2, IKNMRK(INAAR), IK2NR )

c...........Van segment = inactief
            IF ( IK1VN .EQ. 0 ) THEN

c              RadTop = RadSurf
               PMSA(IP8 + (IVAN-1)  * IN8) = PMSA( IP3 + (IVAN-1)*IN3 )

c              RadBot = Radsurf
               PMSA(IP9 + (IVAN-1)  * IN9) = PMSA( IP3 + (IVAN-1)*IN3 )

c...........Van segment = actief water segment
            ELSE IF (IK1VN .EQ. 1) THEN

c..............Van segment = water segment met surface
               IF ( IK2VN .EQ. 1 ) THEN

                  EXTVL  = PMSA( IP1 + (IVAN-1) * IN1 )
                  DEPTH  = PMSA( IP2 + (IVAN-1) * IN2 )
                  RADTOP = PMSA( IP3 + (IVAN-1) * IN3 )

                  RADBOT = RADTOP * EXP( -EXTVL * DEPTH )

                  PMSA(IP8 + (INAAR-1) * IN8) = RADBOT
                  PMSA(IP9 + (IVAN -1) * IN9) = RADBOT

               ENDIF

c..............Van segment = water segment zonder surface of bodem
               IF ( IK2VN .EQ. 2 ) THEN

                  EXTVL  = PMSA( IP1 + (IVAN -1) * IN1 )
                  DEPTH  = PMSA( IP2 + (IVAN -1) * IN2 )
                  RADTOP = PMSA( IP8 + (IVAN -1) * IN8 )

                  RADBOT = RADTOP * EXP( -EXTVL * DEPTH )

                  PMSA(IP8 + (INAAR-1) * IN8) = RADBOT
                  PMSA(IP9 + (IVAN -1) * IN9) = RADBOT

               ENDIF

            ENDIF

c...........Naar segment = inactief
            IF ( IK1NR .EQ. 0 ) THEN

c              RadTop = RadSurf
               PMSA(IP8 + (INAAR-1) * IN8) = PMSA( IP3 + (INAAR-1)*IN3 )

c              RadBot = Radsurf
               PMSA(IP9 + (INAAR-1) * IN9) = PMSA( IP3 + (INAAR-1)*IN3 )

c...........Naar segment = actief water segment
            ELSE IF (IK1NR .EQ. 1) THEN

c...........Naar segment = water segment met bodem
               IF ( IK2NR .EQ. 3 ) THEN

                  EXTVL  = PMSA( IP1 + (INAAR-1) * IN1 )
                  DEPTH  = PMSA( IP2 + (INAAR-1) * IN2 )
                  RADTOP = PMSA( IP8 + (INAAR-1) * IN8 )

                  RADBOT = RADTOP * EXP( -EXTVL * DEPTH )

                  PMSA(IP9 + (INAAR-1) * IN9) = RADBOT

               ENDIF

            ENDIF
         ENDIF

 2000 CONTINUE


C     the sediment columns

      DO IK = 1 , Coll%cursize

          IWA1 = Coll%set(IK)%fstwatsed
          IWA2 = Coll%set(IK)%lstwatsed
          ITOP = Coll%set(IK)%topsedsed
          IBOT = Coll%set(IK)%botsedsed

C         average RAD at water-sediment interface

          RADTOP  = 0.0
          TOTSURF = 0.0
          DO IQ = IWA1,IWA2
             IWATER  = IEXPNT(1,IQ)
             IBODEM  = IEXPNT(2,IQ)
             RADSURF    =      PMSA(IP3+(IWATER-1)*IN3)
             SURF       =      PMSA(IP5+(IWATER-1)*IN5)
             SWEMERSION = NINT(PMSA(IP6+(IWATER-1)*IN6))
             RADBOT     =      PMSA(IP9+(IWATER-1)*IN9)
             IF ( SWEMERSION .EQ. 1 ) THEN
                RADTOP = RADTOP + RADSURF*SURF
             ELSE
                RADTOP = RADTOP + RADBOT*SURF
             ENDIF
             TOTSURF = TOTSURF + SURF
          ENDDO
          A_ENH  = PMSA(IP4+(IWATER-1)*IN4)
          RADTOP = RADTOP*A_ENH/TOTSURF

C         extinction over the layers of the column

          DO IQ = ITOP,IBOT
              IBODEM = IEXPNT(1,IQ)
              EXTVL  = PMSA(IP1+(IBODEM-1)*IN1)
              DEPTH  = PMSA(IP2+(IBODEM-1)*IN2)
              IF ( RADTOP .LT. 1.E-10 ) THEN
                 RADBOT = 0.0
              ELSE
                 RADBOT = RADTOP * EXP( -EXTVL * DEPTH )
              ENDIF
              PMSA(IP8+(IBODEM-1)*IN8) = RADTOP
              PMSA(IP9+(IBODEM-1)*IN9) = RADBOT
              RADTOP = RADBOT
          ENDDO

      ENDDO

      RETURN
      END
