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

      SUBROUTINE DHAGKM ( NOSEG , NODIM2, NOGRID, IKNMRK, GRDNOS,
     +                    GRDSEG)
C
C     Deltares
C
C     Created             : Oct. 1998 by Jan van Beek
C
C     Function            : Aggregates kenmerk array
C
C     Subroutines called  : DHKMRK, evaluate kenmerk
C
C     Arguments           :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOSEG   INTEGER  1          INPUT   Number of segments
C     NODIM2  INTEGER  1          INPUT   second dimension kenmerk array
C     NOGRID  INTEGER  1          INPUT   number of grids
C     IKNMRK  INTEGER  *          IN/OUT  kenmerk array
C     GRDNOS  INTEGER  *          INPUT   number of grid cells per grid
C     GRDSEG  INTEGER  *          INPUT   segment pointers
C
C     Declaration of arguments
C
      INTEGER        NOSEG , NODIM2, NOGRID
      INTEGER        GRDNOS(NOGRID)
      INTEGER        GRDSEG(NOSEG,NOGRID)
      INTEGER        IKNMRK(NOSEG,NODIM2,NOGRID)
C
C     Local declaration
C
C     IGRID   INTEGER  1          LOCAL   Grid index
C     ISEG    INTEGER  1          LOCAL   Segment index base grid
C     ISEG2   INTEGER  1          LOCAL   Segment index coarser grid
C     K1_G1   INTEGER  1          LOCAL   Kenmerk 1 base grid
C     K1_G2   INTEGER  1          LOCAL   Kenmerk 1 coarser grid
C     K2_G1   INTEGER  1          LOCAL   Kenmerk 2 base grid
C     K2_G2   INTEGER  1          LOCAL   Kenmerk 2 coarser grid
C
      INTEGER        IGRID , ISEG  , ISEG2 , K1_G1 , K1_G2 ,
     +               K2_G1 , K2_G2
C
C     Set kenmerk array for all coarser grids
C
      DO IGRID = 2 , NOGRID
C
C        Set all first kenmerk inactive, all second kenmerk middle ( 20 )
C
         DO ISEG2 = 1 , GRDNOS(IGRID)
            IKNMRK(ISEG2,1,IGRID) = 20
         ENDDO
C
         DO ISEG = 1 , NOSEG
            ISEG2 = GRDSEG(ISEG,IGRID)
C
C           Kenmerk 1 , 0 = inactive , 1 = active , 2 = GEM bottom
C
            CALL DHKMRK(1,IKNMRK(ISEG,1,1)     ,K1_G1)
            CALL DHKMRK(1,IKNMRK(ISEG2,1,IGRID),K1_G2)
            IF ( K1_G1 .GT. 0 ) THEN
               K1_G2 = K1_G1
            ENDIF
C
C           Kenmerk 2 , 0 = depth integrated
C                       1 = surface
C                       2 = middle segment
C                       3 = bottom
C
            CALL DHKMRK(2,IKNMRK(ISEG,1,1)     ,K2_G1)
            CALL DHKMRK(2,IKNMRK(ISEG2,1,IGRID),K2_G2)
            IF ( K2_G1 .EQ. 0 ) THEN
               K2_G2 = 0
            ELSEIF ( K2_G1 .EQ. 1 ) THEN
               IF ( K2_G2 .EQ. 2 ) THEN
                  K2_G2 = 1
               ELSEIF ( K2_G2 .EQ. 3 ) THEN
                  K2_G2 = 0
               ENDIF
            ELSEIF ( K2_G1 .EQ. 3 ) THEN
               IF ( K2_G2 .EQ. 2 ) THEN
                  K2_G2 = 3
               ELSEIF ( K2_G2 .EQ. 1 ) THEN
                  K2_G2 = 0
               ENDIF
            ENDIF
C
            IKNMRK(ISEG2,1,IGRID) = K1_G2 + 10*K2_G2
C
         ENDDO
      ENDDO
C
      RETURN
      END
