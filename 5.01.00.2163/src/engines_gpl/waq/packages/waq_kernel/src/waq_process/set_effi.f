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

      subroutine set_effi( temper, radiat, ext   , depthw, daylen,
     &                     id    )
!>\file
!>       calculate and store efficiency for all species

C     modules

      USE      DATA_3DL   ! data and routine for 3D light approach

C     BLOOM commons

      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'arran.inc'

C     arguments

      REAL     TEMPER     ! input , temperature
      REAL     RADIAT     ! input , radiation
      REAL     EXT        ! input , total extinction
      REAL     DEPTHW     ! input , depth of the layer
      REAL     DAYLEN     ! input , daylength in hours
      INTEGER  ID         ! input , weeknumber

C     local decalarations

      REAL*8   ALPHA      ! reflection factor
      REAL*8   TEMP       ! temperature
      REAL*8   CSOL       ! radiation
      REAL*8   DSOL       ! radiation
      REAL*8   DEP        ! depth
      REAL*8   EXTTOT     ! total extinction
      REAL*8   DAY        ! daylength in hours
      REAL*8   DEAT       ! DEAT
      REAL*8   ZOODD      ! ZOODD
      REAL*8   TCORR      ! TCORR
      REAL*8   SURF_TYP   ! scaled, converted and corrected radiation for a type
      INTEGER  IGROUP     ! index number of BLOOM algae group
      INTEGER  ITYPE      ! index number of BLOOM algae type
      REAL*8   PMAX20(MT),SDMIXN(MT)


      DEP    = DEPTHW
      EXTTOT = EXT
      TEMP   = TEMPER
      CSOL   = SOLACO * RADIAT
      DAY    = DAYLEN
      DEAT   = 0D0
      ZOODD  = 0D0
      CALL NATMOR ( DEAT  , ZOODD , TEMP  , 1)
      DO ITYPE = 1,NTYP_3DL
         IF (SDMIX(ITYPE) .LT. 0.0) THEN
            SDMIXN(ITYPE) = 1.0D0 + SDMIX(ITYPE)
C           DMIX(K) = DABS(SDMIX(ITYPE)) * DEP
         ELSE
            SDMIXN(ITYPE) = 0.0D0
         ENDIF
      ENDDO


      CALL MAXPRD ( TEFCUR )
      DO ITYPE = 1,NTYP_3DL
         PMAX20(ITYPE) = PMAX(ITYPE)
      ENDDO
      CALL MAXPRD ( TEMP  )
      ALPHA=0.95
      IF ((ID .LE. 17) .OR. (ID .GE. 32)) ALPHA=0.94
      IF ((ID .LE. 13) .OR. (ID .GE. 36)) ALPHA=0.92
      IF ((ID .LE.  4) .OR. (ID .GE. 45)) ALPHA=0.90
      IF (ID .GT. 0) CSOL=ALPHA * CSOL
      DSOL=1428.57*CSOL
      DO IGROUP = 1 , NGRO_3DL
         DO ITYPE = IT2(IGROUP,1),IT2(IGROUP,2)
            TCORR      = PMAX20(ITYPE)/PMAX(ITYPE)
            SURF_TYP   = TCORR * DSOL * DEXP (- EXTTOT * SDMIXN(ITYPE) * DEP)
            SURF_TYP   = SURF_TYP/DAY
            CALL EFFILAY_3DL( SURF_TYP, EXTTOT, DEP   , IGROUP, ITYPE )
         ENDDO
      ENDDO

      RETURN
      END SUBROUTINE SET_EFFI
