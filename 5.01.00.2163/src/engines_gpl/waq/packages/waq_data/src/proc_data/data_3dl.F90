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

      MODULE DATA_3DL

      INTEGER, SAVE ::  NOSEG_3DL                 ! number of segments, copy of NOSEG
      INTEGER, SAVE ::  NOSEGL_3DL                ! number of segments per layer
      INTEGER, SAVE ::  NOLAY_3DL                 ! number of layers
      INTEGER, SAVE ::  NGRO_3DL                  ! number of BLOOM algae groups, copy of NGRO_A
      INTEGER, SAVE ::  NTYP_3DL                  ! number of BLOOM algae types, copy of NTYP_A
      INTEGER, SAVE ::  ISEG_3DL                  ! actual segment for which bloom is called
      INTEGER, SAVE ::  ILAY_3DL                  ! actual layer for which bloom is called
      LOGICAL, SAVE ::  ACTIVE_3DL                ! switch indicating if 3DL functionality is active

      REAL, ALLOCATABLE, SAVE :: RADSURF_3DL(:)   ! radiation at segment surface, is updated with actual extinction from top to bottom after BLOOM call
      REAL, ALLOCATABLE, SAVE :: EFFIC_3DL(:,:)   ! efficiency per algae group, is using total extinction
      REAL, ALLOCATABLE, SAVE :: IFIX_3DL(:)      ! copy of the IFIX array, indication if alg is fixed

      CONTAINS

      SUBROUTINE INIT_3DL( NOSEG , NOSEGW, NOSEGL, NOLAY , NGRO  , NTYP  )

!     FUNCTION : set dimensions and allocate memory for DATA_3DL

!     subroutines called

!     GETMLU, get the untit number of the report file
!     SRSTOP, stops execution

!     arguments

      INTEGER  NOSEG      ! input, total number of segments
      INTEGER  NOSEGW     ! input, number of segments in the water phase
      INTEGER  NOSEGL     ! input, number of segments per layer
      INTEGER  NOLAY      ! input, number of layers
      INTEGER  NGRO       ! input, number of BLOOM algae groups
      INTEGER  NTYP       ! input, number of BLOOM algae types

!     local decalarations

      INTEGER  IERR_ALLOC ! error number memory allocation
      INTEGER  LUNREP     ! unit number report file

      NOSEG_3DL  = NOSEG
      NOSEGL_3DL = NOSEGL
      NOLAY_3DL  = NOLAY
      NGRO_3DL   = NGRO
      NTYP_3DL   = NTYP

      ALLOCATE ( RADSURF_3DL(NOSEG)      ,&
                 EFFIC_3DL(NTYP,NOSEG)   ,&
                 STAT = IERR_ALLOC     )
      IF ( IERR_ALLOC .NE. 0 ) THEN
         CALL GETMLU(LUNREP)
         WRITE ( LUNREP , 1000 ) IERR_ALLOC
         WRITE ( LUNREP , 1001 ) NOSEG
         WRITE ( LUNREP , 1002 ) NTYP
         CALL SRSTOP(1)
      ENDIF
      EFFIC_3DL    = 0.0

      RETURN
 1000 FORMAT(' ERROR: allocating memory in INIT_3DL:',I10)
 1001 FORMAT(' NOSEG, number of segments           :',I10)
 1002 FORMAT(' NTYP , number of BLOOM  algae types :',I10)
      END SUBROUTINE INIT_3DL

      SUBROUTINE EMIN_3DL( EMIN  , EMINLAY, ITYPE  )

!     FUNCTION : Gives minimal effeiciency for current layer given a total minimal efficiency

!     use the results from the vertical distribution VTRANS

      USE      DATA_VTRANS

!     arguments

      REAL*8   EMIN       ! input , total minimal efficiency
      REAL*8   EMINLAY    ! output, minmal efficiency for this layer
      INTEGER  ITYPE      ! input , index number of BLOOM algae type

!     local decalarations

      INTEGER  ILAY       ! layer counter
      INTEGER  ISEG       ! segment number
      REAL*8   ELAYS      ! efficiency in other layers
      REAL*8   FLAY       ! time factor in a specific layer
      REAL*8   ELAY       ! efficiency in a specific layer
      REAL*8   EFFI       ! efficiency in all layers

!     check if active

      IF ( .NOT. ACTIVE_3DL ) THEN

!        if not active then EMINLAY = EMIN

         EMINLAY = EMIN

      ELSE

!        accumulate efficiencies in other layers ELAY * the time fraction per layer FLAY

         ELAYS = 0.0
         DO ILAY = 1 , NOLAY_3DL
            IF ( ILAY .NE. ILAY_3DL ) THEN
               ISEG  = (ILAY-ILAY_3DL)*NOSEGL_3DL + ISEG_3DL
               FLAY  = FRACV(ILAY,ISEG_3DL)
               ELAY  = EFFIC_3DL(ITYPE,ISEG)
               ELAYS = ELAYS + FLAY*ELAY
            ENDIF
         ENDDO

!        compute minimal efficiency for this layer

         FLAY    = FRACV(ILAY_3DL,ISEG_3DL)
         EFFI  = ELAYS  + EFFIC_3DL(ITYPE,ISEG_3DL) * FLAY
         IF ( FLAY .GT. 1.E-20 ) THEN
            EMINLAY = MAX((EMIN-ELAYS)/FLAY,0.01*EMIN)
         ELSE
            EMINLAY = EMIN
         ENDIF

      ENDIF

      RETURN
      END SUBROUTINE EMIN_3DL

      SUBROUTINE EFFI_3DL( EFFI , ITYPE )

!     FUNCTION : Gives average effeiciency over the layers

!     use the results from the vertical distribution VTRANS

      USE      DATA_VTRANS

!     arguments

      REAL*8   EFFI       ! output, average effieciency
      INTEGER  ITYPE      ! input , index number of BLOOM algae type

!     local decalarations

      INTEGER  ILAY       ! layer counter
      INTEGER  ISEG       ! segment number
      REAL*8   FLAY       ! time factor in a specific layer
      REAL*8   ELAY       ! efficiency in a specific layer

!     check if active

      IF ( .NOT. ACTIVE_3DL ) THEN

!        just take efficiency for this layer

         EFFI = EFFIC_3DL(ITYPE,ISEG_3DL)

      ELSE

!        accumulate efficiencies over the layers ELAY * the time fraction per layer FLAY

         EFFI = 0.0
         DO ILAY = 1 , NOLAY_3DL
            ISEG  = (ILAY-ILAY_3DL)*NOSEGL_3DL + ISEG_3DL
            FLAY  = FRACV(ILAY,ISEG_3DL)
            ELAY  = EFFIC_3DL(ITYPE,ISEG)
            EFFI  = EFFI + FLAY*ELAY
         ENDDO
         EFFI = EFFI

      ENDIF

      RETURN
      END SUBROUTINE EFFI_3DL

      SUBROUTINE EFFILAY_3DL( SURF, EXTTOT, DEP   , IGROUP, ITYPE )

!     FUNCTION : calculate and store efficiency for this layer

!     arguments

      REAL*8   SURF       ! input , corrected irradiation
      REAL*8   EXTTOT     ! input , total extinction
      REAL*8   DEP        ! input , depth of the layer
      INTEGER  IGROUP     ! input , index number of BLOOM algae group
      INTEGER  ITYPE      ! input , index number of BLOOM algae type

!     local decalarations

      REAL*8   PHI_S      ! x value tabulated function at surface
      REAL*8   FUN_S      ! function at surface
      REAL*8   DER_S      ! derivative at sutface
      REAL*8   PHI_D      ! x value tabulated function at dep
      REAL*8   FUN_D      ! function at surface at dep
      REAL*8   DER_D      ! derivative at sutface at dep
      REAL*8   EFFI       ! calculated efficiency

      IF ( SURF .GT. 1.0 ) THEN
         PHI_S = - DLOG(SURF)
         CALL EBCALC(PHI_S,FUN_S,DER_S,IGROUP)
         PHI_D = EXTTOT*DEP - DLOG(SURF)
         CALL EBCALC(PHI_D,FUN_D,DER_D,IGROUP)
         EFFI   = (FUN_D-FUN_S)/EXTTOT/DEP
         EFFI = MAX(EFFI,0.0)
      ELSE
         EFFI = 0.0
      ENDIF

!     store for later use

      EFFIC_3DL(ITYPE,ISEG_3DL) = EFFI

      RETURN
      END SUBROUTINE EFFILAY_3DL

      END MODULE DATA_3DL
