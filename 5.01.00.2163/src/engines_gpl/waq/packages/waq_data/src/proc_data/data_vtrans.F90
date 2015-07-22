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

      MODULE DATA_VTRANS

!     results and local storage for the VTRANS routine

      INTEGER, SAVE ::  NOLAYLOCAL, NOSEGLOCAL
      REAL   , SAVE ::  TIMTOT
      LOGICAL           ACTIVE_VTRANS                 ! switch indicating if VTRANS functionality is active
      LOGICAL           RESET_VTRANS                  ! switch indicating if new distribution step is needed
      LOGICAL           INIT_VTRANS                   ! switch indicating if VTRANS is initialised
      SAVE              ACTIVE_VTRANS
      SAVE              RESET_VTRANS
      SAVE              INIT_VTRANS
      DATA              ACTIVE_VTRANS  /.FALSE./
      DATA              RESET_VTRANS   /.TRUE./
      DATA              INIT_VTRANS    /.FALSE./

      REAL, ALLOCATABLE, SAVE :: CONCV(:,:)           ! calculated concentration distribution per layer
      REAL, ALLOCATABLE, SAVE :: TIMEV(:,:)           ! accumulated time per layer
      REAL, ALLOCATABLE, SAVE :: FRACV(:,:)           ! fraction of the time per layer, updated every accumulating step
      REAL, ALLOCATABLE, SAVE :: DERVV(:,:)           ! help array

      END MODULE
