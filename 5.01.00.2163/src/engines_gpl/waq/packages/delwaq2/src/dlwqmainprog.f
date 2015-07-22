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

      PROGRAM DLWQ2
          USE DELWAQ2_DATA

          IMPLICIT NONE
          include 'actions.inc'

          CHARACTER(LEN=20), DIMENSION(10) :: ARGV
          INTEGER                          :: ACTION
          INTEGER                          :: ARGC
          INTEGER                          :: I

          TYPE(DELWAQ_DATA)                :: DLWQD
      real :: a1, b1
      a1=1e-20
      b1=1e-20
      a1=a1*b1

          ARGC = 0
          ARGV = '   '

!!        ACTION = ACTION_FULLCOMPUTATION
!!
!
!         Remarks:
!         * This loop makes clear that the start and stop times must be
!           exposed to this level (or even to the main program)
!         * We need to do the command-line arguments
!         * We need to collect all local variables in a structure
!         * We need to identify which variables must be kept between calls
!           Done for:
!           dlwqmain.f
!           delwaq2.f
!         * We need to eliminate ESM/FSM
!         * Move all data in COMMON blocks to the structure
!         * Variables like MYPART and NPART must be checked!
!         * Timers must be properly initialised ...
!
!         Steps:
!         * Introduce a ACTION variable: relates the state of the
!           computations to the tasks that must be done
!           Now in: dlwqmain, delwq2, dlwqn1
!         * Identify the variables that should retain their value
!           between calls (should be moved to a derived type
!           variable that will get passed as an argument)
!         * Eliminate FSM - done
!         * Rationalise the actions and related tasks - done
!         * Create derived type - done
!         * TODO: restore ONLINE() - must not use RBUF ... directly
!
!         Tests:
!         * Integration option 1 - wq-2d-case
!         * Does the new program give the same results as the current
!           official one?
!         * First done with ACTION_FULLCOMPUTATION, now step by step
!
          ACTION = ACTION_FULLCOMPUTATION
          CALL DLWQMAIN( ACTION, ARGC, ARGV, DLWQD )

      END PROGRAM
