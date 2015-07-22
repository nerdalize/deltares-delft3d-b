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

!> @file
!!    Run the computation step by step
!!

!> Main program steering the computation
program delwaq2_stepwise

    implicit none

    integer :: dummy
    integer :: ModelInitialize_By_Id, ModelPerformTimeStep, ModelFinalize
    integer :: GetTimeHorizon, GetWQCurrentTime

    character(len=200)     :: runid
    real(kind=kind(1.0d0)) :: startTime, stopTime, currentTime
    integer                :: status

    !
    ! Get the run-ID or quit
    !
    call get_command_argument( 1, runid, status )

    if ( runid == ' ' ) then
        write(*,*) 'Please specify the run-ID!'
        stop
    endif

    !
    ! Start the computation - stepwise
    !
    dummy = ModelInitialize_By_Id( runid )

    dummy = ModelInitialize_By_Id( runid ) ! Twice as a test

    dummy = GetTimeHorizon( startTime, stopTime )

    do
        dummy = ModelPerformTimeStep()
        dummy = GetWQCurrentTime( currentTime )
        if ( currentTime >= stopTime ) then
            dummy = ModelPerformTimeStep()
            exit
        endif
    enddo

    !
    ! Properly complete the computation
    !
    dummy = ModelFinalize()

end program delwaq2_stepwise
