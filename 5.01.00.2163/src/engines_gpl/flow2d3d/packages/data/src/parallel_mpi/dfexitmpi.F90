subroutine dfexitmpi ( iexit )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: dfexitmpi.F90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfexitmpi.F90 $
!!--description-----------------------------------------------------------------
!
!   Exit or abort parallel application
!
!!--pseudo code and references--------------------------------------------------
!
!   if MPI has been initialized
!      synchronize nodes
!      if error
!         abort MPI
!      else
!         close MPI
!
!
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    !
    implicit none
!
! Global variables
!
    integer :: iexit          ! exit return value
!
! Local variables
!
    integer :: ierr           ! error value of MPI call
    logical :: mpi_is_initialized       ! if true, parallel process is carried out with MPI
!
!! executable statements -------------------------------------------------------
!
#if defined (DFMPI)
    call mpi_initialized ( mpi_is_initialized, ierr )
    if ( mpi_is_initialized ) then

       call mpi_barrier ( MPI_COMM_WORLD, ierr )

       if ( iexit /= 0 ) then

       ! in case of an error abort all MPI processes

          call mpi_abort ( MPI_COMM_WORLD, iexit, ierr )

       else

       ! otherwise stop MPI operations on this computer

          call mpi_finalize ( ierr )

       endif

    endif
#endif

end subroutine dfexitmpi
