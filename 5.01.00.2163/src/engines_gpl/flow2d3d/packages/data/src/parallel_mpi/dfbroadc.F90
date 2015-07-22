subroutine dfbroadc ( iptr, ilen, itype, gdp )
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
!  $Id: dfbroadc.F90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfbroadc.F90 $
!!--description-----------------------------------------------------------------
!
!   Broadcasts data from the master to all other processes
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for MPI_BCAST
!
!
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, pointer       :: lundia
    integer, intent(inout) :: iptr  ! pointer to first element of array to be sent
    integer                :: ilen  ! length of array to be sent
    integer, intent(inout) :: itype ! type of data
!
! Local variables
!
    integer       :: ierr   ! error value of MPI call
    character(80) :: msgstr ! string to pass message
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
#if defined (DFMPI)
    call mpi_bcast ( iptr, ilen, itype, master-1, MPI_COMM_WORLD, ierr )
    if ( ierr /= MPI_SUCCESS ) then
       write (msgstr,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif
#endif

end subroutine dfbroadc
