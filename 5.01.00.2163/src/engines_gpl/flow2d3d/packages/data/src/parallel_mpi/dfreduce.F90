subroutine dfreduce ( iptr, ilen, itype, ityprd, gdp )
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
!  $Id: dfreduce.F90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfreduce.F90 $
!!--description-----------------------------------------------------------------
!
!   Performs a global reduction of type ITYPRD on
!   array IPTR of type ITYPE to collect values from
!   all processes
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for MPI_ALLREDUCE
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
    integer             :: iptr   ! pointer to first element of array to be collect
    integer, intent(in) :: ilen   ! length of array to be collect
    integer, intent(in) :: itype  ! type of data
    integer, intent(in) :: ityprd ! type of reduction
!
! Local variables
!
    integer, pointer :: lundia
    integer       :: ierr   ! error value of MPI call
    integer       :: ioptr  ! pointer to first element of output array
    character(80) :: msgstr ! string to pass message
!
!! executable statements -------------------------------------------------------
!
    ! if not parallel, return
    !
    if (.not.parll) return
    !
    lundia => gdp%gdinout%lundia
    ioptr = 0
    !
#if defined (DFMPI)
    call mpi_allreduce ( iptr, ioptr, ilen, itype, ityprd, MPI_COMM_WORLD, ierr )
    if ( ierr /= MPI_SUCCESS ) then
       write (msgstr,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif
#endif
    !
    if ( itype == dfint ) then
       call cparri ( ioptr, iptr, ilen, gdp )
    else if ( itype == dfreal ) then
       call cparrr ( ioptr, iptr, ilen, gdp )
    else if ( itype == dfdble ) then
       call cparrd ( ioptr, iptr, ilen, gdp )
    endif

end subroutine dfreduce
