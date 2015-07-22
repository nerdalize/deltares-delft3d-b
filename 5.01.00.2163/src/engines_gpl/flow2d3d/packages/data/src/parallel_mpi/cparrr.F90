subroutine cparrr ( iarr1, iarr2, length, gdp )
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
!  $Id: cparrr.F90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/cparrr.F90 $
!!--description-----------------------------------------------------------------
!
!   Copies real array IARR1 to IARR2
!
!!--pseudo code and references--------------------------------------------------
!
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, pointer                          :: lundia
    integer                      , intent(in)  :: length ! array length
    real(fp), dimension(1:length), intent(in)  :: iarr1  ! source array
    real(fp), dimension(1:length), intent(out) :: iarr2  ! target array
!
! Local variables
!
    integer :: i ! loop counter
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    !
    ! check array length
    !
    if ( length <= 0 ) then
       call prterr(lundia, 'U021', 'Array length should be positive')
    endif
    !
    ! copy elements of array IARR1 to IARR2
    !
    do i = 1, length
       iarr2(i) = iarr1(i)
    enddo

end subroutine cparrr
