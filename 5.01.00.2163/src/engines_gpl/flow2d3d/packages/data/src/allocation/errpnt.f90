subroutine errpnt(pntnam    ,soort     ,callty    ,gdp       )
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
!  $Id: errpnt.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/allocation/errpnt.f90 $
!!--description-----------------------------------------------------------------
!
!   ERRPNT is called by the GT?PNT/MK?PNT functions to print
!   an error message and terminate the program (process/thread).
!   An attempt is made to write to the diagnostic file.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    character(*), intent(in)       :: callty !!  Type of call 'getprt' or 'makptr'
    character(*), intent(in)       :: pntnam !!  Character string containing array name (hence max 6 characters).
    character(*), intent(in)       :: soort  !!  Character string containing type of pointer to be made
!
! Local variables
!
    integer                        :: lundia ! Unit number of diagnostic file =0 No sub-system defined or diagnostic file is not open 
!
!! executable statements -------------------------------------------------------
!
    call getlun(lundia, gdp)
    if (lundia /= 0) then
       write (lundia, *) '*** ERROR in ', soort, ' array ', callty, ' of ', pntnam
       write (lundia, *) '          Contact Deltares'
    endif
    write (*, *) '*** ERROR in ', soort, ' array ', callty, ' of ', pntnam
    write (*, *) '          Contact Deltares'
    call d3stop(1, gdp)
end subroutine errpnt
