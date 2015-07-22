subroutine checklicense(success)
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
!  $Id: checklicense.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/wave/src/checklicense.f90 $
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
implicit none
!
! Global variables
!
logical     , intent(out) :: success
!
! Local variables
!
    character(80)     :: txtfil
    character(256)    :: version_full  !  Version nr. of the module of the current package
!
!! executable statements -------------------------------------------------------
!
    txtfil        = '--------------------------------------------------------------------------------'
    version_full  = ' '
    call getfullversionstring_WAVE(version_full)
    write (*, '(a)') txtfil
    write (*, '(a)') '-  Delft3D'
    write (*, '(2a)') '-  ', trim(version_full)
    write (*, '(a)') '-  Open source'
    write (*, '(a)') '-'
    write (*, '(a)') txtfil
    success = .true.
end subroutine checklicense
