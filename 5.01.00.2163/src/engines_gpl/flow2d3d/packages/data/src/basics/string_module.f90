module string_module
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
!  $Id: string_module.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/basics/string_module.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Various string processing routines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

private

!
! functions and subroutines
!
public string_module_info
public str_lower
public str_upper

contains



! ------------------------------------------------------------------------------
!   Subroutine: string_module_info
!   Purpose:    Add info about this string module to the messages stack
!   Summary:    Add id string and URL
!   Arguments:
!   messages    Stack of messages to add the info to
! ------------------------------------------------------------------------------
subroutine string_module_info(messages)
    use message_module
    !
    type(message_stack), pointer :: messages
    !
    call addmessage(messages,'$Id: string_module.f90 1180 2012-01-13 17:05:48Z mourits $')
    call addmessage(messages,'$URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/basics/string_module.f90 $')
end subroutine string_module_info



! ------------------------------------------------------------------------------
!   Subroutine: str_lower
!   Purpose:    Convert upper case characters to lower case
!   Summary:    Scan string for upper case characters and
!               convert them.
!   Arguments:
!   string      String to be converted
!   lenstr      Length of string to be converted
! ------------------------------------------------------------------------------
subroutine str_lower(string, lenstr)
    implicit none
    !
    ! Call variables
    !
    integer     , optional, intent(in) :: lenstr
    character(*)                       :: string
    !
    ! Local variables
    !
    integer :: i
    integer :: j
    integer :: newlen
    !
    !! executable statements ---------------------------------------------------
    !
    if (present(lenstr)) then
       newlen = min(lenstr, len_trim(string))
    else
       newlen = len_trim(string)
    endif
    do i = 1, newlen
       j = ichar(string(i:i))
       if ((j>64) .and. (j<91)) then
          j = j + 32
          string(i:i) = char(j)
       endif
    enddo
end subroutine str_lower



! ------------------------------------------------------------------------------
!   Subroutine: str_upper
!   Purpose:    Convert lower case characters to upper case
!   Summary:    Scan string for lower case characters and
!               convert them.
!   Arguments:
!   string      String to be converted
!   lenstr      Length of string to be converted
! ------------------------------------------------------------------------------
subroutine str_upper(string, lenstr)
    implicit none
    !
    ! Call variables
    !
    integer     , optional, intent(in) :: lenstr
    character(*)                       :: string
    !
    ! Local variables
    !
    integer :: i
    integer :: j
    integer :: newlen
    !
    !! executable statements ---------------------------------------------------
    !
    if (present(lenstr)) then
       newlen = min(lenstr, len_trim(string))
    else
       newlen = len_trim(string)
    endif
    do i = 1, newlen
       j = ichar(string(i:i))
       if ((j>96) .and. (j<123)) then
          j = j - 32
          string(i:i) = char(j)
       endif
    enddo
end subroutine str_upper

end module string_module
