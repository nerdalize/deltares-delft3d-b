module ec_message
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: ec_message.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_message.f90 $
!!--description-----------------------------------------------------------------
!
! see ec_module.f90
! More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!
!!--pseudo code and references--------------------------------------------------
!
! arjen.markus@deltares.nl
! adri.mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
  use precision
  !
  implicit none
  !
  private
!
! Module parameters
!
  integer, parameter :: maxMessageLen  = 300
!
! Module variables
!
  character(maxMessageLen) :: ECMessage = " "
!
! interfaces
!
  interface setECMessage
    module procedure setECMessage_char
    module procedure setECMessage_int
  end interface
!
! public entities
!
  public :: maxMessageLen
  public :: setECMessage
  public :: getECMessage

contains
!
!==============================================================================
! Message composition routines
!==============================================================================
subroutine setECMessage_char(string, suffix)
  !
  ! arguments
  character(len=*), intent(in)           :: string
  character(len=*), intent(in), optional :: suffix
  !
  ! body
  if (present(suffix)) then
    ECMessage = trim(string) // ' ' // suffix
  else
    ECMessage = string
  endif
end subroutine setECMessage_char
!
!
!==============================================================================
subroutine setECMessage_int(string, value)
  !
  ! arguments
  character(*), intent(in) :: string
  integer     , intent(in) :: value
  !
  ! body
  write(ECMessage, '(a,a,i0)') trim(string), ' ', value
end subroutine setECMessage_int
!
!
!==============================================================================
function getECMessage( ) result(retval)
  !
  ! result
  character(len=len(ECMessage)) :: retval
  !
  ! body
  retval    = trim(ECMessage)
  ECMessage = ' '
end function getECMessage



end module ec_message
