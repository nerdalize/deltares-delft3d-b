subroutine noendspaces(name      ,length    )
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!    Function: Removes the spaces at the end of the name.
!              Also returns the length of the cleaned name.
!              WARNING: do not call this subroutine with a
!              constant character string
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(out)           :: length
                                   !!  Length of cleaned name
    character(*), intent(in)       :: name
                                   !!  Name to be cleaned
!
!
! Local variables
!
    integer                        :: endpos
    character(1)                   :: achr
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----initialization
    !
    endpos = len(name)
    if (endpos==0) goto 888
    !
    !-----skip backside spaces
    !
  100 continue
    achr = name(endpos:endpos)
    if (achr==' ') then
       endpos = endpos - 1
       if (endpos>0) goto 100
    endif
    !
    !-----adjust output
    !
  888 continue
    length = endpos
end subroutine noendspaces
