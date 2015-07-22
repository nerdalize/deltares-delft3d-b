subroutine getlun(lundia    ,gdp       )
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
!  $Id: getlun.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/allocation/getlun.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Get unit number of active sun-system
!              The sub-system is defined by DEFSUB (0-MXSYS)
!              The unit number will be set with SETLUN
! Method used:
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
    integer               , pointer :: isubs
    integer, dimension(:) , pointer :: lunsys
!
! Global variables
!
    integer         :: lundia !  Description and declaration in inout.igs
!
!
! Local variables
!
    logical                        :: opend                ! Help flag = TRUE when file is still open (DELFT3D) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    isubs   => gdp%gddiagno%isubs
    lunsys  => gdp%gddiagno%lunsys
    !
    if (isubs==0) then
       lundia = 0
    else
       !
       !-----For ISUBS <> 0 LUNDIA := LUNSYS(ISUBS ) if unit is opened
       !               else LUNDIA := 0
       !
       lundia = lunsys(isubs)
       if (lundia/=0) then
          inquire (lundia, opened = opend)
          if (.not.opend) lundia = 0
       endif
    endif
end subroutine getlun
