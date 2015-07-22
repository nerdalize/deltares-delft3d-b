subroutine setlun(lundia    ,subsys    ,gdp       )
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
!  $Id: setlun.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/general/setlun.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Set unit number of sub-system
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
    integer, intent(in)            :: lundia !  Description and declaration in inout.igs
    character(*), intent(in)       :: subsys
                                   !!  String containing sub-system of
                                   !!  Delft3D package
!
!
! Local variables
!
    integer                        :: iind                 ! Index number of SUBSYS in TOTSYS 
    integer                        :: iisubs               ! Index number for sub-system from Delft3D 0 = No Delft3D sub-system 1 = BOTT 2 = CHEM 3 = ECO 4 = FLOW 5 = MOR 6 = PART 7 = SED 8 = TRAN 9 = WAQ 10 = WAVE 
    integer                        :: lsubs                ! Length of chartacter string SUBSYS 
    character(4)                   :: chulp ! Help string 
    character(50)                  :: totsys ! String containing all sub-systems defined in Delft3D 
!
    !
    !
    data totsys/'bott chem eco  flow mor  part sed  tran waq  wave '/
!
!! executable statements -------------------------------------------------------
!
    !
    !-----Reset SUBSYS to small characters in 4 character string
    !
    !
    !     GLOBAL DATA INITIALISATION
    isubs   => gdp%gddiagno%isubs
    lunsys  => gdp%gddiagno%lunsys
    !
    lsubs = min(len(subsys), 4)
    chulp = subsys(:lsubs)
    !
    call small(chulp     ,4         )
    !
    !-----Define IISUBS on sub-system requested
    !
    iind = index(totsys, chulp)
    iisubs = 0
    if (iind/=0) iisubs = (iind - 1)/5 + 1
    !
    !-----For IISUBS = 0  nothing will happen
    !
    if (iisubs/=0) then
       lunsys(iisubs) = lundia
    endif
end subroutine setlun
