subroutine tdatmain(runid, alone, subsys, filmrs, nuerr, gdp)
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
!  $Id: tdatmain.F90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/manager/src/tdatmain.F90 $
!!--description-----------------------------------------------------------------
!
!    Function: Main program for Delft3D-FLOW stand alone to call
!              TDATOM sub-routine
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(6), pointer :: prognm
!
! Global variables
!
    integer                    :: nuerr  ! Exit code: 0 := ok, < 0 then error
    logical       , intent(in) :: alone  ! TRUE when flow runs stand-alone, FALSE when flow is part of morsys 
    character(4)  , intent(in) :: subsys ! Sub-system definition of Delft3D here SUBSYS = 'flow' 
    character(*)  , intent(in) :: runid
    character(12) , intent(in) :: filmrs ! File name for DELFT3D_MOR FLOW input file (MD-flow.xxx) 
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    !
    prognm      => gdp%gdprognm%prognm 
    !
    ! Set program name and numdomains (tdatom can only be called for one subdomain)
    !
    prognm     = 'TDATOM'
    !
    ! Initialize local parameters
    !
    nuerr  = 0
    !
    ! Initialize sub-system for Delft3D-FLOW
    !
    call defsub(subsys, gdp)
    !
    ! Call to major routine to create time dependent data files
    !
    call tdatom(runid, filmrs, nuerr ,alone, gdp)
    !
end subroutine tdatmain
