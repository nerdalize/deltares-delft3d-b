subroutine clrtrachy(istat, gdp)
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
!  $Id: clrtrachy.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/clrtrachy.f90 $
!!--description-----------------------------------------------------------------
! NONE
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
! Global variables
!
    integer,intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdp%gdtrachy%ittaru)) deallocate (gdp%gdtrachy%ittaru, STAT = istat)
    if (associated(gdp%gdtrachy%ittarv)) deallocate (gdp%gdtrachy%ittarv, STAT = istat)
    if (associated(gdp%gdtrachy%ittdef)) deallocate (gdp%gdtrachy%ittdef, STAT = istat)
    !
    if (associated(gdp%gdtrachy%vegh2d)) deallocate (gdp%gdtrachy%vegh2d, STAT = istat)
    if (associated(gdp%gdtrachy%vden2d)) deallocate (gdp%gdtrachy%vden2d, STAT = istat)
    !
    if (associated(gdp%gdtrachy%rgcalu)) deallocate (gdp%gdtrachy%rgcalu, STAT = istat)
    if (associated(gdp%gdtrachy%rgcalv)) deallocate (gdp%gdtrachy%rgcalv, STAT = istat)
    if (associated(gdp%gdtrachy%rttaru)) deallocate (gdp%gdtrachy%rttaru, STAT = istat)
    if (associated(gdp%gdtrachy%rttarv)) deallocate (gdp%gdtrachy%rttarv, STAT = istat)
    if (associated(gdp%gdtrachy%rttdef)) deallocate (gdp%gdtrachy%rttdef, STAT = istat)
    if (associated(gdp%gdtrachy%rttfu))  deallocate (gdp%gdtrachy%rttfu , STAT = istat)
    if (associated(gdp%gdtrachy%rttfv))  deallocate (gdp%gdtrachy%rttfv , STAT = istat)
end subroutine clrtrachy
