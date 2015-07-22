subroutine clreqtran(istat, gdp)
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
!  $Id: clreqtran.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/clreqtran.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
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
! Local variables
!
    integer                     :: i
    integer(pntrsize), external :: close_shared_library
    integer(pntrsize)           :: error
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdp%gdeqtran%dll_handle)) then
       do i = 1,size(gdp%gdeqtran%dll_handle)
          if (gdp%gdeqtran%dll_handle_settle(i) /= 0) then
             error = close_shared_library(gdp%gdeqtran%dll_handle_settle(i))
          endif
          if (gdp%gdeqtran%dll_handle(i) /= 0) then
             error = close_shared_library(gdp%gdeqtran%dll_handle(i))
          endif
       enddo
    endif
    !
    if (associated(gdp%gdeqtran%dll_function_settle)) deallocate(gdp%gdeqtran%dll_function_settle, STAT = istat)
    if (associated(gdp%gdeqtran%dll_name_settle    )) deallocate(gdp%gdeqtran%dll_name_settle    , STAT = istat)
    if (associated(gdp%gdeqtran%dll_handle_settle  )) deallocate(gdp%gdeqtran%dll_handle_settle  , STAT = istat)
    if (associated(gdp%gdeqtran%dll_integers_settle)) deallocate(gdp%gdeqtran%dll_integers_settle, STAT = istat)
    if (associated(gdp%gdeqtran%dll_reals_settle   )) deallocate(gdp%gdeqtran%dll_reals_settle   , STAT = istat)
    if (associated(gdp%gdeqtran%dll_strings_settle )) deallocate(gdp%gdeqtran%dll_strings_settle , STAT = istat)
    if (associated(gdp%gdeqtran%dll_usrfil_settle  )) deallocate(gdp%gdeqtran%dll_usrfil_settle  , STAT = istat)
    !
    if (associated(gdp%gdeqtran%dll_function)) deallocate(gdp%gdeqtran%dll_function, STAT = istat)
    if (associated(gdp%gdeqtran%dll_handle  )) deallocate(gdp%gdeqtran%dll_handle  , STAT = istat)
    if (associated(gdp%gdeqtran%dll_integers)) deallocate(gdp%gdeqtran%dll_integers, STAT = istat)
    if (associated(gdp%gdeqtran%dll_reals   )) deallocate(gdp%gdeqtran%dll_reals   , STAT = istat)
    if (associated(gdp%gdeqtran%dll_strings )) deallocate(gdp%gdeqtran%dll_strings , STAT = istat)
    if (associated(gdp%gdeqtran%dll_usrfil  )) deallocate(gdp%gdeqtran%dll_usrfil  , STAT = istat)
    if (associated(gdp%gdeqtran%flstrn      )) deallocate(gdp%gdeqtran%flstrn      , STAT = istat)
    if (associated(gdp%gdeqtran%iform       )) deallocate(gdp%gdeqtran%iform       , STAT = istat)
    if (associated(gdp%gdeqtran%name        )) deallocate(gdp%gdeqtran%name        , STAT = istat)
    if (associated(gdp%gdeqtran%par         )) deallocate(gdp%gdeqtran%par         , STAT = istat)
end subroutine clreqtran
