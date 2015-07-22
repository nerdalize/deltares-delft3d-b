subroutine initeqtran(gdp       )
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
!  $Id: initeqtran.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/initeqtran.f90 $
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
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: npar
!
!! executable statements -------------------------------------------------------
!
    npar          => gdp%gdeqtran%npar
    !
    ! Note: 30 is hardcoded in sediment transport formulae
    !
    npar   = 30
    !
    nullify(gdp%gdeqtran%dll_function_settle)
    nullify(gdp%gdeqtran%dll_name_settle)
    nullify(gdp%gdeqtran%dll_handle_settle)
    nullify(gdp%gdeqtran%dll_integers_settle)
    nullify(gdp%gdeqtran%dll_reals_settle)
    nullify(gdp%gdeqtran%dll_strings_settle)
    nullify(gdp%gdeqtran%dll_usrfil_settle)
    nullify(gdp%gdeqtran%dll_function)
    nullify(gdp%gdeqtran%dll_handle)
    nullify(gdp%gdeqtran%dll_integers)
    nullify(gdp%gdeqtran%dll_reals)
    nullify(gdp%gdeqtran%dll_strings)
    nullify(gdp%gdeqtran%dll_usrfil)
    nullify(gdp%gdeqtran%flstrn)
    nullify(gdp%gdeqtran%iform)
    nullify(gdp%gdeqtran%name)
    nullify(gdp%gdeqtran%par)
end subroutine initeqtran
