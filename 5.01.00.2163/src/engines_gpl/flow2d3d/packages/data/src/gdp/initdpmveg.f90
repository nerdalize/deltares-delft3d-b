subroutine initdpmveg(gdp       )
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
!  $Id: initdpmveg.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/initdpmveg.f90 $
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
    integer                              , pointer :: itplant
    integer                              , pointer :: nveg
    integer, dimension(:,:)              , pointer :: planttype
    real(fp), dimension(:,:)             , pointer :: nplants
    real(fp)                             , pointer :: clplant
    character(256)                       , pointer :: fildpmv
    type (dpm_vegetation), dimension(:)  , pointer :: vegs
    type (gd_dpmveg)                     , pointer :: gddpmveg
!
!! executable statements -------------------------------------------------------
!
    itplant    => gdp%gddpmveg%itplant
    nveg       => gdp%gddpmveg%nveg
    planttype  => gdp%gddpmveg%planttype
    nplants    => gdp%gddpmveg%nplants
    clplant    => gdp%gddpmveg%clplant
    fildpmv    => gdp%gddpmveg%fildpmv
    vegs       => gdp%gddpmveg%vegs
    gddpmveg   => gdp%gddpmveg
    !
    itplant   = 0
    nveg      = 0
    nullify(gdp%gddpmveg%planttype)
    nullify(gdp%gddpmveg%nplants)
    clplant   = 0.0
    fildpmv   = ' '
    nullify(gdp%gddpmveg%vegs)
end subroutine initdpmveg
