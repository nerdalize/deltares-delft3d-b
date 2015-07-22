subroutine initarrays(gdp)
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
!  $Id: initarrays.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/initarrays.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
!
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
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: nofou
!
! Local variables
!
    integer               :: imissval
    real(fp)              :: rmissval
!
!! executable statements -------------------------------------------------------
!
    nofou               => gdp%d%nofou
    !
    imissval = -1
    rmissval = -999.999_fp
    !
    ! Initialise arrays for Fourier analysis
    !
    if (nofou > 0) then
       gdp%gdfourier%fconno   = imissval
       gdp%gdfourier%flayno   = imissval
       gdp%gdfourier%fnumcy   = imissval
       gdp%gdfourier%ftmsto   = imissval
       gdp%gdfourier%ftmstr   = imissval
       gdp%gdfourier%ifoupt   = imissval
       gdp%gdfourier%iofset   = imissval
       !
       gdp%gdfourier%fknfac   = rmissval
       gdp%gdfourier%foucomp  = rmissval
       gdp%gdfourier%foufas   = rmissval
       gdp%gdfourier%fousma   = rmissval
       gdp%gdfourier%fousmb   = rmissval
       gdp%gdfourier%fouvec   = rmissval
       gdp%gdfourier%fv0pu    = rmissval
       !
       gdp%gdfourier%fouelp   = ' '
       gdp%gdfourier%founam   = ' '
       gdp%gdfourier%foutyp   = ' '
    endif
    !
end subroutine initarrays
