subroutine rdmor1(ilun      ,morfac    ,tmor      ,thresh    ,morupd    , &
                & eqmbc     ,densin    ,aksfac    ,rwave     ,alfabs    , &
                & alfabn    ,sus       ,bed       ,susw      ,bedw      , &
                & sedthr    ,thetsd    ,hmaxth    ,fwfac     ,epspar    , &
                & iopkcw    ,rdc       ,rdw       )
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
!  $Id: rdmor1.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdmor1.f90 $
!!--description-----------------------------------------------------------------
!
! Read  morphology input version 1
! The first line in the file must be:
! * version 1
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)  :: ilun
    integer, intent(out) :: iopkcw
    logical, intent(out) :: densin !  Description and declaration in morpar.igs
    logical, intent(out) :: epspar
    logical, intent(out) :: eqmbc !  Description and declaration in morpar.igs
    logical, intent(out) :: morupd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: aksfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabn !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabs !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bed !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bedw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: fwfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: hmaxth !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: morfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: rdc
    real(fp), intent(out)    :: rdw
    real(fp), intent(out)    :: rwave !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sedthr !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sus !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: susw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thetsd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thresh !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: tmor !  Description and declaration in morpar.igs
!
!
! Local variables
!
    integer       :: iost
    character(78) :: string
!
!
!! executable statements -------------------------------------------------------
!
    ! morphological timescale factor
    read (ilun, *) morfac
    ! start for calculating morphological changes
    read (ilun, *) tmor
    ! calculation of ITMOR has been moved to TRICOM
    ! threshold value for slowing erosion near a fixed layer (m)
    read (ilun, *) thresh
    ! flag for doing bottom updates
    read (ilun, *) morupd
    ! flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for sand sediment
    read (ilun, *) eqmbc
    ! flag for including sediment in fluid density calculations
    read (ilun, *) densin
    ! factor for setting aks height
    read (ilun, *) aksfac
    ! factor for calculating wave-related roughness from ripple dimensions
    read (ilun, *) rwave
    !
    ! IF Rouse skip
    !
    read (ilun, '(A)') string
    call small(string, len(string))
    if (index(string, 'rouse')==0) backspace (ilun)
    ! factor for longitudinal bed load transport
    read (ilun, *) alfabs
    ! factor for transverse bed load transport
    read (ilun, *) alfabn
    ! factor for calculating suspended load transport
    read (ilun, *) sus
    ! factor for calculating bed load transport
    read (ilun, *) bed
    ! wave-related suspended sediment factor
    read (ilun, *) susw
    ! wave-related bed-load sediment factor
    read (ilun, *) bedw
    ! minimum depth for sediment calculations
    read (ilun, *) sedthr
    ! global / maximum dry cell erosion factor
    read (ilun, *) thetsd
    ! maximum depth for variable dry cell erosion factor
    read (ilun, *) hmaxth
    ! factor for adjusting intensity of energy dissipation in wave boundary layer
    read (ilun, *) fwfac
    ! flag for parametric epsilon distribution in case of K-Eps model
    read (ilun, *) epspar
    !
    read (ilun, *, iostat = iost) iopkcw, rdc, rdw
    ! IOSTAT to prevent problems if rdc and rdw are not given
end subroutine rdmor1
