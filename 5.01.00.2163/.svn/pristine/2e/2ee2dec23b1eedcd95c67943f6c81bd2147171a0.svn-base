subroutine initrtc(gdp)
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
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)     , dimension(:,:) , pointer :: zrtcsta
    integer                       , pointer :: ifirstrtc
    integer                       , pointer :: stacnt
    integer                       , pointer :: rtcmod
    integer      , dimension(:,:) , pointer :: mnrtcsta
    character(20), dimension(:)   , pointer :: namrtcsta
    logical                       , pointer :: rtcact
    character(256)                , pointer :: filrtc
!
!! executable statements -------------------------------------------------------
!
    zrtcsta    => gdp%gdrtc%zrtcsta
    ifirstrtc  => gdp%gdrtc%ifirstrtc
    stacnt     => gdp%gdrtc%stacnt
    rtcmod     => gdp%gdrtc%rtcmod
    mnrtcsta   => gdp%gdrtc%mnrtcsta
    namrtcsta  => gdp%gdrtc%namrtcsta
    rtcact     => gdp%gdrtc%rtcact
    filrtc     => gdp%gdrtc%filrtc
    !
    ifirstrtc = 1
    stacnt = 0
    rtcmod = noRTC
    !
    nullify(gdp%gdrtc%mnrtcsta)
    nullify(gdp%gdrtc%namrtcsta)
    nullify(gdp%gdrtc%zrtcsta)
    !
    rtcact = .false.
    !
    filrtc = ' '
end subroutine initrtc
