!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: ectest_gdp.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ectest/ectest_gdp.f90 $
module gdp
  ! adri.mourits@deltares.nl
  use precision
  use ec_module
  !
  implicit none
  !
  ! Grid
  !
  integer                          , save :: kmax
  integer                          , save :: mmax
  integer                          , save :: nmax
  integer , dimension(:),   pointer, save :: kcs
  real(hp), dimension(:),   pointer, save :: x
  real(hp), dimension(:),   pointer, save :: y
  real(fp), dimension(:),   pointer, save :: uwind
  real(fp), dimension(:),   pointer, save :: vwind
  real(fp), dimension(:),   pointer, save :: patm
  real(fp), dimension(:,:), pointer, save :: uwind2d
  real(fp), dimension(:,:), pointer, save :: vwind2d
  real(fp), dimension(:,:), pointer, save :: patm2d
  logical                          , save :: sferic
  !
  ! Time
  !
  integer , save :: nst
  integer , save :: ntstop
  real(hp), save :: dt
  real(hp), save :: curtim
  !
  ! EC stuff
  !
  integer        , save :: gridECItemId
  integer        , save :: patmECItemId  ! Store the ECItemIds of all target items
  integer        , save :: uwindECItemId
  integer        , save :: vwindECItemId
  type(tECHandle), save :: ECHandle      ! The one and only EC access handle ensuring thread safety
  logical        , save :: ECPrivate     ! FALSE: Use the (only) EC-module, usable by all kernels

end module gdp

