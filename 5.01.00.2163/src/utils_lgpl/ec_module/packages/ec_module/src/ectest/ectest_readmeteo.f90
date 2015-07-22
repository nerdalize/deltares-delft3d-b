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
!  $Id: ectest_readmeteo.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ectest/ectest_readmeteo.f90 $
module ectest_meteo
  !
  ! meteo_module:
  !     Ensure the interface to ectest_readMeteo is automatically known
  !
  ! adri.mourits@deltares.nl
contains
!
!
!==============================================================================
subroutine ectest_readMeteo(testnr)
  use precision
  use gdp
  use ec_module
  !
  implicit none
!
! Arguments
!
   integer        :: testnr
!
! Local variables
!
   integer        :: qId
   integer        :: meteoECId
   integer        :: provId
   logical        :: success
   character(256) :: filename
!
!! executable statements -------------------------------------------------------
!
  ! Add p/u/v quantities/ECItems defined on FLOW grid
  ! Add a reference to the actual array to avoid unnecessary copy actions
  !
  qId      = addQuantity(ECHandle, 'air_pressure')
  patmECItemId = addECItem(ECHandle, qId, gridECItemId)
  if (patmECItemId == 0) call checkResult(ECHandle)
  if (testnr == 1) then
     success  = addECItemField(ECHandle, patmECItemId, patm)
  else
     success  = addECItemField(ECHandle, patmECItemId, patm2d)
  endif
  call checkResult(ECHandle, success)
  !
  qId       = addQuantity(ECHandle, 'x_wind')
  uwindECItemId = addECItem(ECHandle, qId, gridECItemId)
  if (uwindECItemId == 0) call checkResult(ECHandle)
  if (testnr == 1) then
     success   = addECItemField(ECHandle, uwindECItemId, uwind)
  else
     success   = addECItemField(ECHandle, uwindECItemId, uwind2d)
  endif
  call checkResult(ECHandle, success)
  !
  qId       = addQuantity(ECHandle, 'y_wind')
  vwindECItemId = addECItem(ECHandle, qId, gridECItemId)
  if (vwindECItemId == 0) call checkResult(ECHandle)
  if (testnr == 1) then
     success   = addECItemField(ECHandle, vwindECItemId, vwind)
  else
     success   = addECItemField(ECHandle, vwindECItemId, vwind2d)
  endif
  call checkResult(ECHandle, success)
  !
  if (testnr == 1) then
     !
     ! Uniform wind speed
     ! file ectest_input.wnd contains time, windspeed, winddirection
     !
     filename = '..\ectest_input.wnd'
     provId   = addProvider(ECHandle, provType_file, filename, provFile_unimagdir)
     if (provId == 0) call checkResult(ECHandle)
     success  = initProvider(ECHandle, provId)
     call checkResult(ECHandle, success)
     !
     ! air pressure: default value
     !
     provId   = addProvider(ECHandle, provType_default)
     if (provId == 0) call checkResult(ECHandle)
     success  = initProvider(ECHandle, provId, 'air_pressure', 101300.0_fp)
     call checkResult(ECHandle, success)
  else if (testnr == 2) then
     !
     ! meteo_on_flow_grid
     ! file dec82.wnd contains time, windcomponents and pressure
     !
     filename = '..\dec82.wnd'
     provId   = addProvider(ECHandle, provType_file, filename, provFile_svwp)
     if (provId == 0) call checkResult(ECHandle)
     success  = initProvider(ECHandle, provId)
     call checkResult(ECHandle, success)
  else
     !
     ! gribfiles on rotated grid
     ! file HIRLAM_SURFACE_1998040100.grib contains time, windcomponents and pressure
     !
     filename = '..\HIRLAM_SURFACE_1998040?00.grib'
    !filename = '..\HIRLAM_SURFACE_2009010?00.grib'
     provId   = addProvider(ECHandle, provType_file, filename, provFile_grib)
     if (provId == 0) call checkResult(ECHandle)
     success  = initProvider(ECHandle, provId)
     call checkResult(ECHandle, success)
  endif

end subroutine ectest_readMeteo



end module ectest_meteo
