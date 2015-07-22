module ec_typedefs
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
!  $Id: ec_typedefs.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_typedefs.f90 $
!!--description-----------------------------------------------------------------
!
! type definitions of ec-module
!    use to be in several files, but that gives too much dependencies.
!    drawback: fields can't be private anymore
!
!!--pseudo code and references--------------------------------------------------
!
! edwin.spee@deltares.nl
!
!!--declarations----------------------------------------------------------------
use precision
  implicit none
!
! parameters
!
  integer, parameter :: maxNameLen     = 100
  integer, parameter :: maxFileNameLen = 256
!
! types
! order inside a type: doubles, reals, integers, pointers, logicals, characters
!
  type tQuantity
    integer               :: id       ! unique quantity identification
    character(maxNameLen) :: name     ! name given to a dataset/quantity at initialisation
  end type tQuantity
!
  type ecElementSet
    integer                         :: id          ! unique elementSet identification
    integer                         :: dim         ! size of IDs, kcs, x and y
    integer , dimension(:), pointer :: kcs         ! value = 0: invalid point   value /= 0: valid point
    real(hp), dimension(:), pointer :: x           ! size = 2: x(1)=x0 x(2)=dx, dim= number of points
    real(hp), dimension(:), pointer :: y           ! size = 2: x(1)=x0 x(2)=dx, dim= number of points
    character(len=24), dimension(:), pointer :: IDs ! string array with IDs
    logical                         :: spherical   ! true: spherical coordinates
    logical                         :: xy_based    ! true: xy based, otherwise ID based
  end type ecElementSet
!
  type tIndexWeight
    integer                           :: id            ! unique indexWeight identification
    integer                           :: sourceElSetId
    integer                           :: targetElSetId
    integer ,dimension(:,:), pointer  :: indices       ! indices      , eq (3,nmx) for triangulation
    real(fp),dimension(:,:), pointer  :: weightFactors ! weightfactors, eq (3,mnx) for triangulation
  end type tIndexWeight
  !
  type tECConverter
    integer                                       :: id                  ! unique ECConnection identification
    integer                                       :: type                ! see convType enumeration
    integer                                       :: interpolationMethod ! both converter and provider must know it
    integer                                       :: operand             ! each converter has an operator
    integer                                       :: method              ! initially, store the desired spacetime method
                                                                         ! later, these methods are collected and put in relations
    integer                                       :: iweight             ! pointer to indexweight, only for method weightfactors
    type(tECConnectionPtr), dimension(:), pointer :: connections         ! connections on which this converter will be applied
    type(tIndexWeight)                  , pointer :: indexWeightId
  end type tECConverter
!
  type tECField
     real(hp)                            :: time
     real(hp)                            :: missingValue
     integer                             :: elementSetId ! pointer naar elementset bijhorend bij datagrid (plaats)
     real(fp), dimension(:)    , pointer :: arr1d        ! 1-dim array veld
     real(fp), dimension(:,:)  , pointer :: arr2d        ! 2-dim array veld
     real(fp), dimension(:,:,:), pointer :: arr3d        ! 3-dim array veld
  end type tECField
!
  type tECConnection
    integer                                 :: id              ! unique ECConnection identification
    integer                                 :: converterId     ! unique ECConverter identification
    type(tECItemPtr), dimension(:), pointer :: sourceItems     ! source item
    type(tECItemPtr), dimension(:), pointer :: targetItems     ! source item
  end type tECConnection
  type tECConnectionPtr
    type(tECConnection), pointer :: ECConnectionPtr
  end type tECConnectionPtr
!
  type tECItem
    integer                                :: id                  ! unique ECItem identification
    character(len=maxNameLen)              :: name                ! unique name for ECItem
    integer                                :: type                ! see ECItemType enumeration
    integer                                :: elementSetId        ! Id of the related eleementSet
    integer                                :: t0FieldId           ! index in field of values at the old time
    integer                                :: t1FieldId           ! index in field of values at the new time
    integer     , dimension(:), pointer    :: providerIds         ! sourceItem: Unique provider identification
                                                                  ! targetItem: List of providers to be updated for a getValue
    integer     , dimension(:), pointer    :: connectionIds       ! Ordered(!) list of connections in which this ECItem is used
                                                                  ! 0 in case this item is not used (will occur for OpenMI candidates)
    integer     , dimension(:), pointer    :: converterIds        ! targetItem: List of converters to be used for a getValue
                                                                  ! sourceItem: empty
    type(tQuantity)           , pointer    :: quantity
    integer                                :: quantityId          ! Id of the related quantity
    type(ecElementSet)        , pointer    :: elementSet
    type(tECField), dimension(:), pointer    :: fields              ! Values on source elementSet
  end type tECItem
!
  type tECItemPtr
    type(tECItem), pointer :: ECItemPtr
  end type tECItemPtr
!
  type tECData
    integer                                       :: internalGrid     ! Index of the elementSet containing the grid used internally
    type(tQuantity)       , dimension(:), pointer :: quantities       ! All quantities (sources and targets)
    type(ecElementSet)    , dimension(:), pointer :: elementSets      ! All collections on which quantities are defined (sources and targets)
    type(tECItemPtr)      , dimension(:), pointer :: ECItemPtrs       ! All relations between quantities, elementSets, Fields
    type(tECConnectionPtr), dimension(:), pointer :: connectionPtrs   ! All relations between sources and targets
    logical                                       :: initialized
  end type tECData
!
  type tGrib_meta
     real(hp) :: latsp  ! latitude of south pole (rotated spherical coordinates)
     real(hp) :: lonsp  ! latitude of south pole (rotated spherical coordinates)
     real(hp) :: dx     ! grid spacing in x/longitude direction
     real(hp) :: dy     ! grid spacing in y/latitude direction
     real(hp) :: x0     ! first x/longitude coordinate
     real(hp) :: y0     ! first y/latitude coordinate
  end type tGrib_meta
!
  type tECProvider
     integer                                 :: id           ! unique provider identification
     integer                                 :: type         ! see provType enumeration
     integer                                 :: fileType     ! see provFile enumeration
     integer                                 :: filenr       ! nr when using wildcards
     integer                                 :: pHandle      ! handle to file or openmi handle
     type(tECItemPtr), dimension(:), pointer :: items        ! items to be updated by this provider
     character(maxNameLen)                   :: name
     character(maxFileNameLen)               :: fileName     ! file containing data
     type(tGrib_meta)                        :: grib_meta    ! meta info from grib-files
  end type tECProvider
!
  type tEC
     type(tECData)                    , pointer :: ECData      => null() ! source items, target items and connections
     type(tECConverter) , dimension(:), pointer :: converters  => null()
     type(tECProvider)  , dimension(:), pointer :: providers   => null()
     logical                                    :: initialized =  .false.
  end type tEC
  !
  ! The EC-module has one public handle to a tEC set that is visible/usable by all kernels: public_fields
  ! Each kernel has its own tEC set, not visible/usable by other kernels                  : fields
  !
  type tECHandle
     type(tEC), pointer :: fields => null()
  end type tECHandle
  !
end module ec_typedefs
