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
!  $Id: flow2d3d_openda_dll.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/flow2d3d_openda/src/flow2d3d_openda_dll.f90 $
!
!======================================================================
!
!-------------------------------------------------------------------------------
!
! The following functions were originally located in d3df_dll.f90
!
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!
function SE_GetTimeHorizon(componentID, schemID, startMJD, endMJD) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetTimeHorizon
    !
    implicit none
    !
    ! result
    integer :: retVal   ! 0: OK
    !
    ! externals
    integer, external :: GetTimeHorizon
    !
    ! arguments
    character(*)    , intent(in) :: componentID ! RR, RTC, etc., actually it is "D3D_flow"
    character(*)    , intent(in) :: schemID     ! schem. file (*.fnm)
    
    double precision, intent(out):: startMJD      ! Model's start time (MJD)
    double precision, intent(out):: endMJD        ! Model's end time (MJD)    
    
    double precision :: startm      ! Model's start time (minutes)
    double precision :: endm        ! Model's end time (minutes)
    !
    ! body
    !
    retVal = GetTimeHorizon(componentID, schemID, startMJD, endMJD)
    !
end function SE_GetTimeHorizon
!
!-------------------------------------------------------------------------------
!
function SE_GetDeltaT(componentID, schemID, deltaT_MJD) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetDeltaT
    !
    implicit none
    !
    ! result
    integer :: retVal   ! 0: OK
    !
    ! externals
    integer, external :: GetDeltaT
    !
    ! arguments
    character(*)    , intent(in) :: componentID ! RR, RTC, etc., actually it is "D3D_flow"
    character(*)    , intent(in) :: schemID     ! schem. file (*.fnm)
    double precision, intent(out):: deltaT_MJD      ! Model's DeltaT (in Modified Julian )
    double precision                :: deltaT      ! Model's DeltaT (in minutes  )
    !
    ! body
    !
    retVal = GetDeltaT(componentID, schemID, deltaT_MJD)
    !
end function SE_GetDeltaT
!
!-------------------------------------------------------------------------------
!
subroutine SE_GetCurrentTime(componentID, schemID, retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_GetCurrentTime
    !
    implicit none
    !
    ! arguments
    double precision             :: retVal       ! Current Model time (minutes?)
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    !
    call GetCurrentTime(componentID, schemID, retVal)
    !
end subroutine SE_GetCurrentTime
!
!-------------------------------------------------------------------------------
!
function SE_PerformTimeStep(componentID, schemID, time_step) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_PerformTimeStep
    !
    implicit none
    !
    ! result
    integer :: retVal
    !
    ! externals
    integer, external :: PerformTimeStep
    !
    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    integer                      :: time_step    ! Current time step
    !
    ! body
    !
    retVal = PerformTimeStep(componentID, schemID, time_step)
    !
end function SE_PerformTimeStep
!
!----------------------------------------------------------------------------------
!
function GETERROR(error, errorDescription) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: GETERROR
    !
    implicit none
    !
    ! return value
    integer                       :: retVal         ! >=0 : Success; <0 : Error
    !
    ! externals
    integer, external :: GETERROR_core
    !
    ! arguments
    character(len=*), intent(out) :: errorDescription ! error description text
    integer         , intent(in)  :: error            ! error index
    !
    ! body
    !
    retVal = GETERROR_core(error, errorDescription)
    !
end function GETERROR
!
!-------------------------------------------------------------------------------
!
! The following functions were originally located in get_openda_exchange_items.f90
!
!-------------------------------------------------------------------------------
!
function SE_get_exchange_item_count() result(count)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_exchange_item_count
    !
    implicit none
    !
    ! return value
    integer :: count ! # exchange items
    !
    ! externals
    integer, external :: get_exchange_item_count
    !
    ! body
    !
    count = get_exchange_item_count()
    !
end function SE_get_exchange_item_count
!
!-------------------------------------------------------------------------------
!
function SE_get_exchange_item_id_II(location_id,quantity_id) result(id)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_exchange_item_id_II
    !
    implicit none
    !
    ! return value
    integer :: id ! # exchange id
    !
    ! externals
    integer, external :: get_exchange_item_id_II
    !
    !arguments
    integer :: location_id, quantity_id
    !
    ! body
    !
    id = get_exchange_item_id_II(location_id,quantity_id)
    !
end function SE_get_exchange_item_id_II
!
!-------------------------------------------------------------------------------
!
function SE_get_exchange_item_id_CI(location_id_c,quantity_id) result(id)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_exchange_item_id_CI
    !
    implicit none
    !
    ! return value
    integer :: id ! # exchange id
    !
    ! externals
    integer, external :: get_exchange_item_id_CI
    !        
    !arguments
    character(len=*)  :: location_id_c
    integer           :: quantity_id
    !
    ! body
    !
    id = get_exchange_item_id_CI(location_id_c, quantity_id)
    !
end function SE_get_exchange_item_id_CI
!
!-------------------------------------------------------------------------------
!
function SE_get_values_count_for_time_span(instance, exchange_item_id, start_time, end_time) result(ret_val)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_values_count_for_time_span
    !
    implicit none
    !
    ! return value
    integer :: ret_val
    !
    ! externals
    integer, external :: get_values_count_for_time_span
    !
    ! arguments
    integer         , intent(in) :: instance         ! model instance
    integer         , intent(in) :: exchange_item_id ! type and location of quantity 
    double precision, intent(in) :: start_time       ! start time of values
    double precision, intent(in) :: end_time         ! end time of values
    !
    ! body
    !
    ret_val = get_values_count_for_time_span(instance, exchange_item_id, start_time, end_time)
    !
end function SE_get_values_count_for_time_span
!
!-------------------------------------------------------------------------------
!
function SE_get_values_for_time_span(exchange_item_id, start_time, end_time, nvals,values) result(ret_val)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_values_for_time_span
    !
    implicit none
    !    
    ! return value
    integer :: ret_val
    !
    ! externals
    integer, external :: get_values_for_time_span
    !
    ! arguments
    integer                           , intent(in)    :: exchange_item_id ! type and location of quantity 
                                                                          ! (e.g. discharge or waterlevel at point M7)
    double precision                  , intent(in)    :: start_time       ! start time of bc values
    double precision                  , intent(in)    :: end_time         ! end time of bc values
    integer                           , intent(in)    :: nvals            ! size of values array
    double precision, dimension(nvals), intent(inout) :: values           ! returned values
    !
    ! body
    !
    ret_val = get_values_for_time_span(exchange_item_id, start_time, end_time, nvals,values)
    !
end function SE_get_values_for_time_span
!
!-------------------------------------------------------------------------------
!
function SE_set_noise_for_time_span(exchange_item_id, start_time, end_time, operation, nvals,values) result(ret_val)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_set_noise_for_time_span
    !
    implicit none
    !
    ! return value
    integer :: ret_val
    !
    ! externals
    integer, external :: set_noise_for_time_span
    !
    ! arguments
    integer                           , intent(in) :: exchange_item_id ! type and location of quantity 
                                                                       ! (e.g. discharge or waterlevel at point M7)
    double precision                  , intent(in) :: start_time       ! start time of bc values
    double precision                  , intent(in) :: end_time         ! end time of bc values
    integer                           , intent(in) :: operation        ! operation: oper_multiply, oper_add, oper_set
    integer                           , intent(in) :: nvals            ! size of values array
    double precision, dimension(nvals), intent(in) :: values           ! returned values
    !
    ! body
    !
    ret_val = set_noise_for_time_span(exchange_item_id, start_time, end_time, operation, nvals,values)
    !
end function SE_set_noise_for_time_span
!
!--------------------------------------------------------------------------------------------
!
subroutine SE_Set_Max_Instances_In_Memory(max_instances)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Set_Max_Instances_In_Memory
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: max_instances  ! max #instances to be kept in memory
    !
    max_instances_in_memory = max_instances
    !
end subroutine SE_Set_Max_Instances_In_Memory
!
!======================================================================
!
function SE_Create_Instance() result(instance_id)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Create_Instance
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: instance_id  ! instance identifier (instanceId >= 0 : success)
    !
    instance_id = d3da_create_instance()
    !
end function SE_Create_Instance
!
!==============================================================================
!
subroutine SE_Select_Instance(instance_id)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Select_Instance
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! arguments
    integer, intent(in) :: instance_id  ! instance identifier
    !
    call d3da_select_new_instance(instance_id)
    !
end subroutine SE_Select_Instance
!
!==============================================================================
!
subroutine SE_Select_Instance_from_Restartfile(instance_id, filename)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Select_Instance_from_Restartfile
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! arguments
    integer         , intent(in) :: instance_id  ! instance identifier
    character(Len=*), intent(in) :: filename
    !
    call d3da_select_instance_from_restartfile(instance_id, filename)
    !
end subroutine SE_Select_Instance_from_Restartfile
!
!==============================================================================
!
function SE_Store_Current_Instance(storage_level) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Store_Current_Instance
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! argument
    integer :: storage_level 
    !    
    ! result
    integer :: retVal
    !
    call d3da_store_current_instance(storage_level,'default')
    retVal = 0
    !
end function SE_Store_Current_Instance
!
!==============================================================================
!
function SE_Store_Current_Instance_Restartfile(filename) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Store_Current_Instance_Restartfile
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! arguments
    character(Len=*), intent(in) :: filename    
    ! result
    integer :: retVal
    !
    call d3da_store_current_instance_restartfile(filename)
    retVal = 0
    !
end function SE_Store_Current_Instance_Restartfile
!
!==============================================================================
!
function SE_Get_Instance_Core_State(corestate, size_corestate) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Get_Instance_Core_State
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: retVal
    !
    integer                                       :: size_corestate
    double precision, dimension(size_corestate)   :: corestate
    !
    call d3da_getcorestate(corestate,size_corestate,retval)
    !
end function  SE_Get_Instance_Core_State
!
!==============================================================================
!
function SE_Set_Instance_Core_State(corestate,size_corestate) result (retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Set_Instance_Core_State
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: retVal
    !
    integer                                                  :: size_corestate
    double precision, dimension(size_corestate), intent(in)  :: corestate
    !
    call d3da_setcorestate(corestate,size_corestate,retVal)
    !
end function  SE_Set_Instance_Core_State
!
!==============================================================================
!
function SE_Get_Instance_Size() result (inst_size)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Get_Instance_Size
    !
    use m_d3dstate_2_openda
    !
    implicit none
    !
    ! result
    integer :: inst_size
    !
    call d3da_getinstancesize(inst_size)
    !
end function  SE_Get_Instance_Size
!
!==============================================================================
!
subroutine SE_Export_Current_Instance(doappend)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Export_Current_Instance
    ! 
    use m_d3dstate_2_openda
    ! 
    integer :: ierr
    logical :: doappend
    !
    ! to do: export in append mode
    !
    call d3da_ctastate_to_netcdf(ierr)
    !
end subroutine SE_Export_Current_Instance
!
!==============================================================================
!
function SE_Initialize_openda(componentID, schemID) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Initialize_openda
    !
    implicit none
    !
    ! result
    integer :: retVal              ! retVal == 0 : success
    !
    ! externals
    integer, external :: Initialize_openda
    !
    ! arguments
    character(*), intent(in) :: componentID  ! RR, RTC, etc.
    character(*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    !
    retval = Initialize_openda(componentID, schemID)
    !
end function SE_Initialize_openda
!
!==============================================================================
!
function SE_Finalize_openda(componentID, schemID) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_Finalize_openda
    !
    use m_d3dstate_2_openda
    !    
    implicit none
    !
    ! result
    integer     :: retVal           ! retVal == 0 : success
    !
    ! externals
    integer, external :: Finalize_openda
    !
    ! arguments
    character(Len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(Len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    !
    retval = Finalize_openda(componentID, schemID)
    !
end function SE_Finalize_openda
!
!==============================================================================
!
subroutine define_ordinary_state(imode)
    !DEC$ ATTRIBUTES DLLEXPORT :: define_ordinary_state
    !
    implicit none
    !    
    integer :: imode
    !
    call d3da_define_ordinary_state(imode)
    !
end subroutine define_ordinary_state
!
!==============================================================================
!
function SE_get_observed_localization(location_id_c, dist, nvals, values) result(retVal)
    !DEC$ ATTRIBUTES DLLEXPORT :: SE_get_observed_localization
    !
    implicit none
    !
    ! result
    integer :: retVal
    !
    ! externals
    integer, external :: getObservedLocalization
    !
    ! arguments
    integer         , intent(in)                    :: nvals        ! size of values array
    double precision, intent(in)                    :: dist         ! the characteristic distance
    double precision, dimension(nvals), intent(out) :: values       ! returned values
    character(*)    , intent(in)                    :: location_id_c ! location name
    !
    ! body
    !

    retVal = getObservedLocalization(location_id_c, dist, nvals, values)
    !
end function SE_get_observed_localization
