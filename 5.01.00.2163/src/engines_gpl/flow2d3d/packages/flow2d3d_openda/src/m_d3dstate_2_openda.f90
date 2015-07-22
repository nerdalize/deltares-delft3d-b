module m_d3dstate_2_openda
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
!  $Id: m_d3dstate_2_openda.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/flow2d3d_openda/src/m_d3dstate_2_openda.f90 $
!-------------------------------------------------------------------------------
!
use m_d3d_state
implicit none

private

integer, parameter :: instance_block_size = 100            ! increase
integer, parameter :: no_instance_yet = -1                 ! initial value, indicating: no instances yet
integer, parameter :: storage_only_at_ctastate = 0         ! storage level: from d3d towards COSTA state
integer, parameter :: storage_only_to_disk     = 1         ! storage level: from COSTA state to disk
integer, parameter :: storage_whole_range      = 2         ! storage level: to disk, all the 
                                                           ! way from d3d level 
integer            :: max_instances_in_memory = 256        ! maximum number of instances in memory

integer, parameter :: UNCREATED_STATE = -1
integer, parameter :: UNCREATED_HANDLE = -1

integer            :: instance_count = 0                   ! actual #instances
integer            :: current_instance = no_instance_yet   ! currently active instance
integer            :: instances_in_memory = 0              ! current number of instances in memory

integer            :: state_handles(0:instance_block_size) = UNCREATED_STATE ! COSTA state handles
logical            :: state_in_memory(0:instance_block_size) ! flag: is the state in memory or on file?
character(len=256) :: file_names(0:instance_block_size)      ! State file names
integer            :: file_handles(0:instance_block_size) = UNCREATED_HANDLE   ! COSTA state files handles


public :: d3da_create_instance, d3da_select_new_instance, d3da_store_current_instance,&
          d3da_ctastate_to_netcdf, d3da_getcorestate, d3da_setcorestate, d3da_getinstancesize, &
          max_instances_in_memory, d3da_select_instance_from_restartfile, &
          d3da_store_current_instance_restartfile, d3da_reset_all, d3da_close_cta_state_files

character(len=10)  :: state_file_extension = '.txt'           ! default: ascii; other possible value: nc (netcdf)

integer, dimension(9), public :: sub_core_offsets = 0

contains

!----------------------------
subroutine d3da_reset_all()

  implicit none
  
  max_instances_in_memory = 256
  instance_count = 0
  current_instance = no_instance_yet
  instances_in_memory = 0
  state_handles = UNCREATED_STATE
  file_handles = UNCREATED_HANDLE
  
end subroutine d3da_reset_all


!-----------------
subroutine d3da_set_state_file_extension(a_state_file_extension)

    ! arguments
    character(len=*), intent(in) :: a_state_file_extension
    
    ! body

    state_file_extension = a_state_file_extension

end subroutine d3da_set_state_file_extension


!-----------------
function d3da_create_instance() result(instance_id)

    ! return value
    integer :: instance_id
    
    ! locals
    logical :: isbackground
    integer :: ierr

    if (current_instance == no_instance_yet) then
       ! First instance creation. Initialize Costa
        call CTA_CORE_INITIALISE(ierr)
        if (ierr .ne. 0) then
            print *,'ERROR initializing COSTA! '
            stop
        endif
    endif

    ! store current instance if needed
    if  (instances_in_memory == max_instances_in_memory) then
       call d3da_store_current_instance(storage_only_to_disk,'default')
       instances_in_memory = instances_in_memory - 1
       state_in_memory(current_instance) = .false.               
       state_handles(instance_count) = state_handles(current_instance) 
         ! This will be, in general, <> uncreated_state. So d3da_create_state_vector will
         ! not allocate new memory but instead use the state structure of the old state. 
    endif 

    current_instance = instance_count

    isbackground = (current_instance == 0)

    call d3da_create_cta_state_vector(isbackground, ierr)

    instance_id = current_instance
    
    ! memory/file management
    state_in_memory(current_instance) = .true.
    instances_in_memory = instances_in_memory + 1
    
    if (ierr /= 0) then
        instance_id = ierr  ! this is negative!
    endif

    instance_count = instance_count + 1    

end function d3da_create_instance


!-------------------------------------------
subroutine d3da_select_new_instance(instance_id)

    ! arguments
    integer, intent(in) :: instance_id

    !local
    integer :: ierr

    
    ! TODO: check id and throw error if not OK
    
    ! First check if instance is already selected
    if (current_instance /= instance_id) then
    
       ! First store the old instance (it may be necessary to save to disk
       ! if the new instance is not in memory yet)
       if (instances_in_memory == max_instances_in_memory .and.    &
               (.not. state_in_memory(instance_id))) then

          call d3da_store_current_instance(storage_whole_range,'default') 


          ! state handle is now free, so reserve for the new instance!
          state_handles(instance_id) = state_handles(current_instance) 
                
          instances_in_memory = instances_in_memory - 1
          state_in_memory(current_instance) = .false.

       else
          ! saving to disk is not necessary
          call d3da_store_current_instance(storage_only_at_ctastate,'default')
       endif
       
       current_instance = instance_id

    
       ! If necessary: Load the new instance from disk
       if (.not. state_in_memory(current_instance)) then
          call d3da_retrieve_state_from_disk(current_instance,'default')

          instances_in_memory = instances_in_memory + 1
          state_in_memory(current_instance) = .true.          
       endif
    
       ! set the instance state towards delft3d
       call d3da_set_ctastate_to_d3d(ierr)
    endif

end subroutine d3da_select_new_instance

!-------------------------------------------
subroutine d3da_select_instance_from_restartfile(instance_id, filename)

    ! arguments
    integer, intent(in)         :: instance_id
    character(Len=*), intent(in) :: filename  

    !local
    integer :: ierr

    
    ! TODO: check id and throw error if not OK
    
    ! Also load the state if instance is already selected

    
       ! store the old instance (it may be necessary to save to disk
       ! if the new instance is not in memory yet)
       
       ! DO NOT store if the selected instance equals the current instance!
       if ( current_instance /= instance_id) then
          if (instances_in_memory == max_instances_in_memory .and.    &
               (.not. state_in_memory(instance_id))) then

             call d3da_store_current_instance(storage_whole_range,'default') 


             ! state handle is now free, so reserve for the new instance!
             state_handles(instance_id) = state_handles(current_instance) 
                
             instances_in_memory = instances_in_memory - 1
             state_in_memory(current_instance) = .false.

          else
          ! saving to disk is not necessary
             call d3da_store_current_instance(storage_only_at_ctastate,'default')
          endif
       endif
       
       current_instance = instance_id

    
       ! ALWAYS: Load the new instance from disk
       
       call d3da_retrieve_state_from_disk(current_instance, filename)

       if (.not. state_in_memory(current_instance)) instances_in_memory = instances_in_memory + 1
       state_in_memory(current_instance) = .true.          
      
    
       ! set the instance state towards delft3d
       call d3da_set_ctastate_to_d3d(ierr)

end subroutine d3da_select_instance_from_restartfile

!-------------------------------------------------------------------
subroutine d3da_store_current_instance(storage_level, status)

   implicit none

   integer :: storage_level
   character(len=*), intent(in) :: status
   integer :: ierr

   !  first get the instance state from delft3d.
   ! if the cta_state is already up to date, this is not necessary!
   if (storage_level == storage_only_at_ctastate .or.            &
       storage_level == storage_whole_range) then 
       call d3da_get_ctastate_from_d3d(ierr)
   endif   
   !  If necessary: Save the  instance to disk
   !  TODO
   if (storage_level == storage_only_to_disk .or.            &
       storage_level == storage_whole_range) then 
       call d3da_save_instance_to_disk(current_instance, status) 
   endif


end subroutine d3da_store_current_instance

!-----------------------------------------------------

subroutine d3da_store_current_instance_restartfile(filename)

   implicit none

   character(Len=*), intent(in) :: filename  

   call d3da_store_current_instance(storage_whole_range, filename)


end subroutine d3da_store_current_instance_restartfile



!---------------------------------------------
subroutine d3da_retrieve_state_from_disk(instance_id, status)
   implicit none
   include 'cta_f77.inc'   

   integer :: instance_id
   character(len=*) :: status
   integer :: file_handle, sFilename, sReadmode
   integer :: ierr
   character(len=10)  :: instance_as_string
   character(len=256) :: fname

!  note: file contains only one time level, of the entire state
!  The file is opened, read, and immediately closed again 
!  TODO delete the file? 

   if (status == 'default' .or. status == '') then  ! assign standard name for file
      write(instance_as_string,'(i3.3)') instance_id
      fname='RSTstate_'//trim(instance_as_string)//'.nc'
   else
      fname=status
   endif

   call CTA_STRING_CREATE(sFilename,ierr)
   call CTA_STRING_SET(sFileName,fname,ierr)
   call CTA_FILE_CREATE(file_handle,ierr)
   call CTA_STRING_CREATE(sReadmode,ierr)
   call CTA_STRING_SET(sReadmode,"r",ierr)   
   ! the netcdf-file should already exist; so nc_create should not be called!
   call CTA_FILE_OPEN(file_handle,sFileName,sReadmode,ierr) 
   if (ierr .ne. 0) then
      print *,'ERROR: reading stored state instance : ',fname
   endif
   
 
   call CTA_TREEVECTOR_IMPORT(state_handles(instance_id),file_handle,ierr)
   if (ierr .ne. 0) then
     print *,'ERROR: cta_treevector_import for filename',fname
   endif
   ! close netcdf-file
   call CTA_FILE_FREE(file_handle,ierr)
   call CTA_STRING_FREE(sFilename,ierr)
   call CTA_STRING_FREE(sReadmode,ierr)

end subroutine d3da_retrieve_state_from_disk

!---------------------------------------------
subroutine d3da_save_instance_to_disk(instance_id, status)
   implicit none
   include 'cta_f77.inc'   

   integer, intent(in)          :: instance_id
   character(len=*), intent(in) :: status
    
   integer :: file_handle, sFilename, sWritemode
   integer :: ierr
   character(len=10)  :: instance_as_string
   character(len=256) :: fname
   
!  note: only one time level is saved, of the entire state
!  The fle is created or opened, written, and immediately closed again 

   if (status =='default' .or. status =='') then  ! assign standard name for file
      write(instance_as_string,'(i3.3)') instance_id
      fname='RSTstate_'//trim(instance_as_string)//'.nc'
   else
      fname = status  ! explicit filename has been provided
   
   endif
   
   call CTA_STRING_CREATE(sFilename,ierr)
   call CTA_STRING_SET(sFileName,fname,ierr)
   call CTA_FILE_CREATE(file_handle,ierr)
   call CTA_STRING_CREATE(sWritemode,ierr)
   call CTA_STRING_SET(sWritemode,"w",ierr)
   call CTA_FILE_OPEN(file_handle,sFileName,sWritemode,ierr)
 
   call CTA_TREEVECTOR_EXPORT(state_handles(instance_id),file_handle,ierr)
   ! close netcdf-file
   call CTA_FILE_FREE(file_handle,ierr)
   call CTA_STRING_FREE(sFilename,ierr)
   call CTA_STRING_FREE(sWritemode,ierr)
   
end subroutine d3da_save_instance_to_disk


!-----------------------------------------------
subroutine d3da_getinstancesize(inst_size)

  implicit none
  include 'cta_f77.inc'
  integer :: inst_size, ierr
   
  ! not necessary to update the state; size does not change
   call CTA_TREEVECTOR_GETSIZE(state_handles(current_instance), inst_size, ierr)
   if (ierr .ne. CTA_OK) then
      print *,'d3da_getinstancesize: error getting size ', ierr
      inst_size = -1
   endif   

end subroutine  d3da_getinstancesize

!-------------------------------------------------------------------
subroutine d3da_getcorestate(corestate, size_corestate, retval)

! note: we would like to get the state directly from d3d_state and NOT from Costa!
! however, we need it in 1D. So we first get the costa state and then perform a getvals.
! Cumbersome, but this is only temporary.

  implicit none
 
  include 'cta_f77.inc'
  
  integer, intent (out) :: retval
  integer :: s_core,  ierr, size_corestate
  double precision, dimension(size_corestate) :: corestate

  call d3da_get_ctastate_from_d3d(ierr)


  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  call CTA_TREEVECTOR_GETVALS(s_core, corestate, size_corestate, CTA_DOUBLE,ierr)
  if (ierr .ne. CTA_OK) then
     print *,'d3da_getcorestate: error getting values ',size_corestate
     retval = -1
  else
     retval = 0
  endif   

   
end subroutine d3da_getcorestate

!-------------------------------------------------------------------
!-------------------------------------------------------------------
subroutine d3da_setcorestate(corestate,size_corestate,retval)
! note: we would like to set the state directly to d3d_state and NOT to Costa!
! however, we provide it in 1D. So we first set the costa state and then perform a setvals.
! Cumbersome, but this is only temporary.

  implicit none

 
  include 'cta_f77.inc'
  integer :: size_corestate 
  double precision, dimension(size_corestate), intent(in) :: corestate
  integer, intent(out) :: retval
  integer :: s_core, ierr

  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  call CTA_TREEVECTOR_SETVALS(s_core, corestate, size_corestate, CTA_DOUBLE,ierr)  
  if (ierr .ne. CTA_OK) then
      print *,'d3da_setcorestate: error setting values ',size_corestate
      retval = -1
  else

     call d3da_set_ctastate_to_d3d(ierr)
     retval = ierr
  endif       
         
end subroutine d3da_setcorestate


!-------------------------------------------------------------------

subroutine d3da_create_cta_state_vector(isbackground, ierr)
 
  include 'cta_f77.inc'
 !arguments
  integer         , intent(out) :: ierr          ! error status
  logical         , intent(in)  :: isbackground  ! is this the background state?
  
  !Local variables
  integer :: sfilename
  character(len=10) :: instance_as_string
  ! Sub-treevectors
  integer ::s_core, s_pseudo, s_output
  ! Sub-treevectors
  integer :: s_sep, s_u,s_v,s_dp,s_umnldf,s_vmnldf,s_rtur1,s_r1
  integer :: s_w, s_vicww, s_vicuv, s_kfs, s_kfu, s_kfv, s_hydrbc, s_procbc, s_disch, s_rint
  integer :: s_names, s_timeadmin
  ! Array of sub-treeVectors
  integer, dimension(3) :: sub_states
  integer, dimension(8) :: sub_cores
  integer, dimension(12) :: sub_pseudos
  integer :: hdescr    ! Costa metainfo handle
  integer :: ier2
  integer :: nsub      ! length of sub-treevector
! prepare output file : create and open
  write(instance_as_string,'(i3.3)') current_instance
  file_names(current_instance)='cta_state_'//trim(instance_as_string)// trim(state_file_extension)

  ! file creating and opening is not needed yet!
  ! That part has moved to an actual corestate-export; there
  ! the first check is if the file already has been opened.

  !
  
  if (state_handles(current_instance) == UNCREATED_STATE) then
    ! this will in general be the case for new instances. Costa will create a handle
    ! for the treevector. Later, when the tv is filled, memory will be allocated. 
    ! However, if a handle already exists, no new tv-handles will be created. Later,
    ! when the tv is filled, no more extra memory is allocated. The values will
    ! be overwritten so they should have been saved before! 

      call CTA_TREEVECTOR_CREATE('whole state','d3d',state_handles(current_instance), ierr) ;if (ierr/=CTA_OK) goto 9999
      call CTA_TREEVECTOR_CREATE('core state',  'core',   s_core, ierr); if (ierr/=CTA_OK) goto 9999
      call CTA_TREEVECTOR_CREATE('pseudo state', 'pseudo', s_pseudo, ierr); if (ierr/=CTA_OK) goto 9999
      call CTA_TREEVECTOR_CREATE('output state', 'output', s_output, ierr); if (ierr/=CTA_OK) goto 9999
       
      call CTA_TREEVECTOR_CREATE('waterlevel','sep',s_sep, ierr)   
      call CTA_TREEVECTOR_CREATE('u-vel','u',s_u, ierr)
      call CTA_TREEVECTOR_CREATE('v-vel','v',s_v, ierr)
      call CTA_TREEVECTOR_CREATE('dp','dp',s_dp, ierr)
      call CTA_TREEVECTOR_CREATE('umnldf','umnldf',s_umnldf, ierr)
      call CTA_TREEVECTOR_CREATE('vmnldf','vmnldf',s_vmnldf, ierr)
      call CTA_TREEVECTOR_CREATE('turbulence','rtur1',s_rtur1, ierr)
      call CTA_TREEVECTOR_CREATE('concentrations','r1',s_r1, ierr)    
      if (ierr/=CTA_OK) goto 9999
      
       ! now add metainfo to substates. First: sep
       call CTA_METAINFO_CREATE(hdescr,ierr)
       if (ierr .ne. 0) goto 9999
       call CTA_METAINFO_SETREGGRID(hdescr, 'sepGrid',d3d_dims(2)-d3d_dims(1)+1 ,&
                                        d3d_dims(4)- d3d_dims(3)+1 , 1, &
                                    0.0d0, 0.0d0, 0.0d0,            &
                                    1.0d0, 1.0d0, 0.0d0, ierr)
       if (ierr .ne. 0) goto 9999
       call CTA_TREEVECTOR_SETMETAINFO(s_sep, hdescr, ierr);
       call CTA_TREEVECTOR_SETMETAINFO(s_dp, hdescr, ierr);
       call CTA_TREEVECTOR_SETMETAINFO(s_umnldf, hdescr, ierr);
       call CTA_TREEVECTOR_SETMETAINFO(s_vmnldf, hdescr, ierr);  

       if (ierr .ne. 0) goto 9999
       call CTA_METAINFO_SETREGGRID(hdescr, 'dpuvGrid',d3d_dims(2)-d3d_dims(1)+1 ,&
                                        d3d_dims(4)- d3d_dims(3)+1 , d3d_dims(5), &
                                    0.0d0, 0.0d0, 0.0d0,            &
                                    1.0d0, 1.0d0, 0.0d0, ierr)
       if (ierr .ne. 0) goto 9999                                
       call CTA_TREEVECTOR_SETMETAINFO(s_u, hdescr, ierr);
          if (ierr .ne. 0) goto 9999
       call CTA_TREEVECTOR_SETMETAINFO(s_v, hdescr, ierr);
       if (ierr/=CTA_OK) goto 9999
       if (d3d_dims(5)*d3d_dims(7).gt.0) then
          call CTA_METAINFO_SETREGGRID(hdescr, 'r1Grid',d3d_dims(2)-d3d_dims(1)+1 ,&
                                        d3d_dims(4)- d3d_dims(3)+1 , d3d_dims(5)*d3d_dims(7), &
                                    0.0d0, 0.0d0, 0.0d0,            &
                                    1.0d0, 1.0d0, 0.0d0, ierr)       
         ! note that we have to sweep together the kmax and the lstsci fro the r1; otherswise it will
         ! not fit in 3D.    
          call CTA_TREEVECTOR_SETMETAINFO(s_r1, hdescr, ierr);
       endif
       
       if (ierr/=CTA_OK) goto 9999    
       
    !  turbulence. Note; this array may be dummy of size 1 in case of no turbulence!
       if (d3d_dims(6) .gt.0) then 
         call CTA_METAINFO_SETREGGRID(hdescr, 'turbGrid',d3d_dims(2)-d3d_dims(1)+1 ,&
                                        d3d_dims(4)- d3d_dims(3)+1 , d3d_dims(5)+1, &
                                    0.0d0, 0.0d0, 0.0d0,            &
                                    1.0d0, 1.0d0, 0.0d0, ierr)   
         call CTA_TREEVECTOR_SETMETAINFO(s_rtur1, hdescr, ierr);
       endif   
       ! VORtech: not sure if metainfo should be freed now
       call CTA_METAINFO_FREE(hdescr,ier2)  
       if (ier2/=CTA_OK) then
           print *,'error in freeing metainfo'   
           pause
       endif
     
       sub_cores(1)=s_sep
       sub_cores(2)=s_u
       sub_cores(3)=s_v
       sub_cores(4)=s_dp
       sub_cores(5)=s_umnldf
       sub_cores(6)=s_vmnldf
       sub_cores(7)=s_rtur1
       sub_cores(8)=s_r1

       ! concatenate all core substates substate 'core'
       call CTA_TREEVECTOR_CONC(s_core, sub_cores, 8, ierr); if (ierr/=CTA_OK) goto 9999

    ! now: pseudo state
      call CTA_TREEVECTOR_CREATE('w','w',s_w, ierr)   
      call CTA_TREEVECTOR_CREATE('vicww','vicww',s_vicww, ierr)
      call CTA_TREEVECTOR_CREATE('vicuv','vicuv',s_vicuv, ierr)
      call CTA_TREEVECTOR_CREATE('kfs','kfs',s_kfs, ierr)
      call CTA_TREEVECTOR_CREATE('kfu','kfu',s_kfu, ierr)
      call CTA_TREEVECTOR_CREATE('kfv','kfv',s_kfv, ierr)
      call CTA_TREEVECTOR_CREATE('procbc','procbc',s_procbc, ierr)
      call CTA_TREEVECTOR_CREATE('hydrbc','hydrbc',s_hydrbc, ierr)
      call CTA_TREEVECTOR_CREATE('rint','rint',s_rint, ierr)
      call CTA_TREEVECTOR_CREATE('disch','disch',s_disch, ierr)

      call CTA_TREEVECTOR_CREATE('names','names',s_names, ierr)
      call CTA_TREEVECTOR_CREATE('timeadmin','timeadmin',s_timeadmin, ierr)  
      sub_pseudos(1)=s_w
      sub_pseudos(2)=s_vicww
      sub_pseudos(3)=s_vicuv
      sub_pseudos(4)=s_kfs
      sub_pseudos(5)=s_kfu
      sub_pseudos(6)=s_kfv
      sub_pseudos(7)=s_procbc
      sub_pseudos(8)=s_hydrbc
      sub_pseudos(9)=s_rint
      sub_pseudos(10)=s_disch
      sub_pseudos(11)=s_names
      sub_pseudos(12)=s_timeadmin  

      
     ! concatenate all pseudo substates to substate 'pseudo'
      call CTA_TREEVECTOR_CONC(s_pseudo, sub_pseudos, 12, ierr); if (ierr/=CTA_OK) goto 9999

      sub_states(1) = s_core
      sub_states(2) = s_pseudo
      sub_states(3) = s_output 
       ! concatenate the three main substates in d3d state
      call CTA_TREEVECTOR_CONC(state_handles(current_instance), sub_states, 3, ierr); if (ierr/=CTA_OK) goto 9999
       
      ! set 'nocompute' flag for pseudo and output   
      call CTA_TREEVECTOR_SETSUBTREENOCOMPUTE(state_handles(current_instance), 'pseudo',ierr)
      call CTA_TREEVECTOR_SETSUBTREENOCOMPUTE(state_handles(current_instance), 'output',ierr)

  endif
  ! --endif of section "create new treevectors"

  ! now initialize the values of the state
  ! note: if the state is 'old', this does not require extra memory!
  call d3da_get_ctastate_from_d3d(ierr)

  ! now adjust output part of d3d_state, if necessary
  if (isbackground) then
     call define_background_state(1)
  else
     call define_ordinary_state(1)     
  endif 
   ! and retrieve it again (cumbersome, but occurs only here in initialization)
  call d3da_get_ctastate_from_d3d(ierr)

  ! compute offsets for variables in state vector
  sub_core_offsets(1) = 1
  call CTA_TREEVECTOR_GETSIZE(s_sep   , nsub ,ierr)
  sub_core_offsets(2) = sub_core_offsets(1) + nsub
  call CTA_TREEVECTOR_GETSIZE(s_u     , nsub,ierr)
  sub_core_offsets(3) = sub_core_offsets(2) + nsub
  call CTA_TREEVECTOR_GETSIZE(s_v     , nsub,ierr)
  sub_core_offsets(4) = sub_core_offsets(3) + nsub
  call CTA_TREEVECTOR_GETSIZE(s_dp    , nsub,ierr)
  sub_core_offsets(5) = sub_core_offsets(4) + nsub
  call CTA_TREEVECTOR_GETSIZE(s_umnldf, nsub,ierr)
  sub_core_offsets(6) = sub_core_offsets(5) + nsub
  call CTA_TREEVECTOR_GETSIZE(s_vmnldf, nsub,ierr)
  sub_core_offsets(7) = sub_core_offsets(6) + nsub
  call CTA_TREEVECTOR_GETSIZE(s_rtur1 , nsub,ierr)
  sub_core_offsets(8) = sub_core_offsets(7) + nsub
  call CTA_TREEVECTOR_GETSIZE(s_r1    , nsub,ierr)
  sub_core_offsets(9) = sub_core_offsets(8) + nsub

  return

 9999 continue
   print *,'FATAL ERROR IN d3da_create_cta_state_vector ierr=',ierr
   call exit(-1)

end subroutine d3da_create_cta_state_vector
!----------------------------------


!-----------------
subroutine d3da_ctastate_to_netcdf(ierr)
 
  include 'cta_f77.inc'

 !arguments
  integer, intent(out) :: ierr

  ! local
  integer :: s_core  ! COSTA handle to core substate
  integer :: sfilename 

  ! first check if the file already has been created!
  if (file_handles(current_instance) == UNCREATED_HANDLE) then 
     call CTA_STRING_CREATE(sFilename,ierr)
     call CTA_STRING_SET(sFileName,file_names(current_instance),ierr)
     call CTA_FILE_CREATE(file_handles(current_instance),ierr)
     call CTA_FILE_OPEN(file_handles(current_instance),sFileName,CTA_NULL,ierr)
     call CTA_STRING_FREE(sFilename,ierr)
  endif

 
 ! only export core state!
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  call CTA_TREEVECTOR_EXPORT(s_core,file_handles(current_instance),ierr)
  if (ierr .ne. CTA_OK) then
     print *, 'ERROR ctastate_to_netcdf ',ierr
  endif

end subroutine d3da_ctastate_to_netcdf  
!--------------------------------


subroutine d3da_close_cta_state_files(ierr)
 
  include 'cta_f77.inc'
 !arguments
  integer, intent(out) :: ierr
  
  integer :: i

  do i=0, instance_count-1
    if (file_handles(i) /= UNCREATED_HANDLE) then
       call CTA_FILE_FREE(file_handles(i),ierr)
    endif  
  enddo
! close open files

end subroutine d3da_close_cta_state_files


!----------------------------------------------------------------

subroutine d3da_set_ctastate_to_d3d(ierr)

  ! use a d3d_state (dimension 1!) as bridge
  
  include 'cta_f77.inc'
 !arguments
 ! (none)

 !local parameters
  integer :: s_core, s_pseudo, s_output
  integer :: s_sep, s_u,s_v,s_dp,s_umnldf,s_vmnldf,s_rtur1,s_r1
  integer :: s_w, s_vicww, s_vicuv, s_kfs, s_kfu, s_kfv, s_hydrbc, s_procbc, s_disch, s_rint
  integer :: s_names, s_timeadmin  
  integer :: hnames(3)
  integer :: vals_timeadmin(9), vals_output(11)
  integer :: ierr, i

! use getsubtreevec! (by reference, so no need to free)
! immediately fill d3d_state
! NOTE we can not doe one large getvals because not all are CTA_doubles

  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'sep', s_sep, ierr)  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'u', s_u, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'v', s_v, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'dp', s_dp, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'umnldf', s_umnldf, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'vmnldf', s_vmnldf, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'rtur1', s_rtur1, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'r1', s_r1, ierr)
          
  call CTA_TREEVECTOR_GETVALS(s_sep, d3d_state(1)%core%sep, size_d3d_state%sep, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_GETVALS(s_u, d3d_state(1)%core%u, size_d3d_state%u, CTA_DOUBLE,ierr) 
  call CTA_TREEVECTOR_GETVALS(s_v, d3d_state(1)%core%v, size_d3d_state%u, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_GETVALS(s_dp, d3d_state(1)%core%dp, size_d3d_state%sep, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_GETVALS(s_umnldf, d3d_state(1)%core%umnldf, size_d3d_state%sep, CTA_DOUBLE,ierr) 
  call CTA_TREEVECTOR_GETVALS(s_vmnldf, d3d_state(1)%core%vmnldf, size_d3d_state%sep, CTA_DOUBLE,ierr)
  
  if (size_d3d_state%rtur .gt. 0) call CTA_TREEVECTOR_GETVALS(s_rtur1, d3d_state(1)%core%rtur, size_d3d_state%rtur, CTA_DOUBLE,ierr) 
  if (size_d3d_state%r1 .gt.0) call CTA_TREEVECTOR_GETVALS(s_r1, d3d_state(1)%core%r1, size_d3d_state%r1, CTA_DOUBLE,ierr)
  
! pseudo ...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'pseudo', s_pseudo, ierr)
  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'w', s_w, ierr)  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'vicww', s_vicww, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'vicuv', s_vicuv, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'kfs', s_kfs, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'kfu', s_kfu, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'kfv', s_kfv, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'procbc', s_procbc, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'hydrbc', s_hydrbc, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'rint', s_rint, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'disch', s_disch, ierr)  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'names', s_names, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'timeadmin', s_timeadmin, ierr) 
  
  call CTA_TREEVECTOR_GETVALS(s_w, d3d_state(1)%pseudo%w, size_d3d_state%w, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_GETVALS(s_vicww, d3d_state(1)%pseudo%vicww, size_d3d_state%w, CTA_DOUBLE,ierr) 
  call CTA_TREEVECTOR_GETVALS(s_vicuv, d3d_state(1)%pseudo%vicuv, size_d3d_state%vicuv, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_GETVALS(s_kfs, d3d_state(1)%pseudo%kfs, size_d3d_state%kfs, CTA_INTEGER,ierr)
  call CTA_TREEVECTOR_GETVALS(s_kfu, d3d_state(1)%pseudo%kfu, size_d3d_state%kfs, CTA_INTEGER,ierr) 
  call CTA_TREEVECTOR_GETVALS(s_kfv, d3d_state(1)%pseudo%kfv, size_d3d_state%kfs, CTA_INTEGER,ierr)
  if (size_d3d_state%procbc .gt. 0) call CTA_TREEVECTOR_GETVALS(s_procbc, d3d_state(1)%pseudo%procbc, size_d3d_state%procbc, CTA_DOUBLE,ierr) 
  if (size_d3d_state%hydrbc .gt. 0) call CTA_TREEVECTOR_GETVALS(s_hydrbc, d3d_state(1)%pseudo%hydrbc, size_d3d_state%hydrbc, CTA_DOUBLE,ierr)  
  if (size_d3d_state%rint .gt. 0) call CTA_TREEVECTOR_GETVALS(s_rint, d3d_state(1)%pseudo%rint, size_d3d_state%rint, CTA_DOUBLE,ierr) 
  if (size_d3d_state%disch .gt. 0) call CTA_TREEVECTOR_GETVALS(s_disch,d3d_state(1)%pseudo%disch, size_d3d_state%disch,CTA_DOUBLE,ierr)  
  !strings
  do i=1,3
    call CTA_STRING_CREATE(hnames(i),ierr)
  enddo
  call CTA_TREEVECTOR_GETVALS(s_names,hnames,3,CTA_STRING,ierr)
  call CTA_STRING_GET(hnames(1),d3d_state(1)%pseudo%runid,ierr)
  call CTA_STRING_GET(hnames(2),d3d_state(1)%pseudo%trifil,ierr)  
  call CTA_STRING_GET(hnames(3),d3d_state(1)%pseudo%comfil,ierr)
  do i=1,3
    call CTA_STRING_FREE(hnames(i),ierr)
  enddo
 
  ! time admin
  call CTA_TREEVECTOR_GETVALS(s_timeadmin,vals_timeadmin,9,CTA_INTEGER,ierr)  
  d3d_state(1)%pseudo%timestep = vals_timeadmin(1)
  d3d_state(1)%pseudo%trisol_ifirst = vals_timeadmin(2)  
  d3d_state(1)%pseudo%gdinttim_itstrt = vals_timeadmin(3)  
  d3d_state(1)%pseudo%gdinttim_itfinish = vals_timeadmin(4)
  d3d_state(1)%pseudo%gdinttim_ntstep = vals_timeadmin(5)
  d3d_state(1)%pseudo%gdinttim_itstop = vals_timeadmin(6)  
  d3d_state(1)%pseudo%gdinttim_itinit = vals_timeadmin(7)  
  d3d_state(1)%pseudo%gdinttim_time_nodal_update_bnd = vals_timeadmin(8)  
  d3d_state(1)%pseudo%gdinttim_time_nodal_update_tgf = vals_timeadmin(9)   
  
! output...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'output', s_output, ierr)
  call CTA_TREEVECTOR_GETVALS(s_output, vals_output, 11, CTA_INTEGER,ierr)
  d3d_state(1)%output%itcomc = vals_output(1)
  d3d_state(1)%output%ithisc = vals_output(2)
  d3d_state(1)%output%ithisi = vals_output(3)
  d3d_state(1)%output%iphisc = vals_output(4)    
  d3d_state(1)%output%ipmapc = vals_output(5)
  d3d_state(1)%output%itmapc = vals_output(6)
  d3d_state(1)%output%itmapi = vals_output(7)
  d3d_state(1)%output%itmapl = vals_output(8) 
  d3d_state(1)%output%itdroi = vals_output(9)
  d3d_state(1)%output%nofou = vals_output(10)
  d3d_state(1)%output%itrstc = vals_output(11)  
    
  ! now d3d_state is completely filled and it can be passed to the model d3d itself.
  call set_d3d_state_to_d3d(1)

end subroutine d3da_set_ctastate_to_d3d


!---------------------------------------------

subroutine d3da_get_ctastate_from_d3d(ierr)

  ! use a d3d_state (dimension 1!) as bridge
  
  include 'cta_f77.inc'
 !arguments

  integer, intent(out) :: ierr


!local parameters
  integer :: s_core, s_pseudo, s_output
  integer :: s_sep, s_u,s_v,s_dp,s_umnldf,s_vmnldf,s_rtur1,s_r1
  integer :: s_w, s_vicww, s_vicuv, s_kfs, s_kfu, s_kfv, s_hydrbc, s_procbc, s_disch, s_rint
  integer :: s_names, s_timeadmin  
  integer :: hnames(3)  ! three CTA_Strings
  integer :: vals_timeadmin(9), vals_output(11)
  integer :: i
  double precision :: rdum(1)

  rdum(1) = 0.0d0

  ! first get d3d_state from the model d3d.
  call get_d3d_state_from_d3d(1)

! use getsubtreevec! (by reference, so no need to free)
! immediately fill d3d_state
! NOTE we can not doe one large getvals because not all are CTA_doubles

  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'core', s_core, ierr)
  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'sep', s_sep, ierr)  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'u', s_u, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'v', s_v, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'dp', s_dp, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'umnldf', s_umnldf, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'vmnldf', s_vmnldf, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'rtur1', s_rtur1, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_core, 'r1', s_r1, ierr)
          
  call CTA_TREEVECTOR_SETVALS(s_sep, d3d_state(1)%core%sep, size_d3d_state%sep, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_SETVALS(s_u, d3d_state(1)%core%u, size_d3d_state%u, CTA_DOUBLE,ierr) 
  call CTA_TREEVECTOR_SETVALS(s_v, d3d_state(1)%core%v, size_d3d_state%u, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_SETVALS(s_dp, d3d_state(1)%core%dp, size_d3d_state%sep, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_SETVALS(s_umnldf, d3d_state(1)%core%umnldf, size_d3d_state%sep, CTA_DOUBLE,ierr) 
  call CTA_TREEVECTOR_SETVALS(s_vmnldf, d3d_state(1)%core%vmnldf, size_d3d_state%sep, CTA_DOUBLE,ierr)
  if (size_d3d_state%rtur .gt. 0) then
    call CTA_TREEVECTOR_SETVALS(s_rtur1, d3d_state(1)%core%rtur, size_d3d_state%rtur, CTA_DOUBLE,ierr) 
  else  ! make a state of size 1
    call CTA_TREEVECTOR_SETVALS(s_rtur1, rdum, 1, CTA_DOUBLE,ierr)
  endif 
  if (size_d3d_state%r1 .gt.0) then
     call CTA_TREEVECTOR_SETVALS(s_r1, d3d_state(1)%core%r1, size_d3d_state%r1, CTA_DOUBLE,ierr)
  else ! make a state of size 1
     call CTA_TREEVECTOR_SETVALS(s_r1, rdum, 1, CTA_DOUBLE,ierr) 
  endif
  
! pseudo ...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'pseudo', s_pseudo, ierr)
  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'w', s_w, ierr)  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'vicww', s_vicww, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'vicuv', s_vicuv, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'kfs', s_kfs, ierr) 
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'kfu', s_kfu, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'kfv', s_kfv, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'procbc', s_procbc, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'hydrbc', s_hydrbc, ierr)
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'rint', s_rint, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'disch', s_disch, ierr)  
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'names', s_names, ierr)    
  call CTA_TREEVECTOR_GETSUBTREEVEC(s_pseudo, 'timeadmin', s_timeadmin, ierr) 
  
  call CTA_TREEVECTOR_SETVALS(s_w, d3d_state(1)%pseudo%w, size_d3d_state%w, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_SETVALS(s_vicww, d3d_state(1)%pseudo%vicww, size_d3d_state%w, CTA_DOUBLE,ierr) 
  call CTA_TREEVECTOR_SETVALS(s_vicuv, d3d_state(1)%pseudo%vicuv, size_d3d_state%vicuv, CTA_DOUBLE,ierr)
  call CTA_TREEVECTOR_SETVALS(s_kfs, d3d_state(1)%pseudo%kfs, size_d3d_state%kfs, CTA_INTEGER,ierr)
  call CTA_TREEVECTOR_SETVALS(s_kfu, d3d_state(1)%pseudo%kfu, size_d3d_state%kfs, CTA_INTEGER,ierr) 
  call CTA_TREEVECTOR_SETVALS(s_kfv, d3d_state(1)%pseudo%kfv, size_d3d_state%kfs, CTA_INTEGER,ierr)

  if (size_d3d_state%procbc .gt. 0) then
    call CTA_TREEVECTOR_SETVALS(s_procbc, d3d_state(1)%pseudo%procbc, size_d3d_state%procbc, CTA_DOUBLE,ierr) 
  else
    call CTA_TREEVECTOR_SETVALS(s_procbc, rdum, 1, CTA_DOUBLE,ierr)
  endif

  if (size_d3d_state%hydrbc .gt. 0) then
    call CTA_TREEVECTOR_SETVALS(s_hydrbc, d3d_state(1)%pseudo%hydrbc, size_d3d_state%hydrbc, CTA_DOUBLE,ierr)  
  else
    call CTA_TREEVECTOR_SETVALS(s_hydrbc, rdum, 1, CTA_DOUBLE,ierr) 
  endif

  if (size_d3d_state%rint .gt. 0) then
    call CTA_TREEVECTOR_SETVALS(s_rint, d3d_state(1)%pseudo%rint, size_d3d_state%rint, CTA_DOUBLE,ierr) 
  else
    call CTA_TREEVECTOR_SETVALS(s_rint, rdum, 1, CTA_DOUBLE,ierr)
  endif

  if (size_d3d_state%disch .gt. 0) then  
    call CTA_TREEVECTOR_SETVALS(s_disch,d3d_state(1)%pseudo%disch, size_d3d_state%disch,CTA_DOUBLE,ierr)  
  else
    call CTA_TREEVECTOR_SETVALS(s_disch, rdum, 1, CTA_DOUBLE,ierr)
  endif

  !strings
  do i=1,3
    call CTA_STRING_CREATE(hnames(i),ierr)
  enddo
  call CTA_STRING_SET(hnames(1),d3d_state(1)%pseudo%runid,ierr)
  call CTA_STRING_SET(hnames(2),d3d_state(1)%pseudo%trifil,ierr)  
  call CTA_STRING_SET(hnames(3),d3d_state(1)%pseudo%comfil,ierr)  
  call CTA_TREEVECTOR_SETVALS(s_names,hnames,3,CTA_STRING,ierr)
  do i=1,3
    call CTA_STRING_FREE(hnames(i),ierr)
  enddo

  ! time admin
  vals_timeadmin(1) = d3d_state(1)%pseudo%timestep 
  vals_timeadmin(2) =d3d_state(1)%pseudo%trisol_ifirst   
  vals_timeadmin(3) =d3d_state(1)%pseudo%gdinttim_itstrt  
  vals_timeadmin(4) =d3d_state(1)%pseudo%gdinttim_itfinish
  vals_timeadmin(5) =d3d_state(1)%pseudo%gdinttim_ntstep 
  vals_timeadmin(6) =d3d_state(1)%pseudo%gdinttim_itstop   
  vals_timeadmin(7) =d3d_state(1)%pseudo%gdinttim_itinit   
  vals_timeadmin(8) =d3d_state(1)%pseudo%gdinttim_time_nodal_update_bnd   
  vals_timeadmin(9) =d3d_state(1)%pseudo%gdinttim_time_nodal_update_tgf    
  call CTA_TREEVECTOR_SETVALS(s_timeadmin,vals_timeadmin,9,CTA_INTEGER,ierr) 
  
! output...
  call CTA_TREEVECTOR_GETSUBTREEVEC(state_handles(current_instance), 'output', s_output, ierr)

  vals_output(1) = d3d_state(1)%output%itcomc
  vals_output(2) =  d3d_state(1)%output%ithisc 
  vals_output(3) =d3d_state(1)%output%ithisi 
  vals_output(4) =  d3d_state(1)%output%iphisc     
  vals_output(5) =  d3d_state(1)%output%ipmapc
  vals_output(6) =  d3d_state(1)%output%itmapc 
  vals_output(7) =  d3d_state(1)%output%itmapi 
  vals_output(8) =  d3d_state(1)%output%itmapl 
  vals_output(9) =  d3d_state(1)%output%itdroi 
  vals_output(10) =  d3d_state(1)%output%nofou
  vals_output(11) =  d3d_state(1)%output%itrstc   
    
  call CTA_TREEVECTOR_SETVALS(s_output, vals_output, 11, CTA_INTEGER,ierr)
  
end subroutine d3da_get_ctastate_from_d3d

!----------------------------------------------------------------------

end module m_d3dstate_2_openda
