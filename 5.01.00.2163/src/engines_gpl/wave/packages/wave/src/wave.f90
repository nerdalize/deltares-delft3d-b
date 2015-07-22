program waves_main
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
!  $Id: wave.f90 1489 2012-05-15 17:12:58Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/wave/src/wave.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use swan_flow_grid_maps
   use swan_input
   use flow_data
   use sync_flowwave
   use wave_data
   use meteo
   !
   implicit none
!
! Global variables
!
!
! Local variables
!
   integer                                      :: i
   integer                                      :: ierr
   integer                                      :: mode_in
   integer                                      :: n_swan_grids ! number of SWAN grids
   integer                                      :: n_flow_grids ! number of FLOW grids
   integer                                      :: i_flow       ! counter
   integer                                      :: i_swan       ! counter
   integer                                      :: i_meteo      ! counter
   integer                                      :: timtscale    ! time in tscale units, integer representation
   integer                                      :: it01flow     ! reference date obtained from FLOW
   integer                                      :: command
   real                                         :: tscaleflow   ! basic time unit == flow time step (s)
   real(fp)       , dimension(:,:), allocatable :: x_fp         ! Copy of x-coordinate of grid in flexible precision, needed for meteo module
   real(fp)       , dimension(:,:), allocatable :: y_fp         ! Copy of y-coordinate of grid in flexible precision, needed for meteo module
   logical                                      :: success      ! flag indicating whether delftio communication went correct
   logical                                      :: mud
   character(20)                                :: tmpchar
   character(256)                               :: mdw_file     ! filename mdw file
   character(256), dimension(:), allocatable    :: meteotypes
   character(500)                               :: message
   type(wave_data_type),target                  :: wavedata
!
!! executable statements -----------------------------------------------
!
   !
   ! ====================================================================================
   ! INIT
   ! ====================================================================================
   !
   mode_in = 0
   select case(COMMAND_ARGUMENT_COUNT())
   case (1)
      call getarg(1,mdw_file)
   case (2)
      call getarg(1,mdw_file)
      call getarg(2,tmpchar)
      read(tmpchar,*,iostat=ierr) mode_in
   case default
      call usage()
      stop
   endselect
   call small(mdw_file,len(mdw_file))

   do i=1,256
      if (ichar(mdw_file(i:i)) == 0) mdw_file(i:i) = ' '
   enddo
   call checklicense(success)
   if ( .not. success ) then
      write(*,'(a)') '*** ERROR: No authorization'
      stop
   endif
   !
   call initialize_wavedata(wavedata)
   call setmode(wavedata, mode_in)
   call wave_init(mdw_file)
   !
   ! Read mdw file
   !
   call read_swan(mdw_file, swan_run, wavedata)
   n_swan_grids = swan_run%nnest
   !
   ! Initialisation from flow (write file runid(s))
   !
   if (wavedata%mode /= stand_alone) then
      it01flow = 0
      call flow_init(wavedata%mode, it01flow, tscaleflow)
      call settscale(wavedata%time, tscaleflow)
      if (wavedata%time%refdate == 0) then
         !
         ! No reference date in mdw-file or waves_alone
         ! Use reference date from flow
         !
         call setrefdate(wavedata%time, it01flow)
      else
         if (wavedata%time%refdate == it01flow) then
            !
            ! Reference date from flow is identical to reference date from mdw-file/waves_alone
            !
         else
            write(*,'(a,i8,a,i8,a)') '*** ERROR: Reference date from FLOW (', &
                & it01flow, ') differs from WAVE (', wavedata%time%refdate, ').'
            stop
         endif
      endif
   else
      !
      ! stand_alone, flow data may be used or written
      !
      if (swan_run%useflowdata .or. swan_run%swwav) then
         !
         ! In this case, refdate and tscale are read from the com-file
         ! Only tscale must be set in wavedata%time
         !
         call flow_init(wavedata%mode, it01flow, tscaleflow)
         call settscale (wavedata%time, tscaleflow)
      endif
   endif
   if (wavedata%time%refdate == 0) then
      write(*,'(a)') '*** ERROR: Reference date not set'
      write(*,'(a)') '           Use Delft3D-WAVE-GUI version 4.90.00 or higher to create the mdw-file.'
      stop
   else
      if (wavedata%time%refdate < 19000000 .or. wavedata%time%refdate > 22000000) then
         write(*,'(a,i8)') '*** ERROR: Unrealistic reference date ',wavedata%time%refdate
         stop
      endif
   endif
   !
   ! Read wave grids and flow grids; make grid-maps
   !
   call grids_and_gridmaps(n_swan_grids, n_flow_grids, swan_run, wavedata%mode)
   !
   ! Allocate swan output fields defined on flow grids; they have to be
   ! stored and updated over multiple nested swan runs
   !
   do i_flow=1,n_flow_grids
      flow_output_fields(i_flow)%n_outpars = 0
      call alloc_output_fields(flow_grids(i_flow),flow_output_fields(i_flow))
   enddo
   !
   ! Set mode to spherical if first swan grid is spherical
   !
   swan_run%sferic = swan_grids(1)%sferic
   !
   ! Meteo data from file?
   ! Only if 1 or more meteoitems have been specified
   ! and meteo information has not been received from FLOW
   !
   do i_swan = 1, n_swan_grids
      if (swan_run%dom(i_swan)%n_meteofiles_dom > 0 .and. swan_run%dom(i_swan)%qextnd(q_wind) == 0) then
         !
         ! Grid coordinates of all swan grids are needed by the meteo module
         !
         success  = initmeteo(swan_grids(i_swan)%grid_name)
         call checkmeteoresult_wave(success)
         !
         ! Allocate local copies of coordinate arrays
         ! Must be in flexible precision for the meteo module
         !
         allocate(x_fp(swan_grids(i_swan)%mmax,swan_grids(i_swan)%nmax))
         allocate(y_fp(swan_grids(i_swan)%mmax,swan_grids(i_swan)%nmax))
         x_fp = real(swan_grids(i_swan)%x, fp)
         y_fp = real(swan_grids(i_swan)%y, fp)
         !
         ! Read the meteo files
         !
         do i_meteo = 1, swan_run%dom(i_swan)%n_meteofiles_dom
            success = addmeteoitem(swan_grids(i_swan)%grid_name               , &
                                 & swan_run%dom(i_swan)%meteofile_dom(i_meteo), &
                                 & swan_grids(i_swan)%sferic                  , &
                                 & swan_grids(i_swan)%mmax                    , &
                                 & swan_grids(i_swan)%nmax                    )
            call checkmeteoresult_wave(success)
            !
            success  = gridtometeo(   swan_grids(i_swan)%grid_name, &
                                 &    swan_grids(i_swan)%nmax     , &
                                 &    swan_grids(i_swan)%mmax     , &
                                 & 1, swan_grids(i_swan)%nmax     , &
                                 & 1, swan_grids(i_swan)%mmax     , &
                                 &    swan_grids(i_swan)%kcs      , &
                                 &    x_fp                        , &
                                 &    y_fp                        )
            call checkmeteoresult_wave(success)
         enddo
         !
         ! Deallocate local copies of coordinate arrays
         !
         deallocate(x_fp)
         deallocate(y_fp)
         success = getmeteotypes(swan_grids(i_swan)%grid_name, meteotypes)
         call checkmeteoresult_wave(success)
         do i_meteo = 1, size(meteotypes)
            if (meteotypes(i_meteo) == "meteo_on_computational_grid") then
               write(*,'(a)') '*** ERROR: "meteo on computational grid" (flow grid) is not supported by Delft3D-WAVE'
               stop
            endif
         enddo
         deallocate(meteotypes)
      endif
   enddo
   !
   ! ====================================================================================
   ! CHECK
   ! ====================================================================================
   !
   call check_input(swan_run, wavedata)
   !
   ! ====================================================================================
   ! RUN
   ! ====================================================================================
   !
   if (wavedata%mode /= stand_alone) then
      !
      ! In combination with flow, perform the swan computation (including mapping etc.)
      !
      command = flow_wave_comm_perform_step
      do while (command == flow_wave_comm_perform_step)
         write(*,'(a)') 'Waiting for communication with Delft3D-FLOW ...'
         mud     = .false.
         success = wave_from_flow_command(command, mud, timtscale)
         if ( .not. success ) then
            write(*,'(a)') '*** ERROR: Communication with Delft3D-FLOW failed.'
            stop
         endif
         if (wavedata%mode == flow_mud_online) then
            write(*,'(a)') 'Waiting for communication with MUD ...'
            mud     = .true.
            success = wave_from_flow_command(command, mud, timtscale)
            if ( .not. success ) then
               write(*,'(a)') '*** ERROR: Communication with MUD layer failed.'
               stop
            endif
         endif
         write(*,'(a)')'*****************************************************************'
         write(*,'(a)')'*  Start of Delft3D-WAVE ...'
         !
         ! Update wave and wind conditions
         !
         if (timtscale >= 0) then
            call settimtscale(wavedata%time, timtscale)
            !
            ! Run n_swan nested SWAN run
            !
            call swan_tot(n_swan_grids, n_flow_grids, wavedata)
         endif
         write(*,'(a)')'*  End of Delft3D-WAVE'
         write(*,'(a)')'*****************************************************************'
         mud = .false.
         call wave_to_flow_status(flow_wave_comm_result_ok, mud)
         if (wavedata%mode == flow_mud_online) then
            mud = .true.
            call wave_to_flow_status(flow_wave_comm_result_ok, mud)
         endif
      enddo
      call deallocate_flow_data()
   else
      !
      ! Standalone swan computation
      ! Run n_swan nested SWAN runs
      !
      call swan_tot(n_swan_grids, n_flow_grids, wavedata)
   endif
   call del_temp_files(n_swan_grids)
   !
   ! Deallocate memory used by meteo module
   !
   do i_swan = 1, n_swan_grids
      call deallocmeteo(swan_grids(i_swan)%grid_name)
   enddo
   !   
   write(*,'(a)') 'Delft3D-WAVE finished normally.'
end program waves_main
