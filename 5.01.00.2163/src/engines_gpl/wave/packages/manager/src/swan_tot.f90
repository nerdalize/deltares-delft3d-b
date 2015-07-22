subroutine swan_tot (n_swan_grids, n_flow_grids, wavedata)
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
!  $Id: swan_tot.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/manager/src/swan_tot.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use swan_flow_grid_maps
   use swan_input
   use update_waves
   use wave_data
   use buffer
   use meteo
   !
   implicit none
!
! Global variables
!
   integer, intent(in)  :: n_flow_grids
   integer, intent(in)  :: n_swan_grids
   type(wave_data_type) :: wavedata
!
! Local variables
!
   integer                                       :: count
   integer                                       :: i_flow
   integer                                       :: i_swan
   integer                                       :: istat
   integer                                       :: itide
   integer                                       :: itidewrite
   integer                                       :: lunhot
   integer                         , external    :: new_lun
   real(fp)                                      :: wave_timezone
   real(fp)                                      :: wave_timmin
   real(fp)        , dimension(:,:), pointer     :: patm_fp
   real(fp)        , dimension(:,:), pointer     :: windu_fp
   real(fp)        , dimension(:,:), pointer     :: windv_fp
   logical                                       :: extr_var1
   logical                                       :: extr_var2
   logical                                       :: sumvars
   logical                                       :: positiveonly
   logical                                       :: success
   logical                                       :: exists
   logical                         , external    :: deletehotfile
   character(256)                                :: fname
   character(500)                                :: message
   type(swan_dom), pointer                       :: dom
!
!! executable statements -------------------------------------------------------
!
   !
   ! check to see if a wave map should be written
   !
   if (wavedata%mode == stand_alone) then
      call setwrite_wavm(wavedata%output, .true.)
   else
      if (wavedata%time%timsec >= wavedata%output%nexttim) then
         call setwrite_wavm(wavedata%output, .true.)
         call setnexttim   (wavedata%output, wavedata%time%timsec + swan_run%wavm_write_interval * 60.0)
      else
         call setwrite_wavm(wavedata%output, .false.)
      endif
      !
      ! Keep the hotfile when:
      !   current time >= nextint
      !   int2keephotfile is specified
      !
   endif
   do itide = 1, swan_run%nttide
      if (wavedata%output%write_wavm) then
         call setoutputcount(wavedata%output, wavedata%output%count + 1)
      endif
      !
      ! Set time in case of standalone run
      !
      if (wavedata%mode == stand_alone) call settimmin(wavedata%time, swan_run%timwav(itide))
      !
      ! Update wave and wind conditions
      !
      call update_wavecond(swan_run, wavedata%time)
      !
      ! Start loop over SWAN grids
      !
      write(*,'(a,f15.3)') '  Start loop over SWAN grids, time = ',wavedata%time%timmin
      do i_swan = 1, n_swan_grids
         dom => swan_run%dom(i_swan)

         write(*,'(a)') '  Allocate input fields'
         call alloc_input_fields (swan_grids(i_swan), swan_input_fields, wavedata%mode)
         call init_input_fields  (swan_input_fields, swan_run, itide)

         if (dom%curvibot==1) then
            write(*,'(a)') '  Allocate and read SWAN depth'
            call get_swan_depth (swan_input_fields,dom%botfil)
         endif
         !
         ! Vegetation map
         if (dom%vegetation == 1) then
            write(*,'(a)') '  Allocate and read Vegetation map'
            call get_vegi_map (swan_input_fields,dom%vegfil)
         endif
         ! If flow results are used
         !
         if (swan_run%useflowdata) then
            do i_flow = 1, n_flow_grids
               write(*,'(a,i0,a)') '  Get flow fields, domain ',i_flow,' :'
               call get_flow_fields (swan_input_fields, flow_grids(i_flow), &
                                   & flow2swan_maps(i_swan,i_flow), wavedata, swan_run, dom%flowVelocityType)
            enddo
         endif
         !
         ! Get meteo data from file?
         ! Only when using space varying meteo data and
         ! when meteo data has not been obtained from FLOW
         !
         if (dom%n_meteofiles_dom > 0 .and. dom%qextnd(q_wind) == 0) then
            wave_timezone = 0.0_fp
            wave_timmin   = real(wavedata%time%timmin, fp)
            success       = meteoupdate(swan_grids(i_swan)%grid_name, wavedata%time%refdate, wave_timezone, wave_timmin)
            call checkmeteoresult_wave(success)
            !
            ! update windu array
            !
            call get_buffer(windu_fp, swan_input_fields%mmax, swan_input_fields%nmax)
            success = getmeteoval(swan_grids(i_swan)%grid_name, 'windu'     , wave_timmin, &
                                & 1,1, 1, swan_input_fields%nmax   , &
                                &      1, swan_input_fields%mmax   , &
                                & windu_fp    , 0   )
            call checkmeteoresult_wave(success)
            swan_input_fields%windu = real(windu_fp, sp)
            !
            ! update windv array
            !
            call get_buffer(windv_fp, swan_input_fields%mmax, swan_input_fields%nmax)
            success = getmeteoval(swan_grids(i_swan)%grid_name, 'windv'     , wave_timmin, &
                                & 1, 1,  1, swan_input_fields%nmax   , &
                                &        1, swan_input_fields%mmax   , &
                                & windv_fp     , 0  )
            call checkmeteoresult_wave(success)
            swan_input_fields%windv = real(windv_fp, sp)
            !
            ! update patm array
            !
            !call get_buffer(patm_fp, swan_input_fields%mmax, swan_input_fields%nmax)
            !success = getmeteoval(swan_grids(i_swan)%grid_name, 'patm'      , wave_timmin, &
            !                    & 1, 1, 1, swan_input_fields%nmax   , &
            !                    &       1, swan_input_fields%mmax   , &
            !                    & patm_fp   , 0     )
            !call checkmeteoresult_wave(success)
            !swan_input_fields%patm = real(patm_fp, sp)
            !
            ! Deallocate buffer
            !
            call dealloc_buffer()
         endif
         !
         ! Extend FLOW data on this SWAN grid?
         !
         if (dom%curvibot == 1) then
            !
            ! Write SWAN depth file
            !
            write(*,'(a)') '  Write SWAN depth file'
            sumvars      = .true.
            positiveonly = .false.
            extr_var1 = dom%qextnd(q_bath) == 2
            extr_var2 = dom%qextnd(q_wl)   == 2
            call write_swan_datafile (swan_input_fields%dps         , &
                                    & swan_input_fields%s1          , &
                                    & swan_input_fields%mmax        , &
                                    & swan_input_fields%nmax        , &
                                    & swan_grids(i_swan)%covered    , &
                                    & 'BOTNOW', extr_var1, extr_var2, &
                                    & sumvars , positiveonly        )
         endif
         if (dom%qextnd(q_cur)>0 .or. swan_run%swuvi) then
            !
            ! Write SWAN velocity file
            !
            write(*,'(a)') '  Write SWAN velocity file'
            sumvars      = .false.
            positiveonly = .false.
            extr_var1 = dom%qextnd(q_cur)  == 2
            extr_var2 = dom%qextnd(q_cur)  == 2
            call write_swan_datafile (swan_input_fields%u1          , &
                                    & swan_input_fields%v1          , &
                                    & swan_input_fields%mmax        , &
                                    & swan_input_fields%nmax        , &
                                    & swan_grids(i_swan)%covered    , &
                                    & 'CURNOW', extr_var1, extr_var2, &
                                    & sumvars , positiveonly        )
         endif
         if (dom%qextnd(q_wind)>0 .or. dom%n_meteofiles_dom > 0) then
            !
            ! Write SWAN wind file
            !
            write(*,'(a)') '  Write SWAN wind file'
            sumvars      = .false.
            positiveonly = .false.
            extr_var1 = dom%qextnd(q_wind) == 2
            extr_var2 = dom%qextnd(q_wind) == 2
            call write_swan_datafile (swan_input_fields%windu       , &
                                    & swan_input_fields%windv       , &
                                    & swan_input_fields%mmax        , &
                                    & swan_input_fields%nmax        , &
                                    & swan_grids(i_swan)%covered    , &
                                    & 'WNDNOW', extr_var1, extr_var2, &
                                    & sumvars , positiveonly        )
         endif
         if (wavedata%mode == flow_mud_online) then
            !
            ! Write SWAN mud file
            ! Never extend!
            !
            write(*,'(a)') '  Write SWAN mud file'
            sumvars      = .true.
            positiveonly = .true.
            extr_var1    = .false.
            extr_var2    = .false.
            call write_swan_datafile (swan_input_fields%dpsmud      , &
                                    & swan_input_fields%s1mud       , &
                                    & swan_input_fields%mmax        , &
                                    & swan_input_fields%nmax        , &
                                    & swan_grids(i_swan)%covered    , &
                                    & 'MUDNOW', extr_var1, extr_var2, &
                                    & sumvars , positiveonly        )
         endif
         if (dom%vegetation == 1) then
            !
            ! Write Vegetation map file
            !
            write(*,'(a)') '  Write Vegetation map file'
            sumvars      = .true.
            positiveonly = .false.
            extr_var1 = dom%qextnd(q_bath) == 2
            extr_var2 = dom%qextnd(q_wl)   == 2
            call write_swan_datafile (swan_input_fields%veg         , &
                                    & swan_input_fields%s1veg       , &
                                    & swan_input_fields%mmax        , &
                                    & swan_input_fields%nmax        , &
                                    & swan_grids(i_swan)%covered    , &
                                    & 'VEGNOW', extr_var1, extr_var2, &
                                    & sumvars , positiveonly        )
         endif

         write(*,'(a)') '  Deallocate input fields'
         call dealloc_input_fields (swan_input_fields, wavedata%mode)

         ! Update SWAN wind and wave conditions in SWAN input file based on wavecon time-series file

         ! Write SWAN input
         write(*,'(a)') '  Write SWAN input'
         dom%curlif = swan_grids(i_swan)%tmp_name

         call write_swan_input (swan_run, itide, wavedata%output%count, i_swan, wavedata)

         ! The following commented code was used for a special version
         ! - to be implemented in a more constructive way
         ! Prepare input files for Part file runs
         !write(sfile,'(a4,i3.3,a4)') 'part', istep, '.txt'
         !call cp_file(sfile,'partmiod.txt','copy',nuerr)

         ! Run SWAN
         write(*,'(a)') '<<Run SWAN...'
         call run_swan (swan_run%casl)

         ! Allocate swan output fields
         write(*,'(a)') '  Allocate output fields'
         if (allocated(swan_run%add_out_names)) then
            swan_output_fields%n_outpars = size(swan_run%add_out_names)
         else
            swan_output_fields%n_outpars = 0
         endif
         call alloc_output_fields (swan_grids(i_swan), swan_output_fields)
         if (allocated(swan_run%add_out_names)) then
            swan_output_fields%add_out_names = swan_run%add_out_names
         endif
         !
         ! Read SWAN output
         !
         write(*,'(a)') '  Read SWAN output'
         call read_swan_output(swan_output_fields, swan_run)

         if (swan_run%swwav) then
            !
            ! For each com-file (flow domain)
            !
            do i_flow = 1, n_flow_grids
               !
               ! Map WAVE parameters to FLOW grid
               !
               write(*,'(a,i10)') '  Map WAVE parameters to FLOW grid ', i_flow
               call map_swan_output(swan_output_fields,            &
                           &        flow_output_fields(i_flow),    &
                           &        swan2flow_maps(i_swan,i_flow), &
                           &        flow_grids(i_flow)            )
            enddo
         endif
         if (dom%cgnum .and. wavedata%output%write_wavm) then
            !
            ! Write output to WAVE map file
            !
            write(*,'(a,i10,a,f10.3)') '  Write WAVE map file, nest ',i_swan,' time ',wavedata%time%timmin
            call write_wave_map (swan_grids(i_swan), swan_output_fields, n_swan_grids, &
                               & wavedata, swan_run%casl)
         endif
         call dealloc_output_fields (swan_output_fields)
         !
         if (deletehotfile(wavedata)) then
            !
            ! The hotfile related to "usehottime" has been used and can now be deleted
            !
            write (fname,'(a,i0,2a)') 'hot_', i_swan, '_', trim(swan_run%usehottime)
            inquire (file = trim(fname), exist = exists)
            if (exists) then
               lunhot = new_lun()
               open (lunhot, file = trim(fname))
               close (lunhot, status = 'delete')
            endif
            !
            ! When using SWAN-MPI, the names of the hot-files generated are extended with -001, -002 etc
            ! Keep removing them until there is no one left
            !
            count = 0
            do
               count = count + 1
               write (fname,'(a,i0,3a,i3.3)') 'hot_', i_swan, '_', trim(swan_run%usehottime), '-', count
               inquire (file = trim(fname), exist = exists)
               if (exists) then
                  lunhot = new_lun()
                  open (lunhot, file = trim(fname))
                  close (lunhot, status = 'delete')
               else
                  !
                  ! exit do loop
                  !
                  exit
               endif
            enddo
         endif
      enddo ! nested swan grids
      !
      ! After all hotfiles are handled correctly for each i_swan:
      ! Next time, use the last written hotfile
      !
      swan_run%usehottime = swan_run%writehottime
      !
      ! gl THINK THIS CHECK SHOULD BE AROUND ALL WRITING TO THE COM FILE
      !
      if (swan_run%swwav) then
         do i_flow = 1, n_flow_grids
            !
            ! Convert vector fields to curvilinear directions
            !
            write(*,'(a,i10)') '  Convert vector field ', i_flow
            call wave2flow (flow_output_fields(i_flow), flow_grids(i_flow))
            !
            ! Convert some parameters to communication parameters
            !
            write(*,'(a)') '  Convert parameters'
            call wave2com (flow_output_fields(i_flow), swan_run)
            !
            ! Write to communication file(s)
            !
            write(*,'(a)') '  Write to com-file'
            itidewrite = itide
            if (swan_run%append_com) itidewrite = -1
            call put_wave_fields (flow_grids(i_flow), flow_output_fields(i_flow),  &
                                & itidewrite        , wavedata%time             , swan_run%swflux)
         enddo
      endif
   enddo   ! time steps
end subroutine swan_tot
