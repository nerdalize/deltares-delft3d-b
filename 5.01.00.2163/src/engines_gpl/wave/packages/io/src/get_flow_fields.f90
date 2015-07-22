subroutine get_flow_fields (sif, fg, f2s, wavedata, sr, flowVelocityType)
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
!  $Id: get_flow_fields.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/io/src/get_flow_fields.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use swan_flow_grid_maps
   use swan_input
   use flow_data
   use wave_data
   !
   implicit none
!
! Global variables
!
   integer              :: flowVelocityType
   type(input_fields)   :: sif              ! input fields defined on swan grid
   type(grid)           :: fg               ! flow grid
   type(grid_map)       :: f2s              ! flow to swn grid mapper
   type(input_fields)   :: fif              ! input fields defined on flow grid
   type(wave_data_type) :: wavedata
   type(swan)           :: sr               ! swan input structure
!
! Local variables
!
   integer        :: iprint    = 0
   real           :: alpb      = 0.0
   real           :: dummy     = -999.0
   logical        :: clbot     = .true.
   character(256) :: mudfilnam = ' '
!
!! executable statements -------------------------------------------------------
!
   ! Allocate memory swan input fields defined on flow grid
   !
   call alloc_input_fields(fg, fif, wavedata%mode)
   !
   if (sr%swmor) then
      !
      ! Read depth from com-file
      !
      call get_dep (fif%dps, fif%mmax, fif%nmax, &
                  & fg%grid_name)
      !
      ! Map depth to SWAN grid
      !
      call grmap (fif%dps      , fif%npts        , &
                & sif%dps      , sif%npts        , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
   endif
   !
   ! Read polygons fixed structures
   !
   call dam_cod (fg%x, fg%y, fg%kcs, fg%mmax, fg%nmax)
   !
   if (sr%swwlt) then
      !
      ! Read water level from com-file
      !
      call get_lev (wavedata%time , &
                  & fif%s1, fif%mmax, fif%nmax, &
                  & fg%grid_name)
      !
      ! Map water level to SWAN grid
      !
      call grmap (fif%s1       , fif%npts        , &
                & sif%s1       , sif%npts        , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
   endif
   !
   if (sr%swuvt) then
      !
      ! Read velocity from com-file
      !
      call get_cur (wavedata%time, &
                  & fif%kfu, fif%kfv, fif%u1, fif%v1, fif%mmax, fif%nmax, &
                  & fg%kmax, fg%grid_name, fg%layer_model, flowVelocityType, &
                  & fif%dps, fif%s1)
      !
      ! Convert to Cartesian, cell centres
      !
      call flow2wav (fif%u1  , fif%v1 , &
                   & fg%alfas, fg%guu , fg%gvv, fg%mmax, fg%nmax, fg%kcs, &
                   & fif%kfu , fif%kfv, alpb  , clbot  )
      !
      ! Map velocity to SWAN grid
      ! NOTE: mapping procedure only updates the part of SWAN grid covered by current FLOW domain
      !
      call grmap (fif%u1       , fif%npts        , &
                & sif%u1       , sif%npts        , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
      call grmap (fif%v1       , fif%npts        , &
                & sif%v1       , sif%npts        , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
   endif
   !
   if (sr%swwindt .and. sr%dom(1)%qextnd(q_wind) >= 1) then
      !
      ! Read wind from com-file
      !
      call get_wind (wavedata%time, &
                   & fif%windu, fif%windv, fif%mmax, fif%nmax, &
                   & fg%grid_name)
      !
      ! Map wind to SWAN grid
      !
      call grmap (fif%windu    , fif%npts        , &
                & sif%windu    , sif%npts        , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
      call grmap (fif%windv    , fif%npts   , &
                & sif%windv    , sif%npts   , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
   endif
   !
   if (wavedata%mode == flow_mud_online) then
      write(*,'(4x,a)') 'Mud:'
      write(mudfilnam,'(a,a)')'com-',trim(mudids(1))
      !
      ! Read mud parameters needed by SWAN
      !
      call get_params  (dummy, sr%rhomud, mudfilnam)
      call get_visc (wavedata%time , sr%viscmud, fif%mmax, fif%nmax, mudfilnam)
      !
      ! Read depth from mud-com-file
      ! ASSUMPTIONS:
      ! - Only one mud domain
      ! - Mud grid is identical to the grid of the only water domain
      !
      call get_dep (fif%dpsmud, fif%mmax, fif%nmax, &
                  & mudfilnam)
      !
      ! Map depth to SWAN grid
      !
      call grmap (fif%dpsmud   , fif%npts        , &
                & sif%dpsmud   , sif%npts        , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
      !
      ! Read mud level from mud-com-file
      !
      call get_lev (wavedata%time , &
                  & fif%s1mud, fif%mmax, fif%nmax, &
                  & mudfilnam)
      !
      ! Map mud level to SWAN grid
      !
      call grmap (fif%s1mud    , fif%npts        , &
                & sif%s1mud    , sif%npts        , &
                & f2s%ref_table, f2s%weight_table, f2s%n_surr_points, &
                & iprint       )
   endif
   !
   ! Deallocate memory swan input fields defined on flow grid
   !
   call dealloc_input_fields(fif, wavedata%mode)
end subroutine get_flow_fields
