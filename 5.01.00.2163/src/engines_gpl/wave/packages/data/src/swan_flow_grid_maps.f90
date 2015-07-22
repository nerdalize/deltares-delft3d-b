module swan_flow_grid_maps
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
!  $Id: swan_flow_grid_maps.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/data/src/swan_flow_grid_maps.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_basics
    !
    type grid_map
       integer                            :: n_surr_points   ! number of surrounding points
                                                             ! (4 for curvilin., 3 for triangular grids)
       integer                            :: npts_provider   ! number of points provider grid
       integer                            :: npts_receiver   ! number of points receiver grid
       integer, dimension(:,:), pointer   :: ref_table       ! reference table from client grid point
                                                             ! to surrounding provider grid points
       real   , dimension(:,:), pointer   :: weight_table    ! weights of surrounding provider grid
                                                             ! points
       character (256)                    :: provider_name   ! name of provider grid
       character (256)                    :: receiver_name   ! name of receiver grid
    end type grid_map
    !
    type grid
       integer                            :: mmax            ! number of columns
       integer                            :: nmax            ! number of rows
       integer                            :: kmax            ! number of layers
       integer                            :: npts            ! number of points
       integer, dimension(:,:), pointer   :: kcs             ! mask-array inactive points
       integer, dimension(:,:), pointer   :: covered         ! mask-array points covered by "other" program
       real                               :: xymiss          ! missing value
       real(kind=hp), dimension(:,:), pointer   :: x         ! x-coordinates cell center
       real(kind=hp), dimension(:,:), pointer   :: y         ! y-coordinates cell center
       real   , dimension(:,:), pointer   :: alfas           ! grid direction cell center
       real   , dimension(:,:), pointer   :: guu             ! grid size u-cell wall
       real   , dimension(:,:), pointer   :: gvv             ! grid size v-cell wall
       logical                            :: sferic          ! spherical coordinate system (t/f)
       character (256)                    :: grid_name       ! name of grid
       character (37)                     :: tmp_name        ! temporary filename
       character (4)                      :: grid_file_type  ! type of grid file (SWAN/FLOW/COM/TRIM)
       character (6)                      :: xy_loc          ! location of xy coords (CORNER/CENTER)
       character (16)                     :: layer_model     ! Vertical layering type: "Sigma-model" or "Z-model"
    end type   grid
    !
    type input_fields
       integer                            :: mmax            ! number of columns
       integer                            :: nmax            ! number of rows
       integer                            :: npts            ! number of points
       integer, dimension(:,:), pointer   :: kfu             ! mask array u-velocity points
       integer, dimension(:,:), pointer   :: kfv             ! mask array v-velocity points
       real   , dimension(:,:), pointer   :: s1              ! water level
       real   , dimension(:,:), pointer   :: u1              ! u-velocity
       real   , dimension(:,:), pointer   :: v1              ! v-velocity
       real   , dimension(:,:), pointer   :: dps             ! depth in water level points
       real   , dimension(:,:), pointer   :: windu           ! wind velocity component in x direction
       real   , dimension(:,:), pointer   :: windv           ! wind velocity component in y direction
       real   , dimension(:,:), pointer   :: s1mud           ! mud level
       real   , dimension(:,:), pointer   :: dpsmud          ! mud related depth in water level points
       real   , dimension(:,:), pointer   :: veg             ! vegetation map - densities of plants per m2
       real   , dimension(:,:), pointer   :: s1veg           ! vegetation level - dummy field of zeros
    end type input_fields
    !
    type output_fields
       integer                            :: mmax            ! number of columns
       integer                            :: nmax            ! number of rows
       integer                            :: npts            ! number of points
       integer                            :: n_outpars       ! number of additional output parameters
       real, dimension(:,:), pointer      :: hs              !
       real, dimension(:,:), pointer      :: dir             !
       real, dimension(:,:), pointer      :: dirc            !
       real, dimension(:,:), pointer      :: dirs            !
       real, dimension(:,:), pointer      :: period          !
       real, dimension(:,:), pointer      :: depth           !
       real, dimension(:,:), pointer      :: fx              !
       real, dimension(:,:), pointer      :: fy              !
       real, dimension(:,:), pointer      :: wsbodyu         !
       real, dimension(:,:), pointer      :: wsbodyv         !
       real, dimension(:,:), pointer      :: mx              !
       real, dimension(:,:), pointer      :: my              !
       real, dimension(:,:,:), pointer    :: dissip          !
       real, dimension(:,:), pointer      :: ubot            !
       real, dimension(:,:), pointer      :: steep           !
       real, dimension(:,:), pointer      :: wlen            !
       real, dimension(:,:), pointer      :: u               !
       real, dimension(:,:), pointer      :: v               !
       real, dimension(:,:), pointer      :: dspr            !
       real, dimension(:,:), pointer      :: rleak           !
       real, dimension(:,:), pointer      :: qb              !
       real, dimension(:,:), pointer      :: x               !
       real, dimension(:,:), pointer      :: y               !
       real, dimension(:,:), pointer      :: rtp             !
       real, dimension(:,:), pointer      :: hrms            !
       real, dimension(:,:), pointer      :: tp              !
       real, dimension(:,:), pointer      :: pdir            !
       real, dimension(:,:), pointer      :: windu           !
       real, dimension(:,:), pointer      :: windv           !
       real, dimension(:,:), pointer      :: tps             !
       real, dimension(:,:), pointer      :: tm02            !
       real, dimension(:,:), pointer      :: tmm10           !
       real, dimension(:,:), pointer      :: dhsign          !
       real, dimension(:,:), pointer      :: drtm01          !
       real, dimension(:,:), pointer      :: setup           !
       real, dimension(:,:,:), pointer    :: add_out_vals    !
       character(7), dimension(:), pointer :: add_out_names  !
    end type output_fields
    !
    type (grid)         , dimension(:)  , pointer, save :: swan_grids         ! pointer array swan grids
    type (grid)         , dimension(:)  , pointer, save :: flow_grids         ! pointer array flow grids
    type (grid_map)     , dimension(:,:), pointer, save :: flow2swan_maps     ! mappers flow->swan
    type (grid_map)     , dimension(:,:), pointer, save :: swan2flow_maps     ! mappers swan->flow
    type (input_fields)                          , save :: swan_input_fields  ! pointer to input fields, swan grid
    type (output_fields)                         , save :: swan_output_fields ! pointer array to output fields, swan grid
    type (output_fields), dimension(:)  , pointer, save :: flow_output_fields ! pointer array to output fields, flow grid
    !
contains
!
!
!==============================================================================
subroutine init_grids (n_swan_grids, n_flow_grids)
   implicit none
   !
   integer, intent(in) :: n_swan_grids
   integer, intent(in) :: n_flow_grids
   !
   allocate (swan_grids(n_swan_grids))
   allocate (flow_grids(n_flow_grids))
   allocate (flow2swan_maps(n_swan_grids,n_flow_grids))
   allocate (swan2flow_maps(n_swan_grids,n_flow_grids))
   allocate (flow_output_fields(n_flow_grids))
end subroutine Init_grids
!
!
!==============================================================================
subroutine alloc_and_get_grid(g,grid_name,grid_file_type,xy_loc)
   use read_grids
   !
   implicit none
   !
   type(grid)                :: g
   character (*), intent(in) :: grid_name       ! name of grid
   character (4), intent(in) :: grid_file_type  ! type of grid file (SWAN/FLOW/COM/TRIM)
   character (6), intent(in) :: xy_loc          ! location of xy coords (CORNER/CENTER)
   !
   ! Assign attributes to grid structure
   !
   g%grid_name      = grid_name
   g%grid_file_type = grid_file_type
   g%xy_loc         = xy_loc
   !
   ! Obtain grid dimensions
   !
   select case(grid_file_type)
      case ('COM')
         !
         ! This is a FLOW grid
         ! read data from com-file
         !
         call get_gri(g%grid_name ,g%x   ,g%y      ,g%guu  ,g%gvv , &
             &        g%alfas     ,g%kcs    ,g%covered     ,g%mmax ,g%nmax, &
             &        g%kmax      ,g%xymiss ,g%layer_model )
         !
         ! In case of DomainDecomposition, some adaptions are necessary
         !
         call grid_dd_corrections(g%alfas, g%kcs,g%mmax, g%nmax)
         !
         ! array covered is only used for SWAN grids
         ! Currently it is also generated for FLOW grids
         !
      case ('FLOW')
         !
         ! This is a SWAN grid
         ! read data from grd-file
         !
         call read_grd(g%grid_name, g%x   ,g%y    ,g%kcs ,g%covered, g%mmax, g%nmax ,g%sferic, g%xymiss)
      case default
         ! grid type not supported
         write(*,'(a)') '*** ERROR: Grid type ''',trim(grid_file_type), ''' not supported.'
         stop
   endselect
   g%npts = g%mmax*g%nmax
end subroutine alloc_and_get_grid
!
!
!==============================================================================
subroutine grid_dd_corrections(alfas, kcs, mmax, nmax)
    implicit none
    !
    ! global parameters
    integer                      , intent(in)  :: mmax
    integer                      , intent(in)  :: nmax
    integer, dimension(mmax,nmax), intent(in)  :: kcs
    real   , dimension(mmax,nmax)              :: alfas
    !
    ! local parameters
    !
    integer :: m
    integer :: mNeighbour
    integer :: n
    integer :: nNeighbour
    !
    ! body
    !
    do m = 1, mmax
       do n = 1, nmax
          if (kcs(m,n) == 3) then
             !
             ! alfa is undefined; copy from one of the 4 neighbours with kcs=1
             !
             mNeighbour = min(m+1,mmax)
             nNeighbour = n
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             mNeighbour = max(m-1,1)
             nNeighbour = n
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             mNeighbour = m
             nNeighbour = min(n+1,nmax)
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             mNeighbour = m
             nNeighbour = max(n-1,1)
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             !
             ! The following statement should not be reached
             !
             write(*,'(a,i0,a,i0,a)') '*** ERROR: alfas(m=', m, ',n=', n, ') is undefined' 
             stop
          endif
       enddo
    enddo
end subroutine grid_dd_corrections
!
!
!==============================================================================
subroutine make_grid_map(g1,g2,gm)
   implicit none
   !
   type(grid)     :: g1,g2
   type(grid_map) :: gm
   !
   ! Local work arrays
   !
   integer                                         :: ierr
   integer                                         :: iprint
   integer, dimension(:), allocatable              :: iflag,nrin,nrx,nry
   real(kind=hp)   , dimension(:), allocatable     :: xs,ys
   !
   ! Allocate memory local work arrays
   !
   allocate (xs   (g2%npts))
   allocate (ys   (g2%npts))
   allocate (iflag(g2%npts))
   allocate (nrin (g2%npts))
   allocate (nrx  (g2%npts))
   allocate (nry  (g2%npts))
   !
   ! Set gridmap attributes
   !
   gm%provider_name = g1%grid_name
   gm%receiver_name = g2%grid_name
   gm%npts_provider = g1%npts
   gm%npts_receiver = g2%npts
   gm%n_surr_points = 4
   !
   ! Allocate memory reference and weight tables
   !
   allocate(gm%ref_table    (gm%n_surr_points,g2%npts))
   allocate(gm%weight_table (gm%n_surr_points,g2%npts))
   !
   ! print option
   !
   iprint=0
   call mkmap (g1%kcs, g1%x ,  g1%y  , g1%mmax, g1%nmax,     &
             & g2%x  , g2%y , g2%npts, xs     ,ys      ,     &
             & nrx   , nry  , iflag  , nrin   ,              &
             & gm%weight_table     , gm%ref_table      ,     &
             & iprint, g2%covered, g1%xymiss)
   !
   ! Deallocate memory local work arrays
   !
   deallocate (xs   , stat=ierr)
   deallocate (ys   , stat=ierr)
   deallocate (iflag, stat=ierr)
   deallocate (nrin , stat=ierr)
   deallocate (nrx  , stat=ierr)
   deallocate (nry  , stat=ierr)
end subroutine make_grid_map
!
!
!==============================================================================
subroutine alloc_input_fields (g,inpfld, mode)
   use wave_data
   implicit none
   !
   integer                 :: mode
   type(grid)              :: g
   type(input_fields)      :: inpfld
   !
   ! Assign grid dimensions
   !
   inpfld%mmax = g%mmax
   inpfld%nmax = g%nmax
   inpfld%npts = g%npts
   !
   ! Allocate variables
   !
   allocate (inpfld%s1   (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%u1   (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%v1   (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%kfu  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%kfv  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%dps  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%windu(inpfld%mmax,inpfld%nmax))
   allocate (inpfld%windv(inpfld%mmax,inpfld%nmax))
   allocate (inpfld%veg  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%s1veg(inpfld%mmax,inpfld%nmax))
   !
   ! Initialise arrays
   !
   inpfld%s1    = 0.
   inpfld%u1    = 0.
   inpfld%v1    = 0.
   inpfld%dps   = 0.
   inpfld%veg   = 0.
   inpfld%s1veg = 0.
   inpfld%kfu   = 0
   inpfld%windu = 0.
   inpfld%windv = 0.
   !
   ! Interaction with Fluid Mud
   !
   if (mode == flow_mud_online) then
      allocate (inpfld%s1mud   (inpfld%mmax,inpfld%nmax))
      allocate (inpfld%dpsmud  (inpfld%mmax,inpfld%nmax))
      inpfld%s1mud    = 0.0
      inpfld%dpsmud   = 0.0
   endif
end subroutine alloc_input_fields
!
!
!==============================================================================
subroutine init_input_fields (inpfld,sr,itide)
   use swan_input
   !
   implicit none
   !
   type(input_fields)      :: inpfld
   type(swan)              :: sr
   integer                 :: itide
   !
   ! locals
   !
   real :: pi
   real :: d2r
   real :: cartDir
   ! body
   !
   ! Initialise arrays with overall values from swan input
   !
   inpfld%s1 = sr%zeta(itide)
   inpfld%u1 = sr%ux0(itide)
   inpfld%v1 = sr%uy0(itide)
   pi           = 4.0 * atan(1.0)
   d2r          = pi / 180.0
   if (sr%nautconv) then
      cartDir = 270.0 - sr%wdir(itide)
   else
      cartDir = sr%wdir(itide)
   endif
   cartDir = cartDir*d2r
   inpfld%windu = sr%wvel(itide) * cos(cartDir)
   inpfld%windv = sr%wvel(itide) * sin(cartDir)
end subroutine init_input_fields
!
!
!==============================================================================
subroutine dealloc_input_fields (inpfld, mode)
   use wave_data
   implicit none
   !
   integer                 :: ierr
   integer                 :: mode
   type(input_fields)      :: inpfld
   !
   deallocate (inpfld%s1   , stat=ierr)
   deallocate (inpfld%u1   , stat=ierr)
   deallocate (inpfld%v1   , stat=ierr)
   deallocate (inpfld%kfu  , stat=ierr)
   deallocate (inpfld%kfv  , stat=ierr)
   deallocate (inpfld%dps  , stat=ierr)
   deallocate (inpfld%veg  , stat=ierr)
   deallocate (inpfld%s1veg, stat=ierr)
   deallocate (inpfld%windu, stat=ierr)
   deallocate (inpfld%windv, stat=ierr)
   if (mode == flow_mud_online) then
      deallocate (inpfld%s1mud , stat=ierr)
      deallocate (inpfld%dpsmud, stat=ierr)
   endif
end subroutine dealloc_input_fields
!
!
!==============================================================================
subroutine dealloc_output_fields (outfld)
   implicit none
   !
   type(output_fields) :: outfld
   integer             :: ierr
   !
   ! Deallocate variables
   !
   deallocate (outfld%hs    , stat=ierr)
   deallocate (outfld%dir   , stat=ierr)
   deallocate (outfld%dirc  , stat=ierr)
   deallocate (outfld%dirs  , stat=ierr)
   deallocate (outfld%period, stat=ierr)
   deallocate (outfld%depth , stat=ierr)
   deallocate (outfld%fx    , stat=ierr)
   deallocate (outfld%fy    , stat=ierr)
   deallocate (outfld%wsbodyu, stat=ierr)
   deallocate (outfld%wsbodyv, stat=ierr)
   deallocate (outfld%mx    , stat=ierr)
   deallocate (outfld%my    , stat=ierr)
   deallocate (outfld%dissip, stat=ierr)
   deallocate (outfld%ubot  , stat=ierr)
   deallocate (outfld%steep , stat=ierr)
   deallocate (outfld%wlen  , stat=ierr)
   deallocate (outfld%u     , stat=ierr)
   deallocate (outfld%v     , stat=ierr)
   deallocate (outfld%dspr  , stat=ierr)
   deallocate (outfld%rleak , stat=ierr)
   deallocate (outfld%qb    , stat=ierr)
   deallocate (outfld%x     , stat=ierr)
   deallocate (outfld%y     , stat=ierr)
   deallocate (outfld%rtp   , stat=ierr)
   deallocate (outfld%hrms  , stat=ierr)
   deallocate (outfld%tp    , stat=ierr)
   deallocate (outfld%pdir  , stat=ierr)
   deallocate (outfld%windu , stat=ierr)
   deallocate (outfld%windv , stat=ierr)
   deallocate (outfld%tps   , stat=ierr)
   deallocate (outfld%tm02  , stat=ierr)
   deallocate (outfld%tmm10 , stat=ierr)
   deallocate (outfld%dhsign, stat=ierr)
   deallocate (outfld%drtm01, stat=ierr)
   deallocate (outfld%setup , stat=ierr)
   if (associated(outfld%add_out_vals)) then
      deallocate (outfld%add_out_vals , stat=ierr)
      deallocate (outfld%add_out_names , stat=ierr)
   endif
end subroutine dealloc_output_fields
!
!
!==============================================================================
subroutine alloc_output_fields (g,outfld)
   implicit none
   !
   type(grid)              :: g
   type(output_fields)     :: outfld
   !
   ! Assign grid dimensions
   !
   outfld%mmax = g%mmax
   outfld%nmax = g%nmax
   outfld%npts = g%npts
   !
   ! Allocate variables
   !
   allocate (outfld%hs    (outfld%mmax,outfld%nmax))
   allocate (outfld%dir   (outfld%mmax,outfld%nmax))
   allocate (outfld%dirc  (outfld%mmax,outfld%nmax))
   allocate (outfld%dirs  (outfld%mmax,outfld%nmax))
   allocate (outfld%period(outfld%mmax,outfld%nmax))
   allocate (outfld%depth (outfld%mmax,outfld%nmax))
   allocate (outfld%fx    (outfld%mmax,outfld%nmax))
   allocate (outfld%fy    (outfld%mmax,outfld%nmax))
   allocate (outfld%wsbodyu(outfld%mmax,outfld%nmax))
   allocate (outfld%wsbodyv(outfld%mmax,outfld%nmax))
   allocate (outfld%mx    (outfld%mmax,outfld%nmax))
   allocate (outfld%my    (outfld%mmax,outfld%nmax))
   allocate (outfld%dissip(outfld%mmax,outfld%nmax,4))
   allocate (outfld%ubot  (outfld%mmax,outfld%nmax))
   allocate (outfld%steep (outfld%mmax,outfld%nmax))
   allocate (outfld%wlen  (outfld%mmax,outfld%nmax))
   allocate (outfld%u     (outfld%mmax,outfld%nmax))
   allocate (outfld%v     (outfld%mmax,outfld%nmax))
   allocate (outfld%dspr  (outfld%mmax,outfld%nmax))
   allocate (outfld%rleak (outfld%mmax,outfld%nmax))
   allocate (outfld%qb    (outfld%mmax,outfld%nmax))
   allocate (outfld%x     (outfld%mmax,outfld%nmax))
   allocate (outfld%y     (outfld%mmax,outfld%nmax))
   allocate (outfld%rtp   (outfld%mmax,outfld%nmax))
   allocate (outfld%hrms  (outfld%mmax,outfld%nmax))
   allocate (outfld%tp    (outfld%mmax,outfld%nmax))
   allocate (outfld%pdir  (outfld%mmax,outfld%nmax))
   allocate (outfld%windu (outfld%mmax,outfld%nmax))
   allocate (outfld%windv (outfld%mmax,outfld%nmax))
   allocate (outfld%tps   (outfld%mmax,outfld%nmax))
   allocate (outfld%tm02  (outfld%mmax,outfld%nmax))
   allocate (outfld%tmm10 (outfld%mmax,outfld%nmax))
   allocate (outfld%dhsign(outfld%mmax,outfld%nmax))
   allocate (outfld%drtm01(outfld%mmax,outfld%nmax))
   allocate (outfld%setup (outfld%mmax,outfld%nmax))
   if (outfld%n_outpars > 0) then
      allocate (outfld%add_out_vals (outfld%mmax,outfld%nmax,outfld%n_outpars))
      allocate (outfld%add_out_names (outfld%n_outpars))
   endif
   !
   ! Initialise arrays
   !
   outfld%hs      = 0.
   outfld%dir     = 0.
   outfld%dirc    = 0.
   outfld%dirs    = 0.
   outfld%period  = 0.
   outfld%depth   = 0.
   outfld%fx      = 0.
   outfld%fy      = 0.
   outfld%wsbodyu = 0.
   outfld%wsbodyv = 0.
   outfld%mx      = 0.
   outfld%my      = 0.
   outfld%dissip  = 0.
   outfld%ubot    = 0.
   outfld%steep   = 0.
   outfld%wlen    = 0.
   outfld%u       = 0.
   outfld%v       = 0.
   outfld%dspr    = 0.
   outfld%rleak   = 0.
   outfld%qb      = 0.
   outfld%x       = 0.
   outfld%y       = 0.
   outfld%rtp     = 0.
   outfld%hrms    = 0.
   outfld%tp      = 0.
   outfld%pdir    = 0.
   outfld%windu   = 0.
   outfld%windv   = 0.
   outfld%tps     = 0.
   outfld%tm02    = 0.
   outfld%tmm10   = 0.
   outfld%dhsign  = 0.
   outfld%drtm01  = 0.
   outfld%setup   = 0.
   if (outfld%n_outpars > 0) then
      outfld%add_out_vals   = 0.
      outfld%add_out_names   = ' '
   endif
end subroutine alloc_output_fields



end module swan_flow_grid_maps
