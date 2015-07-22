module dffunctionals
!
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
!  $Id: dffunctionals.F90 2163 2013-02-01 13:30:53Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dffunctionals.F90 $
!!--description-----------------------------------------------------------------
!
!    Function: regroups functionalities for operations spanning all partitions
!              and requiring communications
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   !
   implicit none
   !
   real(sp), dimension(:,:),     allocatable, save :: glbarr2
   real(sp), dimension(:,:,:),   allocatable, save :: glbarr3
   real(sp), dimension(:,:,:,:), allocatable, save :: glbarr4
   integer,  dimension(:,:),     allocatable, save :: glbari2
   !
   interface dfgather_filter
      module procedure dfgather_filter_C
      module procedure dfgather_filter_I1D
      module procedure dfgather_filter_I2D
      module procedure dfgather_filter_R1D_sp
      module procedure dfgather_filter_R1D_hp
      module procedure dfgather_filter_R1D_hphp
      module procedure dfgather_filter_R2D_sp
      module procedure dfgather_filter_R2D_hp
      module procedure dfgather_filter_R3D_sp
      module procedure dfgather_filter_R3D_hp
   end interface dfgather_filter
   !
   interface dfgather
      module procedure dfgather_I2e
      module procedure dfgather_R2e_sp
      module procedure dfgather_R2e_hp
      module procedure dfgather_R3e_sp
      module procedure dfgather_R3e_hp
      module procedure dfgather_R4e_sp
      module procedure dfgather_R4e_hp
   end interface dfgather
   !
   interface dfgather_seq
      module procedure dfgather_R2e_seq_sp
      module procedure dfgather_R2e_seq_hp      
      module procedure dfgather_R3e_seq_sp
      module procedure dfgather_R3e_seq_hp 
      module procedure dfgather_R4e_seq_sp
      module procedure dfgather_R4e_seq_hp 
      module procedure dfgather_I2_seq
   end interface dfgather_seq
   
   interface dffind_duplicate
      module procedure dffind_duplicate_C
      module procedure dffind_duplicate_I
   end interface dffind_duplicate
   !
contains
!
!
!
!===============================================================================
subroutine dfgather_filter_C(lundia, nblocal, nbtotal, nbglobal, order, &
                           & inbuff, oubuff, gdp )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                            :: gdp
integer                                          :: lundia !  Description and declaration in inout.igs
integer                                          :: nblocal
integer                                          :: nbtotal
integer                                          :: nbglobal
integer      , dimension(nblocal)  , intent(in)  :: order
character(20), dimension(1:nblocal), intent(in)  :: inbuff
character(20), dimension(1:nbglobal)             :: oubuff
!
! Local variables
!
integer                                   :: j
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer, dimension(:), allocatable        :: ibuff
character(20), dimension(:), allocatable  :: rbuff
integer                                   :: onode
integer                                   :: ierr                   ! error value of MPI call
#if defined (DFMPI)
    integer                               :: istat(mpi_status_size) ! MPI status array
#endif
character(80)                             :: msgstr                 ! string to pass message
integer                                   :: itag
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       allocate( rbuff(nbtotal) )
       allocate( ibuff(1:nbtotal) )
    endif
    call dfgather_lowlevel ( rbuff, 20*nbtotal, inbuff, 20*nblocal, dfchar, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    if (inode == master) then
       do n = 1, nbtotal
          if (ibuff(n) /= 0) oubuff(ibuff(n)) = rbuff(n)
       enddo
       deallocate( ibuff )
       deallocate( rbuff )
    endif
    call dfsync(gdp)
end subroutine dfgather_filter_C
!
!
!
!===============================================================================
subroutine dfgather_filter_I1D(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, crosec_case )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                      :: gdp
integer                                    :: lundia !  Description and declaration in inout.igs
integer                                    :: nblocal
integer                                    :: nbtotal
integer                                    :: nbglobal
integer, dimension(nblocal)  , intent(in)  :: order
integer, dimension(1:nblocal), intent(in)  :: inbuff
integer, dimension(1:nbglobal)             :: oubuff
logical, intent(in), optional              :: crosec_case
!
! Local variables
!
integer                                   :: m
integer                                   :: n
integer, dimension(:), allocatable        :: tempbuff
integer, dimension(:), allocatable        :: ibuff
integer                                   :: onode
integer                                   :: ierr                   ! error value of MPI call
#if defined (DFMPI)
    integer                               :: istat(mpi_status_size) ! MPI status array
#endif
character(80)                             :: msgstr                 ! string to pass message
integer                                   :: itag
logical                                   :: crosec_case_l
!
!! executable statements -------------------------------------------------------
!
    if (present(crosec_case)) then
        crosec_case_l = crosec_case
    else
        crosec_case_l = .false.
    endif
    if (inode == master) then
       allocate( tempbuff(nbtotal) )
       allocate( ibuff(1:nbtotal) )
       tempbuff = 0       
    endif
    call dfgather_lowlevel ( tempbuff, nbtotal, inbuff, nblocal, dfint, gdp )
    call dfgather_lowlevel ( ibuff   , nbtotal, order , nblocal, dfint, gdp )
    if (inode == master) then
       oubuff = 0
       if (crosec_case_l) then
          do n = 1, nbtotal
             if (ibuff(n) /= 0) oubuff(ibuff(n)) = oubuff(ibuff(n)) + tempbuff(n)
          enddo
       else
          do n = 1, nbtotal
             if (ibuff(n) /= 0) oubuff(ibuff(n)) = tempbuff(n)
          enddo
       endif
       deallocate( ibuff )
       deallocate( tempbuff )
    endif
    call dfsync(gdp)
end subroutine dfgather_filter_I1D
!
!
!
!===============================================================================
subroutine dfgather_filter_I2D(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                             & inbuff, oubuff, gdp )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                             :: gdp
integer                                           :: lundia !  Description and declaration in inout.igs
integer                                           :: nblocal
integer                                           :: nbtotal
integer                                           :: nbglobal
integer                                           :: jf
integer                                           :: jl
integer, dimension(nblocal)         , intent(in)  :: order
integer, dimension(jf:jl, 1:nblocal), intent(in)  :: inbuff
integer, dimension(jf:jl, 1:nbglobal)             :: oubuff
!
! Local variables
!
integer                                   :: j
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer, dimension(:)  , allocatable      :: ibuff
integer, dimension(:,:), allocatable      :: rbuff
integer                                   :: onode
integer                                   :: ierr                   ! error value of MPI call
#if defined (DFMPI)
    integer                               :: istat(mpi_status_size) ! MPI status array
#endif
character(80)                             :: msgstr                 ! string to pass message
integer                                   :: itag
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       allocate( rbuff(jf:jl, nbtotal) )
       allocate( ibuff(1:nbtotal) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*nbtotal, inbuff, (jl-jf+1)*nblocal, dfint, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    if (inode == master) then
       oubuff = 0
       do n = 1, nbtotal
          if (ibuff(n) /= 0) oubuff(jf:jl, ibuff(n)) = rbuff(jf:jl, n)
       enddo
       deallocate( ibuff )
       deallocate( rbuff )
    endif
    call dfsync(gdp)
end subroutine dfgather_filter_I2D
!
!
!
!===============================================================================
subroutine dfgather_filter_R1D_sp(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, crosec_case )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                 :: gdp
integer                                               :: lundia !  Description and declaration in inout.igs
integer                                               :: nblocal
integer                                               :: nbtotal
integer                                               :: nbglobal
integer , dimension(nblocal)   , intent(in)           :: order
real(sp), dimension(1:nblocal) , intent(in)           :: inbuff
real(sp), dimension(1:nbglobal)                       :: oubuff
logical                        , intent(in), optional :: crosec_case
!
! Local variables
!
integer                                   :: m
integer                                   :: n
real(sp), dimension(:), allocatable       :: rbuff
integer , dimension(:), allocatable       :: ibuff
integer                                   :: onode
integer                                   :: ierr                   ! error value of MPI call
#if defined (DFMPI)
    integer                               :: istat(mpi_status_size) ! MPI status array
#endif
character(80)                             :: msgstr                 ! string to pass message
integer                                   :: itag
logical                                   :: crosec_case_l
!
!! executable statements -------------------------------------------------------
!
    if (present(crosec_case)) then
        crosec_case_l = crosec_case
    else
        crosec_case_l = .false.
    endif
    if (inode == master) then
       allocate( rbuff(nbtotal) )
       allocate( ibuff(1:nbtotal) )
       rbuff = 0.0 
    endif
    call dfgather_lowlevel ( rbuff, nbtotal, inbuff, nblocal, dfreal, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    if (inode == master) then
       oubuff = 0.0
       if (crosec_case_l) then
          do n = 1, nbtotal
             if (ibuff(n) /= 0) oubuff(ibuff(n)) = oubuff(ibuff(n)) + rbuff(n)
          enddo
       else
          do n = 1, nbtotal
             if (ibuff(n) /= 0) oubuff(ibuff(n)) = rbuff(n)
          enddo
       endif
       deallocate( ibuff )
       deallocate( rbuff )
    endif
    call dfsync(gdp)
end subroutine dfgather_filter_R1D_sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R1D_hp(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, crosec_case )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!              the input of this function is in high precision, and output is in single precision.  This function is used to wrihis.
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                 :: gdp
integer                                               :: lundia !  Description and declaration in inout.igs
integer                                               :: nblocal
integer                                               :: nbtotal
integer                                               :: nbglobal
integer , dimension(nblocal)   , intent(in)           :: order
real(hp), dimension(1:nblocal) , intent(in)           :: inbuff
real(sp), dimension(1:nbglobal)                       :: oubuff
logical                        , intent(in), optional :: crosec_case
!
! Local variables
!
real(sp), dimension(:), allocatable       :: rbuff
!
!! executable statements -------------------------------------------------------
!
    allocate(rbuff(nblocal))
    rbuff = real(inbuff,sp)
    call dfgather_filter_R1D_sp(lundia, nblocal, nbtotal, nbglobal, order, &
                                 & rbuff, oubuff, gdp, crosec_case )
    deallocate(rbuff)
end subroutine dfgather_filter_R1D_hp
!
!
!
!===============================================================================
subroutine dfgather_filter_R1D_hphp(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, crosec_case )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!              the input of this function is in high precision, and output is in high precision.  This function is used for incbc.
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                 :: gdp
integer                                               :: lundia !  Description and declaration in inout.igs
integer                                               :: nblocal
integer                                               :: nbtotal
integer                                               :: nbglobal
integer , dimension(nblocal)   , intent(in)           :: order
real(hp), dimension(1:nblocal) , intent(in)           :: inbuff
real(hp), dimension(1:nbglobal)                       :: oubuff
logical                        , intent(in), optional :: crosec_case
!
! Local variables
!
integer                                   :: m
integer                                   :: n
real(hp), dimension(:), allocatable       :: rbuff
integer , dimension(:), allocatable       :: ibuff
integer                                   :: onode
integer                                   :: ierr                   ! error value of MPI call
#if defined (DFMPI)
    integer                               :: istat(mpi_status_size) ! MPI status array
#endif
character(80)                             :: msgstr                 ! string to pass message
integer                                   :: itag
logical                                   :: crosec_case_l
!
!! executable statements -------------------------------------------------------
!
    if (present(crosec_case)) then
        crosec_case_l = crosec_case
    else
        crosec_case_l = .false.
    endif
    if (inode == master) then
       allocate( rbuff(nbtotal) )
       allocate( ibuff(1:nbtotal) )
       rbuff = 0.0_hp 
    endif
    call dfgather_lowlevel ( rbuff, nbtotal, inbuff, nblocal, dfdble, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    if (inode == master) then
       oubuff = 0.0_hp
       if (crosec_case_l) then
          do n = 1, nbtotal
             if (ibuff(n) /= 0) oubuff(ibuff(n)) = oubuff(ibuff(n)) + rbuff(n)
          enddo
       else
          do n = 1, nbtotal
             if (ibuff(n) /= 0) oubuff(ibuff(n)) = rbuff(n)
          enddo
       endif
       deallocate( ibuff )
       deallocate( rbuff )
    endif
    call dfsync(gdp)
end subroutine dfgather_filter_R1D_hphp
!
!
!===============================================================================
subroutine dfgather_filter_R2D_sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                             & inbuff, oubuff, gdp, crosec_case )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer , dimension(nblocal)          , intent(in)            :: order
real(sp), dimension(1:nblocal, jf:jl) , intent(in)            :: inbuff
real(sp), dimension(1:nbglobal, jf:jl)                        :: oubuff
logical                               , intent(in) , optional :: crosec_case
!
! Local variables
!
integer                                   :: j
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer , dimension(:)  , allocatable     :: ibuff
real(sp), dimension(:,:), allocatable     :: rbuff
real(sp), dimension(:,:), allocatable     :: tbuff
integer                                   :: onode
integer                                   :: ierr                   ! error value of MPI call
#if defined (DFMPI)
    integer                               :: istat(mpi_status_size) ! MPI status array
#endif
character(80)                             :: msgstr                 ! string to pass message
integer                                   :: itag
logical                                   :: crosec_case_l
!
!! executable statements -------------------------------------------------------
!
    if (present(crosec_case)) then
        crosec_case_l = crosec_case
    else
        crosec_case_l = .false.
    endif
    allocate( tbuff(jf:jl, 1:nblocal) )
    do m = jf, jl
       do n = 1, nblocal
          tbuff(m, n) = inbuff(n, m)
       enddo
    enddo
    if (inode == master) then
       allocate( rbuff(jf:jl, nbtotal) )
       allocate( ibuff(1:nbtotal) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*nbtotal, tbuff, (jl-jf+1)*nblocal, dfreal, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    deallocate( tbuff )
    if (inode == master) then
       oubuff = 0.0
       if (crosec_case_l) then
       ! for cross section records, we sum the contributions from each partition
          do n = 1, nbtotal
             if (ibuff(n) /= 0) oubuff(ibuff(n), jf:jl) = oubuff(ibuff(n), jf:jl) + rbuff(jf:jl, n)
          enddo
       else
          do n = 1, nbtotal
             if (ibuff(n) > 0 .and. ibuff(n) <= size(oubuff,1)) then
                oubuff(ibuff(n), jf:jl) = rbuff(jf:jl, n)
             endif
          enddo
       endif
       deallocate( ibuff )
       deallocate( rbuff )
    endif
    call dfsync(gdp)
end subroutine dfgather_filter_R2D_sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R2D_hp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                             & inbuff, oubuff, gdp, crosec_case )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer , dimension(nblocal)          , intent(in)            :: order
real(hp), dimension(1:nblocal, jf:jl) , intent(in)            :: inbuff
real(sp), dimension(1:nbglobal, jf:jl)                        :: oubuff
logical                               , intent(in) , optional :: crosec_case
!
! Local variables
!
real(sp), dimension(:,:), allocatable     :: rbuff
!
!! executable statements -------------------------------------------------------
!
    allocate(rbuff(1:nblocal,jf:jl))
    rbuff = real(inbuff,sp)
    call dfgather_filter_R2D_sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                             & rbuff, oubuff, gdp, crosec_case )
    deallocate(rbuff)
end subroutine dfgather_filter_R2D_hp
!
!
!
!===============================================================================
subroutine dfgather_filter_R3D_sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, kf, kl, order, &
                             & inbuff, oubuff, gdp )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                      :: gdp
integer                                                    :: lundia !  Description and declaration in inout.igs
integer                                                    :: nblocal
integer                                                    :: nbtotal
integer                                                    :: nbglobal
integer                                                    :: jf
integer                                                    :: jl
integer                                                    :: kf
integer                                                    :: kl
integer , dimension(nblocal)                 , intent(in)  :: order
real(sp), dimension(1:nblocal, jf:jl, kf:kl) , intent(in)  :: inbuff
real(sp), dimension(1:nbglobal, jf:jl, kf:kl)              :: oubuff
!
! Local variables
!
integer                                   :: j
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer , dimension(:)    , allocatable   :: ibuff
real(sp), dimension(:,:,:), allocatable   :: rbuff
real(sp), dimension(:,:,:), allocatable   :: tbuff
integer                                   :: onode
integer                                   :: ierr                   ! error value of MPI call
#if defined (DFMPI)
    integer                               :: istat(mpi_status_size) ! MPI status array
#endif
character(80)                             :: msgstr                 ! string to pass message
integer                                   :: itag
!
!! executable statements -------------------------------------------------------
!
    allocate( tbuff(jf:jl, kf:kl, 1:nblocal) )
    do m = jf, jl
       do k = kf, kl
          do n = 1, nblocal
             tbuff(m, k, n) = inbuff(n, m, k)
          enddo
       enddo
    enddo
    if (inode == master) then
       allocate( rbuff(jf:jl, kf:kl, 1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*(kl-kf+1)*nbtotal, tbuff, (jl-jf+1)*(kl-kf+1)*nblocal, dfreal, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    deallocate( tbuff )
    if (inode == master) then
       do n = 1, nbtotal
          if (ibuff(n) /= 0) oubuff(ibuff(n), jf:jl, kf:kl) = rbuff(jf:jl, kf:kl, n)
       enddo
       deallocate( rbuff )
       deallocate( ibuff )
    endif
    call dfsync(gdp)
end subroutine dfgather_filter_R3D_sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R3D_hp(lundia, nblocal, nbtotal, nbglobal, jf, jl, kf, kl, order, &
                             & inbuff, oubuff, gdp )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                      :: gdp
integer                                                    :: lundia !  Description and declaration in inout.igs
integer                                                    :: nblocal
integer                                                    :: nbtotal
integer                                                    :: nbglobal
integer                                                    :: jf
integer                                                    :: jl
integer                                                    :: kf
integer                                                    :: kl
integer , dimension(nblocal)                 , intent(in)  :: order
real(hp), dimension(1:nblocal, jf:jl, kf:kl) , intent(in)  :: inbuff
real(sp), dimension(1:nbglobal, jf:jl, kf:kl)              :: oubuff
!
! Local variables
!
real(sp), dimension(:,:,:), allocatable   :: rbuff
!
!! executable statements -------------------------------------------------------
!
    allocate(rbuff(1:nblocal, jf:jl, kf:kl))
    rbuff = real(inbuff,sp)
    call dfgather_filter_R3D_sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, kf, kl, order, &
                             & rbuff, oubuff, gdp )
    deallocate(rbuff)
end subroutine dfgather_filter_R3D_hp
!
!
!===============================================================================
subroutine dfgather_I2e(inparr,nf,nl,mf,ml,iarrc,gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays in global array globar (owned by
!                 master) for writing to Map files
!    Method used: dfgather + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                        :: gdp
integer, dimension(:,:)        , intent(in)  :: inparr
integer, dimension(4,0:nproc-1), intent(in)  :: iarrc
integer, dimension(0:nproc-1)  , intent(in)  :: nf
integer, dimension(0:nproc-1)  , intent(in)  :: nl
integer, dimension(0:nproc-1)  , intent(in)  :: mf
integer, dimension(0:nproc-1)  , intent(in)  :: ml
!
! Local variables
!
integer                         , pointer :: nmaxgl
integer                         , pointer :: mmaxgl
integer                         , pointer :: nfg
integer                         , pointer :: nlg
integer                         , pointer :: mfg
integer                         , pointer :: mlg
integer                                   :: iif
integer                                   :: iil
integer                                   :: jjf
integer                                   :: jjl
integer                                   :: ip
integer                                   :: itag
integer                                   :: istat
integer                                   :: ierr
integer                                   :: nfi
integer                                   :: nla
integer                                   :: mfi
integer                                   :: mla
integer                                   :: nmdim
integer                                   :: n
integer                                   :: m
integer                                   :: nm
integer                                   :: msiz
integer                                   :: nsiz
integer                                   :: lenlo
integer                                   :: lengl
integer                                   :: is
integer, dimension(:), allocatable        :: tmp
integer, dimension(:,:), allocatable      :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
    endif
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl))
    inparr_slice(iif:iil,jjf:jjl) = inparr(iif:iil,jjf:jjl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfint, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(glbari2)) deallocate(glbari2)
       allocate( glbari2(nmaxgl, mmaxgl) )
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                glbari2(n, m) = tmp(nm)
             enddo
          enddo
          is = is + msiz*nsiz
       enddo
       deallocate(tmp)
    endif
#if defined (DFMPI)
call mpi_barrier(MPI_COMM_WORLD, ierr)
#endif
end subroutine dfgather_I2e
!
!
!===============================================================================
subroutine dfgather_R2e_sp(inparr,nf,nl,mf,ml,iarrc,gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays in global array globar (owned by
!                 master) for writing to Map files
!    Method used: dfgather + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                        :: gdp
real(sp), dimension(:,:)        , intent(in) :: inparr
integer , dimension(4,0:nproc-1), intent(in) :: iarrc
integer , dimension(0:nproc-1)  , intent(in) :: nf
integer , dimension(0:nproc-1)  , intent(in) :: nl
integer , dimension(0:nproc-1)  , intent(in) :: mf
integer , dimension(0:nproc-1)  , intent(in) :: ml
!
! Local variables
!
integer                      , pointer :: nmaxgl
integer                      , pointer :: mmaxgl
integer                      , pointer :: nfg
integer                      , pointer :: nlg
integer                      , pointer :: mfg
integer                      , pointer :: mlg
integer                                :: iif
integer                                :: iil
integer                                :: jjf
integer                                :: jjl
integer                                :: ip
integer                                :: itag
integer                                :: istat
integer                                :: ierr
integer                                :: nfi
integer                                :: nla
integer                                :: mfi
integer                                :: mla
integer                                :: nmdim
integer                                :: n
integer                                :: m
integer                                :: nm
integer                                :: msiz
integer                                :: nsiz
integer                                :: lenlo
integer                                :: lengl
integer                                :: is
real(sp), dimension(:), allocatable    :: tmp
real(sp), dimension(:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
    endif
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl))
    inparr_slice(iif:iil,jjf:jjl) = inparr(iif:iil,jjf:jjl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfreal, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(glbarr2)) deallocate(glbarr2)
       allocate( glbarr2(nmaxgl, mmaxgl) )
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                glbarr2(n, m) = tmp(nm)
             enddo
          enddo
          !
          is = is + msiz*nsiz
          !
       enddo
       deallocate(tmp)
    endif
#if defined (DFMPI)
call mpi_barrier(MPI_COMM_WORLD, ierr)
#endif
end subroutine dfgather_R2e_sp
!
!
!
!===============================================================================
subroutine dfgather_R2e_hp(inparr,nf,nl,mf,ml,iarrc,gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays in global array globar (owned by
!                 master) for writing to Map files
!    Method used: dfgather + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                        :: gdp
real(hp), dimension(:,:)        , intent(in) :: inparr
integer , dimension(4,0:nproc-1), intent(in) :: iarrc
integer , dimension(0:nproc-1)  , intent(in) :: nf
integer , dimension(0:nproc-1)  , intent(in) :: nl
integer , dimension(0:nproc-1)  , intent(in) :: mf
integer , dimension(0:nproc-1)  , intent(in) :: ml
!
! Local variables
!
real(sp), dimension(:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2)))
tmp = real(inparr,sp)
call dfgather_R2e_sp(tmp,nf,nl,mf,ml,iarrc,gdp)
deallocate(tmp)
end subroutine dfgather_R2e_hp
!
!
!===============================================================================
subroutine dfgather_R3e_sp(inparr,nf,nl,mf,ml,iarrc,gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays in global array globar (owned by
!                 master) for writing to Map files
!    Method used: dfgather + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                         :: gdp
real(sp), dimension(:,:,:)      , intent(in)  :: inparr
integer , dimension(4,0:nproc-1), intent(in)  :: iarrc
integer , dimension(0:nproc-1)  , intent(in)  :: nf
integer , dimension(0:nproc-1)  , intent(in)  :: nl
integer , dimension(0:nproc-1)  , intent(in)  :: mf
integer , dimension(0:nproc-1)  , intent(in)  :: ml
!
! Local variables
!
integer                       , pointer :: nmaxgl
integer                       , pointer :: mmaxgl
integer                       , pointer :: nfg
integer                       , pointer :: nlg
integer                       , pointer :: mfg
integer                       , pointer :: mlg
integer                                 :: iif
integer                                 :: iil
integer                                 :: jjf
integer                                 :: jjl
integer                                 :: kf
integer                                 :: kl
integer                                 :: ip
integer                                 :: itag
integer                                 :: istat
integer                                 :: ierr
integer                                 :: nfi
integer                                 :: nla
integer                                 :: mfi
integer                                 :: mla
integer                                 :: nmdim
integer                                 :: k
integer                                 :: n
integer                                 :: m
integer                                 :: nm
integer                                 :: msiz
integer                                 :: nsiz
integer                                 :: lenlo
integer                                 :: lengl
integer                                 :: is
real(sp), dimension(:)    , allocatable :: tmp
real(sp), dimension(:,:,:), allocatable :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)
       allocate(tmp(lengl))
    endif
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl))
    inparr_slice(iif:iil,jjf:jjl,kf:kl) = inparr(iif:iil,jjf:jjl,kf:kl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfreal, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(glbarr3)) deallocate(glbarr3)
       allocate( glbarr3(nmaxgl, mmaxgl, kf:kl) )
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do k = kf, kl
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   nm = is + (k - kf)*nsiz*msiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   glbarr3(n, m, k) = tmp(nm)
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)
       enddo
       deallocate(tmp)
    endif
#if defined (DFMPI)
call mpi_barrier(MPI_COMM_WORLD, ierr)
#endif
end subroutine dfgather_R3e_sp
!
!
!
!===============================================================================
subroutine dfgather_R3e_hp(inparr,nf,nl,mf,ml,iarrc,gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays in global array globar (owned by
!                 master) for writing to Map files
!    Method used: dfgather + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                         :: gdp
real(hp), dimension(:,:,:)      , intent(in)  :: inparr
integer , dimension(4,0:nproc-1), intent(in)  :: iarrc
integer , dimension(0:nproc-1)  , intent(in)  :: nf
integer , dimension(0:nproc-1)  , intent(in)  :: nl
integer , dimension(0:nproc-1)  , intent(in)  :: mf
integer , dimension(0:nproc-1)  , intent(in)  :: ml
!
! Local variables
!
real(sp), dimension(:,:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2),size(inparr,3)))
tmp = real(inparr,sp)
call dfgather_R3e_sp(tmp,nf,nl,mf,ml,iarrc,gdp)
deallocate(tmp)
end subroutine dfgather_R3e_hp
!
!
!===============================================================================
subroutine dfgather_R4e_sp(inparr,nf,nl,mf,ml,iarrc,gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays in global array globar (owned by
!                 master) for writing to Map files
!    Method used: dfgather + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                         :: gdp
real(sp), dimension(:,:,:,:)    , intent(in)  :: inparr
integer , dimension(4,0:nproc-1), intent(in)  :: iarrc
integer , dimension(0:nproc-1)  , intent(in)  :: nf
integer , dimension(0:nproc-1)  , intent(in)  :: nl
integer , dimension(0:nproc-1)  , intent(in)  :: mf
integer , dimension(0:nproc-1)  , intent(in)  :: ml
!
! Local variables
!
integer                          , pointer :: nmaxgl
integer                          , pointer :: mmaxgl
integer                          , pointer :: nfg
integer                          , pointer :: nlg
integer                          , pointer :: mfg
integer                          , pointer :: mlg
integer                                    :: iif
integer                                    :: iil
integer                                    :: jjf
integer                                    :: jjl
integer                                    :: kf
integer                                    :: kl
integer                                    :: lf
integer                                    :: ll
integer                                    :: ip
integer                                    :: itag
integer                                    :: istat
integer                                    :: ierr
integer                                    :: nfi
integer                                    :: nla
integer                                    :: mfi
integer                                    :: mla
integer                                    :: nmdim
integer                                    :: k
integer                                    :: l
integer                                    :: n
integer                                    :: m
integer                                    :: nm
integer                                    :: msiz
integer                                    :: nsiz
integer                                    :: lenlo
integer                                    :: lengl
integer                                    :: is
real(sp), dimension(:)      , allocatable  :: tmp
real(sp), dimension(:,:,:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    lf = lbound(inparr,4)
    ll = ubound(inparr,4)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)*(ll-lf+1)
       allocate(tmp(lengl), stat=istat)
       if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R4e allocation problem for tmp array'
    endif
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)*(ll-lf+1)
    iif = 1-gdp%d%nlb+1
    iil = gdp%d%nmax-gdp%d%nlb+1
    jjf = -1-gdp%d%mlb+1
    jjl = gdp%d%mmax+2-gdp%d%mlb+1
    !
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll))
    inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll) = inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfreal, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(glbarr4)) deallocate(glbarr4)
       allocate( glbarr4(nmaxgl, mmaxgl, kf:kl, lf:ll) , stat=istat)
       if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R4e allocation problem for glbarr4 array'
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do l = lf, ll
          do k = kf, kl
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   nm = is + (l - lf)*msiz*nsiz*(kl-kf+1) + (k - kf)*msiz*nsiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   glbarr4(n, m, k, l) = tmp(nm)
                enddo
             enddo
          enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)*(ll-lf+1)
       enddo
       deallocate(tmp)
    endif
#if defined (DFMPI)
call mpi_barrier(MPI_COMM_WORLD, ierr)
#endif
end subroutine dfgather_R4e_sp
!
!
!
!===============================================================================
subroutine dfgather_R4e_hp(inparr,nf,nl,mf,ml,iarrc,gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays in global array globar (owned by
!                 master) for writing to Map files
!    Method used: dfgather + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#if defined (DFMPI)
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                         :: gdp
real(hp), dimension(:,:,:,:)    , intent(in)  :: inparr
integer , dimension(4,0:nproc-1), intent(in)  :: iarrc
integer , dimension(0:nproc-1)  , intent(in)  :: nf
integer , dimension(0:nproc-1)  , intent(in)  :: nl
integer , dimension(0:nproc-1)  , intent(in)  :: mf
integer , dimension(0:nproc-1)  , intent(in)  :: ml
!
! Local variables
!
real(sp), dimension(:,:,:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2),size(inparr,3),size(inparr,4)))
tmp = real(inparr,sp)
call dfgather_R4e_sp(tmp,nf,nl,mf,ml,iarrc,gdp)
deallocate(tmp)
end subroutine dfgather_R4e_hp
!
!
!
!===============================================================================
subroutine dffind_duplicate_C(lundia, nb, nbto, nbgl, nam, duplicate, gdp)
!!--description-----------------------------------------------------------------
!
!    Function: find duplicated stations/cross-sections/etc. over partitions
!              returns nbto (total), nbgl (global=original number without duplicates)
!                      duplicate array (=0 if no duplicate; =i if duplicate of i)
!    Method used: based on names
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                 :: gdp
integer                                 , intent(in)  :: lundia
integer                                 , intent(in)  :: nb
integer                                 , intent(out) :: nbto
integer                                 , intent(out) :: nbgl
character(20), dimension(nb)            , intent(in)  :: nam
integer      , dimension(:), allocatable              :: duplicate
!
! Local variables
!
integer                                     :: m
integer                                     :: n
integer                                     :: ip
integer                                     :: indx
integer      , dimension(:), allocatable    :: nbarr
character(20), dimension(:), allocatable    :: namto
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) allocate( nbarr( nproc ) )
    call dfgather_lowlevel ( nbarr, nproc, nb, 1, dfint, gdp )
    if (inode == master) then
       nbto = SUM(nbarr)
       allocate( namto(1:nbto) )
    endif
    call dfgather_lowlevel ( namto, 20*nbto, nam, 20*nb, dfchar, gdp )
    if (inode == master) then
       allocate( duplicate(nbto) )
       ip = 1
       indx = nbarr(ip)
       do n = 1, nbto
          duplicate(n) = 0
          m = 1
          do while (m < n .and. duplicate(n) == 0)
             if (namto(m) == namto(n)) duplicate(n) = m
             m = m + 1
          enddo
          if (n >= indx) then
             ip = ip + 1
             indx = indx + nbarr(ip)
          endif
       enddo
       nbgl = count(duplicate == 0) 
       deallocate( namto )
       deallocate( nbarr )
    endif
    call dfbroadc( nbgl, 1, dfint, gdp)
    call dfbroadc( nbto, 1, dfint, gdp)
end subroutine dffind_duplicate_C
!
!
!
!===============================================================================
subroutine dffind_duplicate_I(lundia, nb, nbto, nbgl, order, gdp)
!!--description-----------------------------------------------------------------
!
!    Function: find duplicated stations/cross-sections/etc. over partitions
!              returns nbgl
!    Method used: based on original global numbering
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
!
! Global variables
!
type(globdat), target                 :: gdp
integer                 , intent(in)  :: lundia
integer                 , intent(in)  :: nb
integer                 , intent(out) :: nbto
integer                 , intent(out) :: nbgl
integer, dimension(nb)  , intent(in)  :: order
!
! Local variables
!
logical                                     :: found
integer                                     :: m
integer                                     :: n
integer, dimension(:), allocatable          :: nbarr
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) allocate( nbarr( nproc ) )
    call dfgather_lowlevel ( nbarr, nproc, nb, 1, dfint, gdp )
    if (inode == master) then
       nbto = SUM(nbarr)
       deallocate( nbarr )
       allocate( nbarr(1:nbto) )
    endif
    call dfgather_lowlevel ( nbarr, nbto, order, nb, dfint, gdp )
    if (inode == master) then
       nbgl = 0
       do m = 1, nbto
          if (nbarr(m) == 0) cycle ! does not count when order==0
          n = 1
          found = .false.
          search_loop: do while (n <= nbto .and. n /= m)
             if (nbarr(m) == nbarr(n)) then
                found = .true.
                exit search_loop
             endif
             n = n + 1
          enddo search_loop
          if (.not.found) nbgl = nbgl + 1
       enddo
       deallocate( nbarr )
    endif
    call dfbroadc( nbgl, 1, dfint, gdp)
    call dfbroadc( nbto, 1, dfint, gdp)
end subroutine dffind_duplicate_I
!
!
!===============================================================================
subroutine dfgather_I2_seq(inparr,noff,moff,nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array in global array globar
!                  for writing to Map files (sequential mode)
!    Method used: shift indices of input array 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
!
! Global variables
!
integer                         , intent(in) :: nmaxgl
integer                         , intent(in) :: mmaxgl
integer                         , intent(in) :: moff       ! desired offset w.r.t. globarr 
integer                         , intent(in) :: noff       ! desired offset w.r.t. globarr
integer, dimension(:,:)         , intent(in) :: inparr
!
! Local variables
!
integer                                      :: m
integer                                      :: n
!
!! executable statements -------------------------------------------------------
!
if (allocated(glbari2)) deallocate(glbari2)
allocate(glbari2(1:nmaxgl,1:mmaxgl))
do n = 1, nmaxgl
   do m = 1, mmaxgl
      glbari2(n,m) = inparr(n+noff,m+moff)
   enddo
enddo
!
end subroutine dfgather_I2_seq
!
!===============================================================================
subroutine dfgather_R2e_seq_sp(inparr,noff, moff, nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array in global array globar
!                  for writing to Map files (sequential mode)
!    Method used: shift indices of input array 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
!
! Global variables
!
integer                         , intent(in) :: nmaxgl
integer                         , intent(in) :: mmaxgl
integer                         , intent(in) :: moff     ! desired offset w.r.t. globarr
integer                         , intent(in) :: noff     ! desired offset w.r.t. globarr
real(sp), dimension(:,:)        , intent(in) :: inparr
!
! Local variables
!
integer                                      :: m
integer                                      :: n
!
!! executable statements -------------------------------------------------------
!
if (allocated(glbarr2)) deallocate(glbarr2)
allocate(glbarr2(1:nmaxgl,1:mmaxgl))
do n = 1, nmaxgl
   do m = 1, mmaxgl
      glbarr2(n,m) = inparr(n+noff,m+moff)
   enddo
enddo

end subroutine dfgather_R2e_seq_sp
!
!
!===============================================================================
subroutine dfgather_R2e_seq_hp(inparr,noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array in global array globar
!                  for writing to Map files (sequential mode)
!    Method used: shift indices of input array 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
!
! Global variables
!
integer                         , intent(in) :: nmaxgl
integer                         , intent(in) :: mmaxgl
real(hp), dimension(:,:)        , intent(in) :: inparr
integer                         , intent(in) :: moff     ! desired offset w.r.t. globarr
integer                         , intent(in) :: noff     ! desired offset w.r.t. globarr 
!
! Local variables
!
real(sp), dimension(:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2)))
tmp = real(inparr,sp)
call dfgather_R2e_seq_sp(tmp,noff,moff,nmaxgl,mmaxgl)
deallocate(tmp)
end subroutine dfgather_R2e_seq_hp
!
!
!===============================================================================
subroutine dfgather_R3e_seq_sp(inparr,noff,moff,nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array in global array globar
!                  for writing to Map files (sequential mode)
!    Method used: shift indices of input array 

!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
!
! Global variables
!
integer                         , intent(in) :: nmaxgl
integer                         , intent(in) :: mmaxgl
integer                         , intent(in) :: moff     ! desired offset w.r.t. globarr
integer                         , intent(in) :: noff     ! desired offset w.r.t. globarr
real(sp), dimension(:,:,:)      , intent(in) :: inparr
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(inparr,3)
kl = ubound(inparr,3)
if (allocated(glbarr3)) deallocate(glbarr3)
allocate(glbarr3(1:nmaxgl,1:mmaxgl,kf:kl))
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         glbarr3(n,m,k) = inparr(n+noff,m+moff,k)
      enddo
   enddo
enddo
!
end subroutine dfgather_R3e_seq_sp
!
!
!===============================================================================
subroutine dfgather_R3e_seq_hp(inparr,noff,moff,nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array in global array globar
!                  for writing to Map files (sequential mode)
!    Method used: shift indices of input array 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
!
! Global variables
!
integer                         , intent(in) :: nmaxgl
integer                         , intent(in) :: mmaxgl
integer                         , intent(in) :: moff         ! desired offset w.r.t. globarr
integer                         , intent(in) :: noff         ! desired offset w.r.t. globarr
real(hp), dimension(:,:,:)      , intent(in) :: inparr
!
! Local variables
!
real(sp), dimension(:,:,:), allocatable    :: tmp
integer :: istat
!
!! executable statements -------------------------------------------------------
!

allocate(tmp(size(inparr,1),size(inparr,2),size(inparr,3)))


tmp = real(inparr,sp)
call dfgather_R3e_seq_sp(tmp,noff,moff,nmaxgl,mmaxgl)

deallocate(tmp)
end subroutine dfgather_R3e_seq_hp
!
!
!===============================================================================
subroutine dfgather_R4e_seq_sp(inparr,noff,moff,nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array in global array globar
!                  for writing to Map files (sequential mode)
!    Method used: shift indices of input array 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
!
! Global variables
!
integer                         , intent(in) :: nmaxgl
integer                         , intent(in) :: mmaxgl
integer                         , intent(in) :: moff         ! desired offset w.r.t. globarr
integer                         , intent(in) :: noff         ! desired offset w.r.t. globarr
real(sp), dimension(:,:,:,:)    , intent(in) :: inparr
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
integer                                      :: l
integer                                      :: lf
integer                                      :: ll
!
!! executable statements -------------------------------------------------------
!
kf = lbound(inparr,3)
kl = ubound(inparr,3)
lf = lbound(inparr,4)
ll = ubound(inparr,4)
    
if (allocated(glbarr4)) deallocate(glbarr4)
allocate(glbarr4(1:nmaxgl,1:mmaxgl,kf:kl,lf:ll))
do l = lf, ll
   do k = kf, kl
      do n = 1, nmaxgl
         do m = 1, mmaxgl
            glbarr4(n,m,k,l) = inparr(n+noff,m+moff,k,l)
         enddo
      enddo
   enddo
enddo   

end subroutine dfgather_R4e_seq_sp
!
!
!===============================================================================
subroutine dfgather_R4e_seq_hp(inparr,noff,moff,nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array in global array globar
!                  for writing to Map files (sequential mode)
!    Method used: shift indices of input array 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
!
! Global variables
!
integer                         , intent(in) :: nmaxgl
integer                         , intent(in) :: mmaxgl
integer                         , intent(in) :: moff       ! desired offset w.r.t. globarr
integer                         , intent(in) :: noff       ! desired offset w.r.t. globarr
real(hp), dimension(:,:,:,:)    , intent(in) :: inparr
!
! Local variables
!
real(sp), dimension(:,:,:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2),size(inparr,3),size(inparr,4)))
tmp = real(inparr,sp)
call dfgather_R4e_seq_sp(tmp,noff,moff,nmaxgl,mmaxgl)
deallocate(tmp)
end subroutine dfgather_R4e_seq_hp
!
!
!===============================================================================
subroutine dfcleanup_glbarrs
   if (allocated(glbarr2)) deallocate(glbarr2)
   if (allocated(glbarr3)) deallocate(glbarr3)
   if (allocated(glbarr4)) deallocate(glbarr4)
   if (allocated(glbari2)) deallocate(glbari2)
end subroutine dfcleanup_glbarrs


end module



