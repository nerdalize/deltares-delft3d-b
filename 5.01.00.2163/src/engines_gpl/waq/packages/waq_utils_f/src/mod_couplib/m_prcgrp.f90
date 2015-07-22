!!  Copyright(C) Stichting Deltares, 2012.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!-- MODULE m_prcgrp ----------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Definition of data-structures and subroutines regarding processes and
!   groups of the CouPLib communication library
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/m_prcgrp.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------
module m_prcgrp
use m_timings
use m_coupcns
implicit none
!-----------------------------------------------------------------------------
! WARNING: using default PUBLIC instead of PRIVATE because of mpif.h
!-----------------------------------------------------------------------------
public

! the set of processes that is coupled via CouPLib should be defined in an
! MPI-communicator "MPI_COMM_ALL". This is the main process-group 'all' as
! far as CouPLib is concerned.
!
! In a future version, various sub-groups of 'all' may be defined. This module
! registers the data per process-group. For instance in domain decomposition
! multiple groups 'mydomain' may be defined; each process of group 'all' says
! to which version of 'mydomain' it belongs (version 1 for domain 1, instance
! 2 for domain 2, etc), and each process registers the data for its own version
! of the process group.
!
! Processes are counted from 1 to numprc, with numprc the number of processes in
! a group. MPI communication is done via ranks, counting from 0 to numprc-1. For
! now we assume that processes are ordered by rank, such that the task-id of a
! process is itid = irank = iprc-1.


include 'mpif.h'
!integer, parameter, public :: MPI_REAL8 = MPI_DOUBLE_PRECISION

! subroutines for configuration of processes and groups:
public  prcgrp_initmod

! overall MPI process group as far as CouPLib is concerned:
! (maybe this value should be settable by the calling program)
integer, parameter, public   :: MPI_COMM_ALL = MPI_COMM_WORLD

! flag indicating whether this is a coupled run or not
integer, public              :: iscple=0

! total number of MPI processes in group 'all'
integer, public              :: numprc=0

! own process number and MPI rank within process group 'all'
integer, public              :: myprc=0, myrank=0

! possible extensions for group 'all':
!  - itids, list of task-id's of all processes in the group
!  - iprtid, task-id of parent process or master-process?
!  - the group-names may be stored
!  - the data needed for forming message-tags may be defined separately per
!    group and may be added to the group-administration.

integer, private             :: prcgrp_initialized=0

contains


!-- SUBROUTINE prcgrp_initmod ------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Initialize the CouPLib data-structures w.r.t. processes and groups
!!>
!!  On entry:
!!  idebug      optional requested level of debug output (0=none)
!!
!!  On return:
!!  -           MPI and the variables for processes/groups have been initialized
!!<
!-----------------------------------------------------------------------------
subroutine prcgrp_initmod(idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in), optional :: idebug

!-- LOCAL VARIABLES
! internal value of idebug
integer           :: my_idebug
integer           :: lennam, ierr
character(len=80) :: hostname
integer           :: supported_level
!-----------------------------------------------------------------------------

!  handle optional idebug

   my_idebug = 2
   if (present(idebug)) my_idebug = idebug

!  Note: during initialization, output-stream LOUT may not have been opened
!        yet!!

   if (prcgrp_initialized.gt.0) then

      write(*,*) 'prcgrp_initmod: Error: called for the second time.'

   else

!     First register with MPI and start-up the parallel run

      if (my_idebug.ge.2) write(*,*) 'starting MPI...'
      call mpi_init_thread(mpi_thread_funneled, supported_level, ierr)
      if ( supported_level < mpi_thread_funneled ) then
          write(*,*) 'MPI and OpenMP do not mix! Sorry!'
          stop
      endif

!     Determine total number of processes

      call mpi_comm_size(MPI_COMM_ALL, numprc, ierr)
      if (my_idebug.ge.2) write(*,*) 'prcgrp: numprc=',numprc,', ierr=',ierr

!     If numprc > 1: this is a coupled run, else non-coupled run

      iscple = 0
      if (numprc.gt.1) iscple = 1

!     Determine own rank (0:numprc-1) and process-id (1:numprc) within group of
!     all processes

      call mpi_comm_rank(MPI_COMM_ALL, myrank, ierr)
      myprc = myrank + 1
      if (my_idebug.ge.2) write(*,*) 'prcgrp: myrank,prc=',myrank,myprc

!     Print host/processor name when requested

      if (my_idebug.ge.1) then
         call mpi_get_processor_name(hostname, lennam, ierr)
         write(*,'(2(a,i3),3a)') ' Process', myprc,' of',numprc, &
            ': running at host="', trim(hostname), '"'
      endif

   endif

   prcgrp_initialized = 1

end subroutine prcgrp_initmod


end module m_prcgrp
