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

!-- PACKAGE m_mpidum ---------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Dummy MPI implementation for compilation of an application that uses
!   CouPLib on a platform where no real MPI-implementation is available.
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_io/src/mpidum/mpidum.f90 $
!   $Revision: 31 $, $Date: 2007-11-01 11:16:19 +0100 (Thu, 01 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  04-09-2007  initial version
!-----------------------------------------------------------------------------

subroutine mpi_init(ierr)
integer ierr
   ierr = 0
end subroutine mpi_init

subroutine mpi_init_thread(level, supported, ierr)
integer level
integer supported
integer ierr
   supported = level ! We lie, of course, but this is to ensure the program
                     ! continues fine
   ierr = 0
end subroutine mpi_init_thread

subroutine mpi_comm_rank(comm, rank, ierr)
integer comm, rank, ierr
   rank = 0
   ierr = 0
end subroutine mpi_comm_rank

subroutine mpi_comm_size(comm, size, ierr)
integer comm, size, ierr
   size = 1
   ierr = 0
end subroutine mpi_comm_size

subroutine mpi_finalize(ierr)
integer ierr
   ierr = 0
end subroutine mpi_finalize

subroutine mpi_get_processor_name(hostname, lennam, ierr)
integer lennam, ierr
character(len=*) hostname
   hostname = 'unknown'
   ierr = 0
end subroutine mpi_get_processor_name


!-- SUBROUTINE mpidum_error --------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Give error message and stop program
!!>
!!  On entry:
!!  subnam      name of MPI routine thtat is called by external program
!!
!!  On return:
!!  -           an error message is written to stdout and the program is
!!              aborted.
!!<
!-----------------------------------------------------------------------------
subroutine mpidum_error(subnam)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in)  :: subnam

!-- LOCAL VARIABLES
!-----------------------------------------------------------------------------

   write(*,*)
   write(*,'(3a)') 'ERROR: the program is trying to access MPI-function "', &
      trim(subnam),'"'
   write(*,'(2a)') '       However, in this program, MPI-version mpidum is ', &
      'used: a dummy'
   write(*,'(2a)') '       MPI-implementation. Therefore the use of MPI-', &
      'functions is not '
   write(*,'(1a)') '       supported. Aborting program.'
   call exit(1)
end subroutine mpidum_error


subroutine mpi_allreduce(send, recv, numval, datatype, mpi_op, comm, ierr)
   call mpidum_error('mpi_allreduce')
end subroutine mpi_allreduce

subroutine mpi_barrier(comm,ierr)
   call mpidum_error('mpi_barrier')
end subroutine mpi_barrier

subroutine mpi_bcast(send, nelem, mpi_op, iroot, comm, ierror)
   call mpidum_error('mpi_bcast')
end subroutine mpi_bcast

subroutine mpi_bsend
   call mpidum_error('mpi_bsend')
end subroutine mpi_bsend

subroutine mpi_comm_create()
   call mpidum_error('mpi_comm_create')
end subroutine mpi_comm_create

subroutine mpi_get_count(stat, mpi_op, nelem, ierror)
   call mpidum_error('mpi_get_count')
end subroutine mpi_get_count

subroutine mpi_isend(send, nelem, mpi_op, dest, tag, comm, ireq, ierror)
   call mpidum_error('mpi_isend')
end subroutine mpi_isend

subroutine mpi_op_create(combine_handle, switch, handle, ierr)
   call mpidum_error('mpi_op_create')
end subroutine mpi_op_create

subroutine mpi_probe(src, tag, comm, stat, ierror)
   call mpidum_error('mpi_probe')
end subroutine mpi_probe

subroutine mpi_recv(work, nelem, mpi_op, src, tag, comm, stat, ierror)
   call mpidum_error('mpi_recv')
end subroutine mpi_recv

subroutine mpi_send(send, nelem, mpi_ip, dest, tag, comm, ierror)
   call mpidum_error('mpi_send')
end subroutine mpi_send

subroutine mpi_type_commit(dattyp, ierr)
   call mpidum_error('mpi_type_commit')
end subroutine mpi_type_commit

subroutine mpi_type_contiguous(int, mpi_op, dattyp, ierr)
   call mpidum_error('mpi_type_contiguous')
end subroutine mpi_type_contiguous

subroutine mpi_type_free(mpi_type, ierr)
   call mpidum_error('mpi_type_free')
end subroutine mpi_type_free

subroutine mpi_type_size(itype, ndim, ierr)
   call mpidum_error('mpi_type_size')
end subroutine mpi_type_size

subroutine mpi_wait(request,stat,ierr)
   call mpidum_error('mpi_wait')
end subroutine mpi_wait

