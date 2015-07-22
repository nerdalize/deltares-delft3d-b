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

!-- MODULE m_globcomm --------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Subroutines for global data exchange within groups of processes in a
!   coupled run.
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/m_globcomm.f90 $
!   $Revision: 35 $, $Date: 2007-11-05 16:39:45 +0100 (Mon, 05 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------
module m_globcomm
use m_timings
use m_coupcns
use m_prcgrp
implicit none
private

! default level of debug-output for the global communication operations:
integer, parameter :: idebug_globcomm = 0

! subroutines for global communications:

public globcomm_initmod
public sync_processes
public combine_data
public combine_1d_idata
public combine_1d_rdata
public combine_1d_ddata
public combine_2d_idata
public combine_2d_rdata
public combine_2d_ddata

interface combine_data
   module procedure combine_1d_idata
   module procedure combine_1d_rdata
   module procedure combine_1d_ddata
   module procedure combine_2d_idata
   module procedure combine_2d_rdata
   module procedure combine_2d_ddata
end interface combine_data

! counter for global operations per process group (currently: MPI_COMM_ALL),
! used for determining message tags.
! in COCLIB 1000 tags are used per group and recycled after 1000 communications;
! an offset can be used to avoid conflicts with other types of operations.
! in COCLIB a separate counter is used for the total number of updates, a.o.
! for output- and debugging purposes.
integer                 :: cnt_glob   = 0

! MPI data-types for global communication operations that work on small vectors
integer, private, parameter :: MAX_VECLEN = 10
integer, private            :: dattyp_int(MAX_VECLEN)
integer, private            :: dattyp_real(MAX_VECLEN)
integer, private            :: dattyp_dble(MAX_VECLEN)

! handles to routines for combining vectors of data
integer, private            :: ihndl_imax1 = 0
integer, private            :: ihndl_rmax1 = 0
integer, private            :: ihndl_dmax1 = 0

integer, private            :: globcomm_initialized = 0

contains

!-- SUBROUTINE globcomm_initmod ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Initialize the CouPLib data-structures w.r.t. global communication
!!>
!!  On entry:
!!  idebug      optional requested level of debug output (0=none)
!!
!!  On return:
!!  -           MPI-types needed for global communication have been initialized
!!<
!-----------------------------------------------------------------------------
subroutine globcomm_initmod(idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in), optional :: idebug

!-- LOCAL VARIABLES
! internal value of idebug
integer         :: my_idebug
integer         :: i, ierr
!-----------------------------------------------------------------------------

!  handle optional idebug

   my_idebug = 1
   if (present(idebug)) my_idebug = idebug

!  return directly in non-coupled runs, avoid use of MPI

   if (iscple.eq.0) then

      globcomm_initialized = 1

   elseif (globcomm_initialized.gt.0) then

      write(LOUT,*) 'globcomm_initmod: Error: called for the second time.'

   else

!     Register subroutine for global operation CP_MAX1

      if (my_idebug.ge.2) write(LOUT,*) 'globcomm_initmod: register operation',&
         ' combine_oper_max1...'
      call mpi_op_create(combine_oper_imax1, .true., ihndl_imax1, ierr)
      call mpi_op_create(combine_oper_rmax1, .true., ihndl_rmax1, ierr)
      call mpi_op_create(combine_oper_dmax1, .true., ihndl_dmax1, ierr)

!     Register MPI datatypes for global operations on vectors of 1..MAX_VECLEN
!        elements

      do i = 1, MAX_VECLEN
         if (my_idebug.ge.2) write(LOUT,*) 'globcomm_initmod: register type ',&
            'dattyp_int (',i,')...'
         call mpi_type_contiguous(i, MPI_INTEGER, dattyp_int(i), ierr)
         call mpi_type_commit(dattyp_int(i), ierr)

         if (my_idebug.ge.2) write(LOUT,*) 'globcomm_initmod: register type ',&
            'dattyp_real(',i,')...'
         call mpi_type_contiguous(i, MPI_REAL, dattyp_real(i), ierr)
         call mpi_type_commit(dattyp_real(i), ierr)

         if (my_idebug.ge.2) write(LOUT,*) 'globcomm_initmod: register type ',&
            'dattyp_dble(',i,')...'
         call mpi_type_contiguous(i, MPI_REAL8, dattyp_dble(i), ierr)
         call mpi_type_commit(dattyp_dble(i), ierr)
      enddo
   endif

   globcomm_initialized = 1

end subroutine globcomm_initmod


!-- SUBROUTINE sync_processes ------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Synchronize a group of processes in a parallel run; small wrapper around
!!  mpi_barrier.
!!>
!!  On entry:
!!  -
!!
!!  On return:
!!  -           all processes of the group have called this routine
!!<
!-----------------------------------------------------------------------------
subroutine sync_processes()

!-- HEADER VARIABLES/ARGUMENTS
implicit none

!-- LOCAL VARIABLES
integer     :: ierr
!-----------------------------------------------------------------------------

!  return directly in non-coupled runs, avoid use of MPI
   if (iscple.eq.0) return

   if (use_timers) call timer_start(itimer_commop(ISYNCTM,ISYNCHR))
   call mpi_barrier(MPI_COMM_ALL, ierr)
   if (use_timers) call timer_stop(itimer_commop(ISYNCTM,ISYNCHR))
end subroutine sync_processes


!-- SUBROUTINE combine_1d_idata ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Combine data of a group of processes in a parallel run; small wrapper
!!  around mpi_allreduce.
!!>
!!  On entry:
!!  iarray      contribution to output data of current process
!!  numval      number of data-items in iarray, must be identical on all
!!              processes
!!  oper        type of operation to be performed when combining data of
!!              different processes: cp_sum, cp_max, etc.
!!
!!  On return:
!!  iarray      the data contributed by the different processes has been
!!              combined and the result is available to all processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine combine_1d_idata(iarray, numval, oper, info)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,               intent(in)     :: numval
integer, dimension(*), intent(inout)  :: iarray(numval)
integer,               intent(in)     :: oper
integer,               intent(out)    :: info

!-- LOCAL VARIABLES
integer         :: ierr
! handle to MPI combination operator
integer         :: mpi_op
!-----------------------------------------------------------------------------

!  return directly in non-coupled runs, avoid use of MPI
   if (iscple.eq.0) return

!  If synchronization time is to be measured separately:
!     wait for other processes, measure the time needed

   if (measure_idletime) then
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,ICOMBIN))
      call mpi_barrier(MPI_COMM_ALL, ierr)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,ICOMBIN))
   endif

!  Start timing of communication phase of combine_data

   if (use_timers) call timer_start(itimer_commop(ICOMMTM,ICOMBIN))

!  determine MPI-operation corresponding to CouPLib operation "oper"

   mpi_op = MPI_UNDEFINED
   if     (oper.eq.CP_SUM) then
      mpi_op = MPI_SUM
   elseif (oper.eq.CP_MAX) then
      mpi_op = MPI_MAX
   elseif (oper.eq.CP_MIN) then
      mpi_op = MPI_MIN
   endif

   call mpi_allreduce(iarray, iarray, numval, MPI_INTEGER, mpi_op, &
                      MPI_COMM_ALL, ierr)
   info = ierr

!  Stop timing of communication phase of combine_data

   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,ICOMBIN))

end subroutine combine_1d_idata


!-- SUBROUTINE combine_1d_rdata ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Combine data of a group of processes in a parallel run; small wrapper
!!  around mpi_allreduce.
!!>
!!  On entry:
!!  rarray      contribution to output data of current process
!!  numval      number of data-items in rarray, must be identical on all
!!              processes
!!  oper        type of operation to be performed when combining data of
!!              different processes: cp_sum, cp_max, etc.
!!
!!  On return:
!!  rarray      the data contributed by the different processes has been
!!              combined and the result is available to all processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine combine_1d_rdata(rarray, numval, oper, info)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,                    intent(in)     :: numval
real(kind=4), dimension(*), intent(inout)  :: rarray(numval)
integer,                    intent(in)     :: oper
integer,                    intent(out)    :: info

!-- LOCAL VARIABLES
integer         :: ierr
! handle to MPI combination operator
integer         :: mpi_op
!-----------------------------------------------------------------------------

!  return directly in non-coupled runs, avoid use of MPI
   if (iscple.eq.0) return

!  If synchronization time is to be measured separately:
!     wait for other processes, measure the time needed

   if (measure_idletime) then
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,ICOMBIN))
      call mpi_barrier(MPI_COMM_ALL, ierr)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,ICOMBIN))
   endif

!  Start timing of communication phase of combine_data

   if (use_timers) call timer_start(itimer_commop(ICOMMTM,ICOMBIN))

!  determine MPI-operation corresponding to CouPLib operation "oper"

   mpi_op = MPI_UNDEFINED
   if     (oper.eq.CP_SUM) then
      mpi_op = MPI_SUM
   elseif (oper.eq.CP_MAX) then
      mpi_op = MPI_MAX
   elseif (oper.eq.CP_MIN) then
      mpi_op = MPI_MIN
   endif

   call mpi_allreduce(rarray, rarray, numval, MPI_REAL, mpi_op, &
                      MPI_COMM_ALL, ierr)
   info = ierr

!  Stop timing of communication phase of combine_data

   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,ICOMBIN))

end subroutine combine_1d_rdata


!-- SUBROUTINE combine_1d_ddata ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Combine data of a group of processes in a parallel run; small wrapper
!!  around mpi_allreduce.
!!>
!!  On entry:
!!  darray      contribution to output data of current process
!!  numval      number of data-items in darray, must be identical on all
!!              processes
!!  oper        type of operation to be performed when combining data of
!!              different processes: cp_sum, cp_max, etc.
!!
!!  On return:
!!  darray      the data contributed by the different processes has been
!!              combined and the result is available to all processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine combine_1d_ddata(darray, numval, oper, info)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,                    intent(in)     :: numval
real(kind=8), dimension(*), intent(inout)  :: darray(numval)
integer,                    intent(in)     :: oper
integer,                    intent(out)    :: info

!-- LOCAL VARIABLES
integer         :: ierr
! handle to MPI combination operator
integer         :: mpi_op
!-----------------------------------------------------------------------------

!  return directly in non-coupled runs, avoid use of MPI
   if (iscple.eq.0) return

!  If synchronization time is to be measured separately:
!     wait for other processes, measure the time needed

   if (measure_idletime) then
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,ICOMBIN))
      call mpi_barrier(MPI_COMM_ALL, ierr)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,ICOMBIN))
   endif

!  Start timing of communication phase of combine_data

   if (use_timers) call timer_start(itimer_commop(ICOMMTM,ICOMBIN))

!  determine MPI-operation corresponding to CouPLib operation "oper"

   mpi_op = MPI_UNDEFINED
   if     (oper.eq.CP_SUM) then
      mpi_op = MPI_SUM
   elseif (oper.eq.CP_MAX) then
      mpi_op = MPI_MAX
   elseif (oper.eq.CP_MIN) then
      mpi_op = MPI_MIN
   endif

   call mpi_allreduce(darray, darray, numval, MPI_REAL8, mpi_op, &
                      MPI_COMM_ALL, ierr)
   info = ierr

!  Stop timing of communication phase of combine_data

   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,ICOMBIN))

end subroutine combine_1d_ddata


!-- SUBROUTINE combine_2d_idata ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Combine data of a group of processes in a parallel run; The data consists
!!  of numvec small vectors of lenvec items each. The corresponding vectors
!!  of different processes are combined using operation "oper".
!!>
!!  On entry:
!!  iarray      contribution to output data of current process.
!!  lenvec      length of each vector in iarray.
!!  numvec      number of data-items (vectors) in iarray, must be identical
!!              on all processes
!!  oper        type of operation to be performed when combining data of
!!              different processes: cp_max1, etc.
!!
!!  On return:
!!  iarray      the data contributed by the different processes has been
!!              combined and the result is available to all processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine combine_2d_idata(iarray, lenvec, numvec, oper, info)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,                      intent(in)     :: lenvec
integer,                      intent(in)     :: numvec
integer,      dimension(:,:), intent(inout)  :: iarray(lenvec,numvec)
integer,                      intent(in)     :: oper
integer,                      intent(out)    :: info

!-- LOCAL VARIABLES
integer         :: ierr
! handle to MPI combination operator, data-type
integer         :: mpi_op, mpi_type
!-----------------------------------------------------------------------------

!  return directly in non-coupled runs, avoid use of MPI
   if (iscple.eq.0) return

!  for element-wise operations: call 1D version of this routine

   if (oper.eq.CP_SUM .or. oper.eq.CP_MAX .or. oper.eq.CP_MIN) then

      call combine_1d_idata(iarray, lenvec*numvec, oper, info)

!  vector-wise operations:

   else

!     If synchronization time is to be measured separately:
!        wait for other processes, measure the time needed

      if (measure_idletime) then
         if (use_timers) call timer_start(itimer_commop(ISYNCTM,ICOMBIN))
         call mpi_barrier(MPI_COMM_ALL, ierr)
         if (use_timers) call timer_stop(itimer_commop(ISYNCTM,ICOMBIN))
      endif

!     Start timing of communication phase of combine_data

      if (use_timers) call timer_start(itimer_commop(ICOMMTM,ICOMBIN))

!     determine handle of MPI datatype for vectors of lenvec elements

      if (lenvec.le.MAX_VECLEN) then
         mpi_type = dattyp_int(lenvec)
      else
         call mpi_type_contiguous(lenvec, MPI_INTEGER, mpi_type, ierr)
         call mpi_type_commit(mpi_type, ierr)
      endif

!     determine MPI-operation corresponding to CouPLib operation "oper"

      mpi_op = MPI_UNDEFINED
      if     (oper.eq.CP_MAX1) then
         mpi_op = ihndl_imax1
      endif

      call mpi_allreduce(iarray, iarray, numvec, mpi_type, mpi_op, &
                         MPI_COMM_ALL, ierr)
      info = ierr

!     de-register on-the-fly created MPI datatype

      if (lenvec.gt.MAX_VECLEN) call mpi_type_free(mpi_type, ierr)

!     Stop timing of communication phase of combine_data

      if (use_timers) call timer_stop(itimer_commop(ICOMMTM,ICOMBIN))

   endif

end subroutine combine_2d_idata


!-- SUBROUTINE combine_2d_rdata ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Combine data of a group of processes in a parallel run; The data consists
!!  of numvec small vectors of lenvec items each. The corresponding vectors
!!  of different processes are combined using operation "oper".
!!>
!!  On entry:
!!  rarray      contribution to output data of current process.
!!  lenvec      length of each vector in rarray.
!!  numvec      number of data-items (vectors) in rarray, must be identical
!!              on all processes
!!  oper        type of operation to be performed when combining data of
!!              different processes: cp_max1, etc.
!!
!!  On return:
!!  rarray      the data contributed by the different processes has been
!!              combined and the result is available to all processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine combine_2d_rdata(rarray, lenvec, numvec, oper, info)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,                      intent(in)     :: lenvec
integer,                      intent(in)     :: numvec
real(kind=4), dimension(:,:), intent(inout)  :: rarray(lenvec,numvec)
integer,                      intent(in)     :: oper
integer,                      intent(out)    :: info

!-- LOCAL VARIABLES
integer         :: ierr
! handle to MPI combination operator, data-type
integer         :: mpi_op, mpi_type
!-----------------------------------------------------------------------------

!  return directly in non-coupled runs, avoid use of MPI
   if (iscple.eq.0) return

!  for element-wise operations: call 1D version of this routine

   if (oper.eq.CP_SUM .or. oper.eq.CP_MAX .or. oper.eq.CP_MIN) then

      call combine_1d_rdata(rarray, lenvec*numvec, oper, info)

!  vector-wise operations:

   else

!     If synchronization time is to be measured separately:
!        wait for other processes, measure the time needed

      if (measure_idletime) then
         if (use_timers) call timer_start(itimer_commop(ISYNCTM,ICOMBIN))
         call mpi_barrier(MPI_COMM_ALL, ierr)
         if (use_timers) call timer_stop(itimer_commop(ISYNCTM,ICOMBIN))
      endif

!     Start timing of communication phase of combine_data

      if (use_timers) call timer_start(itimer_commop(ICOMMTM,ICOMBIN))

!     determine handle of MPI datatype for vectors of lenvec elements

      if (lenvec.le.MAX_VECLEN) then
         mpi_type = dattyp_real(lenvec)
      else
         call mpi_type_contiguous(lenvec, MPI_REAL, mpi_type, ierr)
         call mpi_type_commit(mpi_type, ierr)
      endif

!     determine MPI-operation corresponding to CouPLib operation "oper"

      mpi_op = MPI_UNDEFINED
      if     (oper.eq.CP_MAX1) then
         mpi_op = ihndl_rmax1
      endif

      call mpi_allreduce(rarray, rarray, numvec, mpi_type, mpi_op, &
                         MPI_COMM_ALL, ierr)
      info = ierr

!     de-register on-the-fly created MPI datatype

      if (lenvec.gt.MAX_VECLEN) call mpi_type_free(mpi_type, ierr)

!     Stop timing of communication phase of combine_data

      if (use_timers) call timer_stop(itimer_commop(ICOMMTM,ICOMBIN))

   endif

end subroutine combine_2d_rdata


!-- SUBROUTINE combine_2d_ddata ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Combine data of a group of processes in a parallel run; The data consists
!!  of numvec small vectors of lenvec items each. The corresponding vectors
!!  of different processes are combined using operation "oper".
!!>
!!  On entry:
!!  darray      contribution to output data of current process.
!!  lenvec      length of each vector in darray.
!!  numvec      number of data-items (vectors) in darray, must be identical
!!              on all processes
!!  oper        type of operation to be performed when combining data of
!!              different processes: cp_max1, etc.
!!
!!  On return:
!!  darray      the data contributed by the different processes has been
!!              combined and the result is available to all processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine combine_2d_ddata(darray, lenvec, numvec, oper, info)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,                      intent(in)     :: lenvec
integer,                      intent(in)     :: numvec
real(kind=8), dimension(:,:), intent(inout)  :: darray(lenvec,numvec)
integer,                      intent(in)     :: oper
integer,                      intent(out)    :: info

!-- LOCAL VARIABLES
integer         :: ierr
! handle to MPI combination operator, data-type
integer         :: mpi_op, mpi_type
!-----------------------------------------------------------------------------

!  return directly in non-coupled runs, avoid use of MPI
   if (iscple.eq.0) return

!  for element-wise operations: call 1D version of this routine

   if (oper.eq.CP_SUM .or. oper.eq.CP_MAX .or. oper.eq.CP_MIN) then

      call combine_1d_ddata(darray, lenvec*numvec, oper, info)

!  vector-wise operations:

   else

!     If synchronization time is to be measured separately:
!        wait for other processes, measure the time needed

      if (measure_idletime) then
         if (use_timers) call timer_start(itimer_commop(ISYNCTM,ICOMBIN))
         call mpi_barrier(MPI_COMM_ALL, ierr)
         if (use_timers) call timer_stop(itimer_commop(ISYNCTM,ICOMBIN))
      endif

!     Start timing of communication phase of combine_data

      if (use_timers) call timer_start(itimer_commop(ICOMMTM,ICOMBIN))

!     determine handle of MPI datatype for vectors of lenvec elements

      if (lenvec.le.MAX_VECLEN) then
         mpi_type = dattyp_dble(lenvec)
      else
         call mpi_type_contiguous(lenvec, MPI_REAL8, mpi_type, ierr)
         call mpi_type_commit(mpi_type, ierr)
      endif

!     determine MPI-operation corresponding to CouPLib operation "oper"

      mpi_op = MPI_UNDEFINED
      if     (oper.eq.CP_MAX1) then
         mpi_op = ihndl_dmax1
      endif

      call mpi_allreduce(darray, darray, numvec, mpi_type, mpi_op, &
                         MPI_COMM_ALL, ierr)
      info = ierr

!     de-register on-the-fly created MPI datatype

      if (lenvec.gt.MAX_VECLEN) call mpi_type_free(mpi_type, ierr)

!     Stop timing of communication phase of combine_data

      if (use_timers) call timer_stop(itimer_commop(ICOMMTM,ICOMBIN))

   endif

end subroutine combine_2d_ddata


!-- SUBROUTINE combine_oper_imax1 --------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  MPI-callable routine for combination of small vectors for operation CP_MAX1:
!!  selecting input-vector with maximum 1st element
!!>
!!  On entry:
!!  numvec      number of data-items of MPI datatype "itype"
!!  iarr1       first array of vectors ("itype,numvec") for combination
!!              operation, input only
!!  iarr2       second array of vectors for combination operation, input/output
!!  itype       handle to MPI datatype for the elements of iarr1 and iarr2
!!
!!  On return:
!!  ovar1       description of output-variables
!!<
!-----------------------------------------------------------------------------
subroutine combine_oper_imax1(iarr1, iarr2, numvec, itype)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,      dimension(*), intent(in)    :: iarr1
integer,      dimension(*), intent(inout) :: iarr2
integer,                    intent(in)    :: numvec
integer,                    intent(in)    :: itype

!-- LOCAL VARIABLES
integer         :: ndim, ivec, iel, iof, ierr
!-----------------------------------------------------------------------------

!  determine the number of data-items per vector

   call mpi_type_size(itype, ndim, ierr)
   ndim = ndim / 4

!  for all elements of iarr1 and iarr2 do:

   do ivec = 1, numvec
      iof = (ivec-1)*ndim

!     if first entry of vec1 is larger than first entry of vec2

      if (iarr1(iof+1) .gt. iarr2(iof+1)) then

!        copy vec1 to vec2

         do iel = 1, ndim
            iarr2(iof+iel) = iarr1(iof+iel)
         enddo
      endif
   enddo

end subroutine combine_oper_imax1


subroutine combine_oper_rmax1(rarr1, rarr2, numvec, itype)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
real(kind=4), dimension(*), intent(in)    :: rarr1
real(kind=4), dimension(*), intent(inout) :: rarr2
integer,                    intent(in)    :: numvec, itype
!-- LOCAL VARIABLES
integer         :: ndim, ivec, iel, iof, ierr

   call mpi_type_size(itype, ndim, ierr)
   ndim = ndim / 4
   do ivec = 1, numvec
      iof = (ivec-1)*ndim
      if (rarr1(iof+1) .gt. rarr2(iof+1)) then
         do iel = 1, ndim
            rarr2(iof+iel) = rarr1(iof+iel)
         enddo
      endif
   enddo
end subroutine combine_oper_rmax1


subroutine combine_oper_dmax1(darr1, darr2, numvec, itype)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
real(kind=8), dimension(*), intent(in)    :: darr1
real(kind=8), dimension(*), intent(inout) :: darr2
integer,                    intent(in)    :: numvec, itype
!-- LOCAL VARIABLES
integer         :: ndim, ivec, iel, iof, ierr

   call mpi_type_size(itype, ndim, ierr)
   ndim = ndim / 8

   do ivec = 1, numvec
      iof = (ivec-1)*ndim
      if (darr1(iof+1) .gt. darr2(iof+1)) then
         do iel = 1, ndim
            darr2(iof+iel) = darr1(iof+iel)
         enddo
      endif
   enddo
end subroutine combine_oper_dmax1


end module m_globcomm
