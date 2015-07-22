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

!-- MODULE m_sndrcv ----------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Subroutines for distributing data between different processes in a
!   coupled run using generic index tables.
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/m_sndrcv.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------
module m_sndrcv
use m_timings
use m_coupcns
use m_prcgrp
use m_ixset
use m_intfc
implicit none
private

! default level of debug-output for the distribute, collect, update and
! accumulate operations:
integer, parameter :: idebug_distribute_card  = 0
integer, parameter :: idebug_distribute_named = 0
integer, parameter :: idebug_collect          = 0
integer, parameter :: idebug_update           = 0
integer, parameter :: idebug_accumulate       = 0

! subroutines for the communications:

public sndrcv_stopmod

public distribute_data
public distribute_idata
public distribute_rdata
public distribute_ddata

public collect_data
public collect_idata
public collect_rdata
public collect_ddata

public update_data
public update_idata
public update_rdata
public update_ddata

public accumulate_data
public accumulate_idata
public accumulate_rdata
public accumulate_ddata

! The generic distribute-, collect-, update- and accumulate-operations are
! implemented using separate routines for logical, integer, real, double
! precision and character data.
!
! There are three versions of distribute: one for replicated index-sets, where
! each process holds all indices and the entire array must be received, and
! two for distributed index-sets where each process needs a different section
! of the array. The latter two versions differ in that one assumes that the
! structure of the array is fully described by the named index-set, whereas the
! other allows for pre- and post-multiplication factors. This allows to
! define replication of the index-set and interface on the fly. This approach
! will probably not hold for interpolation in replicated dimensions.
!
! The various routines are provided in 0d, 1d, 2d, 3d and 4d variants because
! of a "feature" of Fortran90. The 1d-versions should be sufficient because of
! so-called "assumed-size" declaration of the dummy argument (dimension(*)).
! This indeed works correctly when the 1d-subroutine is made public, even when
! a 2d, 3d or 4d actual argument is used. However, it does not go together with
! a generic interface. When generic routine distribute_data is called with a 2d
! actual argument, the compiler complains that no matching specific subroutine
! can be found. Therefore additional routines are defined for 2d, 3d and 4d
! arrays that are wrappers around the 1d-versions of the subroutines.
!
! TODO: explain reasons for introducing _idata, _rdata, _ddata variants

interface distribute_data
   module procedure distribute_0d_ldata_on_cardset
   module procedure distribute_1d_ldata_on_cardset

   module procedure distribute_0d_idata_on_cardset
   module procedure distribute_1d_idata_on_cardset
   module procedure distribute_2d_idata_on_cardset
   module procedure distribute_3d_idata_on_cardset
   module procedure distribute_4d_idata_on_cardset
!
   module procedure distribute_0d_rdata_on_cardset
   module procedure distribute_1d_rdata_on_cardset
   module procedure distribute_2d_rdata_on_cardset
   module procedure distribute_3d_rdata_on_cardset
   module procedure distribute_4d_rdata_on_cardset
!
   module procedure distribute_0d_ddata_on_cardset
   module procedure distribute_1d_ddata_on_cardset
   module procedure distribute_2d_ddata_on_cardset
   module procedure distribute_3d_ddata_on_cardset
   module procedure distribute_4d_ddata_on_cardset
!
   module procedure distribute_1d_cdata_on_cardset
!
   module procedure distribute_1d_idata_on_namedset
   module procedure distribute_2d_idata_on_namedset
   module procedure distribute_3d_idata_on_namedset
   module procedure distribute_4d_idata_on_namedset
!
   module procedure distribute_1d_rdata_on_namedset
   module procedure distribute_2d_rdata_on_namedset
   module procedure distribute_3d_rdata_on_namedset
   module procedure distribute_4d_rdata_on_namedset
!
   module procedure distribute_1d_ddata_on_namedset
   module procedure distribute_2d_ddata_on_namedset
   module procedure distribute_3d_ddata_on_namedset
   module procedure distribute_4d_ddata_on_namedset
!
   module procedure distribute_1d_idata_on_repl_namedset
   module procedure distribute_2d_idata_on_repl_namedset
   module procedure distribute_3d_idata_on_repl_namedset
   module procedure distribute_4d_idata_on_repl_namedset
!
   module procedure distribute_1d_rdata_on_repl_namedset
   module procedure distribute_2d_rdata_on_repl_namedset
   module procedure distribute_3d_rdata_on_repl_namedset
   module procedure distribute_4d_rdata_on_repl_namedset
!
   module procedure distribute_1d_ddata_on_repl_namedset
   module procedure distribute_2d_ddata_on_repl_namedset
   module procedure distribute_3d_ddata_on_repl_namedset
   module procedure distribute_4d_ddata_on_repl_namedset
end interface distribute_data

interface collect_data
   module procedure collect_1d_idata_on_namedset
   module procedure collect_2d_idata_on_namedset
   module procedure collect_3d_idata_on_namedset
   module procedure collect_4d_idata_on_namedset
!
   module procedure collect_1d_rdata_on_namedset
   module procedure collect_2d_rdata_on_namedset
   module procedure collect_3d_rdata_on_namedset
   module procedure collect_4d_rdata_on_namedset
!
   module procedure collect_1d_ddata_on_namedset
   module procedure collect_2d_ddata_on_namedset
   module procedure collect_3d_ddata_on_namedset
   module procedure collect_4d_ddata_on_namedset
!
   module procedure collect_1d_idata_on_repl_namedset
   module procedure collect_2d_idata_on_repl_namedset
   module procedure collect_3d_idata_on_repl_namedset
   module procedure collect_4d_idata_on_repl_namedset
!
   module procedure collect_1d_rdata_on_repl_namedset
   module procedure collect_2d_rdata_on_repl_namedset
   module procedure collect_3d_rdata_on_repl_namedset
   module procedure collect_4d_rdata_on_repl_namedset
!
   module procedure collect_1d_ddata_on_repl_namedset
   module procedure collect_2d_ddata_on_repl_namedset
   module procedure collect_3d_ddata_on_repl_namedset
   module procedure collect_4d_ddata_on_repl_namedset
end interface collect_data

interface update_data
   module procedure update_1d_idata_on_namedset
   module procedure update_2d_idata_on_namedset
   module procedure update_3d_idata_on_namedset
   module procedure update_4d_idata_on_namedset
!
   module procedure update_1d_rdata_on_namedset
   module procedure update_2d_rdata_on_namedset
   module procedure update_3d_rdata_on_namedset
   module procedure update_4d_rdata_on_namedset
!
   module procedure update_1d_ddata_on_namedset
   module procedure update_2d_ddata_on_namedset
   module procedure update_3d_ddata_on_namedset
   module procedure update_4d_ddata_on_namedset
!
   module procedure update_1d_idata_on_repl_namedset
   module procedure update_2d_idata_on_repl_namedset
   module procedure update_3d_idata_on_repl_namedset
   module procedure update_4d_idata_on_repl_namedset
!
   module procedure update_1d_rdata_on_repl_namedset
   module procedure update_2d_rdata_on_repl_namedset
   module procedure update_3d_rdata_on_repl_namedset
   module procedure update_4d_rdata_on_repl_namedset
!
   module procedure update_1d_ddata_on_repl_namedset
   module procedure update_2d_ddata_on_repl_namedset
   module procedure update_3d_ddata_on_repl_namedset
   module procedure update_4d_ddata_on_repl_namedset
end interface update_data

interface accumulate_data
   module procedure accumulate_1d_idata_on_namedset
   module procedure accumulate_2d_idata_on_namedset
   module procedure accumulate_3d_idata_on_namedset
   module procedure accumulate_4d_idata_on_namedset
!
   module procedure accumulate_1d_rdata_on_namedset
   module procedure accumulate_2d_rdata_on_namedset
   module procedure accumulate_3d_rdata_on_namedset
   module procedure accumulate_4d_rdata_on_namedset
!
   module procedure accumulate_1d_ddata_on_namedset
   module procedure accumulate_2d_ddata_on_namedset
   module procedure accumulate_3d_ddata_on_namedset
   module procedure accumulate_4d_ddata_on_namedset
!
   module procedure accumulate_1d_idata_on_repl_namedset
   module procedure accumulate_2d_idata_on_repl_namedset
   module procedure accumulate_3d_idata_on_repl_namedset
   module procedure accumulate_4d_idata_on_repl_namedset
!
   module procedure accumulate_1d_rdata_on_repl_namedset
   module procedure accumulate_2d_rdata_on_repl_namedset
   module procedure accumulate_3d_rdata_on_repl_namedset
   module procedure accumulate_4d_rdata_on_repl_namedset
!
   module procedure accumulate_1d_ddata_on_repl_namedset
   module procedure accumulate_2d_ddata_on_repl_namedset
   module procedure accumulate_3d_ddata_on_repl_namedset
   module procedure accumulate_4d_ddata_on_repl_namedset
end interface accumulate_data

! The actual communication for the distribute, collect, update and accumulate
! operations is mostly implemented through the sendrecv-operation. This
! operation takes an interface, sends data to all neighbours in the interface,
! and then receives data from all neighbours in the interface.
!
! The communication is done with non-blocking send and blocking receive in
! order to prevent dead-locks in the send-first, receive-later scheme. In the
! sendrecv-operation messages are composed in work-arrays (buffers) because of
! the usage of subsets of index-sets. Therefore it is ok for us to leave the
! work-array untouched for a while after sending data, such that no buffering
! has to be done in MPI and un-buffered sends (mpi_isend) can be used.
!
! In one-direction communications (distribute: master->workers, update: certain
! stencils) we allow the sender to proceed after it has sent its data and do not
! require him to wait until the receiver is ready to accept the data. Therefore
! the send-buffer must be kept after the sendrecv-operation finishes on the
! master process. On the other hand, the sender may not proceed too far ahead
! of the receiver, this might exhaust the capacity of the memory or
! communication network of the computer used. This is solved for now by waiting
! in the next sendrecv-call for termination of the previous one. In the future
! this may be extended to a situation in which there may be up to, say, 10
! unfinished sendrecv-calls.
! Note: there is no feedback from workers to the master when using distribute
! using the cardset variants. Maybe new mpi_bcast operations should wait for
! termination of previous ones too.

! definition of one unfinished sendrecv-operation:
type                    :: t_sndrcv_op
   integer, dimension(:), pointer  :: ireq   !! handle to the MPI communication
                                             !! requests per neighbour
   integer, dimension(:), pointer  :: sndbuf !! pointer to send-buffer
                                             !! used in the communication
end type t_sndrcv_op

! module-variable with data for the previous sendrecv-operation:
type(t_sndrcv_op)       :: prev_sndrcv_op

! counter for sendrecv-operations per process group (currently: MPI_COMM_ALL),
! used for determining message tags.
! in COCLIB 1000 tags are used per group and recycled after 1000 communications;
! an offset can be used to avoid conflicts with other types of operations.
! in COCLIB a separate counter is used for the total number of updates, a.o.
! for output- and debugging purposes.
integer                 :: cnt_sndrcv = 0
integer                 :: num_calls(NUMCOMM)
integer                 :: amount_data(2,NUMCOMM)

contains

!-- SUBROUTINE sndrcv_stopmod ------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Terminate usage of CouPLib w.r.t. communication operations
!!>
!!  On entry:
!!  -
!!
!!  On return:
!!  -           the usage statistics have been printed
!!<
!-----------------------------------------------------------------------------
subroutine sndrcv_stopmod()

!-- HEADER VARIABLES/ARGUMENTS
implicit none

!-- LOCAL VARIABLES
integer comm_op
!-----------------------------------------------------------------------------
   do comm_op = 1, NUMCOMM
      if (max(amount_data(ISEND,comm_op), &
              amount_data(IRECV,comm_op)).gt.1e6) then

         write(LOUT,'(a14,a,i9,a,2(f10.1,a))') &
            trim(namcomm(comm_op)),': #calls=',num_calls(comm_op), ',', &
            real(amount_data(ISEND,comm_op)*BYTES_P_INT)/1e6,' MB sent,   ', &
            real(amount_data(IRECV,comm_op)*BYTES_P_INT)/1e6,' MB rcvd'

      elseif (num_calls(comm_op).gt.0) then

         write(LOUT,'(a14,a,i9,a,2(i10,a))') &
            trim(namcomm(comm_op)),': #calls=',num_calls(comm_op), ',', &
            amount_data(ISEND,comm_op)*BYTES_P_INT,' bytes sent,', &
            amount_data(IRECV,comm_op)*BYTES_P_INT,' bytes rcvd'
      endif
   enddo
end subroutine sndrcv_stopmod


! In this file we maintain the versions of all variants of distribute,
! collect, update and accumulate operations for 1D integer arrays only.
! The versions for other array-shapes and variable types are included here:

include 'sndrcv_wrappers.f90'

!-- SUBROUTINE distribute_1d_idata_on_cardset -----------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Distribute integer data defined by the cardinality of the array from one
!!  master-process (mypart=1) to all worker-processes (mypart>1).
!!>
!!  On entry:
!!  mypart      part-number of each computing process; 1==master, >1==worker
!!  iarray      on master: array containing integer data to be distributed
!!  nelem       number of elements of iarray
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      on workers: all elements of iarray have been filled by copying
!!              the corresponding values of the master process.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine distribute_1d_idata_on_cardset(mypart, iarray, nelem, info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)                       :: mypart
integer, intent(in)                       :: nelem
integer, dimension(*), intent(inout)      :: iarray(nelem)
integer, intent(out)                      :: info
integer, intent(in), optional             :: idebug

!-- LOCAL VARIABLES
! rank of the process in MPI-communicator that must send data
integer :: iroot
! internal level of debug-output, 0=none
integer :: my_idebug
! number of elements received in mpi-communication
integer :: nrecvd
! status-code of mpi-operation
integer :: ierror
!-----------------------------------------------------------------------------

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug

!  Return directly in sequential runs

   if (iscple.eq.0) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: non-coupled run, nothing to be done'
      return
   endif

!  If synchronization time is to be measured separately:
!     wait for other processes, measure the time needed

   if (measure_idletime) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: entering barrier',&
         ' for measuring idle time'
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,IDSTRBC))
      call mpi_barrier(MPI_COMM_ALL, ierror)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,IDSTRBC))
   endif
   if (use_timers) call timer_start(itimer_commop(ICOMMTM,IDSTRBC))

!  Broadcast the data themselves from mypart=1 (rank 0) to others

   iroot = 0
   num_calls(IDSTRBC) = num_calls(IDSTRBC) + 1
   if (my_idebug.ge.1) write(LOUT,'(a,i3,a,i6,a,i7)') 'mypart=',mypart, &
       ': entering ',num_calls(IDSTRBC), '-th mpi_bcast, integer data, nelem=',nelem
   call mpi_bcast(iarray, nelem, MPI_INTEGER, iroot, MPI_COMM_ALL, ierror)
   if (my_idebug.ge.2) write(LOUT,'(a,i3,a,2i7)') 'mypart=',mypart, &
       ': distribute_cardset: ierror=',ierror,nelem
   info = ierror

!  Update statistics

   if (mypart.eq.1) then
      amount_data(ISEND,IDSTRBC) = amount_data(ISEND,IDSTRBC) + nelem*(numprc-1)
   else
      amount_data(IRECV,IDSTRBC) = amount_data(IRECV,IDSTRBC) + nelem
   endif

!  Stop timer

   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,IDSTRBC))

end subroutine distribute_1d_idata_on_cardset


!-- SUBROUTINE distribute_1d_idata_on_namedset -------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Distribute integer data defined on a named index-set from one master-process
!!  (mypart=1) to all worker-processes (mypart>1).
!!>
!!  On entry:
!!  mypart      part-number of each computing process; 1==master, >1==worker
!!  iarray      on master: array containing integer data to be distributed
!!  namixs      name of index-set describing the structure of iarray
!!  namitf      name of communication interface to be used, describing which
!!              elements per worker need to be filled in.
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      on workers: the indices that are contained in the communication
!!              interface used have been filled in by copying the corresponding
!!              values of the master process.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine distribute_1d_idata_on_namedset(mypart, iarray, namixs, namitf, &
                                           info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)                       :: mypart
integer,      dimension(*), intent(inout) :: iarray
character(len=*), intent(in)              :: namixs
character(len=*), intent(in)              :: namitf
integer, intent(out)                      :: info
integer, intent(in), optional             :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)

end subroutine distribute_1d_idata_on_namedset


!-- SUBROUTINE distribute_1d_idata_on_repl_namedset --------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Distribute integer data defined on a temporary index-set, created by
!!  replication of a named index-set, from one master-process (mypart=1) to
!!  all worker-processes (mypart>1).
!!>
!!  On entry:
!!  mypart      part-number of each computing process; 1==master, >1==worker
!!  iarray      on master: array containing integer data to be distributed
!!  presiz      pre-multiplication factor for index-set, leading array-dimension
!!  namixs      name of index-set describing the structure/distribution of
!!              iarray; the full index-set used is 'presiz'*'namixs'*'possiz'
!!  possiz      post-multiplication factor for index-set, trailing array-
!!              dimension
!!  namitf      name of communication interface to be used, describing which
!!              elements per worker need to be filled in.
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      on workers: the indices that are contained in the communication
!!              interface used have been filled in by copying the corresponding
!!              values of the master process.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine distribute_1d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                  namixs, possiz, namitf, info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,               intent(in)    :: mypart
integer, dimension(*), intent(inout) :: iarray
integer,               intent(in)    :: presiz
character(len=*),      intent(in)    :: namixs
integer,               intent(in)    :: possiz
character(len=*),      intent(in)    :: namitf
integer, intent(out)                 :: info
integer, intent(in), optional        :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)

end subroutine distribute_1d_idata_on_repl_namedset


!-- SUBROUTINE collect_1d_idata_on_namedset ----------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Collect integer data defined on a named index-set from all worker-processes
!!  (mypart>1) on the master-process (mypart=1)
!!>
!!  On entry:
!!  mypart      part-number of each computing process; 1==master, >1==worker
!!  iarray      on workers: array containing integer data to be collected
!!  namixs      name of index-set describing the structure of iarray
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      on master: all elements of the array have been filled in by
!!              copying the corresponding values of the worker processes
!!              (as long as the collect interface is complete).
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine collect_1d_idata_on_namedset(mypart, iarray, namixs, info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)                       :: mypart
integer,      dimension(*), intent(inout) :: iarray
character(len=*), intent(in)              :: namixs
integer, intent(out)                      :: info
integer, intent(in), optional             :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)

end subroutine collect_1d_idata_on_namedset


!-- SUBROUTINE collect_1d_idata_on_repl_namedset -----------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Collect integer data defined on a temporary index-set, created by
!!  replication of a named index-set, from all worker-processes (mypart>1)
!!  on the master-process (mypart=1)
!!>
!!  On entry:
!!  mypart      part-number of each computing process; 1==master, >1==worker
!!  iarray      on workers: array containing integer data to be collected
!!  presiz      pre-multiplication factor for index-set, leading array-dimension
!!  namixs      name of index-set describing the structure/distribution of
!!              iarray; the full index-set used is 'presiz'*'namixs'*'possiz'
!!  possiz      post-multiplication factor for index-set, trailing array-
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      on master: all elements of the array have been filled in by
!!              copying the corresponding values of the worker processes
!!              (as long as the collect interface is complete).
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine collect_1d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                             namixs, possiz, info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,               intent(in)    :: mypart
integer, dimension(*), intent(inout) :: iarray
integer,               intent(in)    :: presiz
character(len=*),      intent(in)    :: namixs
integer,               intent(in)    :: possiz
integer, intent(out)                 :: info
integer, intent(in), optional        :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                info=info, idebug=my_idebug)

end subroutine collect_1d_idata_on_repl_namedset


!-- SUBROUTINE update_1d_idata_on_namedset -----------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Exchange integer data defined on an index-set between different processes
!!  using a communication interface.
!!>
!!  On entry:
!!  iarray      array containing integer data for own indices of index-set,
!!              available for other processes
!!  namixs      name of index-set describing the structure of iarray
!!  namitf      name of communication interface to be used, describing which
!!              array-elements are to be sent to/received from neighbouring
!!              processes
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      the indices that are contained in the receive-areas of the
!!              communication interface have been filled in by copying the
!!              corresponding values of the neighbouring processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine update_1d_idata_on_namedset(iarray, namixs, namitf, info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,      dimension(*), intent(inout) :: iarray
character(len=*), intent(in)              :: namixs
character(len=*), intent(in)              :: namitf
integer, intent(out)                      :: info
integer, intent(in), optional             :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)

end subroutine update_1d_idata_on_namedset


!-- SUBROUTINE update_1d_idata_on_repl_namedset ------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Exchange integer data defined on a temporary index-set, created by
!!  replication of a named index-set, between different processes
!!  using a communication interface.
!!>
!!  On entry:
!!  iarray      array containing integer data for own indices of index-set,
!!              available for other processes
!!  presiz      pre-multiplication factor for index-set, leading array-dimension
!!  namixs      name of index-set describing the structure/distribution of
!!              iarray; the full index-set used is 'presiz'*'namixs'*'possiz'
!!  possiz      post-multiplication factor for index-set, trailing array-
!!              dimension
!!  namitf      name of communication interface to be used, describing which
!!              array-elements are to be sent to/received from neighbouring
!!              processes
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      the indices that are contained in the receive-areas of the
!!              communication interface have been filled in by copying the
!!              corresponding values of the neighbouring processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine update_1d_idata_on_repl_namedset(iarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,          dimension(*), intent(inout) :: iarray
integer,                        intent(in)    :: presiz
character(len=*),               intent(in)    :: namixs
integer,                        intent(in)    :: possiz
character(len=*),               intent(in)    :: namitf
integer,                        intent(out)   :: info
integer, intent(in), optional                 :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)

end subroutine update_1d_idata_on_repl_namedset


!-- SUBROUTINE accumulate_1d_idata_on_namedset -------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Exchange integer data defined on an index-set between different processes
!!  using a communication interface, thereby adding up values received for
!!  the same array-element.
!!>
!!  On entry:
!!  iarray      array containing integer data for all indices defined in
!!              the send-areas of the interface "namitf" used, available for
!!              other processes
!!  namixs      name of index-set describing the structure of iarray
!!  namitf      name of communication interface to be used, describing which
!!              array-elements are to be sent to/received from neighbouring
!!              processes
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      the indices that are contained in the receive-areas of the
!!              communication interface have been modified by adding the
!!              corresponding values of the neighbouring processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine accumulate_1d_idata_on_namedset(iarray, namixs, namitf, info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,      dimension(*), intent(inout) :: iarray
character(len=*), intent(in)              :: namixs
character(len=*), intent(in)              :: namitf
integer, intent(out)                      :: info
integer, intent(in), optional             :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)

end subroutine accumulate_1d_idata_on_namedset


!-- SUBROUTINE accumulate_1d_idata_on_repl_namedset --------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Exchange integer data defined on a temporary index-set, created by
!!  replication of a named index-set, between different processes using a
!!  communication interface, thereby adding up values received for the same
!!  array-element.
!!>
!!  On entry:
!!  iarray      array containing integer data for all indices defined in
!!              the send-areas of the interface "namitf" used, available for
!!              other processes
!!  presiz      pre-multiplication factor for index-set, leading array-dimension
!!  namixs      name of index-set describing the structure/distribution of
!!              iarray; the full index-set used is 'presiz'*'namixs'*'possiz'
!!  possiz      post-multiplication factor for index-set, trailing array-
!!              dimension
!!  namixs      name of index-set describing the structure of iarray
!!  namitf      name of communication interface to be used, describing which
!!              array-elements are to be sent to/received from neighbouring
!!              processes
!!  idebug      requested level of debug-output, 0=none
!!
!!  On return:
!!  iarray      the indices that are contained in the receive-areas of the
!!              communication interface have been modified by adding the
!!              corresponding values of the neighbouring processes.
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine accumulate_1d_idata_on_repl_namedset(iarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, dimension(*), intent(inout) :: iarray
integer,               intent(in)    :: presiz
character(len=*),      intent(in)    :: namixs
integer,               intent(in)    :: possiz
character(len=*),      intent(in)    :: namitf
integer,               intent(out)   :: info
integer, intent(in), optional        :: idebug

!-- LOCAL VARIABLES
! internal level of debug-output, 0=none
integer :: my_idebug
!-----------------------------------------------------------------------------

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug

   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, idebug=my_idebug)

end subroutine accumulate_1d_idata_on_repl_namedset


!-- SUBROUTINE sendrecv_data -------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Exchange data defined on an index-set between different processes using
!!  a communication interface.
!!>
!!  On entry:
!!  iarray      optional: array containing integer data for own indices of
!!              index-set, available for other processes
!!  rarray      optional: array containing real data for own indices of
!!              index-set, available for other processes
!!  darray      optional: array containing double precision data for own
!!              indices of index-set, available for other processes
!!  namixs      name of index-set describing the structure of rarray
!!  namitf      name of communication interface to be used, describing which
!!              array-elements are to be sent to/received from neighbouring
!!              processes
!!  ladd        when true the received values must be added to the original
!!              contents of [ird]array, else the values are replaced (default)
!!  comm_op     type of communication operation being performed, one of
!!              IDSTRBX, ICOLLCT, IUPDATE or IACCUM.
!!  idebug      optional: requested level of debug-output
!!
!!  On return:
!!  iarray      the indices that are contained in the receive-areas of the
!!              communication interface have been filled in by copying the
!!              corresponding values of the neighbouring processes.
!!  rarray      see iarray
!!  darray      see iarray
!!  info        status-code of the operation
!!<
!-----------------------------------------------------------------------------
subroutine sendrecv_data(iarray, rarray, darray, presiz, namixs, possiz, &
                         namitf, ladd, comm_op, info, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,      dimension(*), intent(inout), optional :: iarray
real(kind=4), dimension(*), intent(inout), optional :: rarray
real(kind=8), dimension(*), intent(inout), optional :: darray
integer,                    intent(in),    optional :: presiz
character(len=*),           intent(in)              :: namixs
integer,                    intent(in),    optional :: possiz
character(len=*),           intent(in)              :: namitf
logical,                    intent(in),    optional :: ladd
integer,                    intent(out)             :: info
integer,                    intent(in)              :: comm_op
integer,                    intent(in),    optional :: idebug

!-- LOCAL VARIABLES
! number of subroutine arguments
integer                                    :: nargs
! internal value for flag ladd
logical                                    :: lladd
! handle of index-set, interface, factors of repl.itf, number of neighbours
integer                                    :: iset, nelem, iifc
integer                                    :: nfac, ifac, ifcdst, itfrpl
integer, dimension(:), pointer             :: pfctrs
integer                                    :: nngb, ingb, iprc
! list of send/receive-areas for interface, single send/receive area
type(t_sndrcv_area), dimension(:), pointer :: nghtbl
type(t_sndrcv_area),               pointer :: sndrcv
! number of items to send/receive, loop counter
integer                                    :: nsnd_tot, nsend, nrecv, nexpct
! internal values of presiz, possiz
integer                                    :: ipresiz, ipossiz, is
! number of messages to receive
integer                                    :: nummsg
! help flag
logical                                    :: found
! work-arrays to compose message for all neighbours/receive msg from one ngb
integer, dimension(:), pointer             :: sndbuf, rcvbuf
! number of ints needed to store one element of input-array i/r/darray
integer                                    :: elem_size
! offset within buffer to start for current neighbour, offset within message
integer                                    :: iof, nitm
! help variables for MPI-operations
integer                                    :: idest, isrc, msgtag, ierror
integer                                    :: mpstat(MPI_STATUS_SIZE)
integer, dimension(:), pointer             :: ireq
! dummy array for calling reshape_data
integer                                    :: inrlp
integer, dimension(:)                      :: idum(1)
! level of debug-output, 0=none
integer                                    :: my_idebug
!-----------------------------------------------------------------------------

!  Set requested level of debug-output
!    idebug= 0: no output
!    idebug= 2: information on phases of send/recv
!    idebug= 3: information on which messages are sent
!    idebug= 5: detailed information on message packing/unpacking
!    idebug=10: dump of data received

   my_idebug = 0
   if (present(idebug)) my_idebug = idebug

!  Return directly in sequential runs

   if (iscple.eq.0) then
      if (my_idebug.ge.2) write(LOUT,*) 'sndrcv: non-coupled run, nothing to be done'
      return
   endif

!  Get optional pre- and post replication factors

   ipresiz = 1
   ipossiz = 1
   if (present(presiz)) ipresiz = presiz
   if (present(possiz)) ipossiz = possiz

   if (my_idebug.ge.3) write(LOUT,*)
   if (my_idebug.ge.1) then
      if (present(presiz) .or. present(possiz)) then
         write(LOUT,'(a,i5,5a,2i5)') ' starting', cnt_sndrcv+1, &
            '-th sendrecv, ixs="',trim(namixs),'", itf="', &
            trim(namitf),'", pre/pos=', ipresiz, ipossiz
      else
         write(LOUT,'(a,i5,5a)') ' starting', cnt_sndrcv+1, &
            '-th sendrecv, ixs="',trim(namixs),'", itf="', trim(namitf),'"'
      endif
   endif

!  Check correctness of call: only one of iarray, rarray and darray can and must
!  be specified

   nargs = 0
   if (present(iarray)) nargs = nargs + 1
   if (present(rarray)) nargs = nargs + 1
   if (present(darray)) nargs = nargs + 1
   if (nargs.ne.1) then
      write(LOUT,*) 'sendrecv_data: Error: precisely one of the arguments ', &
         'iarray, rarray and darray must be specified; actual number of ', &
         'arguments is',nargs
      stop
   endif

!  Get optional argument lladd; overwrite or add to contents of [ird]array

   lladd = .false.
   if (present(ladd)) lladd = ladd

   if (my_idebug.ge.2) then
      if (present(iarray)) write(LOUT,*) 'sendrecv: integer data, add=',lladd
      if (present(rarray)) write(LOUT,*) 'sendrecv: real data, add=',lladd
      if (present(darray)) write(LOUT,*) 'sendrecv: double precision data, ',&
         'add=', lladd
   endif

!  If synchronization time is to be measured separately:
!     wait for other processes, measure the time needed

   if (measure_idletime) then
      if (idebug.ge.2) write(LOUT,*) 'sendrecv: entering barrier for ', &
         'measuring idle time'
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,comm_op))
      call mpi_barrier(MPI_COMM_ALL, ierror)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,comm_op))
   endif

!  Start timing of communication phase of sndrcv

   if (use_timers) call timer_start(itimer_commop(ICOMMTM,comm_op))

!  If sendrecv has been called before: wait for completion of previous
!     sendrecv-operation

   !write(LOUT,*) 'cnt_sndrcv=',cnt_sndrcv
   if (cnt_sndrcv.gt.0) then
      if (my_idebug.ge.7) write(LOUT,*) 'sendrecv: wait for completion of ',&
         'previous sendrecv-operation...'

!     wait for completion of all non-blocking send requests of previous
!     sendrecv-operation

      nngb = size(prev_sndrcv_op%ireq)
      do ingb = 1, nngb
         call mpi_wait(prev_sndrcv_op%ireq(ingb), mpstat, ierror)
         if (ierror.ne.0) write(LOUT,*) 'sendrecv: Error in wait (',ierror,&
            ') for ngb=',ingb
      enddo

!     de-allocate previous send-buffer, requests

      deallocate(prev_sndrcv_op%sndbuf)
      deallocate(prev_sndrcv_op%ireq)
   endif

!  Get handle to index-set, get distributed factor, size of distributed factor

   iset = ixset_hndl(namixs, IFATAL)
   call ixset_getprops(iset, ifcdst=ifcdst)
   call ixset_getprops(ifcdst, nelem=nelem)
   if (my_idebug.ge.6) write(LOUT,*) 'iset=',iset,', ifcdst=',ifcdst, &
      ', nelem=',nelem

!  Get handle to interface, get properties w.r.t. replication, get neighbour-
!  table of the interface that is replicated

   iifc = intfc_hndl(namitf, iset, IFATAL)
   call intfc_getprops(iifc, nsnd_tot=nsnd_tot, &
                       nfac=nfac, pfctrs=pfctrs, itfrpl=itfrpl)
   if (my_idebug.ge.6) write(LOUT,*) 'iifc=',iifc,', nfac=',nfac, &
      ', itfrpl=',itfrpl
   call intfc_getprops(itfrpl, nngb=nngb, nghtbl=nghtbl)
   if (my_idebug.ge.6) write(LOUT,*) 'itfrpl=',itfrpl,': nngb=',nngb

!  Determine the leading and trailing size of replicated factors in
!  the decomposition 'presiz' * 'product-set' * 'possiz'
!   --> the distributed dimension always becomes the second/middle one
!   --> multiple replicated dimensions before or after the distributed dimension
!       are collapsed.
!   --> the resulting index-set has structure 'ipresiz' * 'distr.set' *'ipossiz'

   found = .false.
   do ifac = 1, nfac
      if (pfctrs(ifac).le.0) then
         if (found) then
            ipossiz = ipossiz * -pfctrs(ifac)
         else
            ipresiz = ipresiz * -pfctrs(ifac)
         endif
      else
         found = .true.
      endif
   enddo

!-----------------------------------------------------------------------------
!  SENDING PHASE
!-----------------------------------------------------------------------------
!  Increment counter for number of sendrecv-operations

   cnt_sndrcv = cnt_sndrcv + 1
   num_calls(comm_op) = num_calls(comm_op) + 1
   msgtag  = mod(cnt_sndrcv,1000)
   if (my_idebug.ge.3) write(LOUT,'(2(a,i6))') ' sendrecv: starting',cnt_sndrcv, &
        '-th exchange, msgtag=',msgtag

!  Allocate buffer for composing messages to all neighbours,
!  allocate list of MPI communication requests,
!  store pointers in prev_sndrcv_op

   if (present(rarray)) then
      elem_size = SIZE_REAL
   elseif (present(darray)) then
      elem_size = SIZE_DBLE
   else
      elem_size = 1
   endif

   allocate(sndbuf(max(1, 2*nngb + ipresiz*nsnd_tot*ipossiz*elem_size)))
   iof = 0     ! pointer to last array-element for previous neighbour

   allocate(ireq(max(1,nngb)))
   ireq = MPI_REQUEST_NULL

   prev_sndrcv_op%sndbuf => sndbuf
   prev_sndrcv_op%ireq => ireq

!  Loop over all neighbours, send data

   if (my_idebug.ge.2) write(LOUT,'(a,i3,a)') ' sendrecv: nngb=',nngb, &
      ', start sending phase'

   do ingb = 1, nngb

!     prepare message: items sndrcv%isnd of iarray, rarray or darray

      !write(LOUT,*) 'send_data: starting ingb=',ingb
      sndrcv => nghtbl(ingb)
      !write(LOUT,*) 'send_data: ingb=',ingb,': iprc=',sndrcv%iprc
      if (associated(sndrcv%isnd)) then
         nsend = size(sndrcv%isnd)
      else
         nsend = 0
      endif
      if (my_idebug.ge.8) write(LOUT,*) 'send_data: ingb=',ingb,': nsend=',nsend
      if (nsend.le.0) then
         if (my_idebug.ge.2) write(LOUT,'(a,i3,a,i3,a)') ' send_data: ngb',ingb, &
                ' is process', sndrcv%iprc, ': nothing to send'
      else

!        compose message: store process-id, number of values to send

         sndbuf(iof+1) = myprc
         sndbuf(iof+2) = ipresiz*nsend*ipossiz
         nitm = 2

!        compose message: store values themselves

         inrlp = 2
         if (present(iarray)) then
            call reshape_idata(inrlp, iarray, ipresiz, nelem, ipossiz, 2, &
                         sndrcv%isnd, nsend, sndbuf(iof+nitm+1), &
                         ipresiz, nsend, ipossiz, 2, idum, 1, my_idebug)
         elseif (present(rarray)) then
            call reshape_rdata(inrlp, rarray, ipresiz, nelem, ipossiz, 2, &
                         sndrcv%isnd, nsend, sndbuf(iof+nitm+1), &
                         ipresiz, nsend, ipossiz, 2, idum, 1, my_idebug)
         else
            call reshape_ddata(inrlp, darray, ipresiz, nelem, ipossiz, 2, &
                         sndrcv%isnd, nsend, sndbuf(iof+nitm+1), &
                         ipresiz, nsend, ipossiz, 2, idum, 1, my_idebug)
         endif
         nitm = nitm + ipresiz*nsend*ipossiz * elem_size

!        send message

         idest = sndrcv%iprc - 1
         if (my_idebug.ge.5) write(LOUT,*) 'sendrecv: sending',nitm, &
            ' items to dest=',idest,', tag=',msgtag
         call mpi_isend(sndbuf(iof+1), nitm, MPI_INTEGER, idest, msgtag, &
                  MPI_COMM_ALL, ireq(ingb), ierror)
         if (ierror.ne.0) write(LOUT,*) 'sendrecv: Error in isend=',ierror

!        update statistics

         amount_data(ISEND,comm_op) = amount_data(ISEND,comm_op) + nitm

!        set pointer in sndbuf for next neighbour

         iof = iof + nitm

      endif
   enddo ! ingb

!-----------------------------------------------------------------------------
!  RECEIVING PHASE
!-----------------------------------------------------------------------------

!  Determine number of messages to be received

   nummsg = 0
   do ingb = 1, nngb
      if (associated(nghtbl(ingb)%ircv)) then
         nrecv = size(nghtbl(ingb)%ircv)
         if (nrecv.gt.0) nummsg = nummsg + 1
      endif
   enddo

!  WHILE not all expected messages received:
!     === handle incoming messages ===

   if (my_idebug.ge.2) write(LOUT,'(2a,i3,a)') ' sendrecv: start receiving ', &
      'phase, expecting nummsg=',nummsg, ' messages'

   do while (nummsg.gt.0)

!     receive message: probe for a message, allocate rcvbuf, receive message

      isrc = MPI_ANY_SOURCE
      call mpi_probe(isrc, msgtag, MPI_COMM_ALL, mpstat, ierror)
      if (ierror.ne.0) write(LOUT,*) 'sendrecv: Error in probe=',ierror

      call mpi_get_count(mpstat, MPI_INTEGER, nitm, ierror)
      if (my_idebug.ge.5) write(LOUT,'(2(a,i6))') ' recv_data: get_count: nitm=',&
          nitm, ', ierror=',ierror
      allocate(rcvbuf(nitm))

      call mpi_recv(rcvbuf, nitm, MPI_INTEGER, isrc, msgtag, MPI_COMM_ALL, &
               mpstat, ierror)
      if (ierror.ne.0) write(LOUT,*) 'sendrecv: Error in recv=',ierror
      if (my_idebug.ge.10) write(LOUT,*) 'recv_data: rcvbuf=',rcvbuf

!     unpack the sender, find in list of neighbours

      iprc = rcvbuf(1)
      if (my_idebug.ge.5) write(LOUT,*) 'recv_data: obtained iprc=',iprc

      ingb = 0
      found = .false.
      do while (.not.found .and. ingb.lt.nngb)
         ingb = ingb + 1
         if (iprc.eq.nghtbl(ingb)%iprc) found = .true.
      enddo
      if (.not.found) then
         write(LOUT,*) 'sendrecv: Error: obtained message from process',iprc,&
                ', which does not occur in interface "',trim(namitf),'"'
         stop
      endif

      if (my_idebug.ge.3) write(LOUT,'(2(a,i3),a)') ' recv_data: obtained iprc=',&
         iprc,', ingb=',ingb
      sndrcv => nghtbl(ingb)

!     unpack the number of items received, check

      if (associated(sndrcv%ircv)) then
         nrecv = size(sndrcv%ircv)
         !write(LOUT,*) 'ircv=',sndrcv%ircv,': nrecv=',nrecv
      else
         nrecv = 0
      endif
      nexpct = ipresiz*nrecv*ipossiz
      if (rcvbuf(2).ne.nexpct) then
         write(LOUT,'(a,i5,a,i3,a,i5,a)') 'recv_data: Error: expected',nexpct,&
                ' items in message of process',iprc,', obtained',&
                rcvbuf(2),' items instead.'
         stop
      endif
      if (my_idebug.ge.2) write(LOUT,'(a,i6,a,i3)') ' recv_data: received', &
         nexpct,' items of process',iprc

!     unpack the data-items themselves

      inrlp = 3
      if (lladd) inrlp = 4
      if (present(iarray)) then
         call reshape_idata(inrlp, &
                      rcvbuf(3), ipresiz, nrecv, ipossiz, 2, idum, 1, &
                      iarray, ipresiz, nelem, ipossiz, 2, sndrcv%ircv, nrecv, &
                      my_idebug)
      elseif (present(rarray)) then
         call reshape_rdata(inrlp, &
                      rcvbuf(3), ipresiz, nrecv, ipossiz, 2, idum, 1, &
                      rarray, ipresiz, nelem, ipossiz, 2, sndrcv%ircv, nrecv, &
                      my_idebug)
      else
         call reshape_ddata(inrlp, &
                      rcvbuf(3), ipresiz, nrecv, ipossiz, 2, idum, 1, &
                      darray, ipresiz, nelem, ipossiz, 2, sndrcv%ircv, nrecv, &
                      my_idebug)
      endif
      if (my_idebug.ge.5) write(LOUT,'(2a,i3)') ' recv_data: done reshaping ',&
         'data of process',iprc

      deallocate(rcvbuf)
      if (my_idebug.ge.5) write(LOUT,*) 'recv_data: done deallocate rcvbuf'

!     update statistics

      amount_data(IRECV,comm_op) = amount_data(IRECV,comm_op) + nitm

!     decrement the number of messages remaining

      nummsg = nummsg - 1

   enddo ! ingb

   info = 0

   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,comm_op))

end subroutine sendrecv_data

end module m_sndrcv
