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

!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/sndrcv_wrappers.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------

!-- distribute data on index-set defined by cardinality ----------------------
!-- version for 0D, 1D logical arrays: ---------------------------------------

subroutine distribute_0d_ldata_on_cardset(mypart, lscalr, nelem, info, idebug)
integer, intent(in)                         :: mypart, nelem
logical, intent(inout)                      :: lscalr
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
logical, dimension(:)                       :: larray(1)
integer                                     :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   larray(1) = lscalr
   call distribute_1d_ldata_on_cardset(mypart, larray, nelem, info, my_idebug)
   lscalr = larray(1)
end subroutine distribute_0d_ldata_on_cardset


subroutine distribute_1d_ldata_on_cardset(mypart, larray, nelem, info, idebug)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)                       :: mypart
integer, intent(in)                       :: nelem
logical, dimension(*), intent(inout)      :: larray(nelem)
integer, intent(out)                      :: info
integer, intent(in), optional             :: idebug
!-- LOCAL VARIABLES
! rank of the process in MPI-communicator that must send data
integer :: iroot
! level of debug-output, 0=none
integer :: my_idebug
! number of elements received in mpi-communication
integer :: nrecvd
! status-code of mpi-operation
integer :: ierror

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug

   if (iscple.eq.0) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: non-coupled run, nothing to be done'
      return
   endif

   if (measure_idletime) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: entering barrier',&
         ' for measuring idle time'
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,IDSTRBC))
      call mpi_barrier(MPI_COMM_ALL, ierror)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,IDSTRBC))
   endif
   if (use_timers) call timer_start(itimer_commop(ICOMMTM,IDSTRBC))

   iroot = 0
   num_calls(IDSTRBC) = num_calls(IDSTRBC) + 1
   if (my_idebug.ge.1) write(LOUT,'(a,i3,a,i6,a,i7)') 'mypart=',mypart, &
       ': entering ',num_calls(IDSTRBC), '-th mpi_bcast, logical data, nelem=',nelem
   call mpi_bcast(larray, nelem, MPI_LOGICAL, iroot, MPI_COMM_ALL, ierror)
   if (my_idebug.ge.2) write(LOUT,'(a,i3,a,2i7)') 'mypart=',mypart, &
       ': distribute_cardset: ierror=',ierror, nelem
   info = ierror

   if (mypart.eq.1) then
      amount_data(ISEND,IDSTRBC) = amount_data(ISEND,IDSTRBC) + nelem*(numprc-1)
   else
      amount_data(IRECV,IDSTRBC) = amount_data(IRECV,IDSTRBC) + nelem
   endif
   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,IDSTRBC))
end subroutine distribute_1d_ldata_on_cardset


!-- distribute data on index-set defined by cardinality ----------------------
!-- versions for 0D, 2D, 3D and 4D integer arrays: ---------------------------

subroutine distribute_0d_idata_on_cardset(mypart, iscalr, nelem, info, idebug)
integer, intent(in)                         :: mypart, nelem
integer, intent(inout)                      :: iscalr
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer, dimension(:)                       :: iarray(1)
integer                                     :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   iarray(1) = iscalr
   call distribute_1d_idata_on_cardset(mypart, iarray, nelem, info, my_idebug)
   iscalr = iarray(1)
end subroutine distribute_0d_idata_on_cardset


subroutine distribute_2d_idata_on_cardset(mypart, iarray, nelem, info, idebug)
integer, intent(in)                         :: mypart, nelem
integer, dimension(:,:), intent(inout)      :: iarray
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                     :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_idata_on_cardset(mypart, iarray, nelem, info, my_idebug)
end subroutine distribute_2d_idata_on_cardset


subroutine distribute_3d_idata_on_cardset(mypart, iarray, nelem, info, idebug)
integer, intent(in)                         :: mypart, nelem
integer, dimension(:,:,:), intent(inout)    :: iarray
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                     :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_idata_on_cardset(mypart, iarray, nelem, info, my_idebug)
end subroutine distribute_3d_idata_on_cardset


subroutine distribute_4d_idata_on_cardset(mypart, iarray, nelem, info, idebug)
integer, intent(in)                         :: mypart, nelem
integer, dimension(:,:,:,:), intent(inout)  :: iarray
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                     :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_idata_on_cardset(mypart, iarray, nelem, info, my_idebug)
end subroutine distribute_4d_idata_on_cardset


!-- distribute data on index-set defined by cardinality ----------------------
!-- versions for 0D, 1D, 2D, 3D and 4D real arrays: --------------------------

subroutine distribute_0d_rdata_on_cardset(mypart, rscalr, nelem, info, idebug)
integer, intent(in)                          :: mypart, nelem
real(kind=4), intent(inout)                  :: rscalr
integer, intent(out)                         :: info
integer, intent(in), optional                :: idebug
real(kind=4), dimension(:)                   :: rarray(1)
integer                                      :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   rarray(1) = rscalr
   call distribute_1d_rdata_on_cardset(mypart, rarray, nelem, info, my_idebug)
   rscalr = rarray(1)
end subroutine distribute_0d_rdata_on_cardset


subroutine distribute_1d_rdata_on_cardset(mypart, rarray, nelem, info, idebug)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)                          :: mypart
integer, intent(in)                          :: nelem
real(kind=4), dimension(*), intent(inout)    :: rarray(nelem)
integer, intent(out)                         :: info
integer, intent(in), optional                :: idebug
!-- LOCAL VARIABLES
! rank of the process in MPI-communicator that must send data
integer :: iroot
! level of debug-output, 0=none
integer :: my_idebug
! number of elements received in mpi-communication
integer :: nrecvd
! status-code of mpi-operation
integer :: ierror

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug

   if (iscple.eq.0) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: non-coupled run, nothing to be done'
      return
   endif

   if (measure_idletime) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: entering barrier',&
         ' for measuring idle time'
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,IDSTRBC))
      call mpi_barrier(MPI_COMM_ALL, ierror)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,IDSTRBC))
   endif
   if (use_timers) call timer_start(itimer_commop(ICOMMTM,IDSTRBC))

   iroot = 0
   num_calls(IDSTRBC) = num_calls(IDSTRBC) + 1
   if (my_idebug.ge.1) write(LOUT,'(a,i3,a,i6,a,i7)') 'mypart=',mypart, &
       ': entering ',num_calls(IDSTRBC), '-th mpi_bcast, real    data, nelem=',nelem
   call mpi_bcast(rarray, nelem, MPI_REAL, iroot, MPI_COMM_ALL, ierror)
   if (my_idebug.ge.2) write(LOUT,'(a,i3,a,2i7)') 'mypart=',mypart, &
       ': distribute_cardset: ierror=',ierror,nelem
   info = ierror

   if (mypart.eq.1) then
      amount_data(ISEND,IDSTRBC) = amount_data(ISEND,IDSTRBC) + &
                                                     (numprc-1)*nelem*SIZE_REAL
   else
      amount_data(IRECV,IDSTRBC) = amount_data(IRECV,IDSTRBC) + nelem*SIZE_REAL
   endif
   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,IDSTRBC))
end subroutine distribute_1d_rdata_on_cardset


subroutine distribute_2d_rdata_on_cardset(mypart, rarray, nelem, info, idebug)
integer, intent(in)                          :: mypart, nelem
real(kind=4), dimension(:,:), intent(inout)  :: rarray
integer, intent(out)                         :: info
integer, intent(in), optional                :: idebug
integer                                      :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_rdata_on_cardset(mypart, rarray, nelem, info, my_idebug)
end subroutine distribute_2d_rdata_on_cardset


subroutine distribute_3d_rdata_on_cardset(mypart, rarray, nelem, info, idebug)
integer, intent(in)                             :: mypart, nelem
real(kind=4), dimension(:,:,:), intent(inout)   :: rarray
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                      :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_rdata_on_cardset(mypart, rarray, nelem, info, my_idebug)
end subroutine distribute_3d_rdata_on_cardset


subroutine distribute_4d_rdata_on_cardset(mypart, rarray, nelem, info, idebug)
integer, intent(in)                             :: mypart, nelem
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                      :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_rdata_on_cardset(mypart, rarray, nelem, info, my_idebug)
end subroutine distribute_4d_rdata_on_cardset


!-- distribute data on index-set defined by cardinality ----------------------
!-- versions for 0D, 1D, 2D, 3D and 4D double precision arrays: --------------

subroutine distribute_0d_ddata_on_cardset(mypart, dscalr, nelem, info, idebug)
integer, intent(in)                              :: mypart, nelem
real(kind=8), intent(inout)                      :: dscalr
integer, intent(out)                             :: info
integer, intent(in), optional                    :: idebug
real(kind=8), dimension(:)                       :: darray(1)
integer                                      :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   darray(1) = dscalr
   call distribute_1d_ddata_on_cardset(mypart, darray, nelem, info, my_idebug)
   dscalr = darray(1)
end subroutine distribute_0d_ddata_on_cardset


subroutine distribute_1d_ddata_on_cardset(mypart, darray, nelem, info, idebug)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)                       :: mypart
integer, intent(in)                       :: nelem
real(kind=8), dimension(*), intent(inout) :: darray(nelem)
integer, intent(out)                      :: info
integer, intent(in), optional             :: idebug
!-- LOCAL VARIABLES
! rank of the process in MPI-communicator that must send data
integer :: iroot
! level of debug-output, 0=none
integer :: my_idebug
! number of elements received in mpi-communication
integer :: nrecvd
! status-code of mpi-operation
integer :: ierror

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug

   if (iscple.eq.0) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: non-coupled run, nothing to be done'
      return
   endif

   if (measure_idletime) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: entering barrier',&
         ' for measuring idle time'
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,IDSTRBC))
      call mpi_barrier(MPI_COMM_ALL, ierror)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,IDSTRBC))
   endif
   if (use_timers) call timer_start(itimer_commop(ICOMMTM,IDSTRBC))

   iroot = 0
   num_calls(IDSTRBC) = num_calls(IDSTRBC) + 1
   if (my_idebug.ge.1) write(LOUT,'(a,i3,a,i6,a,i7)') 'mypart=',mypart, &
       ': entering ',num_calls(IDSTRBC), '-th mpi_bcast, double  data, nelem=',nelem
   call mpi_bcast(darray, nelem, MPI_REAL8, iroot, MPI_COMM_ALL, ierror)
   if (my_idebug.ge.2) write(LOUT,'(a,i3,a,2i7)') 'mypart=',mypart, &
       ': distribute_cardset: ierror=',ierror,nelem
   info = ierror

   if (mypart.eq.1) then
      amount_data(ISEND,IDSTRBC) = amount_data(ISEND,IDSTRBC) + &
                                                     (numprc-1)*nelem*SIZE_DBLE
   else
      amount_data(IRECV,IDSTRBC) = amount_data(IRECV,IDSTRBC) + nelem*SIZE_DBLE
   endif
   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,IDSTRBC))
end subroutine distribute_1d_ddata_on_cardset


subroutine distribute_2d_ddata_on_cardset(mypart, darray, nelem, info, idebug)
integer, intent(in)                             :: mypart, nelem
real(kind=8), dimension(:,:), intent(inout)     :: darray
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_ddata_on_cardset(mypart, darray, nelem, info, my_idebug)
end subroutine distribute_2d_ddata_on_cardset


subroutine distribute_3d_ddata_on_cardset(mypart, darray, nelem, info, idebug)
integer, intent(in)                             :: mypart, nelem
real(kind=8), dimension(:,:,:), intent(inout)   :: darray
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_ddata_on_cardset(mypart, darray, nelem, info, my_idebug)
end subroutine distribute_3d_ddata_on_cardset


subroutine distribute_4d_ddata_on_cardset(mypart, darray, nelem, info, idebug)
integer, intent(in)                             :: mypart, nelem
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug
   call distribute_1d_ddata_on_cardset(mypart, darray, nelem, info, my_idebug)
end subroutine distribute_4d_ddata_on_cardset


!-- distribute data on index-set defined by cardinality ----------------------
!-- versions for 1D character string arrays: ---------------------------------

subroutine distribute_1d_cdata_on_cardset(mypart, carray, nelem, info, idebug)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)                           :: mypart
integer, intent(in)                           :: nelem
character(len=*), dimension(*), intent(inout) :: carray
integer, intent(out)                          :: info
integer, intent(in), optional                 :: idebug
!-- LOCAL VARIABLES
! rank of the process in MPI-communicator that must send data
integer :: iroot
! number of elements received in mpi-communication
integer :: nrecvd
! status-code of mpi-operation
integer :: ierror
! internal value of idebug
integer :: my_idebug

   my_idebug = idebug_distribute_card
   if (present(idebug)) my_idebug = idebug

   if (iscple.eq.0) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: non-coupled run, nothing to be done'
      return
   endif

   if (measure_idletime) then
      if (my_idebug.ge.2) write(LOUT,*) 'distribute_cardset: entering barrier',&
         ' for measuring idle time'
      if (use_timers) call timer_start(itimer_commop(ISYNCTM,IDSTRBC))
      call mpi_barrier(MPI_COMM_ALL, ierror)
      if (use_timers) call timer_stop(itimer_commop(ISYNCTM,IDSTRBC))
   endif
   if (use_timers) call timer_start(itimer_commop(ICOMMTM,IDSTRBC))

   iroot = 0
   num_calls(IDSTRBC) = num_calls(IDSTRBC) + 1
   !write(LOUT,*) 'mypart=',mypart,': nelem=',nelem
   call mpi_bcast(carray, nelem, MPI_CHARACTER, iroot, MPI_COMM_ALL, ierror)
   !write(LOUT,*) 'mypart=',mypart,': ierror=',ierror
   info = ierror

   if (mypart.eq.1) then
      amount_data(ISEND,IDSTRBC) = amount_data(ISEND,IDSTRBC) + nelem*(numprc-1)
   else
      amount_data(IRECV,IDSTRBC) = amount_data(IRECV,IDSTRBC) + nelem
   endif
   if (use_timers) call timer_stop(itimer_commop(ICOMMTM,IDSTRBC))
end subroutine distribute_1d_cdata_on_cardset


!-- distribute on index-set defined by ixset-name ----------------------------
!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine distribute_2d_idata_on_namedset(mypart, iarray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
integer,      dimension(:,:), intent(inout)     :: iarray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_2d_idata_on_namedset


subroutine distribute_3d_idata_on_namedset(mypart, iarray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
integer,      dimension(:,:,:), intent(inout)   :: iarray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_3d_idata_on_namedset


subroutine distribute_4d_idata_on_namedset(mypart, iarray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
integer,      dimension(:,:,:,:), intent(inout) :: iarray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_4d_idata_on_namedset


!-- distribute on index-set defined by ixset-name ----------------------------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine distribute_1d_rdata_on_namedset(mypart, rarray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(*), intent(inout)       :: rarray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

!  write(LOUT,*) 'distribute_1d_rdata_on_namedset: starting for namixs="', &
!     trim(namixs),'", namitf="',trim(namitf),'"'
   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_1d_rdata_on_namedset


subroutine distribute_2d_rdata_on_namedset(mypart, rarray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(:,:), intent(inout)     :: rarray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

!  write(LOUT,*) 'distribute_2d_rdata_on_namedset: starting for namixs="', &
!     trim(namixs),'", namitf="',trim(namitf),'"'
   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_2d_rdata_on_namedset


subroutine distribute_3d_rdata_on_namedset(mypart, rarray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(:,:,:), intent(inout)   :: rarray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

!  write(LOUT,*) 'distribute_3d_rdata_on_namedset: starting for namixs="', &
!     trim(namixs),'", namitf="',trim(namitf),'"'
   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_3d_rdata_on_namedset


subroutine distribute_4d_rdata_on_namedset(mypart, rarray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

!  write(LOUT,*) 'distribute_4d_rdata_on_namedset: starting for namixs="', &
!     trim(namixs),'", namitf="',trim(namitf),'"'
   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_4d_rdata_on_namedset


!-- distribute on index-set defined by ixset-name ----------------------------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine distribute_1d_ddata_on_namedset(mypart, darray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(*), intent(inout)       :: darray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_1d_ddata_on_namedset


subroutine distribute_2d_ddata_on_namedset(mypart, darray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(:,:), intent(inout)     :: darray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_2d_ddata_on_namedset


subroutine distribute_3d_ddata_on_namedset(mypart, darray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(:,:,:), intent(inout)   :: darray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_3d_ddata_on_namedset


subroutine distribute_4d_ddata_on_namedset(mypart, darray, namixs, namitf, &
                                           info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
character(len=*), intent(in)                    :: namixs, namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IDSTRBX, info=info, idebug=my_idebug)
end subroutine distribute_4d_ddata_on_namedset


!-- distribute data on index-set defined by ixset-name + replication ---------
!-- versions for use without interface: --------------------------------------

subroutine distribute_idata(mypart, iarray, presiz, namixs, possiz, namitf, &
                            info, idebug)
integer,               intent(in)            :: mypart, presiz, possiz
integer, dimension(*), intent(inout)         :: iarray
character(len=*),      intent(in)            :: namixs, namitf
integer,               intent(out)           :: info
integer,               intent(in), optional  :: idebug
integer                                      :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_idata


subroutine distribute_rdata(mypart, rarray, presiz, namixs, possiz, namitf, &
                            info, idebug)
integer,                    intent(in)            :: mypart, presiz, possiz
real(kind=4), dimension(*), intent(inout)         :: rarray
character(len=*),           intent(in)            :: namixs, namitf
integer,                    intent(out)           :: info
integer,                    intent(in), optional  :: idebug
integer                                           :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_rdata


subroutine distribute_ddata(mypart, darray, presiz, namixs, possiz, namitf, &
                            info, idebug)
integer,                    intent(in)            :: mypart, presiz, possiz
real(kind=8), dimension(*), intent(inout)         :: darray
character(len=*),           intent(in)            :: namixs, namitf
integer,                    intent(out)           :: info
integer,                    intent(in), optional  :: idebug
integer                                           :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_ddata


!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine distribute_2d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
integer, dimension(:,:),          intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_2d_idata_on_repl_namedset


subroutine distribute_3d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
integer, dimension(:,:,:),        intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_3d_idata_on_repl_namedset


subroutine distribute_4d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
integer, dimension(:,:,:,:),      intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_4d_idata_on_repl_namedset


!-- distribute data on index-set defined by ixset-name + replication ---------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine distribute_1d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(*),       intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_1d_rdata_on_repl_namedset


subroutine distribute_2d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(:,:),     intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_2d_rdata_on_repl_namedset


subroutine distribute_3d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(:,:,:),   intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_3d_rdata_on_repl_namedset


subroutine distribute_4d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_4d_rdata_on_repl_namedset


!-- distribute data on index-set defined by ixset-name + replication ---------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine distribute_1d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(*),       intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_1d_ddata_on_repl_namedset


subroutine distribute_2d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(:,:),     intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_2d_ddata_on_repl_namedset


subroutine distribute_3d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(:,:,:),   intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_3d_ddata_on_repl_namedset


subroutine distribute_4d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                        namixs, possiz, namitf, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_distribute_named
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IDSTRBX, &
                      info=info, idebug=my_idebug)
end subroutine distribute_4d_ddata_on_repl_namedset

!-- collect data on index-set defined by ixset-name --------------------------
!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine collect_2d_idata_on_namedset(mypart, iarray, namixs, info, idebug)
integer, intent(in)                             :: mypart
integer,      dimension(:,:), intent(inout)     :: iarray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_2d_idata_on_namedset


subroutine collect_3d_idata_on_namedset(mypart, iarray, namixs, info, idebug)
integer, intent(in)                             :: mypart
integer,      dimension(:,:,:), intent(inout)   :: iarray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_3d_idata_on_namedset


subroutine collect_4d_idata_on_namedset(mypart, iarray, namixs, info, idebug)
integer, intent(in)                             :: mypart
integer,      dimension(:,:,:,:), intent(inout) :: iarray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_4d_idata_on_namedset


!-- collect data on index-set defined by ixset-name --------------------------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine collect_1d_rdata_on_namedset(mypart, rarray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(*), intent(inout)       :: rarray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_1d_rdata_on_namedset


subroutine collect_2d_rdata_on_namedset(mypart, rarray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(:,:), intent(inout)     :: rarray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_2d_rdata_on_namedset


subroutine collect_3d_rdata_on_namedset(mypart, rarray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(:,:,:), intent(inout)   :: rarray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_3d_rdata_on_namedset


subroutine collect_4d_rdata_on_namedset(mypart, rarray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_4d_rdata_on_namedset


!-- collect data on index-set defined by ixset-name --------------------------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine collect_1d_ddata_on_namedset(mypart, darray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(*), intent(inout)       :: darray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_1d_ddata_on_namedset


subroutine collect_2d_ddata_on_namedset(mypart, darray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(:,:), intent(inout)     :: darray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_2d_ddata_on_namedset


subroutine collect_3d_ddata_on_namedset(mypart, darray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(:,:,:), intent(inout)   :: darray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_3d_ddata_on_namedset


subroutine collect_4d_ddata_on_namedset(mypart, darray, namixs, info, idebug)
integer, intent(in)                             :: mypart
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
character(len=*), intent(in)                    :: namixs
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf='collect_itf', &
                      comm_op=ICOLLCT, info=info, idebug=my_idebug)
end subroutine collect_4d_ddata_on_namedset


!-- collect data on index-set defined by ixset-name + replication ------------
!-- versions for use without interface: --------------------------------------

subroutine collect_idata(mypart, iarray, presiz, namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
integer, dimension(*),            intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_idata


subroutine collect_rdata(mypart, rarray, presiz, namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(*),       intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_rdata


subroutine collect_ddata(mypart, darray, presiz, namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(*),       intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_ddata


!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine collect_2d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
integer, dimension(:,:),          intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_2d_idata_on_repl_namedset


subroutine collect_3d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
integer, dimension(:,:,:),        intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_3d_idata_on_repl_namedset


subroutine collect_4d_idata_on_repl_namedset(mypart, iarray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
integer, dimension(:,:,:,:),      intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_4d_idata_on_repl_namedset


!-- collect data on index-set defined by ixset-name + replication ------------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine collect_1d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(*),       intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_1d_rdata_on_repl_namedset


subroutine collect_2d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(:,:),     intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_2d_rdata_on_repl_namedset


subroutine collect_3d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(:,:,:),   intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_3d_rdata_on_repl_namedset


subroutine collect_4d_rdata_on_repl_namedset(mypart, rarray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_4d_rdata_on_repl_namedset


!-- collect data on index-set defined by ixset-name + replication ------------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine collect_1d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(*),       intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_1d_ddata_on_repl_namedset


subroutine collect_2d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(:,:),     intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_2d_ddata_on_repl_namedset


subroutine collect_3d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(:,:,:),   intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_3d_ddata_on_repl_namedset


subroutine collect_4d_ddata_on_repl_namedset(mypart, darray, presiz, &
                                             namixs, possiz, info, idebug)
integer,                          intent(in)    :: mypart, presiz, possiz
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_collect
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf='collect_itf', comm_op=ICOLLCT, &
                      info=info, idebug=my_idebug)
end subroutine collect_4d_ddata_on_repl_namedset


!-- update data on index-set defined by ixset-name using interface -----------
!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine update_2d_idata_on_namedset(iarray, namixs, namitf, info, idebug)
implicit none
integer, dimension(:,:), intent(inout)      :: iarray
character(len=*), intent(in)                :: namixs
character(len=*), intent(in)                :: namitf
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_2d_idata_on_namedset


subroutine update_3d_idata_on_namedset(iarray, namixs, namitf, info, idebug)
implicit none
integer, dimension(:,:,:), intent(inout)    :: iarray
character(len=*), intent(in)                :: namixs
character(len=*), intent(in)                :: namitf
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_3d_idata_on_namedset


subroutine update_4d_idata_on_namedset(iarray, namixs, namitf, info, idebug)
implicit none
integer, dimension(:,:,:,:), intent(inout)  :: iarray
character(len=*), intent(in)                :: namixs
character(len=*), intent(in)                :: namitf
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_4d_idata_on_namedset


!-- update data on index-set defined by ixset-name using interface -----------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine update_1d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(*), intent(inout)       :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_1d_rdata_on_namedset


subroutine update_2d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(:,:), intent(inout)     :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_2d_rdata_on_namedset


subroutine update_3d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(:,:,:), intent(inout)   :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_3d_rdata_on_namedset


subroutine update_4d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_4d_rdata_on_namedset


!-- update data on index-set defined by ixset-name using interface -----------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine update_1d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(*), intent(inout)       :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_1d_ddata_on_namedset


subroutine update_2d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(:,:), intent(inout)     :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_2d_ddata_on_namedset


subroutine update_3d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(:,:,:), intent(inout)   :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_3d_ddata_on_namedset


subroutine update_4d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      comm_op=IUPDATE, info=info, idebug=my_idebug)
end subroutine update_4d_ddata_on_namedset


!-- update data on index-set defined by ixset-name + replication -------------
!-- versions for use without interface: --------------------------------------

subroutine update_idata(iarray, presiz, namixs, possiz, namitf, info, idebug)
integer, dimension(*),            intent(inout) :: iarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_idata


subroutine update_rdata(rarray, presiz, namixs, possiz, namitf, info, idebug)
real(kind=4), dimension(*),       intent(inout) :: rarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_rdata


subroutine update_ddata(darray, presiz, namixs, possiz, namitf, info, idebug)
real(kind=8), dimension(*),       intent(inout) :: darray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_ddata


!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine update_2d_idata_on_repl_namedset(iarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
integer, dimension(:,:),          intent(inout) :: iarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_2d_idata_on_repl_namedset


subroutine update_3d_idata_on_repl_namedset(iarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
integer, dimension(:,:,:),        intent(inout) :: iarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_3d_idata_on_repl_namedset


subroutine update_4d_idata_on_repl_namedset(iarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
integer, dimension(:,:,:,:),      intent(inout) :: iarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_4d_idata_on_repl_namedset


!-- update data on index-set defined by ixset-name + replication ---------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine update_1d_rdata_on_repl_namedset(rarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=4), dimension(*),       intent(inout) :: rarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_1d_rdata_on_repl_namedset


subroutine update_2d_rdata_on_repl_namedset(rarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=4), dimension(:,:),     intent(inout) :: rarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_2d_rdata_on_repl_namedset


subroutine update_3d_rdata_on_repl_namedset(rarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=4), dimension(:,:,:),   intent(inout) :: rarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_3d_rdata_on_repl_namedset


subroutine update_4d_rdata_on_repl_namedset(rarray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_4d_rdata_on_repl_namedset


!-- update data on index-set defined by ixset-name + replication ---------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine update_1d_ddata_on_repl_namedset(darray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=8), dimension(*),       intent(inout) :: darray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_1d_ddata_on_repl_namedset


subroutine update_2d_ddata_on_repl_namedset(darray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=8), dimension(:,:),     intent(inout) :: darray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_2d_ddata_on_repl_namedset


subroutine update_3d_ddata_on_repl_namedset(darray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=8), dimension(:,:,:),   intent(inout) :: darray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_3d_ddata_on_repl_namedset


subroutine update_4d_ddata_on_repl_namedset(darray, presiz, namixs, possiz, &
                                            namitf, info, idebug)
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
integer,                          intent(in)    :: presiz, possiz
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_update
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, comm_op=IUPDATE, &
                      info=info, idebug=my_idebug)
end subroutine update_4d_ddata_on_repl_namedset


!-- accumulate data on index-set defined by ixset-name -----------------------
!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine accumulate_2d_idata_on_namedset(iarray, namixs, namitf, info, idebug)
implicit none
integer, dimension(:,:), intent(inout)      :: iarray
character(len=*), intent(in)                :: namixs
character(len=*), intent(in)                :: namitf
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_2d_idata_on_namedset


subroutine accumulate_3d_idata_on_namedset(iarray, namixs, namitf, info, idebug)
implicit none
integer, dimension(:,:,:), intent(inout)    :: iarray
character(len=*), intent(in)                :: namixs
character(len=*), intent(in)                :: namitf
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_3d_idata_on_namedset


subroutine accumulate_4d_idata_on_namedset(iarray, namixs, namitf, info, idebug)
implicit none
integer, dimension(:,:,:,:), intent(inout)  :: iarray
character(len=*), intent(in)                :: namixs
character(len=*), intent(in)                :: namitf
integer, intent(out)                        :: info
integer, intent(in), optional               :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_4d_idata_on_namedset


!-- accumulate data on index-set defined by ixset-name -----------------------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine accumulate_1d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(*), intent(inout)       :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_1d_rdata_on_namedset


subroutine accumulate_2d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(:,:), intent(inout)     :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_2d_rdata_on_namedset


subroutine accumulate_3d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(:,:,:), intent(inout)   :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_3d_rdata_on_namedset


subroutine accumulate_4d_rdata_on_namedset(rarray, namixs, namitf, info, idebug)
implicit none
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_4d_rdata_on_namedset


!-- accumulate data on index-set defined by ixset-name -----------------------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine accumulate_1d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(*), intent(inout)       :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_1d_ddata_on_namedset


subroutine accumulate_2d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(:,:), intent(inout)     :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_2d_ddata_on_namedset


subroutine accumulate_3d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(:,:,:), intent(inout)   :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_3d_ddata_on_namedset


subroutine accumulate_4d_ddata_on_namedset(darray, namixs, namitf, info, idebug)
implicit none
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
character(len=*), intent(in)                    :: namixs
character(len=*), intent(in)                    :: namitf
integer, intent(out)                            :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug
   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, namixs=namixs, namitf=namitf, &
                      ladd=.true., comm_op=IACCUM, info=info, idebug=my_idebug)
end subroutine accumulate_4d_ddata_on_namedset


!-- accumulate data on index-set defined by ixset-name + replication ---------
!-- versions for use without interface: --------------------------------------

subroutine accumulate_idata(iarray, presiz, namixs, possiz, namitf, info, &
                            idebug)
integer,                          intent(in)    :: presiz, possiz
integer, dimension(*),            intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_idata


subroutine accumulate_rdata(rarray, presiz, namixs, possiz, namitf, info, &
                            idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=4), dimension(*),       intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_rdata


subroutine accumulate_ddata(darray, presiz, namixs, possiz, namitf, info, &
                            idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=8), dimension(*),       intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_ddata


!-- versions for 2D, 3D and 4D integer arrays: -------------------------------

subroutine accumulate_2d_idata_on_repl_namedset(iarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
integer, dimension(:,:),          intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_2d_idata_on_repl_namedset


subroutine accumulate_3d_idata_on_repl_namedset(iarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
integer, dimension(:,:,:),        intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_3d_idata_on_repl_namedset


subroutine accumulate_4d_idata_on_repl_namedset(iarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
integer, dimension(:,:,:,:),      intent(inout) :: iarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(iarray=iarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_4d_idata_on_repl_namedset


!-- accumulate data on index-set defined by ixset-name + replication ---------
!-- versions for 1D, 2D, 3D and 4D real arrays: ------------------------------

subroutine accumulate_1d_rdata_on_repl_namedset(rarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=4), dimension(*),       intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_1d_rdata_on_repl_namedset


subroutine accumulate_2d_rdata_on_repl_namedset(rarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=4), dimension(:,:),     intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_2d_rdata_on_repl_namedset


subroutine accumulate_3d_rdata_on_repl_namedset(rarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=4), dimension(:,:,:),   intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_3d_rdata_on_repl_namedset


subroutine accumulate_4d_rdata_on_repl_namedset(rarray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=4), dimension(:,:,:,:), intent(inout) :: rarray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(rarray=rarray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_4d_rdata_on_repl_namedset


!-- accumulate data on index-set defined by ixset-name + replication ---------
!-- versions for 1D, 2D, 3D and 4D double precision arrays: ------------------

subroutine accumulate_1d_ddata_on_repl_namedset(darray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=8), dimension(*),       intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_1d_ddata_on_repl_namedset


subroutine accumulate_2d_ddata_on_repl_namedset(darray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=8), dimension(:,:),     intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_2d_ddata_on_repl_namedset


subroutine accumulate_3d_ddata_on_repl_namedset(darray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=8), dimension(:,:,:),   intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_3d_ddata_on_repl_namedset


subroutine accumulate_4d_ddata_on_repl_namedset(darray, presiz, namixs, &
                                                possiz, namitf, info, idebug)
integer,                          intent(in)    :: presiz, possiz
real(kind=8), dimension(:,:,:,:), intent(inout) :: darray
character(len=*),                 intent(in)    :: namixs, namitf
integer,                          intent(out)   :: info
integer, intent(in), optional                   :: idebug
integer                                         :: my_idebug

   my_idebug = idebug_accumulate
   if (present(idebug)) my_idebug = idebug
   call sendrecv_data(darray=darray, presiz=presiz, namixs=namixs, &
                      possiz=possiz, namitf=namitf, ladd=.true., &
                      comm_op=IACCUM, info=info, &
                      idebug=my_idebug)
end subroutine accumulate_4d_ddata_on_repl_namedset


