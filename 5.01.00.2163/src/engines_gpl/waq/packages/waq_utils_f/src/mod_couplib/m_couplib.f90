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

!-- MODULE m_couplib ---------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Subroutines that allow communication between different processes in a
!   coupled run using generic index tables.
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/m_couplib.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------
module m_couplib
use m_timings
use m_coupcns
use m_prcgrp
use m_ixset
use m_intfc
use m_sndrcv
use m_globcomm
implicit none
private

! constants available for external program units:

!-----------------------------------------------------------------------------
! From m_coupcns:
public IDUNNO, IVOID, IUNITY
public CP_SUM, CP_MAX, CP_MIN, CP_MAX1

!-----------------------------------------------------------------------------
! From m_prcgrp:
public myprc, numprc
! public MPI_COMM_ALL

!-----------------------------------------------------------------------------
! From m_ixset:
public ixset_hndl
public ixset_getprops
public ixset_print
public ixset_define
public ixset_product

!-----------------------------------------------------------------------------
! From m_intfc:
public intfc_hndl
public intfc_print
public intfc_define_collcitf
public intfc_define_dstrbitf
public intfc_define_updatitf
public intfc_replicate

!-----------------------------------------------------------------------------
! From m_sndrcv:
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

!-----------------------------------------------------------------------------
! From m_globcomm:
public sync_processes
public combine_data
public combine_1d_idata
public combine_1d_rdata
public combine_1d_ddata
public combine_2d_idata
public combine_2d_rdata
public combine_2d_ddata

!-----------------------------------------------------------------------------
! subroutines for configuration of the communications:
public couplib_init
public couplib_timers_init
public couplib_stop


contains


!-- SUBROUTINE couplib_init --------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Initialize the CouPLib data-structures
!!>
!!  On entry:
!!  lunout      optional: logical unit number of stdout
!!  idebug      optional: level of output requested (0=none)
!!
!!  On return:
!!  -           the data-structures have been properly initialized
!!<
!-----------------------------------------------------------------------------
subroutine couplib_init(lunout, idebug)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,          intent(in), optional  :: lunout
integer,          intent(in), optional  :: idebug

!-- LOCAL VARIABLES
! internal value of idebug
integer                 :: my_idebug
!-----------------------------------------------------------------------------

!  handle optional logical unit number for output messages

   if (present(lunout)) LOUT = lunout

!  handle optional idebug

   my_idebug = 1
   if (present(idebug)) my_idebug = idebug

   call prcgrp_initmod(idebug=my_idebug)
   call ixset_initmod()
   call intfc_initmod()
   call globcomm_initmod()
end subroutine couplib_init


!-- SUBROUTINE couplib_timers_init -------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Initialize the timers for CouPLib routines
!!>
!!  On entry:
!!  itimer1     number of first timer that may be used by CouPLib
!!  max_timers  maximum number of timers that may be used by CouPLib
!!  measr_idle  flag that indicates whether idle-time should be measured
!!              (by using barrier synchronisation) or not
!!
!!  On return:
!!  -           the timers for CouPLib have been properly initialized
!!<
!-----------------------------------------------------------------------------
subroutine couplib_timers_init(itimer_couplib1, max_timers, measr_idle)
!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,          intent(in) :: itimer_couplib1
integer,          intent(in) :: max_timers
logical,          intent(in) :: measr_idle

!-- LOCAL VARIABLES
integer               :: comm_op, itimr
character(len=STRLEN) :: namtmr
!-----------------------------------------------------------------------------

!  Check the supplied number of timers

   if (max_timers.lt.1) then
      write(LOUT,*) 'couplib_timers_init: Error: the number of timers ', &
         'assigned to CouPLib (', max_timers,') should be >= 1.'
      stop
   endif
   if (max_timers.lt.numtimers_couplib) then
      write(LOUT,*) 'couplib_timers_init: Warning: the number of timers ', &
         'assigned to CouPLib (', max_timers,') should better be >=', &
         numtimers_couplib
   endif

!  Register that timer numbers have been defined, and that timings may be used

   use_timers = .true.

!  Register whether measurement of idle-time should be used or not

   measure_idletime = measr_idle

!  Initialize first timer: "other", pointer all other timers to this one

   itimer_couplib_other = itimer_couplib1
   call timer_name(itimer_couplib_other, namtmr='Other CouPLib'   )

   do comm_op = 1, NUMCOMM
      itimer_commop(ICOMMTM,comm_op) = itimer_couplib_other
      itimer_commop(ISYNCTM,comm_op) = itimer_couplib_other
   enddo
   itimer_couplib_ixset         = itimer_couplib_other
   itimer_couplib_intfc         = itimer_couplib_other

!  For all timers for which a separate entry is supplied: set number, name

   itimr = 2
   do comm_op = 1, NUMCOMM
      if (max_timers.ge.itimr) then
         itimer_commop(ICOMMTM,comm_op) = itimer_couplib_other-1 + itimr
         namtmr = 'Comm. ' // trim(namcomm(comm_op))
         call timer_name(itimer_commop(ICOMMTM,comm_op), namtmr=namtmr)
         itimr = itimr + 1
      endif

      if (max_timers.ge.itimr) then
         itimer_commop(ISYNCTM,comm_op) = itimer_couplib_other-1 + itimr
         namtmr = 'Sync. ' // trim(namcomm(comm_op))
         call timer_name(itimer_commop(ISYNCTM,comm_op), namtmr=namtmr)
         itimr = itimr + 1
      endif
   enddo

   if (max_timers.ge.itimr) then
      itimer_couplib_ixset       = itimer_couplib_other-1 + itimr
      call timer_name(itimer_couplib_ixset, namtmr='Index-sets         ')
      itimr = itimr + 1
   endif

   if (max_timers.ge.itimr) then
      itimer_couplib_intfc       = itimer_couplib_other-1 + itimr
      call timer_name(itimer_couplib_intfc, namtmr='Interfaces         ')
      itimr = itimr + 1
   endif

end subroutine couplib_timers_init


!-- SUBROUTINE couplib_stop --------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Terminate usage of CouPLib
!!>
!!  On entry:
!!  -
!!
!!  On return:
!!  -           the MPI subsystem has been finalized
!!              Note: to be extended with cleanup of CouPLib data-structures
!!<
!-----------------------------------------------------------------------------
subroutine couplib_stop()
!-- HEADER VARIABLES/ARGUMENTS
implicit none

!-- LOCAL VARIABLES
integer     :: ierr
!-----------------------------------------------------------------------------

   call sndrcv_stopmod()
   call mpi_finalize(ierr)
end subroutine couplib_stop


end module m_couplib
