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

!-- MODULE m_timings ---------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Subroutines that allow detailed measurement of cpu- and wallclock-time
!   used by different portions of code of a simulation program.
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_timings/m_timings.f90 $
!   $Revision: 33 $, $Date: 2007-11-02 16:51:48 +0100 (Fri, 02 Nov 2007) $
!
!   Version 1.0  21-09-2007  initial version
!
!-----------------------------------------------------------------------------
module m_timings
implicit none

!
! A timer can be compared to a stop-watch.
!
! You can reset it, start, stop, start again, stop again, read it, and so
! on. When you read it you get the accumulated time over all periods that
! it was started ("active").
!
! - our timers measure both cpu-time (cpu-usage) and wallclock-time (elapsed
!   time). Also the number of times that it was started is reported.
! - a name/description of up to 20 characters can be assigned to each timer.
!   This name/description is reported again when the timer is read.
! - start and stop are implemented using different functions, instead of by
!   a single button. It is a (non-fatal) error to call start again before
!   stop has been called. A warning will be printed and the second start
!   will be ignored.
! - it is possible to read a timer while it is running. This reports the
!   accumulated time of all previous elapsed periods plus the time of the
!   running period.
!
! This module is built around a "large" array of timers: timer_table.
!
! The user may define the size of the table, and may decide for himself for
! which purpose each timer is used. A possible layout is to use timer 1 for the
! total time for the application, timers 2-9 for the main chunks, 20-29 for
! further subdivision of timer 2, and so on.
!
! The table is two-dimensional: for each timer (first dimension) there may be
! one or more "instances" (second dimension). The instance-argument of the
! various routines is optional. These instances can be used when a segment
! of code is traversed multiple times. For instance to measure times for first
! and second half time-steps separately. Or to time all iterations of a loop
! separately and calculate statistics of the timings afterwards.
!

public timer_table_size
public timer_name
public timer_start
public timer_stop
public timer_read
public timer_reset

! Logical unit-number of stdout, used for error messages and debug info
! (LOUTTM may be redirected to a file, * is not).
! This variable may be changed outside m_timings.
integer, public    :: LOUTTM   = 6

! Level of debug-output of these routines, 0==none
integer            :: idebug = 0

private

! definition of one timer:
type, private :: t_one_timer
   character(len=20) :: namtmr  !! name of the timer, used for output
   real(kind=8)      :: totcpu  !! accumulated cpu-time for this timer
   real(kind=8)      :: totwal  !! accumulated wall-clock time for this timer
   integer           :: numtms  !! number of times timer has been called and
                                !! times have been added to totcpu and totwal
   integer           :: isactv  !! flag indicating that a new timing is active
                                !! (has been started) (1) or not (0)
   real(kind=8)      :: stacpu  !! cpu-time at start of current (active) timing
   real(kind=8)      :: stawal  !! walltime at start of current (active) timing
end type t_one_timer

! maximum extent of 2D table (particularly for print-statements)
integer, parameter :: MAX_NTIMER=99999
integer, parameter :: MAX_NINSTN=  999

! definition of the entire 2D table of timers:
type(t_one_timer), dimension(:,:), allocatable, target :: timer_table

contains

!-- SUBROUTINE timer_table_size ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Set the size of the timer_table, allocate the table, and initialize all
!!  timers.
!!>
!!  On entry:
!!  ntimer      number of timers to be used in the table
!!  ninstn_arg  optional: number instances per timer to be used (default 1)
!!
!!  On return:
!!  -           the timer_table has been allocated and initialized
!!<
!-----------------------------------------------------------------------------
subroutine timer_table_size(ntimer, ninstn_arg)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)           :: ntimer
integer, intent(in), optional :: ninstn_arg

!-- LOCAL VARIABLES
! loop counters, actual number of instances
integer :: itimer, iinstn, ninstn
!-----------------------------------------------------------------------------

!  Handle optional ninstn-argument

   if (present(ninstn_arg)) then
      ninstn = ninstn_arg
   else
      ninstn = 1
   endif

!  Check that requested size of the table is valid

   if (ntimer.le.0 .or. ninstn.le.0) then
      write(LOUTTM,*) 'timer_table_size: Error: dimensions must be >0!'
      write(LOUTTM,*) '   ntimer=', ntimer,', ninstn=',ninstn
      stop
   endif

   if (ntimer.gt.MAX_NTIMER .or. ninstn.gt.MAX_NINSTN) then
      write(LOUTTM,*) 'timer_table_size: Error: requested dimensions too large!'
      write(LOUTTM,*) '   ntimer=', ntimer,', max=',MAX_NTIMER
      write(LOUTTM,*) '   ninstn=', ninstn,', max=',MAX_NINSTN
      stop
   endif

!  Check that table has not been allocated before
!  (different behaviour is possible: re-allocate, re-initialize?)

   if (allocated(timer_table)) then
      deallocate(timer_table)
!      write(LOUTTM,*) 'timer_table_size: Error: table is already allocated!'
!      stop
   endif

!  Allocate the table

   allocate(timer_table(ntimer,ninstn))

!  Initialize all timers and instances

   do itimer = 1, ntimer
      do iinstn = 1, ninstn
         timer_table(itimer,iinstn)%namtmr = ' '
         timer_table(itimer,iinstn)%numtms = 0
         timer_table(itimer,iinstn)%totcpu = 0d0
         timer_table(itimer,iinstn)%totwal = 0d0
         timer_table(itimer,iinstn)%isactv = 0
      enddo
   enddo

end subroutine timer_table_size


!-- SUBROUTINE timer_name ----------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Set the name of timer (stop-watch) with coordinates (itimer, iinstn_arg)
!!>
!!  On entry:
!!  itimer      number of timer to be started
!!  iinstn_arg  optional: instance of timer to be started, default 1
!!  namtmr      name/description of the timer
!!
!!  On return:
!!  -           the name has been registered in the timer_table
!!<
!-----------------------------------------------------------------------------
subroutine timer_name(itimer, iinstn_arg, namtmr)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,          intent(in)           :: itimer
integer,          intent(in), optional :: iinstn_arg
character(len=*), intent(in)           :: namtmr

!-- LOCAL VARIABLES
! dimensions of timer_table, actual instance
integer      :: ntimer, ninstn, iinstn
!-----------------------------------------------------------------------------

!  Handle optional iinstn-argument

   if (present(iinstn_arg)) then
      iinstn = iinstn_arg
   else
      iinstn = 1
   endif

!  Check that table is allocated and that requested timer is within bounds

   if (.not.allocated(timer_table)) then
      write(LOUTTM,*) 'timer_name: Error: timer_table has not been allocated yet!'
      stop
   endif

   ntimer = size(timer_table,1)
   ninstn = size(timer_table,2)
   if (itimer.le.0 .or. itimer.gt.ntimer .or. &
       iinstn.le.0 .or. iinstn.gt.ninstn) then
      write(LOUTTM,91) 'timer_name: Error: requested timer (',itimer,',',&
         iinstn, ') is out of bounds; ntimer=',ntimer,', ninstn=',ninstn,'.'
      stop
   endif

!  register the name/description of the timer in timer_table

   timer_table(itimer,iinstn)%namtmr = namtmr

91 format(a,i5,a,i3,a,i10,a,i5)
92 format(a,i5,a,i3,a)

end subroutine timer_name


!-- SUBROUTINE timer_start ---------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Start the timer (stop-watch) with coordinates (itimer, iinstn_arg)
!!>
!!  On entry:
!!  itimer      number of timer to be started
!!  iinstn_arg  optional: instance of timer to be started, default 1
!!
!!  On return:
!!  -           the start-time has been registered in the timer_table
!!<
!-----------------------------------------------------------------------------
subroutine timer_start(itimer, iinstn_arg)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)           :: itimer
integer, intent(in), optional :: iinstn_arg

!-- LOCAL VARIABLES
! dimensions of timer_table, actual instance
integer      :: ntimer, ninstn, iinstn
! current cpu/wallclock-time
real(kind=8) :: curcpu, curwal
!-----------------------------------------------------------------------------

!  Handle optional iinstn-argument

   if (present(iinstn_arg)) then
      iinstn = iinstn_arg
   else
      iinstn = 1
   endif

!  Check that table is allocated and that requested timer is within bounds

   if (.not.allocated(timer_table)) then
      write(LOUTTM,*) 'timer_start: Error: timer_table has not been allocated ',&
         'yet!'
      stop
   endif

   ntimer = size(timer_table,1)
   ninstn = size(timer_table,2)
   if (itimer.le.0 .or. itimer.gt.ntimer .or. &
       iinstn.le.0 .or. iinstn.gt.ninstn) then
      write(LOUTTM,91) 'timer_start: Error: requested timer (',itimer,',',&
         iinstn, ') is out of bounds; ntimer=',ntimer,', ninstn=',ninstn,'.'
      stop
   endif

!  Check that no timing is active yet for the requested timer

   if (timer_table(itimer,iinstn)%isactv.gt.0) then
      write(LOUTTM,92) 'timer_start: Error: requested timer (',itimer,',',&
         iinstn,') has already been started; current start will be ignored.'
   else

!     get current cpu/wallclock time and store in timer_table

      call cpu_time(curcpu)
      call wall_time(curwal)
      timer_table(itimer,iinstn)%isactv = 1
      timer_table(itimer,iinstn)%stacpu = curcpu
      timer_table(itimer,iinstn)%stawal = curwal

   endif

91 format(a,i5,a,i3,a,i10,a,i5)
92 format(a,i5,a,i3,a)

end subroutine timer_start


!-- SUBROUTINE timer_stop ----------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Stop the timer (stop-watch) with coordinates (itimer, iinstn_arg)
!!>
!!  On entry:
!!  itimer      number of timer to be stopped
!!  iinstn_arg  optional: instance of timer to be stopped, default 1
!!
!!  On return:
!!  -           the elapsed (cpu and wallclock) time since the start of the
!!              timing has been added to the timer in the timer_table
!!<
!-----------------------------------------------------------------------------
subroutine timer_stop(itimer, iinstn_arg)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)           :: itimer
integer, intent(in), optional :: iinstn_arg

!-- LOCAL VARIABLES
! dimensions of timer_table, actual instance
integer      :: ntimer, ninstn, iinstn
! current cpu/wallclock-time
real(kind=8) :: curcpu, curwal
! pointer to current timer
type(t_one_timer), pointer :: timer
!-----------------------------------------------------------------------------

!  Handle optional iinstn-argument

   if (present(iinstn_arg)) then
      iinstn = iinstn_arg
   else
      iinstn = 1
   endif

!  Check that table is allocated and that requested timer is within bounds

   if (.not.allocated(timer_table)) then
      write(LOUTTM,*) 'timer_stop: Error: timer_table has not been allocated yet!'
      stop
   endif

   ntimer = size(timer_table,1)
   ninstn = size(timer_table,2)
   if (itimer.le.0 .or. itimer.gt.ntimer .or. &
       iinstn.le.0 .or. iinstn.gt.ninstn) then
      write(LOUTTM,91) 'timer_stop: Error: requested timer (',itimer,',',iinstn, &
                  ') is out of bounds; ntimer=',ntimer,', ninstn=',ninstn,'.'
      stop
   endif

!  Check that a timing is active for the requested timer

   if (timer_table(itimer,iinstn)%isactv.le.0) then
      write(LOUTTM,92) 'timer_stop: Error: requested timer (',itimer,',',iinstn, &
                  ') has not been started; current stop will be ignored.'
   else

!     get current cpu/wallclock time; compute elapsed time since start and
!     add to totals in timer_table

      call cpu_time(curcpu)
      call wall_time(curwal)

      timer => timer_table(itimer,iinstn)
      timer%isactv = 0
      timer%numtms = timer%numtms + 1
      timer%totcpu = timer%totcpu + curcpu - timer%stacpu
      timer%totwal = timer%totwal + curwal - timer%stawal

   endif

91 format(a,i5,a,i3,a,i10,a,i5)
92 format(a,i5,a,i3,a)

end subroutine timer_stop


!-- SUBROUTINE timer_read ----------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Return the name and times for timer (stop-watch) with coordinates
!!  (itimer, iinstn_arg)
!!>
!!  On entry:
!!  itimer      number of timer to be read
!!  iinstn_arg  optional: instance of timer to be read, default 1
!!
!!  On return:
!!  namtmr      name/description of timer
!!  numtms      number of timings performed with this timer; number of times
!!              that the timer was started and stopped since the timer was
!!              initialized/reset.
!!  cputim      total amount of CPU-time used in the timings with this timer
!!  waltim      total amount of wallclock-time elapsed in the timings with
!!              this timer
!!<
!-----------------------------------------------------------------------------
subroutine timer_read(itimer, iinstn_arg, namtmr, numtms, cputim, waltim)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,          intent(in)            :: itimer
integer,          intent(in), optional  :: iinstn_arg
character(len=*), intent(out)           :: namtmr
integer,          intent(out)           :: numtms
real(kind=8),     intent(out)           :: cputim
real(kind=8),     intent(out)           :: waltim

!-- LOCAL VARIABLES
! dimensions of timer_table, actual instance
integer      :: ntimer, ninstn, iinstn
! current cpu/wallclock-time
real(kind=8) :: curcpu, curwal
!-----------------------------------------------------------------------

!  Handle optional iinstn-argument

   if (present(iinstn_arg)) then
      iinstn = iinstn_arg
   else
      iinstn = 1
   endif

!  Check that table is allocated and that requested timer is within bounds

   if (.not.allocated(timer_table)) then
      write(LOUTTM,*) 'timer_read: Error: timer_table has not been allocated yet!'
      stop
   endif

   ntimer = size(timer_table,1)
   ninstn = size(timer_table,2)
   if (itimer.le.0 .or. itimer.gt.ntimer .or. &
       iinstn.le.0 .or. iinstn.gt.ninstn) then
      write(LOUTTM,91) 'timer_read: Error: requested timer (',itimer,',',iinstn, &
                  ') is out of bounds; ntimer=',ntimer,', ninstn=',ninstn,'.'
      stop
   endif

!  Get current accumulated timings from timer_table

   namtmr = timer_table(itimer,iinstn)%namtmr
   numtms = timer_table(itimer,iinstn)%numtms
   cputim = timer_table(itimer,iinstn)%totcpu
   waltim = timer_table(itimer,iinstn)%totwal

!  If a timing is active for the requested timer, get time and add to
!     accumulated time

   if (timer_table(itimer,iinstn)%isactv.gt.0) then
      call cpu_time(curcpu)
      call wall_time(curwal)

      numtms = numtms + 1
      cputim = cputim + curcpu - timer_table(itimer,iinstn)%stacpu
      waltim = waltim + curwal - timer_table(itimer,iinstn)%stawal
   endif

91 format(a,i5,a,i3,a,i10,a,i5)
92 format(a,i5,a,i3,a)

end subroutine timer_read


!-- SUBROUTINE timer_reset ---------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Reset the timer (stop-watch) with coordinates (itimer, iinstn)
!!>
!!  On entry:
!!  itimer      number of timer to be reset
!!  iinstn      optional: instance of timer to be reset
!!
!!  On return:
!!  -           the timer has been reset
!!<
!-----------------------------------------------------------------------------
subroutine timer_reset(itimer, iinstn_arg)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)           :: itimer
integer, intent(in), optional :: iinstn_arg

!-- LOCAL VARIABLES
! dimensions of timer_table, actual instance
integer      :: ntimer, ninstn, iinstn
!-----------------------------------------------------------------------

!  Handle optional iinstn-argument

   if (present(iinstn_arg)) then
      iinstn = iinstn_arg
   else
      iinstn = 1
   endif

!  Check that table is allocated and that requested timer is within bounds

   if (.not.allocated(timer_table)) then
      write(LOUTTM,*) 'timer_reset: Error: timer_table has not been allocated ',&
         'yet!'
      stop
   endif

   ntimer = size(timer_table,1)
   ninstn = size(timer_table,2)
   if (itimer.le.0 .or. itimer.gt.ntimer .or. &
       iinstn.le.0 .or. iinstn.gt.ninstn) then
      write(LOUTTM,91) 'timer_reset: Error: requested timer (',itimer,',',&
         iinstn, ') is out of bounds; ntimer=',ntimer,', ninstn=',ninstn,'.'
      stop
   endif

!  Check that no timing is active for the requested timer

   if (timer_table(itimer,iinstn)%isactv.gt.0) then
      write(LOUTTM,92) 'timer_reset: Warning: requested timer (',itimer,',',&
         iinstn, ') is active; timer will be reset anyway.'
   endif

!  Reset requested timer in timer_table

   timer_table(itimer,iinstn)%numtms = 0
   timer_table(itimer,iinstn)%totcpu = 0d0
   timer_table(itimer,iinstn)%totwal = 0d0
   timer_table(itimer,iinstn)%isactv = 0

91 format(a,i5,a,i3,a,i10,a,i5)
92 format(a,i5,a,i3,a)

end subroutine timer_reset


!-- SUBROUTINE wall_time -----------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Deliver the elapsed wallclock time in seconds with respect to an unknown
!!  but fixed reference
!!>
!!  On entry:
!!  -
!!
!!  On return:
!!  curwal      current wallclock time
!!<
!-----------------------------------------------------------------------------
subroutine wall_time(curwal)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
real(kind=8), intent(out) :: curwal

!-- LOCAL VARIABLES
logical         :: is_initialized = .false.
! conversion-factor from clock-ticks to seconds
real(kind=8)    :: mulfac_to_secs
! the number of times the clock has been reset during program execution
integer         :: num_periods = 0
! the amount of time per period in which the clock is reset once
real(kind=8)    :: secs_per_period
! value of the system_clock in the previous call to this routine, for
! detecting that the system_clock was reset
integer         :: last_count = 0
! internal state-variables:
save is_initialized, mulfac_to_secs
save num_periods, secs_per_period, last_count
! auxiliary variables for calling the system_clock
integer :: curr_ticks, tick_rate, tick_max
! auxiliary variables for debug-output
real(kind=8)    :: rem_secs
!-----------------------------------------------------------------------------

! Initialization of this routine:
!  - determine the measuring-unit of subroutine system_clock (e.g. 10000
!    ticks per second)
!  - derive the multiplication factor for conversion to seconds (e.g. 0.00001)
!  - determine the maximum value of this clock (e.g. 2147483647)
!  - derive the number of seconds after which the timer tops over (e.g. 214748,
!    which is about 59 hours)

  if (.not.is_initialized) then
     call system_clock(curr_ticks, tick_rate, tick_max)
     if (idebug.ge.2) write(LOUTTM,*) 'curr_ticks=',curr_ticks,', tick_max=',&
        tick_max, 'tick_rate=',tick_rate
     rem_secs = (real(tick_max,8) - real(curr_ticks,8)) / real(tick_rate,8)
     if (idebug.ge.1) write(LOUTTM,*) 'time to timer-roll-over is approx.', &
         int(rem_secs/3600.), ' hr', &
         nint( (rem_secs-3600.*int(rem_secs/3600.))/60. ), ' min'
     mulfac_to_secs  = 1d0 / real(tick_rate,8)
     secs_per_period = tick_max * mulfac_to_secs
     is_initialized  = .true.
  else
     call system_clock(curr_ticks)
  endif

! Detect whether the timer has been reset since the last time that this routine
! was called. If this routine is called often enough (e.g. at least once in
! every 24 hours) then all resets will be detected.

  if (curr_ticks.lt.last_count) then
     if (idebug.ge.2) write(LOUTTM,*) 'wall_time: timer roll-over. curr_ticks=',&
        curr_ticks, ', last_count=',last_count
     num_periods = num_periods + 1
     if (idebug.ge.2) write(LOUTTM,*) 'num_periods=',num_periods
  endif
  last_count = curr_ticks

! Compute the number of seconds since the last time the system clock was
! reset before the application was started: the current number of ticks plus
! the number of times the clock was reset during program execution

  curwal = real(curr_ticks,8) * mulfac_to_secs + num_periods * secs_per_period

end subroutine wall_time

end module m_timings
