! Module for synchronisation RTC to and from Flow
module SyncRtcFlow
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: sync_rtcflow.f90 1333 2012-03-16 13:53:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/delftio_sync/packages/delftio_sync/src/sync_rtcflow.f90 $
!!--description-----------------------------------------------------------------
! Organizes the communication between the FLOW
! executable and the RTC executable.
! This module is the interface between DelftIO and
! Delft3D-FLOW.
! Method used: Handled by DelftIO
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
  use Dio_Plt_Rw
!
  implicit none
!
! VARIABLES USED BY BOTH RTC AND FLOW SIDE
!
  type(DioPltType)  :: SignalFlowToRtc
  type(DioPltType)  :: SignalRtcToFlow
  type(DioPltType)  :: DataRtcToFlow
!
  integer, dimension(:,:), pointer :: SignalRValues
  character(len=DioMaxParLen), pointer, dimension(:) :: SignalRPars
  character(len=DioMaxLocLen), pointer, dimension(:) :: SignalRLocs
!
  integer, dimension(1,3) :: SignalXValues
  character*20, dimension(1) :: SignalXPars
  character*20, dimension(3) :: SignalXLocs
!
  data SignalXPars / 'Signal' /
  data SignalXLocs / 'Status', 'Date', 'Time' /
!
  integer, parameter :: commNone            = 0
  integer, parameter :: commBarriers        = 1
  integer, parameter :: commSalinity        = 2
!
  integer :: communicationMode = commNone
!
! VARIABLES USED BY RTC SIDE
!
  ! Barrier names received from Flow
  character(len=DioMaxParLen), Allocatable, Save :: BarrierNames(:)
!
  ! Array to store results of calculations
  real        , Allocatable, Save :: Valbar(:,:)
!
  ! Array to store data for sending
  ! Specification of array is rectengular to data in Flow/Rtc
  real        , Allocatable, Save :: DataValuesOut(:,:)
!
  ! Number of barriers in Flow
  Integer :: numBarriers
!
  ! Locations for sending data
  character*20, dimension(2) :: Locbar
!
  data Locbar / 'Status', 'Height' /
!
! VARIABLES USED BY D3D FLOW SIDE
!
  character(len=DioMaxParLen), pointer, dimension(:) :: DataPars
  character(len=DioMaxLocLen), pointer, dimension(:) :: DataLocs
  real, dimension(:,:), pointer :: DataValuesIn


contains

!!!
!!! FUNCTIONS RTC SIDE
!!!

! Initialise communication between RTC and Flow
subroutine SyncRtcFlow_Init(n2steps, lerror, commMode)

  integer :: n2steps
  logical :: lerror
  integer :: commMode

  integer :: nRpar, nRLoc
  integer :: nDummyLocs
  integer :: FlowStatus

  integer :: itest

  ! Stream for receiving barrier names
  type(DioPltType)  :: InfoFlowToRtc
  character(len=DioMaxParLen), pointer, dimension(:) :: InfoPars
  character(len=DioMaxLocLen), pointer, dimension(:) :: DummyLocs


  ! Initialise lerror
  lerror = .false.

  ! Init DelftIO
  call DioInit

  ! store commMode
  communicationMode = commMode

  ! Signal from Flow to RTC
  ! First to receive number of (half) time steps
  ! Then to receive status (< 0 = Quit), Date (I8) and Time (I6)
  SignalFlowToRtc = DioPltGetDataset('SignalToRtc')
  nRPar = DioPltGetNPar(SignalFlowToRtc)
  nRLoc = DioPltGetNLoc(SignalFlowToRtc)

  if (DioPltGet(SignalFlowToRtc, SignalRValues)) then
    n2steps = SignalRValues(1,1)
  else
    n2steps = -2
  endif

  if (n2steps .eq. -1) then
    lerror = .true.
    return
  endif
  !
  ! Check whether the communication mode requires barrier heights to be sent to FLOW
  !
  if (communicationMode == commBarriers) then

    ! Signal from RTC to Flow
    ! Then to send status (< 0 = Quit), Date (I8) and Time (I6)
    SignalRtcToFlow = DioPltDefine('SignalToFlow', Dio_Plt_Integer, SignalXPars, SignalXLocs)

    ! Initialise send values
    SignalXValues(1,1) = 1
    SignalXValues(1,2) = 2
    SignalXValues(1,3) = 3

    if (n2steps .le. 0) then
      SignalXValues(1,1) = -1
      ! Send bad status
      call DioPltPut(SignalRtcToFlow, SignalXValues)
      lerror = .true.
      return
    endif

    ! Send OK status due to time step value
    SignalXValues(1,1) = 1
    call DioPltPut(SignalRtcToFlow, SignalXValues)

    ! Get stream with barrier names
    InfoFlowToRtc = DioPltGetDataset('InfoToRtc')
    numBarriers         = DioPltGetNPar(InfoFlowToRtc)
    nDummyLocs    = DioPltGetNLoc(InfoFlowToRtc)

    if (numBarriers .le. 0) then
      SignalXValues(1,1) = -1
      ! Send bad status
      call DioPltPut(SignalRtcToFlow, SignalXValues)
      ! Cleanup Info-stream did its work however we are not satisfied
      call DioPltDestroy(InfoFlowToRtc)
      lerror = .true.
      return
    endif

    ! Allocate arrays for barrier names and data and store the names
    Allocate ( BarrierNames(numBarriers) )
    Allocate ( Valbar(2, numBarriers) )
    Allocate ( DataValuesOut(numBarriers, 2) )
    InfoPars => DioPltGetPars(InfoFlowToRtc)
    BarrierNames = InfoPars
    Valbar = -999.0

    ! Send OK status due to barrier names
    SignalXValues(1,1) = 1
    call DioPltPut(SignalRtcToFlow, SignalXValues)

    ! Cleanup Info-stream did its work
    call DioPltDestroy(InfoFlowToRtc)

    ! Setup the actual data-stream to Flow
    DataRtcToFlow = DioPltDefine('DataToFlow', Dio_Plt_Real, BarrierNames, Locbar)

    ! Check if flow received the barrier names
    if (DioPltGet(SignalFlowToRtc, SignalRValues)) then
      FlowStatus = SignalRValues(1,1)
    else
      FlowStatus = -1
    endif

    if (FlowStatus .lt. 0) then
      lerror = .true.
    endif

  endif
  
end subroutine SyncRtcFlow_Init


subroutine SyncRtcFlow_Get(istat, idate, itime)

  ! Routine to get flow status, actual date and time from Flow.
  ! If the flow status is < 0, then RTC will quit.

  ! ISTAT   O   L*4                  Status, < 0 tells RTC to quit.
  ! IDATE   O   I*4                  Actual date (YYYYMMDD)
  ! ITIME   O   I*4                  Actual time (HHMMSS)

  integer istat, idate, itime

  ! Get from Flow
  if (DioPltGet(SignalFlowToRtc, SignalRValues)) then
    ! Retrieve data from R-Array
    istat = SignalRValues(1,1)
    idate = SignalRValues(1,2)
    itime = SignalRValues(1,3)
  else
    istat = -1
  endif

end subroutine SyncRtcFlow_Get


! Sends calculated dat to Flow
subroutine SyncRtcFlow_Send(RtcStatus)

  integer :: RtcStatus
  integer :: i, itemp    ! Loop counter

  ! First send status through signal
  SignalXValues(1,1) = RtcStatus
  call DioPltPut(SignalRtcToFlow, SignalXValues)

  ! If RtcStatus < 0, sending data is not necessary, because
  ! Flow will quit
  if (RtcStatus .ge. 0) then
    ! Convert/copy data
    itemp = 933
    open(itemp,file='rtc_d3d.dat',position='append')
    write(itemp,*) ' put data for timestep X for ', numBarriers, ' locations'
    do i = 1, numBarriers
      DataValuesOut(i, 1) = ValBar(1, i)
      DataValuesOut(i, 2) = ValBar(2, i)
      write(itemp,*) ' isluv    datavalues ', i, DataValuesOut(i,1), DataValuesOut(i,2)
    enddo
    call DioPltPut(DataRtcToFlow, DataValuesOut)
    close(itemp)
  endif

end subroutine SyncRtcFlow_Send


subroutine SyncRtcFlow_Close

  ! Close open streams
  call DioPltDestroy(DataRtcToFlow)
  call DioPltDestroy(SignalFlowToRtc)
  call DioPltDestroy(SignalRtcToFlow)

end subroutine SyncRtcFlow_Close

!!!
!!! FUNCTIONS D3DFLOW SIDE
!!!
!==============================================================================
subroutine syncflowrtc_quit
    use precision
! Shut down RTC in case of 'early' error
!
    implicit none
!
!! executable statements -------------------------------------------------------
!
    ! Init DelftIO
    call dioinit
    !
    ! Signal from Flow to RTC
    ! To send the shutdown message
    signalflowtortc = diopltdefine('SignalToRtc', dio_plt_integer, signalxpars, &
                    & signalxlocs)
    ! Send shutdown code to RTC
    signalxvalues(1, 1) = -1
    signalxvalues(1, 2) = 1
    signalxvalues(1, 3) = 2
    !
    call diopltput(signalflowtortc, signalxvalues)
    ! Close stream
    call diopltdestroy(signalflowtortc)
end subroutine syncflowrtc_quit
!
!
!
!==============================================================================
subroutine syncflowrtc_init(error, nambar, nsluv, charlen, nsteps, commMode)
    use precision
! Initialise communication between Flow and RTC
!
    implicit none
!
! Global variables
!
    integer                        ,intent (in)  :: charlen
    integer                        ,intent (in)  :: nsluv
    integer                        ,intent (in)  :: nsteps
    integer                        ,intent (in)  :: commMode
    logical                        ,intent (out) :: error
    character(charlen), dimension(nsluv)         :: nambar ! WARNING: both charlen and nsluv must be passed via parameter list for Intel 9.0
!
! Local variables
!
    integer                         :: ndatapars
    integer                         :: nrloc
    integer                         :: nrpar
    integer                         :: rtcstatus
    character(20), dimension(1)     :: dummylocs
    type (dioplttype)               :: infoflowtortc
!
    data dummylocs/'Dummy'/
!
!! executable statements -------------------------------------------------------
!
    ! Init DelftIO
    call dioinit!

    ! store commMode
    communicationMode = commMode

    !
    ! Signal from Flow to RTC
    ! First to send number of (half) time steps
    ! Then to send status (< 0 = Quit), Date (I8) and Time (I6)
    signalflowtortc = diopltdefine('SignalToRtc', dio_plt_integer, signalxpars, &
                    & signalxlocs)
    ! Send number of half time steps to RTC
    signalxvalues(1, 1) = nsteps*2
    signalxvalues(1, 2) = 1
    signalxvalues(1, 3) = 2
    !
    call diopltput(signalflowtortc, signalxvalues)
    !
    ! Check whether the communication mode requires barrier heights to be received from RTC
    !
    if (communicationMode == commBarriers) then
       !
       ! Signal from RTC to Flow
       ! Then to receive status (< 0 = Quit), Date (I8) and Time (I6)
       ! Status used for shutdown, date and time can be used for checking.
       signalrtctoflow = diopltgetdataset('SignalToFlow')
       nrpar = diopltgetnpar(signalrtctoflow)
       nrloc = diopltgetnloc(signalrtctoflow)
       !
       if (diopltget(signalrtctoflow, signalrvalues)) then
          rtcstatus = signalrvalues(1, 1)
       else
          rtcstatus = -1
       endif
       !
       if (rtcstatus<0) then
          error = .true.
          return
       endif
       !
       ! Setup stream for sending barrier names
       !
       infoflowtortc = diopltdefine('InfoToRtc', dio_plt_integer, nambar,          &
                     & dummylocs)
       ! Check if message came through
       if (diopltget(signalrtctoflow, signalrvalues)) then
          rtcstatus = signalrvalues(1, 1)
       else
          rtcstatus = -1
       endif
       ! Info-stream did its work and not necessary any more
       call diopltdestroy(infoflowtortc)
       !
       if (rtcstatus<0) then
          error = .true.
          return
       endif
       ! Get the actual data-stream to Flow
       datartctoflow = diopltgetdataset('DataToFlow')
       ndatapars = diopltgetnpar(datartctoflow)
       ! Check on right number of parameters
       if (ndatapars==nsluv) then
          error = .false.
          signalxvalues(1, 1) = 1
       else
          error = .true.
          signalxvalues(1, 1) = -1
       endif
       ! Send status to RTC
       call diopltput(signalflowtortc, signalxvalues)
       !
    endif

end subroutine syncflowrtc_init
!
!
!
!==============================================================================
subroutine syncflowrtc_send(istat, idate, itime)
    use precision
! Routine to send flow status, actual date and time to RTC.
! If the flow status is < 0, then RTC will quit.
!
    implicit none
!
! Global variables
!
    integer,intent (in) :: idate   ! Actual date (YYYYMMDD)
    integer,intent (in) :: istat   ! Status, < 0 tells RTC to quit.
    integer,intent (in) :: itime   ! Actual time (HHMMSS)
!
!! executable statements -------------------------------------------------------
!
    ! Put data in X-Array
    signalxvalues(1, 1) = istat
    signalxvalues(1, 2) = idate
    signalxvalues(1, 3) = itime
    ! Send to RTC
    call diopltput(signalflowtortc, signalxvalues)
    !
end subroutine syncflowrtc_send
!
!
!
!==============================================================================
subroutine syncflowrtc_get(rtcstatus, cbuvrt, nsluv)
    use precision
! Routine to get RTC status and the barrier data from RTC.
! If the flow status is < 0, then RTC will quit.
    implicit none
!
! Global variables
!
    integer                        ,intent (in)  :: nsluv     ! Number of U- and V-Barriers
    integer                                      :: rtcstatus ! Status sent from RTC, < 0 tells Flow to quit.
    real(fp), dimension(2, nsluv), intent (out)  :: cbuvrt    ! Run time barrier data:
                                                              ! CBUVRT(1,*) = Return status from RTC
                                                              !             > 0 : OK
                                                              !             < 0 : Not OK/Found
!
! Local variables
!
    integer :: i
!
!! executable statements -------------------------------------------------------
!
    ! Get status from RTC
    if (diopltget(signalrtctoflow, signalrvalues)) then
                  ! Loop counter
       rtcstatus = signalrvalues(1, 1)
    else
       rtcstatus = -5
    endif
    !
    ! If RtcStatus < 0, data is not sent, because
    ! Flow will quit on bad status
    if (rtcstatus>=0) then
       if (diopltget(datartctoflow, DataValuesIn)) then
          do i = 1, nsluv
             cbuvrt(1, i) = real(DataValuesIn(i, 1),fp)
             cbuvrt(2, i) = real(DataValuesIn(i, 2),fp)
          enddo
       else
          ! No valid data received
          rtcstatus = -5
       endif
    endif
    !
end subroutine syncflowrtc_get
!
!
!
!==============================================================================
subroutine syncflowrtc_close
    use precision
!
    implicit none
!
!! executable statements -------------------------------------------------------
!
    ! Close open streams
    call diopltdestroy(signalflowtortc)
    if ( communicationMode == commBarriers ) then
        call diopltdestroy(signalrtctoflow)
        call diopltdestroy(datartctoflow)
    endif
    !
end subroutine syncflowrtc_close
!
!
!
!==============================================================================
subroutine datatortc(timsec, ifirstrtc, tnparput, tparput,  &
                   & tparput_names, success)
    use precision
    use dio_plt_rw
    !
    implicit none
!
! Global variables
!
    integer             :: ifirstrtc
    integer, intent(in) :: tnparput
    real(fp)                            , intent(in)  :: timsec
    real(fp), dimension(2,tnparput)     , intent(in)  :: tparput
    character(80), dimension(tnparput)  , intent(in)  :: tparput_names
    logical                             , intent(out) :: success
!
! Local variables
!
    integer                                           :: i
    integer                                           :: k
    integer                                           :: istat
    real(sp)               , pointer, dimension(:, :) :: datavaluesToRTC
    character(256)                                    :: filename
    character(DioMaxParLen), pointer, dimension(:)    :: parNames      ! variable(s) in dataset
    character(DioMaxLocLen), pointer, dimension(:)    :: locNames
    type (dioplttype)                                 :: dataflowtortc
!
!! executable statements -------------------------------------------------------
!
   success = .false.
   !
   ! One would like to write the header only once, instead of for each half time step
   ! Not possible yet, due to creating and deleting of 'TMP_FLOWtoRTC.his'-file
   !
   if (.true.) then
      ifirstrtc = 0
      filename = 'TMP_FLOWtoRTC.his'
      allocate(parNames(2),stat=istat)
      if (istat == 0) allocate(locNames(tnparput), stat=istat)
      if (istat /= 0) goto 99
      !
      parNames(1) = 'Elevation'
      parNames(2) = 'Salinity'
      locNames = tparput_names
      !
      dataflowtortc = DioPltDefine(trim(filename), Dio_Plt_Real, parNames, locNames)
      deallocate(parNames)
      deallocate(locNames)
   endif
   !
   allocate (datavaluesToRTC(2,tnparput), stat=istat)
   if (istat /= 0) goto 99
   !
   do i = 1,tnparput
      do k = 1,2
         datavaluesToRTC(k,i) = real(tparput(k,i),sp)
      enddo
   enddo
   call DioPltPut (dataflowtortc, nint(timsec), datavaluesToRTC )
   call DioPltDestroy (dataflowtortc)
   success = .true.
   deallocate (datavaluesToRTC)
   return
   !
99 continue
   write(*,*) 'ERROR: Memory allocation error in datatortc'
end subroutine datatortc



end module SyncRtcFlow

