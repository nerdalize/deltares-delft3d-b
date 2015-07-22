subroutine rtc_comm_get(cursec    ,cbuvrt    ,nsluv     ,gdp       )
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
!  $Id: rtc_comm_get.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/timedep/rtc_comm_get.f90 $
!!--description-----------------------------------------------------------------
!
! This routine receives data from the RTC module
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use timers
    use SyncRtcFlow
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                       , pointer :: ifirstrtc
    integer                       , pointer :: julday
    integer                       , pointer :: kmax
    integer                       , pointer :: lsal
    integer                       , pointer :: lundia
    integer      , dimension(:,:) , pointer :: mnrtcsta
    integer                       , pointer :: parget_offset
    integer                       , pointer :: rtc_domainnr
    integer                       , pointer :: rtc_ndomains
    logical                       , pointer :: rtcact
    integer                       , pointer :: rtcmod
    integer                       , pointer :: stacnt
    real(fp)                      , pointer :: timsec
    integer                       , pointer :: tnparget
    integer                       , pointer :: tnparput
    real(fp)     , dimension(:,:) , pointer :: tparget
    character(80), dimension(:)   , pointer :: tparget_names
    real(fp)     , dimension(:,:) , pointer :: zrtcsta
!
! Global variables
!
    integer                      , intent(in) :: nsluv  ! number of barriers
    real(fp)                     , intent(in) :: cursec ! Current simulation time in seconds 
    real(fp), dimension(2, nsluv)             :: cbuvrt ! Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                :: iacdat      ! Actual simulation day for RTC 
    integer                                :: iactim      ! Actual simulation time for RTC 
    integer                                :: id
    integer                                :: rtcsta      ! Status from RTC: If < 0, RTC quits And Flow also must quit. 
!
!! executable statements -------------------------------------------------------
!
    ifirstrtc      => gdp%gdrtc%ifirstrtc
    julday         => gdp%gdinttim%julday
    kmax           => gdp%d%kmax
    lsal           => gdp%d%lsal
    lundia         => gdp%gdinout%lundia
    mnrtcsta       => gdp%gdrtc%mnrtcsta
    parget_offset  => gdp%gdrtc%parget_offset
    rtc_domainnr   => gdp%gdrtc%rtc_domainnr
    rtc_ndomains   => gdp%gdrtc%rtc_ndomains
    rtcact         => gdp%gdrtc%rtcact
    rtcmod         => gdp%gdrtc%rtcmod
    stacnt         => gdp%gdrtc%stacnt
    timsec         => gdp%gdinttim%timsec
    tnparget       => gdp%gdrtc%tnparget
    tnparput       => gdp%gdrtc%tnparput
    tparget        => gdp%gdrtc%tparget
    tparget_names  => gdp%gdrtc%tparget_names
    zrtcsta        => gdp%gdrtc%zrtcsta
    !
    ! FLOW -> RTC  : send current date and time
    ! RTC  -> FLOW : get steering parameters for current date and time
    !
    call timdat(julday, cursec, iacdat, iactim)
    !
    ! send current date and time to RTC
    !
    call timer_start(timer_wait, gdp)
    if (rtc_domainnr == 1) then
       call syncflowrtc_send(1, iacdat, iactim)
    endif
    !
    ! optionally get data back
    !
    rtcsta = 0
    if (rtcmod == dataFromRTCToFLOW) then
       !
       ! communication with RTC occurs only by the master domain
       !
       if (rtc_domainnr == 1) then
          call syncflowrtc_get(rtcsta, tparget, tnparget)
       else
          tparget = 0.0_fp
       endif
       !
       ! distribute data to all domains
       !
       if (rtc_ndomains > 1) then
          call rtccommunicate(tparget, 2*tnparget)
       endif
       !
       ! copy data to local array
       !
       do id = 1, nsluv
          cbuvrt(:,id) = tparget(:,parget_offset+id)
       enddo
    endif
    call timer_stop(timer_wait, gdp)
    !
    if (rtcsta < 0) then
       rtcact = .false.
       write (*, '(a)') ' '
       write (*, '(a)') ' Stop signal from RTC '
       call prterr(lundia, 'P004', 'Stop signal from RTC ')
       call d3stop(1, gdp)
    endif
end subroutine rtc_comm_get
