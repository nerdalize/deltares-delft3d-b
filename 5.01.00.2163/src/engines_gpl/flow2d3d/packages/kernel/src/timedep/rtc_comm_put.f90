subroutine rtc_comm_put(kfs       ,kfsmin    ,kfsmax    ,sig       , &
                      & zk        ,s1        ,dps       ,r0        , &
                      & gdp       )
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
!  $Id: rtc_comm_put.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/timedep/rtc_comm_put.f90 $
!!--description-----------------------------------------------------------------
!
! This routine sends data to the RTC module
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
    integer                       , pointer :: kmax
    integer                       , pointer :: lsal
    integer      , dimension(:,:) , pointer :: mnrtcsta
    integer                       , pointer :: parput_offset
    integer                       , pointer :: rtc_domainnr
    integer                       , pointer :: rtc_ndomains
    integer                       , pointer :: rtcmod
    integer                       , pointer :: stacnt
    real(fp)                      , pointer :: timsec
    integer                       , pointer :: tnparput
    real(fp)     , dimension(:,:) , pointer :: tparput
    character(80), dimension(:)   , pointer :: tparput_names
    real(fp)     , dimension(:,:) , pointer :: zrtcsta
!
! Global variables
!
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, gdp%d%kmax, gdp%d%lstsci), intent(in)  :: r0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(  gdp%d%kmax)         , intent(in) :: sig
    real(fp)  , dimension(0:gdp%d%kmax)         , intent(in) :: zk
!
! Local variables
!
    integer                                :: i
    integer                                :: iloc
    integer                                :: k
    logical                                :: success
!
!! executable statements -------------------------------------------------------
!
    ifirstrtc      => gdp%gdrtc%ifirstrtc
    kmax           => gdp%d%kmax
    lsal           => gdp%d%lsal
    mnrtcsta       => gdp%gdrtc%mnrtcsta
    parput_offset  => gdp%gdrtc%parput_offset
    rtc_domainnr   => gdp%gdrtc%rtc_domainnr
    rtc_ndomains   => gdp%gdrtc%rtc_ndomains
    rtcmod         => gdp%gdrtc%rtcmod
    stacnt         => gdp%gdrtc%stacnt
    timsec         => gdp%gdinttim%timsec
    tnparput       => gdp%gdrtc%tnparput
    tparput        => gdp%gdrtc%tparput
    tparput_names  => gdp%gdrtc%tparput_names
    zrtcsta        => gdp%gdrtc%zrtcsta
    !
    ! FLOW -> RTC  : send parameters (salinity levels)
    ! RTC  -> FLOW : no communication
    !
    if (rtcmod == dataFromFLOWToRTC) then
       call zrtc(gdp%d%mlb, gdp%d%mub, gdp%d%nlb, gdp%d%nub, kfs, kfsmin, &
               & kfsmax, sig, zk, s1, dps, kmax, gdp)
       !
       ! Collect parameters for this domain
       !
       tparput = 0.0_fp
       iloc = parput_offset
       do i = 1,stacnt
          do k = 1,kmax
             iloc = iloc + 1
             tparput(1,iloc) = zrtcsta(k,i)
             tparput(2,iloc) = r0(mnrtcsta(2,i), mnrtcsta(1,i), k, lsal)
          enddo
       enddo
       !
       ! Collect parameters from all domains
       !
       if (rtc_ndomains>1) then
          call rtccommunicate(tparput, 2*tnparput)
       endif
       !
       ! Communication with RTC occurs only by the master domain
       !
       call timer_start(timer_wait, gdp)
       if (rtc_domainnr == 1) then
          call datatortc(timsec, ifirstrtc, tnparput, tparput, &
                       & tparput_names, success)
       endif
       call timer_stop(timer_wait, gdp)
    endif

end subroutine rtc_comm_put
