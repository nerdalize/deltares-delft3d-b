subroutine rtc_comm_init(error     ,nambar    ,kfs       ,kfsmin    , &
                       & kfsmax    ,sig       ,zk        ,s1        , &
                       & dps       ,r0        ,gdp       )
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
!  $Id: rtc_comm_init.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/timedep/rtc_comm_init.f90 $
!!--description-----------------------------------------------------------------
!
! This routine initializes the communication with the RTC module
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
    integer                       , pointer :: itfinish
    integer                       , pointer :: itstrt
    integer                       , pointer :: kmax
    integer                       , pointer :: lsal
    integer                       , pointer :: lundia
    integer                       , pointer :: mlb
    integer      , dimension(:,:) , pointer :: mnrtcsta
    integer                       , pointer :: mub
    character(20), dimension(:)   , pointer :: namrtcsta
    integer                       , pointer :: nlb
    integer                       , pointer :: nsluv
    integer                       , pointer :: nub
    integer                       , pointer :: numdomains
    integer                       , pointer :: parget_offset
    integer                       , pointer :: parput_offset
    integer                       , pointer :: rtc_domainnr
    integer                       , pointer :: rtc_ndomains
    logical                       , pointer :: rtcact
    integer                       , pointer :: rtcmod
    integer                       , pointer :: stacnt
    real(fp)                      , pointer :: timsec
    integer                       , pointer :: tnparget
    integer                       , pointer :: tnparput
    real(fp)     , dimension(:,:) , pointer :: tparput
    character(80), dimension(:)   , pointer :: tparput_names
    character(80), dimension(:)   , pointer :: tparget_names
    real(fp)     , dimension(:,:) , pointer :: zrtcsta
!
! Global variables
!
    integer       , dimension(gdp%d%nmlb:gdp%d%nmub)                                             , intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nmlb:gdp%d%nmub)                                             , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nmlb:gdp%d%nmub)                                             , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(prec)    , dimension(gdp%d%nmlb:gdp%d%nmub)                                             , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, gdp%d%kmax, gdp%d%lstsci), intent(in) :: r0
    real(fp)      , dimension(gdp%d%nmlb:gdp%d%nmub)                                             , intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(  gdp%d%kmax)                                                      , intent(in) :: sig
    real(fp)      , dimension(0:gdp%d%kmax)                                                      , intent(in) :: zk
    logical                                                                                                   :: error  ! Flag=TRUE if an error is encountered 
    character(20) , dimension(gdp%d%nsluv)                                                                    :: nambar ! names of all parameters to get from RTC
!
! Local variables
!
    integer                           :: i
    integer                           :: iloc
    integer                           :: istat
    integer                           :: k
    integer                           :: n
    real(fp), dimension(:,:), pointer :: nparams
    logical                           :: success      ! Flag = false when an error is encountered
!
!! executable statements -------------------------------------------------------
!
    ifirstrtc      => gdp%gdrtc%ifirstrtc
    itfinish       => gdp%gdinttim%itfinish
    itstrt         => gdp%gdinttim%itstrt
    kmax           => gdp%d%kmax
    lsal           => gdp%d%lsal
    lundia         => gdp%gdinout%lundia
    mlb            => gdp%d%mlb
    mnrtcsta       => gdp%gdrtc%mnrtcsta
    mub            => gdp%d%mub
    namrtcsta      => gdp%gdrtc%namrtcsta
    nlb            => gdp%d%nlb
    nsluv          => gdp%d%nsluv
    nub            => gdp%d%nub
    numdomains     => gdp%gdprognm%numdomains
    parget_offset  => gdp%gdrtc%parget_offset
    parput_offset  => gdp%gdrtc%parput_offset
    rtc_domainnr   => gdp%gdrtc%rtc_domainnr
    rtc_ndomains   => gdp%gdrtc%rtc_ndomains
    rtcact         => gdp%gdrtc%rtcact
    rtcmod         => gdp%gdrtc%rtcmod
    stacnt         => gdp%gdrtc%stacnt
    timsec         => gdp%gdinttim%timsec
    tnparget       => gdp%gdrtc%tnparget
    tnparput       => gdp%gdrtc%tnparput
    tparput        => gdp%gdrtc%tparput
    tparput_names  => gdp%gdrtc%tparput_names
    tparget_names  => gdp%gdrtc%tparget_names
    zrtcsta        => gdp%gdrtc%zrtcsta
    !
    if (rtcmod == dataFromRTCToFLOW .or. rtcmod == dataFromFLOWToRTC) then
      if (rtcmod == dataFromFLOWToRTC) then
         !
         ! Put z coordinates in array rtcsta
         !
         call zrtc(mlb, mub, nlb, nub, kfs, kfsmin, &
                 & kfsmax, sig, zk, s1, dps, kmax, gdp)
      endif
      !
      call timer_start(timer_wait, gdp)
      if (numdomains > 1) then
         call rtcstartcommunication(rtc_domainnr, rtc_ndomains)
         rtc_domainnr = rtc_domainnr+1
      else
         rtc_domainnr = 1
      endif
      !
      ! Determine total number of parameters to be received
      ! and collect all parameter names
      !
      if (rtc_ndomains > 1) then
         allocate(nparams(2,rtc_ndomains),stat=istat)
         if (istat /= 0) goto 999
         nparams = 0.0_fp
         !
         nparams(1,rtc_domainnr) = real(nsluv,fp) ! # parameters get
         nparams(2,rtc_domainnr) = real(stacnt*kmax,fp) ! # parameters put
         !
         call rtccommunicate(nparams, 2*rtc_ndomains)
         !
         tnparget = 0
         tnparput = 0
         parget_offset = 0
         parput_offset = 0
         do i = 1, rtc_ndomains
            n = nint(nparams(1,i))
            tnparget = tnparget + n
            if (i<rtc_domainnr) parget_offset = parget_offset + n
            !
            n = nint(nparams(2,i))
            tnparput = tnparput + n
            if (i<rtc_domainnr) parput_offset = parput_offset + n
         enddo
         deallocate(nparams,stat=istat)
         if (istat /= 0) goto 999
      else
         tnparget = nsluv
         parget_offset = 0
         tnparput = stacnt * kmax
         parput_offset = 0
      endif
      !
      allocate(gdp%gdrtc%tparget(2,tnparget),stat=istat)
      if (istat /= 0) goto 999
      allocate(gdp%gdrtc%tparget_names(tnparget),stat=istat)
      if (istat /= 0) goto 999
      tparget_names => gdp%gdrtc%tparget_names
      tparget_names = ' '
      do i = 1,nsluv
         tparget_names(parget_offset+i) = nambar(i)
      enddo
      !
      if (rtc_ndomains > 1) then
         call rtccharcommunicate(tparget_names, tnparget)
      endif
      !
      if (rtcmod == dataFromFLOWToRTC) then
         !
         ! Collect parameters for this domain
         !
         allocate(gdp%gdrtc%tparput(2,tnparput),stat=istat)
         if (istat /= 0) goto 999
         allocate(gdp%gdrtc%tparput_names(tnparput),stat=istat)
         if (istat /= 0) goto 999
         tparput => gdp%gdrtc%tparput
         tparput_names => gdp%gdrtc%tparput_names
         tparput = 0.0_fp
         tparput_names = ' '
         !
         iloc = parput_offset
         do i = 1,stacnt
            do k = 1,kmax
               iloc = iloc + 1
               write(tparput_names(iloc),'(2a,i0)') trim(namrtcsta(i)), '_', k
            enddo
         enddo
         !
         iloc = parput_offset
         do i = 1,stacnt
            do k = 1,kmax
               iloc = iloc + 1
               tparput(1,iloc) = zrtcsta(k,i)
               if (lsal>0) then
                  tparput(2,iloc) = r0(mnrtcsta(2,i), mnrtcsta(1,i), k, lsal)
               endif
            enddo
         enddo
         !
         ! Collect parameters from all domains
         !
         if (rtc_ndomains>1) then
            call rtccharcommunicate(tparput_names, tnparput)
            call rtccommunicate(tparput, 2*tnparput)
         endif
         !
         ! Communication with RTC occurs only by the master domain
         !
         if (rtc_domainnr == 1) then
            !
            ! Write parameters to his-file
            !
            call datatortc(timsec, ifirstrtc, tnparput, tparput, &
                         & tparput_names, success)
            call syncflowrtc_init(error, tparget_names, tnparget, 80, itfinish - itstrt, commSalinity)
         endif
      else
         !
         ! Communication with RTC occurs only by the master domain
         !
         if (rtc_domainnr == 1) then
            call syncflowrtc_init(error, tparget_names, tnparget, 80, itfinish - itstrt, commBarriers)
         endif
      endif
      call timer_stop(timer_wait, gdp)
      if (error) then
         rtcact = .false.
         call prterr(lundia    ,'J020'    ,'SyncFlowRtc_Init'   )
      else
         rtcact = .true.
      endif
    else
      if (numdomains > 1) then
         !
         ! Notify the rtc iterator that this subdomain
         ! is not interested in rtc communication
         ! If numdomains=1, there is no rtc iterator
         !
         call rtcnocommunication()
      endif
    endif
    return
    !
999 continue
    call prterr(lundia    ,'P004'    ,'Memory allocation error in RTC_COMM_INIT'   )
end subroutine rtc_comm_init
