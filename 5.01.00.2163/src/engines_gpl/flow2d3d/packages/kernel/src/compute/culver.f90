subroutine culver(icx       ,icy       ,kmax      ,nsrc      ,kfs       , &
                & kfsmax    ,kfsmin    ,mnksrc    ,disch     ,dps       , &
                & s0        ,zk        ,thick     ,voldis    ,timsec    , &
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
!  $Id: culver.f90 1249 2012-02-13 15:35:17Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/culver.f90 $
!!--description-----------------------------------------------------------------
! Computes the discharge relation through a culvert.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)      , dimension(:)   , pointer :: arcul
    real(fp)      , dimension(:)   , pointer :: calfa
    real(fp)      , dimension(:)   , pointer :: clcul
    real(fp)      , dimension(:)   , pointer :: cleng
    real(fp)      , dimension(:,:) , pointer :: closs1
    real(fp)      , dimension(:,:) , pointer :: closs2
    real(fp)      , dimension(:,:) , pointer :: closs3
    real(fp)      , dimension(:)   , pointer :: cmann
    real(fp)      , dimension(:)   , pointer :: htcul
    integer       , dimension(:)   , pointer :: numrel1
    integer       , dimension(:)   , pointer :: numrel2
    integer       , dimension(:)   , pointer :: numrel3
    real(fp)      , dimension(:)   , pointer :: poscul
    real(fp)      , dimension(:,:) , pointer :: wetar1
    real(fp)      , dimension(:,:) , pointer :: wetar2
    real(fp)      , dimension(:,:) , pointer :: wetar3
    real(fp)      , dimension(:)   , pointer :: wtcul
    integer                        , pointer :: lundia
    real(fp)                       , pointer :: hdt
    real(fp)                       , pointer :: ag
    logical                        , pointer :: culvert
    logical                        , pointer :: zmodel
!
! Global variables
!
    integer                                     , intent(in)  :: icx    ! Increment in the X-dir., if ICX= NMAX
                                                                        ! then computation proceeds in the X-
                                                                        ! dir. If icx=1 then computation pro-
                                                                        ! ceeds in the Y-dir.    
    integer                                     , intent(in)  :: icy    ! Increment in the Y-dir. (see ICX)
    integer                                     , intent(in)  :: nsrc   ! Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfs    ! Description and declaration in esm_alloc_int.f90   
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfsmax ! Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfsmin ! Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: kmax    
    integer, dimension(7, nsrc)                               :: mnksrc ! Description and declaration in r-i-ch.igs
    real(fp)                                    , intent(in)  :: timsec ! Time in seconds since reference date
    real(fp), dimension(nsrc)                   , intent(out) :: disch  ! Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps    ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: s0     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                   , intent(in)  :: thick  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                 :: voldis ! Description and declaration in esm_alloc_real.f90   
    real(fp), dimension(0:kmax)                 , intent(in)  :: zk     ! See description and declaration of sig in esm_alloc_real.f90
!
! Local variables
!
    integer           :: ddb
    integer           :: i
    integer           :: icxy
    integer           :: iflow
    integer, external :: intlay
    integer           :: isrc
    integer           :: iswitch
    integer           :: k
    integer           :: nmin
    integer           :: nmin2
    integer           :: nmout
    integer           :: nmout2
    real(fp)          :: area
    real(fp)          :: cc1
    real(fp)          :: cd1
    real(fp)          :: cd2
    real(fp)          :: cd3
    real(fp)          :: coefl
    real(fp)          :: delzet ! Water level gradient
    real(fp)          :: h0
    real(fp)          :: height
    real(fp)          :: hin
    real(fp)          :: hout
    real(fp)          :: pos
    real(fp)          :: rdis
    real(fp)          :: zz1
    real(fp)          :: zz2
    real(fp)          :: zdown    
    real(fp)          :: zup

    ! Interface to dll is in High precision!
    !
    real(hp)                    :: disch_dll
    real(hp)                    :: pos1_dll
    real(hp)                    :: pos2_dll
    real(hp)                    :: rmissval
    integer(pntrsize)           :: error_ptr
    integer(pntrsize), external :: perf_function_culvert
    character(256)              :: errmsg
    character(256)              :: message     ! Contains message from
    !
    integer                        , pointer :: max_integers
    integer                        , pointer :: max_reals
    integer                        , pointer :: max_strings
    character(256)   , dimension(:), pointer :: dll_function
    character(256)   , dimension(:), pointer :: dll_usrfil
    integer(pntrsize), dimension(:), pointer :: dll_handle
    integer          , dimension(:), pointer :: dll_integers
    real(hp)         , dimension(:), pointer :: dll_reals
    character(256)   , dimension(:), pointer :: dll_strings
!
!! executable statements -------------------------------------------------------
!
    arcul            => gdp%gdculver%arcul
    calfa            => gdp%gdculver%calfa
    clcul            => gdp%gdculver%clcul
    cleng            => gdp%gdculver%cleng
    closs1           => gdp%gdculver%closs1
    closs2           => gdp%gdculver%closs2
    closs3           => gdp%gdculver%closs3
    cmann            => gdp%gdculver%cmann
    htcul            => gdp%gdculver%htcul
    numrel1          => gdp%gdculver%numrel1
    numrel2          => gdp%gdculver%numrel2
    numrel3          => gdp%gdculver%numrel3
    poscul           => gdp%gdculver%poscul
    wetar1           => gdp%gdculver%wetar1
    wetar2           => gdp%gdculver%wetar2
    wetar3           => gdp%gdculver%wetar3
    wtcul            => gdp%gdculver%wtcul
    lundia           => gdp%gdinout%lundia
    hdt              => gdp%gdnumeco%hdt
    ag               => gdp%gdphysco%ag
    culvert          => gdp%gdprocs%culvert
    zmodel           => gdp%gdprocs%zmodel
    !
    max_integers        => gdp%gdculver%max_integers
    max_reals           => gdp%gdculver%max_reals
    max_strings         => gdp%gdculver%max_strings
    dll_function        => gdp%gdculver%dll_function
    dll_usrfil          => gdp%gdculver%dll_usrfil
    dll_handle          => gdp%gdculver%dll_handle
    dll_integers        => gdp%gdculver%dll_integers
    dll_reals           => gdp%gdculver%dll_reals
    dll_strings         => gdp%gdculver%dll_strings
    !
    ! Statics
    !
    ddb      = gdp%d%ddbound
    icxy     = max(icx, icy)
    rmissval = -1.0e20_hp
    do isrc = 1, nsrc
       !
       ! Discharge relation through culvert (completely submerged culvert)
       ! for "normal" culvert: MNKSRC(7,.) = 3
       !
       if (mnksrc(7, isrc)==3) then
          call n_and_m_to_nm(mnksrc(2,isrc), mnksrc(1,isrc), nmin, gdp)
          call n_and_m_to_nm(mnksrc(5,isrc), mnksrc(4,isrc), nmout, gdp)
          if (kfs(nmin)==1) then
             delzet = s0(nmin) - s0(nmout)
             if (delzet>0.0) then
                disch(isrc) = clcul(isrc)*arcul(isrc)*sqrt(2.*ag*delzet)
             else
                disch(isrc) = 0.0
             endif
          endif
       endif
    enddo
    !
    ! Discharge relation through culvert 
    ! for "WL Borgerhout" culverts: MNKSRC(7,.) = 4 or 5
    ! MNKSRC(7,.) = 4: one-way culvert
    ! MNKSRC(7,.) = 5: two-way culvert
    !
    ! User defined discharge relation for culvert 7.
    !
    do isrc = 1, nsrc
       if ( (mnksrc(7,isrc) == 4) .or. &
          & (mnksrc(7,isrc) == 5) ) then
          call n_and_m_to_nm(mnksrc(2,isrc), mnksrc(1,isrc), nmin, gdp)
          call n_and_m_to_nm(mnksrc(5,isrc), mnksrc(4,isrc), nmout, gdp)
          !
          ! for intake point:
          !
          mnksrc(3,isrc) = intlay(lundia      ,zmodel    ,kfsmin(nmin) ,kfsmax(nmin) , &
                                & poscul(isrc),zk        ,dps(nmin)    ,s0(nmin)     , &
                                & kmax        ,thick     ,isrc         ,'intake '    )
          !
          ! for outfall point:
          !
          mnksrc(6,isrc) = intlay(lundia      ,zmodel    ,kfsmin(nmout),kfsmax(nmout), &
                                & poscul(isrc),zk        ,dps(nmout)   ,s0(nmout)    , &
                                & kmax        ,thick     ,isrc         ,'outfall'    )
          !
          ! compute loss coefficient:
          !
          area  = 0.0
          coefl = 0.0
          if (kfs(nmin) .eq. 1 .or. kfs(nmout) .eq. 1) then
             hin  = max (0.0_fp,s0(nmin )-poscul(isrc))
             hout = max (0.0_fp,s0(nmout)-poscul(isrc))
             height = 0.5 * ( hin + hout)
             height = min ( height , htcul(isrc) )
             area = height * wtcul(isrc)
             !
             ! compute cd1:
             !
             if ( area <  wetar1(isrc,1) ) cd1 = closs1(isrc,1)
             if ( area >= wetar1(isrc,numrel1(isrc)) ) &
                & cd1 = closs1(isrc,numrel1(isrc))
             do i =2,numrel1(isrc)
                if ( area >  wetar1(isrc,i-1) .and. &
                   & area <= wetar1(isrc,i) ) then
                    zz1 = area - wetar1(isrc,i-1)
                    zz2 = wetar1(isrc,i) - wetar1(isrc,i-1)
                    cc1 = closs1(isrc,i) - closs1(isrc,i-1)
                    cd1 = closs1(isrc,i-1) + zz1 / zz2 * cc1
                endif
             enddo
             !
             ! compute cd2:
             !
             if ( area <= wetar2(isrc,1) ) cd2 = closs2(isrc,1)
             if ( area >= wetar2(isrc,numrel2(isrc)) ) &
                & cd2 = closs2(isrc,numrel2(isrc))
             do i =2,numrel2(isrc)
                if ( area >  wetar2(isrc,i-1) .and. &
                   & area <= wetar2(isrc,i) ) then
                    zz1 = area - wetar2(isrc,i-1)
                    zz2 = wetar2(isrc,i) - wetar2(isrc,i-1)
                    cc1 = closs2(isrc,i) - closs2(isrc,i-1)
                    cd2 = closs2(isrc,i-1) + zz1 / zz2 * cc1
                endif
             enddo
             !
             ! compute cd3:
             !
             if ( area <= wetar3(isrc,1) ) cd3 = closs3(isrc,1)
             if ( area >= wetar3(isrc,numrel3(isrc)) ) &
                & cd3 = closs3(isrc,numrel3(isrc))
             do i =2,numrel3(isrc)
                if ( area >  wetar3(isrc,i-1) .and. &
                   & area <= wetar3(isrc,i) ) then
                   zz1 = area - wetar3(isrc,i-1)
                   zz2 = wetar3(isrc,i) - wetar3(isrc,i-1)
                   cc1 = closs3(isrc,i) - closs3(isrc,i-1)
                   cd3 = closs3(isrc,i-1) + zz1 / zz2 * cc1
                endif
             enddo
          endif
       elseif (mnksrc(7,isrc) == 7) then
          !
          ! compute loss coefficient:
          !
          call n_and_m_to_nm(mnksrc(2,isrc), mnksrc(1,isrc), nmin, gdp)
          call n_and_m_to_nm(mnksrc(5,isrc), mnksrc(4,isrc), nmout, gdp)
          if (kfs(nmin) .eq. 1 .or. kfs(nmout) .eq. 1) then
             !
             ! User defined formula in DLL
             ! Input parameters are passed via dll_reals/integers/strings-arrays
             !
             if (max_reals < 7) then
                write(errmsg,'(a,a,a)') 'Insufficient space to pass real values to culvert library.'
                call prterr (lundia,'U021', trim(errmsg))
                call d3stop(1, gdp)
             endif
             dll_reals( 1) = real(timsec ,hp)
             dll_reals( 2) = real(s0(nmin) ,hp)
             dll_reals( 3) = real(s0(nmout),hp)
             dll_reals( 4) = real(-dps(nmin) ,hp)
             dll_reals( 5) = real(-dps(nmout),hp)
             dll_reals( 6) = real(disch(isrc),hp)
             dll_reals( 7) = real(ag,hp)
             !
             if (max_integers < 8) then
                write(errmsg,'(a,a,a)') 'Insufficient space to pass integer values to culvert library.'
                call prterr (lundia,'U021', trim(errmsg))
                call d3stop(1, gdp)
             endif
             dll_integers( 1) = nmin
             dll_integers( 2) = mnksrc(2,isrc)
             dll_integers( 3) = mnksrc(1,isrc)
             dll_integers( 4) = kfs(nmin)
             dll_integers( 5) = nmout
             dll_integers( 6) = mnksrc(5,isrc)
             dll_integers( 7) = mnksrc(4,isrc)
             dll_integers( 8) = kfs(nmout)
             !
             if (max_strings < 2) then
                write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to culvert library.'
                call prterr (lundia,'U021', trim(errmsg))
                call d3stop(1, gdp)
             endif
             dll_strings( 1) = gdp%runid
             dll_strings( 2) = dll_usrfil(isrc)
             !
             ! Initialisation of output variables of user defined transport formulae
             !
             disch_dll    = 0.0_hp
             pos1_dll     = rmissval
             pos2_dll     = rmissval
             message     = ' '
             call psemlun
             error_ptr = 0
             error_ptr = perf_function_culvert(dll_handle(isrc), dll_function(isrc), &
                                               dll_integers    , max_integers      , &
                                               dll_reals       , max_reals         , &
                                               dll_strings     , max_strings       , &
                                               disch_dll       , pos1_dll          , &
                                               pos2_dll        , message)
             call vsemlun
             if (error_ptr /= 0) then
                write(errmsg,'(a,a,a)') 'Cannot find function "',trim(dll_function(isrc)),'" in dynamic library.'
                call prterr (lundia,'U021', trim(errmsg))
                call d3stop(1, gdp)
             endif
             if (message /= ' ') then
                write (lundia,'(a,a,a)') '*** ERROR Message from user defined culvert formula ',trim(dll_function(isrc)),' :'
                write (lundia,'(a,a  )') '          ', trim(message)
                write (lundia,'(a    )') ' '
                call d3stop(1, gdp)
             endif
             !
             ! Output parameters
             !
             disch(isrc) = real(disch_dll,fp)
             !
             ! Layer for intake point in specific layer or default over full depth (layer = 0)
             !
             if (comparereal(pos1_dll,rmissval) /= 0) then
                pos = real(pos1_dll,fp)
                mnksrc(3,isrc) = intlay(lundia   ,zmodel    ,kfsmin(nmin) ,kfsmax(nmin) , &
                                      & pos      ,zk        ,dps(nmin)    ,s0(nmin)     , &
                                      & kmax     ,thick     ,isrc         ,'intake '    )
             else
                mnksrc(3,isrc) = 0
             endif
             !
             ! Layer for outfall point in specific layer or default over full depth (layer = 0)
             !
             if (comparereal(pos2_dll,rmissval) /= 0) then
                pos = real(pos2_dll,fp)
                mnksrc(6,isrc) = intlay(lundia   ,zmodel    ,kfsmin(nmout),kfsmax(nmout), &
                                      & pos      ,zk        ,dps(nmout)   ,s0(nmout)    , &
                                      & kmax     ,thick     ,isrc         ,'outfall'    )
             else
                mnksrc(6,isrc) = 0
             endif
          endif
       endif
       !
       ! in case of a one-way culvert:
       !
       if (mnksrc(7,isrc) == 4) then
          if (kfs(nmin) .eq. 1) then
             if (s0(nmin) .ge. s0(nmout)) then
                iswitch = 0
                call cptdis(lundia     ,ag         ,area       ,calfa(isrc), &
                          & cd1        ,cd2        ,cd3        ,cleng(isrc), &
                          & cmann(isrc),coefl      ,disch(isrc),htcul(isrc), &
                          & iflow      ,iswitch    ,poscul(isrc),rdis      , &
                          & s0(nmin)   ,s0(nmout)  ,wtcul(isrc) ,gdp       )
                disch(isrc) = rdis
             else
                disch(isrc) = 0.0
             endif
          else
             write(lundia,'(a,i2,a)') &
                & 'intake point of one-way culvert nr.',isrc, &
                & ' is dry'
          endif
       endif
       !
       ! in case of a two-way culvert:
       !
       if (mnksrc(7,isrc) == 5) then
          iswitch = 0
          nmin2   = nmin
          nmout2  = nmout
          if (s0(nmin) .lt. s0(nmout)) then
             !
             ! intake and outfall exchange; recompute area and energy loss
             !
             iswitch = 1
             nmin2   = nmout
             nmout2  = nmin
          endif
          if (kfs(nmin2) .eq. 1) then
                call cptdis(lundia     ,ag         ,area       ,calfa(isrc), &
                          & cd1        ,cd2        ,cd3        ,cleng(isrc), &
                          & cmann(isrc),coefl      ,disch(isrc),htcul(isrc), &
                          & iflow      ,iswitch    ,poscul(isrc),rdis      , &
                          & s0(nmin2)  ,s0(nmout2) ,wtcul(isrc) ,gdp       )
             disch(isrc) = rdis
          else
             write(lundia,'(a,i2,a)') &
                & 'intake point of two-way culvert nr.',isrc, &
                & ' is dry'
          endif
       endif
       !
       voldis(isrc) = voldis(isrc) + disch(isrc)*hdt
    enddo
end subroutine culver


integer function intlay    (lundia     ,zmodel    ,kfsmin    ,kfsmax    , &
                          & pos        ,zk        ,dps       ,s0        , &
                          & kmax       ,thick     ,isrc      ,intake    )
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
!  $Id: culver.f90 1249 2012-02-13 15:35:17Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/culver.f90 $
!!--description-----------------------------------------------------------------
! Computes the discharge relation through a culvert.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Function arguments
!
    logical                                     , intent(in)  :: zmodel ! True = zmodel, False = otherwise (sigma)
    integer                                     , intent(in)  :: isrc   ! Number of culvert
    integer                                     , intent(in)  :: kfsmax ! Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: kfsmin ! Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: kmax   ! Number of layers
    integer                                     , intent(in)  :: lundia ! Diagnostic file
    real(prec)                                  , intent(in)  :: dps    ! Bathymetry [m below ref. level]
    real(fp)                                    , intent(in)  :: pos    ! Level of culvert [m above ref. level]
    real(fp)                                    , intent(in)  :: s0     ! Water level [m above ref. level]
    real(fp), dimension(kmax)                   , intent(in)  :: thick  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                 , intent(in)  :: zk     ! See description and declaration of sig in esm_alloc_real.f90
    character(7)                                , intent(in)  :: intake ! 'intake' or 'outfall'
!
! Local variables
!
    integer           :: k
    real(fp)          :: h0
    real(fp)          :: zdown    
    real(fp)          :: zup
!
!! executable statements -------------------------------------------------------
!
    intlay=0
    if (zmodel) then
       do k=kfsmin, kfsmax
          if (pos >= zk(k-1) .and. pos < zk(k)) intlay = k
       enddo
    else
       h0  = real(dps,fp)+s0
       zup = s0
       do k=1,kmax
          zdown = zup - h0 *thick(k)
          if (pos >= zdown .and. pos < zup) intlay = k
          zup = zdown
       enddo
    endif
    if (pos < -real(dps,fp) ) then
       write(lundia,'(a,a,a,i2,a,f5.2,a,f5.2)') &
          & 'vertical position of culvert ',trim(intake),' nr. ',isrc, &
          & ' (',pos,') is below the bottom ',-real(dps,fp)
    endif
end function intlay
