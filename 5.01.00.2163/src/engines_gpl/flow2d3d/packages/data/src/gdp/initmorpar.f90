subroutine initmorpar(gdp)
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
!  $Id: initmorpar.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/initmorpar.f90 $
!!--description-----------------------------------------------------------------
! NONE
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
    integer                              , pointer :: ihidexp
    integer                              , pointer :: itmor
    integer                              , pointer :: iopkcw
    integer                              , pointer :: iopsus
    integer                              , pointer :: islope
    integer                              , pointer :: morfacpar
    integer                              , pointer :: morfacrec
    integer                              , pointer :: morfactable
    integer                              , pointer :: nxx
    integer                              , pointer :: subiw
    integer                              , pointer :: ttlform
    integer                              , pointer :: telform
    real(hp)                             , pointer :: morft
    real(hp)                             , pointer :: morft0
    real(fp)                             , pointer :: morfac
    real(fp)                             , pointer :: thresh
    real(fp)                             , pointer :: aksfac
    real(fp)                             , pointer :: rwave
    real(fp)                             , pointer :: alfabs
    real(fp)                             , pointer :: alfabn
    real(fp)                             , pointer :: camax
    real(fp)                             , pointer :: dzmax
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: tmor
    real(fp)                             , pointer :: thetsd
    real(fp)                             , pointer :: susw
    real(fp)                             , pointer :: sedthr
    real(fp)                             , pointer :: hmaxth
    real(fp)                             , pointer :: bedw
    real(fp)                             , pointer :: rdc
    real(fp)                             , pointer :: rdw
    real(fp)                             , pointer :: espir
    real(fp)                             , pointer :: ashld
    real(fp)                             , pointer :: bshld
    real(fp)                             , pointer :: cshld
    real(fp)                             , pointer :: dshld
    real(fp)                             , pointer :: coulfri
    real(fp)                             , pointer :: flfdrat
    real(fp)                             , pointer :: alfpa
    real(fp)                             , pointer :: thcrpa
    real(fp)                             , pointer :: asklhe
    real(fp)                             , pointer :: mwwjhe
    real(fp)                             , pointer :: pangle
    real(fp)                             , pointer :: fpco
    real(fp)                             , pointer :: factcr
    real(fp)                             , pointer :: ttlalpha
    real(fp)                             , pointer :: ttlmin
    real(fp)                             , pointer :: wetslope
    real(fp)                             , pointer :: avaltime
    real(fp)              , dimension(:) , pointer :: xx
    !
    real(hp)              , dimension(:) , pointer :: mergebuf
    logical                              , pointer :: bedupd
    logical                              , pointer :: cmpupd
    logical                              , pointer :: eqmbcsand
    logical                              , pointer :: eqmbcmud
    logical                              , pointer :: densin
    logical                              , pointer :: rouse
    logical                              , pointer :: epspar
    logical                              , pointer :: updinf
    logical                              , pointer :: neglectentrainment
    logical                              , pointer :: oldmudfrac
    logical                              , pointer :: varyingmorfac
    logical                              , pointer :: multi
    logical                              , pointer :: eulerisoglm
    logical                              , pointer :: glmisoeuler
    character(256)                       , pointer :: bcmfilnam
    character(256)                       , pointer :: flcomp
    character(256)                       , pointer :: mmsyncfilnam
    character(256)                       , pointer :: ttlfil
    character(256)                       , pointer :: telfil
    type (moroutputtype)                 , pointer :: moroutput
    type (mornumericstype)               , pointer :: mornum
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
    type (cmpbndtype)     , dimension(:) , pointer :: cmpbnd
    type (gd_morpar)                     , pointer :: gdmorpar
!
! Local variables
!
    real(fp) :: rmissval
    integer  :: imissval
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    morft               => gdp%gdmorpar%morft
    morft0              => gdp%gdmorpar%morft0
    morfac              => gdp%gdmorpar%morfac
    thresh              => gdp%gdmorpar%thresh
    aksfac              => gdp%gdmorpar%aksfac
    rwave               => gdp%gdmorpar%rwave
    alfabs              => gdp%gdmorpar%alfabs
    alfabn              => gdp%gdmorpar%alfabn
    camax               => gdp%gdmorpar%camax
    dzmax               => gdp%gdmorpar%dzmax
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    tmor                => gdp%gdmorpar%tmor
    thetsd              => gdp%gdmorpar%thetsd
    susw                => gdp%gdmorpar%susw
    sedthr              => gdp%gdmorpar%sedthr
    hmaxth              => gdp%gdmorpar%hmaxth
    bedw                => gdp%gdmorpar%bedw
    rdc                 => gdp%gdmorpar%rdc
    rdw                 => gdp%gdmorpar%rdw
    espir               => gdp%gdmorpar%espir
    ashld               => gdp%gdmorpar%ashld
    bshld               => gdp%gdmorpar%bshld
    cshld               => gdp%gdmorpar%cshld
    dshld               => gdp%gdmorpar%dshld
    coulfri             => gdp%gdmorpar%coulfri
    flfdrat             => gdp%gdmorpar%flfdrat
    alfpa               => gdp%gdmorpar%alfpa
    thcrpa              => gdp%gdmorpar%thcrpa
    asklhe              => gdp%gdmorpar%asklhe
    mwwjhe              => gdp%gdmorpar%mwwjhe
    ttlalpha            => gdp%gdmorpar%ttlalpha
    ttlmin              => gdp%gdmorpar%ttlmin
    wetslope            => gdp%gdmorpar%wetslope
    avaltime            => gdp%gdmorpar%avaltime
    !
    ihidexp             => gdp%gdmorpar%ihidexp
    itmor               => gdp%gdmorpar%itmor
    iopkcw              => gdp%gdmorpar%iopkcw
    iopsus              => gdp%gdmorpar%iopsus
    islope              => gdp%gdmorpar%islope
    morfacpar           => gdp%gdmorpar%morfacpar
    morfacrec           => gdp%gdmorpar%morfacrec
    morfactable         => gdp%gdmorpar%morfactable
    nxx                 => gdp%gdmorpar%nxx
    morbnd              => gdp%gdmorpar%morbnd
    cmpbnd              => gdp%gdmorpar%cmpbnd
    mergebuf            => gdp%gdmorpar%mergebuf
    xx                  => gdp%gdmorpar%xx
    ttlform             => gdp%gdmorpar%ttlform
    telform             => gdp%gdmorpar%telform
    !
    bedupd              => gdp%gdmorpar%bedupd
    cmpupd              => gdp%gdmorpar%cmpupd
    eqmbcsand           => gdp%gdmorpar%eqmbcsand
    eqmbcmud            => gdp%gdmorpar%eqmbcmud
    densin              => gdp%gdmorpar%densin
    rouse               => gdp%gdmorpar%rouse
    epspar              => gdp%gdmorpar%epspar
    updinf              => gdp%gdmorpar%updinf
    neglectentrainment  => gdp%gdmorpar%neglectentrainment
    oldmudfrac          => gdp%gdmorpar%oldmudfrac
    varyingmorfac       => gdp%gdmorpar%varyingmorfac
    multi               => gdp%gdmorpar%multi
    !
    bcmfilnam           => gdp%gdmorpar%bcmfilnam
    flcomp              => gdp%gdmorpar%flcomp
    mmsyncfilnam        => gdp%gdmorpar%mmsyncfilnam
    ttlfil              => gdp%gdmorpar%ttlfil
    telfil              => gdp%gdmorpar%telfil
    !
    istat = 0
    allocate (gdp%gdmorpar%moroutput  , stat = istat)
    allocate (gdp%gdmorpar%mornum     , stat = istat)
    !
    moroutput           => gdp%gdmorpar%moroutput
    mornum              => gdp%gdmorpar%mornum
    pangle              => gdp%gdmorpar%pangle
    fpco                => gdp%gdmorpar%fpco
    factcr              => gdp%gdmorpar%factcr
    subiw               => gdp%gdmorpar%subiw
    eulerisoglm         => gdp%gdmorpar%eulerisoglm
    glmisoeuler         => gdp%gdmorpar%glmisoeuler
    !
    gdp%gdmorpar%moroutput%transptype  = 2
    !
    gdp%gdmorpar%moroutput%cumavg      = .false.
    gdp%gdmorpar%moroutput%dg          = .false.
    gdp%gdmorpar%moroutput%dgsd        = .false.
    gdp%gdmorpar%moroutput%dm          = .false.
    gdp%gdmorpar%moroutput%dzduuvv     = .false.
    gdp%gdmorpar%moroutput%fixfac      = .false.
    gdp%gdmorpar%moroutput%hidexp      = .false.
    gdp%gdmorpar%moroutput%frac        = .false.
    gdp%gdmorpar%moroutput%mudfrac     = .false.
    gdp%gdmorpar%moroutput%sandfrac    = .false.
    gdp%gdmorpar%moroutput%percentiles = .false.
    gdp%gdmorpar%moroutput%sbcuv       = .false.
    gdp%gdmorpar%moroutput%sbcuuvv     = .false.
    gdp%gdmorpar%moroutput%sbwuv       = .false.
    gdp%gdmorpar%moroutput%sbwuuvv     = .false.
    gdp%gdmorpar%moroutput%sswuv       = .false.
    gdp%gdmorpar%moroutput%sswuuvv     = .false.
    gdp%gdmorpar%moroutput%suvcor      = .false.
    gdp%gdmorpar%moroutput%sourcesink  = .false.
    gdp%gdmorpar%moroutput%taurat      = .false.
    gdp%gdmorpar%moroutput%umod        = .false.
    gdp%gdmorpar%moroutput%ustar       = .false.
    gdp%gdmorpar%moroutput%uuuvvv      = .false.
    gdp%gdmorpar%moroutput%zumod       = .false.
    !
    gdp%gdmorpar%mornum%upwindbedload            = .true.
    gdp%gdmorpar%mornum%laterallyaveragedbedload = .false.
    gdp%gdmorpar%mornum%maximumwaterdepth        = .false.
    !
    rmissval           = -999.0_fp
    imissval           = -999
    !
    morft              = 0.0_hp
    morft0             = 0.0_hp
    !
    bcmfilnam          = ' '
    flcomp             = ' '
    mmsyncfilnam       = ' '
    ttlfil             = ' '
    telfil             = ' '
    !
    morfac             = 1.0_fp
    thresh             = 0.1_fp
    aksfac             = 1.0_fp
    rwave              = 2.0_fp
    alfabs             = 1.0_fp
    alfabn             = 1.5_fp
    camax              = 0.65_fp
    dzmax              = 0.05_fp
    sus                = 1.0_fp
    bed                = 1.0_fp
    tmor               = 0.0_fp
    thetsd             = 0.0_fp
    susw               = 1.0_fp
    sedthr             = 0.5_fp
    hmaxth             = 1.0_fp
    bedw               = 1.0_fp
    factcr             = 1.0_fp    
    rdw                = 0.02_fp
    rdc                = 0.01_fp
    espir              = 0.0_fp
    ashld              = 0.85_fp
    bshld              = 0.5_fp
    cshld              = 0.0_fp
    dshld              = 0.0_fp
    pangle             = 0.0_fp
    fpco               = 1.0_fp
    subiw              = 51
    coulfri            = rmissval
    flfdrat            = rmissval
    alfpa              = rmissval
    thcrpa             = rmissval
    asklhe             = rmissval
    mwwjhe             = rmissval
    ttlalpha           = 0.1_fp
    ttlmin             = 0.0_fp
    wetslope           = 10.0_fp
    avaltime           = 86400.0_fp
    !
    ihidexp            = 1
    itmor              = 0
    iopkcw             = 1
    iopsus             = 0
    islope             = 2
    morfacpar          = imissval
    morfacrec          = imissval
    morfactable        = imissval
    nxx                = 0
    ttlform            = 1
    telform            = 1
    !
    bedupd             = .false.
    cmpupd             = .false.
    eqmbcsand          = .true.
    eqmbcmud           = .false.
    eulerisoglm        = .false.    
    glmisoeuler        = .false.    
    densin             = .true.
    rouse              = .false.
    epspar             = .false.
    updinf             = .false.
    neglectentrainment = .false.
    oldmudfrac         = .false.
    varyingmorfac      = .false.
    multi              = .false.
    !
    nullify(gdp%gdmorpar%morbnd)
    nullify(gdp%gdmorpar%cmpbnd)
    nullify(gdp%gdmorpar%xx)
    nullify(gdp%gdmorpar%mergebuf)
end subroutine initmorpar
