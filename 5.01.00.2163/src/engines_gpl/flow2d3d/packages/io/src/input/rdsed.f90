subroutine rdsed(lundia    ,error     ,lsal      ,ltem      ,lsed      , &
               & lsedtot   ,lstsci    ,ltur      , &
               & facdss    ,namcon    ,iopsus    ,gdp       )
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
!  $Id: rdsed.f90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdsed.f90 $
!!--description-----------------------------------------------------------------
!
! Read sediment parameters from an input file
! File type:
!    ASCII-file if file version below 02.00
!    INI  -file if file version is 02.00 or higher
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                           , pointer :: csoil
    real(fp)                           , pointer :: mdcuni
    real(fp)                           , pointer :: kssilt
    real(fp)                           , pointer :: kssand
    integer                            , pointer :: nmudfrac
    real(fp)         , dimension(:)    , pointer :: rhosol
    real(fp)         , dimension(:,:,:), pointer :: logseddia
    real(fp)         , dimension(:)    , pointer :: logsedsig
    real(fp)         , dimension(:)    , pointer :: sedd10
    real(fp)         , dimension(:)    , pointer :: sedd50
    real(fp)         , dimension(:)    , pointer :: sedd50fld
    real(fp)         , dimension(:)    , pointer :: seddm
    real(fp)         , dimension(:)    , pointer :: sedd90
    real(fp)         , dimension(:)    , pointer :: cdryb
    real(fp)         , dimension(:)    , pointer :: dstar
    real(fp)         , dimension(:)    , pointer :: taucr
    real(fp)         , dimension(:)    , pointer :: tetacr
    real(fp)         , dimension(:)    , pointer :: ws0
    real(fp)         , dimension(:)    , pointer :: wsm
    real(fp)         , dimension(:)    , pointer :: salmax
    real(fp)         , dimension(:)    , pointer :: sdbuni
    real(fp)         , dimension(:)    , pointer :: sedtrcfac
    real(fp)         , dimension(:,:)  , pointer :: tcrdep
    real(fp)         , dimension(:)    , pointer :: tcduni
    real(fp)         , dimension(:,:)  , pointer :: tcrero
    real(fp)         , dimension(:)    , pointer :: tceuni
    real(fp)         , dimension(:)    , pointer :: thcmud
    real(fp)         , dimension(:,:)  , pointer :: eropar
    real(fp)         , dimension(:)    , pointer :: erouni
    real(fp)         , dimension(:)    , pointer :: tcguni
    real(fp)         , dimension(:,:)  , pointer :: gamtcr
    real(fp)         , dimension(:)    , pointer :: gamflc
    real(fp)         , dimension(:)    , pointer :: mudcnt
    integer          , dimension(:)    , pointer :: nseddia
    integer          , dimension(:)    , pointer :: sedtyp
    character(10)    , dimension(:)    , pointer :: inisedunit
    character(20)    , dimension(:)    , pointer :: namsed
    character(256)   , dimension(:)    , pointer :: flsdbd
    character(256)   , dimension(:)    , pointer :: flstcd
    character(256)   , dimension(:)    , pointer :: flstce
    character(256)   , dimension(:)    , pointer :: flsero
    character(256)   , dimension(:)    , pointer :: flstcg
    logical                            , pointer :: anymud
    logical                            , pointer :: bsskin
    character(256)                     , pointer :: flsdia
    character(256)                     , pointer :: flsmdc
    type (gd_sedpar)                   , pointer :: gdsedpar
    integer                            , pointer :: npar
    character(256)   , dimension(:)    , pointer :: dll_function_settle
    character(256)   , dimension(:)    , pointer :: dll_name_settle
    integer(pntrsize), dimension(:)    , pointer :: dll_handle_settle
    character(256)   , dimension(:)    , pointer :: dll_usrfil_settle
    character(256)   , dimension(:)    , pointer :: dll_function
    integer(pntrsize), dimension(:)    , pointer :: dll_handle
    character(256)   , dimension(:)    , pointer :: dll_usrfil
    character(256)   , dimension(:)    , pointer :: flstrn
    integer          , dimension(:)    , pointer :: iform
    character(256)   , dimension(:)    , pointer :: name
    real(fp)         , dimension(:,:)  , pointer :: par
    include 'lognormal.inc'
    include 'sedparams.inc'
!
! Global variables
!
    integer                                  , intent(in)  :: lsal    !  Description and declaration in dimens.igs
    integer                                                :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                  , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                  , intent(in)  :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                  , intent(in)  :: ltem    !  Description and declaration in dimens.igs
    integer                                  , intent(in)  :: ltur    !  Description and declaration in esm_alloc_int.f90
    integer                                                :: lundia  !  Description and declaration in inout.igs
    logical                                  , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    real(fp)      , dimension(lsed)                        :: facdss  !  Description and declaration in esm_alloc_real.f90
    character(20) , dimension(lstsci + ltur)               :: namcon  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                     :: i
    integer                     :: iocond
    integer                     :: iopsus
    integer                     :: istat
    integer(pntrsize)           :: istat_ptr
    integer                     :: j
    integer                     :: l
    integer                     :: lbl                 ! bedload fraction number: lbl = l - lsed
    integer                     :: lenc                ! Help var. (length of character var.) 
    integer                     :: lfile
    integer                     :: luninp
    integer                     :: n                   ! Temporary storage for nseddia(l)
    integer                     :: version
    integer          , external :: newlun
    integer(pntrsize), external :: open_shared_library
    real(fp)                    :: logsedd50
    real(fp)                    :: rmissval
    real(fp)                    :: seddxx              ! Temporary storage for sediment diameter
    real(fp)                    :: sedsg               ! Temporary storage for geometric standard deviation
    real(fp)                    :: xxinv               ! Help var. [1/xx or 1/(1-xx) in log unif distrib.]
    real(fp)                    :: xm
    logical                     :: found
    logical                     :: ex
    logical          , external :: stringsequalinsens
    logical                     :: success
    character(20)               :: sedname
    character(256)              :: filsed
    character(256)              :: filtrn
    character(256)              :: rec
    character(300)              :: message
    character(80)               :: parname
    character(20)               :: sedtype             ! Local variable for sediment type
    character(78)               :: string
    character(30)               :: txtput1
    character(10)               :: txtput2
    character(10)               :: versionstring
    character(6)                :: seddxxstring
    character(256)              :: errmsg
    type(tree_data)  , pointer  :: sed_ptr
    type(tree_data)  , pointer  :: sedblock_ptr
!
!! executable statements -------------------------------------------------------
!
    csoil                => gdp%gdsedpar%csoil
    mdcuni               => gdp%gdsedpar%mdcuni
    kssilt               => gdp%gdsedpar%kssilt
    kssand               => gdp%gdsedpar%kssand
    nmudfrac             => gdp%gdsedpar%nmudfrac
    rhosol               => gdp%gdsedpar%rhosol
    logseddia            => gdp%gdsedpar%logseddia
    logsedsig            => gdp%gdsedpar%logsedsig
    sedd10               => gdp%gdsedpar%sedd10
    sedd50               => gdp%gdsedpar%sedd50
    sedd50fld            => gdp%gdsedpar%sedd50fld
    seddm                => gdp%gdsedpar%seddm
    sedd90               => gdp%gdsedpar%sedd90
    cdryb                => gdp%gdsedpar%cdryb
    dstar                => gdp%gdsedpar%dstar
    taucr                => gdp%gdsedpar%taucr
    tetacr               => gdp%gdsedpar%tetacr
    ws0                  => gdp%gdsedpar%ws0
    wsm                  => gdp%gdsedpar%wsm
    salmax               => gdp%gdsedpar%salmax
    sdbuni               => gdp%gdsedpar%sdbuni
    sedtrcfac            => gdp%gdsedpar%sedtrcfac
    tcrdep               => gdp%gdsedpar%tcrdep
    tcduni               => gdp%gdsedpar%tcduni
    tcrero               => gdp%gdsedpar%tcrero
    tceuni               => gdp%gdsedpar%tceuni
    thcmud               => gdp%gdsedpar%thcmud
    eropar               => gdp%gdsedpar%eropar
    erouni               => gdp%gdsedpar%erouni
    tcguni               => gdp%gdsedpar%tcguni
    gamtcr               => gdp%gdsedpar%gamtcr
    gamflc               => gdp%gdsedpar%gamflc
    mudcnt               => gdp%gdsedpar%mudcnt
    nseddia              => gdp%gdsedpar%nseddia
    sedtyp               => gdp%gdsedpar%sedtyp
    inisedunit           => gdp%gdsedpar%inisedunit
    namsed               => gdp%gdsedpar%namsed
    flsdbd               => gdp%gdsedpar%flsdbd
    flstcd               => gdp%gdsedpar%flstcd
    flstce               => gdp%gdsedpar%flstce
    flsero               => gdp%gdsedpar%flsero
    flstcg               => gdp%gdsedpar%flstcg
    anymud               => gdp%gdsedpar%anymud
    bsskin               => gdp%gdsedpar%bsskin
    flsdia               => gdp%gdsedpar%flsdia
    flsmdc               => gdp%gdsedpar%flsmdc
    gdsedpar             => gdp%gdsedpar
    npar                 => gdp%gdeqtran%npar
    dll_function_settle  => gdp%gdeqtran%dll_function_settle
    dll_name_settle      => gdp%gdeqtran%dll_name_settle
    dll_handle_settle    => gdp%gdeqtran%dll_handle_settle
    dll_usrfil_settle    => gdp%gdeqtran%dll_usrfil_settle
    dll_function         => gdp%gdeqtran%dll_function
    dll_handle           => gdp%gdeqtran%dll_handle
    dll_usrfil           => gdp%gdeqtran%dll_usrfil
    flstrn               => gdp%gdeqtran%flstrn
    iform                => gdp%gdeqtran%iform
    name                 => gdp%gdeqtran%name
    par                  => gdp%gdeqtran%par
    !
    rmissval = -999.0_fp
    !
    istat = 0
    if (.not. associated(gdsedpar%sedd50)) then
       !
       ! allocation of namsed, rhosol and sedtyp is moved to dimsedconst.f90
       !
       if (istat==0) allocate (gdsedpar%nseddia   (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%logseddia (2, 101,                   lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%logsedsig (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%sedd10    (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%sedd50    (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%seddm     (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%sedd90    (                          lsedtot), stat = istat)
       !
       if (istat==0) allocate (gdsedpar%cdryb     (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%dstar     (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%taucr     (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%tetacr    (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%sdbuni    (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%sedtrcfac (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%flsdbd    (                          lsedtot), stat = istat)
       if (istat==0) allocate (gdsedpar%inisedunit(                          lsedtot), stat = istat)
       !
       if (istat==0) allocate (gdsedpar%ws0       (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%wsm       (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%salmax    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%tcguni    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%gamflc    (                      max(1,lsed)), stat = istat)
       !
       if (istat==0) allocate (gdsedpar%tcrdep    (gdp%d%nmlb:gdp%d%nmub,max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%tcduni    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%flstcd    (                      max(1,lsed)), stat = istat)
       !
       if (istat==0) allocate (gdsedpar%tcrero    (gdp%d%nmlb:gdp%d%nmub,max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%tceuni    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%thcmud    (gdp%d%nmlb:gdp%d%nmub            ), stat = istat)
       if (istat==0) allocate (gdsedpar%flstce    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%flstcg    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%gamtcr    (gdp%d%nmlb:gdp%d%nmub,max(1,lsed)), stat = istat)
       !
       if (istat==0) allocate (gdsedpar%eropar    (gdp%d%nmlb:gdp%d%nmub,max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%erouni    (                      max(1,lsed)), stat = istat)
       if (istat==0) allocate (gdsedpar%flsero    (                      max(1,lsed)), stat = istat)
       !
       if (istat==0) allocate (gdsedpar%mudcnt    (gdp%d%nmlb:gdp%d%nmub            ), stat = istat)
       if (istat==0) allocate (gdsedpar%sedd50fld (gdp%d%nmlb:gdp%d%nmub            ), stat = istat)
       !
       if (istat/=0) then
          call prterr(lundia, 'U021', 'RDSED: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! update local pointers
       !
       nseddia       => gdp%gdsedpar%nseddia
       logseddia     => gdp%gdsedpar%logseddia
       logsedsig     => gdp%gdsedpar%logsedsig
       sedd10        => gdp%gdsedpar%sedd10
       sedd50        => gdp%gdsedpar%sedd50
       seddm         => gdp%gdsedpar%seddm
       sedd90        => gdp%gdsedpar%sedd90
       !
       cdryb         => gdp%gdsedpar%cdryb
       dstar         => gdp%gdsedpar%dstar
       taucr         => gdp%gdsedpar%taucr
       tetacr        => gdp%gdsedpar%tetacr
       sdbuni        => gdp%gdsedpar%sdbuni
       sedtrcfac     => gdp%gdsedpar%sedtrcfac
       flsdbd        => gdp%gdsedpar%flsdbd
       inisedunit    => gdp%gdsedpar%inisedunit
       !
       ws0           => gdp%gdsedpar%ws0
       wsm           => gdp%gdsedpar%wsm
       salmax        => gdp%gdsedpar%salmax
       !
       tcrdep        => gdp%gdsedpar%tcrdep
       tcduni        => gdp%gdsedpar%tcduni
       flstcd        => gdp%gdsedpar%flstcd
       !
       tcrero        => gdp%gdsedpar%tcrero
       tceuni        => gdp%gdsedpar%tceuni
       thcmud        => gdp%gdsedpar%thcmud
       flstce        => gdp%gdsedpar%flstce
       !
       eropar        => gdp%gdsedpar%eropar
       erouni        => gdp%gdsedpar%erouni
       flsero        => gdp%gdsedpar%flsero
       !
       mudcnt        => gdp%gdsedpar%mudcnt
       sedd50fld     => gdp%gdsedpar%sedd50fld
       gamtcr        => gdp%gdsedpar%gamtcr
       gamflc        => gdp%gdsedpar%gamflc
       tcguni        => gdp%gdsedpar%tcguni
       flstcg        => gdp%gdsedpar%flstcg
       !
       !
       ! end check on assocation of gdsedpar%sedd50
       !
    endif 
    !
    ! Initialization of the just allocated arrays
    !
    flsdbd              = ' '
    filsed              = ' '
    flsmdc              = ' '
    flsdia              = ' '
    dll_function_settle = ' '
    dll_usrfil_settle   = ' '
    flstcd              = ' '
    flstce              = ' '
    flsero              = ' '
    flstcg              = ' '
    filtrn              = ' '
    flstrn              = ' '
    !
    sedtyp              = 0
    nseddia      = 0        ! nseddia counts relevant data
    logseddia    = rmissval
    logsedsig    = rmissval
    sedd10       = rmissval
    sedd50       = rmissval
    seddm        = rmissval
    sedd90       = rmissval
    !
    sedtrcfac    = rmissval
    !
    dstar        = rmissval
    taucr        = rmissval
    tetacr       = rmissval
    inisedunit   = 'kg/m2'
    !
    tcrdep       = rmissval
    tcduni       = rmissval
    !
    tcrero       = rmissval
    thcmud       = rmissval
    !
    eropar       = rmissval
    !
    mudcnt       = rmissval
    sedd50fld    = rmissval
    !
    tcguni       = 1.5
    gamtcr       = 1.5
    gamflc       = 1.0
    !
    ! Initialization of local parameters/arrays
    !
    version      = 0
    error        = .false.
    lenc         = 4
    !       
    facdss       = rmissval
    !
    seddxxstring = 'SedDXX'
    anymud       = .false.
    !
    ! Sediment input is placed in input_tree in subroutine dimsedconst
    !
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filsed', filsed)
    call tree_get_node_by_name( gdp%input_tree, 'Sediment Input', sed_ptr )
    !
    ! Check version number of sed input file
    !
    versionstring = ' '
    call prop_get_string(sed_ptr, 'SedimentFileInformation', 'FileVersion', versionstring)
    if (trim(versionstring) == '02.00' .or. trim(versionstring) == '03.00') then
       error  = .false.
       !
       csoil  = 1.0e4_fp
       call prop_get(sed_ptr, 'SedimentOverall', 'Cref', csoil)
       !
       iopsus = 0
       call prop_get_integer(sed_ptr, 'SedimentOverall', 'IopSus', iopsus)
       !
       call prop_get_string(sed_ptr, 'SedimentOverall', 'MudCnt', flsmdc)
       !
       ! Intel 7.0 crashes on an inquire statement when file = ' '
       !
       if (flsmdc == ' ') then
          ex = .false.
       else
          call combinepaths(filsed, flsmdc)
          inquire (file = flsmdc, exist = ex)
       endif
       if (.not. ex) then
          flsmdc = ' '
          mdcuni = 0.0_fp
          call prop_get(sed_ptr, '*', 'MudCnt', mdcuni)
       endif
       !
       if ( .not. associated(sed_ptr%child_nodes) ) then
          call prterr(lundia, 'U021', 'Unable to read sediment information')
          call d3stop(1, gdp)          
       endif
       !
       ! Get bed shear skin stress parameters
       !
       bsskin = .false.
       call prop_get_logical(sed_ptr, 'SedimentOverall', 'BsSkin', bsskin)
       if (bsskin) then
          kssilt = 0.0_fp
          kssand = 0.0_fp
          call prop_get(sed_ptr, 'SedimentOverall', 'KsSilt', kssilt)
          call prop_get(sed_ptr, 'SedimentOverall', 'KsSand', kssand)
       endif
       !
       do l = 1, lsedtot
          if (l <= lsed) then
             !
             ! Constituent (suspended load fraction): get name from namcon array
             !
             sedname = namcon(max(0, lsal, ltem) + l)
          else
             !
             ! Non-constituent (bed load fraction)
             ! Find bedload fraction number lbl (non-constituent fractions
             ! have been counted in DIMSEDCONST.F90, so it should exist)
             !
             lbl     = l - lsed
             sedname = 'Bedload Name Missing'
             do i = 1, size(sed_ptr%child_nodes)
                !
                ! Does sed_ptr contain a child with name 'Sediment' (converted to lower case)?
                !
                sedblock_ptr => sed_ptr%child_nodes(i)%node_ptr
                parname = tree_get_name( sedblock_ptr )
                if ( trim(parname) /= 'sediment') cycle
                parname = ' '
                sedtype = ' '
                call prop_get_string(sedblock_ptr, '*', 'Name'  , parname)
                call prop_get_string(sedblock_ptr, '*', 'SedTyp', sedtype)
                if (index(sedtype, 'bedload') == 1) then
                   lbl = lbl - 1
                   if (lbl == 0) then
                      sedname = parname(1:20)
                      exit
                   endif
                endif
             enddo
          endif
          namsed(l) = sedname(1:20)
          write(lundia,'(a,i2,2a)') 'Sediment fraction ', l, ': ', sedname
          !
          found = .false.
          do i = 1, size(sed_ptr%child_nodes)
             !
             ! Does sed_ptr contain a child with name 'Sediment' (converted to lower case)?
             !
             sedblock_ptr => sed_ptr%child_nodes(i)%node_ptr
             parname = tree_get_name( sedblock_ptr )
             call small(parname, len(parname))
             if ( trim(parname) /= 'sediment') cycle
             parname = ' '
             call prop_get_string(sedblock_ptr, '*', 'Name', parname)
             if (.not. stringsequalinsens(parname, sedname)) cycle
             !
             found     = .true.
             sedtype   = ' '
             call prop_get_string(sedblock_ptr, '*', 'SedTyp', sedtype)
             if (sedtype /= ' ') then
                call small(sedtype, lenc)
                !
                ! check sedtyp
                !
                if (l<=lsed .and. index(sedtype, 'mud')==1) then
                   !
                   ! if it starts with mud, make it mud
                   !
                   sedtyp(l) = SEDTYP_COHESIVE
                elseif (l<=lsed .and. index(sedtype, 'sand')==1) then
                   !
                   !
                   !
                   sedtyp(l) = SEDTYP_NONCOHESIVE_SUSPENDED
                   !
                elseif (l <= lsed) then
                   ! if it does not start with mud (checked above) and
                   !   if it does not start with sand and
                   !   if it is one of the first LSED sediment fractions (otherwise bedload would also be acceptable)
                   ! then show an errormessage
                   !
                   error = .true.
                   call prterr(lundia, 'U007', 'suspended sediment type (must start with sand or mud)')
                else
                   !
                   ! bedload sediment type
                   !
                endif
             endif
             !
             rhosol(l) = rmissval
             call prop_get(sedblock_ptr, '*', 'RhoSol', rhosol(l))
             !
             ! Get the geometric standard deviation of the sediment fraction
             !
             sedsg = rmissval
             call prop_get(sedblock_ptr, '*', 'SedSg', sedsg)
             if (comparereal(sedsg,rmissval) /= 0) then
                logsedsig(l) = log(sedsg)
             endif
             !
             ! In case of one sediment fraction, it is possible to use a spatially
             ! varying grain size.
             !
             if (lsedtot == 1) then
                call prop_get_string(sedblock_ptr, '*', 'SedD50', flsdia)
                if (flsdia == ' ') then
                   !
                   ! Alternative for SedD50 is SedDia (backward compatibility)
                   !
                   call prop_get_string(sedblock_ptr, '*', 'SedDia', flsdia)
                endif
                !
                ! Intel 7.0 crashes on an inquire statement when file = ' '
                !
                if (flsdia == ' ') then
                   ex = .false.
                else
                   call combinepaths(filsed, flsdia)
                   inquire (file = flsdia, exist = ex)
                endif
                if (.not. ex) flsdia = ' '
             else
                ex = .false.
             endif
             !
             ! If there are multiple sediment fractions, or if no spatially
             ! varying grain size file was specified, read the sediment size
             ! properties.
             !
             if (.not. ex) then
                do j = 0, 100
                   seddxx = rmissval
                   if (j == 0) then
                      call prop_get(sedblock_ptr, '*', 'SedMinDia', seddxx)
                   elseif (j == 100) then
                      call prop_get(sedblock_ptr, '*', 'SedMaxDia', seddxx)
                   else
                      write(seddxxstring(5:6),'(i2.2)') j
                      call prop_get(sedblock_ptr, '*', seddxxstring, seddxx)
                      if (j == 50 .and. comparereal(seddxx,rmissval) == 0) then
                         !
                         ! Alternative for SedD50 is SedDia (backward compatibility)
                         !
                         call prop_get(sedblock_ptr, '*', 'SedDia', seddxx)
                      endif
                   endif
                   if (comparereal(seddxx,rmissval) /= 0) then
                      if (seddxx <= 0.0_fp) then
                         !
                         ! error: sediment diameter less than or equal to zero!!
                         !
                         call prterr(lundia, 'U021', 'Sediment diameters must be positive!')
                         call d3stop(1, gdp)
                      endif
                      !
                      nseddia(l)       = nseddia(l) + 1
                      n                = nseddia(l)
                      logseddia(1,n,l) = real(j,fp)
                      logseddia(2,n,l) = log(seddxx)
                      if (n > 1) then
                         if (logseddia(2,n,l) <= logseddia(2,n-1,l)) then
                            !
                            ! error: sediment diameters not increasing!!
                            !
                            call prterr(lundia, 'U021', 'Sediment diameters must be increasing!')
                            call d3stop(1, gdp)
                         endif
                      endif
                   endif
                enddo
             endif
             !
             if (l <= lsed) then
                rec = ' '
                call prop_get(sedblock_ptr, '*', 'SettleLib', rec)
                dll_name_settle(l) = rec
                if (rec /= ' ') then
                   if (gdp%arch == 'win32') then
                      rec(len_trim(rec)+1:) = '.dll'
                   else
                      rec(len_trim(rec)+1:) = '.so'
                   endif
                   dll_name_settle(l) = rec
                   istat_ptr = 0
                   istat_ptr = open_shared_library(dll_handle_settle(l), dll_name_settle(l))
                   if (istat_ptr /= 0) then
                      write(errmsg,'(a,a)') 'Can not open shared library ', trim(dll_name_settle(l))
                      call prterr(lundia, 'P004', trim(errmsg))
                      call d3stop(1, gdp)
                   endif
                   !
                   call prop_get_string(sedblock_ptr, '*', 'SettleFunction', dll_function_settle(l))
                   call prop_get_string(sedblock_ptr, '*', 'SettleInput'   , dll_usrfil_settle(l))
                endif
                !
                salmax(l) = rmissval
                ws0(l)    = rmissval
                wsm(l)    = rmissval
                call prop_get(sedblock_ptr, '*', 'SalMax', salmax(l))
                call prop_get(sedblock_ptr, '*', 'WS0'   , ws0(l))
                call prop_get(sedblock_ptr, '*', 'WSM'   , wsm(l))
                !
                ! Calibration parameter for flocculation
                !
                call prop_get(sedblock_ptr, '*', 'GamFloc', gamflc(l))
                !
                ! Tracer calibration factor
                !
                call prop_get(sedblock_ptr, '*', 'TracerCalibrationFactor', sedtrcfac(l))
                !
                ! First assume that 'TcrSed' contains a filename
                ! If the file does not exist, assume that 'TcrSed' contains a uniform value (real)
                !
                call prop_get_string(sedblock_ptr, '*', 'TcrSed', flstcd(l))
                !
                ! Intel 7.0 crashes on an inquire statement when file = ' '
                !
                if (flstcd(l) == ' ') then
                   ex = .false.
                else
                   call combinepaths(filsed, flstcd(l))
                   inquire (file = flstcd(l), exist = ex)
                endif
                if (.not. ex) then
                   flstcd(l) = ' '
                   tcduni(l) = rmissval
                   call prop_get(sedblock_ptr, '*', 'TcrSed', tcduni(l))
                endif
                !
                ! First assume that 'TcrEro' contains a filename
                ! If the file does not exist, assume that 'TcrEro' contains a uniform value (real)
                !
                call prop_get_string(sedblock_ptr, '*', 'TcrEro', flstce(l))
                !
                ! Intel 7.0 crashes on an inquire statement when file = ' '
                !
                if (flstce(l) == ' ') then
                   ex = .false.
                else
                   call combinepaths(filsed, flstce(l))
                   inquire (file = flstce(l), exist = ex)
                endif
                if (.not. ex) then
                   flstce(l) = ' '
                   tceuni(l) = rmissval
                   call prop_get(sedblock_ptr, '*', 'TcrEro', tceuni(l))
                endif
                !
                ! First assume that 'EroPar' contains a filename
                ! If the file does not exist, assume that 'EroPar' contains a uniform value (real)
                !
                call prop_get_string(sedblock_ptr, '*', 'EroPar', flsero(l))
                !
                ! Intel 7.0 crashes on an inquire statement when file = ' '
                !
                if (flsero(l) == ' ') then
                   ex = .false.
                else
                   call combinepaths(filsed, flsero(l))
                   inquire (file = flsero(l), exist = ex)
                endif
                if (.not. ex) then
                   flsero(l) = ' '
                   erouni(l) = rmissval
                   call prop_get(sedblock_ptr, '*', 'EroPar', erouni(l))
                endif
                !
                ! First assume that 'GamTcr' contains a filename
                ! If the file does not exist, assume that 'GamTcr' contains a uniform value (real)
                !
                call prop_get_string(sedblock_ptr, '*', 'GamTcr', flstcg(l))
                !
                ! Intel 7.0 crashes on an inquire statement when file = ' '
                !
                if (flstcg(l) == ' ') then
                   ex = .false.
                else
                   call combinepaths(filsed, flstcg(l))
                   inquire (file = flstcg(l), exist = ex)
                endif
                if (.not. ex) then
                   flstcg(l) = ' '
                   tcguni(l) = 1.5
                   call prop_get(sedblock_ptr, '*', 'GamTcr', tcguni(l))
                endif
             endif
             !
             cdryb(l) = rmissval
             call prop_get(sedblock_ptr, '*', 'CDryB', cdryb(l))
             !
             ! First assume that 'IniSedThick'/'SdBUni' contains a filename
             ! If the file does not exist, assume that 'SdBUni' contains a uniform value (real)
             !
             call prop_get_string(sedblock_ptr, '*', 'IniSedThick', flsdbd(l))
             if (flsdbd(l) /= ' ') then
                inisedunit(l) = 'm'
             else
                inisedunit(l) = 'kg/m2'
                call prop_get_string(sedblock_ptr, '*', 'SdBUni', flsdbd(l))
             endif
             !
             ! Intel 7.0 crashes on an inquire statement when file = ' '
             !
             if (flsdbd(l) == ' ') then
                ex = .false.
             else
                call combinepaths(filsed, flsdbd(l))
                inquire (file = flsdbd(l), exist = ex)
             endif
             if (.not. ex) then
                sdbuni(l) = rmissval
                if (inisedunit(l) == 'm') then
                   call prop_get(sedblock_ptr, '*', 'IniSedThick', sdbuni(l), success)
                else
                   call prop_get(sedblock_ptr, '*', 'SdBUni', sdbuni(l), success)
                endif
                if (.not. success) then
                   if (inisedunit(l) == 'm') then
                      errmsg = 'Error in IniSedThick: ' // trim(flsdbd(l)) // ' is not a file and not a value.'
                   else
                      errmsg = 'Error in SdBUni.' // trim(flsdbd(l))
                   endif
                   call prterr(lundia, 'P004', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                flsdbd(l) = ' '
             endif
             !
             if (l <= lsed) then
                call prop_get(sedblock_ptr, '*', 'FacDSS', facdss(l))
             endif
             !
             call prop_get(sedblock_ptr, '*', 'TraFrm', filtrn)
             !
             ! Intel 7.0 crashes on an inquire statement when file = ' '
             !
             if (filtrn == ' ') then
                ex = .false.
             else
                call combinepaths(filsed, filtrn)
                inquire (file = filtrn, exist = ex)
             endif
             if (ex) then
                flstrn(l) = filtrn
             endif
             !
             ! jump out of "do i=1,..child_nodes" loop, since sediment type is found
             !
             exit
          enddo
          if (.not. found) then
             write (message,'(4a)') 'Sediment ', trim(sedname), &
                                  & ' not specified in file ', trim(filsed)
             call prterr(lundia, 'U021', trim(message))
             call d3stop(1, gdp)          
          endif
       enddo
    else
       !
       ! sediment input version is 0 or 1:
       ! No keywords
       !
       call noextspaces(filsed, lfile)
       luninp = newlun(gdp)
       open (luninp, file = filsed(1:lfile), form = 'formatted', status = 'old',            &
           & iostat = iocond)
       if (iocond /= 0) then
          call prterr(lundia, 'U015', trim(filsed))
          call d3stop(1, gdp)
       endif
       !
       ! Free formatted file, skip lines starting with a '*'
       !
       read (luninp, '(a)') string
       do while (string(:1) == '*')
          call small(string, lenc)
          i = index(string, 'version')
          if (i /= 0) then
             read (string(i + 8:), '(i2)') version
          else
             write (lundia, '(a)') 'No version number found'
          endif
          read (luninp, '(a)') string
       enddo
       !
       write (versionstring, '(i4)') version
       !
       rewind (luninp)
       call skipstarlines(luninp)
       !
       do l = 1, lsed
          namsed(l) = namcon(max(0, lsal, ltem) + l)
       enddo
       if (version == 0) then
          call rdsed0(lsed      ,luninp    ,lundia    ,csoil     ,iopsus    , &
                    & facdss    ,sedtyp    ,rhosol    ,sedd50    ,salmax    , &
                    & ws0       ,wsm       ,tcduni    ,flstcd    ,tceuni    , &
                    & flstce    ,erouni    ,flsero    ,sdbuni    ,flsdbd    , &
                    & cdryb     ,error     ,gdp       )
       elseif (version == 1) then
          call rdsed1(lsed      ,luninp    ,lundia    ,csoil     ,iopsus    , &
                    & facdss    ,sedtyp    ,rhosol    ,sedd50    ,salmax    , &
                    & ws0       ,wsm       ,tcduni    ,flstcd    ,tceuni    , &
                    & flstce    ,erouni    ,flsero    ,sdbuni    ,flsdbd    , &
                    & cdryb     ,error     ,gdp       )
       else
       endif
       close (luninp)
       !
       ! Old format: SedDia specified, but stored in "wrong" array.
       !
       do l = 1, lsedtot
          if (sedd50(l) > 0.0_fp) then
             nseddia(l)       = 1
             logseddia(1,1,l) = 50.0_fp
             logseddia(2,1,l) = log(sedd50(l))
          endif
       enddo
    endif
    if (error) then
       goto 9999
    endif
    do l = 1, lsedtot
       if (sedtyp(l) == SEDTYP_COHESIVE) then
          anymud   = .true.
          nmudfrac = nmudfrac + 1
       endif
    enddo
    !
    ! echo input in diagnose-file
    !
    write (lundia, '(a)')   '*** Start  of sediment input'
    write (lundia, '(2a)') '    Sediment File Version: ', trim(versionstring)
    txtput1 = 'Ref concentration'
    write (lundia, '(2a,e12.4)') txtput1, ':', csoil
    if (csoil <= 0.0_fp) then
       call prterr(lundia, 'U021', 'Reference concentration should be positive.')
       call d3stop(1, gdp)
    endif
    txtput1 = 'Option Dss'
    write (lundia, '(2a,i12)') txtput1, ':', iopsus
    if (anymud) then
       if (flsmdc /= ' ' .or. comparereal(mdcuni,0.0_fp) /= 0) then
          call prterr(lundia, 'G051', 'User defined mud content ignored: mud fraction simulated.')
       endif
       flsmdc = ' '
       mdcuni = 0.0_fp
    else
       if (flsmdc /= ' ') then
          txtput1 = 'File mud content'
          write (lundia, '(3a)') txtput1, ':  ', trim(flsmdc)
       else
          txtput1 = 'Uniform mud content'
          write (lundia, '(2a,e12.4)') txtput1, ':', mdcuni
       endif
    endif
    if (bsskin) then
       txtput1 = 'Skin friction soulsby 2004'
       write (lundia, '(a)') txtput1
       txtput1 = 'Kssilt '
       write (lundia, '(2a,f12.6)') txtput1,':', kssilt
       txtput1 = 'Kssand '
       write (lundia, '(2a,f12.6)') txtput1,':', kssand
    endif
    !
    do l = 1, lsedtot
       txtput1 = 'Sediment number'
       write (lundia, '(2a,i12)') txtput1, ':', l
       txtput1 = '  Name'
       write (lundia, '(3a)') txtput1, ': ', trim(namsed(l))
       txtput1 = '  Type'
       select case (sedtyp(l))
          case (SEDTYP_NONCOHESIVE_TOTALLOAD)
             write (lundia, '(2a,a12)') txtput1, ':', 'bedload'
          case (SEDTYP_NONCOHESIVE_SUSPENDED)
             write (lundia, '(2a,a12)') txtput1, ':', 'sand'
          case (SEDTYP_COHESIVE)
             write (lundia, '(2a,a12)') txtput1, ':', 'mud'
       end select
       if (sedtrcfac(l)>0.0_fp) then
           txtput1 = '  Tracer calibration factor '
           write (lundia, '(2a,e12.4)') txtput1, ':', sedtrcfac(l)
       endif
       txtput1 = '  RHOSOL'
       write (lundia, '(2a,e12.4)') txtput1, ':', rhosol(l)
       if (flsdia /= ' ') then
          !
          ! One sediment diameter between 0 and 100%
          !
          write (lundia, '(3a)') txtput1, ':  ', 'lognormal'
          if (comparereal(logsedsig(l),rmissval) == 0) then
             !
             ! no standard deviation specified: use default geometric
             ! standard deviation of 1.34 which is in the middle of the
             ! range of sigma_g values representing "well sorted" mixtures
             ! indicated in Blott & Pye, 2001. Earth Surface Processes
             ! and Landforms 26, p. 1237-1248.
             !
             logsedsig(l) = log(1.34)
          endif
          !
          nseddia(l) = -999
          txtput1 = '  geom. st. dev.'
          write (lundia, '(2a,e12.4)') txtput1, ':', exp(logsedsig(l))
          txtput1 = '  SedD50'
          write (lundia, '(3a)') txtput1, ':  ', trim(flsdia)
       elseif (sedtyp(l) /= SEDTYP_COHESIVE) then
          !
          ! Determine various sediment diameters in case of
          ! sand or bedload.
          !
          txtput1 = '  sed. distribution'
          if (nseddia(l) == 0) then
             !
             ! error: no sediment diameter specified!
             !
             call prterr(lundia, 'U021', 'Missing sediment diameter data')
             call d3stop(1, gdp)
          elseif (nseddia(l) == 1) then
             !
             ! Just one sediment diameter
             !
             if (nint(logseddia(1,1,l)) == 0) then
                !
                ! error: only minimum sediment diameter is insufficient
                !
                call prterr(lundia, 'U021', 'Missing maximum diameter data')
                call d3stop(1, gdp)
             elseif (nint(logseddia(1,1,l)) == 100) then
                !
                ! error: only maximum sediment diameter is insufficient
                !
                call prterr(lundia, 'U021', 'Missing minimum diameter data')
                call d3stop(1, gdp)
             endif
             !
             ! One sediment diameter between 0 and 100%
             !
             if (versionstring == '03.00') then
                !
                ! New behaviour: lognormal distribution
                !
                write (lundia, '(3a)') txtput1, ':  ', 'lognormal'
                if (comparereal(logsedsig(l),rmissval) == 0) then
                   !
                   ! no standard deviation specified: use default geometric
                   ! standard deviation of 1.34 which is in the middle of the
                   ! range of sigma_g values representing "well sorted" mixtures
                   ! indicated in Blott & Pye, 2001. Earth Surface Processes
                   ! and Landforms 26, p. 1237-1248.
                   !
                   logsedsig(l) = log(1.34)
                endif
                txtput1 = '  geom. st. dev.'
                write (lundia, '(2a,e12.4)') txtput1, ':', exp(logsedsig(l))
                !
                ! Approximate lognormal distribution using the following
                ! percentiles:
                ! 0.1 (set to 0) --- ilognormal --- 99.9 (set to 100)
                !
                logsedd50        = logseddia(2,1,l) - logsedsig(l)*lognormal(nint(logseddia(1,1,l)))
                logseddia(1,1,l) = 0.0_fp
                logseddia(2,1,l) = logsedd50 - 3.0_fp*logsedsig(l)
                n = 1
                do i = 1, size(ilognormal)
                   n = n + 1
                   logseddia(1,n,l) = real(ilognormal(i),fp)
                   logseddia(2,n,l) = logsedd50 + lognormal(ilognormal(i))*logsedsig(l)
                enddo
                n = n + 1
                logseddia(1,n,l) = 100.0_fp
                logseddia(2,n,l) = logsedd50 + 3.0_fp*logsedsig(l)
                nseddia(l)       = n
                !
                ! Compute characteristic sediment diameters
                !
                sedd50(l) = exp(logsedd50)
                sedd10(l) = exp(logsedd50 + lognormal(10)*logsedsig(l))
                sedd90(l) = exp(logsedd50 + lognormal(90)*logsedsig(l))
                seddm(l)  = exp(logsedd50 + 0.5_fp*logsedsig(l)*logsedsig(l))
             else
                !
                ! Old behaviour: D10 = 0.75 * D50, D90 = 1.5 * D50
                ! Piecewise loguniform approach: requires D50
                !
                if (nint(logseddia(1,1,l)) /= 50) then
                   !
                   ! error: old approach requires D50
                   !
                   call prterr(lundia, 'U021', 'Missing median diameter data')
                   call d3stop(1, gdp)
                endif
                !
                write (lundia, '(3a)') txtput1, ':  ', 'piecewise loguniform'
                !
                nseddia(l)       = 3
                logseddia(1,2,l) = 50.0_fp
                logseddia(2,2,l) = logseddia(2,1,l)
                !
                logseddia(1,1,l) = 0.0_fp
                logseddia(2,1,l) = logseddia(2,2,l) + 5.0_fp*log(0.75_fp)/4.0_fp
                !
                logseddia(1,3,l) = 100.0_fp
                logseddia(2,3,l) = logseddia(2,2,l) + 5.0_fp*log(1.5_fp)/4.0_fp
                !
                ! Compute characteristic sediment diameters
                !
                sedd50(l) = exp(logseddia(2,2,l))
                sedd10(l) = 0.75_fp * sedd50(l)
                sedd90(l) = 1.50_fp * sedd50(l)
                !
                seddm(l)  = 0.0_fp
                logsedsig(l) = 0.0_fp
                xm = 0.0_fp
                do n = 2, nseddia(l)
                   xxinv    = logseddia(1,n,l) - logseddia(1,n-1,l)
                   seddm(l) = seddm(l) &
                            & + xxinv * (exp(logseddia(2,n,l)) - exp(logseddia(2,n-1,l))) &
                            & / (logseddia(2,n,l) - logseddia(2,n-1,l))
                   logsedsig(l) = logsedsig(l) + xxinv * (&
                                & (logseddia(2,n,l)-logseddia(2,n-1,l))/sqrt(3.0_fp)/2.0_fp &
                                & + ((logseddia(2,n,l)+logseddia(2,n-1,l))/2.0_fp)**2 &
                                & )
                   xm = xm + xxinv * (logseddia(2,n,l)+logseddia(2,n-1,l))/2.0_fp
                enddo
                seddm(l) = seddm(l) / 100.0_fp
                xm = xm / 100.0_fp
                logsedsig(l) = logsedsig(l) / 100.0_fp - xm**2
             endif
          elseif (nseddia(l) == 2) then
             !
             ! Two sediment diameters specified
             !
             if (comparereal(logsedsig(l),rmissval) /= 0) then
                !
                ! standard deviation specified
                ! error: not allowed in combination with multiple
                ! sediment diameters
                !
                call prterr(lundia, 'U021', 'Geom. st. dev. not allowed in combination with multiple sediment diameters')
                call d3stop(1, gdp)
             endif
             if (nint(logseddia(1,1,l))==0 .or. nint(logseddia(1,2,l))==100) then
                !
                ! Minimum or maximum sediment diameter given:
                ! loguniform distribution
                !
                write (lundia, '(3a)') txtput1, ':  ', 'loguniform'
                !
                ! Compute characteristic sediment diameters
                !
                if (nint(logseddia(1,1,l)) > 0) then
                   !
                   ! Only maximum sediment diameter given and some
                   ! percentile: compute minimum sediment diameter.
                   !
                   xxinv            = 1.0_fp / (1.0_fp - (real(logseddia(1,1,l),fp)/100.0_fp))
                   logseddia(2,1,l) = logseddia(2,1,l)*xxinv + logseddia(2,2,l)*(1.0_fp-xxinv)
                   logseddia(1,1,l) = 0.0_fp
                elseif (nint(logseddia(1,2,l)) < 100) then
                   !
                   ! Only minimum sediment diameter given and some
                   ! percentile: compute maximum sediment diameter.
                   !
                   xxinv            = 100.0_fp / real(logseddia(1,2,l),fp)
                   logseddia(2,2,l) = logseddia(2,2,l)*xxinv + logseddia(2,1,l)*(1.0_fp-xxinv)
                   logseddia(1,2,l) = 100.0_fp
                endif
                !
                ! Both minimum and maximum sediment diameters given
                !
                sedd50(l) = exp(0.5_fp*logseddia(2,1,l) + 0.5_fp*logseddia(2,2,l))
                sedd10(l) = exp(0.9_fp*logseddia(2,1,l) + 0.1_fp*logseddia(2,2,l))
                sedd90(l) = exp(0.1_fp*logseddia(2,1,l) + 0.9_fp*logseddia(2,2,l))
                seddm(l)  = (exp(logseddia(2,2,l)) - exp(logseddia(2,1,l))) / &
                          & (logseddia(2,2,l) - logseddia(2,1,l))
                logsedsig(l) = (logseddia(2,2,l) - logseddia(2,1,l))/sqrt(3.0_fp)/2.0_fp
             else
                !
                ! Neither minimum nor maximum sediment diameter given:
                ! lognormal distribution
                !
                write (lundia, '(3a)') txtput1, ':  ', 'lognormal'
                !
                ! Compute geometric standard deviation
                !
                logsedsig(l) = (logseddia(2,2,l) - logseddia(2,1,l))/ &
                             & (lognormal(nint(logseddia(1,2,l))) -   &
                             &  lognormal(nint(logseddia(1,1,l))))
                txtput1 = ' geom. stand. dev.'
                write (lundia, '(2a,e12.4)') txtput1, ':', exp(logsedsig(l))
                !
                ! Approximate lognormal distribution using the following
                ! percentiles:
                ! 0.1 (set to 0) --- ilognormal --- 99.9 (set to 100)
                !
                logsedd50        = logseddia(2,1,l) - logsedsig(l)*lognormal(nint(logseddia(1,1,l)))
                logseddia(1,1,l) = 0.0_fp
                logseddia(2,1,l) = logsedd50 - 3.0_fp*logsedsig(l)
                n = 1
                do i = 1, size(ilognormal)
                   n = n + 1
                   logseddia(1,n,l) = real(ilognormal(i),fp)
                   logseddia(2,n,l) = logsedd50 + lognormal(ilognormal(i))*logsedsig(l)
                enddo
                n = n + 1
                logseddia(1,n,l) = 100.0_fp
                logseddia(2,n,l) = logsedd50 + 3.0_fp*logsedsig(l)
                nseddia(l)       = n
                !
                ! Compute characteristic sediment diameters
                !
                sedd50(l) = exp(logsedd50)
                sedd10(l) = exp(logsedd50 + lognormal(10)*logsedsig(l))
                sedd90(l) = exp(logsedd50 + lognormal(90)*logsedsig(l))
                seddm(l)  = exp(logsedd50 + 0.5_fp*logsedsig(l)*logsedsig(l))
             endif
          else
             !
             ! More than two sediment diameters specified
             !
             if (comparereal(logsedsig(l),rmissval) /= 0) then
                !
                ! standard deviation specified
                ! error: not allowed in combination with multiple
                ! sediment diameters
                !
                call prterr(lundia, 'U021', 'Geom. std. dev. not allowed in combination with multiple sediment diameters')
                call d3stop(1, gdp)
             endif
             !
             ! Always piecewise loguniform distribution
             !
             write (lundia, '(3a)') txtput1, ':  ', 'piecewise loguniform'
             !
             ! Compute characteristic sediment diameters
             !
             if (nint(logseddia(1,1,l)) > 0) then
                !
                ! sediment diameter table does not include sedmindia
                ! replace the first entry by extending the first loguniform range
                ! by keeping the density constant
                !
                xxinv            =  real(logseddia(1,2,l),fp) / &
                                 & (real(logseddia(1,2,l),fp) - real(logseddia(1,1,l),fp))
                logseddia(2,1,l) = logseddia(2,1,l)*xxinv + logseddia(2,2,l)*(1.0_fp-xxinv)
                logseddia(1,1,l) = 0.0_fp
             endif
             n = nseddia(l)
             if (nint(logseddia(1,n,l)) < 100) then
                !
                ! sediment diameter table does not include sedmaxdia
                ! replace the last entry by extending the last loguniform range
                ! by keeping the density constant
                !
                xxinv            = (100.0_fp - real(logseddia(1,n-1,l),fp)) / &
                                 & (real(logseddia(1,n,l),fp) - real(logseddia(1,n-1,l),fp))
                logseddia(2,n,l) = logseddia(2,n,l)*xxinv + logseddia(2,n-1,l)*(1.0_fp-xxinv)
                logseddia(1,n,l) = 100.0_fp
             endif
             !
             seddm(l)  = 0.0_fp
             do n = 2, nseddia(l)
                xxinv    = logseddia(1,n,l) - logseddia(1,n-1,l)
                seddm(l) = seddm(l) &
                         & + xxinv * (exp(logseddia(2,n,l)) - exp(logseddia(2,n-1,l))) &
                         & / (logseddia(2,n,l) - logseddia(2,n-1,l))
                xxinv    = 1.0_fp / xxinv
                if (logseddia(1,n-1,l) < 10.0_fp .and. logseddia(1,n,l) >= 10.0_fp) then
                   sedd10(l) = exp(logseddia(2,n-1,l)*((logseddia(1,n,l)-10.0_fp)*xxinv) + &
                                 & logseddia(2,n,l)*((10.0_fp-logseddia(1,n-1,l))*xxinv))
                endif
                if (logseddia(1,n-1,l) < 50.0_fp .and. logseddia(1,n,l) >= 50.0_fp) then
                   sedd50(l) = exp(logseddia(2,n-1,l)*((logseddia(1,n,l)-50.0_fp)*xxinv) + &
                                 & logseddia(2,n,l)*((50.0_fp-logseddia(1,n-1,l))*xxinv))
                endif
                if (logseddia(1,n-1,l) < 90.0_fp .and. logseddia(1,n,l) >= 90.0_fp) then
                   sedd90(l) = exp(logseddia(2,n-1,l)*((logseddia(1,n,l)-90.0_fp)*xxinv) + &
                                 & logseddia(2,n,l)*((90.0_fp-logseddia(1,n-1,l))*xxinv))
                endif
             enddo
             seddm(l) = seddm(l) / 100.0_fp
          endif
          !
          ! convert percentages to fractions
          !
          do n = 1, nseddia(l)
             logseddia(1,n,l) = logseddia(1,n,l) / 100.0_fp
          enddo
          !
          txtput1 = '  SedD10'
          write (lundia, '(2a,e12.4)') txtput1, ':', sedd10(l)
          txtput1 = '  SedD50'
          write (lundia, '(2a,e12.4)') txtput1, ':', sedd50(l)
          txtput1 = '  SedDM'
          write (lundia, '(2a,e12.4)') txtput1, ':', seddm(l)
          txtput1 = '  SedD90'
          write (lundia, '(2a,e12.4)') txtput1, ':', sedd90(l)
       else
          !
          ! The default sediment formula for mud should be Partheniades-Krone
          !
          iform(l) = -1
       endif
       txtput1 = '  CDRYB'
       write (lundia, '(2a,e12.4)') txtput1, ':', cdryb(l)
       if (flsdbd(l) /= ' ') then
          if (inisedunit(l) == 'kg/m2') then
             txtput1 = '  File IniCon'
          else
             txtput1 = '  File IniThick'
          endif
          write (lundia, '(3a)') txtput1, ':  ', trim(flsdbd(l))
       else
          if (inisedunit(l) == 'kg/m2') then
             txtput1 = '  Uniform IniCon'
             txtput2 = ' [kg/m2]'
          else
             txtput1 = '  Uniform IniThick'
             txtput2 = ' [m]'
          endif
          write (lundia, '(2a,e12.4,a)') txtput1, ':', sdbuni(l), trim(txtput2)
       endif
       if (l <= lsed) then
          txtput1 = '  FACDSS'
          write (lundia, '(2a,e12.4)') txtput1, ':', facdss(l)
          if (facdss(l) <= 0.0_fp) then
             call prterr(lundia, 'U021', 'FACDSS <= 0.0')
             call d3stop(1, gdp)
          endif
       endif
       !
       if (iform(l)==-2) then
          !
          ! Van Rijn 2007
          !
          if (flstcg(l) /= ' ') then
             txtput1 = '  File GamTcr'
             write (lundia, '(3a)') txtput1, ':  ', trim(flstcg(l))
          else
             txtput1 = '  Uniform GamTcr'
             write (lundia, '(2a,e12.4)') txtput1, ':', tcguni(l)
          endif
          !
          txtput1 = '  Flocculation factor GamFloc'
          write (lundia, '(2a,e12.4)') txtput1, ':', gamflc(l)
          !
       endif
       !
       if (flstrn(l) /= ' ') then
          txtput1 = '  Transport formula'
          write (lundia, '(a,a)') txtput1, ':  '
          call rdtrafrm0(error  , iform(l)     , npar           , par(1,l), flstrn(l), &
                       & name(l), dll_handle(l), dll_function(l), dll_usrfil(l), gdp   )
       elseif (sedtyp(l) == SEDTYP_COHESIVE) then
          !
          ! default transport formula: Partheniades-Krone
          !
          if (flstcd(l) /= ' ') then
             txtput1 = '  File TCRDEP'
             write (lundia, '(3a)') txtput1, ':  ', trim(flstcd(l))
          else
             txtput1 = '  Uniform TCRDEP'
             write (lundia, '(2a,e12.4)') txtput1, ':', tcduni(l)
          endif
          if (flstce(l) /= ' ') then
             txtput1 = '  File TCRERO'
             write (lundia, '(3a)') txtput1, ':  ', trim(flstce(l))
          else
             txtput1 = '  Uniform TCRERO'
             write (lundia, '(2a,e12.4)') txtput1, ':', tceuni(l)
          endif
          if (flsero(l) /= ' ') then
             txtput1 = '  File EROPAR'
             write (lundia, '(3a)') txtput1, ':  ', trim(flsero(l))
          else
             txtput1 = '  Uniform EROPAR'
             write (lundia, '(2a,e12.4)') txtput1, ':', erouni(l)
          endif
       endif
       if (dll_function_settle(l) /= ' ') then
          !
          ! User defined settling velocity function
          !
          txtput1 = '  Settle library'
          write (lundia, '(3a)') txtput1, ': ', trim(dll_name_settle(l))
          txtput1 = '  Function in Settle lib'
          write (lundia, '(3a)') txtput1, ': ', trim(dll_function_settle(l))
          if (dll_usrfil_settle(l) /= ' ') then
             txtput1 = '  Input for Settle function'
             write (lundia, '(3a)') txtput1, ': ', trim(dll_usrfil_settle(l))
          endif
       elseif (sedtyp(l) == SEDTYP_COHESIVE) then
          txtput1 = '  SALMAX'
          write (lundia, '(2a,e12.4)') txtput1, ':', salmax(l)
          txtput1 = '  WS0'
          write (lundia, '(2a,e12.4)') txtput1, ':', ws0(l)
          txtput1 = '  WSM'
          write (lundia, '(2a,e12.4)') txtput1, ':', wsm(l)
       endif
    enddo
    !
    ! If Van Rijn 2004 transport formula is used (iform = 0 or -2), switch on
    ! the bed roughness height predictor. By default this predictor is set to
    ! the Van Rijn 2004 formulations; give a warning if this has been set to
    ! a different predictor by the user.
    !
    do i = 1, lsedtot
       if (iform(i) == 0 .or. iform(i) == -2) then
          if (gdp%gdbedformpar%bdfrpt /= 0) then
             call prterr(lundia, 'U190', 'Van Rijn 2004 transport formula combined with different bedform roughness predictor')
          endif
          gdp%gdbedformpar%lfbedfrmrou = .true.
          exit
       endif
    enddo
    !
    write (lundia, '(a)') '*** End    of sediment input'
    write (lundia, *)
 9999 continue
end subroutine rdsed
