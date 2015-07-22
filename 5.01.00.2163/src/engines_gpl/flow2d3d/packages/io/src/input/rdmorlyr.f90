subroutine rdmorlyr(lundia    ,error     ,filmor    ,mmax      ,nmax      , &
                  & nmaxus    ,nmmax     ,nto       ,nambnd    ,version   , &
                  & lsedtot   ,namsed    ,parnames  ,gdp  )
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
!  $Id: rdmorlyr.f90 1989 2012-11-19 12:25:23Z boer_aj $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdmorlyr.f90 $
!!--description-----------------------------------------------------------------
!
! Reads attribute file for 3D morphology computation
!
!!--declarations----------------------------------------------------------------
use precision
use bedcomposition_module
    use properties
    use flow_tables
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                                         , intent(in)  :: lsedtot  !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: lundia   !  Description and declaration in inout.igs
    integer                                         , intent(in)  :: mmax
    integer                                         , intent(in)  :: nmax
    integer                                         , intent(in)  :: nmaxus
    integer                                         , intent(in)  :: nmmax
    integer                                         , intent(in)  :: nto
    integer                                         , intent(in)  :: version
    logical                                         , intent(out) :: error
    character(*)                                                  :: filmor
    character(20)             , dimension(nto)                    :: nambnd   !  Description and declaration in esm_alloc_char.f90
    character(20)             , dimension(lsedtot)                :: namsed   !  Names of all sediment fractions 
    character(MAXTABLECLENGTH), dimension(2*lsedtot)              :: parnames
!
! Local variables
!
    real(fp)                 :: rmissval
    real(fp)                 :: thunlyr
    integer                  :: i
    integer                  :: istat
    integer                  :: j
    integer                  :: l
    integer                  :: mxnulyr
    integer                  :: nm
    integer                  :: nval
    character(11)            :: fmttmp       ! Format file ('formatted  ') 
    character(20)            :: parname
    character(20)            :: txtput2
    character(40)            :: txtput1
    character(80)            :: bndname
    character(256)           :: errmsg
    logical                  :: log_temp
    logical                  :: ex
    logical                  :: found
    type(tree_data), pointer :: mor_ptr
    type(tree_data), pointer :: morbound_ptr
    !
    logical                         , pointer :: exchlyr
    logical                         , pointer :: lfbedfrm
    real(fp)                        , pointer :: bed
    real(fp)                        , pointer :: minmass
    real(fp)                        , pointer :: theulyr
    real(fp)                        , pointer :: thlalyr
    real(fp)         , dimension(:) , pointer :: thexlyr
    real(fp)         , dimension(:) , pointer :: thtrlyr
    real(fp)                        , pointer :: ttlalpha
    real(fp)                        , pointer :: ttlmin
    integer                         , pointer :: iporosity
    integer                         , pointer :: iunderlyr
    integer                         , pointer :: maxwarn
    integer                         , pointer :: neulyr
    integer                         , pointer :: nfrac
    integer                         , pointer :: nlalyr
    integer                         , pointer :: nmlb
    integer                         , pointer :: nmub
    integer                         , pointer :: ttlform
    integer                         , pointer :: telform
    integer                         , pointer :: updbaselyr
    type(handletype)                , pointer :: bcmfile
    type(cmpbndtype) , dimension(:) , pointer :: cmpbnd
    character(256)                  , pointer :: bcmfilnam
    character(256)                  , pointer :: flcomp
    character(256)                  , pointer :: ttlfil
    character(256)                  , pointer :: telfil
!
!! executable statements -------------------------------------------------------
!
    lfbedfrm            => gdp%gdbedformpar%lfbedfrm
    bed                 => gdp%gdmorpar%bed
    ttlalpha            => gdp%gdmorpar%ttlalpha
    ttlmin              => gdp%gdmorpar%ttlmin
    ttlform             => gdp%gdmorpar%ttlform
    telform             => gdp%gdmorpar%telform
    bcmfile             => gdp%gdmorpar%bcmfile
    bcmfilnam           => gdp%gdmorpar%bcmfilnam
    flcomp              => gdp%gdmorpar%flcomp
    ttlfil              => gdp%gdmorpar%ttlfil
    telfil              => gdp%gdmorpar%telfil
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'IUnderLyr', iunderlyr)
    if (istat == 0) istat = bedcomp_getpointer_logical(gdp%gdmorlyr, 'ExchLyr'             , exchlyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'NLaLyr'              , nlalyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'NEuLyr'              , neulyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'NFrac'               , nfrac)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'nmLb'                , nmlb)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'nmUb'                , nmub)
    if (istat == 0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'ThEuLyr'             , theulyr)
    if (istat == 0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'ThLaLyr'             , thlalyr)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'UpdBaseLyr'          , updbaselyr)
    if (istat == 0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'MinMassShortWarning' , minmass)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'MaxNumShortWarning'  , maxwarn)
    if (istat == 0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'IPorosity'           , iporosity)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'Memory problem in RDMORLYR')
       call d3stop(1, gdp)
    endif
    !
    nmlb  = gdp%d%nmlb
    nmub  = gdp%d%nmub
    nfrac = lsedtot
    !
    error      = .false.
    rmissval   = -999.0_fp
    fmttmp     = 'formatted'
    !
    ! allocate memory for boundary conditions
    !
    istat = 0
    allocate (gdp%gdmorpar%cmpbnd(nto), stat = istat)
    !
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'RDMORLYR: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    cmpbnd              => gdp%gdmorpar%cmpbnd
    !
    do j = 1, nto
       cmpbnd(j)%icond = 1
       cmpbnd(j)%ibcmt = 0
    enddo
    !
    ! return if input file is too old, otherwise get
    ! the data tree read from the input file
    !
    if (version < 2) then
       if (allocmorlyr(gdp%gdmorlyr) /= 0) then
          call prterr(lundia, 'U021', 'RDMORLYR: memory alloc error')
          call d3stop(1, gdp)
       endif
       goto 777
    endif
    write (lundia, '(a)') '*** Start  of underlayer input'
    call tree_get_node_by_name( gdp%input_tree, 'Morphology Input', mor_ptr )
    !
    ! underlayer bookkeeping mechanism
    !
    call prop_get_integer(mor_ptr, 'Underlayer', 'IUnderLyr', iunderlyr)
    if (iunderlyr < 1 .or. iunderlyr > 2) then
       call prterr(lundia, 'U021', 'IUnderLyr should be 1 or 2 in ' &
                 & // trim(filmor))
       call d3stop(1, gdp)
    endif
    txtput1 = 'Underlayer mechanism'
    write (lundia, '(2a,i20)') txtput1, ':', iunderlyr
    !
    ! underlayer mechanism parameters
    !
    select case (iunderlyr)
    case(2)
       !
       ! flag for exchange layer
       !
       call prop_get_logical(mor_ptr, 'Underlayer', 'ExchLyr', exchlyr)
       txtput1 = 'Exchange layer'
       if (exchlyr) then
          txtput2 = '                 YES'
       else
          txtput2 = '                  NO'
       endif
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       call prop_get_integer(mor_ptr, 'Underlayer', 'IPorosity', iporosity)
       txtput1 = 'Porosity'
       select case (iporosity)
       case (0)
          txtput2 = '      Based on CDRYB'
       case (1)
          txtput2 = '              Linear'
       case (2)
          txtput2 = '          Non-linear'
       end select
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       nlalyr = 0
       neulyr = 0
       call prop_get_integer(mor_ptr, 'Underlayer', 'NLaLyr', nlalyr)
       if (nlalyr < 0) then
          call prterr(lundia, 'U021', &
                    & 'Number of Lagrangian under layers should be 0 or more in ' &
                    & // trim(filmor))
          call d3stop(1, gdp)
       endif
       call prop_get_integer(mor_ptr, 'Underlayer', 'NEuLyr', neulyr)
       if (neulyr < 0) then
          call prterr(lundia, 'U021', &
                    & 'Number of Eulerian under layers should be 0 or more in ' &
                    & // trim(filmor))
          call d3stop(1, gdp)
       endif
       !
       mxnulyr = nlalyr+neulyr
       call prop_get_integer(mor_ptr, 'Underlayer', 'MxNULyr', mxnulyr)
       if (mxnulyr < 0) then
          call prterr(lundia, 'U021', &
                    & 'Maximum number of under layers should be 0 or more in ' &
                    & // trim(filmor))
          call d3stop(1, gdp)
       endif
       if (mxnulyr /= nlalyr+neulyr) then
          nlalyr = -999
          neulyr = -999
          call prop_get_integer(mor_ptr, 'Underlayer', 'NLaLyr', nlalyr)
          call prop_get_integer(mor_ptr, 'Underlayer', 'NEuLyr', neulyr)
          if (nlalyr<0 .and. neulyr<0) then
             !
             ! neither NLaLyr nor NEuLyr specified
             !
             nlalyr = 0
             neulyr = mxnulyr
          elseif (nlalyr>=0 .and. neulyr>=0) then
             !
             ! mismatch: error
             !
             call prterr(lundia, 'U021', &
                      & 'Remove MxNULyr or set MxNULyr = NLaLyr+NEuLyr in ' &
                      & // trim(filmor))
             call d3stop(1, gdp)
          elseif (nlalyr>=0) then
             !
             ! NLaLyr specified and MxNULyr
             !
             neulyr = mxnulyr - nlalyr
             if (neulyr<0) then
                call prterr(lundia, 'U021', &
                         & 'NLaLyr must be less than MxNULyr in ' &
                         & // trim(filmor))
             endif
          else
             !
             ! NEuLyr specified and MxNULyr
             !
             nlalyr = mxnulyr - neulyr
             if (nlalyr<0) then
                call prterr(lundia, 'U021', &
                          & 'NEuLyr must be less than MxNULyr in ' &
                         & // trim(filmor))
             endif
          endif
       endif
       mxnulyr = nlalyr + neulyr
       txtput1 = 'Number of Lagrangian layers'
       write (lundia, '(2a,i20)') txtput1, ':', nlalyr
       txtput1 = 'Number of Eulerian layers'
       write (lundia, '(2a,i20)') txtput1, ':', neulyr
       !
       if (mxnulyr > 0) then
          thunlyr = rmissval
          call prop_get(mor_ptr, 'Underlayer', 'ThUnLyr', thunlyr)
          theulyr = thunlyr
          thlalyr = thunlyr
          call prop_get(mor_ptr, 'Underlayer', 'ThEuLyr', theulyr)
          call prop_get(mor_ptr, 'Underlayer', 'ThLaLyr', thlalyr)
          !
          if (nlalyr>0) then
             txtput1 = 'Thickness of Lagrangian underlayers'
             write(lundia,'(2a,e20.4)') txtput1, ':', thlalyr
             if (thlalyr <= 0.0_fp) then
                call prterr(lundia, 'U021', 'ThLaLyr should be positive in ' &
                          & // trim(filmor))
                call d3stop(1, gdp)
             endif
          endif
          if (neulyr>0) then
             txtput1 = 'Thickness of Eulerian underlayers'
             write(lundia,'(2a,e20.4)') txtput1, ':', thlalyr
             if (theulyr <= 0.0_fp) then
                call prterr(lundia, 'U021', 'ThEuLyr should be positive in ' &
                          & // trim(filmor))
                call d3stop(1, gdp)
             endif
          endif
       endif
       !
       call prop_get_integer(mor_ptr, 'Underlayer', 'UpdBaseLyr', updbaselyr)
       if (updbaselyr < 1 .or. updbaselyr > 4) then
          call prterr(lundia, 'U021', 'UpdBaseLyr should be 1-4 in ' &
                    & // trim(filmor))
          call d3stop(1, gdp)
       endif
       !
       txtput1 = 'Base layer composition'
       select case (updbaselyr)
       case (1)
          txtput2 = ' computed separately'
       case (2)
          txtput2 = ' constant'
       case (3)
          txtput2 = ' same as layer above'
       case (4)
          txtput1 = 'Base layer composition and thickness'
          txtput2 = '            constant'
       case default
          txtput2 = ' <unknown>'
       end select
       write(lundia,'(3a)') txtput1, ':', txtput2
       !
       ! Numerical settings
       !
       call prop_get(mor_ptr, 'Numerics', 'MinMassShortWarning', minmass)
       call prop_get(mor_ptr, 'Numerics', 'MaxNumShortWarning' , maxwarn)
    case default
    endselect
    !
    if (allocmorlyr(gdp%gdmorlyr) /= 0) then
       call prterr(lundia, 'U021', 'RDMORLYR: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! underlayer mechanism parameters
    !
    select case (iunderlyr)
    case(2)
       !
       ! Get the following pointers after allocating the memory for the arrays
       !
       istat = bedcomp_getpointer_realfp(gdp%gdmorlyr, 'ThTrLyr', thtrlyr)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Memory problem in RDMORLYR')
          call d3stop(1, gdp)
       endif
       !
       txtput1 = 'Thickness transport layer'
       call prop_get_integer(mor_ptr, 'Underlayer', 'TTLForm', ttlform)
       select case (ttlform)
       case (1)
          !
          ! Transport layer thickness constant in time:
          ! uniform or spatially varying thickness
          !
          ttlfil = ''
          call prop_get_string(mor_ptr, 'Underlayer', 'ThTrLyr', ttlfil)
          !
          ! Intel 7.0 crashes on an inquire statement when file = ' '
          !
          if (ttlfil == ' ') ttlfil = 'dummyname'
          inquire (file = ttlfil, exist = ex)
          !
          if (ex) then
             !
             ! read data from file
             !
             write(lundia,'(3a)') txtput1, ':', ttlfil
             !
             call depfil(lundia    ,error     ,ttlfil    ,fmttmp    ,mmax      , &
                       & nmaxus    ,thtrlyr   ,1         ,1         ,gdp       )
             if (error) then
                call prterr(lundia, 'U021', 'Unable to read transport layer thickness from ' &
                          & // trim(ttlfil))
                call d3stop(1, gdp)
             endif
          else
             ttlfil = ' '
             call prop_get(mor_ptr, 'Underlayer', 'ThTrLyr', thtrlyr(1))
             if (thtrlyr(1) <= 0) then
                call prterr(lundia, 'U021', 'ThTrLyr should be positive in ' &
                          & // trim(filmor))
                call d3stop(1, gdp)
             endif
             do nm = 1, nmmax
                thtrlyr(nm) = thtrlyr(1)
             enddo
             !
             write(lundia,'(2a,e20.4)') txtput1, ':', thtrlyr(1)
          endif
       case (2, 3)
          !
          ! Transport layer thickness proportional to
          ! the water depth (2) or dune height (3)
          !
          call prop_get(mor_ptr, 'Underlayer', 'TTLAlpha', ttlalpha)
          call prop_get(mor_ptr, 'Underlayer', 'TTLMin'  , ttlmin)
          !
          txtput2 = ' max(a*H,b)'
          if (ttlform == 3) then
            txtput2 = ' max(a*Hdune,b)'
            if (.not.lfbedfrm) then
                call prterr(lundia, 'U021', 'TTLForm=3 can only be used when dunes are computed')
                call d3stop(1, gdp)
            endif
          endif
          write(lundia,'(3a)') txtput1, ':', txtput2
          txtput1 = '  a'
          write(lundia,'(2a,e20.4)') txtput1, ':', ttlalpha
          txtput1 = '  b'
          write(lundia,'(2a,e20.4)') txtput1, ':', ttlmin
       case default
          call prterr(lundia, 'U021', 'Invalid transport layer thickness option specified in ' &
                    & // trim(filmor))
          call d3stop(1, gdp)
       end select
       !
       if (exchlyr) then
          istat = bedcomp_getpointer_realfp(gdp%gdmorlyr, 'ThExLyr', thexlyr)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'Memory problem in RDMORLYR')
             call d3stop(1, gdp)
          endif
          !
          txtput1 = 'Thickness exchange layer'
          call prop_get_integer(mor_ptr, 'Underlayer', 'TELForm', telform)
          select case (telform)
          case (1)
             !
             ! Exchange layer thickness constant in time:
             ! uniform or spatially varying thickness
             !
             telfil = ''
             call prop_get_string(mor_ptr, 'Underlayer', 'ThExLyr', telfil)
             !
             ! Intel 7.0 crashes on an inquire statement when file = ' '
             !
             if (telfil == ' ') telfil = 'dummyname'
             inquire (file = telfil, exist = ex)
             !
             if (ex) then
                write(lundia,'(3a)') txtput1, ':', telfil
                !
                ! read data from file
                !
                call depfil(lundia    ,error     ,telfil    ,fmttmp    ,mmax      , &
                          & nmaxus    ,thexlyr   ,1         ,1         ,gdp       )
                if (error) then
                   call prterr(lundia, 'U021', 'Unable to read exchange layer thickness from ' &
                             & // trim(telfil))
                   call d3stop(1, gdp)
                endif
             else
                telfil = ' '
                call prop_get(mor_ptr, 'Underlayer', 'ThExLyr', thexlyr(1))
                if (thexlyr(1) <= 0) then
                   call prterr(lundia, 'U021', 'ThExLyr should be positive in ' &
                             & // trim(filmor))
                   call d3stop(1, gdp)
                endif
                do nm = 1, nmmax
                   thexlyr(nm) = thexlyr(1)
                enddo
                !
                write(lundia,'(2a,e20.4)') txtput1, ':', thexlyr(1)
             endif
          case default
             call prterr(lundia, 'U021', 'Invalid exchange layer thickness option specified in ' &
                       & // trim(filmor))
             call d3stop(1, gdp)
          end select
       endif
       !
    case default
    endselect
    !
    ! Boundary conditions
    !
    do i = 1, size(mor_ptr%child_nodes)
       !
       ! Does mor_ptr contain a child with name 'Boundary' (converted to lower case)?
       !
       morbound_ptr => mor_ptr%child_nodes(i)%node_ptr
       bndname = tree_get_name( morbound_ptr )
       if ( trim(bndname) /= 'boundary') cycle
       bndname = ''
       call prop_get_string(morbound_ptr, '*', 'Name', bndname)
       found = .false.
       do j = 1, nto
          !
          ! Search known boundaries for match
          !
          if (bndname == nambnd(j)) then
             found = .true.
             exit
          endif
       enddo
       if (.not.found) then
          call prterr(lundia, 'U021', &
                    & 'Unknown boundary "'//trim(bndname)//'" in '//trim(filmor))
          call d3stop(1, gdp)          
       endif
       !
       call prop_get_integer(morbound_ptr, '*', 'ICmpCond', cmpbnd(j)%icond)
       if (cmpbnd(j)%icond < 0 .or. cmpbnd(j)%icond > 3) then
          call prterr(lundia, 'U021', &
                    & 'Invalid composition boundary condition at "' &
                    & //trim(bndname)//'" in '//trim(filmor))
          call d3stop(1, gdp)          
       endif
       !
    enddo
    do j = 1, nto
       txtput1 = 'Boundary name'
       write (lundia, '(2a,a20)') txtput1, ':', trim(nambnd(j))
       !
       txtput1 = '  Composition condition prescribed'
       select case(cmpbnd(j)%icond)
       case (0)
          txtput2 = '                free'
          parname = ' '
       case (1)
          txtput2 = '               fixed'
          parname = ' '
       case (2)
          txtput1 = '  Mass fraction condition prescribed'
          txtput2 = '         time series'
          parname = 'mass fraction'
          nval = lsedtot
       case (3)
          txtput1 = '  Volume fraction condition prescribed'
          txtput2 = '         time series'
          parname = 'volume fraction'
          nval = lsedtot
       end select
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       ! Check boundary conditions
       !
       if (parname /= ' ') then
          if (bcmfilnam /= ' ') then
             !
             ! Find entries in table
             !
             call flw_gettable(bcmfile            , nambnd(j)          , trim(parname) , &
                             & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                             & cmpbnd(j)%ibcmt(3) , 1                  , gdp           )
             cmpbnd(j)%ibcmt(4) = 1
             txtput1 = '  Variation along boundary'
             !
             ! Check entries in table
             !
             if (cmpbnd(j)%ibcmt(3) == nval) then
                !
                ! Uniform values
                !
                txtput2 = '             uniform'
                write (lundia, '(3a)') txtput1, ':', txtput2
                i = 0
                do l = 1, lsedtot
                   i = i + 1
                   parnames(i) = trim(parname) // ' ' // trim(namsed(l))
                enddo
                !
                call flw_checktableparnames(bcmfile            , parnames           , &
                                          & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                                          & cmpbnd(j)%ibcmt(3) , gdp                )
                call flw_checktable(bcmfile            , &
                                  & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                                  & cmpbnd(j)%ibcmt(3) , CHKTAB_POSITIVE    , gdp )
             elseif (cmpbnd(j)%ibcmt(3) == nval*2) then
                !
                ! Values at "end A" and "end B"
                !
                txtput2 = '              linear'
                write (lundia, '(3a)') txtput1, ':', txtput2
                i = 0
                do l = 1, lsedtot
                   i = i + 1
                   parnames(i)      = trim(parname) // ' ' // trim(namsed(l)) // ' end A'
                   parnames(nval+i) = trim(parname) // ' ' // trim(namsed(l)) // ' end B'
                enddo
                !
                call flw_checktableparnames(bcmfile            , parnames           , &
                                          & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                                          & cmpbnd(j)%ibcmt(3) , gdp                )
                call flw_checktable(bcmfile            , &
                                  & cmpbnd(j)%ibcmt(1) , cmpbnd(j)%ibcmt(2) , &
                                  & cmpbnd(j)%ibcmt(3) , CHKTAB_POSITIVE    , gdp )
             else
                !
                ! Invalid number of values specified
                !
                errmsg = 'Invalid number of parameters specified for ''' // &
                       & trim(parname) // ''' at ''' // nambnd(j) // ''' in ' // &
                       & trim(bcmfilnam)
                call prterr(lundia, 'U021', errmsg)
                call d3stop(1, gdp)
             endif
          else
             errmsg = 'Missing input file for morphological boundary conditions'
             call prterr(lundia, 'U021', errmsg)
             call d3stop(1, gdp)
          endif
       endif
    enddo
    !
    ! Initial Bed Composition (Overrules)
    !
    flcomp = ''
    call prop_get(mor_ptr, 'Underlayer', 'IniComp', flcomp)
    !
    if (iunderlyr /= 2 .and. flcomp /= ' ') then  
       write(lundia,'(a)') 'WARNING: IniComp keyword only supported for IUnderLyr=2'
       flcomp = ' '
    endif
    txtput1 = 'Initial bed composition'
    if (flcomp == ' ') then
       txtput2 = 'from sediment file'
       write(lundia,'(2a,a20)') txtput1, ':', trim(txtput2)
    else
       write(lundia,'(2a,a20)') txtput1, ':', trim(flcomp)
    endif
    !
    write (lundia, '(a)') '*** End    of underlayer input'
    write (lundia, *)
    !
    ! Set sediment properties for the morphological layers
    !
777 continue
    if (iporosity==0) then
       !
       ! porosity is fraction dependent and included in cdryb densities
       !
       call setbedfracprop(gdp%gdmorlyr, gdp%gdsedpar%sedtyp, &
             & gdp%gdsedpar%sedd50, gdp%gdsedpar%logsedsig, &
             & gdp%gdsedpar%cdryb)
    else
       !
       ! porosity is simulated, the cdryb values are ignored
       !
       call setbedfracprop(gdp%gdmorlyr, gdp%gdsedpar%sedtyp, &
             & gdp%gdsedpar%sedd50, gdp%gdsedpar%logsedsig, &
             & gdp%gdsedpar%rhosol)
       ! gdp%gdsedpar%cdryb = gdp%gdsedpar%rhosol
    endif
end subroutine rdmorlyr
