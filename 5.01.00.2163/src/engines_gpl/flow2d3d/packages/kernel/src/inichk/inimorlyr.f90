subroutine inimorlyr(flsdbd    ,sdbuni    ,inisedunit,cdryb     , &
                   & lsedtot   ,mmax      ,nmax      ,nmaxus    ,nmmax     , &
                   & lundia    ,error     ,kcs       ,gdp  )
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
!  $Id: inimorlyr.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/inichk/inimorlyr.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Initialisation underlayer bookkeeping system
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)         , dimension(:)     , pointer :: dpsed
    real(fp)         , dimension(:)     , pointer :: dzi
    real(fp)         , dimension(:)     , pointer :: thexlyr
    real(fp)         , dimension(:)     , pointer :: thtrlyr
    real(fp)         , dimension(:)     , pointer :: rhosol
    logical                             , pointer :: exchlyr
    character(256)                      , pointer :: flcomp
    integer                             , pointer :: i_restart
    character(256)                      , pointer :: restid
!
! Global variables
!
    integer                                         , intent(in)  :: lsedtot
    integer                                                       :: lundia     !  Description and declaration in inout.igs
    integer                                         , intent(in)  :: mmax       !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmax       !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmaxus     !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nmmax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kcs        !  Description and declaration in esm_alloc_int.f90
    logical                                                       :: error      !  Flag=TRUE if an error is encountered
    real(fp)      , dimension(lsedtot)              , intent(in)  :: cdryb      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(lsedtot)              , intent(in)  :: sdbuni     !  Description and declaration in esm_alloc_real.f90
    character(10) , dimension(lsedtot)              , intent(in)  :: inisedunit !  Description and declaration in esm_alloc_char.f90
    character(256), dimension(lsedtot)              , intent(in)  :: flsdbd     !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                       :: icx
    integer                                       :: icy
    integer                             , pointer :: iporosity
    integer                                       :: ised
    integer                             , pointer :: iunderlyr
    integer                                       :: istat
    integer                                       :: k
    integer                             , pointer :: nlyr
    integer                                       :: nm
    integer                                       :: nm2
    integer                                       :: nmlb
    integer                                       :: nmaxddb
    logical                                       :: err
    logical                                       :: success
    character(11)                                 :: fmttmp   ! Format file ('formatted  ')
    character(300)                                :: message
    real(fp)         , dimension(lsedtot)         :: mfrac
    real(fp)                                      :: mfracsum
    real(fp)                                      :: poros
    real(fp)                                      :: svf
    real(prec)       , dimension(:,:)   , pointer :: bodsed
    real(fp)         , dimension(:,:,:) , pointer :: msed
    real(fp)         , dimension(:,:)   , pointer :: thlyr
    real(fp)         , dimension(:,:)   , pointer :: svfrac
!
!! executable statements -------------------------------------------------------
!
    i_restart          => gdp%gdrestart%i_restart
    restid             => gdp%gdrestart%restid
    flcomp             => gdp%gdmorpar%flcomp
    rhosol             => gdp%gdsedpar%rhosol
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'iunderlyr', iunderlyr)
    if (istat==0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'nlyr'   , nlyr)
    if (istat==0) istat = bedcomp_getpointer_realprec(gdp%gdmorlyr, 'bodsed', bodsed)
    if (istat==0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'iporosity', iporosity)
    if (iunderlyr==2) then
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'msed'     , msed)
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'thlyr'    , thlyr)
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'svfrac'   , svfrac)
    endif
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Memory problem in INIMORLYR')
       call d3stop(1, gdp)
    endif
    !
    nmaxddb = gdp%d%nub - gdp%d%nlb + 1
    nmlb    = gdp%d%nmlb
    fmttmp  = 'formatted'
    !
    icx = 1
    icy = nmaxddb
    !
    istat   = 0
    success = .false.
    select case(iunderlyr)
    case(2)
       !
       ! initialise bookkeeping system
       !
       if (restid /= ' ') then
          !
          ! Try restart
          !
          call restart_lyrs ( &
                 & error     ,restid    ,i_restart ,msed      , &
                 & thlyr     ,lsedtot   ,nmaxus    ,cdryb     , &
                 & mmax      ,nlyr      ,success   ,svfrac    , &
                 & iporosity ,gdp       )
          if (success) goto 9999
       endif
    case default
       !
       ! nothing to do, using bodsed as uniformly mixed sediment
       !
    endselect
    !
    ! Start filling array BODSED
    !
    if (restid /= ' ') then
       !
       ! Try restart
       !
       call restart_bodsed ( &
              & error     ,restid    ,i_restart ,bodsed    , &
              & lsedtot   ,nmaxus    ,mmax      ,success   ,gdp       )
    endif
    if (.not.success) then
       !
       ! Try to fill BODSED with input values
       ! flag error must be set to false
       !
       error = .false.
       do ised = 1, lsedtot
          if (flsdbd(ised) == ' ') then
             !
             ! Uniform data has been specified
             !
             do nm = 1, nmmax
                bodsed(ised, nm) = real(sdbuni(ised),prec)
             enddo
          else
             !
             ! Space varying data has been specified
             ! Use routine that also read the depth file to read the data
             !
             if (prec == hp) then
                call depfil_double(lundia    ,error     ,flsdbd(ised)         , &
                                 & fmttmp    ,mmax      ,nmaxus    ,bodsed    , &
                                 & lsedtot   , ised     ,gdp       )
             else
                call depfil(lundia    ,error     ,flsdbd(ised)         , &
                          & fmttmp    ,mmax      ,nmaxus    ,bodsed    , &
                          & lsedtot   ,ised      ,gdp       )
             endif
             if (error) goto 9999
          endif
       enddo
       if (iporosity == 0) then
          do ised = 1, lsedtot
             if (inisedunit(ised) == 'm') then
                do nm = 1, nmmax
                   bodsed(ised, nm) = bodsed(ised, nm) * cdryb(ised)
                enddo
             else
                !
                ! inisedunit(ised) = kg/m2
                ! no conversion needed
                !
             endif
          enddo
       else
          do ised = 2, lsedtot
             if (inisedunit(ised) /= inisedunit(1)) then
                call prterr(lundia, 'U021', 'All sediment fields in the same layer should have unit.')
                call d3stop(1, gdp)
                error = .true.
                goto 9999
             endif
          enddo
          if (inisedunit(1) == 'm') then
             !
             ! all input specified as thickness
             !
             do nm = 1, nmmax
                mfracsum = 0.0_fp
                do ised = 1, lsedtot
                   mfrac(ised) = bodsed(ised, nm) * rhosol(ised)
                   mfracsum = mfracsum + mfrac(ised)
                enddo
                if (mfracsum > 0.0_fp) then
                   do ised = 1, lsedtot
                      mfrac(ised) = mfrac(ised) / mfracsum
                   enddo
                   !
                   call getporosity(gdp%gdmorlyr, mfrac, poros)
                   svf = 1.0_fp - poros
                else
                   svf = 1.0_fp
                endif
                !
                do ised = 1, lsedtot
                   bodsed(ised, nm) = bodsed(ised, nm) * svf * rhosol(ised)
                enddo
             enddo
          else
             !
             ! inisedunit(1) = kg/m2
             ! no conversion needed
             !
          endif
       endif
       !
       ! Check validity of input data
       !
       do nm = 1, nmmax
          if (kcs(nm) == 1) then
             !
             ! At an internal point the composition is important.
             ! Check the values carefully before continuing.
             !
             do ised = 1, lsedtot
                if (bodsed(ised, nm) < 0.0) then
                   write (message,'(a,i2,a,a,a,i0)')  &
                       & 'Negative sediment thickness ',ised,' in file ', &
                       & trim(flsdbd(ised)),' at nm=',nm
                   call prterr(lundia, 'U021',trim(message))
                   call d3stop(1, gdp)
                endif
             enddo
          elseif (kcs(nm) == 2) then
             !
             ! At an open boundary the composition is also important
             ! but if the input is not valid, mark the data as dummy data:
             ! the data will be overwritten with data coming from the
             ! neighbouring internal point.
             !
             err = .false.
             do ised = 1, lsedtot
                if (bodsed(ised, nm)<0.0) err=.true.
             enddo
             if (err) then
                !
                ! set dummy flag
                !
                bodsed(1, nm) = -1.0
             endif
          else
             !
             ! Point that will never be used: don't care about the values.
             ! Just replace whatever was read by something valid.
             !
             do ised = 1, lsedtot
                bodsed(ised, nm) = 0.0
             enddo
          endif
       enddo
       !
       ! Copy BODSED data to open boundary points that have not
       ! yet been assigned valid data.
       !
       do nm = 1, nmmax
          if (kcs(nm)==2 .and. bodsed(1, nm)<0.0) then
             if (kcs(nm-icx) == 1) then
                ! ndm
                nm2 = nm-icx
             elseif (kcs(nm+icx) == 1) then
                ! num
                nm2 = nm+icx
             elseif (kcs(nm-icy) == 1) then
                ! nmd
                nm2 = nm-icy
             else
                ! nmu
                nm2 = nm+icy
             endif
             do ised = 1,lsedtot
                bodsed(ised, nm) = bodsed(ised, nm2)
             enddo
          endif
       enddo
    endif
    !
    ! Use BODSED: compute DPSED and as needed transfer information from BODSED to other arrays
    !
    call bedcomp_use_bodsed(gdp%gdmorlyr)
    !
    select case(iunderlyr)
    case(2)
       if (flcomp /= ' ') then
          !
          ! Read the data from the initial composition file.
          !
          call rdinimorlyr(flcomp    ,msed      ,thlyr     ,cdryb     , &
                         & lsedtot   ,mmax      ,nlyr      ,nmax      , &
                         & nmaxus    ,nmmax     ,lundia    ,kcs       , &
                         & icx       ,icy       ,svfrac    ,iporosity , &
                         & gdp       )
       endif
    endselect
 9999 continue
end subroutine inimorlyr
