subroutine reafou(error     ,lundia    ,lunfou    ,filfou    ,kmax      , &
                & lstsc     ,lsal      ,ltem      ,nofou     ,gdp       )
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
!  $Id: reafou.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/reafou.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Read fourier input file and stores the
!                variables necessary for the analysis in
!                arrays.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                      , pointer :: tstart
    real(fp)                      , pointer :: tstop
    real(fp)                      , pointer :: dt
    integer        , dimension(:) , pointer :: fconno
    integer        , dimension(:) , pointer :: flayno
    integer        , dimension(:) , pointer :: fnumcy
    integer                       , pointer :: fouwrt    
    integer        , dimension(:) , pointer :: ftmsto
    integer        , dimension(:) , pointer :: ftmstr
    real(fp)       , dimension(:) , pointer :: fknfac
    real(fp)       , dimension(:) , pointer :: foufas
    real(fp)       , dimension(:) , pointer :: fv0pu
    character(1)   , dimension(:) , pointer :: fouelp
    character(16)  , dimension(:) , pointer :: founam
    character(1)   , dimension(:) , pointer :: foutyp
!
! Local parameters
!
    integer, parameter :: maxvld = 40
!
! Global variables
!
    integer                          , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                          , intent(in) :: lsal   !  Description and declaration in dimens.igs
    integer                          , intent(in) :: lstsc  !  Description and declaration in dimens.igs
    integer                          , intent(in) :: ltem   !  Description and declaration in dimens.igs
    integer                                       :: lundia !  Description and declaration in inout.igs
    integer                          , intent(in) :: lunfou !!  Unit number fourier input file
    integer                          , intent(in) :: nofou  !  Description and declaration in dimens.igs
    logical                                       :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                  :: filfou !!  File name for fourier analysis input
!
! Local variables
!
    integer                             :: i          ! Counter 
    integer                             :: ifou       ! Counter 
    integer                             :: irelp
    integer                             :: it
    integer                             :: lfile      ! Length of file name 
    integer                             :: linenumber ! Line number in Fourier input file
    integer                             :: nopos      ! Used for format free reading 
    integer                             :: nveld      ! Used for format free reading 
    integer        , dimension(maxvld)  :: il         ! Used for format free reading 
    integer        , dimension(maxvld)  :: ir         ! Used for format free reading 
    logical                             :: dtn
    real(fp)                            :: rstart     ! Start time for fourier analysis 
    real(fp)                            :: rstop      ! Stop  time for fourier analysis 
    real(fp)                            :: t
    character(4)                        :: cdummy     ! Help string to read FOUELP 
    character(26)                       :: errmsg     ! Character var. containing the error message to be written to file.
                                                      ! The message depends on the error. 
    character(7)                        :: fmt        ! Used for format free reading 
    character(300)                      :: message
    character(132)                      :: record     ! Used for format free reading 
!
!
!! executable statements -------------------------------------------------------
!
!
    tstart    => gdp%gdexttim%tstart
    tstop     => gdp%gdexttim%tstop
    dt        => gdp%gdexttim%dt
    fknfac    => gdp%gdfourier%fknfac
    foufas    => gdp%gdfourier%foufas
    fv0pu     => gdp%gdfourier%fv0pu
    fconno    => gdp%gdfourier%fconno
    flayno    => gdp%gdfourier%flayno
    fnumcy    => gdp%gdfourier%fnumcy
    fouwrt    => gdp%gdfourier%fouwrt
    ftmsto    => gdp%gdfourier%ftmsto
    ftmstr    => gdp%gdfourier%ftmstr
    fouelp    => gdp%gdfourier%fouelp
    founam    => gdp%gdfourier%founam
    foutyp    => gdp%gdfourier%foutyp
    !
    error = .false.
    ifou = 1
    do i = 1, nofou
       flayno(i) = 1
       fconno(i) = 1
       foutyp(i) = 'n'
    enddo
    !
    !-------define length of file name
    !
    call noextspaces(filfou, lfile)
    !
    errmsg = 'Times in file ' // filfou(1:lfile)
    cdummy = ' '
    !
    linenumber = 0
    !
    !-----reading file
    !
    ! -->
   20 continue
    read (lunfou, '(a)') record
    !
    linenumber = linenumber + 1
    !
    call small(record, 132)
    call regel(record, il , ir, maxvld, nveld, error)
    if (error) goto 9999
    !
    if (record(il(1):il(1))=='*' .or. nveld==0) goto 20
    ! <--
    !
    !-------determine array names and type (scalar or vectorial) for
    !       fourier analysis
    !
    founam(ifou) = record(il(1):il(1) + 1)
    !
    if (founam(ifou)=='wl') then
       founam(ifou) = 's1              '
       foutyp(ifou) = 's'
    elseif (founam(ifou)=='uv') then
       founam(ifou) = 'u1              '
       founam(ifou + 1) = 'v1              '
       foutyp(ifou) = 'v'
    elseif (founam(ifou)=='qf') then
       founam(ifou) = 'qxk             '
       founam(ifou + 1) = 'qyk             '
       foutyp(ifou) = 'v'
    elseif (founam(ifou)=='bs') then
       founam(ifou) = 'taubpu          '
       founam(ifou + 1) = 'taubpv          '
       foutyp(ifou) = 'v'
    elseif (founam(ifou)=='ct') then
       if (ltem/=0) then
          founam(ifou) = 'r1              '
          foutyp(ifou) = 's'
          fconno(ifou) = ltem
       else
          call prterr(lundia, 'F002', ' ')
          !
          error = .true.
          goto 9999
       endif
    elseif (founam(ifou)=='cs') then
       if (lsal/=0) then
          founam(ifou) = 'r1              '
          foutyp(ifou) = 's'
          fconno(ifou) = lsal
       else
          call prterr(lundia, 'F003', ' ')
          !
          error = .true.
          goto 9999
       endif
    else
       read (founam(ifou)(2:2), '(i1)') fconno(ifou)
       fconno(ifou) = fconno(ifou) + max(lsal, ltem)
       if (fconno(ifou)>lstsc) then
          write (cdummy(1:1), '(i1)') fconno(ifou) - max(lsal, ltem)
          call prterr(lundia, 'F004', cdummy(1:1))
          !
          error = .true.
          goto 9999
       endif
       founam(ifou) = 'r1             '
       foutyp(ifou) = 's'
    endif
    !
    !-------read start time, stop time, number of cycles
    !       determine corresponding integer time step numbers and frequency
    !
    fmt = '(f    )'
    !
    nopos = ir(2) - il(2) + 1
    !
    if (nopos<10) then
       write (fmt(3:3), '(i1)') nopos
       write (fmt(4:5), '(a2)') '.0'
    else
       write (fmt(3:4), '(i2)') nopos
       write (fmt(5:6), '(a2)') '.0'
    endif
    !
    read (record(il(2):ir(2)), fmt) rstart
    !
    ftmstr(ifou) = nint(rstart/dt)
    !
    if (dtn(ftmstr(ifou), rstart, dt)) then
       call prterr(lundia, 'U044', errmsg)
       error = .true.
       goto 9999
    endif
    !
    !
    if (rstart<tstart) then
       call prterr(lundia, 'F005', ' ')
       !
       error = .true.
       goto 9999
    endif
    !
    nopos = ir(3) - il(3) + 1
    !
    if (nopos<10) then
       write (fmt(3:3), '(i1)') nopos
       write (fmt(4:5), '(a2)') '.0'
    else
       write (fmt(3:4), '(i2)') nopos
       write (fmt(5:6), '(a2)') '.0'
    endif
    !
    read (record(il(3):ir(3)), fmt) rstop
    !
    ftmsto(ifou) = nint(rstop/dt)
    if (dtn(ftmsto(ifou), rstop, dt)) then
       call prterr(lundia, 'U044', errmsg)
       error = .true.
       goto 9999
    endif
    !
    !
    if (rstop>tstop) then
       call prterr(lundia, 'F006', ' ')
       !
       error = .true.
       goto 9999
    endif
    !
    ! Fouwrt catches the end of all fourier analyses
    !
    fouwrt = max(fouwrt,(ftmsto(ifou)-1))
    !
    fmt = '(i    )'
    nopos = ir(4) - il(4) + 1
    !
    if (nopos<10) then
       write (fmt(3:3), '(i1)') nopos
    else
       write (fmt(3:4), '(i2)') nopos
    endif
    !
    read (record(il(4):ir(4)), fmt) fnumcy(ifou)
    !
    if (fnumcy(ifou)==0) then
       foufas(ifou) = 0.
    else
       foufas(ifou) = 2.*pi*real(fnumcy(ifou),fp)/real(ftmsto(ifou) - ftmstr(ifou),fp)
    endif
    !
    !-------read nodal amplifications and phase shifts for comparison
    !       with cotidal maps
    !
    fmt = '(f    )'
    nopos = ir(5) - il(5) + 1
    !
    if (nopos<10) then
       write (fmt(3:3), '(i1)') nopos
       write (fmt(4:5), '(a2)') '.0'
    else
       write (fmt(3:4), '(i2)') nopos
       write (fmt(4:5), '(a2)') '.0'
    endif
    !
    read (record(il(5):ir(5)), fmt) fknfac(ifou)
    !
    fmt = '(f    )'
    nopos = ir(6) - il(6) + 1
    !
    if (nopos<10) then
       write (fmt(3:3), '(i1)') nopos
       write (fmt(4:5), '(a2)') '.0'
    else
       write (fmt(3:4), '(i2)') nopos
       write (fmt(4:5), '(a2)') '.0'
    endif
    !
    read (record(il(6):ir(6)), fmt) fv0pu(ifou)
    !
    if (fv0pu(ifou)<0.) fv0pu(ifou) = fv0pu(ifou) + 360.
    fv0pu(ifou) = mod(fv0pu(ifou), 360.0_fp)
    !
    irelp = 7
    !
    if (founam(ifou)(1:2)/='s1' .and. founam(ifou)(1:3)/='tau') then
       fmt = '(i    )'
       nopos = ir(7) - il(7) + 1
       !
       if (nopos<10) then
          write (fmt(3:3), '(i1)') nopos
       else
          write (fmt(3:4), '(i2)') nopos
       endif
       !
       read (record(il(7):ir(7)), fmt) flayno(ifou)
       if (flayno(ifou)>kmax) then
          call prterr(lundia, 'F007', ' ')
          !
          error = .true.
          goto 9999
       endif
       irelp = irelp + 1
    endif
    !
    !-------Elliptic parameters requested / MAX - MIN added
    !
    fouelp(ifou) = 'n'
    if (nveld>=irelp) then
       cdummy = record(il(irelp):ir(irelp))
       !
       !---------check for MAX and or MIN before Y/N
       !
       if (cdummy=='max') then
          fouelp(ifou) = 'x'
          if (fnumcy(ifou)>0) then
             fnumcy(ifou) = 0
             foufas(ifou) = 0.
             call prterr(lundia, 'F008', 'max')
          !
          endif
       elseif (cdummy=='min') then
          fouelp(ifou) = 'i'
          if (fnumcy(ifou)>0) then
             fnumcy(ifou) = 0
             foufas(ifou) = 0.
             call prterr(lundia, 'F008', 'min')
          !
          endif
       !
       !-------elliptic parameters requested only for all foutyp='v'
       !
       elseif (foutyp(ifou)=='v') then
          if (cdummy(1:1)=='n') then
             fouelp(ifou) = 'n'
          elseif (cdummy(1:1)=='y') then
             fouelp(ifou) = 'y'
          elseif (cdummy /= 'mean') then
             write (message, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                   & ': expecting min, max, mean, yes or no, instead of ', trim(cdummy)
             call prterr(lundia, 'P004', trim(message))
             !
             error = .true.
             goto 9999
          endif
       else
          if (cdummy /= 'mean') then
             write (message, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                   & ': expecting min, max or mean, instead of ', trim(cdummy)
             call prterr(lundia, 'P004', trim(message))
             !
             error = .true.
             goto 9999
          endif
       endif
    endif
    !
    if (foutyp(ifou)=='v') then
       ifou = ifou + 1
       foutyp(ifou) = 'v'
       ftmstr(ifou) = ftmstr(ifou - 1)
       ftmsto(ifou) = ftmsto(ifou - 1)
       fnumcy(ifou) = fnumcy(ifou - 1)
       flayno(ifou) = flayno(ifou - 1)
       fconno(ifou) = fconno(ifou - 1)
       foufas(ifou) = foufas(ifou - 1)
       fknfac(ifou) = fknfac(ifou - 1)
       fv0pu (ifou) = fv0pu(ifou - 1)
       fouelp(ifou) = fouelp(ifou - 1)
    endif
    !
    ifou = ifou + 1
    !
    if (ifou<=nofou) goto 20
    ! <--
    !
 9999 continue
end subroutine reafou
