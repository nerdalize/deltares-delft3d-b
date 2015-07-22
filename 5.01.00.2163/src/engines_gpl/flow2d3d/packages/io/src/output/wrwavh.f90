subroutine wrwavh(lundia    ,error     ,trifil    ,ithisc    , &
                & nostat    ,zhs       ,ztp       , &
                & zdir      ,zrlabd    ,zuwb      ,gdp       )
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
!  $Id: wrwavh.f90 1677 2012-07-01 21:04:29Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrwavh.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the time varying data for waves (6 & 7)
! to the NEFIS HIS-DAT file.
! Output is performed conform the times of history
! file and only in case wave.eq.TRUE.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical              , pointer :: first
    integer              , pointer :: celidt
    type (nefiselement)  , pointer :: nefiselem
!
! Global variables
!
    integer                    , intent(in)  :: ithisc
    integer                                  :: ithisi !  Description and declaration in inttim.igs
    integer                                  :: itstrt !  Description and declaration in inttim.igs
    integer                                  :: lundia !  Description and declaration in inout.igs
    integer                                  :: nostat !  Description and declaration in dimens.igs
    integer                                  :: ntruv  !  Description and declaration in dimens.igs
    logical                    , intent(out) :: error
    real(fp), dimension(nostat)              :: zdir
    real(fp), dimension(nostat)              :: zhs
    real(fp), dimension(nostat)              :: zrlabd
    real(fp), dimension(nostat)              :: ztp
    real(fp), dimension(nostat)              :: zuwb
    character(60)              , intent(in)  :: trifil
!
! Local variables
!
    integer                 :: fds
    integer                 :: ierror        ! Local error flag for NEFIS files 
    integer                 :: n 
    integer, dimension(1)   :: idummy
    integer, dimension(3,5) :: uindex
    integer, external       :: getelt
    integer, external       :: putelt
    integer, external       :: inqmxi
    integer, external       :: clsnef
    integer, external       :: open_datdef
    integer, external       :: neferr
    character(16)           :: grnam4
    character(16)           :: grnam5
    character(256)          :: errmsg
    character(60)           :: filnam
    character(1024)         :: error_string
!
! Data statements
!
    data grnam4/'his-infwav-serie'/
    data grnam5/'his-wav-series'/
!
!! executable statements -------------------------------------------------------
!
    !
    nefiselem => gdp%nefisio%nefiselem(nefiswrwavhinf)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    errmsg = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    if (first) then
       !
       ! Set up the element characteristics
       !
       !
       ! his-infwav-serie
       !
       call addelm(nefiswrwavhinf,'ITHISW',' ','[   -   ]','INTEGER',4     , &
         & 'timestep number (ITHISW*DT*TUNIT := time in sec from ITDATE)  ', &
         &  1         ,1         ,0         ,0         ,0         ,0       , &
         &  lundia    ,gdp       )
       call defnewgrp(nefiswrwavhinf ,filnam    ,grnam4   ,gdp)
       !
       ! his-sed-series: stations
       !
       if (nostat>0) then
         call addelm(nefiswrwavh,'ZHS',' ','[   M   ]','REAL',4              , &
           & 'Significant wave height at station                            ', &
           &  1         ,nostat    ,0         ,0         ,0         ,0       , &
           &  lundia    ,gdp       )
         call addelm(nefiswrwavh,'ZTP',' ','[   S   ]','REAL',4              , &
           & 'Peak wave period at station                                   ', &
           &  1         ,nostat    ,0         ,0         ,0         ,0       , &
           &  lundia    ,gdp       )
         call addelm(nefiswrwavh,'ZDIR',' ','[  DEG  ]','REAL',4             , &
           & 'Direction waves are coming from at station (CW from North)    ', &
           &  1         ,nostat    ,0         ,0         ,0         ,0       , &
           &  lundia    ,gdp       )
         call addelm(nefiswrwavh,'ZRLABD',' ','[   M   ]','REAL',4           , &
           & 'Wave length at station                                        ', &
           &  1         ,nostat    ,0         ,0         ,0         ,0       , &
           &  lundia    ,gdp       )
         call addelm(nefiswrwavh,'ZUWB',' ','[  M/S  ]','REAL',4             , &
           & 'Peak near-bed orbital speed at station (Hs, linear theory)    ', &
           &  1         ,nostat    ,0         ,0         ,0         ,0       , &
           &  lundia    ,gdp       )
       endif
       !
       call defnewgrp(nefiswrwavh ,filnam    ,grnam5   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrwavhinf)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    endif
    !
    ierror = open_datdef(filnam   ,fds      )
    if (ierror/= 0) goto 9999
    if (first) then
       !
       ! end of initialization, don't come here again
       !
       ierror = inqmxi(fds, grnam4, celidt)
       first = .false.
    endif
    !
    ! Writing of output on every ithisw
    !
    celidt = celidt + 1
    !
    ! group 4: element 'ITHISW'
    !
    idummy(1)   = ithisc
    uindex(1,1) = celidt
    uindex(2,1) = celidt
    !
    ! celidt is obtained by investigating group his-infwav-serie, identified
    ! with nefiswrwavhinf.
    ! Group his-wav-series, identified with nefiswrwavh, must use the same
    ! value for celidt.
    ! Easy solution:
    gdp%nefisio%nefiselem(nefiswrwavh)%celidt = celidt
    ! Neat solution in pseudo code:
    ! subroutine wrwavh
    !    integer :: celidt
    !    call wrwavhinf(celidt)
    !    call wrwavhdat(celidt)
    ! end subroutine
    !
    ierror     = putelt(fds, grnam4, 'ITHISW', uindex, 1, idummy)
    if (ierror/= 0) goto 9999
    !
    ! his-wav-series: stations
    !
    if (nostat > 0) then
       !
       ! group 5: element 'ZHS'
       !
       call sbuff_checksize(nostat)
       do n = 1, nostat
          sbuff(n) = real(zhs(n),sp)
       enddo
       ierror = putelt(fds, grnam5, 'ZHS', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! group 5: element 'ZTP'
       !
       call sbuff_checksize(nostat)
       do n = 1, nostat
          sbuff(n) = real(ztp(n),sp)
       enddo
       ierror = putelt(fds, grnam5, 'ZTP', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! group 5: element 'ZDIR'
       !
       call sbuff_checksize(nostat)
       do n = 1, nostat
          sbuff(n) = real(zdir(n),sp)
       enddo
       ierror = putelt(fds, grnam5, 'ZDIR', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! group 5: element 'ZRLABD'
       !
       call sbuff_checksize(nostat)
       do n = 1, nostat
          sbuff(n) = real(zrlabd(n),sp)
       enddo
       ierror = putelt(fds, grnam5, 'ZRLABD', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! group 5: element 'ZUWB'
       !
       call sbuff_checksize(nostat)
       do n = 1, nostat
          sbuff(n) = real(zuwb(n),sp)
       enddo
       ierror = putelt(fds, grnam5, 'ZUWB', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
    !
    ierror = clsnef(fds)
    !
    ! write error message if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
 9999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrwavh
