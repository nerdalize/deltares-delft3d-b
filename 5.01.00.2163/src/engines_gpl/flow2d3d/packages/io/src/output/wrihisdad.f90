subroutine wrihisdad(lundia    ,error     ,trifil    ,itdate    , &
                   & tunit     ,dt        ,lsedtot   ,gdp       )
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
!  $Id: wrihisdad.f90 1833 2012-09-12 22:29:52Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrihisdad.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the initial Dredge and Dump group to HIS-DAT
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)      , dimension(:,:) , pointer :: link_percentage
    real(fp)      , dimension(:)   , pointer :: link_distance
    integer                        , pointer :: nadred
    integer                        , pointer :: nadump
    integer                        , pointer :: nasupl
    integer                        , pointer :: nalink
    integer       , dimension(:,:) , pointer :: link_def
    character( 80), dimension(:)   , pointer :: dredge_areas
    character( 80), dimension(:)   , pointer :: dump_areas
    logical                        , pointer :: first
    type (nefiselement)            , pointer :: nefiselem
!
! Global variables
!
    integer     , intent(in)  :: itdate  !  Description and declaration in exttim.igs
    integer     , intent(in)  :: lsedtot !  Description and declaration in exttim.igs
    integer                   :: lundia  !  Description and declaration in inout.igs
    logical     , intent(out) :: error
    real(fp)    , intent(in)  :: dt      !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: tunit   !  Description and declaration in exttim.igs
    character(*), intent(in)  :: trifil
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: l
    integer                                       :: j
    integer                                       :: k
    integer                                       :: ierror ! Local errorflag for NEFIS files
    integer       , dimension(2)                  :: ival   ! Local array for writing ITDATE and
    character(16)                                 :: grnam
    character(256)                                :: filnam
    character(256)                                :: errmsg
    integer    , dimension(3,5)                   :: uindex
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: putelt
    integer                        , external     :: neferr
!
! Data statements
!
    data grnam/'his-dad-const'/
!
!! executable statements -------------------------------------------------------
!
    link_percentage   => gdp%gddredge%link_percentage
    link_distance     => gdp%gddredge%link_distance
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    nalink            => gdp%gddredge%nalink
    link_def          => gdp%gddredge%link_def
    dredge_areas      => gdp%gddredge%dredge_areas
    dump_areas        => gdp%gddredge%dump_areas
    nefiselem => gdp%nefisio%nefiselem(nefiswrihisdad)
    first             => nefiselem%first
    !
    ! Initialize local variables
    !
    ierror = 0
    filnam = trifil(1:3) // 'h' // trifil(5:)
    errmsg = ' '
    !
    if (first .and. inode == master) then
       !
       ! Set up the element chracteristics
       !
       call addelm(nefiswrihisdad,'ITDATE',' ','[YYYYMMDD]','INTEGER',4     , &
          & 'Initial date (input) & time (default 00:00:00)                ', &
          & 1, 2, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrihisdad,'TUNIT',' ','[   S   ]','REAL',4          , &
          & 'Time scale related to seconds                                 ', &
          & 1, 1, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrihisdad,'DT',' ','[   -   ]','REAL',4             , &
          & 'Time step (DT*TUNIT sec)                                      ', &
          & 1, 1, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrihisdad,'DREDGE_AREAS',' ','[   -   ]','CHARACTER',80, &
          & 'Names identifying dredge areas/dredge polygons                ', &
          & 1, nadred+nasupl, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrihisdad,'DUMP_AREAS',' ','[   -   ]','CHARACTER',80, &
          & 'Names identifying dump areas/dump polygons                    ', &
          & 1, nadump, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrihisdad,'LINK_DEF',' ','[   -   ]','INTEGER',4    , &
          & 'Actual transports from dredge(1st col) to dump(2nd col) areas ', &
          & 2, nalink, 2, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrihisdad,'LINK_PERCENTAGES',' ','[   %   ]','REAL',4, &
          & 'Distribution of dredged material from dredge to dump areas    ', &
          & 2, nalink, lsedtot, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrihisdad,'LINK_DISTANCE',' ','[   M   ]','REAL',4  , &
          & 'Link Distance between dredge and dump areas                   ', &
          & 2, nalink, 1, 0, 0, 0, lundia, gdp)
       !
       call defnewgrp(nefiswrihisdad ,filnam    ,grnam   ,gdp)
       first = .false.
    endif
    !
    ! initialize group index
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    if (inode == master) then
       ierror = open_datdef(filnam   ,fds      )
       if (ierror /= 0) goto 9999
       !
       ! element 'ITDATE'
       !
       ival(1) = itdate
       ival(2) = 000000
       ierror = putelt(fds, grnam, 'ITDATE', uindex, 1, ival)
       if (ierror/= 0) goto 9999
       !
       ! element 'TUNIT'
       !
       call sbuff_checksize(1)
       sbuff(1) = tunit
       ierror = putelt(fds, grnam, 'TUNIT', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'DT'
       !
       call sbuff_checksize(1)
       sbuff(1) = dt
       ierror = putelt(fds, grnam, 'DT', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'DREDGE_AREAS'
       !
       ierror = putelt(fds, grnam, 'DREDGE_AREAS', uindex, 1, dredge_areas)
       if (ierror/= 0) goto 9999
       !
       ! element 'DUMP_AREAS'
       !
       ierror = putelt(fds, grnam, 'DUMP_AREAS', uindex, 1, dump_areas)
       if (ierror/= 0) goto 9999
       !
       ! element 'LINK_DEF'
       !
       ierror = putelt(fds, grnam, 'LINK_DEF', uindex, 1, link_def)
       if (ierror/= 0) goto 9999
       !
       ! element 'LINK_PERCENTAGES'
       !
       call sbuff_checksize(nalink*lsedtot)
       do l = 1, lsedtot
          do i = 1, nalink
             sbuff(i+(l-1)*nalink) = link_percentage(i,l)
          enddo
       enddo
       ierror = putelt(fds, grnam, 'LINK_PERCENTAGES', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'LINK_DISTANCE'
       !
       call sbuff_checksize(nalink)
       do i = 1, nalink
          sbuff(i) = link_distance(i)
       enddo
       ierror = putelt(fds, grnam, 'LINK_DISTANCE', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ierror = clsnef(fds)
       !
       ! write error message if error occured and set error= .true.
       ! the files will be closed in clsnef (called in triend)
       !
9999   continue
       if (ierror /= 0) then
          ierror = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error= .true.
       endif
    endif
end subroutine wrihisdad
