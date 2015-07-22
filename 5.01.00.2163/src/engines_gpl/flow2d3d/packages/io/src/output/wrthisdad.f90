subroutine wrthisdad(lundia    ,error     ,trifil    ,ithisc    , &
                   & lsedtot   ,gdp       )
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
!  $Id: wrthisdad.f90 1843 2012-09-14 11:22:48Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrthisdad.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the time varying Dredge and Dump group to the NEFIS HIS-DAT file
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
    real(fp)            , dimension(:,:) , pointer :: link_sum
    real(fp)            , dimension(:,:) , pointer :: voldred
    real(fp)            , dimension(:)   , pointer :: totvoldred
    real(fp)            , dimension(:,:) , pointer :: voldump
    real(fp)            , dimension(:)   , pointer :: totvoldump
    integer             , dimension(:)   , pointer :: ndredged
    integer             , dimension(:)   , pointer :: nploughed
    integer                              , pointer :: nadred
    integer                              , pointer :: nadump
    integer                              , pointer :: nasupl
    integer                              , pointer :: nalink
    integer                              , pointer :: ntimaccum
    character(24)                        , pointer :: date_time
    logical                              , pointer :: first
    integer                              , pointer :: celidt
    type (nefiselement)                  , pointer :: nefiselem
!
! Global variables
!
    integer          , intent(in)  :: ithisc
    integer          , intent(in)  :: lsedtot
    integer                        :: lundia  !  Description and declaration in inout.igs
    logical          , intent(out) :: error
    character(*)     , intent(in)  :: trifil
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: l
    integer                                       :: ierror      ! Local errorflag for NEFIS files
    integer    , dimension(1)                     :: idummy      ! Help array to read/write Nefis files
    integer    , dimension(3,5)                   :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: inqmxi
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    real(fp)                                      :: tfrac
    character(16)                                 :: grnam
    character(256)                                :: filnam
    character(256)                                :: errmsg
    character(24) , dimension(1)                  :: datetimearr ! putgtc expects an array
!
! Data statements
!
    data grnam/'his-dad-series'/
!
!! executable statements -------------------------------------------------------
!
    link_sum          => gdp%gddredge%link_sum
    voldred           => gdp%gddredge%voldred
    totvoldred        => gdp%gddredge%totvoldred
    voldump           => gdp%gddredge%voldump
    totvoldump        => gdp%gddredge%totvoldump
    ndredged          => gdp%gddredge%ndredged
    nploughed         => gdp%gddredge%nploughed
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    nalink            => gdp%gddredge%nalink
    ntimaccum         => gdp%gddredge%ntimaccum
    date_time         => gdp%gdinttim%date_time
    nefiselem => gdp%nefisio%nefiselem(nefiswrthisdad)
    first             => nefiselem%first
    celidt            => nefiselem%celidt
    !
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
       call addelm(nefiswrthisdad,'ITHISC',' ','[   -   ]','INTEGER',4      , &
          & 'timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)  ', &
          & 1, 1, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrthisdad,'DATE_TIME',' ','[   -   ]','CHARACTER',24, &
          & 'Current simulation date and time [YYYY-MM-DD HH:MM:SS.FFFF]   ', &
          & 1, 1, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrthisdad,'LINK_SUM',' ','[  M3   ]','REAL',4       , &
          & 'Cumulative dredged material transported via this link         ', &
          & 2, nalink, lsedtot, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrthisdad,'DREDGE_VOLUME',' ','[  M3   ]','REAL',4  , &
          & 'Cumulative dredged material for this dredge area              ', &
          & 1, nadred+nasupl, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrthisdad,'DUMP_VOLUME',' ','[  M3   ]','REAL',4    , &
          & 'Cumulative dumped material for this dump area                 ', &
          & 1, nadump, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrthisdad,'DREDGE_TFRAC',' ','[   -   ]','REAL',4   , &
          & 'Time fraction spent dredging                                  ', &
          & 1, nadred+nasupl, 0, 0, 0, 0, lundia, gdp)
       call addelm(nefiswrthisdad,'PLOUGH_TFRAC',' ','[   -   ]','REAL',4   , &
          & 'Time fraction spent sploughing                                ', &
          & 1, nadred+nasupl, 0, 0, 0, 0, lundia, gdp)
       !
       call defnewgrp(nefiswrthisdad ,filnam    ,grnam   ,gdp)
    endif
    !
    if (inode == master) then
       ierror = open_datdef(filnam   ,fds      )
       if (ierror /= 0) goto 9999
       !
       if (first) then
          !
          ! end of initialization, don't come here again
          !
          ierror = inqmxi(fds, grnam, celidt)
          first = .false.
       endif
       !
       ! initialize group index
       !
       celidt = celidt + 1
       uindex (1,1) = celidt ! start index
       uindex (2,1) = celidt ! end index
       uindex (3,1) = 1 ! increment in time
       !
       ! element 'ITHISC'
       !
       idummy(1) = ithisc
       ierror = putelt(fds, grnam, 'ITHISC', uindex, 1, idummy)
       if (ierror/= 0) goto 9999
       !
       ! element 'DATE_TIME'
       !
       datetimearr(1)=date_time
       ierror = putelt(fds, grnam, 'DATE_TIME', uindex, 1, datetimearr)
       if (ierror/= 0) goto 9999
       !
       ! element 'LINK_SUM'
       !
       call sbuff_checksize(nalink*lsedtot)
       do l = 1, lsedtot
          do i = 1, nalink
             sbuff(i+(l-1)*nalink) = link_sum(i,l)
          enddo
       enddo
       ierror = putelt(fds, grnam, 'LINK_SUM', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'DREDGE_VOLUME'
       !
       call sbuff_checksize(nadred+nasupl)
       do i = 1, nadred+nasupl
          sbuff(i) = totvoldred(i)
       enddo
       ierror = putelt(fds, grnam, 'DREDGE_VOLUME', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'DUMP_VOLUME'
       !
       call sbuff_checksize(nadump)
       do i = 1, nadump
          sbuff(i) = totvoldump(i)
       enddo
       ierror = putelt(fds, grnam, 'DUMP_VOLUME', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       if (ntimaccum==0) then
          tfrac = 1.0_fp
       else
          tfrac = 1.0_fp/ntimaccum
       endif
       !
       ! element 'DREDGE_TFRAC'
       !
       call sbuff_checksize(nadred+nasupl)
       do i = 1, nadred+nasupl
          sbuff(i) = tfrac*ndredged(i)
       enddo
       ierror = putelt(fds, grnam, 'DREDGE_TFRAC', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'PLOUGH_TFRAC'
       !
       call sbuff_checksize(nadred+nasupl)
       do i = 1, nadred+nasupl
          sbuff(i) = tfrac*nploughed(i)
       enddo
       ierror = putelt(fds, grnam, 'PLOUGH_TFRAC', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ntimaccum = 0
       ndredged  = 0
       nploughed = 0
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
end subroutine wrthisdad
