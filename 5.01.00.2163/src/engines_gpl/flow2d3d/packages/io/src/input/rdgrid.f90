subroutine rdgrid(lunmd     ,lundia    ,error     ,zmodel    ,nrrec     , &
                & mdfrec    ,noui      ,runid     ,mmax      ,nmaxus    , &
                & filgrd    ,fmtgrd    ,flgrd     ,mngrd     ,mxnppt    , &
                & ncpgrd    ,fildry    ,fmtdry    ,fldry     ,mndry     , &
                & mxndry    ,ndry      ,filtd     ,fmttd     ,dirtd     , &
                & fltd      ,mntd      ,mxntd     ,ntd       ,filcut    , &
                & flcut     ,fil45     ,fl45      ,gdp       )
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
!  $Id: rdgrid.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdgrid.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Reads from either the MD-file or the attribute
!                file(s) the following data : FILGRD, FMTGRD,
!                MNGRD, MNDRY, FILTD, FMTTD & MNTD
!              - Sets the default computational grid enclosure if
!                none is specified
!              - Checked against 0 values for c.grid enclosure,
!                dry points and dam points when the coordinates
!                were specified in the MD-file ???
!              - Writes the grid enclosure, the dry points & the
!                thin dams to unformatted semi-scratch files, and
!                sets the related file flags to TRUE
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer :: itis
    integer, pointer :: mfg
    integer, pointer :: nfg
!
! Global variables
!
    integer                                      :: lundia !  Description and declaration in inout.igs
    integer                                      :: lunmd  !  Description and declaration in inout.igs
    integer                        , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                        , intent(in)  :: mxndry !!  Maximum number of dry points
                                                           !!  sections
    integer                        , intent(in)  :: mxnppt !!  Maximum number of grid enclosure,
                                                           !!  thindams etc. points from interactive
                                                           !!  reading
    integer                        , intent(in)  :: mxntd  !!  Maximum number of thin dams from
                                                           !!  interactive reading
    integer                        , intent(out) :: ncpgrd !!  Actual number of computational grid
                                                           !!  enclosure points
    integer                        , intent(out) :: ndry   !!  Actual number of dry points
                                                           !!  sections
    integer                        , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                      :: nrrec  !!  Pointer to the record number in the
                                                           !!  MD-file
    integer                        , intent(out) :: ntd    !!  Actual number of thin dams
    integer, dimension(2, mxnppt)                :: mngrd  !!  M,N coordinates for grid enclosure
                                                           !!  points
    integer, dimension(4, mxndry)                :: mndry  !!  M,N coordinates for begin and end
                                                           !!  dry points for dams
    integer, dimension(4, mxntd)                 :: mntd   !!  M,N coordinates for begin and end
                                                           !!  point thin dams
    logical                                      :: fl45
    logical                                      :: flcut
    logical                                      :: fldry  !  Description and declaration in tmpfil.igs
    logical                                      :: flgrd  !  Description and declaration in tmpfil.igs
    logical                                      :: fltd   !  Description and declaration in tmpfil.igs
    logical                                      :: error  !!  Flag=TRUE if an error is encountered
    logical                        , intent(in)  :: noui   !!
    logical                        , intent(in)  :: zmodel !  Description and declaration in procs.igs
    character(*)                                 :: fil45
    character(*)                                 :: filcut
    character(*)                                 :: fildry !!  File name for the dam points file
    character(*)                                 :: filgrd !!  File name for the grid enclosure
                                                           !!  file
    character(*)                                 :: filtd  !!  File name for the thin dams file
    character(*)                                 :: mdfrec !!  Standard rec. length in MD-file (300)
    character(*)                                 :: runid  !!  Run identification code for the cur-
                                                           !!  rent simulation (used to determine
                                                           !!  the names of the in- /output files
                                                           !!  used by the system)
    character(1), dimension(mxntd)               :: dirtd  !!  Velocity points on which the thin
                                                           !!  dams have been specified (U or V)
    character(2)                   , intent(out) :: fmtdry !!  File format for the dam points file
    character(2)                   , intent(out) :: fmtgrd !!  File format for the grid enclosure
                                                           !!  file
    character(2)                   , intent(out) :: fmttd  !!  File format for the thin dams file
!
! Local variables
!
    integer               :: i      ! Help var. 
    integer               :: idef   ! Help var. containing default va- lue(s) for integer variable 
    integer               :: idry   ! Help var. for the dry points 
    integer               :: imnd   ! Help var. for the dry points 
    integer               :: imng   ! Help var. for the grid points 
    integer               :: imnt   ! Help var. for the thin dam points 
    integer               :: ippt   ! Help var. for the grid points 
    integer               :: itd    ! Help var. for the dam points 
    integer               :: j      ! Help var. 
    integer               :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer               :: lfnm   ! actual length of file name
    integer               :: lkw    ! Actual length of KEYW 
    integer               :: lrid   ! Length of character string runid 
    integer               :: lun45
    integer               :: luncut
    integer               :: lundry ! Unit number of local scratch file for dry point sections 
    integer               :: lungrd ! Unit number of local scratch file for grid enclosure points 
    integer               :: luntd  ! Unit number of local scratch file for thin dam point sections 
    integer               :: n
    integer               :: newlun
    integer               :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer               :: ntrec  ! Help. var to keep track of NRREC 
    integer, dimension(4) :: ival   ! Help array 
    logical               :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical               :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical               :: lerror ! Flag=TRUE if a local error is encountered 
    logical               :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical               :: outsd  ! indicating whether all dry/thin dam points are outside subdomain (.TRUE.) or not (.FALSE.)
    character(1)          :: cdef   ! Default value when CVAR not found 
    character(1)          :: cval   ! Help variable 
    character(11)         :: fmtdef ! Default file format (usually=blank) 
    character(11)         :: fmttmp ! Help variable for file format 
    character(12)         :: fildef ! Default file name (usually = blank) 
    character(256)        :: filnam ! String containing complete file name "TMP_RUNID.extension"
    character(256)        :: fixid  ! fixed size version of runid, needed for character concatenation 
    character(3)          :: errmsg ! Help string for errormessage 
    character(6)          :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    itis  => gdp%gdrdpara%itis
    mfg   => gdp%gdparall%mfg
    nfg   => gdp%gdparall%nfg
    !
    flgrd  = .false.
    fldry  = .false.
    fltd   = .false.
    flcut  = .false.
    fl45   = .false.
    !
    filgrd = ' '
    fildry = ' '
    filtd  = ' '
    filcut = ' '
    fil45  = ' '
    fmtgrd = 'FR'
    fmtdry = 'FR'
    fmttd  = 'FR'
    !
    ncpgrd = 0
    ndry   = 0
    ntd    = 0
    !
    do n = 1, mxnppt
       dirtd(n) = 'U'
    enddo
    !
    ! initialize local paramters
    !
    fildef = ' '
    fmtdef = 'FRformatted'
    cdef   = ' '
    idef   = 0
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    !
    do i = 1, 4
       ival(i) = 0
    enddo
    cval = ' '
    !
    ! define length of runid and put in fixed size array
    ! size is tested in iniid
    !
    call noextspaces(runid     ,lrid      )
    fixid(1:lrid) = runid(1:lrid)
    !=======================================================================
    !
    ! in case of parallel Delft3D-FLOW grid enclosure has already been read
    ! (see routine rdencl)
    !
    if (parll) goto 201
    !
    ! open semi-scratch file, only if noui = .true.
    !
    if (noui) then
       lungrd = newlun(gdp)
       open (lungrd, file = 'TMP_' // fixid(1:lrid) // '.grd',                  &
            & form = 'unformatted', status = 'unknown')
    endif
    !
    ! 'Filgrd': grid enclosure file
    !
    filgrd = fildef
    call prop_get_string(gdp%mdfile_ptr,'*','Filgrd',filgrd)
    if (filgrd /= fildef) then
       !
       ! Grid enclosure in file
       !
       ! locate 'Fmtgrd' record for format definition of input file
       !
       keyw   = 'Fmtgrd'
       ntrec  = nrrec
       nlook  = 1
       lenc   = 2
       fmttmp = ' '
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = fmtdef(3:)
       else
          !
          ! determine file format (unformatted/freeformatted)
          !
          call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
          !
          if (lerror) then
             lerror = .false.
             fmttmp = fmtdef(3:)
          endif
       endif
       fmtgrd = 'FR'
       if (fmttmp(:2)=='un') then
          fmtgrd = 'UN'
       endif
       !
       ! read data from external file only if noui = .true.
       !
       if (noui) then
          call grdfil(lundia    ,lungrd    ,error     ,filgrd    ,fmttmp    , &
                    & flgrd     ,gdp       )
       endif
    else
       !
       ! No grid enclosure file, you have to specify one
       !
       call prterr(lundia ,'P004' ,'You have to specify a grid enclosure')
       error = .true.
    endif
    !
    ! close files
    !
    if (noui) then
       if (error) then
          close (lungrd, status = 'delete')
       else
          close (lungrd)
       endif
    endif
201 continue
    !=======================================================================
    ! open semi-scratch file, only if noui = .true.
    !
    if (noui) then
       lundry = newlun(gdp)
       filnam = 'TMP_' // fixid(1:lrid) // '.dry'
       !
       ! append node number to file name in case of parallel computing within single-domain case
       !
       if ( parll ) then
          call noextspaces(filnam,lfnm)
          write(filnam(lfnm+1:lfnm+4),666) inode
       endif
       open (lundry, file = trim(filnam), form = 'unformatted', status = 'unknown')
    endif
    !
    ! locate 'Fildry' record for dry points in extra input file
    ! If NLOOK = 0 and 'Fildry' not found => no error and item skipped
    !
    keyw  = 'Fildry'
    newkw = .true.
    ntrec = nrrec
    nlook = 0
    lenc  = len(fildry)
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,fildry    ,fildef    ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       fildry = fildef
    endif
    !
    ! dry points in file? <YES>
    !
    if (fildry /= fildef) then
       !
       ! locate 'Fmtdry' record for format definition of input file
       !
       keyw   = 'Fmtdry'
       ntrec  = nrrec
       nlook  = 1
       lenc   = 2
       fmttmp = ' '
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = fmtdef(3:)
       else
          !
          ! calculate file format definition (unformatted/freeformatted)
          !
          call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
          if (lerror) then
             lerror = .false.
             fmttmp = fmtdef(3:)
          endif
       endif
       fmtdry = 'FR'
       if (fmttmp(:2)=='un') fmtdry = 'UN'
       !
       ! read data from external file only if noui = .true.
       !
       if (noui) then
          call dryfil(lundia    ,lundry    ,error     ,fildry    ,fmttmp    , &
                    & fldry     ,gdp       )
       endif
    !
    ! dry points in file? <NO>
    !
    else
       !
       ! Initialize "global" array MNDRY (4,MXNPPT)
       !
       do n = 1, mxnppt
          do j = 1, 4
             mndry(j, n) = 0
          enddo
       enddo
       !
       ! locate 'MNdry' record for dry points
       !
       idry = 1
       imnd = 1
       !
       keyw  = 'MNdry '
       ntrec = nrrec
       newkw = .true.
       lkw   = 5
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       ! not found ?
       !
       if (.not.found) then
          lerror = .true.
          goto 300
       endif
       ! -->
  210  continue
       newkw = .true.
       nlook = 4
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) goto 300
       !
       ! define mndry, and test values
       !
       if (ival(1)==0 .or. ival(2)==0 .or. ival(3)==0 .or. ival(4)==0) then
          if (imnd>1) then
             lerror = .true.
             call prterr(lundia    ,'V003'    ,'Coord. of the dry point'       )
          endif
       else
          ival(1) = ival(1) -mfg +1
          ival(2) = ival(2) -nfg +1
          ival(3) = ival(3) -mfg +1
          ival(4) = ival(4) -nfg +1
          !
          ! check if dry points are fully (.TRUE.) or partly (.FALSE.) outside subdomain
          !
          ! Note: for single domain runs, outsd = .FALSE., i.e. dry points are completely inside domain
          !
          call adjlin (ival,outsd,mmax,nmaxus)
          mndry(1, idry) = ival(1)
          mndry(2, idry) = ival(2)
          mndry(3, idry) = ival(3)
          mndry(4, idry) = ival(4)
          !
          ! write mndry semi-scratch file if noui = .true.
          !
          if (noui .and. .not.outsd) then
             write (lundry) (mndry(j, idry), j = 1, 4)
          endif
          !
          ! check if idry exceeds maximum value, then idry will be
          ! reset, for noui = .true. this will never occur
          ! reset 1 to large, because ndry = idry - 1
          !
          if (.not.noui)  idry = idry + 1
          if (.not.outsd) imnd = imnd + 1
          if (idry>mxndry) then
             lerror = .true.
             write (errmsg, '(i3)') idry - mxndry
             call prterr(lundia    ,'U131'    ,errmsg    )
             idry = mxndry + 1
             goto 300
          endif
          !
          ! next records newkw = false
          !
          newkw = .false.
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          lerror = .false.
          if (found) goto 210
       ! <--
       endif
       !
       ! stop reading
       !
       !
       ! define actual number of dry point sections and define fldry
       !
  300  continue
       ndry = idry - 1
       imnd = imnd - 1
       !
       fldry = .true.
       if (imnd==0) fldry = .false.
       if (lerror) then
          lerror = .false.
          fldry = .false.
       endif
    endif
    !
    ! close files
    !
    if (noui) then
       if (error .or. .not.fldry) then
          close (lundry, status = 'delete')
       else
          close (lundry)
       endif
    endif
    !=======================================================================
    ! open semi-scratch file, only if noui = .true.
    !
    if (noui) then
       luntd = newlun(gdp)
       filnam = 'TMP_' // fixid(1:lrid) // '.td'
       !
       ! append node number to file name in case of parallel computing within single-domain case
       !
       if ( parll ) then
          call noextspaces(filnam,lfnm)
          write(filnam(lfnm+1:lfnm+4),666) inode
       endif
       open (luntd, file = trim(filnam), form = 'unformatted', status = 'unknown')
    endif
    !
    ! locate 'Filtd ' record for thin dams in extra input file
    !
    filtd = fildef
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filtd', filtd)
    !
    ! thin dams in file? <YES>
    !
    if (filtd /= fildef) then
       !
       ! locate 'Fmttd ' record for format definition of input file
       !
       keyw   = 'Fmttd '
       ntrec  = nrrec
       nlook  = 1
       lenc   = 2
       fmttmp = ' '
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = fmtdef(3:)
       else
          !
          ! calculate file format definition (unformatted/freeformatted)
          !
          call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
          if (lerror) then
             lerror = .false.
             fmttmp = fmtdef(3:)
          endif
       endif
       fmttd = 'FR'
       if (fmttmp(:2)=='un') fmttd = 'UN'
       !
       ! read data from external file only if noui = .true.
       !
       if (noui) then
          call tdfil(lundia    ,luntd     ,error     ,filtd     ,fmttmp    , &
                   & fltd      ,gdp       )
       endif
    !
    ! thin dams in file? <NO>
    !
    else
       !
       ! Initialize "global" array MNTD  (4,MXNPPT)
       !
       do n = 1, mxnppt
          do j = 1, 4
             mntd(j, n) = 0
          enddo
       enddo
       !
       ! Locate 'MNtd' record for m1td, n1td, m2td, n2td and dirtd
       ! first time newkw = .true.
       !
       itd  = 1
       imnt = 1
       keyw  = 'MNtd  '
       newkw = .true.
       ntrec = nrrec
       lkw   = 4
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       ! not found ?
       !
       if (.not.found) then
          lerror = .true.
          goto 400
       endif
       ! -->
  310  continue
       newkw = .true.
       nlook = 4
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) goto 400
       !
       ! define mntd, and test values
       !
       !      mntd(1, itd) = ival(1)
       !      mntd(2, itd) = ival(2)
       !      mntd(3, itd) = ival(3)
       !      mntd(4, itd) = ival(4)
       !      if (mntd(1, itd)==0 .or. mntd(2, itd)==0 .or. mntd(3, itd)==0 .or.       &
       !        & mntd(4, itd)==0) then
       if (ival(1)==0 .or. ival(2)==0 .or. ival(3)==0 .or. ival(4)==0) then
          if (imnt > 1) then
             lerror = .true.
             call prterr(lundia    ,'V003'    ,'Thin dam coord.'    )
          endif
       else
          lenc  = 1
          nlook = 1
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,cval      ,cdef      ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          if (lerror) goto 400
          dirtd(itd) = cval
          ival(1) = ival(1) -mfg +1
          ival(2) = ival(2) -nfg +1
          ival(3) = ival(3) -mfg +1
          ival(4) = ival(4) -nfg +1
          !
          ! check if thin dams are fully (.TRUE.) or partly (.FALSE.) outside subdomain
          !
          ! Note: for single domain runs, outsd = .FALSE., i.e. thin dams are completely inside domain
          !
          call adjlin (ival,outsd,mmax,nmaxus)
          mntd(1, itd) = ival(1)
          mntd(2, itd) = ival(2)
          mntd(3, itd) = ival(3)
          mntd(4, itd) = ival(4)
          !
          ! write mntd, dirtd to semi-scratch file if noui = .true.
          !
          if (noui .and. .not.outsd) then
             write (luntd) (mntd(j, itd), j = 1, 4), dirtd(itd)
          endif
          !
          ! check if itd exceeds maximum value, then itd will be
          ! reset, for noui = .true. this will never occur
          ! reset 1 to large, because ntd = itd - 1
          !
          if (.not.noui)   itd = itd + 1
          if (.not.outsd) imnt = imnt + 1
          if (itd > mxntd) then
             lerror = .true.
             write (errmsg, '(i3)') itd - mxntd
             call prterr(lundia    ,'U132'    ,errmsg    )
             itd = mxntd + 1
             goto 400
          endif
          !
          ! next records newkw = false
          !
          newkw = .false.
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          lerror = .false.
          if (found) goto 310
       ! <--
       endif
       !
       ! stop reading
       !
       !
       ! define actual number of thin dams point sections
       ! and define fltd
       !
  400  continue
       ntd  = itd - 1
       imnt = imnt - 1
       fltd = .true.
       if (imnt==0) then
          fltd = .false.
       endif
       if (lerror) then
          lerror = .false.
          fltd  = .false.
       endif
    endif
    !
    ! close files
    !
    if (noui) then
       if (error .or. .not.fltd) then
          close (luntd, status = 'delete')
       else
          close (luntd)
       endif
    endif
    !
    ! open semi-scratch file for "cut-cell" definition of grids
    !
    luncut = newlun(gdp)
    !   open (luncut, file = 'TMP_' // fixid(1:lrid) // '.cut',                     &
    !       & form = 'unformatted', status = 'unknown')
    filnam = 'TMP_' // fixid(1:lrid) // '.cut'
    !
    ! append node number to file name in case of parallel computing within single-domain case
    !
    if ( parll ) then
       call noextspaces(filnam,lfnm)
       write(filnam(lfnm+1:lfnm+4),666) inode
    endif
    open (luncut, file = trim(filnam), form = 'unformatted', status = 'unknown')
    !
    ! locate 'Filcut' record for grid enclosure in extra input file
    !
    keyw  = 'Filcut'
    ntrec = nrrec
    newkw = .true.
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = len(filcut)
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filcut    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          filcut = fildef
       else
          !
          ! read data from external file
          !
          call tdfil(lundia    ,luncut    ,error     ,filcut    ,fmttmp    , &
                   & flcut     ,gdp       )
       endif
    endif
    if (flcut) then
       close (luncut)
    else
       close (luncut, status = 'delete')
    endif
    !
    ! open semi-scratch file for 45 degrees staircase closed boundary
    !
    lun45 = newlun(gdp)
    !   open (lun45, file = 'TMP_' // fixid(1:lrid) // '.45', form = 'unformatted', &
    !       & status = 'unknown')
    filnam = 'TMP_' // fixid(1:lrid) // '.45'
    !
    ! append node number to file name in case of parallel computing within single-domain case
    !
    if ( parll ) then
       call noextspaces(filnam,lfnm)
       write(filnam(lfnm+1:lfnm+4),666) inode
    endif
    open (lun45, file = trim(filnam), form = 'unformatted', status = 'unknown')
    !
    ! locate 'Filcut' record for grid enclosure in extra input file
    !
    keyw  = 'Fil45'
    ntrec = nrrec
    newkw = .true.
    lkw   = 5
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = len(fil45)
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fil45     ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fil45 = fildef
       else
          !
          ! read data from external file
          !
          call dryfil(lundia    ,lun45     ,error     ,fil45     ,fmttmp    , &
                    & fl45      ,gdp       )
       endif
    endif
    if (fl45) then
       close (lun45)
    else
       close (lun45, status = 'delete')
    endif
    !
666 format('-',i3.3)
    !
end subroutine rdgrid
