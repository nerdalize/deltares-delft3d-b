subroutine delfil(runid     ,filmd     ,gdp       )
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
!  $Id: delfil.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/general/delfil.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Deletes all the temporary (unformatted) files
!              which where created by the simulation program.
!              These files are respectively :
!                   TMP_//runid//.bch
!                   TMP_//runid//.bct
!                   TMP_//runid//.grd
!                   TMP_//runid//.bcc
!                   TMP_//runid//.td
!                   TMP_//runid//.dis
!                   TMP_//runid//.dry
!                   TMP_//runid//.tem
!                   TMP_//runid//.eva
!                   TMP_//runid//.wnd
!                   TMP_refinement
!                   com-//runid//.srctmp (old name)
!                   TMP_com-//runid//.src (new name)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall 
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: itis
    integer , pointer :: lunmd
    integer , pointer :: lundia
    integer , pointer :: lunscr
    logical , pointer :: reusetmp !  TRUE when temporary files will be reused if possible 
!
! Global variables
!
    character(*)   , intent(in) :: filmd    !! File name for MD FLOW file 
    character(*)   , intent(in) :: runid    !!  Run identification code for the current simulation
!
! Local variables
!
    integer                        :: lenc    ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                        :: lkw     ! Length of keyword string 
    integer                        :: lmd     ! Length of character string filmd 
    integer                        :: lrid    ! Length of character string runid 
    integer                        :: lunout  ! Unit number for file to write out- put to (:=LUNDIA or LUNSCR) 
    integer                        :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: nrrec   ! Pointer to the record number in the MD-file 
    integer                        :: ntrec   ! Help. var to keep track of NRREC 
    integer, external              :: newlun
    logical                        :: ex      ! Flag to test if file exists 
    logical                        :: found   ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                        :: lerror  ! Flag=TRUE if a local error is encountered 
    logical                        :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                        :: opend   ! Flag to check if file is open 
    character(12)                  :: fildef
    character(256)                 :: filnam  ! String contaning complete file name "TMP_RUNID.extension" 
    character(256)                 :: filrd   ! File name read from Md-file/flow file 
    character(300)                 :: mdfrec  ! Standard rec. length in MD-file (300) 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)                   :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    logical                        :: lmd_open
!
!! executable statements -------------------------------------------------------
!
    lunmd       => gdp%gdinout%lunmd
    lundia      => gdp%gdinout%lundia
    lunscr      => gdp%gdinout%lunscr
    itis        => gdp%gdrdpara%itis
    reusetmp    => gdp%gdtmpfil%reusetmp
    !
    lerror   = .false.
    newkw    = .true.
    found    = .false.
    lmd_open = .false.
    nlook    = 1
    lkw      = 6
    lenc     = 12
    fildef   = ' '
    !
    ! Unit number to write output to
    !
    lunout = lunscr
    inquire (lundia, opened = opend)
    if (opend) lunout = lundia
    !
    ! Define length of runid
    !
    call noextspaces(runid     ,lrid      )
    !
    ! Open mdf/md-file/md-flow file to see if temporary files are really
    ! temporary. If no mdf/md-file/md-flow file defined defined then no
    ! temporary files either
    !
    call noextspaces(filmd     ,lmd       )
    inquire (file = filmd(1:lmd), exist = ex)
    if (ex) then
       inquire (file = filmd(1:lmd), opened = opend)
       if (opend) then
          rewind (lunmd)
       else
          lunmd = newlun(gdp)
          open (lunmd, file = filmd(1:lmd), form = 'formatted')
       endif
       !
       ! Md-file is opened, it should be closed at the end of this routine
       !
       lmd_open = .true.
    else
       goto 9999
    endif
    !
    ! Read first record and define NRREC
    !
    read (lunmd, '(a)') mdfrec
    nrrec = 1
    if (.not. reusetmp) then
        !
        ! Locate 'Filgrd' record in MD-file/flow file
        ! to test temporary file defining grid
        !
        keyw = 'Filgrd'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.grd'
        call rmdel(filnam    ,gdp       )
        ! 
        ! append node number to file name in case of parallel computing within single-domain case 
        ! 
        if ( parll ) then
           write(filnam(8+lrid+1:8+lrid+4),'(a,i3.3)') '-',inode 
        endif
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           call rmdel(filnam    ,gdp       )
        endif
        !
        ! Locate 'Fildry' record in MD-file/flow file
        ! to test temporary file defining permanent dry points
        !
        keyw = 'Fildry'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lundia    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.dry'
        ! 
        ! append node number to file name in case of parallel computing within single-domain case 
        ! 
        if ( parll ) then
           write(filnam(8+lrid+1:8+lrid+4),'(a,i3.3)') '-',inode 
        endif
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           call rmdel(filnam    ,gdp       )
        endif
        !
        ! Locate 'Filtd' record in MD-file/flow file
        ! to test temporary file defining thin dams
        !
        keyw = 'Filtd'
        lkw = 5
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.td'
        ! 
        ! append node number to file name in case of parallel computing within single-domain case 
        ! 
        if ( parll ) then
           write(filnam(7+lrid+1:7+lrid+4),'(a,i3.3)') '-',inode 
        endif
        if (filnam(:7 + lrid)/=filrd(:7 + lrid)) then
           call rmdel(filnam    ,gdp       )
        endif
        !
        ! Locate 'FilbcH' record in MD-file/flow file
        ! to test temporary file defining fourier boundary conditions
        !
        keyw = 'FilbcH'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.bch'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           if (inode == master) call rmdel(filnam    ,gdp       ) 
        endif
        !
        ! Locate 'FilbcT' record in MD-file/flow file
        ! to test temporary files defining time varying hydrodynamic data
        ! at open boundaries
        !
        keyw = 'FilbcT'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.bct'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           if (inode == master) call rmdel(filnam    ,gdp       ) 
        endif
        !
        ! Locate 'FilbcQ' record in MD-file/flow file
        ! to test temporary files defining time varying concentration data
        ! at open boundaries
        !
        keyw = 'FilbcQ'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.bcq'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           call rmdel(filnam    ,gdp       )
        endif
        !
        ! Locate 'FilbcC' record in MD-file/flow file
        ! to test temporary files defining time varying concentration data
        ! at open boundaries
        !
        keyw = 'FilbcC'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.bcc'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           if (inode == master) call rmdel(filnam    ,gdp       ) 
        endif
        !
        ! Locate 'Fildis' record in MD-file/flow file
        ! to test temporary files defining time varying data at discharge
        ! locations
        !
        keyw = 'Fildis'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.dis'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           if (inode == master) call rmdel(filnam    ,gdp       ) 
        endif
        !
        ! Locate 'Filtmp' record in MD-file/flow file
        ! to test temporary files defining time varying data for heat models
        !
        keyw = 'Filtmp'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.tem'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           if (inode == master) call rmdel(filnam    ,gdp       ) 
        endif
        !
        ! Locate 'Fileva' record in MD-file/flow file
        ! to test temporary files defining time varying data for
        ! evaporations and rain
        !
        keyw = 'Fileva'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.eva'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           call rmdel(filnam    ,gdp       )
        endif
        !
        ! Locate 'Filwnd' record in MD-file/flow file
        ! to test temporary files defining time varying wind data
        !
        keyw = 'Filwnd'
        lkw = 6
        ntrec = nrrec
        call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                  & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                  & 'NO'      )
        if (found) then
           call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                     & mdfrec    ,filrd     ,fildef    ,lenc      ,nrrec     , &
                     & ntrec     ,lunout    ,gdp       )
           if (lerror) then
              lerror = .false.
              filrd = fildef
           endif
        else
           filrd = fildef
        endif
        !
        ! First test name of temporary file is not equal to name of
        ! attribute file in MD-file/flow file
        ! If not test existence of intermediate files and if they are opened
        ! close the files and delete them using routine RMDEL
        !
        filnam = 'TMP_' // runid(:lrid) // '.wnd'
        if (filnam(:8 + lrid)/=filrd(:8 + lrid)) then
           call rmdel(filnam    ,gdp       )
        endif
    endif
    !
    ! Temporary file created (Tricom.f90) when leaving online visualisation 
    ! during simulation
    ! close the file and delete it using routine RMDEL
    !
    filnam = 'TMP_VisuOL_closed'
    call rmdel(filnam, gdp)
    !
    ! Temporary file created (mapper_config.cpp) by mappers to check refinement
    !
    filnam = 'TMP_refinement'
    call rmdel(filnam, gdp)
    !
    ! Temporary file possibly created when writing WAQ input files
    !
    filnam = 'TMP_com-' // runid(:lrid) // '.src'
    call rmdel(filnam, gdp)
    ! old name
    filnam = 'com-' // runid(:lrid) // '.srctmp'
    call rmdel(filnam, gdp)
9999 continue
    !
    if (lmd_open) close(lunmd)
end subroutine delfil
