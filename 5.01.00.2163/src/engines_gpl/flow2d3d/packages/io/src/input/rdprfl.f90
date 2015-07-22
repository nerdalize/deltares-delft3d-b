subroutine rdprfl(lunmd     ,lundia    ,nrrec     ,mdfrec    ,tstprt    , &
                & kmax      ,lstsci    ,ltur      ,lsal      ,ltem      , &
                & nostat    ,filsta    ,ntruv     ,filtra    ,prsmap    , &
                & prshis    ,selmap    ,selhis    ,lsed      ,gdp       )
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
!  $Id: rdprfl.f90 1799 2012-08-29 08:25:34Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdprfl.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Reads the output selection options from the MD-
!                file :
!              - Writes the flags to a character string SELHIS
!                (char*23) and SELMAP (char*21).
!                Default SELHIS = 'YYYYYYYYYYYYYYYYYYYYYYY'
!                Default SELMAP = 'YYYYYYYYYYYYYYYYYYYYN'
!              - Reads the output print options from the MD-
!                file :
!              - Writes the flags to a character string PRSHIS
!                (char*23) and PRSMAP (char*19).
!                Default = 'YYYYYYYYYYYYYYYYYYYYYYY'
!              - For NOUI FILSTA and FILTRA are dummy arguments
!                but then the values for NOSTAT and NTRUV are
!                read from attribute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer,               pointer :: itis
    integer, dimension(:), pointer :: smlay
    integer, dimension(:), pointer :: shlay
    logical,               pointer :: htur2d
!
! Global variables
!
    integer      , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer      , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer      , intent(in)  :: lsed   !  Description and declaration in dimens.igs
    integer      , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer      , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer      , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                    :: lundia !  Description and declaration in inout.igs
    integer                    :: lunmd  !  Description and declaration in inout.igs
    integer      , intent(in)  :: nostat !  Description and declaration in dimens.igs
    integer                    :: nrrec  !!  Pointer to the record number in the MD-file
    integer      , intent(in)  :: ntruv  !  Description and declaration in dimens.igs
    logical      , intent(out) :: tstprt !  Description and declaration in tricom.igs
    character(*) , intent(in)  :: filsta !!  File name for the monitoring stations file
    character(*) , intent(in)  :: filtra !!  File name for the cross sections file
    character(*)               :: mdfrec !!  Standard rec. length in MD-file (300)
    character(19)              :: prsmap !  Description and declaration in tricom.igs
    character(21)              :: selmap !  Description and declaration in tricom.igs
    character(23)              :: prshis !  Description and declaration in tricom.igs
    character(23)              :: selhis !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                            :: i
    integer                            :: icount
    integer                            :: istat
    integer                            :: j
    integer                            :: kmaxout
    integer                            :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                            :: lkw    ! Length (in characters) of keyword 
    integer                            :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                            :: ntrec  ! Help. var to keep track of NRREC 
    integer, dimension(:), allocatable :: ival
    logical                            :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                            :: lerror ! Flag=TRUE if an error is encountered 
    logical                            :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    character(10)                      :: cdef   ! Default value when CVAR not found 
    character(10)                      :: chulp  ! Help var. 
    character(6)                       :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
    character(256)                     :: message
!
!! executable statements -------------------------------------------------------
!
    htur2d     => gdp%gdprocs%htur2d
    itis       => gdp%gdrdpara%itis
    newkw = .true.
    cdef  = 'YYYYYYYYYY'
    !
    ! initialize parameters that are to be read
    !
    prshis = 'YYYYYYYYYYYYYYYYYYYYYYY'
    prsmap = 'YYYYYYYYYYYYYYYYYYY'
    selhis = 'YYYYYYYYYYYYYYYYYYYYYYY'
    selmap = 'YYYYYYYYYYYYYYYYYYYYN'
    tstprt = .false.
    !
    ! locate 'PHhydr' record for print flag History hydrodynamic
    !
    keyw  = 'PHhydr'
    ntrec = nrrec
    lenc  = 6
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:6) = cdef(1:6)
    endif
    prshis(1:6) = chulp(1:6)
    !
    ! locate 'PHproc' record for print flag History processes
    !
    keyw  = 'PHproc'
    ntrec = nrrec
    lenc  = 10
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:10) = cdef(1:10)
    endif
    prshis(7:16) = chulp(1:10)
    !
    ! locate 'PHderv' record for print flag History derivitives
    !
    keyw  = 'PHderv'
    ntrec = nrrec
    lenc  = 3
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:3) = cdef(1:3)
    endif
    prshis(17:19) = chulp(1:3)
    !
    ! locate 'PHflux' record for print flag History fluxes (cross-
    ! sections)
    !
    keyw  = 'PHflux'
    ntrec = nrrec
    lenc  = 4
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:4) = cdef(1:4)
    endif
    prshis(20:23) = chulp(1:4)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 23
       if (prshis(i:i) == 'n')       prshis( i: i) = 'N'
       if (prshis(i:i) /= 'N')       prshis( i: i) = 'Y'
    enddo
    if (kmax   == 1)                 prshis( 6: 6) = 'N'
    if (lstsci == 0)                 prshis( 7:14) = 'NNNNNNNN'
    if (ltur   == 0)                 prshis(15:16) = 'NN'
    if (kmax   == 1)                 prshis(17:18) = 'NN'
    if (max(lsal,ltem) == 0)         prshis(19:19) = 'N'
    if (nostat==0 .and. filsta==' ') prshis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
    if (lstsci == 0)                 prshis(22:23) = 'NN'
    if (ntruv==0 .and. filtra==' ')  prshis(20:23) = 'NNNN'
    !
    ! locate 'PMhydr' record for print flag Map hydrodynamic
    !
    keyw  = 'PMhydr'
    ntrec = nrrec
    lenc  = 6
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:6) = cdef(1:6)
    endif
    prsmap(1:6) = chulp(1:6)
    !
    ! locate 'PMproc' record for print flag Map processes
    !
    keyw  = 'PMproc'
    ntrec = nrrec
    lenc  = 10
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:10) = cdef(1:10)
    endif
    prsmap(7:16) = chulp(1:10)
    !
    ! locate 'PMderv' record for print flag Map derivitives
    !
    keyw  = 'PMderv'
    ntrec = nrrec
    lenc  = 3
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:3) = cdef(1:3)
    endif
    prsmap(17:19) = chulp(1:3)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 19
       if (prsmap(i:i) == 'n') prsmap( i: i) = 'N'
       if (prsmap(i:i) /= 'N') prsmap( i: i) = 'Y'
    enddo
    if (kmax   == 1)           prsmap( 6: 6) = 'N'
    if (lstsci == 0)           prsmap( 7:14) = 'NNNNNNNN'
    if (ltur   == 0)           prsmap(15:16) = 'NN'
    if (kmax   == 1)           prsmap(17:18) = 'NN'
    if (max(lsal, ltem)==0)    prsmap(19:19) = 'N'
    !
    ! locate 'SHhydr' record for print flag History hydrodynamic
    !
    keyw  = 'SHhydr'
    ntrec = nrrec
    lenc  = 4
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:4) = cdef(1:4)
    endif
    selhis(1:4) = chulp(1:4)
    !
    ! locate 'SHproc' record for print flag History processes
    !
    keyw  = 'SHproc'
    ntrec = nrrec
    lenc  = 10
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:10) = cdef(1:10)
    endif
    selhis(5:14) = chulp(1:10)
    !
    ! locate 'SHderv' record for print flag History derivitives
    !
    keyw  = 'SHderv'
    ntrec = nrrec
    lenc  = 5
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:5) = cdef(1:5)
    endif
    selhis(15:19) = chulp(1:5)
    !
    ! locate 'SHflux' record for print flag History fluxes (stations
    ! and cross-sections)
    !
    keyw  = 'SHflux'
    ntrec = nrrec
    lenc  = 4
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:4) = cdef(1:4)
    endif
    selhis(20:23) = chulp(1:4)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 23
       if (selhis(i:i)=='n')           selhis(i : i) = 'N'
       if (selhis(i:i)/='N')           selhis(i : i) = 'Y'
    enddo
    !
    if (kmax   == 1)                   selhis(4 : 4) = 'N'
    if (lstsci == 0)                   selhis(5 :12) = 'NNNNNNNN'
    if (ltur   == 0)                   selhis(13:14) = 'NN'
    if (kmax   == 1)                   selhis(17:18) = 'NN'
    if (max(lsal, ltem, lsed) == 0)    selhis(19:19) = 'N'
    if (lstsci == 0)                   selhis(22:23) = 'NN'
    if (nostat==0 .and. filsta==' ') then
                                       selhis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
       if (ntruv==0 .and. filtra==' ') selhis(20:23) = 'NNNN'
    else
       if (ntruv==0 .and. filtra==' ') selhis(21:23) = 'NNN'
    endif
    !
    ! locate 'SMhydr' record for print flag Map hydrodynamic
    !
    keyw  = 'SMhydr'
    ntrec = nrrec
    lenc  = 5
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:5) = cdef(1:5)
    endif
    selmap(1:5) = chulp(1:5)
    !
    ! locate 'SMproc' record for print flag Map processes
    !
    keyw  = 'SMproc'
    ntrec = nrrec
    lenc  = 10
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:10) = cdef(1:10)
    endif
    selmap(6:15) = chulp(1:10)
    !
    ! locate 'SMderv' record for print flag Map derivitives
    !
    keyw  = 'SMderv'
    ntrec = nrrec
    lenc  = 6
    nlook = 0
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       chulp(1:5) = cdef(1:5)
       chulp(6:6) = 'N'
    endif
    !
    ! 6-th character is recently added. When it is not present it
    ! defaults to N
    !
    if (chulp(6:6) == ' ') chulp(6:6) = 'N'
    selmap(16:21) = chulp(1:6)
    !
    ! test for 'N' (default is 'Y')
    !
    do i = 1, 21
       if (selmap(i:i) == 'n') selmap(i:i) = 'N'
       if (selmap(i:i) /= 'N') selmap(i:i) = 'Y'
    enddo
    !
    if (kmax   == 1)                selmap( 4: 5) = 'NN'
    if (lstsci == 0)                selmap( 6:13) = 'NNNNNNNN'
    if (ltur   == 0)                selmap(14:15) = 'NN'
    if (kmax   == 1)                selmap(18:19) = 'NN'
    if (max(lsal, ltem, lsed) == 0) selmap(20:20) = 'N'
    !
    ! Always write turbulence info when HLES is active
    !
    if (htur2d) selmap(21:21) = 'Y'
    !
    ! Layer specified output
    !
    !
    ! SMlay
    !
                  allocate (gdp%gdpostpr%smlay(kmax+1), stat = istat)
    if (istat==0) allocate (gdp%gdpostpr%shlay(kmax+1), stat = istat)
    if (istat==0) allocate (ival(kmax+1), stat = istat)
    !
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'RDPRFL: memory alloc error')
       call d3stop(1, gdp)
    endif
    smlay => gdp%gdpostpr%smlay
    shlay => gdp%gdpostpr%shlay
    ival = -999
    call prop_get(gdp%mdfile_ptr, '*', 'SMlay', ival, kmax+1)
    if (ival(1) == -999) then
       !
       ! No layers specified for map-output
       ! Default: all layers to output, including zero: 0, 1, 2, ..., kmax
       !
       do i = 1, kmax+1
          smlay(i) = i-1
       enddo
    else
       !
       ! layers specified for map-output
       ! Detect the number of output layers(kmaxout):
       !   Search for the first i for which ival(i) == -999. Then kmaxout = i-1
       ! On the flow: check that the specified layers are valid
       ! 
       kmaxout = kmax + 1
       do i = 1, kmax+1
          if (ival(i) == -999) then
             kmaxout = i-1
             exit
          endif
          if (ival(i)<0 .or. ival(i)>kmax) then
             write (message,'(a,i0,a)') "Invalid layer specification '", ival(i), "' in keyword SMlay"
             call prterr(lundia, 'U021', trim(message))
             call d3stop(1, gdp)
          endif
       enddo
       !
       ! Adapt the size of array smlay in case kmaxout<kmax
       !
       if (kmaxout /= kmax+1) then
          deallocate (gdp%gdpostpr%smlay, stat = istat)
          allocate (gdp%gdpostpr%smlay(kmaxout), stat = istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'RDPRFL: memory alloc error')
             call d3stop(1, gdp)
          endif
          smlay => gdp%gdpostpr%smlay
       endif
       !
       ! Fill smlay with the values of ival in increasing order
       !
       icount = 1
       do i = 0, kmax
          do j = 1, kmaxout
             if (ival(j) == i) then
                smlay(icount) = i
                icount        = icount + 1
                exit
             endif
          enddo
       enddo
       write(message,'(a)') "Map output is written for layer(s)"
       do i=1, size(smlay)
          write(message,'(a,a,i0)') trim(message), ' ', smlay(i)
       enddo
       call prterr(lundia, 'G051',trim(message))
    endif
    !
    ! SHlay
    !
    ival = -999
    call prop_get(gdp%mdfile_ptr, '*', 'SHlay', ival, kmax+1)
    if (ival(1) == -999) then
       !
       ! No layers specified for his-output
       ! Default: all layers to output, including zero: 0, 1, 2, ..., kmax
       !
       do i = 1, kmax+1
          shlay(i) = i-1
       enddo
    else
       !
       ! layers specified for map-output
       ! Detect the number of output layers(kmaxout):
       !   Search for the first i for which ival(i) == -999. Then kmaxout = i-1
       ! On the flow: check that the specified layers are valid
       ! 
       kmaxout = kmax + 1
       do i = 1, kmax+1
          if (ival(i) == -999) then
             kmaxout = i-1
             exit
          endif
          if (ival(i)<0 .or. ival(i)>kmax) then
             write (message,'(a,i0,a)') "Invalid layer specification '", ival(i), "' in keyword SHlay"
             call prterr(lundia, 'U021', trim(message))
             call d3stop(1, gdp)
          endif
       enddo
       !
       ! Adapt the size of array shlay in case kmaxout<kmax
       !
       if (kmaxout /= kmax) then
          deallocate (gdp%gdpostpr%shlay, stat = istat)
          allocate (gdp%gdpostpr%shlay(kmaxout), stat = istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'RDPRFL: memory alloc error')
             call d3stop(1, gdp)
          endif
          shlay => gdp%gdpostpr%shlay
       endif
       !
       ! Fill shlay with the values of ival in increasing order
       !
       icount = 1
       do i = 0, kmax
          do j = 1, kmaxout
             if (ival(j) == i) then
                shlay(icount) = i
                icount        = icount + 1
                exit
             endif
          enddo
       enddo
       write(message,'(a)') "History output is written for layer(s)"
       do i=1, size(shlay)
          write(message,'(a,a,i0)') trim(message), ' ', shlay(i)
       enddo
       call prterr(lundia, 'G051',trim(message))
    endif
    !
    ! Read flag for test results in zsol file
    !
    keyw  = 'Tstprt'
    ntrec = nrrec
    lkw   = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc     = 1
       cdef(:1) = 'N'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          chulp(:1) = cdef(:1)
       endif
       tstprt = .false.
       if (chulp(:1)=='y' .or. chulp(:1)=='Y') tstprt = .true.
    endif
    deallocate(ival, stat=istat)
end subroutine rdprfl
