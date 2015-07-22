subroutine wrthisbal(ithisc    ,trifil    ,lundia    ,error     ,gdp       )
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
!  $Id: wrthisbal.f90 1677 2012-07-01 21:04:29Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrthisbal.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the time varying mass balance data to the NEFIS HIS-DAT file
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
    logical                        , pointer :: first
    integer                        , pointer :: celidt
    type (nefiselement)            , pointer :: nefiselem
    !
    logical                        , pointer :: massbal
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
    integer      , dimension(:,:)  , pointer :: neighb
    real(fp)     , dimension(:)    , pointer :: accdps
    real(fp)     , dimension(:)    , pointer :: horareas
    real(fp)     , dimension(:)    , pointer :: volumes
    real(fp)     , dimension(:,:)  , pointer :: mass_r1
    real(fp)     , dimension(:,:)  , pointer :: fluxes
    real(fp)     , dimension(:,:,:), pointer :: fluxes_r1
    real(fp)     , dimension(:,:,:), pointer :: fluxes_sd
    !
    integer                        , pointer :: lstsci
    integer                        , pointer :: lsedtot
!
! Global variables
!
    integer                           , intent(in)  :: ithisc      !!  Current time counter for the HIS data file
    integer                           , intent(in)  :: lundia      !  Description and declaration in inout.igs
    character(*)                      , intent(in)  :: trifil      !!  File name for FLOW NEFIS output
    logical                           , intent(out) :: error       !!  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer                           , external    :: putelt
    integer                           , external    :: inqmxi
    integer                           , external    :: clsnef
    integer                           , external    :: open_datdef
    integer                           , external    :: neferr
!
    integer                                         :: fds          ! NEFIS file handle
    integer                                         :: i
    integer         , dimension(1)                  :: idummy       ! Help array to read/write Nefis files 
    integer                                         :: ierror       ! Local error flag for NEFIS files 
    integer                                         :: l
    integer                                         :: n
    integer         , dimension(3,5)                :: uindex
    character(16)                                   :: grpnam       ! Data-group name for the NEFIS-file
    character(256)                                  :: filnam       ! Help var. for FLOW file name 
    character(256)                                  :: errmsg       ! Character var. containing the error message to be written to file. The message depends on the error. 
    character(1024)                                 :: error_string
!
!! executable statements -------------------------------------------------------
!
    massbal        => gdp%gdmassbal%massbal
    !
    if (.not. massbal) return
    !
    nefiselem      => gdp%nefisio%nefiselem(nefiswrthisbal)
    first          => nefiselem%first
    celidt         => nefiselem%celidt
    !
    nbalpol        => gdp%gdmassbal%nbalpol
    nneighb        => gdp%gdmassbal%nneighb
    neighb         => gdp%gdmassbal%neighb
    accdps         => gdp%gdmassbal%accdps
    horareas       => gdp%gdmassbal%horareas
    volumes        => gdp%gdmassbal%volumes
    mass_r1        => gdp%gdmassbal%mass_r1
    fluxes         => gdp%gdmassbal%fluxes
    fluxes_r1      => gdp%gdmassbal%fluxes_r1
    fluxes_sd      => gdp%gdmassbal%fluxes_sd
    !
    lstsci         => gdp%d%lstsci
    lsedtot        => gdp%d%lsedtot
    !
    ! Initialize local variables
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    errmsg = ' '
    grpnam = 'his-bal-series'
    !
    if (first) then
       call wrihisbal(trifil    ,lundia    ,error     ,gdp       )
       !
       ! Set up the element chracteristics
       !
       call addelm(nefiswrthisbal,'ITHISC',' ','[   -   ]','INTEGER',4    , &
          & 'timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrthisbal,'BALVOLUME',' ','[  M3   ]','REAL',4    , &
          & 'Volume within polygon                                       ', &
          & 1         ,nbalpol   ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrthisbal,'BALFLUX',' ','[  M3   ]','REAL',4      , &
          & 'Accumulated flux between polygons                           ', &
          & 2         ,2         ,nneighb   ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       if (lstsci>0) then
          call addelm(nefiswrthisbal,'BALR1CONC',' ','[   -   ]','REAL',4    , &
             & 'Average concentration within polygon                        ', &
             & 2         ,nbalpol   ,lstsci    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrthisbal,'BALR1FLUX',' ','[   -   ]','REAL',4    , &
             & 'Accumulated constituent flux between polygons               ', &
             & 3         ,2         ,nneighb   ,lstsci    ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (lsedtot>0) then
          call addelm(nefiswrthisbal,'BALDPS',' ','[   -   ]','REAL',4       , &
             & 'Average bottom depth within polygon                         ', &
             & 1         ,nbalpol   ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrthisbal,'BALSDFLUX',' ','[   -   ]','REAL',4    , &
             & 'Accumulated sediment flux between polygons                  ', &
             & 3         ,2         ,nneighb   ,lsedtot   ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       !
       call defnewgrp(nefiswrthisbal ,filnam    ,grpnam   ,gdp)
    endif
    !
    nefiselem      => gdp%nefisio%nefiselem(nefiswrthisbal)
    first          => nefiselem%first
    celidt         => nefiselem%celidt
    !
    ierror = open_datdef(filnam   ,fds      )
    if (ierror/= 0) goto 9999
    if (first) then
       !
       ! end of initialization, don't come here again
       !
       ierror = inqmxi(fds, grpnam, celidt)
       first = .false.
    endif
    !
    celidt = celidt + 1
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = celidt ! start index
    uindex (2,1) = celidt ! end index
    uindex (3,1) = 1      ! increment in time
    !
    idummy(1)   = ithisc
    ierror = putelt(fds, grpnam, 'ITHISC', uindex, 1, idummy)
    if (ierror/=0) goto 9999
    !
    call sbuff_checksize(nbalpol)
    do n = 1,nbalpol
       sbuff(n) = real(volumes(n),sp)
    enddo
    ierror = putelt(fds, grpnam, 'BALVOLUME', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    call sbuff_checksize(2*nneighb)
    i = 0
    do n = 1,nneighb
       sbuff(i+1) = real(fluxes(1,n),sp)
       sbuff(i+2) = real(fluxes(2,n),sp)
       i = i+2
    enddo
    ierror = putelt(fds, grpnam, 'BALFLUX', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    if (lstsci>0) then
       call sbuff_checksize(nbalpol*lstsci)
       i = 0
       do l = 1,lstsci
          do n = 1,nbalpol
             i = i+1
             sbuff(i) = real(mass_r1(n,l)/volumes(n),sp)
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'BALR1CONC', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       call sbuff_checksize(2*nneighb*lstsci)
       i = 0
       do l = 1,lstsci
          do n = 1,nneighb
             sbuff(i+1) = real(fluxes_r1(1,n,l),sp)
             sbuff(i+2) = real(fluxes_r1(2,n,l),sp)
             i = i+2
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'BALR1FLUX', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
    endif
    !
    if (lsedtot>0) then
       !call sbuff_checksize(nbalpol) !already checked for volumes
       do n = 1,nbalpol
          sbuff(n) = real(accdps(n)/horareas(n),sp)
       enddo
       ierror = putelt(fds, grpnam, 'BALDPS', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       call sbuff_checksize(2*nneighb*lsedtot)
       i = 0
       do l = 1,lsedtot
          do n = 1,nneighb
             sbuff(i+1) = real(fluxes_sd(1,n,l),sp)
             sbuff(i+2) = real(fluxes_sd(2,n,l),sp)
             i = i+2
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'BALSDFLUX', uindex, 1, sbuff)
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
end subroutine wrthisbal
