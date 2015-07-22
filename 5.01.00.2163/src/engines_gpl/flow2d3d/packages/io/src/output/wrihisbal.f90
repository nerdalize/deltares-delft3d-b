subroutine wrihisbal(trifil    ,lundia    ,error     ,gdp       )
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
!  $Id: wrihisbal.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrihisbal.f90 $
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
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
    integer      , dimension(:,:)  , pointer :: neighb
    character(80), dimension(:)    , pointer :: volnames
    real(fp)     , dimension(:)    , pointer :: horareas
!
! Global variables
!
    integer                                                                           , intent(in)  :: lundia      !  Description and declaration in inout.igs
    character(*)                                                                      , intent(in)  :: trifil      !!  File name for FLOW NEFIS output
    logical                                                                           , intent(out) :: error       !!  Flag=TRUE if an error is encountered
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
    integer                                         :: n
    integer         , dimension(3,5)                :: uindex
    character(16)                                   :: grpnam       ! Data-group name for the NEFIS-file
    character(256)                                  :: filnam       ! Help var. for FLOW file name 
    character(256)                                  :: errmsg       ! Character var. containing the error message to be written to file. The message depends on the error. 
    character(1024)                                 :: error_string
!
!! executable statements -------------------------------------------------------
!
    nbalpol        => gdp%gdmassbal%nbalpol
    nneighb        => gdp%gdmassbal%nneighb
    neighb         => gdp%gdmassbal%neighb
    volnames       => gdp%gdmassbal%volnames
    horareas       => gdp%gdmassbal%horareas
    !
    ! Initialize local variables
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    errmsg = ' '
    grpnam = 'his-bal-const'
    !
    ! Set up the element chracteristics
    !
    call addelm(nefiswrihisbal,'BALVOLNAMES',' ','[   -   ]','CHARACTER',80 , &
       & 'Volume/polygon names                                        ', &
       & 1         ,nbalpol+1 ,0         ,0         ,0         ,0      , &
       & lundia    ,gdp       )
    call addelm(nefiswrihisbal,'BALAREAS',' ','[  M2   ]','REAL',4     , &
       & 'Volume/polygon surface areas                                ', &
       & 1         ,nbalpol+1 ,0         ,0         ,0         ,0      , &
       & lundia    ,gdp       )
    call addelm(nefiswrihisbal,'BALNEIGHB',' ','[   -   ]','INTEGER',4 , &
       & 'Neighbouring volumes/polygons                               ', &
       & 2         ,2         ,nneighb   ,0         ,0         ,0      , &
       & lundia    ,gdp       )
    !
    call defnewgrp(nefiswrihisbal ,filnam    ,grpnam   ,gdp)
    !
    ierror = open_datdef(filnam   ,fds      )
    if (ierror/= 0) goto 9999
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ierror = putelt(fds, grpnam, 'BALVOLNAMES', uindex, 1, volnames)
    if (ierror/= 0) goto 9999
    !
    call sbuff_checksize(nbalpol)
    n = 0
    do n = 1, nbalpol
       sbuff(n) = real(horareas(n),sp)
    enddo
    ierror = putelt(fds, grpnam, 'BALAREAS', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ierror = putelt(fds, grpnam, 'BALNEIGHB', uindex, 1, neighb)
    if (ierror/= 0) goto 9999
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
end subroutine wrihisbal
