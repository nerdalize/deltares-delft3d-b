subroutine rdtimw(comfil    ,lundia    ,error     ,ntwav     ,timwav    , &
                & maxtim    ,waverd    ,nmaxus    ,mmax      ,gdp       )
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
!  $Id: rdtimw.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/rdtimw.f90 $
!!--description-----------------------------------------------------------------
!
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    integer, dimension(:, :) , pointer :: elmdms
    type (nefiselement)      , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 3
!
! Global variables
!
    integer                                   :: lundia !  Description and declaration in inout.igs
    integer                     , intent(in)  :: maxtim !!  Max.nr. of timesteps for the communication file
    integer                                   :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                   :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                   :: ntwav  !!  Total number of timesteps on comm. file (to read from)
    integer , dimension(maxtim)               :: timwav !!  Array with time steps on comm. file for wave results
    logical                     , intent(out) :: error  !!  Flag = TRUE if an error is encountered
    logical                                   :: waverd !!  Flag = TRUE if wave process and communication file exist
    character(*)                              :: comfil !!  Name for communication file com-<case><label>
!
! Local variables
!
    integer                           :: ierr    ! Flag for error when writing to Communication file 
    integer                           :: nelmx1
    integer                           :: nelmx2
    integer        , dimension(1)     :: idummy  ! Help array to read/write Nefis files 
    integer        , dimension(nelmx) :: nbytsg  ! Array containing the number of by- tes of each single ELMTPS 
    integer        , external         :: neferr
    logical                           :: wrswch  ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(10)  , dimension(nelmx) :: elmunt  ! Array with element physical unit 
    character(16)                     :: grnam1  ! Data-group name defined for the COM-files (CURNT) 
    character(16)                     :: grnam2  ! Data-group name defined for the COM-files (CURTIM) 
    character(16)  , dimension(nelmx) :: elmnms  ! Element name defined for the COM-files 
    character(16)  , dimension(nelmx) :: elmqty  ! Array with element quantity 
    character(16)  , dimension(nelmx) :: elmtps  ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                    :: errmsg  ! Character var. containing the errormessage to be written to file. 
                                                 ! The message depends on the error. 
    character(64)  , dimension(nelmx) :: elmdes  ! Array with element description 
!
! Data statements
!
    data grnam1/'WAVNT'/
    data grnam2/'WAVTIM'/
    data elmnms/'NTWAV', 'SWFLUX', 'TIMWAV'/
    data elmqty/nelmx*' '/
    data elmunt/'[   -   ]', '[   -   ]', '[ TSCALE]'/
    data elmtps/'INTEGER', 'LOGICAL', 'INTEGER'/
    data nbytsg/nelmx*4/
    data elmdes/'Number of wave fields in group WAVTIM                         ',    &
              & 'Mass flux written to comm. file (.true. or .false.)           ',    &
              & 'Time of wave field rel. to reference date/time                '/
!
!! executable statements -------------------------------------------------------
!
!
    nefiselem => gdp%nefisio%nefiselem(nefisrdtimw)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    ! Initialize local variables
    !
    ierr   = 0
    nelmx1 = 1
    nelmx2 = nelmx - nelmx1
    wrswch = .false.
    !
    ! Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
    endif
    !
    ! Read the number of timesteps available at the file
    !
    celidt = 1
    call putgti(comfil    ,grnam1    ,nelmx1    ,elmnms(1) ,elmdms(1, 1)         , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1) , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 8888
    ntwav = idummy(1)
    !
    ! Test if number of time steps on file are inside defined array boundary maxtim.
    ! If error occurred then write errormessage to diagnostic file
    !
    if (ntwav>maxtim) then
       call prterr(lundia    ,'D008'    ,' '       )
       !
       error = .true.
       goto 9999
    endif
    !
    ! Read the array with time information
    !
    do celidt = 1, ntwav
       call putgti(comfil    ,grnam2    ,nelmx2    ,elmnms(3) ,elmdms(1, 3)         , &
                 & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
                 & elmnms(3) ,celidt    ,wrswch    ,ierr      ,timwav(celidt)       )
       if (ierr/=0) then
          exit
       endif
    enddo
    !
 8888 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
    !
 9999 continue
end subroutine rdtimw
