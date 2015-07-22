subroutine wrparm(comfil    ,lundia    ,error     , &
                & dt        ,nfltyp    ,tscale    ,itlen     ,it01      , &
                & it02      ,tzone     ,gdp       )
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
!  $Id: wrparm.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrparm.f90 $
!!--description-----------------------------------------------------------------
!
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
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
    real(fp)                 , pointer :: rhow
    real(fp)                 , pointer :: ag
!
! Local parameters
!
    integer, parameter :: nelmx = 9
!
! Global variables
!
    integer     , intent(in)  :: it01   !  Description and declaration in esm_alloc_int.f90
    integer     , intent(in)  :: it02   !  Description and declaration in esm_alloc_int.f90
    integer     , intent(in)  :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                   :: lundia !  Description and declaration in inout.igs
    integer     , intent(in)  :: nfltyp !  Description and declaration in esm_alloc_int.f90
    logical     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , intent(in)  :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: tscale !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)  :: tzone  !  Description and declaration in exttim.igs
    character(*)              :: comfil !!  Name for communication file
!
! Local variables
!
    integer                         :: i       ! Help variable
    integer                         :: ierr    ! Flag for error when writing to Communication file 
    integer      , dimension(1)     :: idummy  ! Help array to read/write Nefis files 
    integer      , dimension(nelmx) :: nbytsg  ! Array containing the number of by- tes of each single ELMTPS 
    integer, external               :: neferr
    logical                         :: wrswch  ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)     , dimension(1)     :: rdummy  ! Help array to read/write Nefis files 
    character(10), dimension(nelmx) :: elmunt  ! Array with element physical unit 
    character(16)                   :: grpnam  ! Data-group name defined for the COM-files 
    character(16), dimension(nelmx) :: elmnms  ! Element name defined for the COM-files 
    character(16), dimension(nelmx) :: elmqty  ! Array with element quantity 
    character(16), dimension(nelmx) :: elmtps  ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                  :: errmsg  ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64), dimension(nelmx) :: elmdes  ! Array with element description 
!
! Data statements
!
    data grpnam/'PARAMS'/
    data elmnms/'AG', 'RHOW', 'DT', 'NFLTYP', 'TSCALE', 'IT01', 'IT02', 'TZONE', 'ITLEN' /
    data elmqty/nelmx*' '/
    data elmunt/'[  M/S2 ]', '[ KG/M3 ]', '[ TUNIT ]', '[   -   ]', '[   S   ]', &
              & '[YYYYMMDD]', '[ HHMMSS]', '[ HOUR  ]', '[ TSCALE]'/
    data elmtps/3*'REAL', 'INTEGER', 'REAL', 2*'INTEGER', 'REAL', 'INTEGER'/
    data nbytsg/nelmx*4/
    data elmdes/'Acceleration of gravity                                       ', &
            &   'Density of water                                              ', &
            &   'Timestep FLOW                                              ', &
            &   'Dry point proc. 0 = NO  1 = MEAN  2 = MAX  3 = MIN            ', &
            &   'Basic unit of time, expressed in seconds                      ', &
            &   'Reference date                                                ', &
            &   'Reference time                                                ', &
            &   'Local time zone                                               ', &
            &   'Length of tide cycle ; stand alone and no wave 0              '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrparm)
    first    => nefiselem%first
    celidt   => nefiselem%celidt
    elmdms   => nefiselem%elmdms
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    !
    ! Initialize local variables
    !
    ierr   = 0
    wrswch = .true.
    !
    ! Set up the element dimensions
    !
    if (first) then
       first = .false.
       i = 1   ! 'AG'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'RHOW'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'DT'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'NFLTYP'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'TSCALE'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'IT01'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'IT02'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'TZONE'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       i = i+1 ! 'ITLEN'
       call filldm(elmdms    ,i         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
    endif
    !
    ! Write all elements to file; all definition and creation of files,
    !     data groups, cells and elements is handled by PUTGET.
    !
    ! element AG
    !
    i = 1
    rdummy(1) = ag
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) goto 9999
    !
    ! element RHOW
    ! 
    i = i+1
    rdummy(1) = rhow
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) goto 9999
    !
    ! element DT
    !
    i = i+1
    rdummy(1) = dt
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) goto 9999
    !
    ! element NFLTYP
    !
    i = i+1
    idummy(1) = nfltyp
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element TSCALE
    !
    i = i+1
    rdummy(1) = tscale
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) goto 9999
    !
    ! element IT01
    !
    i = i+1
    idummy(1) = it01
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element IT02
    !
    i = i+1
    idummy(1) = it02
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element TZONE
    !
    i = i+1
    rdummy(1) = tzone
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) goto 9999
    !
    ! element ITLEN
    !
    i = i+1
    idummy(1) = itlen
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(i) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrparm
