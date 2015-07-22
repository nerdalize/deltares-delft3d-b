subroutine wrboun(comfil    ,lundia    ,error     ,norow     ,nocol     , &
                & noroco    ,nrob      ,nto       ,irocol    ,mnbnd     , &
                & nob       ,gdp       )
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
!  $Id: wrboun.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrboun.f90 $
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
!
! Local parameters
!
    integer, parameter :: nelmx = 8
!
! Global variables
!
    integer                                    :: lundia !  Description and declaration in inout.igs
    integer, intent(in)                        :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                    :: noroco !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                        :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                    :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer                                    :: nto    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nto)                 :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, noroco)              :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(8, nrob)                :: nob    !  Description and declaration in esm_alloc_int.f90
    logical                      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                               :: comfil !!  Name for communication file
                                                         !!  com-<case><label>
!
! Local variables
!
    integer                         :: ierr    ! Flag for error when writing to Communication file 
    integer, dimension(1)           :: idummy  ! Help array to read/write Nefis files 
    integer, dimension(nelmx)       :: nbytsg  ! Array containing the number of by- tes of each single ELMTPS 
    integer, external               :: neferr
    logical                         :: wrswch  ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
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
    data grpnam/'BOUNDCNST'/
    data elmnms/'NOROW', 'NOCOL', 'NOROCO', 'IROCOL', 'NTO', 'MNBND', 'NROB', 'NOB'/
    data elmqty/8*' '/
    data elmunt/8*'[   -   ]'/
    data elmtps/8*'INTEGER'/
    data nbytsg/8*4/
    data elmdes/'Number of computational grid rows in IROCOL table             ',  &
              & 'Number of computational grid columns in IROCOL table          ',  &
              & 'NOROW+NOCOL                                                   ',  &
              & 'Administration of zeta-points, IROCOL-table                   ',  &
              & 'Number of open boundaries                                     ',  &
              & 'Open boundary begin and end points (ml, mh, nl, nh, kl, kh)   ',  &
              & 'Number of open boundary points                                ',  &
              & 'Administration of open boundary points                        '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrboun)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    ! Initialize local variables
    !
    ierr = 0
    wrswch = .true.
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
       call filldm(elmdms    ,4         ,2         ,5         ,noroco    , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,5         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       if (nto>0) then
          call filldm(elmdms    ,6         ,2         ,6         ,nto       , &
                    & 0         ,0         ,0         )
       endif
       call filldm(elmdms    ,7         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       if (nrob>0) then
          call filldm(elmdms    ,8         ,2         ,8         ,nrob      , &
                    & 0         ,0         ,0         )
       endif
    endif
    !
    ! Write all elements to file; all definition and creation of files,
    ! data groups, cells and elements is handled by PUTGTI.
    !
    ! element  1 NOROW
    !
    idummy(1) = norow
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element  2 NOCOL
    !
    idummy(1) = nocol
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element  3 NOROCO
    !
    idummy(1) = noroco
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element  4 IROCOL
    !
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(4) ,celidt    ,wrswch    ,ierr      ,irocol    )
    if (ierr/=0) goto 9999
    !
    ! element  5 NTO
    !
    idummy(1) = nto
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(5) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element  6 MNBND (only if NTO > 0)
    !
    if (nto>0) then
       call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(6) ,celidt    ,wrswch    ,ierr      ,mnbnd(1:6,:) )
       if (ierr/=0) goto 9999
    endif
    !
    ! element  7 NROB
    !
    idummy(1) = nrob
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(7) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    ! element  8 NOB (only if NROB > 0)
    !
    if (nrob>0) then
       call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(8) ,celidt    ,wrswch    ,ierr      ,nob       )
       if (ierr/=0) then
       endif
    endif
    !
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrboun
