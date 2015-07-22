subroutine wrspcp(comfil    ,lundia    ,error     ,nsrc      ,namsrc    , &
                & mnksrc    ,xyzsrc    ,gdp       )
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
!  $Id: wrspcp.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrspcp.f90 $
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
    integer, parameter :: nelmx = 4
!
! Global variables
!
    integer                                        :: lundia !  Description and declaration in inout.igs
    integer                                        :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(7, nsrc)              :: mnksrc !  Description and declaration in esm_alloc_int.f90
    logical                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)     , dimension(3, nsrc)              :: xyzsrc !  Description and declaration in esm_alloc_real.f90
    character(*)                                   :: comfil !!  Name for communication file com-<case><label>
    character(20), dimension(nsrc)                 :: namsrc !  Description and declaration in esm_alloc_char.f90
!
!
! Local variables
!
    integer                                    :: ierr   ! Flag for error when writing to Communication file 
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of bytes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grpnam ! Data-group name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                             :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grpnam/'SPECPOINTS'/
    data elmnms/'NSRC', 'NAMSRC', 'MNKSRC', 'XYZSRC'/
    data elmqty/4*' '/
    data elmunt/3*'[   -   ]', '[   M   ]'/
    data elmtps/'INTEGER', 'CHARACTER', 'INTEGER', 'REAL'/
    data nbytsg/4, 20, 4, 4/
    data elmdes/'Number of sources                                             '&
       & , 'Name of discharge sources                                     ',    &
        & '(M,N,K) indices of discharge sources and time dep. location   ',      &
        & 'Exact X,Y,Z-position                                          '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrspcp)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----Initialize local variables
    !
    ierr = 0
    wrswch = .true.
    !
    !-----Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       if (nsrc>0) then
          call filldm(elmdms    ,2         ,1         ,nsrc      ,0         , &
                    & 0         ,0         ,0         )
          call filldm(elmdms    ,3         ,2         ,7         ,nsrc      , &
                    & 0         ,0         ,0         )
          call filldm(elmdms    ,4         ,2         ,3         ,nsrc      , &
                    & 0         ,0         ,0         )
       endif
    endif
    !
    !-----Write all elements to file; all definition and creation of files,
    !     data groups, cells and elements is handled by PUTGET.
    !
    !-----element  1 NSRC
    !
    idummy(1) = nsrc
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    !-----element  2 NAMSRC (only if NSRC > 0)
    !
    if (nsrc>0) then
       call putgtc(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(2) ,celidt    ,wrswch    ,ierr      ,namsrc    )
       if (ierr/=0) goto 9999
       !
       !--------element  3 MNKSRC (only if NSRC > 0)
       !
       call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(3) ,celidt    ,wrswch    ,ierr      ,mnksrc    )
       if (ierr/=0) goto 9999
       !
       !--------element  4 XYZSRC (only if NSRC > 0)
       !
       call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(4) ,celidt    ,wrswch    ,ierr      ,xyzsrc    )
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
end subroutine wrspcp
