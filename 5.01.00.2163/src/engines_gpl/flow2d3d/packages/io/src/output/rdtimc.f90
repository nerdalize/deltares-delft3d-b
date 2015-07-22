subroutine rdtimc(comfil    ,lundia    ,error     ,commrd    ,itlen     , &
                & tscale    ,gdp       )
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
!  $Id: rdtimc.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/rdtimc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Read time parameters from communication file
!                if the file does not exist then commrd = .false.
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
    integer, parameter :: nelmx = 8
!
! Global variables
!
    integer      , intent(out) :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                    :: lundia !  Description and declaration in inout.igs
    logical      , intent(out) :: commrd !!  Flag = TRUE if communication file exists and can be read from
    logical      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)     , intent(out) :: tscale !  Description and declaration in esm_alloc_real.f90
    character(*)               :: comfil !!  First part of file name
!
!
! Local variables
!
    integer                                    :: ierr   ! errorflag when closing the NEFIS files 
    integer                                    :: lfil   ! Actual length of name file COMFIL 
    integer                                    :: luntmp
    integer                                    :: newlun
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)      , dimension(1)               :: rdummy ! Help array to read/write Nefis files 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grpnam ! Data-group name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                             :: fixcom ! fixed size version of comfil, needed for character concatenation 
    character(5)                               :: cdummy ! Character string containing text "dummy" 
    character(256)                             :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grpnam/'PARAMS'/
    data elmnms/'AG', 'RHOW', 'DT', 'NFLTYP', 'TSCALE', 'IT01', 'IT02', 'ITLEN'/
    data elmqty/8*' '/
    data elmunt/'[  M/S2 ]', '[ KG/M3 ]', '[ TUNIT ]', '[   -   ]', '[   S   ]',&
        & '[ YYMMDD]', '[ HHMMSS]', '[ TSCALE]'/
    data elmtps/3*'REAL', 'INTEGER', 'REAL', 3*'INTEGER'/
    data nbytsg/8*4/
    data elmdes/'Acceleration of gravity                                       '&
       & , 'Density of water                                              ',    &
        & 'Timestep FLOW                                                 ',      &
        & 'Dry point proc. 0 = NO  1 = MEAN  2 = MAX  3 = MIN            ',      &
        & 'Basic unit of time, expressed in seconds                      ',      &
        & 'Reference date                                                ',      &
        & 'Reference time                                                ',      &
        & 'Length of tide cycle ; stand alone and no wave 0              '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefisrdtimc)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,5         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,6         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,7         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,8         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
    endif
    !
    !-----Read TSCALE and ITLEN from communication file
    !     read in dummy variables because in case ierr <> 0, the original
    !     values still exist
    !
    wrswch = .false.
    rdummy(1) = 0.
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(5) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) then
       if (ierr== - 211) then
          error  = .false.
          commrd = .false.
       elseif (ierr== - 20011) then
          luntmp = newlun(gdp)
          call noextspaces(comfil    ,lfil      )
          fixcom(1:lfil) = comfil(1:lfil)
          open (luntmp, file = fixcom(1:lfil) // '.def')
          read (luntmp, '(a)') cdummy
          call small(cdummy    ,5         )
          if (cdummy=='dummy') then
             close (luntmp, status = 'delete')
             open (luntmp, file = fixcom(1:lfil) // '.dat')
             close (luntmp, status = 'delete')
             error  = .false.
             commrd = .false.
          else
             ierr = neferr(0, errmsg)
             call prterr(lundia, 'P004', errmsg)
             error = .true.
          endif
       else
          ierr = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error = .true.
       endif
       goto 9999
    endif
    !
    idummy(1) = 0
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(8) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
       goto 9999
    endif
    !
    tscale = rdummy(1)
    itlen = idummy(1)
    !
 9999 continue
end subroutine rdtimc
