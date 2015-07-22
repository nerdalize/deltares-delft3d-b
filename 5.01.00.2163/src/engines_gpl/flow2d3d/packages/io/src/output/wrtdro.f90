subroutine wrtdro(lundia    ,error     ,trifil    ,itdroc    ,itdrof    , &
                & itdroi    ,ndro      ,xydro     ,sferic    ,gdp       )
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
!  $Id: wrtdro.f90 1388 2012-04-05 20:26:08Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrtdro.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying groups (2 & 3) to the
!              NEFIS DRO-DAT file
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
    integer, parameter :: nelmx = 2
!
! Global variables
!
    integer                          , intent(in)  :: itdroc !!  Current time counter for the history data file
    integer                          , intent(in)  :: itdrof !  Description and declaration in inttim.igs
    integer                          , intent(in)  :: itdroi !  Description and declaration in inttim.igs
    integer                                        :: lundia !  Description and declaration in inout.igs
    integer                                        :: ndro   !  Description and declaration in dimens.igs
    logical                          , intent(in)  :: sferic !  Description and declaration in tricom.igs
    logical                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(2, ndro)               :: xydro  !  Description and declaration in esm_alloc_real.f90
    character(*)                     , intent(in)  :: trifil !!  File name for FLOW NEFIS output
                                                             !!  files (tri"h/m/d"-"casl""labl".dat/def)
!
!
! Local variables
!
    integer                                    :: ierror ! Local errorflag for NEFIS files 
    integer                                    :: nelmx2
    integer                                    :: nelmx3
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of bytes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grnam2
    character(16)                              :: grnam3
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the NEFIS-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                             :: filnam ! Help var. for FLOW file name 
    character(256)                             :: errmsg ! Character var. containing the error message to be written to file. The message depends on the error. 
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grnam2/'dro-info-series'/
    data grnam3/'dro-series'/
    data elmnms/'ITDROC', 'XYDRO'/
    data elmqty/2*' '/
    data elmunt/'[   -   ]', '[   M   ]'/
    data elmtps/'INTEGER', 'REAL'/
    data nbytsg/2*4/
    data elmdes/'timestep number (ITDROC*DT*TUNIT := time in sec from ITDATE)  '&
       & , 'x- and y-coordinates of drogue tracks                         '/
!
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrtdro)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----initialisation
    !
    nelmx2 = 1
    nelmx3 = nelmx - 1
    ierror = 0
    celidt = int((itdroc - itdrof + 1.01*itdroi)/itdroi)
    !
    if (sferic) then
       elmunt(2) = '[  DEG  ]'
    endif
    !
    filnam = trifil(1:3) // 'd' // trifil(5:)
    errmsg = ' '
    wrswch = .true.
    !
    !-----Set up the element dimension
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,2         ,2         ,ndro      , &
                 & 0         ,0         ,0         )
    endif
    !
    !-----group 2, element 1 'ITDROC'
    !
    idummy(1) = itdroc
    call putgti(filnam    ,grnam2    ,nelmx2    ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 999
    !
    !-----group 3: element 1 'XYDRO'
    !
    call putgtr(filnam    ,grnam3    ,nelmx3    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(2) ,celidt    ,wrswch    ,ierror   ,xydro     )
    if (ierror/=0) then
    endif
    !
    !-----write error message if error occurred and set error = .true.
    !     the files will be closed in clsnef (called in triend)
    !
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrtdro
