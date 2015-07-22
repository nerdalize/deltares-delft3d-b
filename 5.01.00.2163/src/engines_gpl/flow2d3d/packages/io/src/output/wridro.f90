subroutine wridro(lundia    ,error     ,trifil    ,ndro      ,itdrof    , &
                & itdroi    ,simdat    ,itdate    ,tunit     ,dt        , &
                & namdro    ,mndro     ,itdro     ,ibuff     ,dxydro    , &
                & gdp       )
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
!  $Id: wridro.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wridro.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the initial group 1 ('dro-const') to
!              DRO-DAT
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
    integer, parameter :: nelmx = 9
!
! Global variables
!
    integer                          , intent(in)  :: itdate !  Description and declaration in exttim.igs
    integer                          , intent(in)  :: itdrof !  Description and declaration in inttim.igs
    integer                          , intent(in)  :: itdroi !  Description and declaration in inttim.igs
    integer                                        :: lundia !  Description and declaration in inout.igs
    integer                                        :: ndro   !  Description and declaration in dimens.igs
    integer      , dimension(2, ndro)              :: ibuff  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(2, ndro), intent(in)  :: itdro  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(2, ndro), intent(in)  :: mndro  !  Description and declaration in esm_alloc_int.f90
    logical                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                         , intent(in)  :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)                         , intent(in)  :: tunit  !  Description and declaration in exttim.igs
    real(fp)     , dimension(2, ndro)              :: dxydro !  Description and declaration in esm_alloc_real.f90
    character(*)                     , intent(in)  :: trifil !!  File name for FLOW NEFIS output
                                                             !!  files (tri"h/m/d"-"casl""labl".dat/def)
    character(16)                    , intent(in)  :: simdat !!  Simulation date representing the flow condition at this date
    character(20), dimension(ndro)                 :: namdro !  Description and declaration in esm_alloc_char.f90
!
!
! Local variables
!
    integer                                    :: i
    integer                                    :: id
    integer                                    :: ierror ! Local errorflag for NEFIS files 
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(2)               :: ival   ! Local array for writing ITDATE and time (:= 00:00:00) 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of bytes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)      , dimension(1)               :: rdummy ! Help array to read/write Nefis files 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grnam1 ! Data-group name defined for the NEFIS-files 
    character(16) , dimension(1)               :: cdum16
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the NEFIS-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                             :: filnam ! Help var. for FLOW file name 
    character(256)                             :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grnam1/'dro-const'/
    data elmnms/'ITDATE', 'TUNIT', 'DT', 'SIMDAT', 'NDRO', 'NAMDRO', 'MNDRO',   &
        & 'DXYDRO', 'NTDRO'/
    data elmqty/9*' '/
    data elmunt/'[YYYYMMDD]', '[   S   ]', 7*'[   -   ]'/
    data elmtps/'INTEGER', 2*'REAL', 'CHARACTER', 'INTEGER', 'CHARACTER',       &
        & 'INTEGER', 'REAL', 'INTEGER'/
    data nbytsg/3*4, 16, 4, 20, 3*4/
    data (elmdes(i), i = 1, nelmx)                                              &
         & /'Initial date (input) & time (default 00:00:00)                ',    &
         & 'Time scale related to seconds                                 ',     &
         & 'Time step (DT*TUNIT sec)                                      ',     &
         & 'Simulation date and time [YYYYMMDD  HHMMSS]                   ',     &
         & 'Number of drogues released                                    ',     &
         & 'Name of the drogue                                            ',     &
         & '(m,n) indices starting point of drogue track                  ',     &
         & '(dx,dy) indices starting point of drogue track                ',     &
         & 'actual number of step for drogue to be traced                 '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswridro)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----initialisation
    !
    ierror = 0
    celidt = 1
    !
    filnam = trifil(1:3) // 'd' // trifil(5:)
    errmsg = ' '
    wrswch = .true.
    !
    !-----Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,2         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,5         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,6         ,1         ,ndro      ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,7         ,2         ,2         ,ndro      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,8         ,2         ,2         ,ndro      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,9         ,2         ,2         ,ndro      , &
                 & 0         ,0         ,0         )
    endif
    !
    !-----group 1, element 'ITDATE'
    !
    ival(1) = itdate
    ival(2) = 000000
    call putgti(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror    ,ival      )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'TUNIT'
    !
    rdummy(1) = tunit
    call putgtr(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'DT'
    !
    rdummy(1) = dt
    call putgtr(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierror    ,rdummy    )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'SIMDAT'
    !
    cdum16(1) = simdat
    call putgtc(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(4) ,celidt    ,wrswch    ,ierror    ,cdum16    )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'NDRO'
    !
    idummy(1) = ndro
    call putgti(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(5) ,celidt    ,wrswch    ,ierror    ,idummy    )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'NAMDRO'
    !
    call putgtc(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(6) ,celidt    ,wrswch    ,ierror    ,namdro    )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'MNDRO'
    !              because the coordinates used to calculate the drogue
    !              tracks with are different from the coordinates the user
    !              defines (right upper corner versus left lower corner
    !              of a gridcell) the IBUFF array is used
    !
    do id = 1, ndro
       ibuff(1, id) = mndro(1, id) - 1
       ibuff(2, id) = mndro(2, id) - 1
    enddo
    !
    call putgti(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(7) ,celidt    ,wrswch    ,ierror    ,ibuff     )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'DXYDRO'
    !
    call putgtr(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(8) ,celidt    ,wrswch    ,ierror    ,dxydro    )
    if (ierror/=0) goto 999
    !
    !-----group 1, element 'NTDRO'
    !              Time frame relative to start time step (ITDROF) and time
    !              step interval (ITDROI). The IBUFF array is used
    !
    do id = 1, ndro
       ibuff(1, id) = (itdro(1, id) - itdrof + 1.01*itdroi)/itdroi
       ibuff(2, id) = (itdro(2, id) - itdrof + 1.01*itdroi)/itdroi
    enddo
    !
    call putgti(filnam    ,grnam1    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(9) ,celidt    ,wrswch    ,ierror    ,ibuff     )
    if (ierror/=0) then
    endif
    !
    !-----write errormessage if error occurred and set error = .true.
    !     the files will be closed in clsnef (called in triend)
    !
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wridro
