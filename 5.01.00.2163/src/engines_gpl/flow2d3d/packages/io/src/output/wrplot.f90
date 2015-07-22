subroutine wrplot(filnam    ,lundia    ,error     ,mmax      ,nmax      , &
                & nmaxus    ,kcs       ,ibuff     ,xz        ,yz        , &
                & rbuff     ,sferic    ,gdp       )
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
!  $Id: wrplot.f90 1388 2012-04-05 20:26:08Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrplot.f90 $
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
    integer                                                                        :: lundia !  Description and declaration in inout.igs
    integer                                                                        :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(nmaxus, mmax)                                          :: ibuff  !  Description and declaration in esm_alloc_int.f90
    logical                                                          , intent(in)  :: sferic !  Description and declaration in tricom.igs
    logical                                                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nmaxus, mmax)                                          :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                                   :: filnam !!  Name for output file
                                                                                             !!  Comm. file: com-<case><label>
                                                                                             !!  Map file: trim-<case><label>
!
!
! Local variables
!
    integer                                    :: ierr   ! Flag for error when writing to Communication file 
    integer                                    :: m
    integer                                    :: md
    integer                                    :: n
    integer                                    :: nd
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
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
    data grpnam/'TEMPOUT'/
    data elmnms/'XWAT', 'YWAT', 'CODB', 'CODW'/
    data elmqty/4*' '/
    data elmunt/2*'[   M   ]', 2*'[   -   ]'/
    data elmtps/2*'REAL', 2*'INTEGER'/
    data nbytsg/4*4/
    data elmdes/'X-coord. water level point in local system                    '&
       & , 'Y-coord. water level point in local system                    ',    &
        & '1/-1 Active/Non-active bottom point ( w.r.t. coordinates )    ',      &
        & '1/-1 Active/Non-active water level point (w.r.t. coordinates )'/
!
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrplot)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----Initialize local variables
    !
    ierr = 0
    wrswch = .true.
    !
    if (sferic) then
       elmunt(1) = '[  DEG  ]'
       elmunt(2) = '[  DEG  ]'
    endif
    !
    !-----Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
    endif
    !
    !-----Write all elements to file; all definition and creation of files,
    !     data groups, cells and elements is handled by PUTGET.
    !
    !-----element  1 XZ
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = xz(n, m)
       enddo
    enddo
    !
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) goto 9999
    !
    !-----element  2 YZ
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = yz(n, m)
       enddo
    enddo
    !
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) goto 9999
    !
    !-----element  3 CODB
    !
    do m = 1, mmax
       ibuff(1, m) = -1
    enddo
    do n = 1, nmaxus
       ibuff(n, 1) = -1
    enddo
    !
    do m = 2, mmax
       md = m - 1
       do n = 2, nmaxus
          nd = n - 1
          ibuff(n, m) = -1
          if (kcs(n, m)==1) then
             ibuff(n, m) = 1
             ibuff(n, md) = 1
             ibuff(nd, m) = 1
             ibuff(nd, md) = 1
          endif
       enddo
    enddo
    !
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierr      ,ibuff     )
    if (ierr/=0) goto 9999
    !
    !-----element  4 CODW
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = -1
          if (kcs(n, m)==1) ibuff(n, m) = 1
       enddo
    enddo
    !
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(4) ,celidt    ,wrswch    ,ierr      ,ibuff     )
    if (ierr/=0) then
    endif
    !
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrplot
