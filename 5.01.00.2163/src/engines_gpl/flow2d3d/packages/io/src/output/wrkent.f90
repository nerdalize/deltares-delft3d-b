subroutine wrkent(comfil    ,lundia    ,error     ,itcur     ,ntcur     , &
                & itimc     ,mmax      ,nmax      ,nmaxus    ,kfu       , &
                & kfv       ,ibuff     ,gdp       )
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
!  $Id: wrkent.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrkent.f90 $
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
    integer                                                     , intent(in)  :: itcur  !!  Current time counter for the communication file, where starting point depend on CYCLIC
    integer                                                     , intent(in)  :: itimc  !!  Current time step counter for 2D system
    integer                                                                   :: lundia !  Description and declaration in inout.igs
    integer                                                                   :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: ntcur  !!  Total number of timesteps on communication file (to write to)
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmaxus, mmax)                                          :: ibuff  !  Description and declaration in esm_alloc_int.f90
    logical                                                     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                              :: comfil !!  Name for communication file
                                                                                        !!  com-<case><label>
!
!
! Local variables
!
    integer                                    :: ierr   ! Flag for error when writing to Communication file 
    integer                                    :: m
    integer                                    :: n
    integer                                    :: nelmx1
    integer                                    :: nelmx2
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grnam1 ! Data-group name defined for the COM-files (KENMNT) 
    character(16)                              :: grnam2 ! Data-group name defined for the COM-files (KENMTIM) 
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                             :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grnam1/'KENMNT'/
    data grnam2/'KENMTIM'/
    data elmnms/'NTCUR', 'TIMCUR', 'KFU', 'KFV'/
    data elmqty/4*' '/
    data elmunt/'[   -   ]', '[ TSCALE]', 2*'[   -   ]'/
    data elmtps/4*'INTEGER'/
    data nbytsg/4*4/
    data elmdes/'Number of current fields in groups CURTIM and KENMTIM         '&
       & , 'Time of current field rel.to reference date/time              ',    &
        & '0/1 Non-active/Active u-point                                 ',      &
        & '0/1 Non-active/Active v-point                                 '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrkent)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----Initialize local variables
    !
    ierr   = 0
    wrswch = .true.
    nelmx1 = 1
    nelmx2 = nelmx - nelmx1
    !
    !-----Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
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
    !-----element  1 NTCUR for group KENMNT
    !
    celidt = 1
    idummy(1) = ntcur
    call putgti(comfil    ,grnam1    ,nelmx1    ,elmnms(1) ,elmdms(1, 1)         , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1) , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    !-----element  1 ITIMC for group KENMTIM (cel number ITCUR)
    !
    celidt = itcur
    idummy(1) = itimc
    call putgti(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(2) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    !-----For all array the values are buffered in RBUFF, to be sure that
    !     the correct array element are written to data file
    !
    !-----element  2 KFU for group KENMTIM (cel number ITCUR)
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kfu(n, m)
       enddo
    enddo
    !
    call putgti(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(3) ,celidt    ,wrswch    ,ierr      ,ibuff     )
    if (ierr/=0) goto 9999
    !
    !-----element  2 KFV for group KENMTIM (cel number ITCUR)
    !
    do m = 1, mmax
       do n = 1, nmaxus
          ibuff(n, m) = kfv(n, m)
       enddo
    enddo
    !
    call putgti(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
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
end subroutine wrkent
