subroutine wrgrid(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                & kmax      ,nmaxus    , &
                & xcor      ,ycor      ,guu       ,gvv       ,guv       , &
                & gvu       ,gsqs      ,gsqd      ,alfas     ,thick     , &
                & rbuff     ,rbuffz    ,sferic    ,zmodel    ,zbot      , &
                & zk        ,gdp       )
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
!  $Id: wrgrid.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrgrid.f90 $
!!--description-----------------------------------------------------------------
!
! Write group GRID to the COM-file
!
! (Nefis-)errors may occur when trying to write new elements (SPHERIC, ZK,
! ZMODEL) to an already existing COM-file of older type (e.g. without the new
! elements).
! - Subroutine PUTGTI/PUTGTR will generate an errormessage on the screen
! - In this subroutine, wrgrid, all lines
!   "if (ierr/=0) goto 9999"
!   are replaced by writing a warning to the tri-diag file and continue
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
    real(fp)                 , pointer :: amiss
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    integer, dimension(:, :) , pointer :: elmdms
    type (nefiselement)      , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 19
!
! Global variables
!
    integer                                                                    :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: lundia !  Description and declaration in inout.igs
    integer                                                                    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                      , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                     , intent(in)  :: zbot   !  Description and declaration in zmodel.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gsqd   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                                  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                  , intent(in)  :: zk     !!  Vertical coordinates of cell
                                                                                         !!  interfaces
                                                                                         !!  Flag for activation of Z-MODEL
    real(fp), dimension(kmax+1)                                                :: rbuffz
    real(fp), dimension(nmaxus, mmax)                                          :: rbuff  !  Description and declaration in r-i-ch.igs
    logical                                                      , intent(in)  :: sferic
    logical                                                      , intent(in)  :: zmodel !  Description and declaration in procs.igs
    character(*)                                                               :: comfil !!  Name for communication file
                                                                                         !!  com-<case><label>
!
! Local variables
!
    integer                         :: i
    integer                         :: ierr   ! Flag for error when writing to Communication file 
    integer                         :: k
    integer                         :: m
    integer                         :: n
    integer      , dimension(1)     :: idummy ! Help array to read/write Nefis files 
    integer      , dimension(nelmx) :: nbytsg ! Array containing the number of bytes of each single ELMTPS 
    logical                         :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)     , dimension(1)     :: rdummy ! Help array to read/write Nefis files 
    character(10), dimension(nelmx) :: elmunt ! Array with element physical unit 
    character(16), dimension(1)     :: cdummy ! Data-group name defined for the COM-files 
    character(16)                   :: grpnam ! Data-group name defined for the COM-files 
    character(16), dimension(nelmx) :: elmnms ! Element name defined for the COM-files 
    character(16), dimension(nelmx) :: elmqty ! Array with element quantity 
    character(16), dimension(nelmx) :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                  :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64), dimension(nelmx) :: elmdes ! Array with element description 
!
! Data statements
!
    data grpnam/'GRID'/
    data elmnms/'MMAX'  , 'NMAX'  , 'XORI'   , 'YORI'  , 'ALFORI', &
              & 'XCOR'  , 'YCOR'  , 'GUU'    , 'GVV'   , 'GUV'   , &
              & 'GVU'   , 'GSQS'  , 'GSQD'   , 'ALFAS' , 'KMAX'  , &
              & 'THICK' , 'ZK'    , 'COORDINATES', 'LAYER_MODEL'/
    data elmqty/19*' '/
    data elmunt/2*'[   -   ]', 2*'[   M   ]', '[  DEG  ]', 6*'[   M   ]', &
              & 2*'[   M2  ]',   '[  DEG  ]', '[   -   ]',   '[.01 * %]', &
              &   '[   M   ]', 2*'[   -   ]'/
    data elmtps/2*'INTEGER', 12*'REAL', 'INTEGER', 2*'REAL', 2*'CHARACTER'/
    data nbytsg/17*4,2*16/
    data (elmdes(i), i = 1, 10)                                                 &
         & /'Number of cells in ksi-direction                              ',    &
         & 'Number of cells in eta-direction                              ',     &
         & 'X-position of local grid origin        (DO NOT USE)           ',     &
         & 'Y-position of local grid origin        (DO NOT USE)           ',     &
         & 'Orientation of local grid w.r.t. east  (DO NOT USE)           ',     &
         & 'X-coord. bottom point in local system                         ',     &
         & 'Y-coord. bottom point in local system                         ',     &
         & 'GUU grid distance in eta-direction at u-point                 ',     &
         & 'GVV grid distance in ksi-direction at v-point                 ',     &
         & 'GUV grid distance in eta-direction at v-point                 '/
    data (elmdes(i), i = 11, 19)                                                &
         & /'GVU grid distance in ksi-direction at u-point                 ',    &
         & 'Cell area around water level point                            ',     &
         & 'Cell area around bottom point                                 ',     &
         & 'Orientation ksi-axis w.r.t. pos.x-axis at water level point   ',     &
         & 'Number of layers in sigma-direction                           ',     &
         & 'Relative layer thickness (fraction)                           ',     &
         & 'Vertical coordinates of cell interfaces                       ',     &
         & 'Cartesian or Spherical coordinates                            ',     &
         & 'Sigma-model or Z-model                                        '/
!
!! executable statements -------------------------------------------------------
!
    amiss     => gdp%gdconst%amiss
    nefiselem => gdp%nefisio%nefiselem(nefiswrgrid)
    first     => nefiselem%first
    celidt    => nefiselem%celidt
    elmdms    => nefiselem%elmdms
    !
    !
    ! Initialize local variables
    !
    error  = .false.
    errmsg = 'Unable to write parameter             to the COM-file'
    ierr   = 0
    wrswch = .true.
    if (sferic) then
       elmunt(6 ) = '[  DEG  ]'
       elmunt(7 ) = '[  DEG  ]'
    endif
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
       call filldm(elmdms    ,4         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,5         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,6         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,7         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,8         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,9         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,10        ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,11        ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,12        ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,13        ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,14        ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,15        ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,16        ,1         ,kmax      ,0         , &
                 & 0         ,0         ,0         )
       if (zmodel) then
          call filldm(elmdms    ,17        ,1         ,kmax + 1  ,0         , &
                    & 0         ,0         ,0         )
       endif
       call filldm(elmdms    ,18        ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,19        ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
    endif
    !
    ! Write all elements to file; all definition and creation of files,
    ! data groups, cells and elements is handled by PUTGTI/PUTGTR.
    !
    ! element  1 MMAX
    !
    idummy(1) = mmax
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'MMAX       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element  2 NMAX(US)
    !
    idummy(1) = nmaxus
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'NMAX       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element  3 XORI
    ! Not used anymore
    !
    rdummy(1) = amiss
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'XORI       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element  4 YORI
    ! Not used anymore
    !
    rdummy(1) = amiss
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(4) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'YORI       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 5 ALFORI
    ! Not used anymore
    !
    rdummy(1) = amiss
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(5) ,celidt    ,wrswch    ,ierr      ,rdummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'ALFORI     '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! For all array the values are buffered in RBUFF, to be sure that
    ! the correct array element are written to data file
    !
    ! element  6 XCOR
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = xcor(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(6) ,celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'XCOR       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element  7 YCOR
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = ycor(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(7) ,celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'YCOR       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element  8 GUU
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = guu(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(8) ,celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'GUU        '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element  9 GVV
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = gvv(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(9) ,celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'GVV        '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 10 GUV
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = guv(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(10),celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'GUV        '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 11 GVU
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = gvu(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(11),celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'GVU        '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 12 GSQS
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = gsqs(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(12),celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'GSQS       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 13 GSQD
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = gsqd(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(13),celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'GSQD       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 14 ALFAS
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m) = alfas(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(14),celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'ALFAS      '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 15 KMAX
    !
    idummy(1) = kmax
    call putgti(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(15),celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'KMAX       '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 16 THICK
    !
    call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(16),celidt    ,wrswch    ,ierr      ,thick     )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'THICK      '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    if (zmodel) then
       !
       ! element 17 ZK
       !
       do k = 1, kmax
          rbuffz(k + 1) = zk(k)
       enddo
       rbuffz(1) = zbot
       call putgtr(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(17),celidt    ,wrswch    ,ierr      ,rbuffz    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'ZK         '
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    endif
    !
    ! element 18 COORDINATES
    !
    if (sferic) then
       cdummy(1) = 'SPHERICAL'
    else
       cdummy(1) = 'CARTESIAN'
    endif
    call putgtc(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(18),celidt    ,wrswch    ,ierr      ,cdummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'COORDINATES'
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
    !
    ! element 19 ZMODEL
    !
    if (zmodel) then
       cdummy(1) = 'Z-MODEL'
    else
       cdummy(1) = 'SIGMA-MODEL'
    endif
    call putgtc(comfil    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(19),celidt    ,wrswch    ,ierr      ,cdummy    )
    if (ierr/=0) then
       write (errmsg(27:37), '(a)') 'LAYER_MODEL'
       call prterr(lundia    ,'U190'    ,errmsg    )
    endif
end subroutine wrgrid
