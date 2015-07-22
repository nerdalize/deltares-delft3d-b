subroutine wrrolm(lundia    ,error     ,trifil    ,itmapc    ,nmax      , &
                & mmax      ,nmaxus    ,ewave1    ,eroll1    ,qxkr      , &
                & qykr      ,qxkw      ,qykw      ,fxw       ,fyw       , &
                & wsu       ,wsv       ,guu       ,gvv       ,rbuff1    , &
                & hrms      ,gdp       )
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
!  $Id: wrrolm.f90 1677 2012-07-01 21:04:29Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrrolm.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the time varying data for roller effect
! to the NEFIS MAP-DAT file
! Output is performed conform the times of the map
! file and only in case roller = true
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
    integer, parameter :: nelmx = 12
!
! Global variables
!
    integer                                                      , intent(in)  :: itmapc
    integer                                                                    :: lundia !  Description and declaration in inout.igs
    integer                                                                    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                      , intent(out) :: error
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: eroll1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ewave1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: fxw    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: fyw    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: qxkr   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: qxkw   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: qykr   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: qykw   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: wsu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: wsv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmaxus, mmax)                                          :: rbuff1
    character(60)                                                , intent(in)  :: trifil
!
! Local variables
!
    integer                         :: i
    integer                         :: ierror
    integer                         :: m
    integer                         :: n
    integer                         :: nelmx1
    integer                         :: nelmx2
    integer      , dimension(1)     :: idummy
    integer      , dimension(nelmx) :: nbytsg
    integer, external               :: neferr
    logical                         :: wrswch
    character(10), dimension(nelmx) :: elmunt
    character(16)                   :: grnam1
    character(16)                   :: grnam2
    character(16), dimension(nelmx) :: elmnms
    character(16), dimension(nelmx) :: elmqty
    character(16), dimension(nelmx) :: elmtps
    character(256)                  :: errmsg
    character(60)                   :: filnam
    character(64), dimension(nelmx) :: elmdes
!
! Data statements
!
    data grnam1/'map-infrol-serie'/
    data grnam2/'map-rol-series'/
    data elmnms/'ITMAPS', 'HS', 'EWAVE1', 'EROLL1', 'QXKR', 'QYKR', 'QXKW', 'QYKW', &
              & 'FXW', 'FYW', 'WSU', 'WSV'/
    data elmqty/12*' '/
    data elmunt/'[   -   ]', '[  M    ]', '[ J/M2  ]', '[ J/M2  ]', '[  M/S  ]', '[  M/S  ]', &
              & '[  M/S  ]', '[  M/S  ]', '[ N/M2  ]', '[ N/M2  ]', '[ N/M2  ]', &
              & '[ N/M2  ]'/
    data elmtps/'INTEGER', 11*'REAL'/
    data nbytsg/12*4/
    data (elmdes(i), i = 1, 12)                                                 &
         & /'timestep number (ITMAPC*DT*TUNIT := time in sec from ITDATE)  ',   &
         &  'Significant wave height                                       ',   &
         &  'Short-wave energy                                             ',   &
         &  'Roller energy                                                 ',   &
         &  'Transport velocity of roller energy in ksi direction          ',   &
         &  'Transport velocity of roller energy in eta direction          ',   &
         &  'Transport velocity of wave energy in ksi direction            ',   &
         &  'Transport velocity of wave energy in eta direction            ',   &
         &  'Component of wave force in ksi direction                      ',   &
         &  'Component of wave force in eta direction                      ',   &
         &  'Component of roller force in ksi direction                    ',   &
         &  'Component of roller force in eta direction                    '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrrolm)
    first     => nefiselem%first
    celidt    => nefiselem%celidt
    elmdms    => nefiselem%elmdms
    !
    ! Initialize local variables
    ! Default is append mode: celidt = last cell + 1
    !
    ierror = 0
    nelmx1 = 1
    nelmx2 = 11
    !
    filnam = trifil(1:3) // 'm' // trifil(5:)
    errmsg = ' '
    !
    wrswch = .false.
    if (first) then
       call getcel(filnam    ,grnam1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierror   )
       !
       ! Set up the element dimensions
       !
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,5         ,2         ,nmaxus    ,mmax      , &
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
    endif
    !
    ! Writing of output on every itmapc
    !
    celidt = celidt + 1
    !
    wrswch = .true.
    idummy(1) = itmapc
    call putgti(filnam    ,grnam1    ,nelmx1    ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/=0) goto 9999    
    !
    ! group 2: element 'HS'.  hs = sqrt(2)*hrms
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff1(n, m) = sqrt(2.0_fp)*hrms(n,m)
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(2) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'EWAVE1'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff1(n, m) = ewave1(n, m)
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(3) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'EROLL1'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff1(n, m) = eroll1(n, m)
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(4) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'QXKR'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (abs(guu(n, m))>0.) then
             rbuff1(n, m) = qxkr(n, m)/guu(n, m)
          endif
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(5) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'QYKR'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (abs(gvv(n, m))>0.) then
             rbuff1(n, m) = qykr(n, m)/gvv(n, m)
          endif
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(6) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'QXKW'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (abs(guu(n, m))>0.) then
             rbuff1(n, m) = qxkw(n, m)/guu(n, m)
          endif
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(7) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'QYKW'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (abs(gvv(n, m))>0.) then
             rbuff1(n, m) = qykw(n, m)/gvv(n, m)
          endif
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(8) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'FXW'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff1(n, m) = fxw(n, m)
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(9) ,celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'FYW'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff1(n, m) = fyw(n, m)
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(10),celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'WSU'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff1(n, m) = wsu(n, m)
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(11),celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) goto 9999
    !
    ! group 2: element 'WSV'
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff1(n, m) = wsv(n, m)
       enddo
    enddo
    call putgtr(filnam    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
              & elmnms(12),celidt    ,wrswch    ,ierror   ,rbuff1    )
    if (ierror/=0) then
    endif
    !
 9999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrrolm
