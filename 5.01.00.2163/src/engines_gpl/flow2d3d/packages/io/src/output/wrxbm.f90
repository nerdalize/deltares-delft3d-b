subroutine wrxbm(lundia     ,error    ,trifil    ,itmapc    ,nmax      , &
                & mmax      ,nmaxus   ,fxw       ,fyw       , &
                & wsu       ,wsv      ,guu       ,gvv       ,rbuff1    , &
                & hrms      ,gdp       )
!!--copyright-------------------------------------------------------------------
! Copyright (c) 2003, WL | Delft Hydraulics. All rights reserved.
!!--disclaimer------------------------------------------------------------------
! This code is part of the Delft3D software system. WL|Delft Hydraulics has
! developed c.q. manufactured this code to its best ability and according to the
! state of the art. Nevertheless, there is no express or implied warranty as to
! this software whether tangible or intangible. In particular, there is no
! express or implied warranty as to the fitness for a particular purpose of this
! software, whether tangible or intangible. The intellectual property rights
! related to this software code remain with WL|Delft Hydraulics at all times.
! For details on the licensing agreement, we refer to the Delft3D software
! license and any modifications to this license, if applicable. These documents
! are available upon request.
!!--version information---------------------------------------------------------
! $Author$
! $Date$
! $Revision$
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
    integer, parameter :: nelmx = 6
!
! Global variables
!
    integer                                                      , intent(in)  :: itmapc
    integer                                                                    :: lundia !  Description and declaration in inout.igs
    integer                                                                    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                      , intent(out) :: error
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: fxw    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: fyw    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
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
    data grnam1/'map-infrol-serie'/
    data grnam2/'map-rol-series'/
    data elmnms/'ITMAPS', 'HS',  &
              & 'FXW', 'FYW', 'WSU', 'WSV'/
    data elmqty/6*' '/
    data elmunt/'[   -   ]', '[  M    ]', '[ N/M2  ]', '[ N/M2  ]', '[ N/M2  ]', &
              & '[ N/M2  ]'/
    data elmtps/'INTEGER', 5*'REAL'/
    data nbytsg/6*4/
    data (elmdes(i), i = 1, 6)                                                 &
         & /'timestep number (ITMAPC*DT*TUNIT := time in sec from ITDATE)  ',   &
         &  'Significant wave height                                       ',   &
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
    nelmx2 = 5
    !
    filnam = trifil(1:3) // 'm' // trifil(5:)
    errmsg = ' '
    !
    wrswch = .false.
    if (first) then
       call getcel(filnam    ,grnam1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierror    )
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
    endif
    !
    ! Writing of output on every itmapc
    !
    celidt = celidt + 1
    !
    ! group 1: element 1 'ITMAPS'
    !
    wrswch = .true.
    idummy(1) = itmapc
    call putgti(filnam    ,grnam1    ,nelmx1    ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror    ,idummy    )
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
              & elmnms(2) ,celidt    ,wrswch    ,ierror    ,rbuff1    )
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
              & elmnms(3) ,celidt    ,wrswch    ,ierror    ,rbuff1    )
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
              & elmnms(4) ,celidt    ,wrswch    ,ierror    ,rbuff1    )
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
              & elmnms(5) ,celidt    ,wrswch    ,ierror    ,rbuff1    )
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
              & elmnms(6) ,celidt    ,wrswch    ,ierror    ,rbuff1    )
    if (ierror/=0) then
    endif
    !
 9999 continue
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrxbm
