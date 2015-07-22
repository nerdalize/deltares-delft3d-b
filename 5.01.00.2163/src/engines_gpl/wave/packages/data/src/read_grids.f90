module read_grids
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
!  $Id: read_grids.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/data/src/read_grids.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !
    use precision_basics


contains
!
!
!==============================================================================
subroutine get_gri(filnam    ,xz        ,yz        ,guu       ,gvv       , &
                &  alfas     ,kcs       ,covered   ,mmax      ,nmax      , &
                &  kmax      ,xymiss    ,layer_model)
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx  = 17
    integer, parameter :: nelmx2 =  4
    integer, parameter :: nelmx3 =  3
!
! Global variables
!
    integer                         , intent(out) :: mmax
    integer                         , intent(out) :: nmax
    integer                         , intent(out) :: kmax
    integer, dimension(:,:), pointer              :: kcs
    integer, dimension(:,:), pointer              :: covered !  0: target point is not covered by source grid
                                                             !  1: target point is covered by   valid points of source grid
                                                             ! -1: target point is covered by invalid points of source grid
    real                            , intent(out) :: xymiss
    real   , dimension(:,:), pointer              :: alfas
    real   , dimension(:,:), pointer              :: guu
    real   , dimension(:,:), pointer              :: gvv
    real(kind=hp)   , dimension(:,:), pointer     :: xz
    real(kind=hp)   , dimension(:,:), pointer     :: yz
    character(*)                    , intent(in)  :: filnam
    character(*)                    , intent(out) :: layer_model
!
! Local variables
!
    integer, dimension(:,:), allocatable                 :: ibuff
    real,    dimension(:,:), allocatable                 :: rbuff

    integer                         :: celidt
    integer                         :: error
    integer                         :: ielem
    integer                         :: ierr
    integer                         :: m
    integer                         :: n
    integer, dimension(1)           :: ival
    integer, dimension(6, nelmx)    :: elmdms
    integer, dimension(nelmx)       :: nbytsg
    logical                         :: wrswch
    character(16), dimension(1)     :: cval
    character(10), dimension(nelmx) :: elmunt
    character(16)                   :: grpnam
    character(16), dimension(nelmx) :: elmnms
    character(16), dimension(nelmx) :: elmqty
    character(16), dimension(nelmx) :: elmtps
    character(64), dimension(nelmx) :: elmdes

    integer, dimension(6, nelmx2)   :: elmdm2
    integer, dimension(nelmx2)      :: nbyts2
    character(10), dimension(nelmx2):: elmun2
    character(16)                   :: grpna2
    character(16), dimension(nelmx2):: elmnm2
    character(16), dimension(nelmx2):: elmqt2
    character(16), dimension(nelmx2):: elmtp2
    character(64), dimension(nelmx2):: elmde2

    integer, dimension(6, nelmx3)   :: elmdm3
    integer, dimension(nelmx3)      :: nbyts3
    character(10), dimension(nelmx3):: elmun3
    character(16)                   :: grpna3
    character(16), dimension(nelmx3):: elmnm3
    character(16), dimension(nelmx3):: elmqt3
    character(16), dimension(nelmx3):: elmtp3
    character(64), dimension(nelmx3):: elmde3
!
    !     Define data structure; element dimensions are required only
    !     in write-mode.
    !
    data grpnam/'GRID'/
    data elmnms/'MMAX', 'NMAX', 'XORI', 'YORI', 'ALFORI', 'XCOR', &
              & 'YCOR', 'GUU',  'GVV',  'GUV',  'GVU',    'GSQS', 'GSQD', &
              & 'ALFAS','KMAX', 'THICK', 'LAYER_MODEL'/
    data elmtps/2*'INTEGER', 12*'REAL','INTEGER','REAL', 'CHARACTER'/
    data nbytsg/16*4,16/
    data grpna2/'TEMPOUT'/
    data elmnm2/'XWAT','YWAT','CODB','CODW'/
    data elmtp2/2*'REAL',2*'INTEGER'/
    data nbyts2/nelmx2*4/
    data grpna3/'KENMCNST'/
    data elmnm3/'KCU', 'KCV', 'KCS'/
    data elmqt3/3*' '/
    data elmun3/3*'[   -   ]'/
    data elmtp3/3*'INTEGER'/
    data nbyts3/3*4/
!
!! executable statements -------------------------------------------------------
!
    ! missing value is currently not available in the com-file
    ! default is zero
    !
    xymiss = 0.0
    !
    celidt = 1
    call filldm(elmdms    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,15        ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,17        ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    
    wrswch = .false.
    ielem  = 1     ! MMAX
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival(1)   )
    mmax   = ival(1)

    ielem  = 2     ! NMAX
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival(1)   )
    nmax   = ival(1)

    ielem  = 15    ! KMAX
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival(1)   )
    kmax   = ival(1)

    ielem = 17     ! LAYER_MODEL
    call putgtc(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,cval      )
    layer_model = cval(1)

    call filldm(elmdms    ,3         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,5         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,6         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,7         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,8         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,9         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,10        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,11        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,12        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,13        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,14        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,16        ,1         ,kmax      ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,1         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,2         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,3         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,4         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm3    ,1         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm3    ,2         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm3    ,3         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    !
    !
    ! Allocate arrays
    allocate (rbuff (nmax,mmax))    ! Note com-file dimensions are nmax,mmax
    allocate (ibuff (nmax,mmax))    ! Note com-file dimensions are nmax,mmax
    allocate (guu   (mmax,nmax))
    allocate (gvv   (mmax,nmax))
    allocate (alfas (mmax,nmax))
    allocate (xz    (mmax,nmax))
    allocate (yz    (mmax,nmax))
    allocate (kcs   (mmax,nmax))
    allocate (covered(mmax,nmax))
    covered = 0

    ielem = 8      ! GUU
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          guu(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 9      ! GVV
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          gvv(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 14     ! ALFAS
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          alfas(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 1     ! XZ

    call putgtr(filnam    ,grpna2    ,nelmx2    ,elmnm2    ,elmdm2    , &
              & elmqt2    ,elmun2    ,elmde2    ,elmtp2    ,nbyts2    , &
              & elmnm2(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          xz(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 2     ! yZ
    call putgtr(filnam    ,grpna2    ,nelmx2    ,elmnm2    ,elmdm2    , &
              & elmqt2    ,elmun2    ,elmde2    ,elmtp2    ,nbyts2    , &
              & elmnm2(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          yz(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 3     ! KCS
    call putgti(filnam    ,grpna3    ,nelmx3    ,elmnm3    ,elmdm3    , &
              & elmqt3    ,elmun3    ,elmde3    ,elmtp3    ,nbyts3    , &
              & elmnm3(ielem)        ,celidt    ,wrswch    ,error     ,ibuff     )
    do m = 1, mmax
       do n = 1, nmax
          kcs(m, n) = ibuff(n, m)
       enddo
    enddo

    deallocate (ibuff, stat=ierr)
    deallocate (rbuff, stat=ierr)

end subroutine get_gri
!
!
!==============================================================================
subroutine readgriddims(filnam, mmax, nmax)
    implicit none
!
! Global variables
!
    character(*), intent(in)  :: filnam
    integer     , intent(out) :: mmax
    integer     , intent(out) :: nmax
!
! Local variables
!
    integer           :: irgf
    integer, external :: new_lun
    character(256)    :: rec
!
!! executable statements -------------------------------------------------------
!
    irgf   = new_lun()
    open (irgf, file = filnam, form = 'formatted', status = 'old')
    read (irgf, '(a)', end = 7777, err = 8888) rec
10  continue
       read(irgf,'(a)',end = 7777,err=8888) rec
       if (rec(1:1) == '*' &
           & .or. index(rec,'Coordinate System') >= 1 &
           & .or. index(rec,'Missing Value')     >= 1) goto 10
    read(rec,*,err=8888)  mmax, nmax
    goto 9999
 7777 continue
 8888 continue
    write(*,'(2a)') '*** ERROR: Unable to read dimensions in file ',trim(filnam)
    close(irgf)
    stop
 9999 continue
    close(irgf)
end subroutine readgriddims
!
!
!==============================================================================
subroutine read_grd(filnam    ,xb     ,yb   ,codb ,covered, mmax  ,nmax ,sferic ,xymiss)
    implicit none
!
! Global variables
!
    character(232)                   , intent(in)  :: filnam
    integer                          , intent(out) :: mmax
    integer                          , intent(out) :: nmax
    real                             , intent(out) :: xymiss
    integer , dimension(:,:), pointer              :: codb
    integer , dimension(:,:), pointer              :: covered
    real(hp), dimension(:,:), pointer              :: xb
    real(hp), dimension(:,:), pointer              :: yb
    logical                                        :: sferic
!
!
! Local variables
!
    real(hp), dimension(:,:,:),allocatable :: xy
    integer                                :: etamax
    integer                                :: i
    integer                                :: ierr
    integer                                :: irgf
    integer                                :: j
    integer                                :: k
    integer                                :: ksimax
    integer                                :: npareg
    integer, external                      :: new_lun
    integer                                :: pos
    logical                                :: kw_found
    character(10)                          :: dum
    character(256)                         :: rec
!
!! executable statements -------------------------------------------------------
!
    ! Default value for missing value: zero
    !
    xymiss = 0.0
    sferic = .false.
    irgf   = new_lun()
    open (irgf, file = filnam, form = 'formatted', status = 'old')
    !
    ! Copied from file rdrgf
    !
    ! Read file, check for end of file or error in file:
    ! - The first line always contains comments
    !   sferic is true when the first line contains the keyword Spherical
    ! - Skip comment lines (starting with a '*'), while trying to read the
    !   following keywords: 'Coordinate System'
    !                       'Missing Value'
    !   If 'Coordinate System' is present, it overrules the sferic-specification
    !   in the first line!
    ! - The next line contains the dimensions mc and nc
    !   Parameter npart may also be on this line, but it is neglected
    ! - Read the next line containing three zero's
    !   xori, yori and alfori are not used anymore
    ! - Read x coordinates
    ! - Read y coordinates
    !
    read (irgf, '(a)', end = 7777, err = 8888) rec
    if (index(rec, 'Spherical')>=1 .or. index(rec, 'SPHERICAL')>=1) then
       sferic = .true.
    endif
10  continue
       kw_found = .false.
       read(irgf,'(a)',end = 7777,err=8888) rec
       if (rec(1:1) == '*') goto 10
       !
       pos = index(rec,'Coordinate System')
       if (pos >= 1) then
          kw_found = .true.
          if (index(rec(pos+1:),'spherical') >= 1 .or. &
            & index(rec(pos+1:),'Spherical') >= 1 .or. &
            & index(rec(pos+1:),'SPHERICAL') >= 1       ) then
             sferic = .true.
          else
             sferic = .false.
          endif
       endif
       !
       pos = index(rec,'Missing Value')
       if (pos >= 1) then
          kw_found = .true.
          pos      = index(rec,'=') + 1
          read(rec(pos:),*,err=8888) xymiss
       endif
    if (kw_found) goto 10
    !
    if (sferic) then
       write (*, *)
       write (*, '(a)') 'Coordinate System: Spherical'
       write (*, *)
    endif
    read(rec,*,err=8888)  ksimax, etamax

    mmax = ksimax
    nmax = etamax

    allocate (xy    (2,mmax,nmax))
    allocate (xb      (mmax,nmax))
    allocate (yb      (mmax,nmax))
    allocate (codb    (mmax,nmax))
    allocate (covered (mmax,nmax))

    xy      = 0.0_hp
    xb      = 0.0_hp
    yb      = 0.0_hp
    codb    = 0
    covered = 0
    !
    ! read three zero's
    !
    read(irgf,'(a)',end = 7777,err=8888) rec
    !
    ! read X and Y coordinates
    ! read unformatted: The number of digits of xcor may vary
    !
    do k = 1, 2
       do j = 1, etamax
          read (irgf, *, end = 7777, err = 8888) dum,dum,(xy(k, i, j),i=1, ksimax)
       enddo
    enddo
    close (irgf)
    goto 9999
    !
    ! test for reading error : label 7777 end of file
    !                                8888 error while reading
    !
 7777 continue
 8888 continue
    goto 999
    !
 9999 continue
    !
    npareg = 0
    !
    if (npareg<=1) then
       !
       ! grid consists of one rectangle
       !
       do j = 1, etamax
          do i = 1, ksimax
             codb(i, j) = +1
          enddo
       enddo
    else
       !
       ! grid consists of more than one rectangle
       !
    endif
    !
    !     correction of code in bottom points
    !
    do i = 1, ksimax
       do j = 1, etamax
          if (abs(xy(1, i, j))<1.0e-6_hp .and. abs(xy(2, i, j))<1.0e-6_hp) then
             codb(i,j) = 0
          endif
       enddo
    enddo
    !
    !
    ! x- and y-coordinates in bottom points
    !
    do i = 1, ksimax
       do j = 1, etamax
          xb(i, j) = xy(1, i, j)
          yb(i, j) = xy(2, i, j)
       enddo
    enddo
    goto 1000
  999 continue
   write(*,'(2a)') '*** ERROR: reading GRD file', trim(filnam)
   stop
 1000 continue
    deallocate (xy, stat=ierr)
end subroutine read_grd
!
!
!==============================================================================
subroutine readregulargrid(filnam, sferic_exp, xorigin, yorigin, alpha, &
                          & mmax, nmax, dx, dy)
    implicit none
!
! Global variables
!
    character(256), intent(in)                        :: filnam
    logical, intent(in)                               :: sferic_exp
    integer, intent(out)                              :: mmax
    integer, intent(out)                              :: nmax
    real   , intent(out)                              :: xorigin
    real   , intent(out)                              :: yorigin
    real   , intent(out)                              :: alpha
    real   , intent(out)                              :: dx
    real   , intent(out)                              :: dy
!
! Local variables
!
    integer                        :: i
    integer                        :: ierr
    integer                        :: irgf
    integer                        :: j
    integer                        :: k
    integer, external              :: new_lun
    integer                        :: pos
    real                           :: dxx
    real                           :: dxy
    real                           :: dyx
    real                           :: dyy
    real                           :: pi
    real                           :: x1
    real                           :: x2
    real                           :: y1
    real                           :: y2
    real, dimension(:,:,:),allocatable :: xy
    logical                        :: kw_found
    logical                        :: sferic_read
    character(10)                  :: dum
    character(256)                 :: rec
!
!! executable statements -------------------------------------------------------
!
    sferic_read = .false.
    irgf   = new_lun()
    open (irgf, file = filnam, form = 'formatted', status = 'old')
    !
    ! Copied from file rdrgf
    !
    ! Read file, check for end of file or error in file:
    ! - The first line always contains comments
    !   sferic is true when the first line contains the keyword Spherical
    ! - Skip comment lines (starting with a '*'), while trying to read the
    !   following keywords: 'Coordinate System'
    !                       'Missing Value'
    !   If 'Coordinate System' is present, it overrules the sferic-specification
    !   in the first line!
    ! - The next line contains the dimensions mc and nc
    !   Parameter npart may also be on this line, but it is neglected
    ! - Read the next line containing three zero's
    !   xori, yori and alfori are not used anymore
    ! - Read x coordinates
    ! - Read y coordinates
    !
    read (irgf, '(a)', end = 7777, err = 8888) rec
    if (index(rec, 'Spherical')>=1 .or. index(rec, 'SPHERICAL')>=1) then
       sferic_read = .true.
    endif
10  continue
       kw_found = .false.
       read(irgf,'(a)',end = 7777,err=8888) rec
       if (rec(1:1) == '*') goto 10
       !
       pos = index(rec,'Coordinate System')
       if (pos >= 1) then
          kw_found = .true.
          if (index(rec(pos+1:),'spherical') >= 1 .or. &
            & index(rec(pos+1:),'Spherical') >= 1 .or. &
            & index(rec(pos+1:),'SPHERICAL') >= 1       ) then
             sferic_read = .true.
          else
             sferic_read = .false.
          endif
       endif
       !
       pos = index(rec,'Missing Value')
       if (pos >= 1) then
          kw_found = .true.
       endif
    if (kw_found) goto 10
    !
    if (      sferic_read .and. .not. sferic_exp) then
       write (*, '(3a)') '*** ERROR: file ',trim(filnam),' contains Spherical coordinates'
       write (*, '(11x,3a)') 'Expecting Cartesian coordinates'
       close (irgf)
       stop
    endif
    if (.not. sferic_read .and.       sferic_exp) then
       write (*, '(3a)') '*** ERROR: file ',trim(filnam),' contains Cartesian coordinates'
       write (*, '(11x,3a)') 'Expecting Spherical coordinates'
       close (irgf)
       stop
    endif
    read(rec,*,err=8888)  mmax, nmax
    !
    ! poles? no, fences!
    !
    allocate (xy (2,mmax,nmax))
    xy   = 0.
    !
    ! read three zero's
    !
    read(irgf,'(a)',end = 7777,err=8888) rec
    !
    ! read X and Y coordinates
    ! read unformatted: The number of digits of xcor may vary
    !
    do k = 1, 2
       do j = 1, nmax
          read (irgf, *, end = 7777, err = 8888) dum,dum,(xy(k, i, j),i=1, mmax)
       enddo
    enddo
    close (irgf)
    goto 9999
    !
    ! test for reading error : label 7777 end of file
    !                                8888 error while reading
    !
 7777 continue
 8888 continue
    goto 999
    !
 9999 continue
    !
    pi   = 4.*atan(1.)
    alpha = atan2(xy(2,mmax,1)-xy(2,1,1),xy(1,mmax,1)-xy(1,1,1))*180./pi
    xorigin   = xy(1,1,1)
    yorigin   = xy(2,1,1)
    x1   = xy(1,mmax,1)
    y1   = xy(2,mmax,1)
    x2   = xy(1,mmax,nmax)
    y2   = xy(2,mmax,nmax)
    dxx  = (x1 - xorigin)/(mmax - 1)
    dxy  = (x2 - x1)/(nmax - 1)
    dyy  = (y2 - y1)/(nmax - 1)
    dyx  = (y1 - yorigin)/(mmax - 1)
    dx   = sqrt(dxx * dxx + dyx * dyx)
    dy   = sqrt(dyy * dyy + dxy * dxy)
    !
    goto 1000
  999 continue
    write(*,'(2a)') '*** ERROR: reading GRD file', trim(filnam)
    deallocate (xy, stat=ierr)
    stop
 1000 continue
    deallocate (xy, stat=ierr)
end subroutine readregulargrid
!
!
!==============================================================================
subroutine write_swan_grid (x,y,mmax,nmax,inest,fname)
   implicit none
   !
   integer                       , intent(in)  :: mmax
   integer                       , intent(in)  :: nmax
   real(hp), dimension(mmax,nmax), intent(in)  :: x
   real(hp), dimension(mmax,nmax), intent(in)  :: y
   character(37)                               :: fname
   integer                       , intent(in)  :: inest
   !
   integer                        :: m
   integer                        :: n
   integer, external              :: new_lun
   integer                        :: ugrd
   !
   fname       = ' '
   fname(1:13) = 'TMP_grid2swan'
   ugrd        = new_lun()
   write (fname(14:15),'(I2.2)') inest
   open  ( unit = ugrd, file = fname(1:15), form ='formatted')
   write (ugrd,'(A)') 'x-coordinates'
   do n = 1,nmax
      write (ugrd,'(6(E25.17,1X))') (x(m,n),m=1,mmax)
   enddo
   write (ugrd,'(A)') 'y-coordinates'
   do n = 1,nmax
      write (ugrd,'(6(E25.17,1X))') (y(m,n),m=1,mmax)
   enddo
   close (ugrd)
end subroutine write_swan_grid



end module read_grids
