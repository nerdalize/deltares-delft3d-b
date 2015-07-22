subroutine wrcurt(comfil    ,lundia    ,error     ,itcur     ,ntcur     , &
                & itimc     ,mmax      ,nmax      ,kmax      ,nmaxus    , &
                & lstsci    ,lsecfl    ,s1        ,u1        ,v1        , &
                & r1        ,qu        ,qv        ,dzu1      ,dzv1      , &
                & rbuff     ,kmaxz     ,hu        ,hv        ,thick     , &
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
!  $Id: wrcurt.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrcurt.f90 $
!!--description-----------------------------------------------------------------
! NONE
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
    logical                  , pointer :: only_distot_from_com
    logical                  , pointer :: zmodel
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    integer, dimension(:, :) , pointer :: elmdms
    type (nefiselement)      , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 10
!
! Global variables
!
    integer                                                                    , intent(in)  :: itcur  !!  Current time counter for the com-
                                                                                                       !!  munication file, where starting
                                                                                                       !!  point depend on CYCLIC
    integer                                                                    , intent(in)  :: itimc  !!  Current time step counter for 2D
                                                                                                       !!  system
    integer                                                                                  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: kmaxz  !!  = KMAX for Z-model, = 0 for sigma-model
                                                                                                       !!  Needed for correct dimensioning of DZU1 and DZV1
    integer                                                                    , intent(in)  :: lsecfl !  Description and declaration in dimens.igs
    integer                                                                    , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: lundia !  Description and declaration in inout.igs
    integer                                                                                  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: ntcur  !!  Total number of timesteps on com-
                                                                                                       !!  munication file (to write to)
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxz)                     :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxz)                     :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: qu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: qv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmaxus, mmax, kmax)                                                  :: rbuff  !  Description and declaration in r-i-ch.igs
    real(fp), dimension(kmax)                                                                :: thick  !  Description and declaration in esm_alloc_real.f90
    logical                                                                    , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                                                             :: comfil !!  Name for communication file
                                                                                                       !!  com-<case><label>
!
! Local variables
!
    integer                                    :: ierr   ! Flag for error when writing to Communication file 
    integer                                    :: k
    integer                                    :: m
    integer                                    :: n
    integer                                    :: nelmx1
    integer                                    :: nelmx2
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(256)                             :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grnam1 ! Data-group name defined for the COM-files (CURNT) 
    character(16)                              :: grnam2 ! Data-group name defined for the COM-files (CURTIM) 
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grnam1/'CURNT'/
    data grnam2/'CURTIM'/
    data elmnms/'NTCUR', 'TIMCUR', 'QU', 'QV', 'S1', 'U1', 'V1', 'RSP', 'DZU1', 'DZV1'/
    data elmqty/10*' '/
    data elmunt/'[   -   ]', '[ TSCALE]', '[ M3/S  ]', '[ M3/S  ]', '[   M   ]', &
              & '[  M/S  ]', '[  M/S  ]', '[  M/S  ]', '[   M   ]', '[   M   ]'/
    data elmtps/2*'INTEGER', 8*'REAL'/
    data nbytsg/10*4/
    data elmdes/'Number of current fields in groups CURTIM and KENMTIM         ', &
              & 'Time of current field rel.to reference date/time              ', &
              & 'Time-average over latest interval of discharge in u-point     ', &
              & 'Time-average over latest interval of discharge in v-point     ', &
              & 'Water level in zeta point at end of time interval             ', &
              & 'Velocity in u-point at end of time interval                   ', &
              & 'Velocity in v-point at end of time interval                   ', &
              & 'Spiral flow intensity                                         ', &
              & 'Layer thickness in u-point at end of time interval            ', &
              & 'Layer thickness in v-point at end of time interval            '/
!
!! executable statements -------------------------------------------------------
!
    only_distot_from_com => gdp%gdprocs%only_distot_from_com
    zmodel               => gdp%gdprocs%zmodel
    nefiselem            => gdp%nefisio%nefiselem(nefiswrcurt)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    ! Initialize local variables
    !
    ierr   = 0
    wrswch = .true.
    nelmx1 = 1
    nelmx2 = nelmx - nelmx1
    !
    ! Set up the element dimensions
    ! different element dimensions for 2d and 3d applications
    ! if kmax =1 (2d) then only 2 dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms, 1, 1, 1, 0, 0, 0, 0)
       call filldm(elmdms, 2, 1, 1, 0, 0, 0, 0)
       if (kmax>1) then
          call filldm(elmdms, 3, 3, nmaxus, mmax, kmax, 0, 0)
          call filldm(elmdms, 4, 3, nmaxus, mmax, kmax, 0, 0)
       else
          call filldm(elmdms, 3, 2, nmaxus, mmax, 0   , 0, 0)
          call filldm(elmdms, 4, 2, nmaxus, mmax, 0   , 0, 0)
       endif
       call filldm(elmdms, 5, 2, nmaxus, mmax, 0, 0, 0)
       if (kmax>1) then
          call filldm(elmdms, 6, 3, nmaxus, mmax, kmax, 0, 0)
          call filldm(elmdms, 7, 3, nmaxus, mmax, kmax, 0, 0)
       else
          call filldm(elmdms, 6, 2, nmaxus, mmax, 0   , 0, 0)
          call filldm(elmdms, 7, 2, nmaxus, mmax, 0   , 0, 0)
       endif
       call filldm(elmdms,  8, 2, nmaxus, mmax, 0, 0, 0)
       if (kmax>1) then
          call filldm(elmdms, 9 , 3, nmaxus, mmax, kmax, 0, 0)
          call filldm(elmdms, 10, 3, nmaxus, mmax, kmax, 0, 0)
       else
          call filldm(elmdms, 9 , 2, nmaxus, mmax, 0   , 0, 0)
          call filldm(elmdms, 10, 2, nmaxus, mmax, 0   , 0, 0)
       endif

    endif
    !
    ! Write all elements to file; all definition and creation of files,
    ! data groups, cells and elements is handled by PUTGET.
    !
    ! element  1 NTCUR for group CURNT
    !
    celidt = 1
    idummy(1) = ntcur
    call putgti(comfil    ,grnam1    ,nelmx1    ,elmnms(1) ,elmdms(1, 1) , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1)    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy       )
    if (ierr/=0) goto 9999
    !
    ! element  1 ITIMC for group CURTIM (cel number ITCUR)
    !
    celidt    = itcur
    idummy(1) = itimc
    call putgti(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierr      ,idummy       )
    if (ierr/=0) goto 9999
    !
    ! element  2 QU  for group CURTIM (cel number ITCUR)
    !
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff(n, m, k) = qu(n, m, k)
          enddo
       enddo
    enddo
    call putgtr(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierr      ,rbuff        )
    if (ierr/=0) goto 9999
    !
    ! element  3 QV  for group CURTIM (cel number ITCUR)
    !
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff(n, m, k) = qv(n, m, k)
          enddo
       enddo
    enddo
    call putgtr(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
              & elmnms(4) ,celidt    ,wrswch    ,ierr      ,rbuff        )
    if (ierr/=0) goto 9999
    !
    ! element  4 S1  for group CURTIM (cel number ITCUR)
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m, 1) = s1(n, m)
       enddo
    enddo
    call putgtr(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
              & elmnms(5) ,celidt    ,wrswch    ,ierr      ,rbuff        )
    if (ierr/=0) goto 9999
    !
    ! element  5 U1  for group CURTIM (cel number ITCUR)
    !
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff(n, m, k) = u1(n, m, k)
          enddo
       enddo
    enddo
    call putgtr(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
              & elmnms(6) ,celidt    ,wrswch    ,ierr      ,rbuff        )
    if (ierr/=0) goto 9999
    !
    ! element  6 V1  for group CURTIM (cel number ITCUR)
    !
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff(n, m, k) = v1(n, m, k)
          enddo
       enddo
    enddo
    call putgtr(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
              & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
              & elmnms(7) ,celidt    ,wrswch    ,ierr      ,rbuff        )
    if (ierr/=0) goto 9999
    !
    ! element  7 RSP for group CURTIM (cel number ITCUR)
    ! if secondary flow is defined then r1(n,m,1,lsecfl) else 0.
    !
    do m = 1, mmax
       do n = 1, nmaxus
          if (lsecfl/=0) then
             rbuff(n, m, 1) = r1(n, m, 1, lsecfl)
          else
             rbuff(n, m, 1) = 0.0
          endif
       enddo
    enddo
    call putgtr(comfil     ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
              & elmqty(2)  ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
              & elmnms(8)  ,celidt    ,wrswch    ,ierr      ,rbuff        )
    if (ierr/=0) then
    endif
    if (.not. only_distot_from_com) then
       !
       ! element  8 DZU1  for group CURTIM (cel number ITCUR)
       !
       rbuff = 0.0_fp
       if (zmodel) then
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff(n, m, k) = dzu1(n, m, k)
                enddo
             enddo
          enddo
       else
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff(n, m, k) = hu(n, m)*thick(k)
                enddo
             enddo
          enddo
       endif
       call putgtr(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
                 & elmnms(9) ,celidt    ,wrswch    ,ierr      ,rbuff        )
       if (ierr/=0) goto 9999
       !
       ! element  9 DZV1  for group CURTIM (cel number ITCUR)
       !
       rbuff = 0.0_fp
       if (zmodel) then
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff(n, m, k) = dzv1(n, m, k)
                enddo
             enddo
          enddo
       else
          do k = 1, kmax
             do m = 1, mmax
                do n = 1, nmaxus
                   rbuff(n, m, k) = hv(n, m)*thick(k)
                enddo
             enddo
          enddo
       endif
       call putgtr(comfil     ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
                 & elmqty(2)  ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2)    , &
                 & elmnms(10) ,celidt    ,wrswch    ,ierr      ,rbuff        )
       if (ierr/=0) goto 9999
    endif
    !
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrcurt
