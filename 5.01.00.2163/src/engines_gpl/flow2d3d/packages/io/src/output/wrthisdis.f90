subroutine wrthisdis(lundia    ,error     ,trifil    ,ithisc    ,zmodel    , &
                   & kmax      ,lstsc     ,nsrc      ,mnksrc    ,disch     , &
                   & dps       ,rint      ,s1        ,sig       ,zk        , &
                   & voldis    ,xcor      ,ycor      ,sferic    ,gdp       )
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
!  $Id: wrthisdis.f90 1677 2012-07-01 21:04:29Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrthisdis.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the time varying Discharge group to the NEFIS HIS-DAT file
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
    real(fp)      , dimension(:)   , pointer :: poscul
    character(24)                  , pointer :: date_time
    logical                        , pointer :: first
    integer                        , pointer :: celidt
    integer, dimension(:, :)       , pointer :: elmdms
    type (nefiselement)            , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 17
!
! Global variables
!
    integer                                                        , intent(in)  :: ithisc ! Description and declaration in tricom.f90
    integer                                                        , intent(in)  :: kmax
    integer                                                        , intent(in)  :: lstsc  ! Description and declaration in dimens.igs
    integer                                                        , intent(in)  :: lundia ! Description and declaration in inout.igs
    integer                                                        , intent(in)  :: nsrc   ! Description and declaration in dimens.igs
    integer   , dimension(7, nsrc)                                               :: mnksrc ! Description and declaration in r-i-ch.igs
    real(fp)  , dimension(nsrc)                                    , intent(in)  :: disch  ! Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dps    ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(lstsc, nsrc)                             , intent(in)  :: rint   ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: s1     ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                    , intent(in)  :: sig    ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nsrc)                                    , intent(in)  :: voldis ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xcor   ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ycor   ! Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(0:kmax)                                  , intent(in)  :: zk     ! See description and declaration of sig in esm_alloc_real.f90
    logical                                                        , intent(out) :: error  ! Description and declaration in tricom.f90
    logical                                                        , intent(in)  :: zmodel ! Description and declaration in procs.igs
    logical                                                        , intent(in)  :: sferic !  Description and declaration in tricom.igs
    character(*)                                                   , intent(in)  :: trifil ! Description and declaration in trisim.F90
    
!
! Local variables
!
    integer                                           :: i
    integer       , dimension(1)                      :: idummy         ! Help array to read/write Nefis files
    integer                                           :: ierror         ! Local error flag for NEFIS files
    integer                                           :: isrc           ! Index number of discharge location 
    integer                                           :: k
    integer                                           :: m
    integer                                           :: n    
    integer       , dimension(nelmx)                  :: nbytsg         ! Array containing the number of bytes of each single ELMTPS
    integer                                , external :: neferr
    integer                                           :: nelmx2
    real(fp)                                          :: h0             ! Actual Water-height (DP+S1)     
    real(fp)      , dimension(nsrc)                   :: ndummy         ! Help array to read/write Nefis files
    real(fp)      , dimension(nsrc, lstsc)            :: rinttransposed ! transposed version of rint
    real(fp)                                          :: zdown
    real(fp)                                          :: zup
    logical                                           :: wrswch         ! tes of each single ELMTPS Flag to write file    
    character(24) , dimension(1)                      :: datetimearr    ! putgtc expects an array
    character(64) , dimension(nelmx)                  :: elmdes
    character(16) , dimension(nelmx)                  :: elmnms
    character(16) , dimension(nelmx)                  :: elmqty
    character(16) , dimension(nelmx)                  :: elmtps
    character(10) , dimension(nelmx)                  :: elmunt
    character(256)                                    :: errmsg
    character(256)                                    :: filnam         ! Help var. for file name    
    character(16)                                     :: grnam          ! Data-group name defined for the NEFIS-files group 1 
!
! Data statements
!
    data grnam/'his-dis-series'/
    data elmnms/'ITHISC', 'DATE_TIME', 'RINT', 'ZQ', 'ZQ_SUM',        &
              & 'MCOR1', 'NCOR1', 'KCOR1', 'XCOR1', 'YCOR1', 'ZCOR1', &
              & 'MCOR2', 'NCOR2', 'KCOR2', 'XCOR2', 'YCOR2', 'ZCOR2'/              
    data elmqty/17*' '/
    data elmunt/3*'[   -   ]', '[  M3/S ]', '[   M3  ]', 3*'[   -   ]', &
              & 3*'[   M   ]', 3*'[   -   ]', 3*'[   M   ]'/
    data elmtps/'INTEGER', 'CHARACTER', 3*'REAL', 3*'INTEGER', 3*'REAL', &
              & 3*'INTEGER', 3*'REAL'/
    data nbytsg/4,24,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4/
    data (elmdes(i), i = 1, 17)                                               &
         & /'timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)  ', &
         &  'Current simulation date and time [YYYY-MM-DD HH:MM:SS.FFFF]   ', &
         &  'Concentrations of the discharge                               ', &
         &  'Momentary discharge                                           ', &
         &  'Cummulative volume of the discharge                           ', &
         &  'first M coordinate of discharge                               ', &
         &  'first N coordinate of discharge                               ', &         
         &  'first K coordinate of discharge                               ', &         
         &  'first X coordinate of discharge                               ', &
         &  'first Y coordinate of discharge                               ', &         
         &  'first Z coordinate of discharge                               ', &
         &  'second M coordinate of discharge                              ', &
         &  'second N coordinate of discharge                              ', &         
         &  'second K coordinate of discharge                              ', &         
         &  'second X coordinate of discharge                              ', &
         &  'second Y coordinate of discharge                              ', &         
         &  'second Z coordinate of discharge                              '/         
!
!! executable statements -------------------------------------------------------
!
    poscul           => gdp%gdculver%poscul
    date_time        => gdp%gdinttim%date_time
    nefiselem => gdp%nefisio%nefiselem(nefiswrthisdis)
    first            => nefiselem%first
    celidt           => nefiselem%celidt
    elmdms           => nefiselem%elmdms
    !
    nelmx2 = nelmx - 1
    !
    if (sferic) then
       elmunt( 9) = '[  DEG  ]'
       elmunt(10) = '[  DEG  ]'
       elmunt(15) = '[  DEG  ]'
       elmunt(16) = '[  DEG  ]'
    endif
    !
    ! Initialize local variables
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    !
    wrswch = .false.
    if (first) then
       call getcel(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierror   )
    endif
    celidt = celidt + 1
    !
    errmsg = ' '
    wrswch = .true.
    !
    ! Set up the element dimensions
    !
    if (first) then
        first = .false.
        call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,3         ,2         ,nsrc      ,lstsc     , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,4         ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,5         ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,6         ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,7         ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,8         ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,9         ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,10        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,11        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,12        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,13        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,14        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,15        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,16        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )
        call filldm(elmdms    ,17        ,1         ,nsrc      ,0         , &
                  & 0         ,0         ,0         )                  
    endif
    !
    ! element 'ITHISC'
    !
    wrswch = .true.
    idummy(1) = ithisc
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror   ,idummy    )
    if (ierror/= 0) goto 999
    !
    ! Element 'DATE_TIME'
    !
    datetimearr(1)=date_time
    call putgtc(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierror   ,datetimearr )
    if (ierror/= 0) goto 999
    !
    ! Element 'RINT', only if LSTSC > 0
    !
    if (lstsc .gt. 0) then
       do isrc = 1, nsrc
          do i = 1, lstsc
             rinttransposed(isrc, i) = rint(i, isrc)
          enddo
       enddo
       call putgtr(filnam    ,grnam     ,nelmx2    ,elmnms(2) ,elmdms(1, 2) , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                 & elmnms(3) ,celidt    ,wrswch    ,ierror   ,rinttransposed)
       if (ierror/= 0) goto 999
    endif
    !
    ! Element 'ZQ'
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(4) ,celidt    ,wrswch    ,ierror   ,disch      )
    if (ierror/= 0) goto 999    
    !
    ! Element 'ZQ_SUM'
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(5) ,celidt    ,wrswch    ,ierror   ,voldis    )
    if (ierror/= 0) goto 999    
    !
    ! Element 'MCOR1'
    !
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(6) ,celidt    ,wrswch    ,ierror   ,mnksrc(2, 1:nsrc))
    if (ierror/= 0) goto 999    
    !
    ! Element 'NCOR1'
    !
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(7) ,celidt    ,wrswch    ,ierror   ,mnksrc(1, 1:nsrc))
    if (ierror/= 0) goto 999    
    !
    ! Element 'KCOR1'
    !
    ! Recompute layer number of culvert discharge (KCOR1) for T0 (for POSTPR)
    ! NOTE: IMPORTANT
    !       In case of z-model different way of determining the z-coordinate
    !       (using relative layer thickness; complementary value is used due
    !       to inverted definition of layer indices in the z-model)
    !
    do isrc = 1, nsrc
       m = mnksrc(1, isrc)
       n = mnksrc(2, isrc)
       k = mnksrc(3, isrc)
       !
       h0 = real(dps(n, m),fp) + s1(n, m)
       if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
         zup = s1(n, m)
         do k=1,kmax
            zdown = zup + h0 *sig(k)
            if (poscul(isrc) < zup .and. &
              & poscul(isrc) >= zdown) mnksrc(3, isrc) = k
            zup = zdown
         enddo
       endif
    enddo
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(8) ,celidt    ,wrswch    ,ierror   ,mnksrc(3, 1:nsrc))
    if (ierror/= 0) goto 999    
    !
    ! Element 'XCOR1'
    !
    do isrc = 1, nsrc
        m = mnksrc(1, isrc)
        n = mnksrc(2, isrc)
        ndummy(isrc) = xcor(n, m)
    enddo
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(9) ,celidt    ,wrswch    ,ierror   ,ndummy    )
    if (ierror/= 0) goto 999    
    !
    ! Element 'YCOR1'
    !
    do isrc = 1, nsrc
        m = mnksrc(1, isrc)
        n = mnksrc(2, isrc)    
        ndummy(isrc) = ycor(n, m)
    enddo
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(10),celidt    ,wrswch    ,ierror   ,ndummy    )
    if (ierror/= 0) goto 999    
    !
    ! Element 'ZCOR1'
    !
    ! Compute the exact Z-position of the discharges for ZCOR1
    ! NOTE: IMPORTANT
    !       In case of z-model different way of determining the z-coordinate
    !       (using relative layer thickness; complementary value is used due
    !       to inverted definition of layer indices in the z-model)
    !
    do isrc = 1, nsrc
       m = mnksrc(1, isrc)
       n = mnksrc(2, isrc)
       k = mnksrc(3, isrc)
       !
       h0 = real(dps(n, m),fp) + s1(n, m)
       !
       if (k == 0) then
          ndummy(isrc) = s1(n, m) - 0.5 * h0
       elseif (zmodel) then
          ndummy(isrc) = 0.5 * ( zk(k) + zk(k-1) )
       else
          ndummy(isrc) = s1(n, m) + sig(k) * h0
       endif
       if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
          ndummy(isrc) = poscul(isrc)
       endif
    enddo
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(11),celidt    ,wrswch    ,ierror   ,ndummy    )
    if (ierror/= 0) goto 999    
    !
    ! Element 'MCOR2'
    !
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(12),celidt    ,wrswch    ,ierror   ,mnksrc(5, 1:nsrc))
    if (ierror/= 0) goto 999    
    !
    ! Element 'NCOR2'
    !
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(13),celidt    ,wrswch    ,ierror   ,mnksrc(4, 1:nsrc))
    if (ierror/= 0) goto 999    
    !
    ! Element 'KCOR2'
    !
    ! Recompute layer number of culvert discharge (KCOR2) for T0 (for POSTPR)
    ! NOTE: IMPORTANT
    !       In case of z-model different way of determining the z-coordinate
    !       (using relative layer thickness; complementary value is used due
    !       to inverted definition of layer indices in the z-model)
    !
    do isrc = 1, nsrc
       m = mnksrc(4, isrc)
       n = mnksrc(5, isrc)
       k = mnksrc(6, isrc)
       !
       h0 = real(dps(n, m),fp) + s1(n, m)
       if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
         zup = s1(n, m)
         do k=1,kmax
            zdown = zup + h0 *sig(k)
            if (poscul(isrc) < zup .and. &
              & poscul(isrc) >= zdown) mnksrc(6, isrc) = k
            zup = zdown
         enddo
       endif
    enddo
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(14),celidt    ,wrswch    ,ierror   ,mnksrc(6, 1:nsrc))
    if (ierror/= 0) goto 999    
    !
    ! Element 'XCOR2'
    !
    do isrc = 1, nsrc
        m = mnksrc(4, isrc)
        n = mnksrc(5, isrc)
        ndummy(isrc) = xcor(n, m)
    enddo
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(15),celidt    ,wrswch    ,ierror   ,ndummy    )
    if (ierror/= 0) goto 999    
    !
    ! Element 'YCOR2'
    !
    do isrc = 1, nsrc
        m = mnksrc(4, isrc)
        n = mnksrc(5, isrc)    
        ndummy(isrc) = ycor(n, m)
    enddo
    !
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(16),celidt    ,wrswch    ,ierror   ,ndummy    )
    if (ierror/= 0) goto 999    
    !
    ! Element 'ZCOR2'
    !    
    ! Compute the exact Z-position of the discharges for ZCOR2
    ! NOTE: IMPORTANT
    !       In case of z-model different way of determining the z-coordinate
    !       (using relative layer thickness; complementary value is used due
    !       to inverted definition of layer indices in the z-model)
    !
    do isrc = 1, nsrc
       m = mnksrc(4, isrc)
       n = mnksrc(5, isrc)
       k = mnksrc(6, isrc)
       !
       h0 = real(dps(n, m),fp) + s1(n, m)
       !
       if (k == 0) then
          ndummy(isrc) = s1(n, m) - 0.5 * h0
       elseif (zmodel) then
          ndummy(isrc) = 0.5 * ( zk(k) + zk(k-1) )
       else
          ndummy(isrc) = s1(n, m) + sig(k) * h0
       endif
       if (mnksrc(7, isrc) == 4 .or. mnksrc(7, isrc) == 5 ) then
          ndummy(isrc) = poscul(isrc)
       endif
    enddo
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(17),celidt    ,wrswch    ,ierror   ,ndummy    )
    if (ierror/= 0) goto 999    
    !    
    ! Write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrthisdis
