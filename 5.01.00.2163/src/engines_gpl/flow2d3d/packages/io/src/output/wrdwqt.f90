subroutine wrdwqt(comfil    ,lundia    ,error     ,itcur     ,itimc     , &
                & nsrc      ,mnksrc    ,mmax      ,nmax      ,kmax      , &
                & nmaxus    ,lstsci    ,lsal      ,ltem      ,r1        , &
                & dicuv     ,dicww     ,discum    ,taubmx    ,rbuff     , &
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
!  $Id: wrdwqt.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrdwqt.f90 $
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
    logical                  , pointer :: salin
    logical                  , pointer :: temp
    logical                  , pointer :: htur2d
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    integer, dimension(:, :) , pointer :: elmdms
    type (nefiselement)      , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 8
!
! Global variables
!
    integer                                                                        , intent(in)  :: itcur  !!  Current time counter for the communication file,
                                                                                                           !!  where startingpoint depends on CYCLIC
    integer                                                                        , intent(in)  :: itimc  !!  Current time step counter for 2D system
    integer                                                                                      :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                                        , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                        , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                                                      :: lundia !  Description and declaration in inout.igs
    integer                                                                                      :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                      :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                      :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                                      :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(7, nsrc)                                                             :: mnksrc !  Description and declaration in esm_alloc_int.f90
    logical                                                                        , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: taubmx !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2)    , intent(in)  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nmaxus, mmax, kmax)                                                  :: rbuff  !  Description and declaration in r-i-ch.igs
    real(fp)    , dimension(nsrc)                                                                :: discum !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                                 :: comfil !!  Name for communication file
                                                                                                           !!  com-<case><label>
!
!
! Local variables
!
    integer                                    :: ierr   ! Flag for error when writing to Communication file 
    integer                                    :: k
    integer                                    :: m
    integer                                    :: n
    integer                                    :: nelmx1 ! Number of elements for group GRPNM1 
    integer                                    :: nelmx2 ! Number of elements for group GRPNM2 
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grpnm1 ! Data-group name defined for the COM-files (coupled with CURNT) 
    character(16)                              :: grpnm2 ! Data-group name defined for the COM-files (coupled with CURNT) 
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                             :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grpnm1/'DWQTIM'/
    data grpnm2/'TAUTIM'/
    data elmnms/'TIMCUR', 'RSAL', 'RTEM', 'DICUV', 'DICWW', 'DISCUM', 'MNKSRC', &
        & 'TAUMAX'/
    data elmqty/8*' '/
    data elmunt/'[ TSCALE]', '[ PPT   ]', '[ DEG   ]', '[  M2/S ]', '[  M2/S ]',&
        & '[  M3   ]', '[   -   ]', '[   -   ]'/
    data elmtps/'INTEGER', 5*'REAL', 'INTEGER', 'REAL'/
    data nbytsg/8*4/
    data elmdes/'Time of FLOW field rel.to reference date/time              '&
       & , 'Concentrations of salinity in zeta point                      ',    &
        & 'Concentrations of temperature in zeta point                   ',      &
        & 'Horizontal eddy diffusivity in zeta point                     ',      &
        & 'Vertical eddy diffusivity-3D in zeta point                    ',      &
        & 'Cummulative discharge original FLOW input                  ',      &
        & '(M,N,K) indices of discharge sources and time dep. location   ',      &
        & 'Tau_max in zeta points (scalar)                               '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrdwqt)
    !
    !-----Initialize local variables
    !
    !
    !     GLOBAL DATA INITIALISATION
    salin      => gdp%gdprocs%salin
    temp       => gdp%gdprocs%temp
    htur2d     => gdp%gdprocs%htur2d
    first      => nefiselem%first
    celidt     => nefiselem%celidt
    elmdms     => nefiselem%elmdms
    !
    ierr = 0
    nelmx1 = nelmx - 1
    nelmx2 = 1
    wrswch = .true.
    !
    !-----Set up the element dimensions
    !     different element dimensions for 2d and 3d applications
    !     if kmax =1 (2d) then only 2 dimensions
    !     every element must be defined for PUTGTI so if an element is
    !     empty (lsal = 0) then the dimensions are set 1 (RSAL)
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       if (lsal>0) then
          if (kmax>1) then
             call filldm(elmdms    ,2         ,3         ,nmaxus    ,mmax      , &
                       & kmax      ,0         ,0         )
          else
             call filldm(elmdms    ,2         ,2         ,nmaxus    ,mmax      , &
                       & 0         ,0         ,0         )
          endif
       endif
       if (ltem>0) then
          if (kmax>1) then
             call filldm(elmdms    ,3         ,3         ,nmaxus    ,mmax      , &
                       & kmax      ,0         ,0         )
          else
             call filldm(elmdms    ,3         ,2         ,nmaxus    ,mmax      , &
                       & 0         ,0         ,0         )
          endif
       endif
       if (lstsci>0 .and. (kmax>1 .or. htur2d)) then
          call filldm(elmdms    ,4         ,3         ,nmaxus    ,mmax      , &
                    & kmax      ,0         ,0         )
       endif
       if (kmax>1) then
          call filldm(elmdms    ,5         ,3         ,nmaxus    ,mmax      , &
                    & kmax      ,0         ,0         )
       endif
       if (nsrc>0) then
          call filldm(elmdms    ,6         ,1         ,nsrc      ,0         , &
                    & 0         ,0         ,0         )
          call filldm(elmdms    ,7         ,2         ,7         ,nsrc      , &
                    & 0         ,0         ,0         )
       endif
       call filldm(elmdms    ,8         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
    endif
    !
    !-----Write all elements to file; all definition and creation of files,
    !     data groups, cells and elements is handled by PUTGET.
    !
    !-----element  1 ITIMC for group DWQTIM (cel number ITCUR)
    !
    celidt = itcur
    idummy(1) = itimc
    call putgti(comfil    ,grpnm1    ,nelmx1    ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    !
    !-----element  2 RSAL (r1(lsal)) for group DWQTIM (cel number ITCUR)
    !
    if (lsal>0) then
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff(n, m, k) = r1(n, m, k, lsal)
             enddo
          enddo
       enddo
       !
       call putgtr(comfil    ,grpnm1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(2) ,celidt    ,wrswch    ,ierr      ,rbuff     )
       if (ierr/=0) goto 9999
    endif
    !
    !-----element  3 RTEM (r1(ltem)) for group DWQTIM (cel number ITCUR)
    !
    if (ltem>0) then
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff(n, m, k) = r1(n, m, k, ltem)
             enddo
          enddo
       enddo
       !
       call putgtr(comfil    ,grpnm1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(3) ,celidt    ,wrswch    ,ierr      ,rbuff     )
       if (ierr/=0) goto 9999
    endif
    !
    !-----element  4 DICUV for group DWQTIM (cel number ITCUR)
    !     kmax > 1      : dicuv depends on dicww
    !     htur2d = true : dicuv is calculated (HLES) (AND depends on dicww)
    !     kmax+1 contains initial values and should not be writte
    !
    if (lstsci>0 .and. (kmax>1 .or. htur2d)) then
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff(n, m, k) = dicuv(n, m, k)
             enddo
          enddo
       enddo
       !
       call putgtr(comfil    ,grpnm1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(4) ,celidt    ,wrswch    ,ierr      ,rbuff     )
       if (ierr/=0) goto 9999
    endif
    !
    !-----element  5 DICWW for group DWQTIM (cel number ITCUR)
    !
    if (kmax>1) then
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff(n, m, k) = dicww(n, m, k)
             enddo
          enddo
       enddo
       !
       call putgtr(comfil    ,grpnm1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(5) ,celidt    ,wrswch    ,ierr      ,rbuff     )
       if (ierr/=0) goto 9999
    endif
    !
    !-----element  6 DISCUM for group DWQTIM (cel number ITCUR)
    !
    if (nsrc>0) then
       call putgtr(comfil    ,grpnm1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(6) ,celidt    ,wrswch    ,ierr      ,discum    )
       if (ierr/=0) goto 9999
       !
       !-----element  7 MNKSRC for group DWQTIM (cel number ICURC)
       !
       call putgti(comfil    ,grpnm1    ,nelmx1    ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(7) ,celidt    ,wrswch    ,ierr      ,mnksrc    )
       if (ierr/=0) goto 9999
    endif
    !
    !-----element  1 TAUBMX for group TAUTIM (cel number ITCUR)
    !
    do m = 1, mmax
       do n = 1, nmaxus
          rbuff(n, m, 1) = taubmx(n, m)
       enddo
    enddo
    !
    call putgtr(comfil    ,grpnm2    ,nelmx2    ,elmnms(8) ,elmdms(1, 8)         , &
              & elmqty(8) ,elmunt(8) ,elmdes(8) ,elmtps(8) ,nbytsg(8) , &
              & elmnms(8) ,celidt    ,wrswch    ,ierr      ,rbuff     )
    if (ierr/=0) then
    endif
    !
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrdwqt
