subroutine put_wave_fields (fg, fof, itide, wavetime, swflux)
!
! Head routine for calling crewav
!
use wave_data
use swan_flow_grid_maps
!
implicit none
!
integer                    :: itide    ! time step number of wave group on com-file
logical                    :: swflux   ! switch to indicate if mass flux is taken into account
type (grid)                :: fg       ! flow grid
type (output_fields)       :: fof      ! wave output fields defined on flow grid
type (wave_time_type)      :: wavetime
   !
   call crewav(fg%grid_name ,itide    ,fof%hrms  ,fof%tp      ,fof%dir     , &
             & fof%dissip   ,fof%fx   ,fof%fy    ,fof%mx      ,fof%my      , &
             & fof%tps      ,fof%ubot ,fof%wlen  ,fof%wsbodyu ,fof%wsbodyv , &
             & fof%mmax     ,fof%nmax ,swflux    ,wavetime    )
end subroutine put_wave_fields


subroutine crewav(filnam   ,itide    ,hrms     ,tp       ,dir      , &
                & diss     ,fx       ,fy       ,mx       ,my       , &
                & tps      ,ubot     ,wlen     ,wsbu     ,wsbv     , &
                & mmax     ,nmax     ,swflux   ,wavetime )
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
!  $Id: put_wave_fields.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/io/src/put_wave_fields.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use wave_data
    !
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx = 19
!
! Global variables
!
    character(37)                 , intent(in)  :: filnam
    integer                       , intent(in)  :: itide
    integer                       , intent(in)  :: mmax
    integer                       , intent(in)  :: nmax
    real, dimension(mmax, nmax)   , intent(in)  :: dir
    real, dimension(mmax, nmax, 4), intent(in)  :: diss
    real, dimension(mmax, nmax)   , intent(in)  :: fx
    real, dimension(mmax, nmax)   , intent(in)  :: fy
    real, dimension(mmax, nmax)   , intent(in)  :: hrms
    real, dimension(mmax, nmax)   , intent(in)  :: mx
    real, dimension(mmax, nmax)   , intent(in)  :: my
    real, dimension(mmax, nmax)   , intent(in)  :: tp
    real, dimension(mmax, nmax)   , intent(in)  :: tps
    real, dimension(mmax, nmax)   , intent(in)  :: ubot
    real, dimension(mmax, nmax)   , intent(in)  :: wlen
    real, dimension(mmax, nmax)   , intent(in)  :: wsbu
    real, dimension(mmax, nmax)   , intent(in)  :: wsbv
    logical                       , intent(in)  :: swflux
    type (wave_time_type)                       :: wavetime

!
! Local variables
!
    integer                                 :: celidt
    integer                                 :: error
    integer                                 :: ielem
    integer                                 :: ierr
    integer                                 :: iwave
    integer                                 :: m
    integer                                 :: n
    integer                                 :: nelems
    integer, dimension(1)                   :: ival
    integer, dimension(6, nelmx)            :: elmdms
    integer, dimension(nelmx)               :: nbytsg
    real   , dimension(:,:), allocatable    :: rbuff
    logical                                 :: wrswch
    logical, dimension(1)                   :: lval
    character(10), dimension(nelmx)         :: elmunt
    character(16), dimension(2)             :: grpnam
    character(16), dimension(nelmx)         :: elmnms
    character(16), dimension(nelmx)         :: elmqty
    character(16), dimension(nelmx)         :: elmtps
    character(64), dimension(nelmx)         :: elmdes
    !
    data grpnam/'WAVNT', 'WAVTIM'/
    data elmnms/'NTWAV'  , 'SWFLUX' , 'TIMWAV', 'HRMS', 'TP', 'DIR' , 'DISTOT', &
              & 'DISSURF', 'DISWCAP', 'DISBOT', 'FX'  , 'FY', 'WSBU', 'WSBV'  , &
              & 'MX'     , 'MY'     , 'TPS'   , 'UBOT', 'WLEN'/
    data elmdes/'Number of wave fields in group WAVTIM                         ', &
              & 'Mass flux written to comm. file (.true. or .false.)           ', &
              & 'Time of wave field rel. to reference date/time                ', &
              & 'Root mean square wave height                                  ', &
              & 'Peak wave period                                              ', &
              & 'Mean direction of wave propagation relative to ksi-dir. ccw   ', &
              & 'Total wave energy dissipation rate                            ', &
              & 'Wave energy dissipation rate at the free surface              ', &
              & 'Wave energy dissipation rate due to white capping             ', &
              & 'Wave energy dissipation rate at the bottom                    ', &
              & 'Wave forcing term at the free surface in u-point              ', &
              & 'Wave forcing term at the free surface in v-point              ', &
              & 'Wave forcing term in water body in u-point                    ', &
              & 'Wave forcing term in water body in v-point                    ', &
              & 'Wave-induced volume flux in u-point                           ', &
              & 'Wave-induced volume flux in v-point                           ', &
              & 'Smoothed peak period                                          ', &
              & 'Orbital motion near the bottom                                ', &
              & 'Mean wave length                                              '/
    data elmqty/nelmx*' '/
    data elmunt/'[   -   ]', '[   -   ]', '[ TSCALE]', '[   M   ]', '[   S   ]', &
              & '[  DEG  ]', '[  W/M2 ]', '[  W/M2 ]', '[  W/M2 ]', '[  W/M2 ]', &
              & '[ N/M2  ]', '[ N/M2  ]', '[ N/M2  ]', '[ N/M2  ]', '[ M3/SM ]', &
              & '[ M3/SM ]', '[   S   ]', '[  M/S  ]', '[   M   ]'/
    data elmtps/'INTEGER', 'LOGICAL', 'INTEGER', 16*'REAL'/
    data nbytsg/nelmx*4/
!
!! executable statements -------------------------------------------------------
!
    ! Allocate temporary buffer array for switching of dimensions
    !
    allocate (rbuff (nmax,mmax))
    !
    celidt = 1
    call filldm(elmdms    ,1         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,3         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,5         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,6         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,7         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,8         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,9         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,10        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,11        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,12        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,13        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,14        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,15        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,16        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,17        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,18        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,19        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    !
    !        Write all elements to file, or read them from file; all
    !        definition and creation of files, data groups, cells and
    !        elements is handled by PUTGET
    !
    iwave = itide
    if (iwave<0) then
       nelems = 2
       wrswch = .false.
       ielem = 1
       ival(1) = 0
       call putgti(filnam    ,grpnam(1) ,nelems    ,elmnms    ,elmdms    , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
                 & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
       iwave = ival(1)+1
    endif
    !
    nelems = 2
    wrswch = .true.
    ielem = 1
    ival(1) = iwave
    call putgti(filnam    ,grpnam(1) ,nelems    ,elmnms    ,elmdms    , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    ielem = 2
    lval(1) = swflux
    call putgtl(filnam    ,grpnam(1) ,nelems    ,elmnms    ,elmdms    , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,lval      )
    nelems = 17
    celidt = iwave
    ielem = 3
    ival(1) = wavetime%timtscale
    call putgti(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
!
!    Hrms
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = hrms (m,n)
       enddo
    enddo

    ielem = 4
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Tp
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = tp (m,n)
       enddo
    enddo

    ielem = 5
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Dir
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = dir (m,n)
       enddo
    enddo

    ielem = 6
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Distot
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,1)
       enddo
    enddo

    ielem = 7
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Dissurf
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,2)
       enddo
    enddo

    ielem = 8
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Diswcap
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,3)
       enddo
    enddo

    ielem = 9
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Disbot
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,4)
       enddo
    enddo

    ielem = 10
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Fx
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = fx (m,n)
       enddo
    enddo

    ielem = 11
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Fy
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = fy (m,n)
       enddo
    enddo

    ielem = 12
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Wsbu
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = wsbu (m,n)
       enddo
    enddo

    ielem = 13
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Wsbv
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = wsbv (m,n)
       enddo
    enddo

    ielem = 14
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Mx
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = mx (m,n)
       enddo
    enddo

    ielem = 15
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    My
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = my (m,n)
       enddo
    enddo

    ielem = 16
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Tps
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = tps(m,n)
       enddo
    enddo

    ielem = 17
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Ubot
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = ubot(m,n)
       enddo
    enddo

    ielem = 18
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Wlen
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = wlen(m,n)
       enddo
    enddo

    ielem = 19
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!   Deallocate temporary buffer array for switching of dimensions
!
    deallocate (rbuff, stat=ierr)
end subroutine crewav
