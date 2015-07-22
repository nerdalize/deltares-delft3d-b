subroutine restart_bodsed (error     ,restid    ,i_restart ,bodsed    , &
                         & lsedtot   ,nmaxus    ,mmax      ,success   ,gdp       )
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
!  $Id: restart_bodsed.f90 1499 2012-05-21 17:47:06Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/restart_bodsed.f90 $
!!--description-----------------------------------------------------------------
! Reads initial field condition records from an
! NEFIS flow output map file
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
!
! Global variables
!
    integer                                                                               :: i_restart
    integer                                                                               :: lsedtot
    integer                                                                               :: nmaxus
    integer                                                                               :: mmax
    logical                                                                               :: error
    logical                                                                 , intent(out) :: success
    real(prec), dimension(lsedtot, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: bodsed
    character(*)                                                                          :: restid
!
! Local variables
!
    integer                                :: l
    integer                                :: m
    integer                                :: n
    integer                                :: lrid        ! character variables for files Help var., length of restid
    integer                     , external :: crenef
    integer                     , external :: getelt
    integer                     , external :: clsnef
    integer                     , external :: neferr
    integer                                :: rst_lsed
    integer                                :: rst_lsedbl
    integer                                :: rst_lsedtot
    integer                                :: ierror
    integer                                :: fds
    integer , dimension(3,5)               :: cuindex
    integer , dimension(3,5)               :: uindex
    real(sp), dimension(:,:,:,:), pointer  :: sbuff
    character(len=256)                     :: dat_file
    character(len=256)                     :: def_file
    character(len=1024)                    :: errmsg
!
!! executable statements -------------------------------------------------------
!
    nullify(sbuff)
    error        = .false.
    success      = .false.
    call noextspaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    ierror   = crenef(fds, dat_file, def_file, ' ', 'r')
    if (ierror/= 0) then
       error = .true.
       goto 9999
    endif
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    ! initialize group index time dependent data
    !
    uindex (3,1) = 1 ! increment in time
    uindex (1,1) = i_restart
    uindex (2,1) = i_restart
    !
    ! determine number of suspended sediment fractions
    !
    ierror = getelt(fds, 'map-const', 'LSED'  , cuindex, 1, 4, rst_lsed)
    if (ierror /= 0) then
       !
       ! if LSED has not been written to the map-file then LSED=0
       ! remove the error message from NEFIS error stack
       !
       rst_lsed   = 0
       ierror     = neferr(0,errmsg)
    endif
    !
    ! determine number of bedload sediment fractions
    !
    ierror = getelt(fds, 'map-const', 'LSEDBL', cuindex, 1, 4, rst_lsedbl)
    if (ierror /= 0) then
       !
       ! if LSEDBL has not been written to the map-file then LSEDBL=0
       ! remove the error message from NEFIS error stack
       !
       rst_lsedbl = 0
       ierror     = neferr(0,errmsg)
    endif
    rst_lsedtot = rst_lsed + rst_lsedbl
    if (rst_lsedtot /= lsedtot) goto 9999
    !
    allocate(sbuff(nmaxus, mmax, rst_lsedtot, 1))
    !
    ierror = getelt(fds , 'map-sed-series', 'BODSED', uindex, 1, &
                 & mmax*nmaxus*rst_lsedtot*4, sbuff )
    if (ierror/= 0) goto 9999
    do l = 1, lsedtot
       do m = 1, mmax
          do n = 1, nmaxus
             bodsed(l,n,m) = real(sbuff(n,m,l,1),prec)
          enddo
       enddo
    enddo
    success = .true.
9999 continue
    if (associated(sbuff)) deallocate (sbuff)
    ierror = clsnef(fds) 
end subroutine restart_bodsed
