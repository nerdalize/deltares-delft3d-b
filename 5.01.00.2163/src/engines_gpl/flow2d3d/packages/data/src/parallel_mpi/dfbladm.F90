subroutine dfbladm ( ipown, icom, mmax, nmax, gdp )
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
!  $Id: dfbladm.F90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfbladm.F90 $
!!--description-----------------------------------------------------------------
!
!   For the present node, carries out the block administration
!   and determines array bounds with respect to global grid
!
!!--pseudo code and references--------------------------------------------------
!
!   intialize offsets to be used in searching for interfaces
!   determine enclosing box of present subdomain
!   if subdomain appears to be empty
!      give warning and set empty bounding box
!   else
!      extend enclosing box to include halo area
!   determine interface sizes:
!
!      loop over global grid
!         if point belongs to this part
!            for each of the four sizes
!                if a neighbouring subdomain is found there
!                   find it in the list of neighbours
!                   if not yet in the list, add it
!                   store position of neighbour
!                   update number of overlapping unknowns
!                   store indices of point in present and halo areas
!
!   store block administration
!
!
!!--declarations----------------------------------------------------------------
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, intent(in)                       :: mmax  ! number of gridpoints in the x-direction
    integer, intent(in)                       :: nmax  ! number of gridpoints in the y-direction
    !
    integer, dimension(mmax,nmax), intent(in) :: ipown ! array giving the subdomain number of each gridpoint
!
    integer, dimension(-1:mmax+2, nmax), intent(in) :: icom
    type(dfparalltype), pointer          :: gdparall
!
! Local variables
!
    integer, pointer            :: lundia
    integer, pointer            :: nfg
    integer, pointer            :: nlg
    integer, pointer            :: mfg
    integer, pointer            :: mlg
    integer, pointer            :: nmaxgl
    integer, pointer            :: mmaxgl
    integer, dimension(:), pointer            :: iblkad
    integer                              :: i      ! loop counter
    integer, dimension(:,:), allocatable :: icrecv ! array containing positions of unknowns to be received from neighbour
    integer, dimension(:,:), allocatable :: icsend ! array containing positions of unknowns to be sent to neighbour
    integer                              :: idom   ! subdomain number
    integer                              :: ihalo  ! actual width of halo area
    integer                              :: inb    ! neighbour counter
    integer                              :: istat  ! status code of allocation
    integer, dimension(:), allocatable   :: itemp  ! temporary work array to store block administration
    integer, dimension(3,nproc)          :: iwork  ! array used to determine interface sizes
                                                   !       iwork(1,i) = number of the i-th neighbour
                                                   !       iwork(2,i) = position of the i-th neighbour with
                                                   !                    respect to present subdomain
                                                   !                    (resp. top, bottom, right, left)
                                                   !       iwork(3,i) = size of interface to i-th neighbour
    integer                              :: j      ! loop counter
    integer, dimension(2,4)              :: joffs  ! offsets at which a point of a neigbhour domain can be found
    integer                              :: length ! actual length of array IBLKAD
    integer                              :: m      ! current M-index of point in computational row
    integer                              :: moff   ! offset in x-direction
    integer                              :: n      ! current N-index of point in computational column
    integer                              :: nneigh ! number of neighbouring subdomains
    integer                              :: noff   ! offset in y-direction
    integer                              :: novlu  ! number of overlapping unknowns
    integer                              :: nsiz   ! size of present subdomain in y-direction
    !
    character(300)                       :: message ! string to pass message
    !
    integer                              :: istart
    integer                              :: iend
    
!   integer, dimension(:,:), allocatable :: iarrc
!
!! executable statements -------------------------------------------------------
!
    lundia   => gdp%gdinout%lundia
    gdparall => gdp%gdparall
    mmaxgl   => gdparall%mmaxgl
    nmaxgl   => gdparall%nmaxgl
    nfg      => gdparall%nfg
    nlg      => gdparall%nlg
    mfg      => gdparall%mfg
    mlg      => gdparall%mlg
    !
    write(message,'(a,i3.3,a)') 'Running parallel. Partition ', inode, ':'
    call prterr(lundia, 'G051', trim(message))
    !
    ! intialize offsets to be used in searching for interfaces
    !
    joffs = reshape((/0,1,0,-1,1,0,-1,0/), (/2,4/))
    !
    ! determine enclosing box of present subdomain
    !
    nfg = nmax+1
    nlg = 0
    mfg = mmax+1
    mlg = 0
    !
    do m = 1, mmax
       do n = 1, nmax
          if( ipown(m,n) == inode ) then
             nfg = min(n,nfg)
             nlg = max(n,nlg)
             mfg = min(m,mfg)
             mlg = max(m,mlg)

          endif

       enddo
    enddo
    !
    ! if subdomain appears to be empty
    !
    if ( nfg > nlg .or. mfg > mlg ) then
       !
       ! give warning and set empty bounding box
       !
       write (message,'(a,i3.3)') 'Empty subdomain is detected - node number is ',inode
       call prterr(lundia, 'U190', trim(message))

       nfg = 1
       nlg = 0
       mfg = 1
       mlg = 0
    else
       !
       ! extend enclosing box to include halo area
       !
       if (idir==1) then
          nfg = max(   1,nfg-ihalon)
          nlg = min(nmax,nlg+ihalon)
          mfg = 1
          mlg = mmax
       else if (idir==2) then 
          mfg = max(   1,mfg-ihalom)
          mlg = min(mmax,mlg+ihalom)
          nfg = 1
          nlg = nmax
       endif
    endif
    !
    nsiz = nlg - nfg + 1
    if ( mod(nsiz,2)==0 ) nsiz = nsiz + 1
    allocate(icrecv(nproc,max(ihalom,ihalon)*max(mmax,nmax)))
    allocate(icsend(nproc,max(ihalom,ihalon)*max(mmax,nmax)))
    !
    iwork  = 0
    icrecv = 0
    icsend = 0
    !
    ! determine interface sizes
    !
    do m = 1, mmax
       do n = 1, nmax
          !
          ! if point belongs to this part
          !
          if ( ipown(m,n) == inode ) then
             !
             ! for each of the four sizes
             !
             do i = 1, 4
                moff  = joffs(1,i)
                noff  = joffs(2,i)
                if ( i==1 .or. i==2 ) then
                   ihalo = ihalon
                else
                   ihalo = ihalom
                endif
                !
                ! if a neighbouring subdomain is found there
                !
                if ( (m+moff) > 0 .and. (m+moff) <= mmax .and.  &
                     (n+noff) > 0 .and. (n+noff) <= nmax ) then
                   if ( ipown(m+moff,n+noff) /= 0    .and.  &
                        ipown(m+moff,n+noff) /= inode ) then
                      !
                      ! find it in the list of neighbours
                      !
                      idom = ipown(m+moff,n+noff)
                      inb = 1
  100                 continue
                      if ( inb <= nproc .and.  &
                           iwork(1,inb) /= idom .and.  &
                           iwork(1,inb) /= 0 )  then
                         inb = inb + 1
                         goto 100
                      endif

                      if ( inb > nproc ) then
                         call prterr(lundia, 'U021', 'Found more neighbours than subdomains in the partitioning')
                         call d3stop(1, gdp)
                      endif
                      !
                      ! if not yet in the list, add it
                      !
                      if ( iwork(1,inb) == 0 ) iwork(1,inb) = idom
                      !
                      ! store position of neighbour with respect to present subdomain
                      !
                      iwork(2,inb) = i
                      !
                      ! update number of overlapping unknowns
                      !
                      iwork(3,inb) = iwork(3,inb) + ihalo
                      !
                      ! store indices of point in present and halo areas
                      !
                      do j = 1, ihalo
                         icsend(inb,iwork(3,inb)-j+1) = (m-(j-1)*moff-mfg)*nsiz + (n-(j-1)*noff-nfg+1)
                         icrecv(inb,iwork(3,inb)-j+1) = (m+    j*moff-mfg)*nsiz + (n+    j*noff-nfg+1)
                      enddo
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! store block administration to be used for communication
    !
    allocate(itemp(1+2*nproc*(2+max(ihalom,ihalon)*max(mmax,nmax))))
    itemp = -999
    !
    nneigh   = count(iwork(1,:)>0)
    itemp(1) = nneigh
    istart   = 3*nneigh+2
    do inb = 1, nneigh
       itemp(3*inb-1) = iwork(1,inb)
       itemp(3*inb  ) = iwork(2,inb)
       itemp(3*inb+1) = istart
       novlu          = iwork(3,inb)
       itemp(istart)  = novlu
       do i = 1, novlu
          itemp(istart      +i) = icsend(inb,i)
          itemp(istart+novlu+i) = icrecv(inb,i)
       enddo
       istart = istart + 2*novlu+1
    enddo
    !
    length = count(itemp/=-999)
    allocate (gdp%gdparall%iblkad(length), stat = istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'dfbladm: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! Update references
    !
    iblkad   => gdparall%iblkad
    !
    do i = 1, length
       iblkad(i) = itemp(i)
    enddo
    deallocate(icrecv,icsend,itemp)
end subroutine dfbladm
