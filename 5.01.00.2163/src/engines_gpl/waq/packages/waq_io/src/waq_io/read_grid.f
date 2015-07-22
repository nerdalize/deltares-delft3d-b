!!  Copyright(C) Stichting Deltares, 2012.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine read_grid ( lun    , aGrid  , GridPs , oldproc, ierr   )

!       Deltares Software Centre

!>\file
!>                    Reads a grid definition
!>
!>          Sets all properties of the grid and the pointer from each horizontal cell of the
!>          reference grid to this grid. After the pointer is specified, no further
!>          properties are read.\n
!>          The type of the grid has already been set in the calling grid.f routine
!>          Poperties that can be set are:
!>          - NOLAY followed by an integer, number of layers of the grid (default is 0)
!>          - AGGREGATIONFILE followed by a filename of a .lga type binary aggregation file
!>          - REFERENCEGRID followed by the name of the reference grid for this grid
!>          If an integer is met, the routine expects as many integers as in one
!>          layer of the reference grid. They must contain the mapping of all cells of that
!>          reference grid on the cells of this grid.\n
!>          The default reference grid is the system base grid.\n
!>          The routine determines the maximum gridcell nr of this new grid.\n
!>          The routine checks that every cell of this new grid contains at least one cell
!>          of the reference grid.

!     Created        : ......  200.    Jan van Beek  creation of Layered Bed special
!                      May     2011    Leo Postma    merged with standard version

!     Subroutines called : gettoken  to read tokens from the input file is in the rd_token module
!                          dhopenf   to open a binary file to read gridpointer from

!     Functions called   : gridpointercollfind to find the number of the reference grid by name
!                                              function is contained in the grids module

      use grids            !   for the storage of contraction grids
      use rd_token         !   for the reading of tokens
      use timers       !   performance timers

      implicit none

!     declaration of arguments

      integer               , intent(in   ) :: lun(*)    !< unit numbers used
      type(GridPointer)     , intent(inout) :: aGrid     !< collection off all grid definitions
      type(GridPointerColl) , intent(in   ) :: GridPs    !< collection off all grid definitions
      logical               , intent(in   ) :: oldproc   !< true if old processing
      integer               , intent(inout) :: ierr      !< cummulative error count

!     local declarations

      integer               :: itype        ! type of input that was obtained
      integer               :: itoken       ! integer token from input
      integer               :: idummy       ! dummy which content is not used
      character(len=255)    :: ctoken       ! character token from input
      integer               :: ierr2        ! local error indication
      integer               :: i_base_grid  ! index base grid in collection
      integer               :: i_grid       ! index grid in collection
      integer               :: nmax         ! nmax
      integer               :: mmax         ! mmax
      integer               :: noseg        ! number of segments
      integer               :: noseg2       ! number of segments in sub grid
      integer               :: noseg_lay    ! number of segments per layer
      integer               :: noseg_fil    ! number of segments in file
      integer               :: noseg_input  ! number of segments in input
      integer               :: iseg         ! index segment number
      integer               :: iseg2        ! second index segment number
      integer               :: nolay        ! number of layers
      integer, allocatable  :: iwork(:)     ! work array
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "read_grid", ithndl )

!     some init

      i_base_grid   = GridPs%base_grid
      noseg         = GridPs%pointers(i_base_grid)%noseg
      noseg_lay     = GridPs%pointers(i_base_grid)%noseg_lay

!     set default, type is already set in calling routine
!                                          ( bottomgrid, processgrid, subgrid )

      aGrid%name            =  ' '
      aGrid%noseg           =  0
      aGrid%noseg_lay       =  0
      aGrid%iref            = i_base_grid         ! default, may be overridden
      aGrid%name_ref        =  ' '
      aGrid%iarray          => null()
      aGrid%space_var_nolay =  .false.
      aGrid%nolay           =  1                  ! default, may be overridden
      aGrid%nolay_var       => null()
      if ( gettoken( ctoken, ierr2 ) .gt. 0 ) goto 1000    ! get name
      aGrid%name            =  ctoken
      write ( lunut , 2000 ) aGrid%name
      if ( oldproc ) then
         if ( gettoken( agrid%iref, ierr2 ) .gt. 0 ) goto 1000
         agrid%name_ref = gridps%pointers(agrid%iref)%name
      endif

      do

         if ( gettoken( ctoken, itoken, itype, ierr2 ) .gt. 0 ) goto 1000
         if ( itype .eq. 1 ) then                              ! it is a string
            select case ( ctoken )

               case ( 'NOLAY' )
                  if ( gettoken( aGrid%nolay, ierr2 ) .gt. 0 ) goto 1000
                  write ( lunut , 2010 ) aGrid%nolay

               case ( 'AGGREGATIONFILE' )                      ! it is the filename keyword
                  if ( gettoken( ctoken, ierr2 ) .gt. 0 ) goto 1000
                  call dhopnf ( lun(33), ctoken, 33, 1, ierr2 )
                  if ( ierr2 .ne. 0 ) goto 1000
                  read  ( lun(33) ,   *  ) nmax,mmax,noseg_fil,idummy,idummy
                  write ( lunut   , 2020 ) ctoken, nmax, mmax, noseg_fil
                  if ( noseg_fil .ne. noseg_lay ) then
                     write ( lunut , 2030 ) noseg_fil, noseg_lay
                     goto 1000
                  endif
                  allocate ( aGrid%iarray(noseg) )
                  read  ( lun(33) , * ) ( aGrid%iarray(iseg),iseg=1,noseg_fil )
                  close ( lun(33) )
                  exit                                         ! input for the grid is ready

               case ( 'REFERENCEGRID' )
                  if ( gettoken( ctoken, ierr2 ) .gt. 0 ) goto 1000
                  aGrid%name_ref = ctoken
                  write ( lunut , 2040 ) aGrid%name_ref
                  i_grid = gridpointercollfind( GridPs, aGrid%name_ref )
                  if ( i_grid .gt. 0 ) then
                     aGrid%iref      = i_grid
                  else
                     write ( lunut , 2050 )
                     ierr = ierr + 1
                  endif
!jvb not set yet  noseg     = GridPs%pointers(aGrid%iref)%noseg
                  noseg_lay = GridPs%pointers(aGrid%iref)%noseg_lay

               case default
                  write ( lunut , 2060 ) trim(ctoken)          ! ERROR, token not recognised
                  goto 1000

            end select
         else                                                  ! it was an integer.
            allocate ( aGrid%iarray(noseg) )
            aGrid%iarray(1) = itoken                           ! this integer is first pointer
            do iseg = 2, noseg_lay
               if ( gettoken( aGrid%iarray(iseg), ierr2 ) .gt. 0 ) goto 1000
               if ( aGrid%iarray(iseg) .gt. noseg_lay ) then
                  write ( lunut , 2070 ) aGrid%iarray(iseg)
                  ierr = ierr + 1
               endif
            enddo
            exit                                               ! input for the grid is ready
         endif

      enddo

!     Determine nr of segments in aggregated pointer

      noseg2 = 0
      allocate(iwork(noseg))
      iwork  = 0
      do iseg = 1 , noseg_lay
         iseg2 = aGrid%iarray(iseg)
         if ( iseg2 .gt. 0 ) then
            noseg2 = max(noseg2,iseg2)
            iwork(iseg2) = iwork(iseg2) + 1
         endif
      enddo
      do iseg2 = 1 , noseg2
         if ( iwork(iseg2) .eq. 0 ) then
            write ( lunut , 2080 ) iseg2
            ierr = ierr + 1
         endif
      enddo
      aGrid%noseg_lay = noseg2
      deallocate( iwork )

      if (timon) call timstop( ithndl )
      return

 1000 continue
      write( lunut, 2090 )
      ierr = ierr + 1
      return

 2000 format ( ' Name of this grid is: ',A)
 2010 format ( ' Number of layers for this grid:',I10)
 2020 format ( ' Aggregationfile     : ',A,
     &        /' Matrix (',I5,'x',I5,') of ',I7,' elements.' )
 2030 format ( ' ERROR, nr of cells in aggregation file is: ',I10,
     &        /'        nr of hor. cells in simulation is:  ',I10  )
 2040 format ( ' Reference grid for this grid  : ',A)
 2050 format (/' ERROR, reference grid not defined.')
 2060 format (/' ERROR, unrecognized token: ',A)
 2070 format (/' ERROR, segment in sub-grid out of range:',I15 )
 2080 format (/' ERROR, segment in sub-grid not defined:',I15 )
 2090 format (/' ERROR, reading GRID information.')

      end
