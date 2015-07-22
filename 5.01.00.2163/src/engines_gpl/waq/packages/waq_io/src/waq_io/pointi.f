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

      subroutine pointi ( lun    , lchar  , noseg  , noq    , noq1   ,
     &                    noq2   , noq3   , noqt   , nobnd  , ipnt   ,
     &                    intsrt , ipopt1 , jtrack , ftype  , ioutpt ,
     &                    GridPs , ierr   , iwar   )

!       Deltares Software Centre

!>\file
!>            Reads exchange pointers on irregular grid
!>
!>            Routine
!>            - reads the exchange pointers on irregular grid in the waterphase
!>            - calls bound.f to:
!>              - compute number of open boundaries
!>              - adds the bed pointers to the pointer set to make noqt
!>              - compute number of codiagonals for direct implicit matrices

!     Created            : April 1989 by M.E. Sileon and L. Postma

!     Modified           : May   2011 by Leo Postma, Fortran90 look and feel

!     Subroutines called : BOUND

!     Logical units      : lunut   = unit formatted output file
!                          lun( 8) = unit intermediate file ('to-from')

      use grids          ! for the storage of contraction grids
      use rd_token       ! for the reading of tokens
      use timers       !   performance timers

      implicit none

!     Parameters         :

!     kind           function         name            Descriptipon

      integer  ( 4), intent(in   ) :: lun   (*)     !< array with unit numbers
      character( *), intent(inout) :: lchar (*)     !< array with file names of the files
      integer  ( 4), intent(in   ) :: noseg         !< number of computational volumes
      integer  ( 4), intent(in   ) :: noq           !< noq1 + noq2 + noq3
      integer  ( 4), intent(in   ) :: noq1          !< number of exchanges 1st direction
      integer  ( 4), intent(in   ) :: noq2          !< number of exchanges 2nd direction
      integer  ( 4), intent(in   ) :: noq3          !< number of exchanges 3rd direction
      integer  ( 4), intent(in   ) :: noqt          !< total number of exchanges
      integer  ( 4), intent(  out) :: nobnd         !< number of open boundaries
      integer  ( 4), intent(  out) :: ipnt (4,noqt) !< exchange pointer
      integer  ( 4), intent(in   ) :: intsrt        !< integration number
      integer  ( 4), intent(in   ) :: ipopt1        !< file option ( 0 = binary )
      integer  ( 4), intent(  out) :: jtrack        !< number of codiagonals of matrix
      integer  ( 4), intent(in   ) :: ftype         !< type of the pointer file
      integer  ( 4), intent(in   ) :: ioutpt        !< flag for more or less output
      type(GridPointerColl)           GridPs        !< Collection of grid pointers
      integer  ( 4), intent(inout) :: ierr          !< cumulative error   count
      integer  ( 4), intent(inout) :: iwar          !< cumulative warning count

!     Local variables    :

      integer      noq12       ! noq1 + noq2 (horizontal exchanges
      integer      iq          ! loop counter exchanges
      integer      ip          ! loop counter pointers
      integer      ierr2       ! local error count
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "pointi", ithndl )

      ierr2 = 0

!        Read exchange pointers

      noq12 = noq1 + noq2
      if ( ipopt1 .eq. 0 )  then
         call dhopnf  ( lun(8) , lchar(8) , 8      , 2+ftype, ierr2 )
         if ( ierr2 .ne. 0 ) goto 100
         do iq = 1, noq
            read ( lun(8) ) ipnt(:,iq)
         enddo
      else
         do iq = 1 , noq
            do ip = 1 , 4
               if ( gettoken( ipnt(ip,iq), ierr2 ) .gt. 0 ) goto 100
            enddo
         enddo
         call dhopnf  ( lun(8) , lchar(8) , 8     , 1     , ierr2 )
         if ( ierr2 .ne. 0 ) goto 100
         if ( noq1 .gt. 0 ) write( lun(8) )( ipnt(:,iq), iq =       1, noq1  )
         if ( noq2 .gt. 0 ) write( lun(8) )( ipnt(:,iq), iq = noq1 +1, noq12 )
         if ( noq3 .gt. 0 ) write( lun(8) )( ipnt(:,iq), iq = noq12+1, noq   )

         if ( ioutpt .lt. 4 ) then
            write ( lunut , 2000 )
         else
            if ( noq1 .gt. 0 ) then
               write ( lunut, 2010 )
               write ( lunut, 2020 )
               write ( lunut, 2030 ) ( iq, ipnt(:,iq), iq =       1, noq1  )
            endif

            if ( noq2 .gt. 0 ) then
               write ( lunut, 2040 )
               write ( lunut, 2020 )
               write ( lunut, 2030 ) ( iq, ipnt(:,iq), iq = noq1 +1, noq12 )
            endif

            if ( noq3.gt.0 ) then
               write ( lunut, 2050 )
               write ( lunut, 2020 )
               write ( lunut, 2030 ) ( iq, ipnt(:,iq), iq = noq12+1, noq   )
            endif
         endif
      endif

!       calculate number of boundaries and bandwith of matrix

      call bound  ( lun    , noseg  , noq    , noqt   , intsrt ,
     &              ioutpt , GridPs , nobnd  , jtrack , ipnt   ,
     &              ierr   , iwar   )

      close ( lun(8) )
  100 if ( ierr2 .gt. 0 ) ierr = ierr + 1
      if (timon) call timstop( ithndl )
      return

 2000 format (  / ' Exchange pointers are printed for output option 4 and higher !' )
 2010 format (  /,'           First direction :' )
 2020 format (    '   Item nr.  From      To  From-1    To+1' )
 2030 format (     5I8 )
 2040 format (  /,'           Second direction :' )
 2050 format (  /,'           Third direction :' )

      end
