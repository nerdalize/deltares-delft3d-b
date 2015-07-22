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

      subroutine uniset ( lun    , lchar  , nolun  , runid  )

!     Deltares Software Centre

!>\file
!>           Reads the input filename* ( keyboard /command line ) ;
!>           sets filenames* ; opens system files

!     Created           : April 1988  By M.E. Sileon / L. Postma

!     Subroutine called : dhopnf
!                         srstop

!     Logical units     : 5       = keyboard
!                         lun(26) = unit user input file
!                         lun(27) = unit stripped input file
!                         lun(29) = unit formatted output file
!                         lun( 2) = unit system-intermediate file
!                         lun( 3) = unit intermediate file (harmonics)
!                         lun( 4) = unit intermediate file (pointers)

      use timers       !   performance timers

      implicit none

!     Parameters         :

!     kind           function         name            Descriptipon

      integer  ( 4), intent(in   ) :: nolun         !< Amount of unit numbers
      integer  ( 4), intent(in   ) :: lun  (nolun)  !< Unit numbers
      character( *), intent(inout) :: lchar(nolun)  !< File names
      character( *), intent(in   ) :: runid         !< Runid

!     Local

      integer        ilun
      integer        ioerr
      character*(93) check
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "uniset", ithndl )

!        Get filename  ( keyboard / command line )

      check = lchar(29)
      call dhgnam(runid,check)

!        Pad the model name in the file names

      do ilun = 1 , nolun
         lchar(ilun) = trim(runid)//lchar(ilun)
      enddo

!        Remove any existing work files

      do ilun = 1 , nolun
         if ( index( lchar(ilun), '.wrk' ) .gt. 0 ) call dhdelf( lchar(ilun), ioerr )
      enddo

!        Open the neccessary unit numbers

      call dhopnf  ( lun(29) , lchar(29) , 29    , 1     , ioerr )
      call setmlu(lun(29))
      call dhopnf  ( lun(26) , lchar(26) , 26    , 1     , ioerr )
      if ( ioerr .gt. 0 ) then
         write ( lun(29), 1000 ) lun(26) , lchar(26)
         call srstop ( 1 )
      endif
      call dhopnf  ( lun( 2) , lchar( 2) ,  2    , 1     , ioerr )
      call dhopnf  ( lun( 3) , lchar( 3) ,  3    , 1     , ioerr )
      call dhopnf  ( lun( 4) , lchar( 4) ,  4    , 1     , ioerr )
      call dhopnf  ( lun(41) , lchar(41) , 41    , 1     , ioerr )

      if (timon) call timstop( ithndl )
      return

!        Output formats

 1000 format ( /' ERROR input file on unit:',I3,
     &         /' Filename = ',A,
     &         /' Does not exist.',
     &         /' EXECUTION HALTED !!!!!!!!!!!!!')
      end
