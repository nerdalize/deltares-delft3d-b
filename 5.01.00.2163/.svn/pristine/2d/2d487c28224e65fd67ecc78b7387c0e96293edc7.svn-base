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

      subroutine wrwrko ( lunwro, noutp , nbufmx, ioutps, outputs)

!     Deltares Software Centre

!>/File
!>      write output work file

!     Created   : Nov   1994 by Jan van Beek
!     Modified  : Aug   2012 by Jan van Beek, use output structure, modern look and feel

      use timers         !< performance timers
      use output

      implicit none

      integer             , intent(in   ) :: lunwro                 !< output work file
      integer             , intent(in   ) :: noutp                  !< total number of output files
      integer             , intent(in   ) :: nbufmx                 !< maximum buffer length
      integer             , intent(in   ) :: ioutps(7,*)            !< (old) output structure
      type(outputcoll)    , intent(in   ) :: outputs                !< output structure

      ! local

      real                                :: versio                 !  version number output system
      integer                             :: k                      !  loop counter
      integer                             :: nrvart                 !  total number of variables in output
      integer(4)                          :: ithndl = 0             ! handle for performance timer
      if (timon) call timstrt( "wrwrko", ithndl )

      versio = 0.0
      nrvart = outputs%cursize

      ! write work file

      write ( lunwro ) versio
      write ( lunwro ) noutp , nrvart, nbufmx
      write ( lunwro ) ( ioutps(1,k) , k = 1 , noutp )
      write ( lunwro ) ( ioutps(2,k) , k = 1 , noutp )
      write ( lunwro ) ( ioutps(3,k) , k = 1 , noutp )
      write ( lunwro ) ( ioutps(4,k) , k = 1 , noutp )
      write ( lunwro ) ( ioutps(5,k) , k = 1 , noutp )
      write ( lunwro ) ( ioutps(6,k) , k = 1 , noutp )
      if (nrvart.gt.0) then
         write ( lunwro ) ( outputs%pointers(k)   , k = 1 , nrvart)
         write ( lunwro ) ( outputs%names   (k)   , k = 1 , nrvart)
      end if

      if (timon) call timstop( ithndl )
      return
      return
      end
