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

      module BottomSet
C
C          module contains everything for the Jos van Gils bottom collumns
C          created 11 July 2003 by Leo Postma
C
C     contains the fiollowing derived types:
C          BotColmn               ! a set of information on one bottom collumn
C          BotColmnColl           ! a collection of these bottom collumns.
C
C     contains the following functions:
C          FilePropCollAdd        ! to add a collumn to the collection
C
C     contains the following subroutine:
C                                 !
C**WARNING this module also contains a variable Coll, being a collection of
C          bottom collumns. It is no decent programming practice to let a module
C          also transport data from one subroutine to another. For the WAQ processes
C          library routines with fixed calling conventions it is however a reasonable
C          alternative to the mediocre named common blocks
C
C
      integer, parameter, private :: MAX_NUM = 100      ! array space allocated per bunch
C
C          this is the information for one bottom collumn
C
      type BotColmn
         integer          :: fstwatsed         ! first water sediment exchange number
         integer          :: lstwatsed         ! last  water sediment exchange number
         integer          :: topsedsed         ! first within collumn exchange number
         integer          :: botsedsed         ! last exchange of collumn to deeper bnd
      end type BotColmn
C
C          this is the collection of the bottom collumns
C
      type BotColmnColl
         type(BotColmn), pointer :: set(:)     ! array with info for all bottom collumns
         integer                 :: maxsize    ! maximum size of the current array
         integer                 :: cursize    ! filled up to this size
      end type BotColmnColl

C**WARNING here is the mentioned variable Coll
      type ( BotColmnColl ) :: Coll
C
      contains
C
C          function to add a bottom collumn to the collection
C
      function BotColmnCollAdd( aBotColmnColl , ifirst , ilast , itop , ibot ) result ( size )
C
         type(BotColmnColl)      :: aBotColmnColl
         integer                 :: ifirst, ilast, itop, ibot, size
         type(BotColmn), pointer :: tset(:)
         type(BotColmn)          :: aBotColmn
C                          check if there is space left
         if ( aBotColmnColl%cursize .eq. aBotColmnColl%maxsize ) then
            allocate ( tset( aBotColmnColl%maxsize + MAX_NUM ) ) ! allocate new space
            do i = 1 , aBotColmnColl%maxsize
               tset(i) = aBotColmnColl%set(i)                    ! copy the old array
            enddo        ! throw away the old array
            if ( aBotColmnColl%maxsize .ne. 0 ) deallocate ( aBotColmnColl%set )
            aBotColmnColl%set     => tset     ! put new array in place
            aBotColmnColl%maxsize =  aBotColmnColl%maxsize + MAX_NUM  ! increase maximum
         endif
         aBotColmnColl%cursize = aBotColmnColl%cursize + 1       ! increase current size
         aBotColmn%fstwatsed   = ifirst                          ! make a new bottom
         aBotColmn%lstwatsed   = ilast                           ! collumn
         aBotColmn%topsedsed   = itop                            ! from the parameters
         aBotColmn%botsedsed   = ibot                            ! of the call
         aBotColmnColl%set(aBotColmnColl%cursize) = aBotColmn    ! copy the content into the array
         size = aBotColmnColl%cursize                            ! set the return value
C
         return
C
      end function BotColmnCollAdd
C
      end module BottomSet
