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

      module Output
C
C          module contains everything for specification of output variables
C          created 31 January 2003 by Leo Postma
C
C     contains the fiollowing derived types:
C          OutputPointer          ! a set of information with respect to one grid pointer
C          OutputPointerColl      ! a collection of these grid pointers
C
C     contains the following functions:
C
C     contains the following subroutine:
C
      integer, parameter :: NAME_SIZE      =  20                ! size of descriptive names

      integer, parameter :: IMON              =   1          ! type monitoring file
      integer, parameter :: IMO2              =   2          ! type of output file
      integer, parameter :: IDMP              =   3          ! type of output file
      integer, parameter :: IDM2              =   4          ! type of output file
      integer, parameter :: IHIS              =   5          ! type of output file
      integer, parameter :: IHI2              =   6          ! type of output file
      integer, parameter :: IMAP              =   7          ! type of output file
      integer, parameter :: IMA2              =   8          ! type of output file
      integer, parameter :: IBAL              =   9          ! type of output file
      integer, parameter :: IHNF              =  10          ! type of output file
      integer, parameter :: IHN2              =  11          ! type of output file
      integer, parameter :: IMNF              =  12          ! type of output file
      integer, parameter :: IMN2              =  13          ! type of output file
      integer, parameter :: IMO3              =  14          ! type of output file
      integer, parameter :: IMO4              =  15          ! type of output file
      integer, parameter :: IHI3              =  16          ! type of output file
      integer, parameter :: IHI4              =  17          ! type of output file
      integer, parameter :: IHN3              =  18          ! type of output file
      integer, parameter :: IHN4              =  19          ! type of output file
      integer, parameter :: IBA2              =  20          ! type of output file
      integer, parameter :: IBA3              =  21          ! type of output file
C
C          this is the collection of the output pointers
C
      type OutputColl
         character(LEN=NAME_SIZE),pointer :: names(:)           ! names of variables
         integer, pointer                 :: pointers(:)        ! ponters in waq arrays
         integer                          :: cursize            ! filled up to this size
      end type OutputColl
C
      end module Output
