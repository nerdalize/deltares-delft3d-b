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

      subroutine chknmr ( lunrep , mypart , noseg  , owners  , iknmrk )
C
C     Deltares
C
C     Created             : Oct. 2007 by Alja Vrieling
C
C     Function            : Changes kenmerk array
C
C     Subroutines called  : None
C
C
C     Declaration of arguments
C
      use timers
      implicit none
      integer, intent(in)    :: lunrep         ! unit number of output file
      integer, intent(in)    :: mypart         ! identification number of processor
      integer, intent(in)    :: noseg          ! number of segments
      integer, intent(in)    :: owners(noseg)  ! ownership array

      integer, intent(inout) :: iknmrk(noseg)  ! property array
C
C     Local variables :
C
      integer :: iseg                ! segment index
      integer :: var1,var2,var3,var4 ! 1st, 2nd, 3rd and 4th number of feature
      integer :: icount              ! counts the number of changes
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "chknmr", ithandl )
C
C     on input, the kenmerk-array contains two digits:
C        1*var1 + 10*var2
C     where var1 describes whether a segment is active or not, and var2
C     describes its location with respect to the vertical (top segment,
C     middle, bottom, whole column).
C
C     on output, the kenmerk-array contains four digits:
C        1*var1 + 10*var2 + 100*var3 + 1000*var4
C     Now var1 indicates whether a segment is active AND belongs to the
C     current subdomain. Var2 is left untouched. Var3 contains the initial
C     value of var1. And var4 shows whether the segment belongs to the current
C     subdomain or not.
C
      icount = 0
      do iseg = 1, noseg
         if (owners(iseg).ne.mypart) then
C
C           Segment does not belong to current subdomain
C            - reset 1st digit
C            - maintain 2nd digit
C            - copy original 1st digit to 3rd digit
C            - set 4th digit to 0
C
            var1 = 0
            var2 = mod((iknmrk(iseg)/10),10)
            var3 = mod( iknmrk(iseg)    ,10)
            var4 = 0
            iknmrk(iseg) = var1 + 10*var2 + 100*var3 + 1000*var4
            icount = icount + 1
         else
C
C           Segment belongs to current subdomain
C            - maintain 1st digit
C            - maintain 2nd digit
C            - copy original 1st digit to 3rd digit
C            - set 4th digit to 1
C
            var1 = mod( iknmrk(iseg)    ,10)
            var2 = mod((iknmrk(iseg)/10),10)
            var3 = mod( iknmrk(iseg)    ,10)
            var4 = 1
            iknmrk(iseg) = var1 + 10*var2 + 100*var3 + 1000*var4
         end if
      end do
      if ( icount .gt. 0 ) then
         write(lunrep ,*) 'number of changes in feature array', icount
      endif
C
      if ( timon ) call timstop ( ithandl )
      return
      end
