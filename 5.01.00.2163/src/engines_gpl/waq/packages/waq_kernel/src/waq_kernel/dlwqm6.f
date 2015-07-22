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

      subroutine dlwqm6(notot, noseg, conc, thetaseg)

      use timers
      implicit none

      integer, intent(in)    :: notot              ! total number of substances
      integer, intent(in)    :: noseg              ! number of segments
      real,    intent(inout) :: conc(notot, noseg) ! concentrations
      real,    intent(in)    :: thetaseg(noseg)    ! variable theta coefficients per segment

      integer                :: iseg               ! current segment
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqm6", ithandl )

      do iseg=1,noseg
         conc(notot,iseg)=thetaseg(iseg)
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end subroutine dlwqm6
