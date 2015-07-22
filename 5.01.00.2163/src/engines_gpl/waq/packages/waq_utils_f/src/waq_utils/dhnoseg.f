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

      subroutine dhnoseg( pnoseg )
c
c     Deltares
c
c     created             : nov 07 by jan van beek
c
c     function            : get noseg from /sysn/ common , system characteristics
c
c     logical unitnumbers : -
c
c     subroutines called  : -
c
c     parameters          : -
c
c     name    kind     length     funct.  description
c     ----    -----    ------     ------- -----------
c     pnoseg  integer       1     output  copy of the noseg from sysn
c
c     declarations
c
      integer       pnoseg
c
c     common  /  sysn   /   system characteristics
c
      include 'sysn.inc'

      pnoseg = noseg

      return
      end
