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

      subroutine partb(bxx,n1,n2,m,irow,jcol,aij,x)
      implicit real*8 (a-h,o-z)
      dimension bxx(1),irow(1),jcol(1),aij(1),x(1)
      do 10 i=1,m
      bxx(i) =0.0d+0
10    continue
      do 20 j =n1,n2
      jj = irow(j)
      bxx(jj) = bxx(jj) + x(jcol(j)) * aij(j)
20    continue
      return
      end
