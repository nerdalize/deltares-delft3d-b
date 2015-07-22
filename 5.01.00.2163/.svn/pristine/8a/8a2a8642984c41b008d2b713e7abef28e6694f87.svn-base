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

      real(8) function sdot (n,sx,incx,sy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      use timers

 !    real(8) sx(1),sy(1),stemp
 !    integer i,incx,incy,ix,iy,m,mp1,n
      real(8) sx(n),sy(n)
      integer n
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "sdot", ithandl )
      sdot = sum ( sx*sy )
c
 !    stemp = 0.0e0
 !    sdot  = 0.0e0
 !    if(n.le.0)return
 !    if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
 !    ix = 1
 !    iy = 1
 !    if(incx.lt.0)ix = (-n+1)*incx + 1
 !    if(incy.lt.0)iy = (-n+1)*incy + 1
 !    do 10 i = 1,n
 !      stemp = stemp + sx(ix)*sy(iy)
 !      ix = ix + incx
 !      iy = iy + incy
 ! 10 continue
 !    sdot = stemp
 !    goto 9999  !   return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
 ! 20 m = mod(n,5)
 !    if( m .eq. 0 ) go to 40
 !    do 30 i = 1,m
 !      stemp = stemp + sx(i)*sy(i)
 ! 30 continue
 !    if( n .lt. 5 ) go to 60
 ! 40 mp1 = m + 1
 !    do 50 i = mp1,n,5
 !      stemp = stemp + sx(i)*sy(i) + sx(i + 1)*sy(i + 1) +
 !   *   sx(i + 2)*sy(i + 2) + sx(i + 3)*sy(i + 3) + sx(i + 4)*sy(i + 4)
 ! 50 continue
 ! 60 sdot = stemp
 9999 if ( timon ) call timstop ( ithandl )
      return
      end
