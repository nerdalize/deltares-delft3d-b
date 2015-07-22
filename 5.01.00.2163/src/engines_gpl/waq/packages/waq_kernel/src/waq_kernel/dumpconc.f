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

      subroutine dumpconc(lunrep, msg   , rarray  , ndim1 , ndim2 ,
     +                    iprin1 )
c
c     Purpose: rarray is a 2D array with size (ndim1,ndim2);
c              print row i1=iprin1, for all i2=1:ndim2
c
c              typically, ndim1 is 1 or noflux or notot
c                         ndim2 is noseg or noq
c                         iprin1 is 1 or one of [1:notot]
c
c-----subroutine parameters
c
      use timers
      implicit none
      integer          :: lunrep, ndim1, iprin1, ndim2
      real             :: rarray(ndim1,ndim2)
      character(len=*) :: msg
c-----local variables
      integer   :: iseg
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dumpconc", ithandl )

      write(lunrep,'(2a)') 'vt-conc: ',trim(msg)
      do iseg = 1, ndim2
         write(lunrep,'(a,i2,a,i7,a,g14.6)') 'vt-conc(',iprin1,',',iseg,
     +      ') = ',rarray(iprin1,iseg)
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end
