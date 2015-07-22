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

c
c     HASP-3 and MemoHASP interface for Sobek
c     for F77L-EM/32 LAHEY FORTRAN 77 Version 5.01 under 386 Pharlap
c     Dos Extender.
c
      subroutine dhhasp
c
c     A list of the Hasp services.
      integer    ISHASP,
     &           KETCOD,
     &           KETSTA,
     &           MEMORD,
     &           LENGTH
c
      parameter (ISHASP        = 1)
      parameter (KETCOD        = 2)
      parameter (KETSTA        = 5)
      parameter (MEMORD        = 50)
      parameter (LENGTH        = 24)

      integer*2    RetPar(LENGTH)
      integer*4    Seed, Servic, LptNum, Passw1, Passw2
      integer*4    p1, p2, p3 ,p4
      integer*2    i2, th, i  ,l1 ,l2
      integer*1    i1(2)
      equivalence (i1,i2)
      character*48 txt
      character*52 mes(3)
      character*4  pas,head,tail
      character*17 headf,tailf
      data         pas         /'1000'/
      data         headf,tailf /'.................' ,
     &                          '.................' /
      data         mes         /
     &     'HxAxSxPx xnxoxtx xfxoxuxnxdx x x x x x x x x x x x x',
     &     'ixlxlxexgxaxlx xHxAxSxPx x x x x x x x x x x x x x x',
     &     'Nxox xDxExLxWxAxQx xaxuxtxhxoxrxixzxaxtxixoxnx x x x'/
c
c     Set Passwords
c
      read(pas,'(i4)') th
      Passw1 = 2*th-623
      Passw2 = 16*th-301
c
c     LPTNUM=0  (Do an automatic search at all ports)
c
      LptNum = 0
c
c     Check if hasp is connected
c
      Servic = ISHASP
      call hasp(Servic,Seed,LptNum,Passw1,Passw2,p1,p2,p3,p4)
      if (p1 .ne. 1) then
        nr=1
        goto 100
      endif
c
c     Get HASP status
c
      Servic = KETSTA
      call hasp(Servic,Seed,LptNum,Passw1,Passw2,p1,p2,p3,p4)
      if (p2.ne.1) then
        nr=2
        goto 100
      endif
c
c     Get HASP code
c
      Seed=100
      Servic = KETCOD
      call hasp(Servic,Seed,LptNum,Passw1,Passw2,p1,p2,p3,p4)
      if (p1.ne. 33430 .or. p2.ne. 35505 .or.
     &    p3.ne.  4371 .or. p4.ne. 10347)  then
        nr=2
        goto 100
      endif
c
c Read block from HASP memory.
c
c Clear buffer.
c
      do 10 i=1,LENGTH
         RetPar(i) = 0
 10   continue
c
      return

 100  continue
      call getmlu(imlun)
      if ( imlun .ne. 0 ) then
         write (imlun,'(1x,26a1)') (mes(nr)(i:i),i=1,52,2)
      else
         write (*,'(1x,26a1)') (mes(nr)(i:i),i=1,52,2)
      endif
c
      call srstop(1)
      end
