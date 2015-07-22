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

      subroutine unlock(lunrep,l3dmod,nolic)

!     Deltares Software Centre

!>\File
!>        Write intro to screen and report file

      ! Declaration of arguments

      use timers
      integer      , intent(in   ) :: lunrep   !< Unit number report file
      logical      , intent(in   ) :: l3dmod   !< Check 3D feature or not
      logical      , intent(  out) :: nolic    !< No license found

      ! local declarations

      save

      character*20  rundat
      character*120 idstr
      character*75  opkom(11)
      logical       first
      character*10  verwaq
      character*3   os

      data     first /.true./
      data     opkom  /
     +'ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»',
     +'ºÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛº',
     +'ºÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ  D e l f t 3 D - W A Q  ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛº',
     +'ºÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛº',
     +'º Version xx.xxxx  xx-xx-xxxx                                           º',
     +'º Deltares, 1993-2011, 2012                                             º',
     +'º p.o. box 177, 2600 MH Delft, The Netherlands, tel +31 88 3358273      º',
     +'º     Design              : L. Postma                                   º',
     +'º     Technical support   : A. Markus                                   º',
     +'º     Updates and manuals : delft3d.support@deltares.nl                 º',
     +'ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼'/
      integer(4) ithndl /0/
      if ( timon ) call timstrt( "unlock", ithndl )

      nolic =.false.
      if ( first ) then
         first = .false.

         ! set idstr

         call getidentification(idstr, verwaq)

         call getenv('OS',os)
         call dhucas(os,os,len(os))

         write(opkom(5)(3:72),'(a)') idstr(5:74)
         do i = 1 , 11
            if ( os .ne. 'WIN' ) then
               do j = 1,len(opkom(i))
                  if ( ichar(opkom(i)(j:j)) > 127 ) opkom(i)(j:j) = '-'
               enddo
            endif
            write(*,*) opkom(i)
         enddo
         write (lunrep,'(1x,a)') trim(idstr(5:))
         call dattim(rundat)
         write (lunrep,'(2a)') ' Execution start: ',rundat
      endif

      if ( timon ) call timstop( ithndl )
      return
      end
