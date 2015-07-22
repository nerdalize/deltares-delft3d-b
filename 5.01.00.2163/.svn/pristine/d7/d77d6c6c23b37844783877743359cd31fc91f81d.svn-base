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

      subroutine dlwqb4 ( nosys  , notot  , noseg  , volume , amass  ,
     &                    conc   , deriv  , nopa   , paname , param  ,
     &                    nosfun , sfname , segfun , idt    )

!     Deltares Software Centre

!>\File
!>           makes masses from conc (coflowing subs) and sets explicit step (passive subs)

!     Created             :    May     1992 by Jos van Gils

!     Modified            : 13 Januari 2011 by Leo Postma
!                                           2D arrays, fortran 90 look and feel
!                                           conc of passive substances in mass/m2

!     Logical unitnumbers : none

!     Subroutines called  : none

      use timers
      implicit none

!     Parameters          :
!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: nosys                   !< number of transported substances
      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer   (4), intent(in   ) :: noseg                   !< number of computational volumes
      real      (4), intent(inout) :: volume(noseg )          !< volumes of the segments
      real      (4), intent(inout) :: amass (notot ,noseg)    !< masses per substance per volume
      real      (4), intent(inout) :: conc  (notot ,noseg)    !< concentrations per substance per volume
      real      (4), intent(inout) :: deriv (notot ,noseg)    !< derivatives per substance per volume
      integer   (4), intent(in   ) :: nopa                    !< number of parameters
      character(20), intent(in   ) :: paname(nopa  )          !< names of the parameters
      real      (4), intent(in   ) :: param (nopa  ,noseg)    !< parameter values
      integer   (4), intent(in   ) :: nosfun                  !< number of segment functions
      character(20), intent(in   ) :: sfname(nosfun)          !< names of the segment functions
      real      (4), intent(in   ) :: segfun(noseg ,nosfun)   !< segment function values
      integer   (4), intent(in   ) :: idt                     !< integration time step size

!     local variables

      integer(4)          isys            ! loopcounter substances
      integer(4)          iseg            ! loopcounter computational volumes
      integer(4)          indx            ! index of the surf variable in the array
      real   (4)          surf            ! the horizontal surface area of the cell
      real   (4)          vol             ! helpvariable for this volume
      integer(4)          mode            ! -1 segment functions, +1 parameters, 0 none
      integer(4), save :: ithandl         ! timer handle
      data       ithandl /0/

      if ( timon ) call timstrt ( "dlwqb4", ithandl )

!         see if the surface is available

      if ( nosys .ne. notot ) then                         ! if there are bed-substances
         call zoek20 ( 'SURF      ', nopa  , paname , 10 , indx )
         if ( indx .gt. 0 ) then                           ! SURF is found
            mode = 1
         else
            call zoek20 ( 'SURF      ', nosfun, sfname, 10, indx )
            if ( indx .gt. 0 ) then
               mode = -1
            else
               mode = 0
            endif
         endif
      endif

!         loop accross the number of computational volumes for the concentrations

      do 10 iseg = 1, noseg

         vol = volume(iseg)

!         transported substances first

         do isys = 1, nosys
            amass(isys,iseg) = conc (isys,iseg) * vol
            deriv(isys,iseg) = 0.0
         enddo

!         then the passive substances

         if ( notot .gt. nosys ) then
            if ( mode .eq.  1 ) then
               surf = param(indx,iseg)
            elseif ( mode .eq. -1 ) then
               surf = segfun(iseg,indx)
            else
               surf = 1.0
            endif
            do isys = nosys+1, notot
               amass(isys,iseg) = amass(isys,iseg) + idt*deriv(isys,iseg)
               conc (isys,iseg) = amass(isys,iseg) / surf
               deriv(isys,iseg) = 0.0
            enddo
         endif

   10 continue

      if ( timon ) call timstop ( ithandl )
      return
      end
