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

      subroutine dlwq17 ( bset   , bsave  , ibpnt  , nobnd  , nosys  ,
     &                    notot  , idt    , conc   , flow   , bound  )

!     Deltares - Delft Software Department

!>\file
!>            Makes the Tatcher-Harleman boundary conditions
!>
!>            This routine performs per open boundary condition:
!>            - at outflow, updates last saved outflow concentration bsave
!>            - at outflow, sets open boundary condition to this outflow value
!>            - at inflow, set open boundary condition to:
!>                  - prescribed value if inflow time larger then time lag
!>                  - evaluates Tatcher-Harleman boundary if inflow time is less

!     Created     : April      4, 1988 by Leo Postma
!     Modified    : September 12, 2012 by Leo Postma: Fortran 90 look and feel

!     File-IO     : none

!     Subroutines : none

      use timers
      implicit none

!     Arguments

!     kind        function         name                    description
      integer(4), intent(in   ) :: nosys                 !< number of transported substances
      integer(4), intent(in   ) :: notot                 !< total number of substances
      integer(4), intent(in   ) :: nobnd                 !< number of open boundary conditions
      real   (4), intent(in   ) :: bset (nosys,nobnd)    !< prescribed open boundary conditions
      real   (4), intent(inout) :: bsave(nosys,nobnd)    !< saved open boundaries at outflow
      integer(4), intent(inout) :: ibpnt(  4  ,nobnd)    !< 1 = timelags /n
                                                         !  2 = flow pointer (can be negative) /n
                                                         !  3 = segment pointer /n
                                                         !  4 = time on the cosine /n
      integer(4), intent(in   ) :: idt                   !< time step size in system clock units
      real   (4), intent(in   ) :: conc (notot,  *  )    !< model concentrations
      real   (4), intent(in   ) :: flow (  *  )          !< model flows
      real   (4), intent(  out) :: bound(nosys,nobnd)    !< model open boundary conditions

!     Locals

      real   (4), parameter :: pi = 3.141593
      integer(4) ibnd             !  loop variable boundaries
      integer(4) itlag            !  time lag for this boundary
      integer(4) iflow            !  flow number of this boundary (positive if towards boundary)
      real   (4) aflow            !  flow accross boundary, positive is 'out'
      real   (4) at               !  Tatcher Harleman half cosine value
      integer(4) iseg             !  active volume number associated with the boundary
      integer(4) ibtime           !  time since last outflow at this boundary
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq17", ithandl )

      do ibnd = 1, nobnd
         itlag = ibpnt( 1, ibnd )
         if ( itlag .eq. 0 ) then                     !  time lag not used for this boundary
            bound( :, ibnd ) = bset( :, ibnd )
            cycle
         endif
         iflow = ibpnt( 2, ibnd )
         if ( iflow .eq. 0 ) then                     !  no flow associated with this boundary
            bound( :, ibnd ) = bset( :, ibnd )
            cycle
         endif
         aflow = isign(1,iflow)*flow(iabs(iflow))
         if ( aflow .ge. 0.0 ) then                   !  outflow
            ibpnt( 4, ibnd ) = 0
            iseg = ibpnt( 3, ibnd )
            bsave( :, ibnd ) = conc( 1:nosys, iseg )
            bound( :, ibnd ) = conc( 1:nosys, iseg )
         else                                         !  inflow
            ibtime = ibpnt( 4, ibnd ) + idt
            ibpnt( 4, ibnd ) = ibtime
            if ( ibtime  .ge. itlag ) then
               bound( :, ibnd ) = bset( :, ibnd )
            else
               at = 0.5 * cos( float(ibtime)/itlag * pi )
               bound( :, ibnd) = (0.5-at)*bset( :, ibnd) + (0.5+at)*bsave( :, ibnd)
            endif
         endif
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end
