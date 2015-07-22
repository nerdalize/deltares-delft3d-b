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

      subroutine dryfld ( nosegw , noseg  , nolay  , volume , noq12  ,                &
     &                    area   , nocons , coname , cons   , nopa   ,                &
     &                    paname , param  , nosfun , sfname , segfun ,                &
     &                    iknmrk , iknmkv )

!     Deltares Software Centre

!>\File Sets feature of dry cells to zero
!>
!>      Determines which cells were dry at start of time step.
!>      This is an explicit setting of the feature in the
!>      sense that it gives the state of the start of the time step.\n
!>      The DRY_THRESH variable is used as a thickness (default 1.0 mm) together with
!>      the SURF parameter of segment function. If SURF is absent, 1.0 m2 is
!>      assumed and the DRY_THRESH directly compares vomes in m3.\n
!>      A dry cell may have transport, because it may be wet at the
!>      end of the time step. It has however no processes yet. The wetting within the
!>      time step is tested by the dryfle routine later in this file.

!     Created             : September 2010 by Leo Postma
!     Modified            : August    2011 by Leo Postma test the volume of the whole column

!     Files               : none

!     Routines            : zoek20  - to search the DRY_THRESH constant
!                                     and SURF parameter/segfunction
!                           dhkmst  - to set features
!                           dhkmrk  - to get features

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosegw               !< number of computational volumes water
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes total
      integer  ( 4), intent(in   ) :: nolay                !< number of layers
      real     ( 4), intent(inout) :: volume(noseg )       !< volumes at start of time step
      integer  ( 4), intent(in   ) :: noq12                !< number of horizontal exchanges
      real     ( 4), intent(inout) :: area  (noq12 )       !< areas at start of time step
      integer  ( 4), intent(in   ) :: nocons               !< number of constants
      character(20), intent(in   ) :: coname(nocons)       !< names of the constants
      real     ( 4), intent(in   ) :: cons  (nocons)       !< values of the constants
      integer  ( 4), intent(in   ) :: nopa                 !< number of parameters
      character(20), intent(in   ) :: paname(nopa  )       !< names of the parameters
      real     ( 4), intent(in   ) :: param (nopa ,noseg ) !< values of the parametrs
      integer  ( 4), intent(in   ) :: nosfun               !< number of segment functions
      character(20), intent(in   ) :: sfname(nosfun)       !< names of the segment functions
      real     ( 4), intent(in   ) :: segfun(noseg,nosfun) !< values of the constants
      integer  ( 4), intent(in   ) :: iknmrk(noseg )       !< constant feature array
      integer  ( 4), intent(  out) :: iknmkv(noseg )       !< time varying feature array

      integer  ( 4)    idryfld         ! help variable to find dry_tresh constant
      integer  ( 4)    isurf           ! index to find horizontal surface area values
      real     ( 4)    threshold       ! drying and flooding value
      integer  ( 4)    nosegl          ! number of computational volumes per layer
      integer  ( 4)    iseg            ! loop variable volumes
      integer  ( 4)    ivol            ! this computational volumes
      integer  ( 4)    isub            ! loop variable substances
      integer  ( 4)    ilay            ! loop variable layers
      integer  ( 4)    ikm             ! feature
      real     ( 4)    sum             ! help variable

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dryfld", ithandl )

!        Initialisations

      nosegl = nosegw / nolay

      call zoek20 ( 'DRY_THRESH', nocons, coname, 10, idryfld )
      call zoek20 ( 'SURF      ', nopa  , paname, 10, isurf   )

!        SURF is a parameter

      if ( isurf .gt. 0 ) then
         threshold = 0.001                                        ! default value of 1 mm
         if ( idryfld .gt. 0 ) threshold = cons(idryfld)          ! or the given value
         do iseg = 1, nosegl
            sum = 0.0
            do ilay = 1, nolay
               ivol = iseg + (ilay-1)*nosegl
               sum = sum + volume(ivol)
            enddo
            if ( sum .lt. param(isurf,iseg)*threshold ) then
               do ilay = 1, nolay
                  ivol = iseg + (ilay-1)*nosegl
                  call dhkmst(1, iknmkv(ivol), 0 )               ! zero the last bit
                  call dhkmst(2, iknmkv(ivol), 0 )               ! and the second feature
                  call dhkmst(3, iknmkv(ivol), 0 )               ! and the third feature
                  if ( volume(ivol) .lt. 1.0e-25 ) volume(ivol) = 1.0
               enddo
            else
               do ilay = 1, nolay                                ! this copies the constant
                  ivol = iseg + (ilay-1)*nosegl                  ! property in case cell had
                  iknmkv(ivol) = iknmrk(ivol)                    ! become wet again
               enddo
            endif
         enddo
      else
         call zoek20 ( 'SURF      ', nosfun, sfname, 10, isurf )

!        SURF is a spatial time function

         if ( isurf .gt. 0 ) then
            threshold = 0.001
            if ( idryfld .gt. 0 ) threshold = cons(idryfld)
            do iseg = 1, nosegl
               sum = 0.0
               do ilay = 1, nolay
                  ivol = iseg + (ilay-1)*nosegl
                  sum = sum + volume(ivol)
               enddo
               if ( sum .lt. segfun(iseg,isurf)*threshold ) then
                  do ilay = 1, nolay
                     ivol = iseg + (ilay-1)*nosegl
                     call dhkmst(1, iknmkv(ivol), 0 )                  ! zero the last bit
                     call dhkmst(2, iknmkv(ivol), 0 )                  ! and the second feature
                     call dhkmst(3, iknmkv(ivol), 0 )                  ! and the third feature
                     if ( volume(ivol) .lt. 1.0e-25 ) volume(ivol) = 1.0
                  enddo
               else
                  do ilay = 1, nolay
                     ivol = iseg + (ilay-1)*nosegl
                     iknmkv(ivol) = iknmrk(ivol)
                  enddo
               endif
            enddo
         else

!        SURF is not found, so the default value of 1 m3 is used

            threshold = 1.0
            if ( idryfld .gt. 0 ) threshold = cons(idryfld)
            do iseg = 1, nosegl
               sum = 0.0
               do ilay = 1, nolay
                  ivol = iseg + (ilay-1)*nosegl
                  sum = sum + volume(ivol)
               enddo
               if ( sum .lt. threshold ) then
                  do ilay = 1, nolay
                     ivol = iseg + (ilay-1)*nosegl
                     call dhkmst(1, iknmkv(ivol), 0 )               ! zero the last bit
                     call dhkmst(2, iknmkv(ivol), 0 )               ! and the second feature
                     call dhkmst(3, iknmkv(ivol), 0 )               ! and the third feature
                     if ( volume(ivol) .lt. 1.0e-25 ) volume(ivol) = 1.0
                  enddo
               else
                  do ilay = 1, nolay
                     ivol = iseg + (ilay-1)*nosegl
                     iknmkv(ivol) = iknmrk(ivol)
                  enddo
               endif
            enddo
         endif
      endif
      area = max( area, 1.0 )

      if ( timon ) call timstop ( ithandl )

      return
      end

      subroutine dryfle ( nosegw , noseg  , volume , nolay  , nocons ,                &
     &                    coname , cons   , nopa   , paname , param  ,                &
     &                    nosfun , sfname , segfun , iknmrk , iknmkv )

!     Deltares Software Centre

!>\File
!>               Wettens cells that became wet during the time step
!>
!>               Determines which cells have become wet during the time step.
!>               A dry cell may have transport, because it may be wet at the
!>               end of the time step. It has however no processes yet in this step.\n
!>               NB. This routine does NOT set cells dry, it only wettens any dry cells.

!     Created             : September 2010 by Leo Postma
!     Modified            : August    2011 by Leo Postma: test the volume of the water column

!     Files               : none

!     Routines            : none

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosegw               !< number of computational volumes water
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes
      real     ( 4), intent(in   ) :: volume(noseg )       !< volumes at start of time step
      integer  ( 4), intent(in   ) :: nolay                !< number of layers
      integer  ( 4), intent(in   ) :: nocons               !< number of constants
      character(20), intent(in   ) :: coname(nocons)       !< names of the constants
      real     ( 4), intent(in   ) :: cons  (nocons)       !< values of the constants
      integer  ( 4), intent(in   ) :: nopa                 !< number of parameters
      character(20), intent(in   ) :: paname(nopa  )       !< names of the parameters
      real     ( 4), intent(in   ) :: param (nopa ,noseg ) !< values of the parametrs
      integer  ( 4), intent(in   ) :: nosfun               !< number of segment functions
      character(20), intent(in   ) :: sfname(nosfun)       !< names of the segment functions
      real     ( 4), intent(in   ) :: segfun(noseg,nosfun) !< values of the constants
      integer  ( 4), intent(in   ) :: iknmrk(noseg )       !< constant feature array
      integer  ( 4), intent(inout) :: iknmkv(noseg )       !< time varying feature array

      integer  ( 4)    idryfld         ! help variable to find dry_tresh constant
      integer  ( 4)    isurf           ! index to find horizontal surface area values
      real     ( 4)    threshold       ! drying and flooding value
      integer  ( 4)    nosegl          ! number of computational volumes per layer
      integer  ( 4)    iseg            ! loop variable
      integer  ( 4)    ivol            ! this computational volume
      integer  ( 4)    ilay            ! loop variable layers
      real     ( 4)    sum             ! help variable

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dryfle", ithandl )

      nosegl = nosegw / nolay

      call zoek20 ( 'DRY_THRESH', nocons, coname, 10, idryfld )
      call zoek20 ( 'SURF      ', nopa  , paname, 10, isurf   )
      if ( isurf .gt. 0 ) then                              ! surf is parameter
         threshold = 0.001                                  ! default value of 1 mm
         if ( idryfld .gt. 0 ) threshold = cons(idryfld)    ! or the given value
         do iseg = 1, nosegl
            sum = 0.0
            do ilay = 1, nolay
               ivol = iseg + (ilay-1)*nosegl
               sum = sum + volume(ivol)
            enddo
            if ( sum .gt. param(isurf,iseg)*threshold ) then
               do ilay = 1, nolay
                  ivol = iseg + (ilay-1)*nosegl
                  iknmkv(ivol) = iknmrk(ivol)
               enddo
            endif
         enddo
      else
         call zoek20 ( 'SURF      ', nosfun, sfname, 10, isurf )
         if ( isurf .gt. 0 ) then                          ! surf is segment function
            threshold = 0.001
            if ( idryfld .gt. 0 ) threshold = cons(idryfld)
            do iseg = 1, nosegl
               sum = 0.0
               do ilay = 1, nolay
                  ivol = iseg + (ilay-1)*nosegl
                  sum = sum + volume(ivol)
               enddo
               if ( sum .gt. segfun(iseg,isurf)*threshold ) then
                  do ilay = 1, nolay
                     ivol = iseg + (ilay-1)*nosegl
                     iknmkv(ivol) = iknmrk(ivol)
                  enddo
               endif
            enddo
         else
            threshold = 1.0                                 ! no surf found 1 m3 default
            if ( idryfld .gt. 0 ) threshold = cons(idryfld)
            do iseg = 1, nosegl
               sum = 0.0
               do ilay = 1, nolay
                  ivol = iseg + (ilay-1)*nosegl
                  sum = sum + volume(ivol)
               enddo
               if ( sum .gt. threshold ) then
                  do ilay = 1, nolay
                     ivol = iseg + (ilay-1)*nosegl
                     iknmkv(ivol) = iknmrk(ivol)
                  enddo
               endif
            enddo
         endif
      endif

      if ( timon ) call timstop ( ithandl )

      return
      end
