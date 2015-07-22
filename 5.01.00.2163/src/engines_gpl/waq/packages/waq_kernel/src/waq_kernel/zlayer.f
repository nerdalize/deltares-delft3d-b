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

      subroutine zlayer ( nosegw , noseg  , nosys  , notot  , nolay  ,
     &                    volume , noq12  , noq    , area   , nocons ,
     &                    coname , cons   , nopa   , paname , param  ,
     &                    nosfun , sfname , segfun , conc   , mass   ,
     &                    iknmrk , iknmkv , ifrmto )

!     Deltares Software Centre

!>\File
!>      Sets feature of dry cells below bed to zero, determines position bed layer
!>
!>      - Only works if the constant 'Z_THRESH' exists, otherwise it only:
!>        - copies the constant property array to the variable array
!>        - minimizes the area array at 1.0 m2
!>      - Only works for horizontal cells with the first layer active
!>        - so it does not override a column specified as inactive by the user
!>      - Tests the value of the volume in an active column from the bed
!>        - if too low:
!>          - it sets the property to 'inactive'
!>          - it sets the second property (surface,middle,bed) to zero
!>          - it modifies the second property of the layer above accordingly
!>          - it sets the third property (saved value of the first property) to 'inactive'
!>      - The first property of the top layer is never modified so 'active' remains 'active'
!>      - The Z_THRESH value is the thickness of the cell to decide for too low or not
!>      - This is a modification of the property array at start of simulation.\n
!>      - A tricky point is that for z-layers the from- to pointer is not
!>        adapted any more. This is because EDFs TELEMAC may produce flows below the
!>        bed, that should be taken into account for mass conservation.
!>      NB. This routine also initialises the variable property array. It does so for the
!>      whole array, so inclusive of any bed cells.

!     Created             : September 2010 by Leo Postma
!     Modified            : February  2011 by Leo Postma determine position of bedlayer
!                           August    2011 by Leo Postma more limited functionality

!     Files               : none

!     Routines            : zoek20  - to search the DRY_TRESH constant
!                                     and SURF parameter/segfunction
!                           dhkmst  - to set features
!                           dhkmrk  - to get features

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosegw               !< number of computational volumes water
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes total
      integer  ( 4), intent(in   ) :: nosys                !< number of transported substance
      integer  ( 4), intent(in   ) :: notot                !< total number of substance
      integer  ( 4), intent(in   ) :: nolay                !< number of layers
      real     ( 4), intent(in   ) :: volume(noseg )       !< volumes at start of time step
      integer  ( 4), intent(in   ) :: noq12                !< number of horizontal exchanges
      integer  ( 4), intent(in   ) :: noq                  !< total number of exchanges
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
      real     ( 4), intent(inout) :: conc  (notot,noseg ) !< model concentrations
      real     ( 4), intent(inout) :: mass  (notot,noseg ) !< model masses
      integer  ( 4), intent(inout) :: iknmrk(noseg )       !< constant feature array
      integer  ( 4), intent(  out) :: iknmkv(noseg )       !< time varying feature array
      integer  ( 4), intent(inout) :: ifrmto(4,noq )       !< exchange pointer array

      integer  ( 4)    idryfld         ! help variable to find dry_tresh constant
      integer  ( 4)    isurf           ! index to find horizontal surface area values
      real     ( 4)    threshold       ! drying and flooding value
      integer  ( 4)    nosegl          ! number of computational volumes per layer
      integer  ( 4)    iseg            ! loop variable volumes
      integer  ( 4)    iq              ! loop variable exchanges
!     integer  ( 4)    i, j            ! general loop variables
      integer  ( 4)    ivol            ! this computational volumes
      integer  ( 4)    isub            ! loop variable substances
      integer  ( 4)    ilay            ! loop variable layers
      integer  ( 4)    ikm             ! feature

      integer(4) ithandl /0/

      call zoek20 ( 'Z_THRESH  ', nocons, coname, 10, idryfld )
      if ( idryfld .le. 0 ) then                                       ! constant not found
         iknmkv = iknmrk                                               ! set variable property to
         area = max( area, 1.0 )                                       ! initial property, area at
         return                                                        ! least at 1.0 and return
      endif
      threshold = cons(idryfld)                                        ! apply the given value
                                                                       ! and proceed with z-layer
      if ( timon ) call timstrt ( "zlayer", ithandl )                  ! correction
      nosegl = nosegw / nolay
      call zoek20 ( 'SURF      ', nopa  , paname, 10, isurf   )

!        SURF is a parameter

      if ( isurf .gt. 0 ) then
         do iseg = 1, nosegl
            call dhkmrk( 1, iknmrk(iseg), ikm )
            if ( ikm .eq. 0 ) cycle
            do ilay = nolay, 1, -1                                     ! from bottom to top
               ivol = iseg + (ilay-1)*nosegl
               if ( volume(ivol) .lt. param(isurf,ivol)*threshold ) then
                  if ( ilay .gt. 1 ) then
                     call dhkmst(1, iknmrk(ivol), 0 )                  ! zero the last bit
                     call dhkmst(2, iknmrk(ivol), 0 )                  ! and the second feature
                     call dhkmst(3, iknmrk(ivol), 0 )                  ! and the third feature
                     call dhkmrk(2, iknmrk(ivol-nosegl), ikm )
                     select case ( ikm )
                        case ( 1 )                                     ! the cell on top is surface cell
                           call dhkmst(2, iknmrk(ivol-nosegl), 0 )     ! now it also has a bed
                        case ( 2 )                                     ! the cell on top is middle cell
                           call dhkmst(2, iknmrk(ivol-nosegl), 3 )     ! now it is the bed
                     end select
                     do isub = nosys+1,notot
                        conc(isub,ivol-nosegl) = conc(isub,ivol-nosegl) + conc(isub,ivol)
                        mass(isub,ivol-nosegl) = mass(isub,ivol-nosegl) + mass(isub,ivol)
                        conc(isub,ivol) = 0
                        mass(isub,ivol) = 0
                     enddo
                  endif
               else
                  exit
               endif
            enddo
         enddo
      else
         call zoek20 ( 'SURF      ', nosfun, sfname, 10, isurf )

!        SURF is a spatial time function (often with 1D models)

         if ( isurf .gt. 0 ) then
            do iseg = 1, nosegl
               call dhkmrk( 1, iknmrk(iseg), ikm )
               if ( ikm .eq. 0 ) cycle
               do ilay = nolay, 1, -1                                  ! from bottom to top
                  ivol = iseg + (ilay-1)*nosegl
                  if ( volume(ivol) .lt. segfun(ivol,isurf)*threshold ) then
                     if ( ilay .gt. 1 ) then
                        call dhkmst(1, iknmrk(ivol), 0 )               ! zero the last bit
                        call dhkmst(2, iknmrk(ivol), 0 )               ! and the second feature
                        call dhkmst(3, iknmrk(ivol), 0 )               ! and the third feature
                        call dhkmrk(2, iknmrk(ivol-nosegl), ikm )
                        select case ( ikm )
                           case ( 1 )                                  ! the cell on top is surface cell
                              call dhkmst(2, iknmrk(ivol-nosegl), 0 )  ! now it also has a bed
                           case ( 2 )                                  ! the cell on top is middle cell
                              call dhkmst(2, iknmrk(ivol-nosegl), 3 )  ! now it is the bed
                        end select
                        do isub = nosys+1,notot
                           conc(isub,ivol-nosegl) = conc(isub,ivol-nosegl) + conc(isub,ivol)
                           mass(isub,ivol-nosegl) = mass(isub,ivol-nosegl) + mass(isub,ivol)
                           conc(isub,ivol) = 0
                           mass(isub,ivol) = 0
                        enddo
                     endif
                  else
                     exit
                  endif
               enddo
            enddo
         else

!        SURF is not found, so the default value of 1 m2 is used

            do iseg = 1, nosegl
               call dhkmrk( 1, iknmrk(iseg), ikm )
               if ( ikm .eq. 0 ) cycle
               do ilay = nolay, 1, -1                             ! from bottom to top
                  ivol = iseg + (ilay-1)*nosegl
                  if ( volume(ivol) .lt. threshold ) then
                     if ( ilay .gt. 1 ) then
                        call dhkmst(1, iknmrk(ivol), 0 )            ! zero the last bit
                        call dhkmst(2, iknmrk(ivol), 0 )            ! and the second feature
                        call dhkmst(3, iknmrk(ivol), 0 )            ! and the third feature
                        call dhkmrk(2, iknmrk(ivol-nosegl), ikm )
                        select case ( ikm )
                           case ( 1 )                                  ! the cell on top is surface cell
                              call dhkmst(2, iknmrk(ivol-nosegl), 0 )  ! now it also has a bed
                           case ( 2 )                                  ! the cell on top is middle cell
                              call dhkmst(2, iknmrk(ivol-nosegl), 3 )  ! now it is the bed
                        end select
                        do isub = nosys+1,notot
                           conc(isub,ivol-nosegl) = conc(isub,ivol-nosegl) + conc(isub,ivol)
                           mass(isub,ivol-nosegl) = mass(isub,ivol-nosegl) + mass(isub,ivol)
                           conc(isub,ivol) = 0.0
                           mass(isub,ivol) = 0.0
                        enddo
                     endif
                  else
                     exit
                  endif
               enddo
            enddo
         endif
      endif

      iknmkv = iknmrk
      area = max( area, 1.0 )

!          update the vertical exchange pointer
!          this needs more sophisticated approach when atmosphere and bed
!          have been attached as open boundary conditions

 !    do iq = noq12+1, noq
 !       do i = 1, 4
 !          j = ifrmto(i,iq)
 !          if ( j .gt. 0 ) then
 !             if ( .not. btest( iknmkv(j), 0 ) ) ifrmto(i,iq) = 0
 !          endif
 !       enddo
 !    enddo

      if ( timon ) call timstop ( ithandl )

      return
      end
