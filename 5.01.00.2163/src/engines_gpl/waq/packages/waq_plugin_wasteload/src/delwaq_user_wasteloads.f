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

      module delwaq_user_wasteloads
      use delwaq_loads
      contains
      subroutine delwaq_user_wasteload ( nowst , wasteloads, notot , nosys , noseg ,
     +                                   itime , conc      , syname)
      !DEC$ ATTRIBUTES DLLEXPORT::delwaq_user_wasteload

      ! routine to insert user functionality to the wasteloads
      ! contains currently the inlet-outlet coupling functionality

      implicit none

      ! arguments declarations

      integer                             :: nowst                  ! number of wasteloads
      type(wasteload), pointer            :: wasteloads(:)          ! array of all wasteloads (structure)
      integer                             :: notot                  ! total number of substances
      integer                             :: nosys                  ! number of active substances
      integer                             :: noseg                  ! number of segments
      integer                             :: itime                  ! system time
      real                                :: conc(notot,noseg)      ! concentration array
      character(len=*)                    :: syname(notot)          ! substance names

      ! local declarations

      integer, save       :: ifirst = 1
      integer             :: lunrep
      integer             :: iwst
      integer             :: isys

      ! the inlet outlet coupling

      call delwaq_user_inlet_outlet ( nowst , wasteloads, notot , nosys , noseg ,
     +                                itime , conc      , syname)

      ! report on wasteloads

      call delwaq_user_bubble_bath  ( nowst , wasteloads, notot , nosys , noseg ,
     &                                itime , conc      , syname)

      ifirst = 0

      return
      end subroutine delwaq_user_wasteload

      subroutine delwaq_user_bubble_bath  ( nowst  , wls    , notot  , nosys  , noseg  ,
     &                                      itime  , conc   , syname )

!       routine to set the bubble screen option for Nieuwe Meer
!                made by Leo Postma at 6 october 2006

!       global declarations

      implicit none

!       arguments declarations

      integer                  :: nowst             ! number of wasteloads
      type(wasteload), pointer :: wls(:)            ! array of all wasteloads (structure)
      integer                  :: notot             ! total number of substances
      integer                  :: nosys             ! number of active substances
      integer                  :: noseg             ! number of segments
      integer                  :: itime             ! system time
      real                     :: conc(notot,noseg) ! concentration array
      character(len=IDLEN)     :: syname(notot)     ! substance names

!       local declarations

      logical                  :: first = .true.    ! initialisation indicator
      logical                  :: l_exi             ! file exists or not
      integer                  :: noscrn            ! number of bubble screens
      integer                  :: iscrn             ! loop counter screens
      integer                  :: iwst              ! loop counter loads
      integer                  :: isub              ! loop counter substances
      real                     :: wflow             ! flow of a load
      character(len=IDLEN), pointer :: scrnam(:)    ! array with screen names
      integer             , pointer :: scrloc(:)    ! array with screen locations
      real                , pointer :: scrwtd(:,:)  ! withdrawal mass per substance
      real                , pointer :: scrwdf(:)    ! withdrawal flow

      ! Save all local variables

      SAVE

      if ( first ) then
         first = .false.
         noscrn = 0
         inquire (file='screen.dat',exist=l_exi)
         if ( l_exi ) then
            open  ( 83 , file='screen.dat' )        !  read file with
            read  ( 83 , * ) noscrn                 !  screen-names
            write ( 32 , * ) 'Number of screens:', noscrn
            if ( noscrn .gt. 0 ) then               !  may be more names
               allocate ( scrnam(noscrn) )          !  than existing in the
               do iscrn = 1, noscrn                 !  problem
                  read  ( 83 , * ) scrnam(iscrn)
                  write ( 32 , * ) 'Screen:',iscrn,' is called: ',scrnam(iscrn)
               enddo
               close ( 83 )
               allocate ( scrloc( nowst  ) )        !  pointer from waste to screen
               allocate ( scrwtd( noscrn, notot ) )
               allocate ( scrwdf( noscrn ) )
               scrloc = 0
               do iwst = 1, nowst
                  do iscrn = 1, noscrn
                     if ( find_string( scrnam(iscrn), wls(iwst)%id%id ) ) then
                        scrloc(iwst) = iscrn
                        write ( 32 , * ) 'Load:',iwst,' is part of screen:',iscrn
                        exit
                     endif
                  enddo
               enddo
            endif
         else
            write ( 32 , * ) 'No file <screen.dat> detected'
         endif
      endif

      if ( noscrn .eq. 0 ) return

!     write ( 32 , * ) 'Time:',itime

!        First  step: sum the withdrawn masses and flow per screen

      scrwtd = 0.0                                  !  zero the withdrawal
      scrwdf = 0.0                                  !  accumulation arrays
      do iwst = 1, nowst
         iscrn = scrloc(iwst)
         if ( iscrn .ne. 0 ) then                   !  screens only
            wflow = wls(iwst)%flow
            if ( wflow .lt. 0.0 ) then              !  withdrawals only
               scrwdf( iscrn ) = scrwdf( iscrn ) + wflow
               do isub = 1, nosys
                  scrwtd ( iscrn, isub ) = scrwtd ( iscrn, isub ) +
     &                                        wflow * conc( isub, wls(iwst)%loc%segnr )
                  wls(iwst)%loads(isub ) = 0.0      !  for safety
               enddo
               do isub = nosys+1, notot
                  scrwtd ( iscrn, isub ) = 0.0
                  wls(iwst)%loads(isub ) = 0.0      !  for safety
               enddo
            endif
         endif
      enddo

!        Second step: mix the withdrawal to get concentrations

      do iscrn = 1, noscrn                          !  make the mixed
         wflow = scrwdf( iscrn )                    !  concentrations
         if ( wflow .ne. 0.0 ) then                 !  per active screen
!           write ( 32 , * ) 'Screen:',iscrn,' Abstracted:',wflow
            do isub = 1, notot
               scrwtd ( iscrn, isub ) = scrwtd ( iscrn, isub ) / wflow
            enddo
         endif
      enddo

!        Third  step: set the mixed concentrations for the releases

      do iwst = 1, nowst
         iscrn = scrloc(iwst)
         if ( iscrn .ne. 0 ) then                   !  screens only
            wflow = wls(iwst)%flow
            if ( wflow .gt. 0.0 ) then              !  releases only
!              write ( 32 , * ) 'Screen:',iscrn,' Released:',wflow
               do isub = 1, notot
                  wls(iwst)%loads(isub ) = scrwtd ( iscrn, isub )
               enddo
            endif
         endif
      enddo

!        NB: in this code it is assumed that also inactive substances are
!            withdrawn and released like active substances ( so with a flow
!            concentration that form together the mass )

      return

      end subroutine delwaq_user_bubble_bath

      subroutine delwaq_user_inlet_outlet ( nowst , wasteloads, notot , nosys , noseg ,
     +                                      itime , conc      , syname)

      ! routine to set the default inlet-outlet coupling

      ! global declarations

      implicit none

      ! arguments declarations

      integer                             :: nowst                  ! number of wasteloads
      type(wasteload), pointer            :: wasteloads(:)          ! array of all wasteloads (structure)
      integer                             :: notot                  ! total number of substances
      integer                             :: nosys                  ! number of active substances
      integer                             :: noseg                  ! number of segments
      integer                             :: itime                  ! system time
      real                                :: conc(notot,noseg)      ! concentration array
      character(len=*)                    :: syname(notot)          ! substance names

      ! local declarations

      integer, parameter  :: mxcomb = 100     ! maximum number of combinations
      integer, save       :: ifirst = 1       ! initialisation indicator
      integer             :: lunrep           ! report file
      logical             :: l_exi            ! file exists or not
      integer             :: ncomb            ! number of possible inlet outlet combinations
      integer             :: ninout           ! actual number of inlet outlet combinations
      character(len=20)   :: c_in             ! read buffer name inlet
      character(len=20)   :: c_out            ! read buffer name outlet
      character(len=20)   :: namin(mxcomb)    ! names inlet in the possible combinations
      character(len=20)   :: namout(mxcomb)   ! names outlet in the possible combinations
      integer             :: iwin(mxcomb)     ! wasteload number inlet of the actual combinations
      integer             :: iwout(mxcomb)    ! wasteload number outlet of the actual combinations
      real                :: flow             ! inlet flow rate
      integer             :: ipin             ! wasteload number inlet
      integer             :: ipout            ! wasteload number outlet
      integer             :: iseg             ! inlet segment number
      integer             :: iwst             ! loop counter wasteloads
      integer             :: isys             ! loop counter substances
      integer             :: icomb            ! loop counter combinations
      integer             :: iinout           ! loop counter of inlet outlet combinations
      integer             :: i                ! loop counter

      ! Save all local variables

      SAVE

      ! test if there are inlet outlet combinations

      lunrep = 32
      if ( ifirst .eq. 1 ) then
         ifirst = 0
         write(lunrep,*)
         write(lunrep,2000)

         ! if availeble read list of possible inlet outlet combinations

         inquire (file='inloutl.dat',exist=l_exi)
         if ( l_exi ) then
            write(lunrep,2004)
            open ( 83 , file='inloutl.dat' )
            ncomb = 0
   10       continue
               read ( 83 , '(2a20)' , end = 20 ) c_in,c_out
               ncomb = ncomb + 1
               if ( ncomb .gt. mxcomb ) then
                  write(lunrep,2005) mxcomb
                  call srstop(1)
               endif
               namin(ncomb) = c_in
               namout(ncomb) = c_out
               goto 10
   20       continue
            close ( 83 )
         else

            ! construct the default list of combination INLETxx/OUTLETxx

            write(lunrep,2006)
            do i = 1 , min(mxcomb,9)
               write(namin(i),2007) i
               write(namout(i),2008) i
            enddo
            do i = 10 , min(mxcomb,99)
               write(namin(i),2009) i
               write(namout(i),2010) i
            enddo
            do i = 100 , min(mxcomb,999)
               write(namin(i),2011) i
               write(namout(i),2012) i
            enddo
            ncomb = mxcomb
         endif

         ! check the actual list of wasteloads with the list of possible combinations

         ninout = 0
         do icomb = 1 , ncomb
            ipin  = find_wasteload( namin(icomb) , wasteloads)
            ipout = find_wasteload( namout(icomb), wasteloads)
            if ( ipin .gt. 0 .and. ipout .gt. 0 ) then

               ! a combination found, print and set administration

               write(lunrep,*)
               write(lunrep,2001) ipin,wasteloads(ipin)%id%id
               write(lunrep,2002) ipout,wasteloads(ipout)%id%id
               write(lunrep,*)
               ninout = ninout + 1
               iwin(ninout)  = ipin
               iwout(ninout) = ipout

            endif
         enddo
         if ( ninout .eq. 0 ) write(lunrep,2013)
         write(lunrep,2003)

      endif

      ! set outlet equal to inlet flow * concentration at inlet segment for all found combinations

      do iinout = 1 , ninout
         ipin  = iwin(iinout)
         ipout = iwout(iinout)
         iseg  = wasteloads(ipin)%loc%segnr
         flow  = wasteloads(ipin)%flow
         wasteloads(ipout)%flow = 0.0
         do isys = 1, nosys
            wasteloads(ipout)%loads(isys) = -flow*conc(isys,iseg)
         enddo
         do isys = nosys + 1 , notot
            wasteloads(ipout)%loads(isys) = 0.0
         enddo
      enddo
c
      return
 2000 format (' extra functionality INLET/OUTLET')
 2001 format ('    waste number:',i5,' name:',a20,' (INLET) coupled to')
 2002 format ('    waste number:',i5,' name:',a20,' (OUTLET)')
 2003 format (' end extra functionality INLET/OUTLET')
 2004 format ('    possible INLET/OUTLET combinations will be read from ',
     +        'file <inloutl.dat>')
 2005 format ('    error : number of combinations exceed maximum:',i4)
 2006 format ('    no file <inloutl.dat> using default combinations names')
 2007 format ('INLET',i1,14x)
 2008 format ('OUTLET',i1,13x)
 2009 format ('INLET',i2,13x)
 2010 format ('OUTLET',i2,12x)
 2011 format ('INLET',i3,12x)
 2012 format ('OUTLET',i3,11x)
 2013 format ('    no INLET/OUTLET combination found')

      end subroutine delwaq_user_inlet_outlet

      function find_wasteload( waste_id, wasteloads) result ( iwst)

         ! function to find a wasteload on id in an array of wasteloads

         character(len=*)                    :: waste_id               ! wasteload id to be found
         type(wasteload), pointer            :: wasteloads(:)          ! array of all wasteloads (structure)
         integer                             :: iwst                   ! on return if found wasteload number, otherwise zero

         ! local declarations

         character(len=20)                   :: name                   ! fixed length copy of waste_id
         integer                             :: nowst                  ! length of wasteloads array
         integer                             :: i                      ! loop counter
         integer                             :: ifound                 ! loop counter

         ! loop over the wasteloads and compare id with delwaq routine zoekns

         nowst = size(wasteloads)
         name  = waste_id
         iwst = 0
         do i = 1 , nowst
            call zoekns(name,1,wasteloads(i)%id%id,20,ifound)
            if ( ifound .eq. 1 ) then
               iwst = i
               return
            endif
         end do

      end function find_wasteload

      function find_substance( substance_id, syname) result ( isys)

         ! function to find a substance on id

         character(len=*)                    :: substance_id           ! substance id to be found
         character(len=20)                   :: syname(:)              ! substance names
         integer                             :: isys                   ! on return if found substance number, otherwise zero

         ! local declarations

         character(len=20)                   :: name                   ! fixed length copy of waste_id
         integer                             :: notot                  ! length of syname array

         ! call the delwaq routine zoekns

         notot = size(syname)
         name  = substance_id
         call zoekns(name,notot,syname,20,isys)

      end function find_substance

      function find_string( string, test ) result ( found )

!            function to find a string in a test-string

      character(*) string
      character(*) test
      logical      found

!            local declarations

      integer      lens, lent, i

      found = .true.
      lens  = len_trim(string) - 1
      lent  = len_trim(test  )
      do i = 1, lent-lens
         if ( string .eq. test(i:i+lens) ) return
      enddo
      found = .false.

      end function find_string

      end module delwaq_user_wasteloads
