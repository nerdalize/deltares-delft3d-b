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

      subroutine read_opt3 ( lun     , lchar   , ndim1   , ndim2p  , ndim3   ,
     &                       ifact   , dtflg   , iwidth  , dtflg3  , vrsion  ,
     &                       ioutpt  , subject , datacoll, ierr    )

!     Deltares Software Centre

!>\File
!>               Reads time dependent variables, old style in new style work file

!     Global declarations

      use rd_token       ! for the reading of tokens
      use dlwq_data      ! for definition and storage of data
      use timers         ! performance timers
      implicit none

!     declaration of arguments

      integer               , intent(in   ) :: lun(*)       !< unit numbers used
      character(len=*)      , intent(inout) :: lchar(*)     !< filenames
      integer               , intent(in   ) :: ndim1        !< first dimension (items)
      integer               , intent(in   ) :: ndim2p       !< second dimension (values)
      integer               , intent(in   ) :: ndim3        !< number of scale factors
      integer               , intent(in   ) :: ifact        !< factor between timescales
      logical               , intent(in   ) :: dtflg        !< option "date"-format
      integer               , intent(in   ) :: iwidth       !< width of output file
      logical               , intent(in   ) :: dtflg3       !< 'date'-format (F;ddmmhhss,T;yydddhh)
      real                  , intent(in   ) :: vrsion       !< program version number
      integer               , intent(in   ) :: ioutpt       !< output file option
      integer               , intent(in   ) :: subject      !< subject of input(boundary,waste,function..)
      type(t_dlwqdatacoll)  , intent(inout) :: datacoll     !< all the data blocks from file
      integer               , intent(inout) :: ierr         !< Cumulative error count

!     local declarations

      type(t_dlwqdata)                      :: dlwqdata     ! one data block
      integer                               :: iopt3        ! function type option
      integer                               :: ndim2        ! local copy of ndim2p, not zero
      integer                               :: nvarnw       ! nr of items in specific block
      integer                               :: ndim1_left   ! nr of items left to read
      integer                               :: nobrk        ! nr of breakpoint, harmonics + 1
      integer                               :: nhar         ! nr of harmonics
      integer                               :: ntotal       ! number of items read
      integer                               :: ntt          ! number of values to be read
      integer               , pointer       :: ipnt(:)      ! item pointers, either location or parameter in one block
      integer               , pointer       :: ipnt_all(:)  ! item pointers, either location or parameter all
      integer               , pointer       :: times(:)     ! times / periods
      real                  , pointer       :: scale(:)     ! scale factors
      real                  , pointer       :: phase(:)     ! phase
      real                  , pointer       :: values(:,:,:)! avregae and amplitudes
      character(len=255)                    :: cdummy       ! dummy, but used somewhere with length 255 !!!
      real                                  :: adummy       ! dummy
      real                                  :: vrswrk       ! version wrk files
      integer                               :: idummy       ! dummy
      integer                               :: itype        ! type to be read
      integer                               :: i, i2        ! loop index
      integer                               :: idata        ! index
      integer                               :: lunuit       ! dummy unit number
      logical                               :: found        ! then found
      integer                               :: ierr2        ! local error
      integer                               :: ierr_alloc   ! local error allocating
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "read_opt3", ithndl )

      ! some help variables

      ndim2  = max(1,ndim2p)
      ntotal = 0
      lunut  = lun(29)
      allocate(ipnt_all(ndim1))

      write ( lunut , 2000 )

      ! read data till every item is read

      do while ( ntotal .lt. ndim1 )

         ! read function type

         if ( gettoken( iopt3, ierr2 ) .ne. 0 ) goto 105
         write ( lunut , 2010 ) iopt3
         if ( iopt3 .lt. 1 .or. iopt3 .gt. 4 ) then
            write ( lunut , 2020 )
            goto 105
         endif

         ! read item pointer for next data block

         ndim1_left = ndim1 - ntotal
         call rdpoin ( ndim1_left, iopt3  , ioutpt , ipnt_all(ntotal+1:ndim1), nvarnw,
     &                 ierr      )
         allocate(ipnt(nvarnw))
         ipnt = abs(ipnt_all(ntotal+1:))

         if ( iopt3 .eq. FUNCTYPE_BLOCK .or. iopt3 .eq. FUNCTYPE_LINEAR ) then

            ! read time-dependent items on breakpoints

            if ( gettoken( nobrk, ierr2 ) .ne. 0 ) goto 105
            write( lunut, 2030 ) nobrk

            ! read the scale factors

            allocate(scale(max(ndim3,ndim2)))
            do i = 1, ndim3
               if ( gettoken( scale(i), ierr2 ) .ne. 0 ) goto 105
            enddo

            ! read the data itself

            ntt = nvarnw*ndim2
            allocate(times(nobrk))
            allocate(values(ndim1,ndim2,nobrk))
            call fmread ( nvarnw , ipnt   , ndim2  , ndim3  , scale  ,
     &                    nobrk  , times  , values , dtflg  , dtflg3 ,
     &                    ifact  , iwidth , ioutpt , ierr   )
            deallocate(scale)

         elseif ( iopt3 .eq. FUNCTYPE_HARMONIC .or. iopt3 .eq. FUNCTYPE_FOURIER ) then

            ! Read items as functions

            write ( lunut , 2050 )

            ! nr harmonics

            if ( gettoken( nhar, ierr2 ) .ne. 0 ) goto 105

            nobrk = nhar + 1
            allocate(times(nobrk),phase(nobrk),values(nvarnw,ndim2,nobrk),stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then
               write(lunut,2130) ierr_alloc,nvarnw,ndim2,nhar
               call srstop(1)
            endif

            vrswrk = 5.0
            call rwfun2 ( iopt3  , nhar   , nvarnw , ndim2  , ipnt   ,
     &                    values , times  , phase  , ifact  , dtflg  ,
     &                    iwidth , lunuit , dtflg3 , vrsion , ioutpt ,
     &                    vrswrk , ierr   )

         endif

         dlwqdata%functype     = iopt3
         dlwqdata%subject      = subject
         dlwqdata%igrid        = 1
         dlwqdata%extern       = .false.
         dlwqdata%filetype     = FILE_NONE
         dlwqdata%filename     = ' '
         dlwqdata%lun          = 0
         dlwqdata%param_named  = .false.
         dlwqdata%loc_named    = .false.
         dlwqdata%loc_defaults = .false.
         dlwqdata%scaled       = .false.
         dlwqdata%param_scaled = .false.
         dlwqdata%loc_scaled   = .false.
         if ( subject .eq. SUBJECT_BOUNDARY     .or.
     &        subject .eq. SUBJECT_WASTE            ) then
            dlwqdata%iorder          = ORDER_PARAM_LOC
            dlwqdata%no_loc          = nvarnw
            dlwqdata%loc_pointered   = .true.
            dlwqdata%loc_pointers    => ipnt
            dlwqdata%no_param        = ndim2
            dlwqdata%param_pointered = .false.
         elseif ( subject .eq. SUBJECT_FUNCTION .or.
     &            subject .eq. SUBJECT_SEGFUNC      ) then
            dlwqdata%iorder          = ORDER_PARAM_LOC
            dlwqdata%no_param        = nvarnw
            dlwqdata%param_pointered = .true.
            dlwqdata%param_pointers  => ipnt
            dlwqdata%no_loc          = ndim2
            dlwqdata%loc_pointered   = .false.
         else
            write(lunut,*) ' ERROR: unexpected data item for new file processing'
            write(lunut,*) ' subject type:',dlwqdata%subject
            write(  *  ,*) ' ERROR: unexpected data item for new file processing'
            call srstop(1)
         endif

         dlwqdata%no_brk =  nobrk
         dlwqdata%times  => times ; nullify(times)
         dlwqdata%phase  => phase ; nullify(phase)
         dlwqdata%values => values; nullify(values)
         nullify(ipnt)

         ! add data block to collection

         idata = dlwqdataCollAdd( datacoll, dlwqdata )

         ! Check for completion of input

         ntotal = ntotal + nvarnw

      enddo

      if ( ntotal .gt. ndim1 ) then
           write ( lunut , 2060 ) ntotal , ndim1
           ierr = ierr + 1
      endif

      ! check complete pointer structure

      do i = 1,ndim1
         found = .false.
         do i2 = 1, ndim1
            if ( abs(ipnt_all(i2)) .eq. i ) then
               if ( found ) then
                  write ( lunut , 2070 ) i
                  ierr = ierr+1
               else
                  found = .true.
               endif
            endif
         enddo
         if ( .not. found ) then
            write ( lunut , 2080 ) i
            ierr = ierr+1
         endif
      enddo

      if (timon) call timstop( ithndl )
      return

  105 ierr = ierr+1
      if (timon) call timstop( ithndl )
      return

!       Output formats

 2000 format (    ' Time variable data.')
 2010 format (  /,' Option selected : ',I2 )
 2020 format (  /,' ERROR, option not implemented')
 2030 format (   ' Number of breakpoints:',I4 )
 2050 format (    ' Block with periodic functions.')
 2060 format (    ' ERROR, too many (',I5,') items, ',I5,' expected!')
 2070 format (    ' ERROR, duplicate item:',I5)
 2080 format (    ' ERROR, non-initialised item:',I5)
 2130 format(   ' ERROR: allocating work arrays for harmonics:',I10,
     &        /,' number of items    :',I10,
     &        /,' number of values   :',I10,
     &        /,' number of harmonics:',I10)

      end
