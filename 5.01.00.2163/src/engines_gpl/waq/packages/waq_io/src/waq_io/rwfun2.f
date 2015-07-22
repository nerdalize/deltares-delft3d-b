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

      subroutine rwfun2 ( iopt   , nhar   , ndim1  , ndim2  , item   ,
     &                    values , period , phase  , ifact  , dtflg  ,
     &                    iwidth , lununf , dtflg3 , vrsion , ioutpt ,
     &                    vrswrk , ierr   )

!     Deltares Software Centre

!>\file
!>               read harmonic and fourier functions old style

!     global declarations

      use rd_token     !   for the reading of tokens
      use dlwq_data
      use timers       !   performance timers

      implicit none

!     declaration of the arguments

      integer               , intent(in   ) :: iopt                       !< function option
      integer               , intent(in   ) :: nhar                       !< number of harmonics
      integer               , intent(in   ) :: ndim1                      !< first dimension (items)
      integer               , intent(in   ) :: ndim2                      !< second dimension (values)
      integer               , intent(in   ) :: item  (ndim1)              !< item numbers
      real                  , intent(  out) :: values(ndim1,ndim2,nhar+1) !< average and amplitudes
      integer               , intent(  out) :: period(nhar+1)             !< periods
      real                  , intent(  out) :: phase (nhar+1)             !< phase
      integer               , intent(in   ) :: ifact                      !< factor between timescales
      logical               , intent(in   ) :: dtflg                      !< option "date"-format
      integer               , intent(in   ) :: iwidth                     !< width of output file
      integer               , intent(in   ) :: lununf                     !< unit number unformatted work file
      logical               , intent(in   ) :: dtflg3                     !< 'date'-format (F;ddmmhhss,T;yydddhh)
      real                  , intent(in   ) :: vrsion                     !< program version number
      integer               , intent(in   ) :: ioutpt                     !< output file option
      integer               , intent(in   ) :: vrswrk                     !< version number unformatted
      integer               , intent(inout) :: ierr                       !< Cumulative error count

!     local declarations

      integer                               :: ibase                      ! base period for fourier function
      character(len=1)                      :: cdummy                     ! dummy
      integer                               :: idummy                     ! dummy
      real                                  :: adummy                     ! dummy
      integer                               :: itype                      ! token type asked / received
      integer                               :: ierr2                      ! local error indicatior
      integer                               :: i , i1, i2, k              ! loop counters
      integer                               :: ib, ie                     ! indexes in print statement
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "rwfun2", ithndl )

      !  read values

      if ( iopt .eq. FUNCTYPE_HARMONIC ) then

         ! first record average value

         do i2 = 1, ndim2
            do i1 = 1, ndim1
               if ( gettoken( values(i1,i2,1), ierr2 ) .ne. 0 ) goto 190
            enddo
         enddo

         ! harmonics

         if ( nhar .gt. 0 ) then
            do i = 2, nhar+1
               if ( gettoken( period(i), ierr2 ) .ne. 0 ) goto 190
               if ( gettoken( phase (i), ierr2 ) .ne. 0 ) goto 190
               do i2 = 1, ndim2
                  do i1 = 1, ndim1
                     if ( gettoken( values(i1,i2,i), ierr2 ) .ne. 0 ) goto 190
                  enddo
               enddo
            enddo
            call conver ( period(2) , nhar  , ifact  , dtflg , dtflg3 )
         endif
         phase(1) = float(nhar)

      else

         ! read values if IOPT = 4 ( fourier function )

         if ( gettoken( ibase, ierr2 ) .ne. 0 ) goto 190

         ! first record average value

         do i2 = 1, ndim2
            do i1 = 1, ndim1
               if ( gettoken( values(i1,i2,1), ierr2 ) .ne. 0 ) goto 190
            enddo
         enddo

         ! harmonics

         if ( nhar .gt. 0 ) then
            do i = 2, nhar+1
               if ( gettoken( phase (i), ierr2 ) .ne. 0 ) goto 190
               do i2 = 1, ndim2
                  do i1=1, ndim1
                     if ( gettoken( values(i1,i2,i), ierr2 ) .ne. 0 ) goto 190
                  enddo
               enddo
            enddo
         endif

         ! set period from base period

         call cnvtim ( ibase , ifact , dtflg , dtflg3 )
         phase(1) = float(nhar)
         do i = 2, nhar+1
            period(i)  = ibase/(i-1)
         enddo

      endif

      ! control writing

      if ( ioutpt .lt. 4 ) write ( lunut , 2090 )
      if ( iopt .eq. 3 .and. ioutpt .ge. 4 ) then
         write ( lunut, 2000 ) nhar
      else
         write ( lunut, 2010 ) nhar, ibase
      endif
      if ( ioutpt .ge. 4 ) then
         write ( lunut, 2020 )
         do i2 = 1,ndim2,iwidth
            write ( lunut, 2030 ) (k,k=i2,min(i2+iwidth-1,ndim2))
            do i1 = 1,ndim1
               ib = i2
               ie = min(i2+iwidth-1,ndim2)
               write ( lunut, 2040 ) item(i1),(values(i1,k,1),k=ib,ie)
            enddo
         enddo
         do i  = 2,nhar+1
            write ( lunut, 2050 ) period(i),phase(i)
            if ( period(i) .le. 0 ) then
               write ( lunut, 2060 )
               ierr = ierr+1
            endif
            do i2 = 1,ndim2,iwidth
               write ( lunut, 2030 ) (k,k=i2,min(i2+iwidth-1,ndim2))
               do i1 = 1,ndim1
                  ib = i2
                  ie = min(i2+iwidth-1,ndim2)
                  write ( lunut, 2040 ) item(i1),(values(i1,k,i),k=ib,ie)
               enddo
            enddo
         enddo
      endif

      ! write to work file if appropiate

      period(1)  = ndim1*ndim2
      if ( vrswrk .lt. 4.91 ) write ( lununf ) nhar+1
      if ( vrswrk .lt. 4.81 ) then
         do i = 1,nhar+1
            write ( lununf ) period(i),((values(i1,i2,i),i1=1,ndim1),i2=1,ndim2)
         enddo
      endif

      if (timon) call timstop( ithndl )
      return

      ! error handling

  190 continue
      ierr = ierr + 1
      if (timon) call timstop( ithndl )
      return

!        output formats

 2000 format( /,' Number of harmonics:',I4)
 2010 format( /,' Number of Fouriers:',I4,' base period:',I10)
 2020 format(   ' Mean values :')
 2030 format(   '      Item',I8,9I12 )
 2040 format(           I10,2X,1P,10E12.4)
 2050 format(   ' Period:',I10,'   Phase:',1P,E12.4)
 2060 format(   ' ERROR, PERIOD is less or equal to zero!')
 2090 format(   ' Printed output for output option 4 and higher !')

      end
