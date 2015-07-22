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

      subroutine partit ( lunrep , noseg  , nolay  , noq    , ipoint ,
     +                    mypart , npart  , owners , ownerq , intsrt )
c
c     Deltares     Sector Waterresources and Environment
c
c     Created:    october 2007 by E. Vollebregt
c
c     Function            : Determines the partitioning of the Delwaq
c                           mesh/grid for parallel computing
c
C     Logical unitnumbers : lunrep - monitoring output file
c
C     Subroutines called  : SRSTOP, stops execution
c
c     Parameters          :
c
      use timers
      implicit none
      integer, intent(in)  :: lunrep        ! logical unit of stdout
      integer, intent(in)  :: noseg         ! total number of segments
      integer, intent(in)  :: nolay         ! number of layers in the water
      integer, intent(in)  :: noq           ! total number of exchanges
      integer, intent(in)  :: mypart        ! own subdomain number
      integer, intent(in)  :: npart         ! requested number of parts
      integer, intent(in)  :: ipoint(4,noq) ! exchange pointers
      integer, intent(out) :: owners(noseg) ! partitioning of segments, for
                                            ! each segment the number of the
                                            ! part to which it belongs
      integer, intent(out) :: ownerq(noq)   ! partitioning of exchanges
      integer, intent(in)  :: intsrt
c
c     Local variables     :
c
      integer, parameter :: idebug=2     ! level of debug output, 0=none
      real, parameter    :: frac1st=0.90 ! relative size of first subdomain
                                         ! when compared to other subdomains
      integer :: nseghr  ! number of segments in horizontal plane
      integer :: nseg1st ! number of segments for first subdomain
      real    :: loadpp  ! workload per part, real number
      integer :: nsegpp  ! number of segments per part (integer)
      integer :: nsegrm  ! remaining segments after equal distribution
      integer :: nassgn  ! number of segments to be assigned to a subdomain
      integer :: ilay    ! loop counter for layers
      integer :: ipart   ! loop counter for parts/subdomains
      integer :: ioffs   ! number of already assigned segments
      integer :: iq      ! loop counter for exchanges
      integer :: ifrom   ! from-segment for an exchange
      integer :: ito     ! to-segment for an exchange

      integer :: lunout  ! logical unit of output-file
      logical :: is_ok   ! flag indicating whether lunout may be used
      logical :: lopen   ! flag indicating whether lunout is open already
      character(len=40) :: dscrp_file(4)  ! description for output-file
      character(len=20) :: dscrp_sys      ! description for output-file
      integer :: itime   ! time for writing output-file

      ! Arjen
      LOGICAL            OLCFWQ, SRWACT, RTCACT, DDWAQ
      COMMON /COMMUN/    OLCFWQ, SRWACT, RTCACT, DDWAQ
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "partit", ithandl )

!     write(*,*) 'Arjen P (1): RTCACT = ', RTCACT
!     write(*,*) 'Arjen P (1): SRWACT = ', SRWACT
!     write(*,*) 'Arjen P (1): OLCFWQ = ', OLCFWQ

c
c     use trivial partitioning of the segments in the horizontal plane
c
c     compute segments per part in horizontal plane plus remaining segments
c
      nseghr = noseg / nolay
      if (mod(noseg,nolay).ne.0) then

!jvb     delwaq-g bodem moet nog over nagedacht worden
         if ( npart .ne. 1 ) then
            write(*,*) 'ERROR: the number of segments is not a multiple',
     +                 ' of the number of layers:',noseg,nolay
            call srstop(1)
         else
            owners = 1
            ownerq = 1
            goto 9999
         endif
      endif

      if (idebug.ge.2) write(lunrep,*) 'partit: noseg,nolay=',noseg,
     +   nolay,', nseghr=',nseghr

      if (npart.eq.1 .or. (noseg.eq.-226 .and. intsrt.eq.5)) then
!
!        Partitie voor sequentiele runs en voor debuggen:
!           Subdomein met hoogste nummer krijgt alle segmenten, andere
!           subdomeinen geen
!
         if (idebug.ge.2) write(lunrep,'(a,i3,a)') ' partit: npart=',npart,
     +      ', assigning all segments to part "npart", none to other parts'

         owners = npart

!     write(*,*) 'Arjen P (2): RTCACT = ', RTCACT
!     write(*,*) 'Arjen P (2): SRWACT = ', SRWACT
!     write(*,*) 'Arjen P (2): OLCFWQ = ', OLCFWQ


      elseif (noseg.eq.-13860 .and. intsrt.eq.12) then
!
!        Tijdelijke partitie voor debuggen:
!           Subdomein 2 krijgt alleen segment 1 van het horizontale vlak
!
         write(lunrep,'(a,i3,a)') ' partit: npart=',npart,
     +      ', assigning only first segment to part 2'
         owners = 1
         if (npart.gt.1) then
            do ilay = 1, nolay
               owners((ilay-1)*nseghr+1) = 2
               write(lunrep,'(a,i5,a)') 'owners(',(ilay-1)*nseghr+1,') = 2'
            enddo
         endif

      else
!
!        Echte partitie, voorlopig:
!           - het horizontale vlak wordt in 'npart' delen gesplitst
!           - de workers 2 .. npart krijgen allemaal een gelijk deel
!           - het eerste subdomein is "frac1st" maal zo groot als de rest
!
         loadpp  = real(nseghr) / real(npart-1+frac1st)
         nseg1st = nint(loadpp*frac1st)
         nsegpp  = (nseghr - nseg1st) / (npart-1)
         nsegrm  =  nseghr - nseg1st - (npart-1)*nsegpp

         if (idebug.ge.1) then
            write(lunrep,'(a,i3,a,i7,a,i3,a)') ' partit: npart=',npart,
     +         ', segments per part=', nsegpp,' horizontally x',nolay,
     +         ' layers'
            if (frac1st.eq.0.0) then
               write(lunrep,*) '        no segments to first subdomain'
            elseif (frac1st.ne.1.0) then
               write(lunrep,'(a,i7,a,i2,a)') '         size of first subdom is',
     +            nseg1st,' segments, ', nint(frac1st*100.0),
     +            ' % of other subdomains'
            endif
            if (nsegrm.eq.0) then
               write(lunrep,*) '        even distribution; no remaining ',
     +            'segments'
            else
               write(lunrep,'(3(a,i3))') '        ',nsegrm,
     +            ' remaining segments, assigned to parts',npart-nsegrm+1,
     +            ':',npart
            endif
         endif

!     write(*,*) 'Arjen P (3): RTCACT = ', RTCACT
!     write(*,*) 'Arjen P (3): SRWACT = ', SRWACT
!     write(*,*) 'Arjen P (3): OLCFWQ = ', OLCFWQ

c
c        assign nsegpp segments to each part, assign one extra segment to the
c        last nsegrm parts
c
         ioffs = 0
         do ilay = 1, nolay
c
c           original ordering of subdomains:
c
c           do ipart = 1, npart
c
c           reversed ordering of subdomains:
c
            do ipart = npart, 1, -1
c
               if (ipart.eq.1) then
                  nassgn = nseg1st
               elseif (ipart.lt.npart-nsegrm+1) then
                  nassgn = nsegpp
               else
                  nassgn = nsegpp + 1
               endif

               if (idebug.ge.2) write(lunrep,*) 'assigning segments [',
     +            ioffs+1,',',ioffs+nassgn,'] to part',ipart

               owners(ioffs+1:ioffs+nassgn) = ipart
               ioffs = ioffs + nassgn
            enddo
         enddo ! ilay
      endif
c
c     determine partitioning of exchanges:
c     assign each exchange to the "from"-segment using the ipoint-table
c
      do iq = 1, noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if (ifrom.gt.0) then
            ownerq(iq) = owners(ifrom)
         elseif (ito.gt.0) then
            ownerq(iq) = owners(ito)
         else
            ownerq(iq) = 0
         endif
      enddo
c
c     write partitioning to output-file for visualisation by user
c
      if (mypart.eq.1 .and. npart.gt.1) then
         lunout = 8
         is_ok  = .false.
  10     if (.not.is_ok) then
            inquire(unit=lunout, opened=lopen)
            if (.not.lopen) then
               is_ok = .true.
            else
               write(lunrep,*) 'unit',lunout,' is already open, trying',lunout+1
               lunout = lunout + 1
            endif
            goto 10
         endif

         write(lunrep,*) 'opening unit=',lunout,' for partition output-file'
         open(lunout, file='test-partit.map', form='binary')
         dscrp_file(1)='test map-file for parallel WAQ'
         dscrp_file(2)='sample partitioning of Hong-Kong grid'
         dscrp_file(3)='created by program map_file'
         dscrp_file(4)=' '
         write(lunout) dscrp_file

         write(lunout) 1, noseg
         dscrp_sys = 'grid-partitioning'
         write(lunout) dscrp_sys

         itime = 1
         write(lunout) itime, real(owners(1:noseg))
         close(lunout)
      endif

      if (idebug.ge.1) write(lunrep,*)

!     write(*,*) 'Arjen P (4): RTCACT = ', RTCACT
!     write(*,*) 'Arjen P (4): SRWACT = ', SRWACT
!     write(*,*) 'Arjen P (4): OLCFWQ = ', OLCFWQ

 9999 continue
      if ( timon ) call timstop ( ithandl )
      return
      end
