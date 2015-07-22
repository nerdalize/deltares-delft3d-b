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

      subroutine ixsets ( lunrep , mypart , notot  , nosys  , noseg  ,
     +                    noq    , ipoint , owners , ownerq , ndmpar ,
     +                    ndmps  , ntdmpq , ndmpq  , ibflag , isdmp  ,
     +                    ipdmp  , iqdmp  )
!
!     Deltares     Sector Waterresources and Environment
!
!     Created             : October 2007 by E. Vollebregt
!
!     Function            : Defines index-sets needed for communication
!                           between subdomains in parallel runs
!
!     Logical unitnumbers : lunrep - monitoring output file
!
!     Subroutines called  : SRSTOP, stops execution
!
!     Parameters          :
!
      use timers
      use m_couplib
      implicit none
      integer, intent(in)  :: lunrep        ! logical unit of stdout
      integer, intent(in)  :: mypart        ! part/subdomain number of
                                            ! current computing proces
      integer, intent(in)  :: notot         ! total number of systems
      integer, intent(in)  :: nosys         ! number of active systems
      integer, intent(in)  :: noseg         ! total number of segments
      integer, intent(in)  :: noq           ! total number of exchanges
      integer, intent(in)  :: ipoint(4,noq) ! exchange pointers
      integer, intent(in)  :: owners(noseg) ! partitioning of segments, for
                                            ! each segment the number of the
                                            ! part to which it belongs
      integer, intent(in)  :: ownerq(noq)   ! partitioning of exchanges
      integer, intent(in)  :: ndmpar        ! number of dump areas
      integer, intent(in)  :: ndmps         ! number of dump segments
      integer, intent(in)  :: ntdmpq        ! total number of exchanges in
                                            ! dump areas
      integer, intent(in)  :: ndmpq         ! number of dump exchanges
      integer, intent(in)  :: ibflag        ! if 1 then mass balance output
      integer, intent(in)  :: isdmp(noseg)  ! pointers for dumped segments
      integer, intent(in)  :: ipdmp(*)      ! pointer-structure for dump areas
      integer, intent(in)  :: iqdmp(noq)    ! pointers for dumped exchanges
!
!     Local variables     :
!
      integer, parameter :: idebug=0 ! level of debug output, 0=none
      integer :: iseg           ! loop counter for segments
      integer :: iq             ! loop counter for exchanges
      integer :: idq            ! loop counter for dump exchanges
      integer :: ifrom          ! from-segment for an exchange
      integer :: ito            ! to-segment for an exchange
      integer, allocatable, dimension(:) :: ineedq    ! mask-array for which exchanges are relevant
                                                      ! to current subdomain
      integer, allocatable, dimension(:)  :: ineeds   ! mask-array for which segments are relevant
                                                      ! to current subdomain
      integer, allocatable, dimension(:)  :: owndmps  ! ownership-array for dump-segments
      integer, allocatable, dimension(:)  :: owndmpq  ! ownership-array for dump-exchanges
      integer, allocatable, dimension(:)  :: ihavar   ! ihave-array for involvement of workers
                                ! with dump-areas
      integer :: ioffs1         ! offset in ipdmp-structure for obtaining nsc
      integer :: ioffs2         ! offset in ipdmp-structure for obtaining iseg
      integer :: idmpar         ! loop counter for dump-areas
      integer :: nsc            ! number of contributing segments for dump area
      integer :: isc            ! loop counter for contributing segments
      integer :: j              ! loop counter
      integer(4) ithandl /0/

      if ( timon ) call timstrt ( "ixsets", ithandl )


!-----------------------------------------------------------------------------
!  Explicitly allocate the local arrays

      allocate( ineedq(noq)    )
      allocate( ineeds(noseg)  )
      allocate( owndmps(ndmps) )
      allocate( owndmpq(ndmpq) )
      allocate( ihavar(ndmpar) )

!-----------------------------------------------------------------------------
!  Define index-set 'notot'; replicated index-set for number of systems

      call ixset_define('notot', notot)
      if (idebug.ge.2) call ixset_print('notot')

!  Define index-set 'nosys'; replicated index-set for active systems

      call ixset_define('nosys', nosys)
      if (idebug.ge.2) call ixset_print('nosys')

!  Define index-set 'noseg' for segments, using ownership given in owners

      call ixset_define('noseg', noseg, iowner=owners)
      if (idebug.ge.2) call ixset_print('noseg')

!  Define product-set 'notot*noseg' for concentration/flux arrays

      call ixset_product('notot*noseg', 'notot', 'noseg')
      if (idebug.ge.2) call ixset_print('notot*noseg')

!  Define product-set 'nosys*noseg' for concentration/flux arrays

      call ixset_product('nosys*noseg', 'nosys', 'noseg')
      if (idebug.ge.2) call ixset_print('nosys*noseg')

!  Define index-set 'noq' for exchanges, using ownership given in ownerq

      call ixset_define('noq', noq, iowner=ownerq)
      if (idebug.ge.2) call ixset_print('noq')

!-----------------------------------------------------------------------------
!  Derive distribute-interface for 'noq' from ipoint-table and ownership
!     of directly adjacent segments

      ineedq(1:noq) = 0

      do iq = 1, noq
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
!        if (adjacent segment belongs to my subdomain) then
!           mark exchange as needed
         if (ifrom.gt.0) then
            if (owners(ifrom).eq.mypart) ineedq(iq) = 1
         endif
         if (ito.gt.0) then
            if (owners(ito).eq.mypart) ineedq(iq) = 1
         endif
      enddo

!  Define distribute-interface for 'noq'

      call intfc_define_dstrbitf('noq', 'distrib_itf', mypart, ineedq)
      if (idebug.ge.2) call intfc_print('distrib_itf', 'noq')

!  Define collect-interface for 'noq'

      call intfc_define_collcitf('noq', mypart)
      if (idebug.ge.2) call intfc_print('collect_itf', 'noq')

!  Define update-interface for 'noq'

      call intfc_define_updatitf('noq', 'exchg_for_ownseg', ineedq)
      if (idebug.ge.2) call intfc_print('exchg_for_ownseg', 'noq')

!-----------------------------------------------------------------------------
!  Derive distribute-interface for 'noseg' from ipoint-table and ownership
!     of from- or to-segments, as encoded in ineedq

      ineeds(1:noseg) = 0

      do iq = 1, noq
!        if (segment ifrom or ito directly adjacent to exchange belongs
!            to my subdomain) then
!           mark all four neighbouring segments (ifrom, ito, ifrom2, ito2)
!           as needed
         if (ineedq(iq).eq.1) then
            do j = 1, 4
               if (ipoint(j,iq).gt.0) ineeds(ipoint(j,iq)) = 1
            enddo
         endif
      enddo

!  Define distribute-interface for 'noseg'

      call intfc_define_dstrbitf('noseg', 'distrib_itf', mypart, ineeds)
      if (idebug.ge.2) call intfc_print('distrib_itf', 'noseg')

!  Replicate distribute-interface for ix-set 'notot*noseg'

      call intfc_replicate('distrib_itf', 'notot*noseg')
      if (idebug.ge.2) call intfc_print('distrib_itf', 'notot*noseg')

!  Replicate distribute-interface for ix-set 'nosys*noseg'

      call intfc_replicate('distrib_itf', 'nosys*noseg')
      if (idebug.ge.2) call intfc_print('distrib_itf', 'nosys*noseg')

!  Define collect-interface for 'noseg'

      call intfc_define_collcitf('noseg', mypart)
      if (idebug.ge.2) call intfc_print('collect_itf', 'noseg')

!  Replicate collect-interface for ix-set 'notot*noseg'

      call intfc_replicate('collect_itf', 'notot*noseg')
      if (idebug.ge.2) call intfc_print('collect_itf', 'notot*noseg')

!  Replicate collect-interface for ix-set 'nosys*noseg'

      call intfc_replicate('collect_itf', 'nosys*noseg')
      if (idebug.ge.2) call intfc_print('collect_itf', 'nosys*noseg')

!-----------------------------------------------------------------------------
!  Derive distribute-interface for 'noseg' for testing purposes: all
!     values distributed to all worker processes

      ineeds(1:noseg) = 1

!  Define distribute-interface 'all' for 'noseg'

      call intfc_define_dstrbitf('noseg', 'all', mypart, ineeds)
      if (idebug.ge.2) call intfc_print('all', 'noseg', idebug=1)

!-----------------------------------------------------------------------------
!  Derive update-interface 'stc1' for 'noseg' from ipoint-table
!   --> all segments adjacent to an exchange adjacent to one of our segments
!   --> relevant exchanges are already marked in ineedq

      ineeds(1:noseg) = 0

      do iq = 1, noq
!        if (segment ifrom or ito of exchange belongs to my subdomain) then
!           mark both segments (ifrom, ito) as needed
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if (ineedq(iq).eq.1) then
            if (ifrom.gt.0) ineeds(ifrom) = 1
            if (ito  .gt.0) ineeds(ito) = 1
         endif
      enddo

!  Define update-interface 'stc1' for 'noseg'

      call intfc_define_updatitf('noseg', 'stc1', ineeds)
      if (idebug.ge.2) call intfc_print('stc1', 'noseg')

!  Derive update-interface 'stc2' for 'noseg' from ipoint-table

      ineeds(1:noseg) = 0

      do iq = 1, noq
!
!        if (exchange is needed by my subdomain)
         if (ineedq(iq).eq.1) then
!
!           mark all segments (ifrom, ito, ifrom2, ito2) as needed
            do j = 1, 4
               if (ipoint(j,iq).gt.0) ineeds(ipoint(j,iq)) = 1
            enddo
         endif
      enddo

!  Define update-interface 'stc2' for 'noseg'

      call intfc_define_updatitf('noseg', 'stc2', ineeds)
      if (idebug.ge.2) call intfc_print('stc2', 'noseg')

!-----------------------------------------------------------------------------
!  If computation of mass-balances is requested:
!     Define index-sets for working with dump-segments and dump-areas

      if (ibflag.gt.0) then
         if (idebug.ge.2) write(lunrep,*) 'ixsets: define index-sets ',
     +      'for dump-areas and -segments'

!  Define index-set 'ndmps' for dump-segments, using ownership derived
!     from owners

         owndmps = 0
         do iseg = 1, noseg
            if (isdmp(iseg).gt.0) owndmps(isdmp(iseg)) = owners(iseg)
         enddo

         call ixset_define('ndmps', ndmps, iowner=owndmps)
         if (idebug.ge.2) call ixset_print('ndmps')

!  Define distribute-interface for 'ndmps'

         call intfc_define_dstrbitf('ndmps', mypart)
         if (idebug.ge.2) call intfc_print('distrib_itf', 'ndmps')

!  Define collect-interface for 'ndmps'

         call intfc_define_collcitf('ndmps', mypart)
         if (idebug.ge.2) call intfc_print('collect_itf', 'ndmps')

!  Define index-set 'ndmpar' for dump-areas, with possibly overlapping
!     ownership, multiple workers contributing to same dump-area
!     ==> ownership is filled with 'mypart' for own indices, '0' elsewhere.
!         This is done in array ihavar, which is filled using ownership of
!         segments and administration of which segments contribute to each
!         dump-area

         ihavar = 0
         ioffs1 = ndmpar + ntdmpq
         ioffs2 = ndmpar + ntdmpq + ndmpar

         do idmpar = 1, ndmpar
            nsc = ipdmp(ioffs1+idmpar)
            if (idebug.ge.2) write(lunrep,'(a,i3,a,i4)') 'dump-area',idmpar,
     +         ': num segments=',nsc
            do isc = 1, nsc
               iseg = ipdmp(ioffs2+isc)
               if (owners(iseg).eq.mypart) ihavar(idmpar) = 1
               if (idebug.ge.2) write(lunrep,'(a,i3,a,i4,a,i7,a,i2)')
     +            'dump-area',idmpar, ': isc=',isc,', iseg=',iseg,
     +            ', owner=',owners(iseg)
            enddo
            ioffs2 = ioffs2 + nsc
         enddo

         ihavar = ihavar*mypart
         call ixset_define('ndmpar', ndmpar, iowner=ihavar)
         if (idebug.ge.2) call ixset_print('ndmpar')

!  Define distribute-interface for data for dump-areas,

         call intfc_define_dstrbitf('ndmpar', mypart)
         if (idebug.ge.2) call intfc_print('distrib_itf', 'ndmpar')

!  Define interface 'my_dmpar' for accumulating data for dump-areas,

         ihavar = ihavar/mypart
         call intfc_define_collcitf('ndmpar', 'my_dmpar', mypart, ihavar)
         if (idebug.ge.2) call intfc_print('my_dmpar', 'ndmpar')

!  Define index-set 'ndmpq' for dump-exchanges, using ownership derived
!     from ownerq

         owndmpq = 0
         do iq = 1, noq
            if (iqdmp(iq).gt.0) owndmpq(iqdmp(iq)) = ownerq(iq)
         enddo

         call ixset_define('ndmpq', ndmpq, iowner=owndmpq)
         if (idebug.ge.2) call ixset_print('ndmpq')

!  Define distribute-interface for 'ndmpq'

         call intfc_define_dstrbitf('ndmpq', mypart)
         if (idebug.ge.2) call intfc_print('distrib_itf', 'ndmpq')

!  Define collect-interface for 'ndmpq'

         call intfc_define_collcitf('ndmpq', mypart)
         if (idebug.ge.2) call intfc_print('collect_itf', 'ndmpq')

      endif

      deallocate( ineedq  )
      deallocate( ineeds  )
      deallocate( owndmps )
      deallocate( owndmpq )
      deallocate( ihavar  )

      if ( timon ) call timstop ( ithandl )

      return
      end
