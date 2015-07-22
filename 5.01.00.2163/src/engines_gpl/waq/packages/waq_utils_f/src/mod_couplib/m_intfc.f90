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

!-- MODULE m_intfc -----------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Definition of data-structures and subroutines regarding communication
!   interfaces of the CouPLib communication library
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/m_intfc.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------
module m_intfc
use m_timings
use m_coupcns
use m_prcgrp
use m_ixset
implicit none
private

! subroutines for configuration of interfaces:

public intfc_initmod
public intfc_hndl
public intfc_getprops
public intfc_print
public intfc_define_collcitf
public intfc_define_dstrbitf
public intfc_define_updatitf
public intfc_replicate

interface intfc_hndl
   module procedure intfc_hndl_namixs
   module procedure intfc_hndl_iset
end interface intfc_hndl

interface intfc_define_dstrbitf
   module procedure intfc_define_dstrbitf_owner
   module procedure intfc_define_dstrbitf_ineed
end interface intfc_define_dstrbitf

interface intfc_define_collcitf
   module procedure intfc_define_collcitf_owner
   module procedure intfc_define_collcitf_ihave
end interface intfc_define_collcitf

! definition of a send and receive-area for one neighbour
! Note: a send/receive-area more or less consists of two subsets of an indexset
type, public :: t_sndrcv_area
   integer                        :: iprc  !! process id of neighbour
   integer                        :: iset  !! index-set for which the send/recv
                                           !! areas are given (index in indset)
   integer, dimension(:), pointer :: isnd  !! send-list, enumerating the indices
                                           !! of iset to be sent to neighbour
   integer, dimension(:), pointer :: ircv  !! recv-list, enumerating the indices
                                           !! of iset to be received from
                                           !! neighbour
end type t_sndrcv_area


! definition of one interface
type    :: t_intfc
   character(len=STRLEN) :: namitf   !! name of the interface
   integer               :: iset     !! index-set for which the interface is
                                     !! defined (index in indset)
   integer               :: nsnd_tot !! total number of indices to send
   integer               :: nrcv_tot !! total number of indices to receive
   integer               :: nngb     !! number of neighbours in the interface
!
!  Interfaces may be "stand-alone" or "replicated". A replicated interface is
!  used for Cartesian product index-sets. It selects all points in the
!  replicated dimensions of the index-set (using interface "IUNITY"), and
!  the selected points of the distributed factor of the product-set (using a
!  stand-alone interface that is replicated).
!
   integer               :: nfac     !! for a replicated interface: number of
                                     !! factors; stand-alone interface: 1
   integer, dimension(:), pointer :: ifctrs   !! factors for a replicated
                                     !! interface. positive values are handles
                                     !! to sub-interfaces. Negative values are
                                     !! the number of elements in replicated
                                     !! dimensions.
                                     !! Stand-alone intfc: handle to itf itself
   integer               :: itfrpl   !! handle to the stand-alone interface
                                     !! that is replicated
!
!  Stand-alone interface:
!
   type(t_sndrcv_area), dimension(:), pointer :: nghtbl
                                     !! neighbour-table (1:nngb) containing the
                                     !! send/receive areas for each neighbour
end type t_intfc


! definition of the table of interfaces: using a pointer, allowing the table
! to be re-allocated when the capacity is exceeded
integer                              :: nintfc = 0
type(t_intfc), dimension(:), pointer :: intfcs => NULL()


contains


!-- SUBROUTINE intfc_initmod -------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Initialize the CouPLib data-structures w.r.t. interfaces
!!>
!!  On entry:
!!  -
!!
!!  On return:
!!  -           the data-structures for interfaces have been initialized
!!<
!-----------------------------------------------------------------------------
subroutine intfc_initmod()

!-- HEADER VARIABLES/ARGUMENTS
implicit none

!-- LOCAL VARIABLES
!-----------------------------------------------------------------------------
   nintfc = 0
   allocate(intfcs(INIT_LENGTH))

end subroutine intfc_initmod


!-- FUNCTION intfc_hndl_namixs -----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Get the handle of a communication interface defined on a specific index-set
!!>
!!  On entry:
!!  namitf      name of the interface to be found
!!  namixs      name of the index-set on which the interface should be defined
!!  ierr        optional: desired behaviour when interface is not found
!!              (positive values of ierr) or is found (negative ierr)
!!               IFATAL: error and stop when not found (default)
!!               IWARN : warning message when not found
!!               INONE : continue as much as possible without warning
!!              -IWARN : warning message when found
!!              -IFATAL: error and stop message when found
!!  On return:
!!  iitf        if found: handle to the interface, IDUNNO otherwise
!!<
!-----------------------------------------------------------------------------
function intfc_hndl_namixs(namitf, namixs, ierr)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in)            :: namitf
character(len=*), intent(in)            :: namixs
integer,          intent(in), optional  :: ierr
integer                                 :: intfc_hndl_namixs

!-- LOCAL VARIABLES
! handle for index-set namixs
integer                 :: iset
!-----------------------------------------------------------------------------

   !write(LOUT,*) 'intfc_hndl_namixs: starting...'

!  Get handle to index-set namixs

   iset = ixset_hndl(namixs)

!  Delegate actual work to intfc_hndl_iset

   if (present(ierr)) then
      intfc_hndl_namixs = intfc_hndl_iset(namitf, iset, ierr)
   else
      intfc_hndl_namixs = intfc_hndl_iset(namitf, iset)
   endif

end function intfc_hndl_namixs


!-- FUNCTION intfc_hndl_iset -------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Get the handle of a communication interface defined on a specific index-set
!!>
!!  On entry:
!!  namitf      name of the interface to be found
!!  iset        handle to the index-set on which the interface should be defined
!!  ierr        optional: desired behaviour when interface is not found
!!              (positive values of ierr) or is found (negative ierr)
!!               IFATAL: error and stop when not found (default)
!!               IWARN : warning message when not found
!!               INONE : continue as much as possible without warning
!!              -IWARN : warning message when found
!!              -IFATAL: error and stop message when found
!!  On return:
!!  iitf        if found: handle to the interface, IDUNNO otherwise
!!<
!-----------------------------------------------------------------------------
function intfc_hndl_iset(namitf, iset, ierr)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in)            :: namitf
integer,          intent(in)            :: iset
integer,          intent(in), optional  :: ierr
integer                                 :: intfc_hndl_iset

!-- LOCAL VARIABLES
! loop counter for interfaces
integer                 :: iitf
logical                 :: found
character(len=STRLEN)   :: namixs
! local variable for ierr
integer                 :: iierr
!-----------------------------------------------------------------------------

   !write(LOUT,*) 'intfc_hndl_iset: starting...'
   if (present(ierr)) then
      iierr = ierr
   else
      iierr = IFATAL
   endif

   call ixset_getprops(iset, namixs)
   !write(LOUT,'(3a,i3,3a)') ' intfc_hndl: searching intfc="',trim(namitf), &
   !     '" on index-set', iset, ' ("', trim(namixs), '")'
   !, nintfc=', nintfc

   iitf = 0
   found = .false.
   do while (.not.found .and. iitf.lt.nintfc)
      iitf = iitf + 1
      if (intfcs(iitf)%iset.eq.iset) then
         if (namitf.eq.intfcs(iitf)%namitf) found = .true.
      endif
   end do

!  Report result, return appropriate handle iitf or IDUNNO

   if (found) then
      !write(LOUT,*) '...found, iitf=',iitf
      if (iierr.eq.-IWARN .or. iierr.eq.-IFATAL) then
         write(LOUT,*) 'Intfc_hdnl: an interface with name "',trim(namitf),&
             '" was found but should not (yet) exist.'
      endif
      if (iierr.eq.-IFATAL) stop
      intfc_hndl_iset = iitf
   else
      if (iierr.eq.IWARN .or. iierr.eq.IFATAL) then
         write(LOUT,*) 'intfc_hndl: Error: cannot find interface with name="', &
                 trim(namitf),'" for index-set "', trim(namixs), '".'
      endif
      if (iierr.eq.IFATAL) stop
      intfc_hndl_iset = IDUNNO
   endif

end function intfc_hndl_iset


!-- SUBROUTINE intfc_getprops ------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Return the properties of a communication interface
!!>
!!  On entry:
!!  iitf        handle of interface for which properties are required
!!
!!  On return:
!!  namitf      optional: name of the interface
!!  iset        optional: index-set for which interface is defined
!!  nngb        optional: number of neighbours in the interface
!!  nsnd_tot    optional: total number of indices to send
!!  nrcv_tot    optional: total number of indices to receive
!!  nfac        optional: number of factors for a replicated interface,
!!              stand-alone interface: 1
!!  pfctrs      optional: pointer to array with factors for a replicated
!!              interface. positive values are handles to sub-interfaces.
!!              Negative values are the number of elements in replicated
!!              dimensions. Stand-alone intfc: handle to itf itself
!!  itfrpl      optional: handle to the stand-alone interface that is replicated
!!  nghtbl      optional: pointer to the list of send/receive-areas for all
!!              neighbours
!!<
!-----------------------------------------------------------------------------
subroutine intfc_getprops(iitf, namitf, iset, nsnd_tot, nrcv_tot, nngb, &
                          nfac, pfctrs, itfrpl, nghtbl)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,                        intent(in)            :: iitf
character(len=*),               intent(out), optional :: namitf
integer,                        intent(out), optional :: iset
integer,                        intent(out), optional :: nsnd_tot
integer,                        intent(out), optional :: nrcv_tot
integer,                        intent(out), optional :: nngb
integer,                        intent(out), optional :: nfac
integer,             dimension(:), pointer,  optional :: pfctrs
integer,                        intent(out), optional :: itfrpl
type(t_sndrcv_area), dimension(:), pointer,  optional :: nghtbl

!-- LOCAL VARIABLES
! pointer to current interface
type(t_intfc), pointer  :: itf
!-----------------------------------------------------------------------------

!  Check input-argument, set pointer to appropriate interface

   if (iitf.le.0 .or. iitf.gt.nintfc) then
      write(LOUT,*) 'intfc_getprops: Error: handle to interface',iitf,&
                    ' out of range 1..',nintfc
      stop
   endif

   itf => intfcs(iitf)

!  Return name, iset when requested

   if (present(namitf)) namitf = itf%namitf
   if (present(iset))   iset   = itf%iset

!  Return number of indices to send/receive when requested

   if (present(nsnd_tot)) nsnd_tot = itf%nsnd_tot
   if (present(nrcv_tot)) nrcv_tot = itf%nrcv_tot

!  Return number of factors, pointer to list of factors when requested

   if (present(nfac))   nfac   =  itf%nfac
   if (present(pfctrs)) pfctrs => itf%ifctrs
   if (present(itfrpl)) itfrpl =  itf%itfrpl

!  Return number of neighbours, neighbour-table when requested

   if (present(nngb))   nngb   =  itf%nngb
   if (present(nghtbl)) nghtbl => itf%nghtbl

end subroutine intfc_getprops


!-- SUBROUTINE print_sndrcv_area ---------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Display the contents of a send/receive area for one neighbour
!!>
!!  On entry:
!!  sndrcv      the send/receive-area to be displayed
!!  idebug      level of output requested (0=none)
!!
!!  On return:
!!  -           the properties of the send/recv-area are written to the screen
!!<
!-----------------------------------------------------------------------------
subroutine print_sndrcv_area(sndrcv, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
type(t_sndrcv_area)            :: sndrcv
integer,            intent(in) :: idebug

!-- LOCAL VARIABLES
!-----------------------------------------------------------------------------

   if (idebug.ge.1) then
      if (associated(sndrcv%isnd)) then
         write(LOUT,'(a,i3,a,i7,a,i3,a)') '   send-area for process', &
            sndrcv%iprc, ' (',size(sndrcv%isnd),' indices of iset=', &
            sndrcv%iset,')'
         if (idebug.ge.2) write(LOUT,'(1000(9x,10i7,:,/))') sndrcv%isnd
      else
         write(LOUT,'(a,i3,a)') '   send-area for process', &
            sndrcv%iprc, ': empty'
      endif

      if (associated(sndrcv%ircv)) then
         write(LOUT,'(a,i3,a,i7,a,i3,a)') '   recv-area for process', &
            sndrcv%iprc, ' (',size(sndrcv%ircv),' indices of iset=', &
            sndrcv%iset,')'
         if (idebug.ge.2) write(LOUT,'(1000(9x,10i7,:,/))') sndrcv%ircv
      else
         write(LOUT,'(a,i3,a)') '   recv-area for process', &
            sndrcv%iprc, ': empty'
      endif
   endif

end subroutine print_sndrcv_area


!-- SUBROUTINE intfc_print ---------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Display the properties of a communication interface
!!>
!!  On entry:
!!  namitf      name of interface to be displayed
!!  namixs      name of index-set on which interface should be defined
!!  idebug      optional: level of output requested (0=none)
!!
!!  On return:
!!  -           the properties of the interface are written to the screen
!!<
!-----------------------------------------------------------------------------
subroutine intfc_print(namitf, namixs, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in)           :: namitf
character(len=*), intent(in)           :: namixs
integer,          intent(in), optional :: idebug

!-- LOCAL VARIABLES
! internal value of idebug
integer                 :: my_idebug
! loop counters for interfaces, elements, neighbours
integer                 :: iitf, ingb
! pointer to current interface
type(t_intfc), pointer  :: pitf
! loop counter for factors, handle of factor
integer                 :: ifac, iitffc
! names of factors of interface
character(len=STRLEN)   :: namfac
!-----------------------------------------------------------------------------

!  handle optional idebug

   my_idebug = 1
   if (present(idebug)) my_idebug = idebug

!  look up the interface 'namitf' in the list of interfaces intfcs

   if (my_idebug.ge.5) write(LOUT,*) 'intfc_print: get handle of interface'
   iitf = intfc_hndl(namitf, namixs, IWARN)
   if (iitf.eq.IDUNNO) return

   if (my_idebug.ge.5) write(LOUT,*) 'intfc_print: iitf=',iitf,', get pointer'
   pitf => intfcs(iitf)

!  Print information about interface 'namitf'

   if (my_idebug.ge.1) then
      write(LOUT,'(a,2(i3,3a))') ' interface',iitf,' "',trim(pitf%namitf), &
           '": defined on ixset', pitf%iset,' "',namixs,'".'
      if (pitf%nfac.gt.1) then
         write(LOUT,'(a,5i4)') '   replicated interface, factors:',pitf%ifctrs
         do ifac = 1, pitf%nfac
            iitffc = pitf%ifctrs(ifac)
            if (iitffc.le.0) then
               write(namfac,'(i7)') -iitffc
            else
               call intfc_getprops(iitffc, namfac)
            endif
            write(LOUT,'(a,i2,a,i7,3a)') '      factor',ifac,' is intfc', &
                  iitffc,': "', trim(namfac), '"'
         enddo
      else
         write(LOUT,'(a)')     '   stand-alone interface'
      endif
      write(LOUT,'(a,i3,2(a,i7))') '   number of neighbours=', pitf%nngb, &
         ', number of indices to send=', pitf%nsnd_tot, ', recv=',pitf%nrcv_tot
   endif

   if (associated(pitf%nghtbl)) then
      do ingb = 1, pitf%nngb
         call print_sndrcv_area( pitf%nghtbl(ingb), my_idebug )
      enddo
   endif

end subroutine intfc_print


!-- SUBROUTINE intfc_define --------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Define a new named stand-alone interface for use in communications later on.
!!  Direct version for when the neighbour-table of the interface is known.
!!>
!!  On entry:
!!  namitf      name of the interface to be registered
!!  iset        handle of the index-set for which the interface is defined
!!  nngb        number of neighbours of current process in this interface
!!  nghtbl      the send/receive-areas for each neighbour in the interface
!!
!!  On return:
!!  -           the data describing the interface has been stored in the
!!              internal data-structures of this module
!!<
!-----------------------------------------------------------------------------
subroutine intfc_define(namitf, iset, nngb, nghtbl)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*),      intent(in)          :: namitf
integer,               intent(in)          :: iset
integer,               intent(in)          :: nngb
type(t_sndrcv_area), dimension(:), pointer :: nghtbl

!-- LOCAL VARIABLES
! help-variable for re-allocating list of interfaces
type(t_intfc), dimension(:), pointer :: wrk_intfcs
! pointer to new interface
integer                              :: iitf
type(t_intfc), pointer               :: new
! loop counter for neighbours
integer                              :: ingb
!-----------------------------------------------------------------------------

   !write(LOUT,*) 'intfc_define: starting for namitf=',trim(namitf)

!  Verify that the CouPLib datastructures have been initialized

   if (.not.associated(intfcs)) then
      write(LOUT,*) 'intfc_define: Error: CouPLib data-structures have not ', &
                 'been initialized properly!'
      stop
   endif

!  check that name 'namitf' does not yet exist in table of interfaces for
!  index-set iset

   iitf = intfc_hndl(namitf, iset, INONE)
   if (iitf.ne.IDUNNO) then
      write(LOUT,*) 'intfc_define: Error: interface "',trim(namitf), &
                 '" already exists for index-set',iset
      stop
   endif

!  Re-allocate the intfcs-table when needed

   if (nintfc.ge.size(intfcs)) then
      write(LOUT,*) 'intfc_define: current capacity of intfcs exceeded, ', &
                 're-allocating with size=', nint(size(intfcs)*GROW_LENGTH)
      allocate(wrk_intfcs(nint(size(intfcs)*GROW_LENGTH)))
      wrk_intfcs(1:size(wrk_intfcs)) = intfcs
      deallocate(intfcs)
      intfcs => wrk_intfcs
      nullify(wrk_intfcs)
   endif

!  Add a new interface

   nintfc = nintfc + 1
   new => intfcs(nintfc)

!  Register the name, the corresponding index-set

   new%namitf = namitf
   new%iset   = iset

!  Register information w.r.t. replication

   new%nfac      = 1
   allocate(new%ifctrs(1))
   new%ifctrs(1) = nintfc
   new%itfrpl    = nintfc

!  Register the number of neighbours and the send/receive-areas

   !write(LOUT,*) '  registering nngb, nghtbl'

   new%nngb   = nngb
   new%nghtbl => nghtbl

!  Count the number of indices in send- and receive-areas per neighbour

   new%nsnd_tot = 0
   new%nrcv_tot = 0
   do ingb = 1, nngb
      if (associated(nghtbl(ingb)%isnd)) then
         new%nsnd_tot = new%nsnd_tot + size(nghtbl(ingb)%isnd)
      endif
      if (associated(nghtbl(ingb)%ircv)) then
         new%nrcv_tot = new%nrcv_tot + size(nghtbl(ingb)%ircv)
      endif
   enddo

   !write(LOUT,*) '  intfc_define: done, returning...'

end subroutine intfc_define


!-- SUBROUTINE intfc_replicate -----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Make a stand-alone interface available on a product index-set by creating
!!  a replicated version of the interface.
!!>
!!  On entry:
!!  namitf      name of the stand-alone interface to be replicated. Also: name
!!              of the replicated interface to be created.
!!  namixs      name of the Cartesian product index-set for which the replicated
!!              interface must be defined.
!!  idebug      optional: level of output requested (0=none)
!!
!!  On return:
!!  -           a replicated interface with name namitf is defined on Cartesian
!!              product set namixs.
!!<
!-----------------------------------------------------------------------------
subroutine intfc_replicate(namitf, namixs, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*),      intent(in)           :: namixs
character(len=*),      intent(in)           :: namitf
integer,               intent(in), optional :: idebug

!-- LOCAL VARIABLES
! internal value of idebug
integer                 :: my_idebug
! handle of index-set, new interface, replicated interface
integer                         :: iset, iitf, itfrpl
! properties of index-sets, interface
integer                         :: ifac, nfac, ifcdst, nelem
integer, dimension(:), pointer  :: pfctrs
! name of distributed factor of product-set
character(len=STRLEN)           :: namdst
! help-variable for re-allocating list of interfaces
type(t_intfc), dimension(:), pointer :: wrk_intfcs
! pointer to new interface
type(t_intfc), pointer               :: new
!-----------------------------------------------------------------------------

!  handle optional idebug

   my_idebug = 0
   if (present(idebug)) my_idebug = idebug

!  get handle to index-set namixs

   if (my_idebug.ge.1) write(LOUT,*) 'intfc_replicate: starting for ixset="', &
      trim(namixs),'", intfc="',trim(namitf),'"'

   iset = ixset_hndl(namixs, IFATAL)

!  check that name 'namitf' does not yet exist in table of interfaces for
!  index-set iset

   iitf = intfc_hndl(namitf, iset, INONE)
   if (iitf.ne.IDUNNO) then
      write(LOUT,*) 'intfc_replicate: Error: interface "',trim(namitf), &
                 '" already exists for index-set',iset
      stop
   endif

!  check that iset is a Cartesian product;
!  get handle to distributed factor of index-set namixs

   call ixset_getprops(iset, nfac=nfac, ifcdst=ifcdst, pfctrs=pfctrs)
   if (nfac.le.1) then
      write(LOUT,*) 'intfc_replicate: Error: index-set "',trim(namixs), &
                 '" must be a Cartesian product.'
      stop
   endif
   if (ifcdst.eq.IVOID) then
      write(LOUT,*) 'intfc_replicate: Error: index-set "',trim(namixs), &
                 '" does not contain a distributed index-set factor.'
      stop
   endif

!  get handle to interface 'namitf' on stand-alone distributed index-set ifcdst

   itfrpl = intfc_hndl(namitf, ifcdst, INONE)
   if (itfrpl.eq.IDUNNO) then
      call ixset_getprops(ifcdst, namixs=namdst)
      write(LOUT,*) 'intfc_replicate: Error: interface "',trim(namitf), &
                 '" must exist for distributed index-set',ifcdst,' "', &
                 trim(namdst),'"'
      stop
   endif

!  Re-allocate the intfcs-table when needed

   if (nintfc.ge.size(intfcs)) then
      write(LOUT,*) 'intfc_define: current capacity of intfcs exceeded, ', &
                 're-allocating with size=', nint(size(intfcs)*GROW_LENGTH)
      allocate(wrk_intfcs(nint(size(intfcs)*GROW_LENGTH)))
      wrk_intfcs(1:size(wrk_intfcs)) = intfcs
      deallocate(intfcs)
      intfcs => wrk_intfcs
      nullify(wrk_intfcs)
   endif

!  Add a new interface

   nintfc = nintfc + 1
   new => intfcs(nintfc)

!  Register the name, the corresponding index-set

   new%namitf = namitf
   new%iset   = iset

!  Register information w.r.t. replication

   new%nfac   = nfac
   allocate(new%ifctrs(nfac))
   new%itfrpl = itfrpl

!  Copy the number of neighbours and number of items to send/receive from
!  interface that is replicated

   new%nngb     = intfcs(itfrpl)%nngb
   new%nsnd_tot = intfcs(itfrpl)%nsnd_tot
   new%nrcv_tot = intfcs(itfrpl)%nrcv_tot
   new%nghtbl   => NULL()

!  For all dimensions of replicated interface do:

   do ifac = 1, nfac
      if (pfctrs(ifac).eq.ifcdst) then

!         - distributed index-set factor: store handle of stand-alone interface

         new%ifctrs(ifac) = itfrpl

      else

!         - replicated index-set factor: get number of elements

         call ixset_getprops(pfctrs(ifac), nelem=nelem)

!         - set replication in interface, increase number of elements to
!           send/receive

         new%ifctrs(ifac) = -nelem
         new%nsnd_tot = new%nsnd_tot * nelem
         new%nrcv_tot = new%nrcv_tot * nelem
      endif
   enddo

end subroutine intfc_replicate


!-- SUBROUTINE intfc_define_collcitf_owner -----------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Create/register a new communication interface meant for collecting data
!!  at process 1.
!!
!!  Note: the interface uses the ownership of the index-set for determining
!!        which items must be sent/received. As such there can be only one
!!        such interface per index-set. Internally, the name 'collect_itf'
!!        is used for this index-set.
!!>
!!  On entry:
!!  namixs      name of index-set to which the interface applies
!!  mypart      own process-id
!!
!!  On return:
!!  -           the new interface has been defined/registered
!!<
!-----------------------------------------------------------------------------
subroutine intfc_define_collcitf_owner(namixs, mypart)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in) :: namixs
integer,          intent(in) :: mypart

!-- LOCAL VARIABLES
! (internal) name of the interface that is created
character(len=STRLEN)          :: namitf='collect_itf'
! work-variable to compose the neighbour-table of the interface
type(t_sndrcv_area), dimension(:), pointer :: nghtbl
type(t_sndrcv_area), pointer               :: sndrcv
! properties of index-set
integer                        :: iset, nelem
integer, dimension(:), pointer :: iowner
! properties of interface
integer                        :: nngb
! work-variables, loop-counters
integer                        :: iprc, ingb
integer                        :: iel, is, nsend, nrecv
!-----------------------------------------------------------------------------

   !write(LOUT,*) 'intfc_define_collcitf_owner: starting for namitf="',&
   !     trim(namitf),'" on ixset "',trim(namixs),'"'

!  get handle to index-set

   iset = ixset_hndl(namixs)

!  get pointer to ownership-array of index-set

   call ixset_getprops(iset, nelem=nelem, piownr=iowner)

   !write(LOUT,*) '  iset=',iset,', nelem=',nelem,', iowner='
   !write(LOUT,*) iowner

!  determine number of neighbours
!   - master: all processes except myself that own one or more points
!   - worker: master when I own one or more points

   if (mypart.eq.1) then
      nngb   = 0
      do iprc = 2, numprc
         if (any(iowner.eq.iprc)) nngb = nngb + 1
      enddo
   else
      nngb = 1
   endif
   !write(LOUT,*) '  nngb=',nngb

!  construct neighbour-table

   allocate(nghtbl(nngb))

!  fill neighbour-table: for each neighbour fill the send/receive areas
!   - master: receive-area for all other processes
!   - worker: send-area to master

   if (mypart.eq.1) then
!     master: nngb neighbours defined in iowner
!     loop over all processes, fill in send/receive area

      ingb = 0
      do 100 iprc = 2, numprc

!        count number of elements to be received from process iprc

         nrecv = count(iowner.eq.iprc)

!        if > 0: new neighbour found, fill in send/receive area

         if (nrecv.gt.0) then
            ingb = ingb + 1

            sndrcv => nghtbl(ingb)
            sndrcv%iprc = iprc
            sndrcv%iset = iset
            sndrcv%isnd => NULL()

            allocate(sndrcv%ircv(nrecv))

!           copy neighbours indices of index-set to recv-area

            is = 0
            do iel = 1, nelem
               if (iowner(iel).eq.iprc) then
                  is = is + 1
                  sndrcv%ircv(is) = iel
               endif
            enddo
            !write(LOUT,*) '  ingb=',ingb,': iprc=',iprc,',', nrecv, &
            !    ' recv-points:'
            !write(LOUT,*) sndrcv%ircv
         endif
 100  continue

   else
!     worker: 1 neighbour == master
!     fill in trivial properties of send/receive-area for ngb 1 (master)

      sndrcv => nghtbl(1)
      sndrcv%iprc = 1
      sndrcv%iset = iset
      sndrcv%ircv => NULL()

!     count number of elements to be sent, allocate send-area

      nsend = count(iowner.eq.mypart)
      allocate(sndrcv%isnd(nsend))

!     copy my own indices of index-set to send-area

      is = 0
      do iel = 1, nelem
         if (iowner(iel).eq.mypart) then
            is = is + 1
            sndrcv%isnd(is) = iel
         endif
      enddo

      !write(LOUT,*) '  ingb=1: iprc=1,', nsend,' send-points:'
      !write(LOUT,*) sndrcv%isnd
   endif

!  Use intfc_define to register the data collected as a new interface

   call intfc_define(namitf, iset, nngb, nghtbl)

end subroutine intfc_define_collcitf_owner


!-- SUBROUTINE intfc_define_collcitf_ihave -----------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Create/register a new communication interface meant for accumulating data
!!  from the worker processes on the master process 1.
!!>
!!  On entry:
!!  namixs      name of index-set to which the interface applies
!!  namitf      name of the new communication interface
!!  mypart      own-process id
!!  ihave       mask array that indicates for which indices the current
!!              process contributes (partial) data to the master process
!!
!!  On return:
!!  -           the new interface has been defined/registered
!!
!!  Note: this routine is largely similar to intfc_define_distrbitf.
!!<
!-----------------------------------------------------------------------------
subroutine intfc_define_collcitf_ihave(namixs, namitf, mypart, ihave)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*),      intent(in) :: namixs
character(len=*),      intent(in) :: namitf
integer,               intent(in) :: mypart
integer, dimension(:), intent(in) :: ihave

!-- LOCAL VARIABLES
! work-variable to compose the neighbour-table of the interface
type(t_sndrcv_area), dimension(:), pointer :: nghtbl
type(t_sndrcv_area), pointer               :: sndrcv
! properties of index-set
integer                        :: iset, nelem
! properties of interface
integer                        :: nngb
! work-variables, loop-counters
integer                        :: iprc, ingb
integer                        :: iel, is, nsend, nrecv
! work-array for master to receive ineed of worker
integer, dimension(:), allocatable :: iwork
! help variables for MPI-operations
integer, parameter             :: TAG_COLCIF = 1002
integer                        :: idest, isrc, msgtag, ierror
integer                        :: mpstat(MPI_STATUS_SIZE)
! level of debug-output, 0=none
integer                        :: idebug
!-----------------------------------------------------------------------------

   idebug = 0
   if (idebug.ge.1) write(LOUT,*) 'intfc_define_collcitf_ihave: starting ', &
        'for namitf="', trim(namitf),'" on ixset "',trim(namixs),'"'

!  start timing of interface definition

   if (use_timers) call timer_start(itimer_couplib_intfc)

!  get handle to index-set

   iset = ixset_hndl(namixs)

!  get number of elements of index-set, check size of ihave

   call ixset_getprops(iset, nelem=nelem)
   if (size(ihave).ne.nelem) then
      write(LOUT,*) 'intfc_define_collcitf_ihave: Error: size of mask-',&
        'array ihave is not correct. Should be',nelem,', actual length=', &
        size(ihave)
      stop
   endif

!  allocate neighbour-table

   if (mypart.eq.1) then
      nngb = numprc - 1
   else
      nngb = 1
   endif
   allocate(nghtbl(nngb))

!  fill neighbour-table:
!  master:
!   - receive array 'ihave' from all workers
!   - create interface with numprc-1 neighbours, for receiving all indices
!     from the workers that are provided in the respective ihave-arrays.
!  workers:
!   - send array 'ihave' to master
!   - fill in neighbour-table with 1 neighbour (master), for sending all
!     indices marked "1" in ihave

   if (mypart.eq.1) then

!     Note: assuming that global adressing is used, such that nelem is
!           identical on all processes and that the mask can be transferred
!           without change from one process to another

      allocate(iwork(nelem))

!     loop over all processes, receive ihave-array and fill in recv-area

      do 100 iprc = 2, numprc
         ingb = iprc - 1

!        obtain ihave-array from process iprc

         isrc = iprc - 1
         msgtag = TAG_COLCIF
         call mpi_recv(iwork, nelem, MPI_INTEGER, isrc, msgtag, MPI_COMM_ALL, &
                  mpstat, ierror)
         if (ierror.ne.0) write(LOUT,*) 'intfc_define_collcitf_ihave: ',&
                  'Error in recv=',ierror

!        fill in data for neighbour ingh

         sndrcv => nghtbl(ingb)
         sndrcv%iprc = iprc
         sndrcv%iset = iset
         sndrcv%isnd => NULL()

         nrecv = count(iwork.eq.1)
         allocate(sndrcv%ircv(nrecv))

!        copy masked indices of index-set in iwork to recv-area

         is = 0
         do iel = 1, nelem
            if (iwork(iel).eq.1) then
               is = is + 1
               sndrcv%ircv(is) = iel
            endif
         enddo
         !write(LOUT,*) '  ingb=',ingb,': iprc=',iprc,',', nrecv, &
         !    ' recv-points:'
         !write(LOUT,*) sndrcv%ircv
 100  continue

   else
!     worker-processes
!     send ihave-array to process 1: master

      idest = 0
      msgtag = TAG_COLCIF
      call mpi_send(ihave, nelem, MPI_INTEGER, idest, msgtag, MPI_COMM_ALL, &
               ierror)
      if (ierror.ne.0) write(LOUT,*) 'intfc_define_collcitf_ihave: Error ',&
               'in send=',ierror

!     fill in data for neighbour ingh=1

      sndrcv => nghtbl(1)
      sndrcv%iprc = 1
      sndrcv%iset = iset
      sndrcv%ircv => NULL()

      nsend = count(ihave.eq.1)
      allocate(sndrcv%isnd(nsend))

!     copy masked indices of index-set in ihave to recv-area

      is = 0
      do iel = 1, nelem
         if (ihave(iel).eq.1) then
            is = is + 1
            sndrcv%isnd(is) = iel
         endif
      enddo
      !write(LOUT,*) '  ingb=',ingb,': iprc=',iprc,',', nsend, &
      !    ' send-points:'
      !write(LOUT,*) sndrcv%isnd

   endif

!  Use intfc_define to register the data collected as a new interface

   call intfc_define(namitf, iset, nngb, nghtbl)

   if (use_timers) call timer_stop(itimer_couplib_intfc)

end subroutine intfc_define_collcitf_ihave


!-- SUBROUTINE intfc_define_dstrbitf_owner -----------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Create/register a new communication interface meant for distributing data
!!  from process 1 to workers.
!!
!!  Note: the interface uses the ownership of the index-set for determining
!!        which items must be sent/received. As such there can be only one
!!        such interface per index-set. Internally, the name 'distrib_itf'
!!        is used for this index-set.
!!>
!!  On entry:
!!  namixs      name of index-set to which the interface applies
!!  mypart      own process-id
!!
!!  On return:
!!  -           the new interface has been defined/registered
!!<
!-----------------------------------------------------------------------------
subroutine intfc_define_dstrbitf_owner(namixs, mypart)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in) :: namixs
integer,          intent(in) :: mypart

!-- LOCAL VARIABLES
! (internal) name of the interface that is created
character(len=STRLEN)          :: namitf='distrib_itf'
! work-variable to compose the neighbour-table of the interface
type(t_sndrcv_area), dimension(:), pointer :: nghtbl
type(t_sndrcv_area), pointer               :: sndrcv
! properties of index-set
integer                        :: iset, nelem
integer, dimension(:), pointer :: iowner
! properties of interface
integer                        :: nngb
! work-variables, loop-counters
integer                        :: iprc, ingb
integer                        :: iel, is, nsend, nrecv
! level of debug-output, 0=none
integer                        :: idebug
!-----------------------------------------------------------------------------

   idebug = 0

   if (idebug.ge.1) write(LOUT,*) 'intfc_define_dstrbitf_owner: starting ',&
      'for namitf="', trim(namitf),'" on ixset "',trim(namixs),'"'

!  get handle to index-set

   iset = ixset_hndl(namixs)

!  get pointer to ownership-array of index-set

   call ixset_getprops(iset, nelem=nelem, piownr=iowner)
   if (idebug.ge.2) write(LOUT,*) '  iset=',iset,', nelem=',nelem

!  determine number of neighbours
!   - master: all processes except myself that own one or more points
!   - worker: master when I own one or more points

   if (mypart.eq.1) then
      nngb   = 0
      do iprc = 2, numprc
         if (any(iowner.eq.iprc)) nngb = nngb + 1
      enddo
   else
      nngb = 1
   endif
   if (idebug.ge.2) write(LOUT,*) '  nngb=',nngb

!  construct neighbour-table

   allocate(nghtbl(nngb))

!  fill neighbour-table: for each neighbour fill the send/receive areas
!   - master: send-area for all other processes
!   - worker: receive-area to master

   if (mypart.eq.1) then
!     master: nngb neighbours defined in iowner
!     loop over all processes, fill in send/receive area

      ingb = 0
      do 100 iprc = 2, numprc

!        count number of elements to be sent to process iprc

         nsend = count(iowner.eq.iprc)

!        if > 0: new neighbour found, fill in send/receive area

         if (nsend.gt.0) then
            ingb = ingb + 1

            sndrcv => nghtbl(ingb)
            sndrcv%iprc = iprc
            sndrcv%iset = iset
            sndrcv%ircv => NULL()

            allocate(sndrcv%isnd(nsend))

!           copy neighbours indices of index-set to send-area

            is = 0
            do iel = 1, nelem
               if (iowner(iel).eq.iprc) then
                  is = is + 1
                  sndrcv%isnd(is) = iel
               endif
            enddo
            if (idebug.ge.2) write(LOUT,*) '  ingb=',ingb,': iprc=',iprc,&
               ',', nsend,' send-points'
            if (idebug.ge.5) write(LOUT,*) sndrcv%isnd
         endif
 100  continue

   else
!     worker: 1 neighbour == master
!     fill in trivial properties of send/receive-area for ngb 1 (master)

      sndrcv => nghtbl(1)
      sndrcv%iprc = 1
      sndrcv%iset = iset
      sndrcv%isnd => NULL()

!     count number of elements to be received, allocate recv-area

      nrecv = count(iowner.eq.mypart)
      allocate(sndrcv%ircv(nrecv))

!     copy my own indices of index-set to recv-area

      is = 0
      do iel = 1, nelem
         if (iowner(iel).eq.mypart) then
            is = is + 1
            sndrcv%ircv(is) = iel
         endif
      enddo

      if (idebug.ge.2) write(LOUT,*) '  ingb=1: iprc=1,', nrecv,' recv-points'
      if (idebug.ge.5) write(LOUT,*) sndrcv%ircv
   endif

!  Use intfc_define to register the data collected as a new interface

   call intfc_define(namitf, iset, nngb, nghtbl)

end subroutine intfc_define_dstrbitf_owner


!-- SUBROUTINE intfc_define_dstrbitf_ineed -----------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Create/register a new communication interface meant for sending data
!!  from process 1 to the other processes.
!!>
!!  On entry:
!!  namixs      name of index-set to which the interface applies
!!  namitf      name of the new communication interface
!!  mypart      own-process id
!!  ineed       mask array that indicates which indices the current process
!!              wants to obtain from other processes
!!
!!  On return:
!!  -           the new interface has been defined/registered
!!<
!-----------------------------------------------------------------------------
subroutine intfc_define_dstrbitf_ineed(namixs, namitf, mypart, ineed)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*),      intent(in) :: namixs
character(len=*),      intent(in) :: namitf
integer,               intent(in) :: mypart
integer, dimension(:), intent(in) :: ineed

!-- LOCAL VARIABLES
! work-variable to compose the neighbour-table of the interface
type(t_sndrcv_area), dimension(:), pointer :: nghtbl
type(t_sndrcv_area), pointer               :: sndrcv
! properties of index-set
integer                        :: iset, nelem
! properties of interface
integer                        :: nngb
! work-variables, loop-counters
integer                        :: iprc, ingb
integer                        :: iel, is, nsend, nrecv
! work-array for master to receive ineed of worker
integer, dimension(:), allocatable :: iwork
! help variables for MPI-operations
integer, parameter             :: TAG_DSTRIF = 1001
integer                        :: idest, isrc, msgtag, ierror
integer                        :: mpstat(MPI_STATUS_SIZE)
! level of debug-output, 0=none
integer                        :: idebug=0
!-----------------------------------------------------------------------------

   if (idebug.ge.1) write(LOUT,*) 'intfc_define_dstrbitf_ineed: starting for', &
        ' namitf="', trim(namitf),'" on ixset "',trim(namixs),'"'

!  start timing of interface definition

   if (use_timers) call timer_start(itimer_couplib_intfc)

!  get handle to index-set

   iset = ixset_hndl(namixs)

!  get number of elements of index-set, check size of ineed

   call ixset_getprops(iset, nelem=nelem)
   if (size(ineed).ne.nelem) then
      write(LOUT,*) 'intfc_define_dstrbitf_ineed: Error: size of mask-array ',&
        'ineed is not correct. Should be',nelem,', actual length=',size(ineed)
      stop
   endif

!  allocate neighbour-table

   if (mypart.eq.1) then
      nngb = numprc - 1
   else
      nngb = 1
   endif
   allocate(nghtbl(nngb))

!  fill neighbour-table:
!  master:
!   - receive array 'ineed' from all workers
!   - create interface with numprc-1 neighbours, for sending all indices
!     to the workers that are requested in the respective ineed-arrays.
!  workers:
!   - send array 'ineed' to master
!   - fill in neighbour-table with 1 neighbour (master), delivering all
!     indices marked "1" in ineed

   if (mypart.eq.1) then

!     Note: assuming that global adressing is used, such that nelem is
!           identical on all processes and that the mask can be transferred
!           without change from one process to another

      allocate(iwork(nelem))

!     loop over all processes, receive ineed-array and fill in send-area

      do 100 iprc = 2, numprc
         ingb = iprc - 1

!        obtain ineed-array from process iprc

         isrc = iprc - 1
         msgtag = TAG_DSTRIF
         call mpi_recv(iwork, nelem, MPI_INTEGER, isrc, msgtag, MPI_COMM_ALL, &
                  mpstat, ierror)
         if (ierror.ne.0) write(LOUT,*) 'intfc_define_dstrbitf_ineed: Error ',&
                  'in recv=',ierror

!        fill in data for neighbour ingh

         sndrcv => nghtbl(ingb)
         sndrcv%iprc = iprc
         sndrcv%iset = iset
         sndrcv%ircv => NULL()

         nsend = count(iwork.eq.1)
         allocate(sndrcv%isnd(nsend))

!        copy masked indices of index-set in iwork to send-area

         is = 0
         do iel = 1, nelem
            if (iwork(iel).eq.1) then
               is = is + 1
               sndrcv%isnd(is) = iel
            endif
         enddo
         !write(LOUT,*) '  ingb=',ingb,': iprc=',iprc,',', nsend, &
         !    ' send-points:'
         !write(LOUT,*) sndrcv%isnd
 100  continue

   else
!     worker-processes
!     send ineed-array to process 1: master

      idest = 0
      msgtag = TAG_DSTRIF
      call mpi_send(ineed, nelem, MPI_INTEGER, idest, msgtag, MPI_COMM_ALL, &
               ierror)
      if (ierror.ne.0) write(LOUT,*) 'intfc_define_dstrbitf_ineed: Error in ',&
               'send=',ierror

!     fill in data for neighbour ingh=1

      sndrcv => nghtbl(1)
      sndrcv%iprc = 1
      sndrcv%iset = iset
      sndrcv%isnd => NULL()

      nrecv = count(ineed.eq.1)
      allocate(sndrcv%ircv(nrecv))

!     copy masked indices of index-set in ineed to recv-area

      is = 0
      do iel = 1, nelem
         if (ineed(iel).eq.1) then
            is = is + 1
            sndrcv%ircv(is) = iel
         endif
      enddo
      !write(LOUT,*) '  ingb=',ingb,': iprc=',iprc,',', nrecv, &
      !    ' recv-points:'
      !write(LOUT,*) sndrcv%ircv

   endif

!  Use intfc_define to register the data collected as a new interface

   call intfc_define(namitf, iset, nngb, nghtbl)

   if (use_timers) call timer_stop(itimer_couplib_intfc)

end subroutine intfc_define_dstrbitf_ineed


!-- SUBROUTINE intfc_define_updatitf -----------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Create/register a new communication interface meant for exchanging data
!!  between neighbouring subdomains
!!>
!!  On entry:
!!  namixs      name of index-set to which the interface applies
!!  namitf      name of the new communication interface
!!  ineed       mask array that indicates which indices the current process
!!              wants to obtain from other processes
!!  idebug      optional: level of output requested (0=none)
!!
!!  On return:
!!  -           the new interface has been defined/registered
!!<
!-----------------------------------------------------------------------------
subroutine intfc_define_updatitf(namixs, namitf, ineed, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*),      intent(in)           :: namixs
character(len=*),      intent(in)           :: namitf
integer, dimension(:), intent(in)           :: ineed
integer,               intent(in), optional :: idebug

!-- LOCAL VARIABLES
! properties of index-set
integer                        :: iset, nelem
integer, dimension(:), pointer :: iowner
! properties of interface
integer                        :: nngb
! work-variable to compose the neighbour-table of the interface
type(t_sndrcv_area), dimension(:), pointer :: nghtbl, nghtbl2
type(t_sndrcv_area), pointer               :: sndrcv
! work-variables, loop-counters
integer                        :: iprc, ingb
integer                        :: iel, is, nsend, nrecv, nitm
! work-array to receive receive-area of neighbour, send-area for us
integer, dimension(:), pointer :: iwork
! help variables for MPI-operations
integer, parameter             :: TAG_UPDTIF = 1002
integer                        :: idest, isrc, msgtag, ierror
integer                        :: mpstat(MPI_STATUS_SIZE)
! internal value of idebug
integer                        :: my_idebug=0
!-----------------------------------------------------------------------------

!  handle optional idebug

   my_idebug = 0
   if (present(idebug)) my_idebug = idebug

   if (my_idebug.ge.1) write(LOUT,*) 'intfc_define_updatitf: starting for ', &
        'namitf="', trim(namitf),'" on ixset "',trim(namixs),'"'

!  start timing of interface definition

   if (use_timers) call timer_start(itimer_couplib_intfc)

!  get handle to index-set

   iset = ixset_hndl(namixs)
   if (my_idebug.ge.10) write(LOUT,*) 'intfc_define_updatitf: iset=',iset

!  get number of elements of index-set, check size of ineed

   call ixset_getprops(iset, nelem=nelem, piownr=iowner)
   if (size(ineed).ne.nelem) then
      write(LOUT,*) 'intfc_define_updatitf: Error: size of mask-array ineed',&
        ' is not correct. Should be',nelem,', actual length=',size(ineed)
      stop
   endif

!  check whether ownership is filled in correctly for all elements needed by the
!  current process.

   do is = 1, nelem
      if (ineed(is).gt.0) then
         if (iowner(is).le.0 .or. iowner(is).gt.numprc) then
            write(LOUT,*) 'intfc_define_updatitf: Error: owner',iowner(is),&
               ' for index',is,' is out of range [1..numprc=',numprc,'].'
            stop
         endif
      endif
   enddo

!  allocate neighbour-table with maximum size; all processes might be neighbours

   nngb = numprc
   allocate(nghtbl(nngb))

!  fill neighbour-table:
!  workers:
!   - send a message to each process, containing the number of indices needed
!     of that process and the indices that are needed
!   - store the elements requested in the receive-area
!   - receive a message from all processes
!   - store the elements requested by other processes in the send-area

!  loop over all processes, send away our receive-area for each process

   ingb = 0
   do iprc = 1, numprc

!     initialize sendrecv-area for process iprc

      sndrcv => nghtbl(iprc)
      sndrcv%iprc = iprc
      sndrcv%iset = iset
      sndrcv%isnd => NULL()
      sndrcv%ircv => NULL()

      if (iprc.ne.myprc) then
         if (my_idebug.ge.10) write(LOUT,*) 'intfc_define_updatitf: starting ',&
            'send iprc=',iprc

         idest = iprc - 1
         msgtag = TAG_UPDTIF

!        if number of elements delivered by iprc is 0

         nrecv = count(ineed.eq.1 .and. iowner.eq.iprc)
         if (nrecv.le.0) then

!           - send message "no elements needed" (IVOID, negative value)

            if (my_idebug.ge.5) write(LOUT,*) '    sending IVOID to iprc=',iprc
            call mpi_send(IVOID, 1, MPI_INTEGER, idest, msgtag, &
                          MPI_COMM_ALL, ierror)
            if (ierror.ne.0) write(LOUT,*) 'intfc_define_updatitf: Error in ',&
               'send=',ierror

         else
            allocate(sndrcv%ircv(nrecv))

!            - copy masked indices of index-set in ineed to recv-area

            is = 0
            do iel = 1, nelem
               if (ineed(iel).eq.1 .and. iowner(iel).eq.iprc) then
                  is = is + 1
                  sndrcv%ircv(is) = iel
               endif
            enddo

!            - send ircv-array to process iprc

            if (my_idebug.ge.5) write(LOUT,*) '    sending recv-area to iprc=',&
                iprc,':',sndrcv%ircv
            call mpi_send(sndrcv%ircv, nrecv, MPI_INTEGER, idest, msgtag, &
                          MPI_COMM_ALL, ierror)
            if (ierror.ne.0) write(LOUT,*) 'intfc_define_updatitf: Error in ',&
               'send=',ierror
         endif
      endif
   enddo ! iprc

!  loop over all processes, obtain receive-area per process, store as send-area

   do iprc = 1, numprc
      if (iprc.ne.myprc) then
         if (my_idebug.ge.10) write(LOUT,*) 'intfc_define_updatitf: starting ',&
            'recv iprc=',iprc

!        obtain message from process iprc in newly allocated iwork

         isrc = iprc - 1
         msgtag = TAG_UPDTIF

         call mpi_probe(isrc, msgtag, MPI_COMM_ALL, mpstat, ierror)
         if (ierror.ne.0) write(LOUT,*) 'intfc_define_updatitf: Error in ',&
            'probe=',ierror

         call mpi_get_count(mpstat, MPI_INTEGER, nitm, ierror)
         if (my_idebug.ge.5) write(LOUT,*) 'intfc_define_updatitf: get_count:',&
             ' nitm=',nitm, ', ierror=',ierror
         allocate(iwork(nitm))

         call mpi_recv(iwork, nitm, MPI_INTEGER, isrc, msgtag, MPI_COMM_ALL, &
                  mpstat, ierror)
         if (ierror.ne.0) write(LOUT,*) 'intfc_define_updatitf: Error in ',&
                  'recv=',ierror
         if (my_idebug.ge.5) write(LOUT,*) '   obtained recv-area of iprc=',&
            iprc,':',iwork

!        if process does not request data (first entry is IVOID)

         if (iwork(1).eq.IVOID) then

!           - clean-up, deallocate iwork

            if (my_idebug.ge.3) write(LOUT,*) 'intfc_define_updatitf: ',&
               'received IVOID=',IVOID,' from process',iprc
            deallocate(iwork)


         else

!        else
!           - set send-area => iwork, set iwork => NULL

            sndrcv => nghtbl(iprc)
            sndrcv%isnd => iwork
            iwork => NULL()
         endif
      endif ! iprc.ne.myprc
   enddo ! iprc

!  Create restricted list of actual neighbours

   if (my_idebug.ge.10) write(LOUT,*) 'intfc_define_updatitf: counting nngb'
   nngb = 0
   do iprc = 1, numprc
      if (associated(nghtbl(iprc)%isnd) .or. associated(nghtbl(iprc)%ircv)) &
         nngb = nngb + 1
   enddo
   if (my_idebug.ge.10) write(LOUT,*) 'intfc_define_updatitf: nngb=',nngb

   allocate(nghtbl2(nngb))
   ingb = 0
   do iprc = 1, numprc
      if (associated(nghtbl(iprc)%isnd) .or. associated(nghtbl(iprc)%ircv)) then
         ingb = ingb + 1
         sndrcv => nghtbl2(ingb)
         nghtbl2(ingb)%iprc =  nghtbl(iprc)%iprc
         nghtbl2(ingb)%iset =  nghtbl(iprc)%iset
         nghtbl2(ingb)%isnd => nghtbl(iprc)%isnd
         nghtbl2(ingb)%ircv => nghtbl(iprc)%ircv
      endif
   enddo
   deallocate(nghtbl)

!  Use intfc_define to register the data collected as a new interface

   if (my_idebug.ge.10) write(LOUT,*) 'intfc_define_updatitf: calling ',&
      'intfc_define'
   call intfc_define(namitf, iset, nngb, nghtbl2)

   if (use_timers) call timer_stop(itimer_couplib_intfc)

end subroutine intfc_define_updatitf


end module m_intfc
