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

!-- MODULE m_ixset -----------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Definition of data-structures and subroutines regarding index-sets
!   of the CouPLib communication library
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/m_ixset.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------
module m_ixset
use m_timings
use m_coupcns
implicit none
private

! subroutines for configuration of index-sets:

public ixset_initmod
public ixset_hndl
public ixset_getprops
public ixset_print
public ixset_define
public ixset_product

interface ixset_print
   module procedure ixset_print_namixs
   module procedure ixset_print_iset
end interface ixset_print


! definition of one index-set:
type    :: t_ixset
   character(len=STRLEN) :: namixs   !! name of the index-set
   integer               :: nelem    !! total number of elements of this index-
                                     !! set within the current process
!
!  Index-sets may be "stand-alone" or Cartesian "products" of stand-alone
!  index-sets. For product-sets, only the references to the factors are stored.
!  The actual data regarding ownership and so on is stored with the factors.
!
   integer               :: nfac     !! number of component index-sets in
                                     !! Cartesian product. Must be <= 4.
                                     !!   1 == stand alone index-set
                                     !!  >1 == true Cartesian product set
   integer, dimension(:), pointer :: ifctrs   !! handles of factors of component
                                     !! index-sets in case of Cartesian products
                                     !! Standalone ixset: handle of ixset itself
   integer               :: ifcdst   !! handle of index-set for the factor that
                                     !! is distributed, or IVOID if all factors
                                     !! are replicated
!
!  For stand-alone index-sets, ownership may be defined, global coordinates or
!  labels, and local storage coordinates.
!
   integer, dimension(:), pointer :: iowner  !! ownership of the indices
                                     !! of the index set, i.e. for each index
                                     !! the number of the process that is
                                     !! owner of the index
!
   integer               :: ndimgl   !! number of dimensions of the global
                                     !! coordinates
   integer, dimension(:), pointer :: icglob  !! global coordinates (labels)
                                     !! of the indices of the index set
!
   integer               :: ndimdt   !! number of dimensions of (storage)
                                     !! array corresponding to the index-set
   integer, dimension(:), pointer :: locsiz  !! size of the corresponding
                                     !! array in each dimension (ndimdt)
   integer, dimension(:), pointer :: icloc   !! storage coordinates (array
                                     !! offsets) of the indices of the index
                                     !! set within the corresponding array
end type t_ixset


! definition of a subset of an index-set
type    :: t_subset
   integer                        :: iset  !! index-set of which a subset is
                                           !! declared (index in indset)
   integer, dimension(:), pointer :: ilist !! array enumerating the indices of
                                           !! index-set contained in the subset
end type t_subset


! definition of the table of index-sets: using a pointer, allowing the table
! to be re-allocated when the capacity is exceeded
integer                              :: nindst = 0
type(t_ixset), dimension(:), pointer :: indset => NULL()


contains


!-- SUBROUTINE ixset_initmod -------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Initialize the CouPLib data-structures w.r.t. index-sets
!!>
!!  On entry:
!!  -
!!
!!  On return:
!!  -           the data-structures for index-sets have been initialized
!!<
!-----------------------------------------------------------------------------
subroutine ixset_initmod()

!-- HEADER VARIABLES/ARGUMENTS
implicit none

!-- LOCAL VARIABLES
!-----------------------------------------------------------------------------
   nindst = 0
   allocate(indset(INIT_LENGTH))

end subroutine ixset_initmod


!-- FUNCTION ixset_hndl ------------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Get a handle to a named index-set
!!>
!!  On entry:
!!  namixs      name of the index-set to be found
!!  ierr        optional: desired behaviour when interface is not found
!!              (positive values of ierr) or is found (negative ierr)
!!               IFATAL: error and stop when not found (default)
!!               IWARN : warning message when not found
!!               INONE : continue as much as possible without warning
!!              -IWARN : warning message when found
!!              -IFATAL: error and stop message when found
!!  On return:
!!  iset        if found: handle to the index-set, IDUNNO otherwise
!!<
!-----------------------------------------------------------------------------
function ixset_hndl(namixs, ierr)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in)            :: namixs
integer,          intent(in), optional  :: ierr
integer                                 :: ixset_hndl

!-- LOCAL VARIABLES
! loop counter for index-sets
integer                 :: iset
logical                 :: found
! local variable for ierr
integer                 :: iierr
! level of debug-output, 0=none
integer                 :: idebug=0
!-----------------------------------------------------------------------------

   if (present(ierr)) then
      iierr = ierr
   else
      iierr = IFATAL
   endif

   if (idebug.ge.1) write(LOUT,*) 'ixset_hndl: searching ixset="',&
        trim(namixs),'", nindst=',nindst

   iset = 0
   found = .false.
   do while (.not.found .and. iset.lt.nindst)
      iset = iset + 1
      if (namixs.eq.indset(iset)%namixs) found = .true.
      if (idebug.ge.10) write(LOUT,*) 'ixset_hndl: iset',iset,' is "', &
         trim(indset(iset)%namixs),'", found=',found
   end do

!  Report result, return appropriate handle iset or IDUNNO

   if (found) then
      if (idebug.ge.3) write(LOUT,*) '...found, iset=',iset
      if (iierr.eq.-IWARN .or. iierr.eq.-IFATAL) then
         write(LOUT,*) 'ixset_hdnl: an index-set with name "',trim(namixs),&
             '" was found but should not (yet) exist.'
      endif
      if (iierr.eq.-IFATAL) stop
      ixset_hndl = iset
   else
      if (iierr.eq.IWARN .or. iierr.eq.IFATAL) then
         write(LOUT,*) 'ixset_hndl: Error: cannot find index-set with name="', &
                 trim(namixs),'"'
      endif
      if (iierr.eq.IFATAL) stop
      ixset_hndl = IDUNNO
   endif
   if (idebug.ge.2) write(LOUT,*) 'ixset_hndl: returning hndl=',ixset_hndl

end function ixset_hndl


!-- SUBROUTINE ixset_getprops ------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Return the properties of an index-set
!!>
!!  On entry:
!!  iset        optional: handle of index-set for which properties are required
!!
!!  On return:
!!  namixs      optional: name of the index-set
!!  nelem       optional: number of elements of the index-set
!!  nfac        optional: number of component index-sets in Cartesian product.
!!               =1 for stand alone index-set, >1 for true Cartesian product set
!!  pfctrs      optional: pointer to list of handles of factors of component
!!              index-sets in case of Cartesian products.
!!  ifcdst      optional: handle of index-set for the factor that is
!!              distributed, or IVOID if all factors are replicated
!!  piownr      optional: pointer to ownership of the indices of the index set,
!!              i.e. for each index the number of the process that is owner of
!!              the index
!!  ndimgl      optional: number of dimensions of the global coordinates
!!  picglb      optional: pointer to global coordinates (labels) of the indices
!!              of the index set
!!  ndimdt      optional: number of dimensions of the (storage) array
!!              corresponding to the index-set
!!  plocsz      optional: pointer to array with size of the corresponding array
!!              in each dimension
!!  picloc      optional: pointer to storage coordinates (array offsets) of the
!!              indices of the index set within the corresponding array
!!<
!-----------------------------------------------------------------------------
subroutine ixset_getprops(iset, namixs, nelem, nfac, pfctrs, ifcdst, piownr, &
                          ndimgl, picglb, ndimdt, plocsz, picloc)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer,                        intent(in)            :: iset
character(len=*),               intent(out), optional :: namixs
integer,                        intent(out), optional :: nelem
integer,                        intent(out), optional :: nfac
integer, dimension(:), pointer,              optional :: pfctrs
integer,                        intent(out), optional :: ifcdst
integer, dimension(:), pointer,              optional :: piownr
integer,                        intent(out), optional :: ndimgl
integer, dimension(:), pointer,              optional :: picglb
integer,                        intent(out), optional :: ndimdt
integer, dimension(:), pointer,              optional :: plocsz
integer, dimension(:), pointer,              optional :: picloc

!-- LOCAL VARIABLES
! pointer to current index-set
type(t_ixset), pointer  :: set
!-----------------------------------------------------------------------------

!  Check input-argument, set pointer to appropriate index-set

   if (iset.le.0 .or. iset.gt.nindst) then
      write(LOUT,*) 'ixset_getprops: Error: handle to indexset',iset,&
                    ' out of range 1..',nindst
      stop
   endif

   set => indset(iset)

!  Return name, nelem when requested

   if (present(namixs)) namixs =  set%namixs
   if (present(nelem))  nelem  =  set%nelem

!  Return data for product-set when requested

   if (present(nfac))   nfac   =  set%nfac
   if (present(pfctrs)) pfctrs => set%ifctrs
   if (present(ifcdst)) ifcdst =  set%ifcdst

!  Return ownership when requested

   if (present(piownr)) piownr => set%iowner

!  Return global coordinates/labels when requested

   if (present(ndimgl)) ndimgl =  set%ndimgl
   if (present(picglb)) picglb => set%icglob

!  Return local/storage coordinates when requested

   if (present(ndimdt)) ndimdt =  set%ndimdt
   if (present(plocsz)) plocsz => set%locsiz
   if (present(picloc)) picloc => set%icloc

end subroutine ixset_getprops


!-- SUBROUTINE ixset_print_namixs --------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Display the properties of a named index-set
!!>
!!  On entry:
!!  namixs      name of index-set to be displayed
!!  idebug      optional: level of output requested (0=none)
!!
!!  On return:
!!  -           the properties of the index-set are written to the screen
!!<
!-----------------------------------------------------------------------------
subroutine ixset_print_namixs(namixs, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*), intent(in)            :: namixs
integer,          intent(in), optional  :: idebug

!-- LOCAL VARIABLES
! handle of index-set
integer                 :: iset
! internal value of idebug
integer                 :: my_idebug
!-----------------------------------------------------------------------------

!  handle optional idebug

   my_idebug = 1
   if (present(idebug)) my_idebug = idebug

!  look up the index-set 'namixs' in the list of index-sets indset

   iset = ixset_hndl(namixs, IWARN)
   if (iset.eq.IDUNNO) return

   call ixset_print_iset(iset, my_idebug)

end subroutine ixset_print_namixs


!-- SUBROUTINE ixset_print_iset ----------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Display the properties of an index-set
!!>
!!  On entry:
!!  iset        handle of the index-set to be displayed
!!  idebug      optional: level of output requested (0=none)
!!
!!  On return:
!!  -           the properties of the index-set are written to the screen
!!<
!-----------------------------------------------------------------------------
subroutine ixset_print_iset(iset, idebug)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
integer, intent(in)            :: iset
integer, intent(in), optional  :: idebug

!-- LOCAL VARIABLES
! internal value of idebug
integer                 :: my_idebug
! pointer to current index-set
type(t_ixset), pointer  :: set
! number of factors, factors of product set
integer                 :: nfac, ifac
integer, dimension(:)   :: ifctrs(4)
! format-string for writing data for one element of ixset
character(len=80)       :: fmt, tmpstr
! work-array for gathering data for one index-set element
integer, dimension(:)   :: iwork(10)
integer                 :: iel, ioffs
!-----------------------------------------------------------------------------

!  handle optional idebug

   my_idebug = 1
   if (present(idebug)) my_idebug = idebug

!  Check input-argument, set pointer to appropriate index-set

   if (iset.le.0 .or. iset.gt.nindst) then
      write(LOUT,*) 'ixset_print: Error: handle to index-set',iset,&
                    ' out of range 1..',nindst
      stop
   endif

   set => indset(iset)

!  Print information about structure of index-set iset, whether it is a product
!  or not.

   if (my_idebug.ge.1) then
      if (associated(set%iowner)) then
         tmpstr = 'distributed'
      else
         tmpstr = 'replicated'
      endif
      if (set%nfac.gt.1) then

!        Cartesian product-set:

         write(LOUT,'(3a,i2,a,i8,2a)') ' index-set "',trim(set%namixs), &
            '": product of',set%nfac,' factors, in total',set%nelem, &
            ' elements, ', trim(tmpstr)

      else

!        stand-alone index-set or factor in product:

         write(LOUT,'(3a,i8,2a)') ' index-set "', trim(set%namixs), &
            '": stand-alone,', set%nelem,' elements, ', trim(tmpstr)

      endif
   endif

!  Print information on the factors of the index-set or the index-set itself in
!  case of a stand-alone ixset.

   nfac = set%nfac
   ifctrs(1:nfac) = set%ifctrs(1:nfac)

   do ifac = 1, nfac

!     set pointer in indset to factor ifac

      set => indset(ifctrs(ifac))

!     write header when not done above with global structure

      if (my_idebug.ge.1 .and. nfac.gt.1) then
         if (associated(set%iowner)) then
            tmpstr = 'distributed'
         else
            tmpstr = 'replicated'
         endif
         write(LOUT,'(a,i2,3a,i8,2a)') '   factor',ifac,' is index-set "', &
            trim(set%namixs), '": stand-alone,', set%nelem,' elements, ', &
            trim(tmpstr)
      endif

!     compose format-string for writing information per index

      if (my_idebug.ge.2) then
         fmt = '(''       index'',i8,'':'''
         if (associated(set%iowner)) then
            write(LOUT,*) '    ownership defined: distributed set'
            fmt = trim(fmt) // ','' own='',i3'
         else
            write(LOUT,*) '    no ownership: replicated set'
         endif
         if (set%ndimgl.gt.0) then
            write(LOUT,*) '    global coordinates:',set%ndimgl,' dimensions'
            write(fmt,'(a,a,i1,a)') trim(fmt), ','' glob='',', set%ndimgl,'i8'
         else
            write(LOUT,*) '    global coordinates: identity'
         endif
         if (set%ndimdt.gt.0) then
            write(LOUT,*) '    local coordinates:',set%ndimdt,' dimensions'
            write(fmt,'(a,a,i1,a)') trim(fmt), ','' loc='',', set%ndimdt,'i8'
         else
            write(LOUT,*) '    local coordinates: identity'
         endif
         fmt = trim(fmt) // ')'
         !write(LOUT,*) 'fmt=',trim(fmt)

!        write information on the elements of factor ifac

         do iel = 1, set%nelem
            iwork(1) = iel
            ioffs = 1
            if (associated(set%iowner)) then
               iwork(ioffs+1) = set%iowner(iel)
               ioffs = ioffs + 1
            endif
            if (set%ndimgl.gt.0) then
               iwork(ioffs+1:ioffs+set%ndimgl) = -1
               ioffs = ioffs + set%ndimgl
            endif
            if (set%ndimdt.gt.0) then
               iwork(ioffs+1:ioffs+set%ndimdt) = -1
               ioffs = ioffs + set%ndimdt
            endif

            write(LOUT,fmt) iwork(1:ioffs)
         enddo ! iel
      endif ! idebug

   enddo ! ifac

end subroutine ixset_print_iset


!-- SUBROUTINE ixset_define --------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Define a new named index-set for use in communications later on.
!!>
!!  On entry:
!!  namixs      name of the index-set to be registered
!!  nelem       number of elements of the new index-set
!!  iowner      ownership of the indices of the index set, i.e. for each
!!              index the number of the process that is owner of the index
!!  ndimgl      number of dimensions of the global coordinates
!!  icglob      global coordinates (labels) of the indices of the index set
!!  ndimdt      number of dimensions of the (storage) array corresponding
!!              to the index-set
!!  locsiz      size of the corresponding array in each dimension
!!  icloc       storage coordinates (array offsets) of the indices of the
!!              index set within the corresponding array
!!
!!  On return:
!!  -           the data describing the index-set has been stored in the
!!              internal data-structures of this module
!!<
!-----------------------------------------------------------------------------
subroutine ixset_define(namixs, nelem, iowner, &
                        ndimgl, icglob, ndimdt, locsiz, icloc)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*),      intent(in)           :: namixs
integer,               intent(in)           :: nelem
integer, dimension(:), intent(in), optional :: iowner(:)
integer,               intent(in), optional :: ndimgl
integer, dimension(:), intent(in), optional :: icglob(:)
integer,               intent(in), optional :: ndimdt
integer, dimension(:), intent(in), optional :: locsiz(:)
integer, dimension(:), intent(in), optional :: icloc(:)

!-- LOCAL VARIABLES
! pointer to new entry in table of index-sets
integer                              :: iset
type(t_ixset), pointer               :: new
! help-variable to re-allocate table of index-sets
type(t_ixset), dimension(:), pointer :: wrk_indset
!-----------------------------------------------------------------------------

   !write(LOUT,*) 'ixset_define: starting for namixs="',trim(namixs),'"'

!  Verify that the CouPLib datastructures have been initialized

   if (.not.associated(indset)) then
      write(LOUT,*) 'ixset_define: Error: CouPLib data-structures have not ', &
                 'been initialized properly!'
      stop
   endif

!  start timing of indexset definition

   if (use_timers) call timer_start(itimer_couplib_ixset)

!  check that name 'namixs' does not yet exist in table of index-sets

   iset = ixset_hndl(namixs, INONE)
   if (iset.ne.IDUNNO) then
      write(LOUT,*) 'ixset_define: Error: index-set "',trim(namixs), &
                 '" already exists.'
      stop
   endif

!  Check the restrictions of this routine
!   - if ownership is given, size must match nelem
!   - global coordinates (labels) are not yet supported
!   - storage (local) coordinates are not yet supported

   !write(LOUT,*) 'ixset_define: check present(parameters)...'

   if (present(iowner)) then
      if (size(iowner).ne.nelem) then
         write(LOUT,*) 'ixset_define: Error: size of ownership-array is not ', &
            'correct. Should be nelem=',nelem,' elements long, actual size=', &
            size(iowner,1),' elements.'
         stop
      endif
   endif
   if (present(ndimgl) .or. present(icglob)) then
      write(LOUT,*) 'ixset_define: Error: global coordinates (labels) are ', &
         'not yet supported'
      stop
   endif
   if (present(ndimdt) .or. present(locsiz) .or. present(icloc)) then
      write(LOUT,*) 'ixset_define: Error: local (storage) coordinates are ', &
         'not yet supported'
      stop
   endif

!  Re-allocate the indset-table when needed

   if (nindst.ge.size(indset)) then
      write(LOUT,*) 'ixset_define: current capacity of indset exceeded, ', &
                 're-allocating with size=', nint(size(indset)*GROW_LENGTH)
      allocate(wrk_indset(nint(size(indset)*GROW_LENGTH)))
      wrk_indset(1:size(wrk_indset)) = indset
      deallocate(indset)
      indset => wrk_indset
      nullify(wrk_indset)
   endif

!  Add a new index-set

   nindst = nindst + 1
   new => indset(nindst)

!  Register the name, number of elements

   new%namixs = namixs
   new%nelem  = nelem

!  Index-set is not a product-set.
!  Register number of factors=1, first factor is index-set itself,
!  distributed factor is index-set itself if ownership is defined.

   new%nfac = 1
   allocate(new%ifctrs(1))
   new%ifctrs(1) = nindst
   if (.not.present(iowner)) then
      new%ifcdst = IVOID
   else
      new%ifcdst = nindst
   endif

!  Register the ownership of the elements

   if (.not.present(iowner)) then
      new%iowner => NULL()
   else
      allocate(new%iowner(nelem))
      new%iowner(1:nelem) = iowner(1:nelem)
   endif

!  Register the global coordinates (labels)

   if (.not.present(ndimgl) .or. .not.present(icglob)) then
      new%ndimgl = 0
      new%icglob => NULL()
   else
      new%ndimgl = ndimgl
      allocate(new%icglob(nelem))
      new%icglob(1:nelem) = icglob(1:nelem)
   endif

!  Register the (local) storage coordinates

   if (.not.present(ndimdt) .or. .not.present(locsiz) .or. &
                                                  .not.present(icloc)) then
      new%ndimdt = 0
      new%locsiz => NULL()
      new%icloc  => NULL()
   else
      new%ndimdt = ndimdt
      allocate(new%locsiz(ndimdt))
      new%locsiz(1:ndimdt) = locsiz(1:ndimdt)
      allocate(new%icloc(nelem))
      new%icloc(1:nelem) = icloc(1:nelem)
   endif

   if (use_timers) call timer_stop(itimer_couplib_ixset)

end subroutine ixset_define


!-- SUBROUTINE ixset_product -------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Description of the purpose of the routine.
!!>
!!  On entry:
!!  namprd      name of new product-set
!!  namix1      name of index-set that is first factor in product-set
!!  namix2      name of index-set that is second factor in product-set
!!  namix3      optional: name of third factor in product-set
!!  namix4      optional: name of fourth factor in product-set
!!
!!  On return:
!!  -           the product-set has been registered
!!<
!-----------------------------------------------------------------------------
subroutine ixset_product(namprd, namix1, namix2, namix3, namix4)

!-- HEADER VARIABLES/ARGUMENTS
implicit none
character(len=*),      intent(in)           :: namprd
character(len=*),      intent(in)           :: namix1
character(len=*),      intent(in)           :: namix2
character(len=*),      intent(in), optional :: namix3
character(len=*),      intent(in), optional :: namix4

!-- LOCAL VARIABLES
! help-variable to re-allocate table of index-sets
type(t_ixset), dimension(:), pointer :: wrk_indset
! pointer to new entry in table of index-sets
type(t_ixset), pointer               :: new
! handles to index-sets
integer                              :: iprd, iset(4), nfac, ifac
! properties of index-sets
integer                              :: nelem, ndimgl, ndimdt, nfacsb
integer, dimension(:), pointer       :: piownr
! level of debug output (0=none)
integer                              :: idebug=0
!-----------------------------------------------------------------------------

   if (idebug.ge.1) then
      write(LOUT,*) 'ixset_product: starting for namprd="',trim(namprd), &
         '", factors="',trim(namix1),'", "',trim(namix2),'"'
      if (present(namix3)) write(LOUT,*) '    "',trim(namix3),'"'
      if (present(namix4)) write(LOUT,*) '    "',trim(namix4),'"'
   endif

!  start timing of indexset definition

   if (use_timers) call timer_start(itimer_couplib_ixset)

!  check that name 'namprd' does not yet exist in table of index-sets

   iprd = ixset_hndl(namprd, INONE)
   if (iprd.ne.IDUNNO) then
      write(LOUT,*) 'ixset_product: Error: index-set "',trim(namprd), &
                 '" already exists.'
      stop
   endif

!  get handles to index-sets that are factors in the product

   iset(1) = ixset_hndl(namix1, IFATAL)
   iset(2) = ixset_hndl(namix2, IFATAL)
   nfac = 2
   if (present(namix3)) then
      nfac = nfac + 1
      iset(nfac) = ixset_hndl(namix3, IFATAL)
   endif
   if (present(namix4)) then
      nfac = nfac + 1
      iset(nfac) = ixset_hndl(namix4, IFATAL)
   endif
   if (idebug.ge.5) write(LOUT,*) 'nfac=',nfac,', iset=',iset

!  Re-allocate the indset-table when needed

   if (nindst.ge.size(indset)) then
      write(LOUT,*) 'ixset_define: current capacity of indset exceeded, ', &
                 're-allocating with size=', nint(size(indset)*GROW_LENGTH)
      allocate(wrk_indset(nint(size(indset)*GROW_LENGTH)))
      wrk_indset(1:size(wrk_indset)) = indset
      deallocate(indset)
      indset => wrk_indset
      nullify(wrk_indset)
   endif

!  Add a new index-set

   nindst = nindst + 1
   new => indset(nindst)

!  Register the name, initialize the number of elements and dimensions of
!  global/storage coordinates

   new%namixs = namprd
   new%nelem  = 1
   new%ndimgl = 0
   new%ndimdt = 0

!  Register the number of factors and the handles of the factors

   new%nfac   = nfac
   allocate(new%ifctrs(nfac))
   new%ifctrs = iset(1:nfac)
   new%ifcdst = IVOID

!  For all factors do:

   do ifac = 1, nfac

!      - get properties of the factor

      call ixset_getprops(iset(ifac), nfac=nfacsb, nelem=nelem, piownr=piownr, &
                          ndimgl=ndimgl, ndimdt=ndimdt)
      if (idebug.ge.3) write(LOUT,'(a,i2,a,i6,3(a,i2))') ' factor',ifac, &
         ': nelem=',nelem,', nfac=',nfacsb,', ndimgl=',ndimgl,', ndimdt=',ndimdt

      if (nfacsb.gt.1) then
         write(LOUT,*) 'ixset_product: Error: in the definition of product-',&
               'set "',trim(namprd),'" factor',ifac,' is already a product of',&
               nfacsb,' factors, which is not allowed.'
         stop
      endif

!      - adjust properties of the product-set

      new%nelem  = new%nelem  * nelem
      new%ndimgl = new%ndimgl + ndimgl
      new%ndimdt = new%ndimdt + ndimdt

      if (associated(piownr)) then
         if (new%ifcdst.ne.IVOID) then
            write(LOUT,*) 'ixset_product: Error: second distributed factor ',&
               'found in definition of product-set "',trim(namprd),'". ',&
               'At most one factor of a product may be a distributed index-set.'
            stop
         else
            new%ifcdst = iset(ifac)
         endif
      endif
   enddo ! ifac

   if (use_timers) call timer_stop(itimer_couplib_ixset)

end subroutine ixset_product


end module m_ixset
