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

!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/reshape_rdata.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------

!-- SUBROUTINE reshape_rdata -------------------------------------------------
!   Author:             Edwin Vollebregt (VORtech)
!-- DESCRIPTION --------------------------------------------------------------
!!
!!  Reshape a REAL field: fill part of array FLDOUT using values from array
!!  FLDIN, possibly using indirect adressing.
!!
!!  The precise operation to be performed is called the `inner loop type'
!!  (variable inrlp):
!!    1. copy: fill entire output-field using entire input-field
!!       (without use of itin and itout)
!!    2. gather: fill entire output-field using part of input-field
!!       (e.g. fill send-buffer; itin defines the selected part of fldin)
!!    3. scatter-replace: fill part of output-field using entire input-field
!!       (e.g. store receive-buffer; itout defines selected part of fldout)
!!    4. scatter-add: modify part of output-field using entire input-field
!!       (e.g. add contents of receive-buffer to existing values of output-
!!        field; itout defines selected part of fldout)
!!>
!!  On entry:
!!  inrlp       type of operation to be performed
!!  fldin       input field, values that are to be copied/added to the
!!              output-field
!!  nin1        size of fldin in first dimension
!!  nin2        size of fldin in second dimension
!!  nin3        size of fldin in third dimension
!!  idimin      number of "distributed dimension" for input-field, i.e. the
!!              dimension to which itin applies
!!  itin        table containing the indices of the input-field to be used
!!  ntin        size of table ntin
!!  nout1       size of fldout in first dimension
!!  nout2       size of fldout in second dimension
!!  nout3       size of fldout in third dimension
!!  idmout      number of "distributed dimension" for output-field, i.e. the
!!              dimension to which itout applies
!!  itout       table containing the indices of the output-field to be written
!!  ntout       size of table itout
!!  idebug      level of debug-output, 0=none
!!
!!  On return:
!!  fldout      output field, completely or partly overwritten or modified
!!              with values from fldin
!!<
!-----------------------------------------------------------------------------
subroutine reshape_rdata(inrlp, &
                         fldin, nin1, nin2, nin3, idimin, itin, ntin, &
                         fldout, nout1, nout2, nout3, idmout, itout, ntout, &
                         idebug)

!-- HEADER VARIABLES/ARGUMENTS
use m_coupcns
implicit none
integer,                        intent(in)  :: inrlp
integer,                        intent(in)  :: nin1, nin2, nin3
real(kind=4), dimension(:,:,:), intent(in)  :: fldin(nin1,nin2,nin3)
integer,                        intent(in)  :: idimin
integer,                        intent(in)  :: ntin
integer,      dimension(:),     intent(in)  :: itin(ntin)
integer,                        intent(in)  :: nout1, nout2, nout3
real(kind=4), dimension(:,:,:), intent(out) :: fldout(nout1,nout2,nout3)
integer,                        intent(in)  :: idmout
integer,                        intent(in)  :: ntout
integer,      dimension(:),     intent(in)  :: itout(ntout)
integer,                        intent(in)  :: idebug

!-- LOCAL VARIABLES
! loop counters for 1st, 2nd and 3rd dimension
integer :: i1, i2, i3
! array for checking sizes of arrays fldin, itin, fldout and itout
integer :: isize(3,2)
! definitions for inner-loops
integer, parameter              :: num_inrlp = 4
character(len=20), dimension(:) :: inrlp_names(num_inrlp) = &
             (/'copy', 'gather', 'scatter-replace', 'scatter-add' /)
!-----------------------------------------------------------------------------

!  print debug-information on the operation to be performed

   if (idebug.ge.2) then
      write(LOUT,'(a,i2,3a)') ' reshape_data: starting for inrlp=',inrlp,&
         ' ("', trim(inrlp_names(inrlp)),'"); real data'
      write(LOUT,'(a,3i8)') '   input-array: size=',nin1,nin2,nin3
      if (idebug.ge.10) then
         write(LOUT,*) '    input-values:'
         write(LOUT,*) '    ',fldin
      endif
      if (inrlp.eq.2) then
         write(LOUT,'(a,i2,a,i6)') '     table itin applies to dim=',idimin, &
                ' and has size=',ntin
         if (idebug.ge.10) then
            write(LOUT,*) '    table itin selects entries'
            write(LOUT,*) '    ',itin
         endif
      endif
      write(LOUT,*) '  output-array: size=',nout1,nout2,nout3
      if (inrlp.eq.3) then
         write(LOUT,'(a,i2,a,i6)') '     table itout applies to dim=',idmout, &
                ' and has size=',ntout
         if (idebug.ge.10) then
            write(LOUT,*) '    table itout selects entries'
            write(LOUT,*) '    ',itout
         endif
      endif
   endif

!  gather array dimensions for simplifying checks

   isize(1,1) = nin1
   isize(2,1) = nin2
   isize(3,1) = nin3
   isize(1,2) = nout1
   isize(2,2) = nout2
   isize(3,2) = nout3

!  perform the requested inner-loop

   if (inrlp.eq.1) then

!     1. copy: fill entire output-field using entire input-field

      if (any(isize(:,1).ne.isize(:,2))) then
         write(LOUT,*) 'reshape_data, inrlp=1: Error: dimensions disagree.', &
                ' fldin:',isize(:,1),', fldout:',isize(:,2)
         stop
      endif

      do i3 = 1, nin3
         do i2 = 1, nin2
            do i1 = 1, nin1
               fldout(i1,i2,i3) = fldin(i1,i2,i3)
            enddo
         enddo
      enddo

   elseif (inrlp.eq.2) then

!     2. gather: fill entire output-field using part of input-field (e.g.
!        fill send-buffer)

      if (idimin.le.0 .or. idimin.gt.3) then
         write(LOUT,*) 'reshape_data, inrlp=2: Error: idimin must be 1, 2 or 3.'
         stop
      endif

!     check size of arrays; in dimension idimin "nout" should be equal to ntin

      isize(idimin,1) = ntin
      if (any(isize(:,1).ne.isize(:,2))) then
         write(LOUT,*) 'reshape_data, inrlp=2: Error: dimensions disagree.', &
                ' fldin,itin:',isize(:,1),', fldout:',isize(:,2)
         stop
      endif

      if (idimin.eq.1) then

!        2.a: table itin works on 1st dimension of fldin

         do i3 = 1, nout3
            do i2 = 1, nout2
               do i1 = 1, nout1
                  fldout(i1,i2,i3) = fldin ( itin(i1), i2, i3 )
               enddo
            enddo
         enddo

      elseif (idimin.eq.2) then

!        2.b: table itin works on 2nd dimension of fldin

         do i3 = 1, nout3
            do i2 = 1, nout2
               do i1 = 1, nout1
                  fldout(i1,i2,i3) = fldin ( i1, itin(i2), i3 )
               enddo
            enddo
         enddo

      elseif (idimin.eq.3) then

!        2.c: table itin works on 3rd dimension of fldin

         do i3 = 1, nout3
            do i2 = 1, nout2
               do i1 = 1, nout1
                  fldout(i1,i2,i3) = fldin ( i1, i2, itin(i3) )
               enddo
            enddo
         enddo

      endif

   elseif (inrlp.eq.3 .or. inrlp.eq.4) then

!     3. scatter-replace: fill part of output-field using entire input-
!           field (e.g. store receive-buffer)
!     4. scatter-add:   modify part of output-field using entire input-
!           field (e.g. add contents of receive-buffer to output-field)

!     check dimension idmout where table itout is applied

      if (idmout.le.0 .or. idmout.gt.3) then
         write(LOUT,'(a,i1,a)') 'reshape_data, inrlp=',inrlp, &
                ': Error: idmout must be 1, 2 or 3.'
         stop
      endif

!     check size of arrays; in dimension idmout "nin" should be equal to ntout

      isize(idmout,2) = ntout
      if (any(isize(:,1).ne.isize(:,2))) then
         write(LOUT,'(a,i1,2(a,i7))') 'reshape_data, inrlp=',inrlp, &
                ': Error: dimensions disagree. fldin:',isize(:,1), &
                ', fldout,itout:',isize(:,2)
         stop
      endif

      if (idmout.eq.1) then

!        3.a, 4.a: table itout works on 1st dimension of fldout

         if (inrlp.eq.3) then
            do i3 = 1, nin3
               do i2 = 1, nin2
                  do i1 = 1, nin1
                     fldout ( itout(i1), i2, i3 ) = fldin(i1,i2,i3)
                  enddo
               enddo
            enddo
         else
            do i3 = 1, nin3
               do i2 = 1, nin2
                  do i1 = 1, nin1
                     fldout ( itout(i1), i2, i3 ) = &
                        fldout ( itout(i1), i2, i3 ) + fldin(i1,i2,i3)
                  enddo
               enddo
            enddo
         endif

      elseif (idmout.eq.2) then

!        3.b, 4.b: table itout works on 2nd dimension of fldout

         if (inrlp.eq.3) then
            do i3 = 1, nin3
               do i2 = 1, nin2
                  do i1 = 1, nin1
                     fldout (i1, itout(i2), i3 ) = fldin(i1,i2,i3)
                  enddo
               enddo
            enddo
         else
            do i3 = 1, nin3
               do i2 = 1, nin2
                  do i1 = 1, nin1
                     fldout (i1, itout(i2), i3 ) = &
                        fldout (i1, itout(i2), i3 ) + fldin(i1,i2,i3)
                  enddo
               enddo
            enddo
         endif

      elseif (idmout.eq.3) then

!        3.c, 4.c: table itout works on 3rd dimension of fldout

         if (inrlp.eq.3) then
            do i3 = 1, nin3
               do i2 = 1, nin2
                  do i1 = 1, nin1
                     fldout ( i1, i2, itout(i3) ) = fldin(i1,i2,i3)
                  enddo
               enddo
            enddo
         else
            do i3 = 1, nin3
               do i2 = 1, nin2
                  do i1 = 1, nin1
                     fldout ( i1, i2, itout(i3) ) = &
                        fldout ( i1, i2, itout(i3) ) + fldin(i1,i2,i3)
                  enddo
               enddo
            enddo
         endif

      endif

   else

!     unknown inner-loop type: not yet supported

      write(LOUT,*) 'reshape_data: inner-loop type',inrlp, &
        ' is not yet supported.'
      stop

   endif

!  print debug-information when requested

   if (idebug.ge.10) then
      write(LOUT,*) '    output-values:'
      write(LOUT,*) '    ',fldout
   endif

end subroutine reshape_rdata


