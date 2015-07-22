module ec_provider_read
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: ec_provider_read.F90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_provider_read.F90 $
!!--description-----------------------------------------------------------------
!
! see ec_module.f90
! More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!
!!--pseudo code and references--------------------------------------------------
!
! stef.hummel@deltares.nl
! herman.kernkamp@deltares.nl
! adri.mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
  use precision
  use ec_data
  use ec_message
  !
  implicit none
  !
contains
!
!
!==============================================================================
function readseries(unitnr, kx, arr1, arr2, tread) result(success)
   !
   !  Read uniform time serie 
   !  number of columns is number of dimensions
   !
   ! result
   logical                         :: success
   !
   ! arguments
   integer                         :: istat
   integer                         :: unitnr
   integer                         :: kx
   real(fp), dimension(:)          :: arr1
   real(fp), dimension(:),optional :: arr2
   real(hp)                        :: tread
   !
   ! locals
   integer                :: k
   character(132)         :: rec
   real(fp), dimension(:), allocatable :: readVals
   !
   ! body
   if ( kx==2 .and. .not.present(arr2) ) then
      call setECMessage("ERROR: ec_provider_read::readseries: wrong sizes")
      success = .false.
      return
   endif
   allocate(readVals(kx), STAT = istat)
   if (istat == 0) then
     success = .true.
   else
     success = .false.
     call setECMessage("ERROR: ec_provider_read::readseries: Unable to allocate additional memory")
     return
   endif
10 read (unitnr,'(a)',end = 100) rec
   if (rec(1:1) .eq. '*') goto 10
   read(rec,*,err = 101) tread, ( readVals(k), k = 1,kx )
   success = .true.
   arr1(1) = readVals(1)
   arr2(1) = readVals(2)
   deallocate(readVals, STAT = istat)
   return
100 continue
   call setECMessage("ERROR: ec_provider_read::readseries: Unexpected end of file in uniform time serie file")
   success = .false.
   deallocate(readVals, STAT = istat)
   return
101 continue
   call setECMessage("ERROR: ec_provider_read::readseries: reading timeseries :", trim(rec))
   success = .false.
   deallocate(readVals, STAT = istat)
   return
end function readseries

function read_spv_block(unitnr, p_conv, xwind, ywind, press, nmax, mmax, kx, tread) result(success)
   !
   ! Read block in meteo_on_flow_grid file
   !
   ! result
   logical                    :: success
   !
   ! arguments
   !
   integer                    :: unitnr
   integer                    :: kx
   integer                    :: nmax
   integer                    :: mmax
   real(fp), dimension(:,:)   :: xwind, ywind, press
   real(hp)                   :: p_conv
   real(hp)                   :: tread
   !
   ! locals
   !
   integer                    :: i
   integer                    :: ierr
   integer                    :: j
   integer                    :: loc_is
   character(132)             :: rec
   !
   if ( size(xwind,1) .ne. nmax .or. size(xwind,2) .ne. mmax .or. &
        size(ywind,1) .ne. nmax .or. size(ywind,2) .ne. mmax .or. &
        size(press,1) .ne. nmax .or. size(press,2) .ne. mmax ) then
      call setECMessage('READ_SPV_BLOCK: wrong sizes')
      success = .false.
      return
   endif
   !
   ! read line with 'TIME = ... minutes ' or a Headerline
   !
   read (unitnr, '(a)') rec
   if (index(rec, 'HEADER') .gt. 0) then
      ! skip header-lines
      do
        read (unitnr, '(a)', iostat=ierr) rec
        if (ierr /= 0) then
           call setECMessage("ERROR: error reading header of meteo-file")
           success = .false.
           return
        endif
        i = index(rec, 'END OF HEADER')
        if (i .gt. 0) then
           !
           ! extra read so rec is always 'TIME = ... minutes ' after this if-do-if-block
           !
           read (unitnr, '(a)') rec
           exit
        endif
      enddo
   endif
   !
   loc_is  = index(rec, '=')
   if (loc_is .gt. 0) then
      read (rec(loc_is+1:), *) tread
   else
      call setECMessage('Could not find time in meteo-file')
      success = .false.
      return
   endif
   !
   ! Loop over the first dimension in flow
   !
   do j = 1,nmax
      read(unitnr,*,end = 100, err=101) ( xwind(j,i), i = 1,mmax )
   enddo
   do j = 1,nmax
      read(unitnr,*,end = 100, err=102) ( ywind(j,i), i = 1,mmax )
   enddo
   do j = 1,nmax
      read(unitnr,*,end = 100, err=103) ( press(j,i), i = 1,mmax )
   enddo
   !
   ! Conversion of pressure to Pa (N/m2). If already Pa, p_conv = 1.0_hp
   !
   if (p_conv .ne. 1d0) then
      press(:,:) = press(:,:) * p_conv
   endif
   !
   success = .true.
   return
100 continue
   call setECMessage('Unexpected end of file in meteo_on_flow_grid file')
   success = .false.
   return
101 continue
   call setECMessage('Error reading wind u-field')
   success = .false.
   return
102 continue
   call setECMessage('Error reading wind v-field')
   success = .false.
   return
103 continue
   call setECMessage('Error reading pressure field')
   success = .false.
   return
end function read_spv_block
!
function ec_grib_open(unitnr, filename, filenr) result(success)
   !
   ! wrapper around pbopen
   !
   ! result
   logical                    :: success
   !
   ! arguments
   !
   integer,      intent(out) :: unitnr
   integer,      intent(in)  :: filenr
   character(*), intent(in)  :: filename
   !
   ! locals
   !
   integer                   :: ierr, wildcard
   character(maxFileNameLen) :: filename2
   !
   success = .false.
   if (filenr == -1) then
      filename2 = filename
   else
      wildcard = index(filename, '?')
      filename2 = filename
      write(filename2(wildcard:wildcard), '(i0)') filenr
   endif
#  if defined GRIB
      call pbopen(unitnr, filename2, 'r', ierr)
#  else
      unitnr = -1
      ierr   = -4
#  endif
   select case (ierr)
   case (0)
      success = .true.
   case (-1)
      call setECMessage("ERROR: grib open: could not open file")
   case (-2)
      call setECMessage("ERROR: grib open: invalid filename")
   case (-3)
      call setECMessage("ERROR: grib open: invalid open mode")
   case (-4)
      call setECMessage("ERROR: grib lib not linked")
   case default
      call setECMessage("ERROR: grib open: unknown error")
   end select       
   !
end function ec_grib_open
!
function read_grib(unitnr, filename, filenr, xwind, ywind, press, nmax, mmax, meta, tread, initialize) result(success)
   !
   ! Read wind and pressure from grib file
   !
   ! result
   logical                            :: success
   !
   ! arguments
   !
   integer                            :: unitnr
   character(maxFileNameLen)          :: filename
   integer                            :: filenr
   integer                            :: nmax       ! out at initialization, otherwise in
   integer                            :: mmax       ! out at initialization, otherwise in
   logical                            :: initialize ! use for initialization
   type(tGrib_meta)                   :: meta
   real(fp), dimension(:,:)           :: xwind, ywind, press
   real(hp)                           :: txwind, tywind, tpress, tread
   !
   ! locals
   !
   character(maxFileNameLen)          :: filename2
   logical, parameter                 :: debug = .false.
   integer                            :: iyear, imonth, iday, ihour, imin
   integer                            :: bufsz1, bufsz2
   integer                            :: ifldtp   ! Type of the field values acording the WMO table 2,
!    used are:  001   Pressure Pa
!               033   u-component of wind m/s
!               034   v-component of wind m/s
!               124   Momentum flux (stress) u component N/m^2 (not yet implemented)
!               125   Momentum flux (stress) v component N/m^2 (not yet implemented)
   integer                             :: ierr
   integer                             :: iword       ! number of elements from inbuff that contain coded data (return from gribex not used)
   integer                             :: lenout
   integer                             :: i, n, m
   integer                             :: wildcard
   integer , dimension(:), allocatable :: buffer
   real(sp), dimension(:), allocatable :: zbuf4
   integer                             :: isec0(2)    ! integer info from section 0 of GRIB file (identification)
   integer                             :: isec1(1024) ! array holding section 1 of GRIB data (time/date)
   integer                             :: isec2(1024) ! array holding section 2 of GRIB data (grid definition)
   integer                             :: isec3(2)    ! info from section 3 of GRIB file (bitmap section)
   integer                             :: isec4(512)  ! info from section 4 of GRIB file (field values)
   real(sp)                            :: zsec2(512)  ! data from section 2 of the GRIB file
   real(sp)                            :: zsec3(2)    ! data from section 3 of the GRIB file
   logical                             :: found_u, found_v, found_p
   character(1)                        :: request     ! 'J' only decodes identification, 'D' decodes all
   !
   ! body
   !
   success = .false.
   !
   if (unitnr < 0) return ! work-around
   !
   if (initialize) then
      request  = 'J'
      bufsz1 = 4 * 999 * 999
      bufsz2 = 1      
   else
      request  = 'D'
      bufsz1 = 4 * mmax * nmax
      bufsz2 = bufsz1
   endif
   allocate(buffer(bufsz1), zbuf4(bufsz2), stat=ierr)
   if (ierr /= 0) then
      call setECMessage("allocate problem in read_grib")
      return
   endif
   !
   found_u = .false.
   found_v = .false.
   found_p = .false.
   outer: do while (.not. (found_u .and. found_v .and. found_p))
      !
      ! read from file and check error status
      !
#     if defined GRIB
         call pbgrib(unitnr, buffer, bufsz1, lenout, ierr)
#     else
         success = .false.
         call setECMessage("ERROR: grib lib not linked")
         exit outer
#     endif
      select case (ierr)
      case (0)
         success = .true.
      case (-1)
         if (filenr == -1) then
            call setECMessage("end of file")
            exit
         else
#           if defined GRIB
               call pbclose(unitnr, ierr)
#           endif
            unitnr = -1
            inner: do i = filenr+1, 9
               wildcard = index(filename, '?')
               filename2 = filename
               write(filename2(wildcard:wildcard), '(i1)') i
               inquire(file=trim(filename2), exist=success)
               if (success) then
                  success = ec_grib_open(unitnr, filename, i)
                  if (.not. success) exit outer
                  filenr = i
#                 if defined GRIB
                     call pbgrib(unitnr, buffer, bufsz1, lenout, ierr)
#                 endif
                  if (ierr == 0) then
                     exit inner
                  else
                     call setECMessage("too much trouble with wildcards in gribfiles")
                     exit outer
                  endif
               endif
            enddo inner
            if (.not. success) then
               call setECMessage("too much trouble with wildcards in gribfiles")
               exit outer
            endif
         endif
      case (-2)
         call setECMessage("error in file handling")
         exit
      case (-3)
         call setECMessage("size of buffer array too small to hold product")
         exit
      case default
         call setECMessage("unknown error in grib_grid")
         exit
      end select
      !
      ! decoded grib data
      !
#     if defined GRIB
         if (debug) call grsdbg(1)
#     endif
      ierr = 0
#     if defined GRIB
      call gribex(isec0, isec1, isec2, zsec2, isec3, zsec3, isec4, zbuf4, bufsz2, buffer, lenout, iword, request, ierr)
#     endif
      select case (ierr)
      case (-4)
         if (debug) write (*,*) "grib decode warning: Bit-map encountered"
      case (-6)
         if (debug) write (*,*) "grib decode warning: ECMWF pseudo-grib data encountered."
      case (0)
         success = .true.
      case (1:999)
         call setECMessage("decoding error in read_grib", ierr)
         success = .false.
         exit
      case default
         if (debug) write (*,*) "grib decode warning number", ierr
      end select
      !
      ! fill metadata
      !
      if (initialize) then
          call fill_grib_metadata(nmax, mmax, meta, isec2)
          exit
      endif
      !
      ! file time tread
      !
      iyear  = (isec1(21)-1)*100+isec1(10)
      imonth = isec1(11)
      iday   = isec1(12)
      ihour  = isec1(13) + isec1(16)
      imin   = isec1(14)
      tread  = real(1440 * (iday -1) + 60 * ihour + imin, hp)
      !
      ! fill wind or pressure
      !
      ifldtp = isec1(6)
      select case (ifldtp)
      case (1)
         if (debug) write (*,*) 'found pressure'
         do m = 1, mmax
             do n = 1, nmax
                press(n, m) = zbuf4(m+(n-1)*mmax)
             enddo
         enddo
         tpress = tread
         found_p = .true.
      case (33)
         if (debug) write (*,*) 'found u-wind'
         do m = 1, mmax
             do n = 1, nmax
                xwind(n,m) = zbuf4(m+(n-1)*mmax)
             enddo
         enddo
         txwind = tread
         found_u = .true.
      case (34)
         if (debug) write (*,*) 'found v-wind'
         do m = 1, mmax
             do n = 1, nmax
                ywind(n,m) = zbuf4(m+(n-1)*mmax)
             enddo
         enddo
         tywind = tread
         found_v = .true.
      case (124:125)
         if (debug) write (*,*) 'found wind stress'
      case default
         if (debug) write (*,*) 'unknown entry: ', ifldtp
      end select
   enddo outer
   !
   if ((tpress /= txwind .or. txwind /= tywind) .and. success .and. .not. initialize) then
      write (*,*) 'different times for pressure and x, y wind: ', tpress, txwind, tywind
      success = .false.
   endif
   !
   deallocate(buffer, zbuf4)
end function read_grib
!
subroutine fill_grib_metadata(nmax, mmax, meta, isec2)
   !
   ! arguments
   !
   integer                  :: nmax, mmax
   type(tGrib_meta)         :: meta
   integer, dimension(1024) :: isec2
   !
   ! locals
   !
   integer                  :: iang1y, iangy, iang1x, iangx
   integer                  :: igdtp
   !
   ! body
   !
   nmax = isec2(3)
   mmax = isec2(2)
   !      
   if ( isec2(7) .gt. isec2(4) ) then
      iang1y = isec2(4)
      iangy  = isec2(7)
   else
      iang1y = isec2(7)
      iangy  = isec2(4)
   endif
   if (isec2(5) .gt. 180000) isec2(5) = isec2(5) - 360000
   if (isec2(8) .gt. 180000) isec2(8) = isec2(8) - 360000
   if ( isec2(8) .gt. isec2(5) ) then
      iang1x = isec2(5)
      iangx  = isec2(8)
   else
      iang1x = isec2(8)
      iangx  = isec2(5)
   endif
   meta%dx = (real (iangx - iang1x, hp) * 1D-3) / real (mmax - 1, hp)
   meta%dy = (real (iangy - iang1y, hp) * 1D-3) / real (nmax - 1, hp)
   meta%x0 = real (iang1x, hp) * 1D-3
   meta%y0 = real (iang1y, hp) * 1D-3
   !
   igdtp  = isec2(1)
   if (igdtp .eq. 10) then
      meta%latsp = real(isec2(13), hp) * 1D-3
      meta%lonsp = real(isec2(14), hp) * 1D-3
   else
      meta%latsp = 0D0
      meta%lonsp = 0D0
   endif
   !
end subroutine fill_grib_metadata
!
end module ec_provider_read
