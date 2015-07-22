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
!  $Id: ectest.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ectest/ectest.f90 $
program ectest
  ! adri.mourits@deltares.nl
  use precision
  use gdp
  use ec_module
  use ectest_meteo
  !
  implicit none
  !
  ! locals
  !
  integer :: i
  integer :: istat
  integer :: j
  integer :: ij
  integer :: mnmax
  integer :: provid
  integer :: testnr
  logical :: success
  character(len=10) :: string
  character(len=*), parameter :: errorstr = 'First commandline argument should be an integer (1..5)'
  !
  logical ectest_rr_cf
  external ectest_rr_cf
!
!! executable statements -------------------------------------------------------
!
  mmax = 0
  nmax = 0
  write(*,*) 'ECTEST start . . .'
  !
  ! Define the test grid
  !
  if (command_argument_count() > 0) then
      call get_command_argument(1, string)
      read(string, '(i1)', iostat=istat) testnr
      if (istat /= 0 .or. testnr < 1 .or. testnr > 5) stop errorstr
  else
      stop errorstr
  endif
  if (testnr == 4) then
     success = ectest_rr_cf()
     stop 'klaar met RR-CF test.'
  else if (testnr == 5) then
     ECPrivate  = .true.
     success  = create(ECHandle, ECPrivate)
     call checkResult(ECHandle, success)
     provId   = addProvider(ECHandle, provType_file, '..\ectest_input.wnd', provFile_table)
     if (provId == 0) call checkResult(ECHandle)
     success  = initProvider(ECHandle, provId)
     call checkResult(ECHandle, success)
     stop 'klaar met tables-test.'
  else if (testnr == 1) then
     nmax   = 2
     mmax   = 3
     mnmax  = mmax*nmax
     kmax   = 1
     sferic = .false.
                     allocate(kcs  (mnmax), STAT = istat)
     if (istat == 0) allocate(x    (mnmax), STAT = istat)
     if (istat == 0) allocate(y    (mnmax), STAT = istat)
     if (testnr == 1) then
        if (istat == 0) allocate(uwind(mnmax), STAT = istat)
        if (istat == 0) allocate(vwind(mnmax), STAT = istat)
        if (istat == 0) allocate(patm (mnmax), STAT = istat)
     endif
  else
     mmax   = 201
     nmax   = 173
     mnmax  = mmax*nmax
     kmax   = 1
     sferic = .true.
                     allocate(kcs  (mnmax), STAT = istat)
     if (istat == 0) allocate(x    (mnmax), STAT = istat)
     if (istat == 0) allocate(y    (mnmax), STAT = istat)
     if (istat == 0) allocate(uwind2d(nmax,mmax), STAT = istat)
     if (istat == 0) allocate(vwind2d(nmax,mmax), STAT = istat)
     if (istat == 0) allocate(patm2d (nmax,mmax), STAT = istat)
  endif
  if (istat /= 0) then
    write(*,*) "ERROR: ectest: Unable to allocate additional memory"
    stop
  endif
  !
  kcs = 1
  do i=1, mmax
     do j=1, nmax
        ij = i + mmax*(j-1)
        x(ij) = real(i,fp) * real(j,fp)
        y(ij) = real(i,fp) * real(j,fp) * 10.0_fp
     enddo
  enddo
  !
  ! Define test time parameters
  !
  nst    = 0
  if (testnr == 1) then
     dt     = 0.1_hp
     ntstop = nint(10.0_hp/dt)
  else if (testnr == 2) then
     dt     = 60.0_hp
     ntstop = nint(1440.0_hp/dt)
  else if (testnr == 3) then
     dt     = 1440.0_hp
     ntstop = 3 ! nint(2880.0_hp/dt)
  else
     dt     = 10.0_hp
     ntstop  = 20
  endif
  curtim = 0.0_hp
  !
  ! EC init
  !
  ECPrivate  = .true.
  success  = create(ECHandle, ECPrivate)
  call checkResult(ECHandle, success)
  !
  ! grid to EC-module
  !
  gridECItemId = addElementSet(ECHandle, x, y, kcs, sferic)
  if (gridECItemId == 0) call checkResult(ECHandle)
  success = setInternalGrid(ECHandle, gridECItemId)
  !
  ! read meteo
  !
  call ectest_readMeteo(testnr)
  !
  ! Enough input for the full simulation period?
  !
  success = ECCheck(ECHandle, real(ntstop,hp) * dt)
  call checkResult(ECHandle, success)
  call ECDump(ECHandle)
  !
  ! calculation
  !
  do nst = 1, ntstop
    curtim = curtim + dt
    write(*,'(a,i0,a,f0.2,a)') 'step:', nst, ' time= ', curtim, ' minutes'
    !
    !
    if (testnr == 1) then
       success = getVal(ECHandle, patmECItemId , curtim, patm)
       call checkResult(ECHandle, success)
       success = getVal(ECHandle, uwindECItemId, curtim, uwind)
       call checkResult(ECHandle, success)
       success = getVal(ECHandle, vwindECItemId, curtim, vwind)
       call checkResult(ECHandle, success)
       write(*,'(a,f10.2,2(a,f10.6))') '    p:', patm(1), '     u:', uwind(1), '     v:',vwind(1)
    else
       success = getVal(ECHandle, patmECItemId , curtim, patm2d)
       call checkResult(ECHandle, success)
       success = getVal(ECHandle, uwindECItemId, curtim, uwind2d)
       call checkResult(ECHandle, success)
       success = getVal(ECHandle, vwindECItemId, curtim, vwind2d)
       call checkResult(ECHandle, success)
       write(*,'(a,f10.2,2(a,f10.6))') '    p:', sum(patm2d) /real(mnmax, fp), &
                                       '    u:', sum(uwind2d)/real(mnmax, fp), &
                                       '    v:', sum(vwind2d)/real(mnmax, fp)
    endif
  enddo
  !
  ! EC finish
  !
  success = free(ECHandle)
  call checkResult(ECHandle, success)
  !
  ! free test
  !
  if (testnr == 1) then
     deallocate(uwind, STAT = istat)
     deallocate(vwind, STAT = istat)
     deallocate(patm , STAT = istat)
  else if (testnr == 2 .or. testnr == 3) then
     deallocate(uwind2d, STAT = istat)
     deallocate(vwind2d, STAT = istat)
     deallocate(patm2d , STAT = istat)
  endif

  write(*,*) 'ECTEST . . . finished'
end program ectest
