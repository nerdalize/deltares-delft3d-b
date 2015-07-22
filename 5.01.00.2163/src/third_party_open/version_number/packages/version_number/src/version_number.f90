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
!  $Id: version_number.f90 1332 2012-03-16 13:42:30Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/third_party_open/version_number/packages/version_number/src/version_number.f90 $
program version_number
implicit none
!
! locals
integer        :: handlein
integer        :: handleout
integer        :: ierr
integer        :: pos
character(20)  :: builtNumber
character(10)  :: vn_part
character(20)  :: key
character(20)  :: major
character(20)  :: minor
character(20)  :: revision
character(20)  :: value
character(256) :: inputFile
character(256) :: line
character(256) :: outputFile
character(256) :: vnInputFile
character(256) :: versionString
!
! body
versionString = '@(#)Deltares, version_number, version 1.00.00.00; Sep 22, 2008'
if (COMMAND_ARGUMENT_COUNT() /= 4) then
   call printUsage()
   stop
endif
builtNumber = ' '
vnInputFile = ' '
inputFile   = ' '
outputFile  = ' '
call GET_COMMAND_ARGUMENT(1,builtNumber)
call GET_COMMAND_ARGUMENT(2,vnInputFile)
call GET_COMMAND_ARGUMENT(3,inputFile)
call GET_COMMAND_ARGUMENT(4,outputFile)
major    = '**'
minor    = '**'
revision = '**'

handlein  = 33
handleout = 34

open (unit=handlein, file=trim(vnInputFile), iostat=ierr)
do
  read(handlein,'(a)', iostat=ierr) line
  if (ierr /= 0) then
    close(handlein)
    exit
  endif
  pos   = index(line,'=')
  if ( pos < 1 ) cycle
  key   = line(:pos-1)
  value = line(pos+1:)
  if (index(key,'major') /= 0) then
    major = trim(adjustl(value))
  endif
  if (index(key,'minor') /= 0) then
    minor = trim(adjustl(value))
  endif
  if (index(key,'revision') /= 0) then
    revision = trim(adjustl(value))
  endif
enddo

open (unit=handlein , file=trim(inputFile), iostat=ierr)
open (unit=handleout, file=trim(outputFile), iostat=ierr)
do
  read(handlein,'(a)', iostat=ierr) line
  if (ierr /= 0) then
    close(handlein)
    close(handleout)
    exit
  endif
  
  key    = 'VN_MAJOR'
  value  = major
  call sed(line, key, value)

  key    = 'VN_MINOR'
  value  = minor
  call sed(line, key, value)
  
  key    = 'VN_REVISION'
  value  = revision
  call sed(line, key, value)

  key    = 'VN_BUILD_NUMBER'
  value  = builtNumber
  call sed(line, key, value)

  write(handleout,'(a)') trim(line)
enddo

end program version_number



subroutine sed(line, key, value)
implicit none
  character(*)              :: line
  character(*), intent(in)  :: key
  character(*), intent(in)  :: value
  integer :: pos
  integer :: lenkey
  lenkey = len_trim(key)
  pos    = index(line,trim(key))
  if (pos /= 0) then
    line = line(:pos-1) // trim(value) // line(pos+lenkey:)
  endif
end subroutine sed



subroutine printUsage
   write(*,*) "USAGE : version_number.tcl buildNumber <vnInputFile> <inputFile> <outputFile>"
   write(*,*) "    buildNumber   : On the build server: buildNumber (=svn revision number)"
   write(*,*) "                    Else: Subversion revisionNumber"
   write(*,*) "    <vnInputFile> : Name of the inifile containing the integers that form the version number"
   write(*,*) "    <inputFile>   : Name of the file containing keywords to be replaced by the actual version number"
   write(*,*) "    <outputFile>  : Name of the file, identical to the inputFile with keywords replaced"
end subroutine printUsage
