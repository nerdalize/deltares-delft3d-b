subroutine scan_fl(checkVersionNumber, versionNumberOK)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!  $Id: scan_fl.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/io/src/scan_fl.f90 $
!!--description-----------------------------------------------------------------
!
! SCANFL scans the file "hiswa.out" for a string "SEVERE" or
! or the Swan file "PRINT" for a string "ERROR"
! If found, the program will be stopped.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    logical, intent(in)  :: checkVersionNumber
!
! Local variables
!
    integer           :: k
    integer           :: verLen
    integer           :: uh
    integer, external :: new_lun
    character(20)     :: versionNumber
    character(*)      :: versionNumberOK
    character(80)     :: line
!
!! executable statements -------------------------------------------------------
!
    verLen = len_trim(versionNumberOK)
    uh = new_lun()
    open (uh, file = 'PRINT', form = 'FORMATTED')
100 continue
       read (uh, '(A)', end = 200) line
       call small(line, 80)
       k   = index(line, 'severe')
       if (k > 0) then
          write (*, '(a)') '*** ERROR: SWAN file PRINT contains SEVERE errors'
          close (uh)
          stop
       endif
       k = index(line, 'error')
       if (k > 0) then
          write (*, '(a)') '*** ERROR: SWAN file PRINT contains ERRORS'
          close (uh)
          stop
       endif
       if (checkVersionNumber) then
          k = index(line, 'version number')
          if (k > 0) then
             versionNumber = line(k+15:k+14+verLen)
             if (versionNumber < versionNumberOK) then
                write (*, '(3a)') '*** ERROR: use SWAN version ',trim(versionNumberOK), ' or newer.'
                close (uh)
                stop
             endif
          endif
       endif
    goto 100
200 continue
    close (uh)
end subroutine scan_fl
