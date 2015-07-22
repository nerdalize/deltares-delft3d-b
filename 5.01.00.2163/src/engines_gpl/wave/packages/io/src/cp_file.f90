subroutine cp_file(filnm1    ,filnm2    ,filtype      ,nuerr         )
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
!  $Id: cp_file.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/io/src/cp_file.f90 $
!!--description-----------------------------------------------------------------
!
! Copy or append file FILNM1 to file FILNM2
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer     , intent(out) :: nuerr
    character(*), intent(in)  :: filnm1
    character(*), intent(in)  :: filnm2
    character(*), intent(in)  :: filtype
!
! Local variables
!
    integer           :: iocond    ! IO status return code
    integer           :: lf1       ! > 0 Error; < 0 End-Of-File Actual length of string FILNM1
    integer           :: lf2       ! Actual length of string FILNM2
    integer           :: lrec      ! Actual length of string REC132
    integer           :: lunf1     ! Unit number for FILNM1
    integer           :: lunf2     ! Unit number for FILNM2
    integer           :: nr
    integer           :: nrec
    integer, external :: new_lun
    logical           :: ex        !      Flag for existing file
    logical           :: opend1    ! Flag to test if file FILNM1 is al-
    logical           :: opend2    ! ready opened Flag to test if file FILNM2 is al-
    character(132)    :: rec132
!
!! executable statements -------------------------------------------------------
!
    nuerr = 0
    lf1 = index(filnm1, ' ')
    if (lf1==0) lf1 = len(filnm1) + 1
    lf1 = lf1 - 1
    opend1 = .false.
    lf2 = index(filnm2, ' ')
    if (lf2==0) lf2 = len(filnm2) + 1
    lf2 = lf2 - 1
    opend2 = .false.
    inquire (file = filnm1(:lf1), exist = ex)
    if (.not.ex) then
       nuerr = 1
       goto 999
    endif
    inquire (file = filnm1(:lf1), opened = opend1)
    if (opend1) then
       inquire (file = filnm1(:lf1), number = lunf1)
       rewind lunf1
    else
       lunf1 = new_lun()
       open (lunf1, file = filnm1(:lf1), form = 'formatted', status = 'old')
    endif
    inquire (file = filnm2(:lf2), exist = ex)
    if (ex) then
       inquire (file = filnm2(:lf2), opened = opend2)
       if (opend2) then
          inquire (file = filnm2(:lf2), number = lunf2)
          rewind lunf2
       else
          lunf2 = new_lun()
          open (lunf2, file = filnm2(:lf2), form = 'formatted', status = 'old')
       endif
       if (filtype=='append') then
          nrec = 0
  100     continue
          read (lunf2, '(A)', iostat = iocond) rec132
          if (iocond/=0) then
             if (iocond<0) then
                rewind lunf2
                do nr = 1, nrec
                   read (lunf2, '(A)') rec132
                enddo
                goto 300
             endif
             nuerr = 2
             goto 400
          endif
          nrec = nrec + 1
          goto 100
       endif
    else
       lunf2 = new_lun()
       open (lunf2, file = filnm2(:lf2), form = 'formatted', status = 'new')
    endif
  300 continue
    rec132 = ' '
    read (lunf1, '(A)', iostat = iocond) rec132
    if (iocond/=0) then
       if (iocond>0) nuerr = 3
       goto 400
    endif
    lrec = 132
  310 continue
    if (rec132(lrec:lrec)==' ') then
       lrec = lrec - 1
       if (lrec>70) goto 310
    endif
    write (lunf2, '(A)') rec132(:lrec)
    goto 300
  400 continue
    close (lunf1)
    close (lunf2)
  999 continue
end subroutine cp_file
