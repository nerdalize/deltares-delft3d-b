subroutine endscr(lunscr    ,merr      ,mwarn     ,date     ,time      , &
                & runid     ,versio    ,soort     )
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
!  $Id: endscr.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/general/endscr.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Presents the last header/logo on the screen
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)            :: lunscr !  Description and declaration in inout.igs
    integer, intent(in)            :: merr   !!  Help var. (nr. of 'ERROR'   string found in the diagnostic file)
    integer, intent(in)            :: mwarn  !!  Help var. (nr. of 'WARNING' string found in the diagnostic file)
    character(*)    :: runid
    character(10), intent(in)      :: date   !!  Date to be filled in the header
    character(5), intent(in)       :: versio !!  Version nr. of the current package
    character(6), intent(in)       :: soort  !!  soort=md-ver is not used anymore; to be removed
    character(8), intent(in)       :: time   !!  Cur. time to be filled in the header
!
! Local variables
!
    integer                        :: icount  ! Help var.; counter 
    integer                        :: lenrun  ! Help var.; length of runid 
    character(78), dimension(10)   :: txtfil  ! Texts to be filled in the header 
!
!! executable statements -------------------------------------------------------
!
    txtfil(1)(1:40) = '----------------------------------------'
    txtfil(2)(1:40) = '              date: ..........          '
    txtfil(3)(1:40) = '              time:   ........          '
    ! MD-verify / FLOW
    txtfil(4)(1:40) = '  MD-Verify result: ... ERRORS and ... W'
    ! MD-Verify
    txtfil(5)(1:40) = '              The diagnostic file <md-di'
    txtfil(6)(1:40) = '              number of ERRORS and/or WA'
    ! FLOW
    txtfil(7)(1:40) = '              Besides all ERRORS and WAR'
    txtfil(8)(1:40) = '              Delft3D-FLOW is written to'
    txtfil(9)(1:40) = '              The file is an ASCII-file '
    txtfil(10)(1:40) = '              printed as any arbitrary A'
    !
    txtfil(1)(41:78) = '--------------------------------------'
    txtfil(2)(41:78) = '                     Version: .....   '
    txtfil(3)(41:78) = '                                      '
    ! MD-verify / FLOW
    txtfil(4)(41:78) = 'ARNINGS were found                    '
    ! MD-Verify
    txtfil(5)(41:78) = 'ag....> is only available when the    '
    txtfil(6)(41:78) = 'RNINGS is greater than zero.          '
    ! FLOW
    txtfil(7)(41:78) = 'NINGS, the SIMulation PERformance of  '
    txtfil(8)(41:78) = ' the diagnostic file <tri-diag....>   '
    txtfil(9)(41:78) = 'and it can be inspected and/or        '
    txtfil(10)(41:78) = 'SCII-file.                            '
    !
    !-----Define "dependent" parts of header
    !
    txtfil(2)(21:30) = date
    txtfil(2)(71:75) = versio
    txtfil(3)(23:30) = time
    write (txtfil(4)(21:23), '(i3)') merr
    write (txtfil(4)(36:38), '(i3)') mwarn
    if (soort=='md-ver') then
       txtfil(4)(3:11) = 'MD-Verify'
    else
       txtfil(4)(3:11) = 'FLOW  '
    endif
    !
    !-----Write first part
    !
    write (lunscr, '(a)') txtfil(1)
    write (lunscr, '(a)') txtfil(2)
    write (lunscr, '(a)') txtfil(3)
    write (lunscr, '(a)') txtfil(1)
    write (lunscr, '(a)') txtfil(4)
    write (lunscr, '(a)') txtfil(1)
    call noextspaces(runid     ,lenrun    )
    !
    !-----Use original layout only when len(runid)<=3
    !
    if (lenrun<=3) then
       !
       !--------Part for SOORT = md-ver
       !
       if (soort=='md-ver') then
          txtfil(5)(35:35) = char(34)
          txtfil(5)(44:46) = runid(1:3)
          txtfil(5)(47:47) = char(34)
          write (lunscr, '(a)') txtfil(5)
          write (lunscr, '(a)') txtfil(6)
       !
       !--------Part for SOORT = trisim
       !
       else
          txtfil(8)(62:62) = char(34)
          txtfil(8)(72:74) = runid(1:3)
          txtfil(8)(75:75) = char(34)
          write (lunscr, '(a)') txtfil(7)
          write (lunscr, '(a)') txtfil(8)
       endif
    !
    !-----len(runid)>3
    !
    !--------Part for SOORT = md-ver
    !
    elseif (soort=='md-ver') then
       txtfil(5)(35:35) = char(34)
       txtfil(5)(47:47) = char(34)
       write (lunscr, '(a)') txtfil(5)(1:34)
       if (lenrun<=54) then
          write (lunscr, '(14X,a9,a,a1)') &
              & txtfil(5)(35:43), runid(1:lenrun), txtfil(5)(47:47)
       else
          write (lunscr, '(14X,a9,a)') txtfil(5)(35:43), runid(1:54)
          icount = 0
  100     continue
          icount = icount + 1
          if (lenrun<=(54 + icount*62)) then
             write (lunscr, '(15X,a,a1)') &
                 & runid((icount*62 - 7):lenrun), txtfil(5)(47:47)
          else
             write (lunscr, '(15X,a)') runid((icount*62 - 7):(54 + icount*62))
             goto 100
          endif
       endif
       write (lunscr, '(14X,a)') txtfil(5)(49:)
       write (lunscr, '(a)') txtfil(6)
    !
    !--------Part for SOORT = trisim
    !
    else
       write (lunscr, '(a)') txtfil(7)
       txtfil(8)(62:62) = char(34)
       txtfil(8)(75:75) = char(34)
       write (lunscr, '(a)') txtfil(8)(1:61)
       if (lenrun<=53) then
          write (lunscr, '(14X,a10,a,a1)') &
              & txtfil(8)(62:71), runid(1:lenrun), txtfil(8)(75:75)
       else
          write (lunscr, '(14X,a10,a)') txtfil(8)(62:71), runid(1:53)
          icount = 0
  200     continue
          icount = icount + 1
          if (lenrun<=(53 + icount*62)) then
             write (lunscr, '(15X,a,a1)') &
                 & runid((icount*62 - 8):lenrun), txtfil(8)(75:75)
          else
             write (lunscr, '(15X,a)') runid((icount*62 - 8):(53 + icount*62))
             goto 200
          endif
       endif
    endif
    !
    !-----Write last part
    !
    write (lunscr, '(a)') txtfil(9)
    write (lunscr, '(a)') txtfil(10)
    write (lunscr, '(a)') txtfil(1)
end subroutine endscr
