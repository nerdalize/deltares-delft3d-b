      subroutine dimbnd(lunbnd,nobnd )
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
!  $Id: dimbnd.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd2/packages/nesthd2/src/dimbnd.f $
      character*1   char
      character*80  rec

      nobnd = 0

  100 read (lunbnd,'(a80)',end = 999) rec

      ip = 20
  110 ip = ip + 1
      read (rec(ip:ip),'(a1)') char
      if (char .eq. ' ') goto 110

  120 ip = ip + 1
      read (rec(ip:ip),'(a1)') char
      if (char .eq. ' ') goto 120

      call small (char,1)

      if (char .eq. 't') nobnd = nobnd + 1

      goto 100

  999 continue

      rewind (lunbnd)

      return

      end
