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
!  $Id: dio-f77-tst.F 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/delftio/tests/tstAllF77LIB/src/dio-f77-tst.F $

      subroutine initValues(d1, d2, rValues)
      integer          d1,d2
      real*4           rValues(d1,d2)
      integer          i,j

      do i = 1,d1
          do j = 1,d2
              rValues(i,j) = 1000.0 * i + 1.0 * j + 0.01
          enddo
      enddo
      end


      subroutine incrementValues(d1, d2, rValues)

      integer          d1,d2
      real*4           rValues(d1,d2)
      integer          i,j

      do i = 1,d1
          do j = 1,d2
              rValues(i,j) = rValues(i,j) + 0.01
          enddo
      enddo

      return
      end


      logical function diffInValues(d1, d2, diffValues, epsilon)

      integer          d1,d2
      double precision diffValues(d1,d2)
      double precision epsilon
      integer          i,j

      diffInValues = .false.
      do i = 1,d1
          do j = 1,d2
              if( diffValues(i,j) .gt.  epsilon .or.
     +            diffValues(i,j) .lt.  (-epsilon)    ) then
                  diffInValues = .true.
              endif
          enddo
      enddo

      end

