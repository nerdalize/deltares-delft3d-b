      subroutine hyd2dh(bndval,typbnd,thick ,nobnd ,notims,kmax  ,
     *                  nolay ,dav                               )
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
!  $Id: hyd2dh.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd2/packages/nesthd2/src/hyd2dh.f $
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : dethyd
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Determines depth-averaged velocities from 3D
!                      computation
! limitations        :
! subroutines called :
! note               : nolay reset to 1 for depth averaged velocities
!                      typbnd set to 'u'
!***********************************************************************

      real        thick (kmax  )

      real        bndval(nobnd ,notims,kmax  ,1     ,2     )

      logical     dav

      character*1 typbnd(nobnd )


      nolay = kmax
      
      if (kmax .eq. 1) then
         do 1 ibnd = 1, nobnd
            if (typbnd(ibnd) .eq. 'c') then
               typbnd(ibnd) = 'u'
            endif
    1    continue
      endif
  
      if (.not. dav) goto 999
!
!-----determine 2Dh velocities and store in layer 1 (k=1)
!
      do 10 ibnd = 1, nobnd
         if (typbnd(ibnd) .eq. 'c') then

            typbnd (ibnd) = 'u'

            do 20 isize = 1, 2
               do 20 itim = 1, notims

                  k = 1
                  bndval (ibnd,itim,k,1,isize) =
     *            thick(k)*bndval (ibnd,itim,k,1,isize)

                  do 20 k = 2, kmax
                     bndval (ibnd,itim,1,1,isize) =
     *               bndval (ibnd,itim,1,1,isize) +
     *               thick(k)*bndval (ibnd,itim,k,1,isize)
   20       continue
         endif
   10 continue

      nolay = 1

  999 continue

      return

      end
