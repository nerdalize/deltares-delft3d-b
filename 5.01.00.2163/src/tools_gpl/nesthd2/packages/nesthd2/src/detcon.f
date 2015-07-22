      subroutine detcon(lundia,fout  ,lunadm,bndval,mcbsp ,ncbsp ,
     *                  mnstat,conc  ,iwet  ,nobnd ,notims,nostat,
     *                  kmax  ,lstci                             )
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
!  $Id: detcon.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd2/packages/nesthd2/src/detcon.f $
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : detcon
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Determines transport boundary conditions
!                      (time-series)
! limitations        :
! subroutines called : getwgh,check
!***********************************************************************

      integer       iwet  (nostat), mnes  (  4   )  , nnes (  4   )

      integer       mnstat(2,nostat),
     *              mcbsp (nobnd ,2), ncbsp(nobnd ,2)

      real          weight(  4   )

      real          conc  (nostat,kmax  ,lstci ,notims)

      real          bndval(nobnd ,notims,kmax  ,lstci ,2)

      character*1   type

      logical       fout

      write (*     ,'('' >>> Generating transport boundary '',
     *                ''conditions <<<'')')
      write (lundia,'('' >>> Generating transport boundary '',
     *                ''conditions <<<'')')
!
!-----cycle over all boundary support points
!
      do 10 ibnd = 1, nobnd
         do 10 isize = 1, 2
!
!-----------first get nesting stations, weights and orientation
!           of support point
!
            m    = mcbsp(ibnd,isize)
            n    = ncbsp(ibnd,isize)
            type = 'z'

            call getwgh (lundia,fout  ,lunadm,m     ,n     ,type  ,
     *                   mnes  ,nnes  ,weight,angle               )
            if (fout) goto 999
!
!-----------Check if available on history file
!           if not: adjust weights
!
            call check  (lundia,fout  ,mnstat,mnes  ,nnes  ,weight ,
     *                   m     ,n     ,iwet  ,nostat               )
            if (fout) goto 999
!
!-----------Finally create time series at boundary support points
!
            do 10 iwght = 1, 4
               if (mnes(iwght) .ne. 0) then
                  do 20 istat = 1, nostat
                     if (mnstat(1,istat) .eq. mnes(iwght) .and.
     *                   mnstat(2,istat) .eq. nnes(iwght) ) then
                        goto 30
                     endif
   20             continue

   30             continue
!
!-----------------Finally create time series support points
!
                  do 40 itim = 1, notims
                     do 40 icon = 1, lstci
                        do 40 k = 1, kmax
                           bndval (ibnd,itim,k,icon,isize) =
     *                        bndval (ibnd,itim,k,icon,isize)   +
     *                        conc  (istat,k,icon,itim) * weight (iwght)
   40             continue
               endif
   10 continue

  999 continue

      return

      end
