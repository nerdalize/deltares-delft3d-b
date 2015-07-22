      subroutine mkpoint(lun   , mmax1 , nmax1  , 
     *                   mmax2 , nmax2 , maxnrp , maxbnd )
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
!  $Id: mkpoint.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd1/packages/nesthd1/src/mkpoint.f $
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : mkpoint
! version            : v1.0
! date               : Oct 2000
! programmer         : Antoon Koster
! function           : make FMM pointers
! error messages     :
! subroutines called : -
!
!***********************************************************************
      EXTERNAL MKIPNT, MKRPNT, MKCPNT
      integer  MKIPNT, MKRPNT, MKCPNT
      integer     ierr
      character*6 pntnam

      pntnam = 'x1'
      ierr   = MKRPNT(pntnam,mmax1*nmax1)
      if (ierr   .le. -9) goto 9010

      pntnam = 'y1'
      ierr   = MKRPNT(pntnam,mmax1*nmax1)
      if (ierr   .le. -9) goto 9010

      pntnam = 'icom1'
      ierr   = MKIPNT(pntnam,mmax1*nmax1)
      if (ierr   .le. -9) goto 9010

      pntnam = 'x2'
      ierr   = MKRPNT(pntnam,mmax2*nmax2)
      if (ierr   .le. -9) goto 9010

      pntnam = 'y2'
      ierr   = MKRPNT(pntnam,mmax2*nmax2)
      if (ierr   .le. -9) goto 9010

      pntnam = 'icom2'
      ierr   = MKIPNT(pntnam,mmax2*(nmax2+1))
      if (ierr   .le. -9) goto 9010

      pntnam = 'typbnd'
      ierr   = MKCPNT(pntnam,1*maxbnd)
      if (ierr   .le. -9) goto 9010

      pntnam = 'nambnd'
      ierr   = MKCPNT(pntnam,20*maxbnd)
      if (ierr   .le. -9) goto 9010

      pntnam = 'angle'
      ierr   = MKRPNT(pntnam,maxbnd)
      if (ierr   .le. -9) goto 9010

      pntnam = 'mcbsp'
      ierr   = MKIPNT(pntnam,maxbnd*2)
      if (ierr   .le. -9) goto 9010

      pntnam = 'ncbsp'
      ierr   = MKIPNT(pntnam,maxbnd*2)
      if (ierr   .le. -9) goto 9010

      pntnam = 'xbnd'
      ierr   = MKRPNT(pntnam,maxbnd*2)
      if (ierr   .le. -9) goto 9010

      pntnam = 'ybnd'
      ierr   = MKRPNT(pntnam,maxbnd*2)
      if (ierr   .le. -9) goto 9010

      pntnam = 'mcnes'
      ierr   = MKIPNT(pntnam,maxbnd*2*4)
      if (ierr   .le. -9) goto 9010

      pntnam = 'ncnes'
      ierr   = MKIPNT(pntnam,maxbnd*2*4)
      if (ierr   .le. -9) goto 9010

      pntnam = 'weight'
      ierr   = MKRPNT(pntnam,maxbnd*2*4)
      if (ierr   .le. -9) goto 9010      

      pntnam = 'ipx'
      ierr   = MKIPNT(pntnam,maxnrp)
      if (ierr   .le. -9) goto 9010      

      pntnam = 'ipy'
      ierr   = MKIPNT(pntnam,maxnrp)
      if (ierr   .le. -9) goto 9010      

      pntnam = 'itotpx'
      ierr   = MKIPNT(pntnam,maxnrp)
      if (ierr   .le. -9) goto 9010      

      pntnam = 'itotpy'
      ierr   = MKIPNT(pntnam,maxnrp)
      if (ierr   .le. -9) goto 9010      

      return
 9010 write(lun,'(//a)') 'Fatal error detected - FMM Memory problem'
      write(lun,'(  a)') 'Pointer to be allocated :'//pntnam
      write(lun,'(  a)') 'NestHD1 aborted'
      stop ' Abnormal end - Check administration-file' 
      end
