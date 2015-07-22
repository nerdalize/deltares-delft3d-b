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
!  $Id: tri-dyn.inc 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd1/packages/nesthd1/include/tri-dyn.inc $
!-------------------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    E & Z
!
!             Module: INCLUDE FILE TRIS-DYN.INC
!               Date: 08-06-1995
!         Programmer: H.H. Leepel
!         CVS header
!            $Author: Mooiman $
!              $Date: 27/07/04 11:21 $
!            $Source: /u/trisula/cvsroot/trisula/include/tri-dyn.inc,v $
!          $Revision: 2 $
!-----------------------------------------------------------------------
!   Common variables:
!   -----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! CH         CH*1  (0:0)           Equivalence character array for use
!                                  in TRISULA
! CHBUF      CH*1  (0:0)           Complete character array for dynamic
!                                  array declaration
! I           I*4  (0:0)           Equivalence integer array for use
!                                  in TRISULA
! IBUF        I*4  (0:0)           Complete integer array for dynamic
!                                  array declaration
! R           R*4  (0:0)           Equivalence real array for use
!                                  in TRISULA
! RBUF        R*4  (0:0)           Complete integer array for dynamic
!                                  array declaration
!-----------------------------------------------------------------------
!
!  declarations
!
      real             r     (0:0)
      integer          i     (0:0)
      character*1      ch    (0:0)
!
      equivalence     (r     (0),rbuf  (0)), &
     &                (i     (0),ibuf  (0)), &
     &                (ch    (0),chbuf (0))
!----------------------------------------------------------------------
!-----Yes, we pretend that TRISULA = 'r i ch'
!----------------------------------------------------------------------

