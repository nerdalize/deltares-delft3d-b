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
!  $Id: diagno.inc 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd2/packages/nesthd2/include/diagno.inc $
!-------------------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: INCLUDE FILE DIAGNO.INC
!               Date: 07-05-2002
!         Programmer: H.H. Leepel
!         CVS header
!            $Author: Mourits $
!              $Date: 8-05-02 9:26 $
!            $Source: $
!          $Revision: 12 $
!-----------------------------------------------------------------------
!          Constants:
!
! Const.      Type
!
! MXSYS       I*4                  Maximum number of sub-systems defined
!                                  for Delft3D
!-----------------------------------------------------------------------
!   Common variables:
!   -----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! ISUBS       I*4                  Index number for sub-system from
!                                  Delft3D
!                                   0 = No Delft3D sub-system
!                                   1 = BOTT
!                                   2 = CHEM
!                                   3 = ECO
!                                   4 = FLOW
!                                   5 = MOR
!                                   6 = PART
!                                   7 = SED
!                                   8 = TRAN
!                                   9 = WAQ
!                                  10 = WAVE
! LUNSYS      I*4  MXSYS           Array containing unit numbers for
!                                  all diagnostic files of Delft3D
!                                  =0 not defined and/or not opened 
!-----------------------------------------------------------------------
!
!  declarations
!
      integer          mxsys
      parameter       (mxsys  =   10)
!
      integer          isubs ,lunsys
!
      dimension        lunsys(mxsys )
!
      common  /diagno/ isubs ,lunsys
