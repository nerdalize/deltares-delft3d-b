      subroutine getlun(lundia    )
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
!  $Id: getlun.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd1/packages/nesthd1/src/getlun.f $
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: SUBROUTINE GETLUN
!           Function: Get unit number of active sun-system 
!                     The sub-system is defined by DEFSUB (0-MXSYS)
!                     The unit number will be set with SETLUN 
!        Method used:
!               Date: 28-07-1998
!         Programmer: Heleen Leepel
!         CVS header
!            $Author: Mooiman $
!              $Date: 27/07/04 11:26 $
!            $Source: $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routines:              VARIOUS
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LUNDIA   O  I*4                  Unit number of diagnostic file were
!                                  error messages should be written to
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
!   Var. I/O  Type Dimensions
!   -------------------------
!
! ISUBS   I   I*4                  Index number for sub-system from
!                                  Delft3D
!                                  0 = No Delft3D sub-system
!                                  1 = BOTT
!                                  2 = CHEM
!                                  3 = ECO
!                                  4 = FLOW
!                                  5 = SED
!                                  6 = TRAN
!                                  7 = WAQ
!                                  8 = WAVE
! LUNSYS  I   I*4  MXSYS           Array containing unit numbers for
!                                  all diagnostic files of Delft3D
!                                  =0 not defined and/or not opened 
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! OPEND       L*4                  Help flag = TRUE when file is still
!                                  open (DELFT3D)
!-----------------------------------------------------------------------
!
!  declarations
!
      include      'diagno.inc'
!
      logical       opend
!-----------------------------------------------------------------------
!-----For ISUBS = 0 LUNDIA := 0
!-----------------------------------------------------------------------
      if (isubs  .eq. 0) then
         lundia = 0
      else
!-----------------------------------------------------------------------
!-----For ISUBS <> 0 LUNDIA := LUNSYS(ISUBS ) if unit is opened
!               else LUNDIA := 0
!-----------------------------------------------------------------------
         lundia = lunsys(isubs )
         if (lundia .ne. 0) then
            inquire(lundia,opened=opend )
            if (.not.opend ) lundia = 0
         endif
      endif
!-----------------------------------------------------------------------
      return
      end
