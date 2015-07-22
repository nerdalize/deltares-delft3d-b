      subroutine D3STOP(iexit)
      implicit none
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
!  $Id: d3stop.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd2/packages/nesthd2/src/d3stop.f $
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    E & Z
!
!             Module: SUBROUTINE D3STOP
!           Function: Terminates execution wit error code.
!                     Is driver for CSTOP and gives possibility to
!                     handle e.g. communication.
!                     Reason to create was the implementation of
!                     coupling with RTC.
!        Method used:
!               Date: 12-04-2002
!         Programmer: J. Zeekant
!         CVS header
!            $Author: Mooiman $
!              $Date: 16-04-04 13:49 $
!            $Source: /u/trisula/cvsroot/trisula/alg/d3stop.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routines:                VARIOUS
!-----------------------------------------------------------------------
!   Called  routines:                SyncRtcFlow_Send
!                                    CSTOP
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! IEXIT   I    I*4                 Exit return value
!-----------------------------------------------------------------------
!    Common variables:
!    -----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
!   IDUMDA     I*4                  Dummy Date
!   IDIMTI     I*4                  Dummy Time
!   ISTATE     I*4                  Status for RTC
!-----------------------------------------------------------------------
!
! declarations and specifications
!
      include 'procs.inc'
!
      integer idumda, idumti, istate, idate , iexit
!
!---- Initialization
!
      idumda = 0
      idumti = 0
      istate = -1
!
!---- Check if RTC-connection is active and if so
!     send (negative) status to shut down RTC
!
!      if (rtcact) then
!        call SyncFlowRtc_Send(istate, idumda, idate)
!        call SyncFlowRtc_Close
!      endif
!
!---- Terminate now
!
      call cstop (iexit, char(0))
      end
