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
!  $Id: vanrijn.inc 878 2011-10-07 12:58:46Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/data/include/vanrijn.inc $
!-------------------------------------------------------------------------------
    real(fp), parameter :: dclay   =  8.0e-6_fp   !  grain size threshold clay   (vRijn:    8um)
    real(fp), parameter :: dsilt   = 32.0e-6_fp   !  grain size threshold silt   (vRijn:   32um)  
    real(fp), parameter :: dsand   = 64.0e-6_fp   !  grain size threshold sand   (vRijn:   62um)  
    real(fp), parameter :: dgravel =  2.0e-3_fp   !  grain size threshold gravel (vRijn: 2000um)
