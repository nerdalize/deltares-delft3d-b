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
!  $Id: lognormal.inc 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/include/lognormal.inc $
!-------------------------------------------------------------------------------
real(fp), dimension(99)  :: lognormal
data lognormal &
  & /-2.3263,-2.0537,-1.8808,-1.7507,-1.6449,-1.5548,-1.4758,-1.4051, &
  &  -1.3408,-1.2816,-1.2265,-1.1750,-1.1264,-1.0803,-1.0364,-0.9945, &
  &  -0.9542,-0.9154,-0.8779,-0.8416,-0.8064,-0.7722,-0.7388,-0.7063, &
  &  -0.6745,-0.6433,-0.6128,-0.5828,-0.5534,-0.5244,-0.4959,-0.4677, &
  &  -0.4399,-0.4125,-0.3853,-0.3585,-0.3319,-0.3055,-0.2793,-0.2533, &
  &  -0.2275,-0.2019,-0.1764,-0.1510,-0.1257,-0.1004,-0.0753,-0.0502, &
  &  -0.0251, 0.0000, 0.0251, 0.0502, 0.0753, 0.1004, 0.1257, 0.1510, &
  &   0.1764, 0.2019, 0.2275, 0.2533, 0.2793, 0.3055, 0.3319, 0.3585, &
  &   0.3853, 0.4125, 0.4399, 0.4677, 0.4959, 0.5244, 0.5534, 0.5828, &
  &   0.6128, 0.6433, 0.6745, 0.7063, 0.7388, 0.7722, 0.8064, 0.8416, &
  &   0.8779, 0.9154, 0.9542, 0.9945, 1.0364, 1.0803, 1.1264, 1.1750, &
  &   1.2265, 1.2816, 1.3408, 1.4051, 1.4758, 1.5548, 1.6449, 1.7507, &
  &   1.8808, 2.0537, 2.3263/
integer, dimension(14)   :: ilognormal
data ilognormal /1, 3, 6, 10, 16, 24, 36, 64, 76, 84, 90, 94, 97, 99/
