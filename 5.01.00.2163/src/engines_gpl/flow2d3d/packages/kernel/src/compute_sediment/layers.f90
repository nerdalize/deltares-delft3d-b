subroutine layers(kmax, h1, aks, thick, sig)
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
!  $Id: layers.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/layers.f90 $
!!--description-----------------------------------------------------------------
!
!   Layers distribution from the reference height to the surface following
!   a logarithmic profile (as used by Van Rijn in Transpor2004)
!   
!   Outputs: thck2d and sig2d, arrays used in 2D mode to compute the equilibrium 
!   concentration profile
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    integer                    , intent(in)  :: kmax
    real(fp)                   , intent(in)  :: h1
    real(fp)                   , intent(in)  :: aks
    real(fp) , dimension(kmax) , intent(out) :: sig
    real(fp) , dimension(kmax) , intent(out) :: thick
!
! Local variables
!
    integer                                  :: k
    real(fp) , dimension(kmax)               :: zz
!
!! executable statements -------------------------------------------------------
!
    do k=1,kmax
       zz (k) = 2.0_fp * aks / h1 * (h1/2.0_fp/aks)**(real(k-1,fp)/real(kmax-1,fp))
    enddo
    do k=1,kmax-1
       thick(k) = zz(kmax+1-k) - zz(kmax-k)
    enddo
    thick(kmax) = zz(1)
    sig(1)=-thick(1) / 2.0_fp
    do k=2,kmax
       sig(k) = -(sum(thick(1:k-1)) + thick(k)/2.0_fp)
    enddo
end subroutine layers
