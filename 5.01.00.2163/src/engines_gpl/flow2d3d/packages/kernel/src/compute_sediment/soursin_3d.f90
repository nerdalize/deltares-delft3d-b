subroutine soursin_3d(h1          ,thick0      ,thick1      ,sigsed      ,thicksed    , &
                    & r0          ,vicmol      ,sigmol      ,seddif      ,rhosol      , &
                    & ce_nm       ,ws          ,aks         , &
                    & sour        ,sink                                               )
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
!  $Id: soursin_3d.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/soursin_3d.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes the sour and sink terms for the 3D case
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!
! Global variables
!
    real(fp), intent(in)  :: ce_nm
    real(fp), intent(in)  :: h1
    real(fp), intent(in)  :: r0
    real(fp), intent(in)  :: rhosol
    real(fp), intent(in)  :: seddif
    real(fp), intent(in)  :: sigsed
    real(fp), intent(in)  :: sigmol
    real(fp), intent(in)  :: thicksed
    real(fp), intent(in)  :: thick0
    real(fp), intent(in)  :: thick1
    real(fp), intent(in)  :: vicmol
    real(fp), intent(in)  :: ws
    real(fp), intent(in)  :: aks
    real(fp), intent(out) :: sour
    real(fp), intent(out) :: sink
!
! Local variables
!
    real(fp) :: a0kmx
    real(fp) :: a0kmxb
    real(fp) :: alpha1
    real(fp) :: alpha2
    real(fp) :: ce_nmtmp
    real(fp) :: ckmxb
    real(fp) :: dc0
    real(fp) :: dcdz
    real(fp) :: diffus
    real(fp) :: dz
    real(fp) :: power
    real(fp) :: power1
    real(fp) :: power2
    real(fp) :: r00
    real(fp) :: temp0
    real(fp) :: temp1
    real(fp) :: temp2
    real(fp) :: zkmx
    real(fp) :: zkmxb
!
!! executable statements -------------------------------------------------------
!
    dc0 = ce_nm*rhosol - r0
    !
    ! Test if upward diffusion will exist
    ! (test against concentration in bottom sand cell at
    ! previous timestep)
    !
    if (dc0 > 0.0) then
       !
       ! More accurate estimation of concentration and
       ! concentration gradient at bottom of kmx cell
       !
       ce_nmtmp = max(1.0e-4_fp,ce_nm*rhosol)
       zkmx     = h1*(1.0 + sigsed)
       zkmxb    = zkmx - 0.5*thicksed*h1
       r00      = max(r0, 1.0e-7_fp)
       a0kmx    = (aks*(h1 - zkmx))/(zkmx*(h1 - aks))
       power1   = log(r00/(ce_nmtmp))
       power2   = log(a0kmx)
       power    = power1/power2
       a0kmxb   = (aks*(h1 - zkmxb))/(zkmxb*(h1 - aks))
       ckmxb    = ce_nmtmp*a0kmxb**power
       alpha1   = ckmxb/r00
       temp0    = power*ce_nmtmp
       temp1    = ((aks*(h1 - zkmxb))/(zkmxb*(h1 - aks)))&
                & **(power - 1.0)
       temp2    = ( -1.0/(h1 - aks))*((aks*h1)/zkmxb**2)
       dcdz     = temp0*temp1*temp2
       dz       = (1.0 + sigsed)*h1 - aks
       if (abs(r00 - ce_nmtmp) < 1.0e-5) then
          alpha2 = 1
       else
          alpha2 = dcdz/((r00 - ce_nmtmp)/dz)
       endif
       !
       alpha1 = min(alpha1, 10.0_fp)
       alpha1 = max(alpha1,  0.1_fp)
       alpha2 = min(alpha2, 10.0_fp)
       alpha2 = max(alpha2,  0.1_fp)
       !
       diffus = vicmol/sigmol + seddif
       !
       sour = alpha2*ce_nmtmp*diffus/dz
       sink = alpha2*diffus/dz + alpha1*ws
       !
       sour = max(sour , 0.0_fp)
       sink = max(sink , 0.0_fp)
       !
       ! Source and sink terms are calculated per unit
       ! volume
       !
       sour = sour / thick0
       sink = sink / thick1
    else
       !
       ! (if downward diffusion into the bed would occur)
       ! No source term is calculated (diffusion ignored)
       ! use simple estimate for settling flux out of
       ! bottom SAND cell
       !
       sink = ws/thick1
    endif
end subroutine soursin_3d
