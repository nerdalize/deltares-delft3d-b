subroutine tranb1(utot      ,d50       ,c         ,h         ,par       , &
                & sbot      ,ssus      )
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
!  $Id: tranb1.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/tranb1.f90 $
!!--description-----------------------------------------------------------------
! computes sediment transport according to
! engelund hansen
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp)               , intent(in)  :: c       ! Description and declaration in esm_alloc_real.f90
    real(fp)               , intent(in)  :: d50
    real(fp)               , intent(in)  :: h
    real(fp)               , intent(out) :: sbot
    real(fp)               , intent(out) :: ssus
    real(fp)               , intent(in)  :: utot
    real(fp), dimension(30), intent(in)  :: par
!
! Local variables
! 
    real(fp)   :: acal
    real(fp)   :: ag      ! gravity acceleration
    real(fp)   :: cc
    real(fp)   :: cf
    real(fp)   :: delta   ! relative density of sediment particle
    real(fp)   :: rk
    real(fp)   :: vster
    real(fp)   :: suspfac
    real(fp)   :: temp
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0_fp
    ssus  = 0.0_fp
    !
    ag      = par(1)
    delta   = par(4)
    acal    = par(11)
    rk      = par(12)
    suspfac = par(13)
    !
    if ((utot<1.0e-6_fp) .or. (h<0.001_fp)) then
       return
    endif
    if (c < 1.0e-6_fp) then
       cc = 18.0_fp * log10(12.0_fp*h/rk)
    else
       cc = c
    endif
    !
    !     bed load
    !
    cf    = ag / cc / cc
    vster = sqrt(cf) * utot
    temp  = acal * 0.05_fp * utot * vster**4 / ag**2 / sqrt(cf) / delta**2 / d50
    sbot  = (1.0_fp-suspfac) * temp
    !
    ! suspended sediment transport
    !
    ssus = suspfac*temp
end subroutine tranb1
