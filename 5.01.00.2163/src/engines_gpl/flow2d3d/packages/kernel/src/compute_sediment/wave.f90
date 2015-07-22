subroutine wave(uo        ,t         ,uuvar     ,pi        ,wh        , &
              & c         ,rk        ,h         ,ag        ,wavek     )
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
!  $Id: wave.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/wave.f90 $
!!--description-----------------------------------------------------------------
! computes a correction factor to
! account for the wave effect on the
! transport relation.
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp), intent(in)               :: ag !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)               :: c !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)               :: h
    real(fp), intent(in)               :: pi !  Description and declaration in const.igs
    real(fp), intent(in)               :: rk
    real(fp), intent(in)               :: t
    real(fp)        :: uo
    real(fp)        :: uuvar
    real(fp), intent(in)               :: wavek
    real(fp), intent(in)               :: wh
!
!
! Local variables
!
    real(fp)                       :: a0
    real(fp)                       :: fw
    real(fp)                       :: rksi
    real(fp)                       :: uor
    real(fp)                       :: var
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    if (wh<=0.) then
       return
    endif
    if (wavek*h>75.) then
       uo = 0.
    else
       uo = pi*wh/t/sinh(wavek*h)
    endif
    uor = uo
    a0 = uor*t/2./pi/rk
    a0 = max(a0, 2.0_fp)
    fw = exp( - 5.977 + 5.213/a0**.194)
    rksi = c*sqrt(fw/2./ag)
    var = rksi*uor
    uuvar = uuvar + .5*var*var
end subroutine wave
