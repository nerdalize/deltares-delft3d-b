subroutine tranb4(utot      ,d         ,c         ,par       ,hidexp    , &
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
!  $Id: tranb4.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/tranb4.f90 $
!!--description-----------------------------------------------------------------
! computes sediment transport according to
! general formula
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    real(fp)               , intent(in)  :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp)               , intent(in)  :: d
    real(fp)               , intent(in)  :: hidexp ! hiding & exposure factor
                                                   ! default value, 1.0, to be used when called from Delft3D-MOR
    real(fp)               , intent(out) :: sbot
    real(fp)               , intent(out) :: ssus
    real(fp)               , intent(in)  :: utot
    real(fp), dimension(30), intent(in)  :: par
!
! Local variables
!
    real(fp) :: acal
    real(fp) :: ag    ! gravity acceleration
    real(fp) :: b     ! correction coefficient shear stress
    real(fp) :: cc
    real(fp) :: delta ! velocity (es/ew)  relative density of sediment particle
    real(fp) :: f     ! real help array
    real(fp) :: rmu
    real(fp) :: th
    real(fp) :: thcr
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0
    ssus  = 0.0
    !
    ag    = par( 1)
    delta = par( 4)
    acal  = par(11)
    b     = par(12)
    cc    = par(13)
    rmu   = par(14)
    thcr  = par(15)
    !
    if ((c<1.0e-6) .or. (utot<1.0e-6)) then
       return
    endif
    th = (utot/c)**2/(delta*d)
    f  = rmu*th - hidexp*thcr
    if (f>1.0e-8) sbot = acal*d**1.5*sqrt(ag*delta)*th**b*f**cc
    ssus = 0.0
end subroutine tranb4
