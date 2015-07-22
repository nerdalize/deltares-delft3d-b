subroutine corrht(hrm       ,deph      ,tp        ,wavel     ,wavek     , &
                & ldep      ,dish      ,dismax    ,choice    ,rho       , &
                & grav      )
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
!  $Id: corrht.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/kernel/src/corrht.f90 $
!!--description-----------------------------------------------------------------
!
!
!     Input:
!     -------
!     HSH,DEPH,TP
!     I/O  I    I/O
!
!     Output:
!     --------
!     WAVEL,WAVEK,LDEP
!     LDEP  : logical variable, .true. when depth or wave height too small
!
!     Adapt wave period TP and wave height HSH
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
    !
    !
    ! COMMON variables
    !
    real ::         pi, twopi, wort2, gamma
    common /const / pi, twopi, wort2, gamma
!
! Global variables
!
    logical, intent(in)            :: choice
    logical, intent(out)           :: ldep
    real                           :: deph
    real                           :: dish
    real                           :: dismax
    real                           :: grav
    real                           :: hrm
    real   , intent(in)            :: rho
    real                           :: tp
    real                           :: wavek
    real   , intent(out)           :: wavel
!
! Local variables
!
    real :: hmax
    real :: hs
    real :: tpmin
!
!! executable statements -------------------------------------------------------
!
    ldep   = .false.
    dismax = 0.0
    if (deph>0.05 .and. hrm>=0.01 .and. tp>0.0) then
       if (choice) then
          !
          ! Adjust wave-coefficients
          !
          call wavenr(deph, tp, wavek)
          !
          hmax   = 0.88/wavek*tanh(gamma*wavek*deph/0.88)
          hrm    = min(hrm, hmax)
          hs     = hrm*wort2
          tpmin  = 14.09*sqrt(hs/grav)
          dismax = 0.25*rho*grav*hmax*hmax/tp
          dish   = min(dish, dismax)
          tp     = min(tp, tpmin)
       endif
       !
       call wavenr(deph, tp, wavek)
       !
       wavel = twopi/wavek
    else
       !
       ! Too shallow water or waves too small
       !
       ldep = .true.
    endif
end subroutine corrht
