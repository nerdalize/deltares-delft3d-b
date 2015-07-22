function shld(dstar     )
!!--copyright-------------------------------------------------------------------
! Copyright (c) 2003, Deltares. All rights reserved.
!!--disclaimer------------------------------------------------------------------
! This code is part of the Delft3D software system. Deltares has
! developed c.q. manufactured this code to its best ability and according to the
! state of the art. Nevertheless, there is no express or implied warranty as to
! this software whether tangible or intangible. In particular, there is no
! express or implied warranty as to the fitness for a particular purpose of this
! software, whether tangible or intangible. The intellectual property rights
! related to this software code remain with Deltares at all times.
! For details on the licensing agreement, we refer to the Delft3D software
! license and any modifications to this license, if applicable. These documents
! are available upon request.
!!--version information---------------------------------------------------------
! $Author$
! $Date$
! $Revision$
!!--description-----------------------------------------------------------------
!
! determines shields parameter according
! to shields curve
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local constants
! Interface is in high precision
!
integer        , parameter :: hp   = kind(1.0d0)
!
! Global variables
!
    real(hp), intent(in) :: dstar ! critical dimensionless grain size parameter
    real(hp)         :: shld
!
!! executable statements -------------------------------------------------------
!
    if (dstar<=4.) then
       shld = 0.240/dstar
    elseif (dstar<=10.) then
       shld = 0.140/dstar**0.64
    elseif (dstar<=20.) then
       shld = 0.040/dstar**0.10
    elseif (dstar<=150.) then
       shld = 0.013*dstar**0.29
    else
       shld = 0.055
    endif
end function shld
