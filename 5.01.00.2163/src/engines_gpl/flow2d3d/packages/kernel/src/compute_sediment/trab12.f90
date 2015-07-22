subroutine trab12(u         ,v         ,hrms      ,h         ,tp        , &
                & dir       ,d50       ,par       ,sbotx     ,sboty     , &
                & ssusx     ,ssusy     ,ubot      ,vonkar    ,ubot_from_com)
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
!  $Id: trab12.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/trab12.f90 $
!!--description-----------------------------------------------------------------
! computes sediment transport according to
! the bed load transport formula of Soulsby;
! first implementation assumes transport in
! current direction only.
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Global variables
!
    real(fp)               , intent(in)  :: d50
    real(fp)               , intent(in)  :: dir
    real(fp)                             :: h
    real(fp)                             :: hrms  !  Description and declaration in esm_alloc_real.f90
    real(fp)                             :: ubot  !  Description and declaration in esm_alloc_real.f90
    real(fp)               , intent(out) :: sbotx
    real(fp)               , intent(out) :: sboty
    real(fp)               , intent(out) :: ssusx
    real(fp)               , intent(out) :: ssusy
    real(fp)                             :: tp    !  Description and declaration in esm_alloc_real.f90
    real(fp)               , intent(in)  :: u
    real(fp)               , intent(in)  :: v
    real(fp), dimension(30), intent(in)  :: par
    real(fp)               , intent(in)  :: vonkar
    logical                , intent(in)  :: ubot_from_com
!
!
! Local variables
!
    integer                        :: modind
    real(fp)                       :: abscos
    real(fp)                       :: acal
    real(fp)                       :: astar
    real(fp)                       :: astarc
    real(fp)                       :: cdrag
    real(fp)                       :: cj
    real(fp)                       :: coeffb
    real(fp)                       :: coeffp
    real(fp)                       :: coeffq
    real(fp)                       :: delta
    real(fp)                       :: dstar
    real(fp)                       :: eps
    real(fp)                       :: facth
    real(fp)                       :: factr
    real(fp)                       :: fw
    real(fp)                       :: ag                   !  gravity acceleration
    real(fp)                       :: k                    ! wave number
    real(fp)                       :: lfc
    real(fp)                       :: phi
    real(fp)                       :: phicur
    real(fp)                       :: phiwav
    real(fp)                       :: phix
    real(fp)                       :: phix1
    real(fp)                       :: phix2
    real(fp)                       :: phiy
    real(fp)                       :: rho                  !  array with densities [kg/m3]
    real(fp)                       :: rnu
    real(fp)                       :: taucur
    real(fp)                       :: taum
    real(fp)                       :: tauwav
    real(fp)                       :: thetam
    real(fp)                       :: thetaw
    real(fp)                       :: thetcr
    real(fp)                       :: thetmx
    real(fp)                       :: uorb                 ! orbital velocity at the bottom layer
    real(fp)                       :: utot                 ! flow velocity
    real(fp)                       :: waveps
    real(fp)                       :: xpar
    real(fp)                       :: ypar
    real(fp)                       :: z0
!
    real(fp), dimension(8, 4) :: aa, bb, mm, nn, pp, qq
    real(fp), dimension(8) :: coeffi, coeffj
    !
    data bb/0.29, 0.65, 0.27, 0.73, 0.22, 0.32, 0.47, -0.06, 0.55, 0.29, 0.51,  &
       & 0.40, 0.73, 0.55, 0.29, 0.26, -0.10, -0.30, -0.10, -0.23, -0.05, 0.00, &
       & -0.09, 0.08, -0.14, -0.21, -0.24, -0.24, -0.35, 0.00, -0.12, -0.03/
    !
    data pp/ - 0.77, -0.60, -0.75, -0.68, -0.86, -0.63, -0.70, -1.00, 0.10,     &
       & 0.10, 0.13, 0.13, 0.26, 0.05, 0.13, 0.31, 0.27, 0.27, 0.12, 0.24, 0.34,&
       & 0.00, 0.28, 0.25, 0.14, -0.06, 0.02, -0.07, -0.07, 0.00, -0.04, -0.26/
    !
    data qq/0.91, 1.19, 0.89, 1.04, -0.89, 1.14, 1.65, 0.38, 0.25, -0.68, 0.40, &
       & -0.56, 2.33, 0.18, -1.19, 1.19, 0.50, 0.22, 0.50, 0.34, 2.60, 0.00,    &
       & -0.42, 0.25, 0.45, -0.21, -0.28, -0.27, -2.50, 0.00, 0.49, -0.66/
    !
    data coeffj/3.00, 0.50, 2.70, 0.50, 2.70, 3.00, 0.60, 1.50/
    !-----for Tau_max
    data aa/ - 0.06, -0.01, -0.07, 0.11, 0.05, 0.00, -0.01, -0.45, 1.70, 1.84,  &
       & 1.87, 1.95, 1.62, 2.00, 1.58, 2.24, -0.29, -0.58, -0.34, -0.49, -0.38, &
       & 0.00, -0.52, 0.16, 0.29, -0.22, -0.12, -0.28, 0.25, 0.00, 0.09, -0.09/
    !
    data mm/0.67, 0.63, 0.72, 0.65, 1.05, 0.00, 0.65, 0.71, -0.29, -0.09, -0.33,&
       & -0.22, -0.75, 0.50, -0.17, 0.27, 0.09, 0.23, 0.08, 0.15, -0.08, 0.00,  &
       & 0.18, -0.15, 0.42, -0.02, 0.34, 0.06, 0.59, 0.00, 0.05, 0.03/
    !
    data nn/0.75, 0.82, 0.78, 0.71, 0.66, 0.00, 0.47, 1.19, -0.27, -0.30, -0.23,&
       & -0.19, -0.25, 0.50, -0.03, -0.66, 0.11, 0.19, 0.12, 0.17, 0.19, 0.00,  &
       & 0.59, -0.13, -0.02, -0.21, -0.12, -0.15, -0.03, 0.00, -0.50, 0.12/
    !
    data coeffi/0.80, 0.67, 0.82, 0.67, 0.82, 1.00, 0.64, 0.77/

!
!
!! executable statements -------------------------------------------------------
!
    !
    !     Initialize Transports to zero
    !     in case of small u, small h, very large h, u<ucr
    !
    sbotx = 0.0
    sboty = 0.0
    !
    !     Initialisations
    !
    astarc = 30.*pi**2
    waveps = 1.E-4
    eps = 1.E-6
    ag = par(1)
    rho = par(2)
    delta = par(4)
    rnu = par(5)
    acal = par(11)
    modind = nint(par(12))
    z0 = d50/par(13)
    facth = 1./(rho*ag*delta*d50)
    !
    !     Velocity magnitude
    !
    utot = u**2 + v**2
    if (utot>0.) utot = sqrt(utot)
    if (utot<0.000001 .or. h>200. .or. h<0.01) goto 999
    !
    !     Wave number k
    !
    if (tp>1.E-6) then
       !
       !     Prevent small tp
       !
       tp = max(tp,1.0_fp)
       !
       call wavenr(h         ,tp        ,k         ,ag        )
       if (ubot_from_com) then
          uorb = ubot
       else
          uorb = pi*hrms/tp/sinh(k*h)
       endif
       ! urms = uorb*0.7071
    else
       uorb = 0.0_fp
    endif
    !
    !
    !-----------wave (skin) friction factor (Swart) and drag coefficient
    !
    astar = tp*uorb/z0
    !     Swart
    if (astar>astarc) then
       fw = 0.00251*exp(14.1/(astar**0.19))
    else
       fw = 0.3
    endif
    !     Soulsby
    !            fw=1.39*(astar/2./pi)**(-0.52)
    !
    !-----------magnitude of bottom skin friction due to waves alone
    !           and due to current alone
    !
    tauwav = 0.5*rho*fw*uorb**2
    cdrag = (vonkar/(1. + log(z0/h)))**2
    taucur = rho*cdrag*utot**2
    !
    !           Angle between current and waves
    !
    phiwav = dir*degrad
    phicur = atan2(v, u)
    phi = phiwav - phicur
    abscos = abs(cos(phi))
    !
    !-----------parameterized models
    !
    if (tauwav<1.0E-8) then
       xpar = 1.
       ypar = 1.
    else
       xpar = taucur/(taucur + tauwav)
       if (xpar<1.0E-8) then
          ypar = 0.
       else
          lfc = log10(fw/cdrag)
          if (abscos>eps) then
             cj = abscos**coeffj(modind)
          else
             cj = 0.
          endif
          coeffb = (bb(modind, 1) + bb(modind, 2)*cj)                           &
                 & + (bb(modind, 3) + bb(modind, 4)*cj)*lfc
          coeffp = (pp(modind, 1) + pp(modind, 2)*cj)                           &
                 & + (pp(modind, 3) + pp(modind, 4)*cj)*lfc
          coeffq = (qq(modind, 1) + qq(modind, 2)*cj)                           &
                 & + (qq(modind, 3) + qq(modind, 4)*cj)*lfc
          ypar = xpar*(1.0 + coeffb*(xpar**coeffp)*((1.0 - xpar)**coeffq))
       endif
    endif
    !
    !-----------bottom friction for combined waves and current
    !
    taum = ypar*(taucur + tauwav)
    thetam = taum*facth
    thetaw = tauwav*facth
    !
    !           Soulsby p. 104, (ag(s-1)/nu^2)^(1/3) = 25926
    !                    for ag=9.81, s=2.65, nu=1e-6
    !
    !     dstar  = 25296.*d50
    dstar = (ag*delta/rnu**2)**(1./3.)*d50
    !
    !           Critical shear stress
    !           Soulsby p. 106, eq. 77
    !
    thetcr = 0.30/(1. + 1.2*dstar) + 0.055*(1. - exp( - 0.020*dstar))
    !
    !          Tau max needed to check the critical tau
    !          Soulsby  p. 168
    !
    thetmx = sqrt((thetam + thetaw*cos(phi))**2 + (thetaw*sin(phi))**2)
    !
    !           Check if critical tau exceeded
    !
    if (thetmx<=thetcr) goto 999
    !
    !
    !           Soulsby 167 eq. 129 a-d
    !           Dimensionless transport in current direction
    !
    phix1 = 12.*sqrt(thetam + waveps)*(thetam - thetcr)
    phix2 = 12.*(0.95 + 0.19*cos(2.*phi))*sqrt(thetaw + waveps)*thetam
    phix = max(phix1, phix2)
    !
    !
    !           Dimensionless transport perpendicular to current direction
    !
    phiy = 12.*(0.19*thetam*thetaw**2*sin(2.*phi))                              &
         & /((thetaw + waveps)**1.5 + 1.5*(thetam + waveps)**1.5)
    !
    !
    !           Dimensionless transport perpendicular to current direction
    !
    factr = sqrt(ag*delta*d50**3)
    sbotx = acal*factr/utot*(phix*u - phiy*v)
    sboty = acal*factr/utot*(phix*v + phiy*u)
    !
    !     End of routine
    !
  999 continue
    ssusx = 0.
    ssusy = 0.
end subroutine trab12
