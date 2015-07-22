subroutine trab10(utot      ,d         ,c         ,h         ,cosa      , &
                & sina      ,dzbdx     ,dzbdy     ,par       ,sbot      , &
                & ssus      )
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
!  $Id: trab10.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/trab10.f90 $
!!--description-----------------------------------------------------------------
!
! computes sediment transport according to
! the formula of Ashida and Michiue
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp)           , intent(in)  :: c     ! chezy value !  Description and declaration in esm_alloc_real.f90
    real(fp)           , intent(in)  :: cosa  ! cosine of shear stress direction at the bed (= near bed flow direction)
    real(fp)           , intent(in)  :: d     ! Grain size specified as d50
    real(fp)           , intent(in)  :: dzbdx ! x-component of bed level gradient
    real(fp)           , intent(in)  :: dzbdy ! y-component of bed level gradient
    real(fp)           , intent(in)  :: h     ! water depth
    real(fp)           , intent(out) :: sbot  ! bed load transport
    real(fp)           , intent(in)  :: sina  ! sine of shear stress direction at the bed (= near bed flow direction)
    real(fp)           , intent(out) :: ssus  ! suspended sediment transport
    real(fp)           , intent(in)  :: utot  ! flow velocity
    real(fp), dimension(30), intent(in)  :: par   ! sediment parameter list
!
! Local variables
!
    integer :: mode
    real(fp):: acal   ! tuning constant of sediment transport
    real(fp):: acalb
    real(fp):: ag     ! gravity acceleration
    real(fp):: delta  ! relative density of sediment particle
    real(fp):: dgd
    real(fp):: hulp
    real(fp):: sbota
    real(fp):: tasck0
    real(fp):: task
    real(fp):: taskf
    real(fp):: tausek
    real(fp):: tausm
    real(fp):: thcr   ! critical shields number
    real(fp):: ustar
    real(fp):: ustarc
    real(fp):: ustare
    real(fp):: xkc
    real(fp):: xku
    real(fp):: xmus   ! static friction coefficient of sediment
!
!! executable statements -------------------------------------------------------
!
    sbot = 0.0
    ssus = 0.0
    !
    ag = par(1)
    delta = par(4)
    acal = par(11)
    xmus = par(13)
    thcr = par(12)
    !
    if ((c<1.E-6) .or. (utot<1.E-6)) then
       return
    endif
    dgd = delta*ag*d
    xkc = 1.0 - ((1. + 1./delta)*cosa*dzbdx + sina*dzbdy)/xmus
    mode = 1
    if (mode==1) then
       tasck0 = par(12)
       taskf = par(18)
       hulp = xkc*tasck0/taskf
       xku = 0.0
       if (hulp>0.0) xku = sqrt(hulp)
       task = par(17)
       tausek = task
       acalb = par(19)
    else
       ustar = sqrt(ag)*utot/c
       tausm = (ustar)**2/(delta*ag*d)
       ustare = utot/(6.0 + 2.5*log(h/d/(1.0 + 2.0*tausm)))
       tausek = ustare**2/dgd
       ustarc = sqrt(dgd*thcr)
       xku = sqrt(xkc)*ustarc/ustar
       acalb = 1.0
    endif
    sbota = 17.0*acal*d*sqrt(dgd)*tausek**1.5/acalb
    if (xku<1.0) then
       sbot = sbota*(1.0 - xku**2)*(1.0 - xku)
    else
       sbot = 0.0
    endif
    ssus = 0.0
end subroutine trab10
