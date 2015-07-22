subroutine tranb5(u         ,v         ,d50       ,d90       ,chezy     , &
                & h         ,hrms      ,tp        ,dir       ,par       , &
                & dzdx      ,dzdy      ,sbotx     ,sboty     ,ssusx     , &
                & ssusy     ,cesus     ,vonkar    )
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
!  $Id: tranb5.f90 1304 2012-03-07 08:53:50Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/tranb5.f90 $
!!--description-----------------------------------------------------------------
! computes sediment transport according to
! bijker with wave effect
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
!
! Global variables
!
    real(fp)                             :: cesus
    real(fp)               , intent(in)  :: chezy
    real(fp)               , intent(in)  :: d50
    real(fp)               , intent(in)  :: d90
    real(fp)               , intent(in)  :: dir
    real(fp)                             :: dzdx
    real(fp)                             :: dzdy
    real(fp)                             :: h
    real(fp)               , intent(out) :: sbotx
    real(fp)               , intent(out) :: sboty
    real(fp)               , intent(out) :: ssusx
    real(fp)               , intent(out) :: ssusy
    real(fp)               , intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)               , intent(in)  :: u
    real(fp)               , intent(in)  :: v
    real(fp)                             :: hrms
    real(fp), dimension(30), intent(in)  :: par
    real(fp)               , intent(in)  :: vonkar
!
! Local variables
!
    integer                        :: ilun
    integer, external              :: newunit
    logical, save                  :: crstr
    logical, save                  :: exist
    logical, save                  :: first
    real(fp)                       :: ag                   ! gravity acceleration
    real(fp)                       :: arga
    real(fp)                       :: b                    ! correction coefficient shear stress
    real(fp)                       :: bd
    real(fp)                       :: bs
    real(fp)                       :: c                    ! velocity (es/ew) chezy waarde
    real(fp)                       :: c90
    real(fp)                       :: cf
    real(fp)                       :: critd
    real(fp)                       :: crits
    real(fp)                       :: delta                ! relative density of sediment particle
    real(fp)                       :: eps                  ! converge criterium for
    real(fp)                       :: fac1
    real(fp)                       :: fac2
    real(fp)                       :: kw
    real(fp)                       :: por
    real(fp)                       :: relhrms
    real(fp)                       :: ri1
    real(fp)                       :: ri2
    real(fp)                       :: rk
    real(fp)                       :: rkh
    real(fp)                       :: rmu
    real(fp)                       :: sbeta
    real(fp)                       :: sbksi
    real(fp)                       :: sbot
    real(fp)                       :: sbota
    real(fp)                       :: sseta
    real(fp)                       :: ssksi
    real(fp)                       :: ssus
    real(fp)                       :: t                    ! continuity equation time in seconds
    real(fp)                       :: theta
    real(fp)                       :: uo                   ! orbital velocity
    real(fp)                       :: utot
    real(fp)                       :: uuvar                ! marginal depths in tidal flats
    real(fp)                       :: uxmean
    real(fp)                       :: uymean
    real(fp)                       :: vstar
    real(fp)                       :: w                    ! flow velocity in z
    real(fp)                       :: z
    real(fp)                       :: zfact
    real(fp), external             :: fgyint
    real(hp), external             :: termfy
    real(hp), external             :: termgy
    real(fp), save                 :: epssl, faca, facu
    !
    !
    data first/.true./
!
!! executable statements -------------------------------------------------------
!
    if (first) then
       inquire (file = 'coef.inp', exist = exist)
       if (exist) then
          ilun = newunit()
          open (ilun, file = 'coef.inp')
          read (ilun, *) faca
          read (ilun, *) facu
          read (ilun, *) epssl
          close (ilun)
          crstr = .true.
          write (*, '(A,/,A)') ' File coef.inp is read',                        &
                              & ' Cross-shore transport is accounted for'
       else
          faca = 0.
          facu = 0.
          epssl = 0.
          crstr = .false.
       endif
       first = .false.
    endif
    sbot = 0.0
    ssus = 0.0
    sbotx = 0.0
    sboty = 0.0
    ssusx = 0.0
    ssusy = 0.0
    cesus = 0.0
    !
    ag = par(1)
    delta = par(4)
    bs = par(11)
    bd = par(12)
    crits = par(13)
    critd = par(14)
    rk = par(16)
    w = par(17)
    por = par(18)
    if (tp<1E-6) then
       t = par(19)
    else
       t = tp
    endif
    !
    if ((h/rk<=1.33) .or. (h>200.)) then
       return
    endif
    if (chezy<1.E-6) then
       c = 18.*log10(12.*h/rk)
    else
       c = chezy
    endif
    uuvar = 0.0
    call wavenr(h         ,t         ,kw        ,ag        )
    uxmean = u
    uymean = v
    theta = dir*degrad
    utot = sqrt(uxmean*uxmean + uymean*uymean)
    if (utot>1.0E-10) uuvar = utot*utot
    if (t>1.E-6) call wave(uo, t, uuvar, pi, hrms, c, rk, h, ag, kw)
    cf = ag/c/c
    relhrms = hrms/h
    b = bs
    if (critd<relhrms .and. relhrms<crits) then
       fac1 = (bs - bd)/(crits - critd)
       fac2 = bd - fac1*critd
       b = fac2 + fac1*relhrms
    endif
    if (relhrms<=critd) b = bd
    c90 = 18.*log10(12.*h/d90)
    rmu = c/c90
    rmu = rmu*sqrt(rmu)
    if (uuvar>1.0E-20) then
       arga = -.27*delta*d50*c*c/(rmu*uuvar)
       arga = max(arga, -50.0_fp)
       arga = min(arga, 50.0_fp)
       vstar = sqrt(cf)*sqrt(uuvar)
       z = w/vonkar/vstar
       z = min(z, 8.0_fp)
    else
       arga = -50.0
       z = 8.0
    endif
    sbota = b*d50/c*sqrt(ag)*exp(arga)*(1. - por)
    eps = .001
    rkh = rk/h
    ri1 = .216*rkh**(z - 1.)/(1. - rkh)**z*fgyint(rkh, 1.0_fp, z, eps, termfy)
    ri2 = .216*rkh**(z - 1.)/(1. - rkh)**z*fgyint(rkh, 1.0_fp, z, eps, termgy)
    zfact = 1.83
    cesus = zfact*sbota*(ri1*log(33.0/rkh) + ri2)
    if (crstr) then
       call bailtr(h         ,hrms      ,t         ,theta     ,w         , &
                 & dzdx      ,dzdy      ,sbksi     ,sbeta     ,ssksi     , &
                 & sseta     ,epssl     ,faca      ,facu      )
!       write(*,'(10e8.2)')h,hrms,t,theta,w,dzdx,sbksi,ssksi,epssl,faca
    else
       sbksi = 0.
       sbeta = 0.
       ssksi = 0.
       sseta = 0.
    endif
    if (utot>1.0E-10) then
       sbot = sbota*utot
       ssus = cesus*utot
       sbotx = sbot*uxmean/utot + sbksi + ssksi
       sboty = sbot*uymean/utot + sbeta + sseta
       ssusx = ssus*uxmean/utot
       ssusy = ssus*uymean/utot
    else
       sbot = 0.0
       ssus = 0.0
       sbotx = sbksi + ssksi
       ssusx = 0.0
       sboty = sbeta + sseta
       ssusy = 0.0
    endif
end subroutine tranb5
