subroutine dens(j         ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
              & lsal      ,ltem      ,lsed      ,saleqs    ,temeqs    , &
              & densin    ,zmodel    ,thick     ,r1        ,rho       , &
              & sumrho    ,rhowat    ,rhosol    ,gdp       )
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
!  $Id: dens.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/dens.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!              Computes mud-water density given sediment
!              mass concentrations (rho).
! Method used: Equation of state following Eckart, (C. Eckart,
!              The equation of state of water and sea water at
!              low temperatures and pressures, American Journal
!              of Science, april 1958) or following UNESCO
!              (UNESCO, Algorithms for computation of fundamental
!              properties of seawater, UNESCO technical papers
!              in marine science, 1983)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: rhow
    integer                , pointer :: idensform
!
! Global variables
!
    integer                                                              :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer, intent(in)                                                  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                  :: lsal   !  Description and declaration in dimens.igs
    integer, intent(in)                                                  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                  :: ltem   !  Description and declaration in dimens.igs
    integer, intent(in)                                                  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                              :: nmmaxj !  Description and declaration in dimens.igs
    logical, intent(in)                                                  :: densin !  Description and declaration in morpar.igs
    logical, intent(in)                                                  :: zmodel !  Description and declaration in procs.igs
    real(fp), intent(in)                                                 :: saleqs !  Description and declaration in tricom.igs
    real(fp), intent(in)                                                 :: temeqs !  Description and declaration in tricom.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                     :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                     :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                     :: sumrho !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in) :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                               , intent(in) :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lsed)                               , intent(in) :: rhosol !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: i
    integer  :: k
    integer  :: l
    integer  :: ll
    integer  :: lst
    integer  :: nm
    real(fp) :: dummy

!
!! executable statements -------------------------------------------------------
!
    ! Notice:
    ! Avoid a select case statement inside a double do-loop
    !
    rhow      => gdp%gdphysco%rhow
    idensform => gdp%gdphysco%idensform
    !
    !
    ! COMPUTE DENSITIES AFTER SALINITY OR TEMPERATURE-COMPUTATION
    !
    if (lsal/=0 .or. ltem/=0) then
       if (ltem == 0) then
          select case (idensform)
          case( dens_Eckart )
             do k = 1, kmax
                do i = 1, nmmax
                   call dens_eck(temeqs, r1(i,k,lsal),rhowat(i,k), dummy, dummy )
                enddo
             enddo
          case( dens_UNESCO )
             do k = 1, kmax
                do i = 1, nmmax
                   call dens_unes(temeqs, r1(i,k,lsal),rhowat(i,k), dummy, dummy )
                enddo
             enddo
          end select
       elseif (lsal == 0) then
          !
          ! CONSTANT SALINITY; TEMPERATURE IS TIME and SPACE DEPENDENT
          !
          select case (idensform)
          case( dens_Eckart )
             do k = 1, kmax
                do i = 1, nmmax
                   call dens_eck   (r1(i,k,ltem), saleqs, rhowat(i,k), dummy, dummy)
                enddo
             enddo
          case( dens_UNESCO )
             do k = 1, kmax
                do i = 1, nmmax
                   call dens_unes   (r1(i,k,ltem), saleqs, rhowat(i,k), dummy, dummy)
                enddo
             enddo
          end select
       else
          !
          ! SALINITY AND TEMPERATURE ARE TIME and SPACE DEPENDENT (COMPUTED)
          !
          select case (idensform)
          case( dens_Eckart )
             do k = 1, kmax
                do i = 1, nmmax
                   call dens_eck   ( r1(i,k,ltem), r1(i,k,lsal), rhowat(i,k), dummy, dummy )
                enddo
             enddo
          case( dens_UNESCO )
             do k = 1, kmax
                do i = 1, nmmax
                   call dens_unes  ( r1(i,k,ltem), r1(i,k,lsal), rhowat(i,k), dummy, dummy )
                enddo
             enddo
          end select
       endif
    else
       !
       ! CONSTANT DENSITY
       !
       do k = 1, kmax
          do i = 1, nmmax
             rhowat(i, k) = rhow
          enddo
       enddo
    endif
    !
    ! COPY RHOWAT TO RHO
    !
    do k = 1, kmax
       do i = 1, nmmax
          rho(i, k) = rhowat(i, k)
       enddo
    enddo
    !
    ! SEDIMENT LOAD
    !
    if (densin) then
       lst = max(lsal, ltem)
       do l = 1, lsed
          ll = lst + l
          do k = 1, kmax
             do nm = 1, nmmax
                rho(nm, k) = rho(nm, k) + r1(nm, k, ll) - rhowat(nm, k)         &
                           & *r1(nm, k, ll)/rhosol(l)
             enddo
          enddo
       enddo
    endif
    !
    ! DETERMINE FLUID DENSITY OVER WATER COLUMN; ONLY FOR SIGMA
    !
    if (.not.zmodel) then
       do nm = 1, nmmax
          sumrho(nm, 1) = .5*thick(1)*rho(nm, 1)
       enddo
       do k = 2, kmax
          do nm = 1, nmmax
             sumrho(nm, k) = sumrho(nm, k - 1)                                  &
                           & + .5*(thick(k)*rho(nm, k) + thick(k - 1)           &
                           & *rho(nm, k - 1))
          enddo
       enddo
    endif
end subroutine dens
