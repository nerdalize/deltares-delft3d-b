subroutine fouana(mmax      ,nmaxus    ,nofou     ,ifou      ,kcs      , &
                & nst       ,rarray    ,gdp       )
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
!  $Id: fouana.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/fouana.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - performs fourier analysis i.e. computes suma
!                and sumb
!              - calculates MAX or MIN value
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
    integer        , dimension(:)        , pointer :: ftmsto
    integer        , dimension(:)        , pointer :: ftmstr
    real(fp)       , dimension(:)        , pointer :: foufas
    real(fp)       , dimension(:,:,:)    , pointer :: fousma
    real(fp)       , dimension(:,:,:)    , pointer :: fousmb
    character(1)   , dimension(:)        , pointer :: fouelp
!
! Global variables
!
    integer                                                                   , intent(in)  :: ifou   !!  Counter
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in)  :: nofou  !  Description and declaration in dimens.igs
    integer                                                                   , intent(in)  :: nst    !!  Time step number
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: rarray !!  Array for fourier analysis
!
! Local variables
!
    integer         :: m    ! Loop counter over MMAX 
    integer         :: n    ! Loop counter over NMAXUS 
    real(fp)        :: angl
!
!! executable statements -------------------------------------------------------
!
    ftmsto    => gdp%gdfourier%ftmsto
    ftmstr    => gdp%gdfourier%ftmstr
    foufas    => gdp%gdfourier%foufas
    fousma    => gdp%gdfourier%fousma
    fousmb    => gdp%gdfourier%fousmb
    fouelp    => gdp%gdfourier%fouelp
    !
    ! Initialize for MAX = -1.0e+30 / MIN = 1.0e+30
    !
    if (nst==ftmstr(ifou)) then
       if (fouelp(ifou)=='x') then
          fousma(:, :, ifou) = -1.0e+30_fp
       elseif (fouelp(ifou)=='i') then
          fousma(:, :, ifou) =  1.0e+30_fp
       else
          fousma(:, :, ifou) =  0.0_fp
          fousmb(:, :, ifou) =  0.0_fp
       endif
    endif
    !
    ! Perform fourier analysis, every timestep as long as NST value
    ! lies in requested time interval FTMSTR and FTMSTO
    !
    if (nst>=ftmstr(ifou) .and. nst<ftmsto(ifou)) then
       !
       ! Calculate MAX value
       !
       if (fouelp(ifou)=='x') then
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n, m) == 1) then
                   fousma(n, m, ifou) = max(fousma(n, m, ifou), rarray(n, m))
                endif
             enddo
          enddo
       !
       ! Calculate MIN value
       !
       elseif (fouelp(ifou)=='i') then
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n, m) == 1) then
                   fousma(n, m, ifou) = min(fousma(n, m, ifou), rarray(n, m))
                endif   
             enddo
          enddo
       !
       ! Calculate total for fourier analyse
       !
       else
          angl = real(nst - ftmstr(ifou),fp)*foufas(ifou)
          do n = 1, nmaxus
             do m = 1, mmax
                if (kcs(n, m) == 1) then
                   fousma(n, m, ifou) = fousma(n, m, ifou) + rarray(n, m)*cos(angl)
                   fousmb(n, m, ifou) = fousmb(n, m, ifou) + rarray(n, m)*sin(angl)
                endif   
             enddo
          enddo
       endif
    endif
    !
end subroutine fouana
