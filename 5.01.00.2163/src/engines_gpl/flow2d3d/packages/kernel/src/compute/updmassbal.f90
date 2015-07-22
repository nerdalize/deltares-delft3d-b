subroutine updmassbal(newvol    ,qxk       ,qyk       ,kcs       ,r1        , &
                    & volum1    ,sbuu      ,sbvv      ,ssuu      ,ssvv      , &
                    & gsqs      ,guu       ,gvv       ,dps       ,gdp       )
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
!  $Id: updmassbal.f90 1303 2012-03-07 08:34:27Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/updmassbal.f90 $
!!--description-----------------------------------------------------------------
!
!

!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: kmax
    integer                        , pointer :: nmmax
    integer                        , pointer :: lsed
    integer                        , pointer :: lsedtot
    integer                        , pointer :: lstsci
    real(fp)                       , pointer :: hdt
    logical                        , pointer :: massbal
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
    logical                        , pointer :: resetfluxes
    integer      , dimension(:)    , pointer :: volnr
    integer      , dimension(:,:)  , pointer :: exchnr
    integer      , dimension(:,:)  , pointer :: neighb
    real(fp)     , dimension(:)    , pointer :: accdps
    real(fp)     , dimension(:)    , pointer :: volumes
    real(fp)     , dimension(:,:)  , pointer :: mass_r1
    real(fp)     , dimension(:,:)  , pointer :: fluxes
    real(fp)     , dimension(:,:,:), pointer :: fluxes_r1
    real(fp)     , dimension(:,:,:), pointer :: fluxes_sd
    !
    real(fp)                       , pointer :: morfac
    real(fp)     , dimension(:,:,:), pointer :: fluxu
    real(fp)     , dimension(:,:,:), pointer :: fluxv
!
! Global variables
!
    logical                                                     :: newvol
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                :: kcs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax,1:gdp%d%lstsci) :: r1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)   :: volum1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)   :: qxk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,1:gdp%d%kmax)   :: qyk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsedtot)  :: sbuu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsedtot)  :: sbvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsed)     :: ssuu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,gdp%d%lsed)     :: ssvv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: gsqs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: guu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                :: gvv
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                :: dps
!
! Local variables
!
    integer                                 :: ex
    integer                                 :: ibnd
    integer                                 :: j
    integer                                 :: ivol
    integer                                 :: jvol
    integer                                 :: k
    integer                                 :: l
    integer                                 :: nm
    integer                                 :: nm2
    integer                                 :: nmaxddb
    real(fp)                                :: hdtmor
    real(fp)                                :: q
    real(fp)                                :: vol
!
!! executable statements -------------------------------------------------------
!
    massbal     => gdp%gdmassbal%massbal
    if (.not. massbal) return
    !
    kmax           => gdp%d%kmax
    nmmax          => gdp%d%nmmax
    lsed           => gdp%d%lsed
    lsedtot        => gdp%d%lsedtot
    lstsci         => gdp%d%lstsci
    hdt            => gdp%gdnumeco%hdt
    !
    nbalpol        => gdp%gdmassbal%nbalpol
    nneighb        => gdp%gdmassbal%nneighb
    resetfluxes    => gdp%gdmassbal%resetfluxes
    volnr          => gdp%gdmassbal%volnr
    exchnr         => gdp%gdmassbal%exchnr
    neighb         => gdp%gdmassbal%neighb
    accdps         => gdp%gdmassbal%accdps
    volumes        => gdp%gdmassbal%volumes
    mass_r1        => gdp%gdmassbal%mass_r1
    fluxes         => gdp%gdmassbal%fluxes
    fluxes_r1      => gdp%gdmassbal%fluxes_r1
    fluxes_sd      => gdp%gdmassbal%fluxes_sd
    !
    morfac         => gdp%gdmorpar%morfac
    fluxu          => gdp%gdflwpar%fluxu
    fluxv          => gdp%gdflwpar%fluxv
    !
    ibnd = nbalpol+1
    nmaxddb = gdp%d%nmax + 2*gdp%d%ddbound
    !
    ! If volumes were determined during previous call, then the accumulation of
    ! the fluxes should start over again.
    !
    if (resetfluxes) then
       fluxes = 0.0_fp
       fluxes_r1 = 0.0_fp
       fluxes_sd = 0.0_fp
       resetfluxes = .false.
    endif
    !
    ! Accumulate water fluxes
    !
    do k = 1,kmax
       do nm = 1, nmmax
          ivol = volnr(nm)
          if (ivol==0) cycle
          !
          do j=1,2
             if (j==1) then
                nm2 = nm+nmaxddb
                q = qxk(nm,k)*hdt
             else
                nm2 = nm+1
                q = qyk(nm,k)*hdt
             endif
             !
             jvol = volnr(nm2)
             if (jvol==0) cycle
             ex = exchnr(j,nm)
             if (ex==0) cycle
             !
             if (ivol==neighb(2,ex)) q = -q
             if (q>0.0_fp) then
                fluxes(1,ex) = fluxes(1,ex) + q
             else
                fluxes(2,ex) = fluxes(2,ex) - q
             endif
          enddo
       enddo
    enddo
    !
    ! Accumulate constituent fluxes; the fluxu array is not yet available upon
    ! the first call from INCHKR (tested by means of associated). However,
    ! that's no problem since no fluxes have to be accumulated at that time.
    !
    if (associated(fluxu)) then
       do l = 1,lstsci
          do k = 1,kmax
             do nm = 1, nmmax
                ivol = volnr(nm)
                if (ivol==0) cycle
                !
                do j = 1,2
                   if (j==1) then
                      nm2 = nm+nmaxddb
                      q = fluxu(nm,k,l)*hdt
                   else
                      nm2 = nm+1
                      q = fluxv(nm,k,l)*hdt
                   endif
                   !
                   jvol = volnr(nm2)
                   if (jvol==0) cycle
                   ex = exchnr(j,nm)
                   if (ex==0) cycle
                   !
                   if (ivol==neighb(2,ex)) q = -q
                   if (q>0.0_fp) then
                      fluxes_r1(1,ex,l) = fluxes_r1(1,ex,l) + q
                   else
                      fluxes_r1(2,ex,l) = fluxes_r1(2,ex,l) - q
                   endif
                enddo
             enddo
          enddo
       enddo
       !
       ! TODO: accumulate 2D fluxes (e.g. anticreep)
       !
    endif
    !
    ! Accumulate bed load and suspended sediment fluxes.
    ! Why not accumulate bed load only since the suspended part is also
    ! included in the FLUX_R1 part? Main reason: SSUU includes SUCOR whereas
    ! the FLUX_R1 computed above doesn't include the SUCOR part. It would be
    ! inconsistent if we were to include SUCOR here in the SBUU accumulation
    ! but in SSUU on the map-file.
    !
    if (lsedtot>0) then
       hdtmor  = hdt*morfac
       !
       do l = 1,lsedtot
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol==0) cycle
             !
             do j = 1,2
                if (j==1) then
                   nm2 = nm+nmaxddb
                   q = sbuu(nm,l)
                   if (l<=lsed) q = q+ssuu(nm,l)
                   q = q*guu(nm)*hdtmor
                else
                   nm2 = nm+1
                   q = sbvv(nm,l)
                   if (l<=lsed) q = q+ssvv(nm,l)
                   q = q*gvv(nm)*hdtmor
                endif
                !
                jvol = volnr(nm2)
                if (jvol==0) cycle
                ex = exchnr(j,nm)
                if (ex==0) cycle
                !
                if (ivol==neighb(2,ex)) q = -q
                if (q>0.0_fp) then
                   fluxes_sd(1,ex,l) = fluxes_sd(1,ex,l) + q
                else
                   fluxes_sd(2,ex,l) = fluxes_sd(2,ex,l) - q
                endif
             enddo
          enddo
       enddo
    endif
    !
    ! TODO: Accumulate fluxes across domains/partitions.
    !
    ! If requested: compute total volume and mass
    !
    if (newvol) then
       volumes = 0.0_fp
       mass_r1 = 0.0_fp
       accdps  = 0.0_fp
       !
       ! Determine volumes
       !
       do k = 1,kmax
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol>0 .and. kcs(nm)==1) then
                volumes(ivol)   = volumes(ivol) + volum1(nm,k)
             endif
          enddo
       enddo
       !
       ! Determine "mass" (i.e. volume*concentration)
       !
       do l = 1, lstsci
          do k = 1,kmax
             do nm = 1, nmmax
                ivol = volnr(nm)
                if (ivol>0 .and. kcs(nm)==1) then
                   mass_r1(ivol,l) = mass_r1(ivol,l) + volum1(nm,k)*r1(nm,k,l)
                endif
             enddo
          enddo
       enddo
       !
       ! Determine cumulative depth
       !
       if (lsedtot>0) then
          do nm = 1, nmmax
             ivol = volnr(nm)
             if (ivol>0 .and. kcs(nm)==1) then
                accdps(ivol) = accdps(ivol) + gsqs(nm)*real(dps(nm),fp)
             endif
          enddo
       endif
       !
       ! TODO: Accumulate volumes and masses across domains/partitions.
       !
       ! At start of next step: reset fluxes to 0.
       !
       !resetfluxes = .true.
    endif
end subroutine updmassbal
