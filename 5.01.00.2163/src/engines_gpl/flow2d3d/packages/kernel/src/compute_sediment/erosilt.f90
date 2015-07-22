subroutine erosilt(thick    ,kmax     ,ws       ,wstau    ,entr     ,lundia   , &
                 & h0       ,h1       ,error    ,fixfac   ,srcmax   , &
                 & frac     ,sinkse   ,sourse   ,oldmudfrac, flmd2l , tcrdep  , &
                 & tcrero   ,eropar   ,iform    , &
                 & numintpar,numrealpar,numstrpar,dllfunc ,dllhandle, &
                 & intpar   ,realpar  ,strpar   )
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
!  $Id: erosilt.f90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/erosilt.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes sediment fluxes at the bed using
!              the Partheniades-Krone formulations.
!              Arrays SOURSE and SINKSE are filled
! Method used:
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    include 'sedparams.inc'
    include 'trapar.inc'
    !
    integer                             , intent(in)    :: iform
    integer                             , intent(in)    :: numintpar
    integer                             , intent(in)    :: numrealpar
    integer                             , intent(in)    :: numstrpar
    integer                             , intent(in)    :: kmax
    integer                                             :: lundia   !  Description and declaration in inout.igs
    integer       , dimension(numintpar), intent(inout) :: intpar
    integer(pntrsize)                   , intent(in)    :: dllhandle
    real(fp)                            , intent(in)    :: entr
    real(fp)       , dimension(0:kmax)  , intent(in)    :: ws
    real(fp)                            , intent(out)   :: wstau
    real(fp)       , dimension(kmax)    , intent(in)    :: thick
    real(fp)                            , intent(in)    :: h0
    real(fp)                            , intent(in)    :: h1
    real(fp)                            , intent(in)    :: fixfac
    real(fp)                            , intent(in)    :: srcmax
    real(fp)                            , intent(in)    :: frac
    real(fp)                            , intent(out)   :: sinkse
    real(fp)                            , intent(out)   :: sourse
    real(fp)                            , intent(in)    :: tcrdep
    real(fp)                            , intent(in)    :: tcrero
    real(fp)                            , intent(in)    :: eropar
    real(hp)     , dimension(numrealpar), intent(inout) :: realpar
    logical                             , intent(out)   :: error
    logical                             , intent(in)    :: oldmudfrac
    logical                             , intent(in)    :: flmd2l
    character(256), dimension(numstrpar), intent(inout) :: strpar
    character(256)                      , intent(in)    :: dllfunc
!
! Local variables
!
    integer  :: k
    real(fp) :: sour
    real(fp) :: sink
    real(fp) :: taub
    real(fp) :: taum
    real(fp) :: thick0
    real(fp) :: thick1
    !
    ! Interface to dll is in High precision!
    !
    real(hp)                    :: sink_dll
    real(hp)                    :: sour_dll
    integer(pntrsize)           :: ierror_ptr
    integer(pntrsize), external :: perf_function_erosilt
    character(256)              :: message     ! Contains message from
!
!! executable statements ------------------
!
    error  = .false.
    !
    ! Calculate total (possibly wave enhanced) roughness
    !
    thick0 = thick(kmax) * h0
    thick1 = thick(kmax) * h1
    taub   = real(realpar(RP_TAUB), fp)
    !
    ! Bed transport following Partheniades and Krone
    ! but in case of fluid mud, source term is determined by
    ! fluid mud part (sourmu). Information is passed via entr()
    ! maximum erosion is sediment available at bed (ignores sediment
    ! settling during the current morphological timestep)
    ! In case of fluid mud the maximum erosion is determined in sourmu
    ! of the fluid mud module. So ignore this check when fluid mud.
    ! Also, taum is not required in the formulation since whether or not
    ! and how much entrainment occurs is entirely handled by the sourmu
    ! routine.
    !
    ! calculation both for mud and floc
    !
    if (flmd2l) then
       !
       ! maximum erosion is sediment available at bed
       ! (ignores sediment settling during the current morphological timestep)
       !
       sour = entr
       if (tcrdep > 0.0) then
          sink = max(0.0_fp , 1.0-taub/tcrdep)
       else
          sink = 0.0
       endif
    else
       if (iform == -1) then
          !
          ! Default Partheniades-Krone formula
          !
          taum = max(0.0_fp, taub/tcrero - 1.0)
          sour = eropar * taum
          if (tcrdep > 0.0) then
             sink = max(0.0_fp , 1.0-taub/tcrdep)
          else
             sink = 0.0
          endif
       elseif (iform == 15) then
          !
          ! Initialisation of output variables of user defined transport formulae
          !
          sink_dll    = 0.0_hp
          sour_dll    = 0.0_hp
          message     = ' '
          !
          ! psem/vsem is used to be sure this works fine in DD calculations
          !
          call psemlun
          ierror_ptr = 0
          ierror_ptr = perf_function_erosilt(dllhandle       , dllfunc           , &
                                             intpar          , numintpar         , &
                                             realpar         , numrealpar        , &
                                             strpar          , numstrpar         , &
                                             sink_dll        , sour_dll          , &
                                             message)
          call vsemlun
          if (ierror_ptr /= 0) then
             write(lundia,'(a,a,a)') '*** ERROR Cannot find function "',trim(dllfunc),'" in dynamic library.'
             error = .true.
             return
          endif
          if (message /= ' ') then
             write (lundia,'(a,a,a)') '*** ERROR Message from user defined erosion/deposition formulae ',trim(dllfunc),' :'
             write (lundia,'(a,a  )') '          ', trim(message)
             write (lundia,'(a    )') ' '
             error = .true.
             return
          endif
          !
          ! Output parameters
          !
          sour    = real(sour_dll,fp)
          sink    = real(sink_dll,fp)
       endif
    endif
    !
    wstau         = ws(kmax) * sink
    !
    if (.not.flmd2l) then
       if (oldmudfrac) then
          sour = fixfac * sour
       else
          sour = fixfac * frac * sour
       endif
    endif
    !
    sour   = min(sour, srcmax)
    !
    sourse = sour / thick0
    sinkse = wstau / thick1
end subroutine erosilt
