subroutine z_updtvol(nmmax     ,kmax      ,kcs       ,kcu       ,kcv       , &
                   & gsqs      ,dzs1      ,dzu0      ,dzv0      , &
                   & volum1    ,areau     ,areav     ,gdp       )
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
!  $Id: z_updtvol.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/z_updtvol.f90 $
!!--description-----------------------------------------------------------------
!
! Update volumes
! (to maintain compatibility with Delft3D-WAQ)
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
    logical                , pointer :: nonhyd
    integer                , pointer :: nh_level
!
! Global variables
!
    integer                                         , intent(in)  :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areau   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: areav   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzv0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: volum1  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: k
    integer :: nm
!
!! executable statements -------------------------------------------------------
!
    nonhyd      => gdp%gdprocs%nonhyd
    nh_level    => gdp%gdnonhyd%nh_level
    !
    do k = 1, kmax
       do nm = 1, nmmax
          volum1(nm, k) = dzs1(nm, k)*gsqs(nm)*min(1,kcs(nm))
       enddo
    enddo
    !
    if (nonhyd .and. nh_level==nh_full) then
       do k = 1, kmax
          do nm = 1, nmmax
             areau(nm, k) = dzu0(nm, k)*gsqs(nm)*min(1,kcu(nm))
             areav(nm, k) = dzv0(nm, k)*gsqs(nm)*min(1,kcv(nm))
          enddo
       enddo
    endif
end subroutine z_updtvol
