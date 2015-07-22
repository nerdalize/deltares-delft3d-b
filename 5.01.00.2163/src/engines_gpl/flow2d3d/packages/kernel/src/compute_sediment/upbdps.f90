subroutine upbdps(mmax      ,nmax      ,kcs       ,&
                & nmaxus    ,dp        ,dps       ,gdp       )
!!--copyright-------------------------------------------------------------------
! Copyright (c) 2008, WL | Delft Hydraulics. All rights reserved.
!!--disclaimer------------------------------------------------------------------
! This code is part of the Delft3D software system. WL|Delft Hydraulics has
! developed c.q. manufactured this code to its best ability and according to the
! state of the art. Nevertheless, there is no express or implied warranty as to
! this software whether tangible or intangible. In particular, there is no
! express or implied warranty as to the fitness for a particular purpose of this
! software, whether tangible or intangible. The intellectual property rights
! related to this software code remain with WL|Delft Hydraulics at all times.
! For details on the licensing agreement, we refer to the Delft3D software
! license and any modifications to this license, if applicable. These documents
! are available upon request.
!!--version information---------------------------------------------------------
!  $Id:  $
!  $HeadURL:  $
!!--description-----------------------------------------------------------------
!
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    
    implicit none

    type(globdat),target :: gdp
!
! Local parameters
!
    integer       :: ddb
    integer       :: m, n
!
! Global variables
!
    integer                     :: ierr   !  Flag for error when writing to Communication file 
    integer, intent(in)         :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)         :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)         :: nmaxus !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   :: dp  !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) :: dps !  Description and declaration in esm_alloc_real.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcs !  Description and declaration in esm_alloc_int.f90
!
!
! Local variables
!
!
!
!! executable statements -------------------------------------------------------
!
    ddb = gdp%d%ddbound
    do n = 1 - ddb, nmaxus
       do m = 1 - ddb, mmax
          dps(n, mmax) = real(dps(n, mmax-1),fp)
          dp (n, mmax) = real( dp(n, mmax-1),fp)
       enddo
    enddo
end subroutine upbdps
