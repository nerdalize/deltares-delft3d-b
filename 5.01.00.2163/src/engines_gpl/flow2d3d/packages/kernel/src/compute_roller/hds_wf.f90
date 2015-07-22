subroutine hds_wf(kfs       ,dps       ,s1        ,xcor      ,ycor      ,&
                & nmax      ,mmax      ,theta     ,rlabda    ,&
                & hbd       ,f_lam     ,gdp       )
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
!  $Id: hds_wf.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/hds_wf.f90 $
!!--description-----------------------------------------------------------------
!
! Adapted from Reniers' Code for Breaker Delay
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                                     , intent(in)  :: mmax
    integer                                                     , intent(in)  :: nmax
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in ) :: kfs     !  Description and declaration in esm_alloc_int.f90 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: theta
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: rlabda  !  Description and declaration in esm_alloc_real.f90 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: xcor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: ycor    !  Description and declaration in esm_alloc_real.f90 
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: hbd     !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                    , intent(in)  :: f_lam
!
! Local variables
!
    integer                 :: j
    integer                 :: m
    integer                 :: pos
    integer                 :: mx
    integer                 :: n
    real(fp)                :: f_lamt
    real(fp)                :: fic
    real(fp)                :: fis
    real(fp)                :: grut
    real(fp)                :: grvt
    real(fp)                :: hdps
    real(fp)                :: mrun
    real(fp)                :: mrup
    real(fp)                :: mrvn
    real(fp)                :: mrvp
    real(fp)                :: sdx
    real(fp), dimension(1:mmax) :: dx
!
! executable statements -------------------------------------------------------
!
    dx(1) = 0.0 
    if (f_lam < 0.0) f_lamt=-f_lam
    do m = 1, mmax
       do n = 1, nmax
          if (kfs(n, m)==1) then
             mx = 1
             sdx  = 0.0
             mrup = 0.0
             mrun = 0.0
             mrvp = 0.0
             mrvn = 0.0
             !
             ! delay in negative x-direction
             !
             do pos = m-1,1,-1
                dx(mx + 1) = xcor(n, m) - xcor(n, pos)
                if (kfs(n, pos)==1 .and. dx(mx + 1)<f_lamt*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dx(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps= max(real(dps(n, m - j + 1),fp)+s1(n, m - j + 1),0.01_fp)
                   mrun = mrun + hdps*(dx(mx) - dx(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrun = hdps
             endif
             !
             ! delay in positive x-direction
             !
             mx  = 1
             sdx = 0.0
             do pos = m+1,mmax,1
                dx(mx + 1) = xcor(n, pos) - xcor(n, m)
                if (kfs(n, pos)==1 .and. dx(mx + 1)<f_lamt*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dx(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps= max(real(dps(n, m + j - 1),fp)+s1(n, m + j - 1),0.01_fp)
                   mrup = mrup + hdps*(dx(mx) - dx(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrup = hdps
             endif
             !
             ! delay in positive y-direction
             !
             mx  = 1
             sdx = 0.0
             do pos = n+1,nmax,1
                dx(mx + 1) = ycor(pos, m) - ycor(n, m)
                if (kfs(pos, m)==1 .and. dx(mx + 1)<f_lamt*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dx(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps= max(real(dps(n + j - 1, m),fp)+s1(n + j - 1, m),0.01_fp)
                   mrvp = mrvp + hdps*(dx(mx) - dx(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrvp = hdps
             endif
             !
             ! delay in negative y-direction
             !
             mx  = 1
             sdx = 0.0
             do pos = n-1,1,-1
                dx(mx + 1) = ycor(n, m) - ycor(pos, m)
                if (kfs(pos, m)==1 .and. dx(mx + 1)<f_lamt*rlabda(n,m)) then
                   mx  = mx + 1
                   sdx = sdx + dx(mx)
                else
                   exit
                endif
             enddo
             if (sdx>0.0) then
                do j = 1, mx
                   hdps= max(real(dps(n - j + 1, m),fp)+s1(n - j + 1, m),0.01_fp)
                   mrvn = mrvn + hdps*(dx(mx) - dx(j))/sdx
                enddo
             else
                hdps = max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
                mrvn = hdps
             endif
             !
             ! combine the contributions to obtain delay in wave direction
             !
             fis = sin(theta(n, m)*degrad)/max(abs(sin(theta(n, m)*degrad)),0.01_fp)
             fic = cos(theta(n, m)*degrad)/max(abs(cos(theta(n, m)*degrad)),0.01_fp)
             !
             grvt = sin(theta(n, m)*degrad)*((1.0 + fis)*mrvp + (1.0 - fis)*mrvn)/2.0
             grut = cos(theta(n, m)*degrad)*((1.0 + fic)*mrun + (1.0 - fic)*mrup)/2.0
             !
             !  total delayed flux in m.n point
             !
             hbd(n, m) =  max(sqrt(grvt**2 + grut**2),0.01_fp)
          else
             hbd(n, m) =  max(real(dps(n, m),fp)+s1(n, m),0.01_fp)
          endif
       enddo
    enddo
end subroutine hds_wf
