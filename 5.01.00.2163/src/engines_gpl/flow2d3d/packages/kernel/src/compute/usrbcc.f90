subroutine usrbcc(j         ,nmmaxj    ,kmin      ,kmax      ,ldim      , &
                & icx       ,icy       ,nrow      ,nto       ,lustof    , &
                & iustof    ,mnbnd     ,ubnd      ,aakl      ,bbkl      , &
                & cckl      ,ddkl      ,gdp       )
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
!  $Id: usrbcc.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/usrbcc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: User defined boundary conditions for constituent
!              iustof using array UBND (LUSTOF,KMIN:KMAX,2,NTO)
!              Store in AAKL, BBKL, CCKL and DDKL.
! Method used:
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
!
! Global variables
!
    integer                                                    , intent(in)  :: icx    !!  Increment in the x-dir., if icx= nmax
                                                                                       !!  then computation proceeds in the x-
                                                                                       !!  dir. if icx=1 then computation pro-
                                                                                       !!  ceeds in the y-dir.
    integer                                                    , intent(in)  :: icy    !!  Increment in the y-dir. (see icx)
    integer                                                    , intent(in)  :: iustof !!  Requested constituents from user
                                                                                       !!  defined array (<= LUSTOF)
    integer                                                                  :: j      !!  Begin pointer for arrays which have
                                                                                       !!  been transformed into 1d arrays.
                                                                                       !!  due to the shift in the 2nd (m-)
                                                                                       !!  index, j = -2*nmax + 1
    integer                                                    , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                    , intent(in)  :: kmin   !!  Starting index of number of layers
                                                                                       !!  in the z-dir. (0 or 1)
    integer                                                    , intent(in)  :: ldim   !!  Dimension for constituents or turbu-
                                                                                       !!  lent quantities if required (>= 1)
    integer                                                    , intent(in)  :: lustof !!  Total number of constituents for
                                                                                       !!  user defined array (eq LUSTOF:=LTUR)
    integer                                                                  :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                    , intent(in)  :: nrow   !!  Flag = 1 for rows; = 2 for columns
    integer                                                    , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nto)                                 , intent(in)  :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(2, lustof, kmin:kmax, 2, nto)          , intent(in)  :: ubnd   !  Description and declaration in trisol.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax, ldim), intent(out) :: aakl   !!  Internal work array, lower diagonal
                                                                                       !!  tridiagonal matrix, implicit coupling
                                                                                       !!  of concentration in (n,m,k) with con-
                                                                                       !!  centration in (n,m,k-1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax, ldim), intent(out) :: bbkl   !!  Internal work array, main diagonal
                                                                                       !!  tridiagonal matrix, implicit coupling
                                                                                       !!  of concentration in (n,m,k)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax, ldim), intent(out) :: cckl   !!  Internal work array, upper diagonal
                                                                                       !!  tridiagonal matrix, implicit coupling
                                                                                       !!  of concentration in (n,m,k) with con-
                                                                                       !!  centration in (n,m,k+1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax, ldim), intent(out) :: ddkl   !!  Internal work array, diagonal space
                                                                                       !!  at (n,m,k,l)
!
! Local variables
!
    integer           :: ddb
    integer           :: icxy          ! Max (ICX,ICY) 
    integer           :: incx
    integer           :: incy
    integer           :: istof
    integer           :: ito
    integer           :: ix            ! Help var. 
    integer           :: iy            ! Help var. 
    integer           :: k
    integer           :: m
    integer           :: m1            ! M-coord. of the first  point 
    integer           :: m2            ! M-coord. of the second point 
    integer           :: maxinc        ! Maximum of (INCXA,INCYA) 
    integer           :: n1            ! N-coord. of the first  point 
    integer           :: n2            ! N-coord. of the second point 
    integer           :: nm            ! Iy*icy+ix*icx-icxy 
    logical           :: error         ! Flag=TRUE if an error is encountered 
    real(fp)          :: autest
    real(fp)          :: utest
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialisation local variable
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! Initialisation of constituent number in "xx"kl array's
    !
    istof = 1
    if (ldim>1) istof = iustof
    !
    ! Loop over boundary for rows
    ! The definition of the open boundary is checked after reading
    ! therefore the outcome of error in INCREM will never be .true.
    !
    do ito = 1, nto
       m1 = mnbnd(1, ito)
       n1 = mnbnd(2, ito)
       m2 = mnbnd(3, ito)
       n2 = mnbnd(4, ito)
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error     )
       utest = ubnd(nrow, iustof, kmin, 1, ito)
       autest = abs(ubnd(nrow, iustof, kmin, 1, ito))
       if (comparereal(autest, utest) == 0) then
          ix = m1 - incx
          iy = n1 - incy
          do m = 1, maxinc + 1
             ix = ix + incx
             iy = iy + incy
             nm = (iy + ddb)*icy + (ix + ddb)*icx - icxy
             do k = kmin, kmax
                aakl(nm, k, istof) = 0.0
                bbkl(nm, k, istof) = 1.0
                cckl(nm, k, istof) = 0.0
                ddkl(nm, k, istof) = ubnd(nrow, iustof, k, 1, ito)
             enddo
          enddo
       endif
    enddo
end subroutine usrbcc
