subroutine soursin_2d(umod      ,ustarc    ,h0        ,h1        , &
                    & ws        ,tsd       ,rsedeq    , &
                    & sour      ,sink      ,gdp                  )
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
!  $Id: soursin_2d.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/soursin_2d.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes the sour and sink terms for the 2D case
!              (Gallappatti aproach)
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
    real(fp) , pointer :: eps
!
! Global variables
!
    real(fp), intent(in)  :: umod
    real(fp), intent(in)  :: ustarc
    real(fp), intent(in)  :: h0
    real(fp), intent(in)  :: h1
    real(fp), intent(in)  :: ws
    real(fp)              :: tsd
    real(fp), intent(in)  :: rsedeq
    real(fp), intent(out) :: sour
    real(fp), intent(out) :: sink

!
! Local variables
!
    real(fp) :: b
    real(fp) :: hots
    real(fp) :: u
    real(fp) :: ulog
    real(fp) :: w
    real(fp) :: wsl
    real(fp) :: x
    real(fp) :: x2
    real(fp) :: x3
!
!! executable statements -------------------------------------------------------
!
    eps       => gdp%gdconst%eps
    !
    wsl = max(1.0e-3_fp,ws)
    if (umod > eps .and. ustarc > eps) then
       if (tsd <= 0.0) then
          !
          ! tsd not given by user transport formula
          ! compute it using the Gallappatti formulations
          !
          u = ustarc/umod
          !
          ! limit u to prevent overflow in tsd below
          !
          u = min(u, 0.15_fp)
          if (ustarc > wsl) then
             w = wsl/ustarc
          else
             w = 1.0
          endif
          b    = 1.0
          x    = w/b
          x2   = x*x
          x3   = x2*x
          ulog = log(u)
          tsd  = x*exp((  1.547           - 20.12*u )*x3 &
               &     + (326.832 *u**2.2047 -  0.2   )*x2 &
               &     + (  0.1385*ulog      -  6.4061)*x  &
               &     + (  0.5467*u         +  2.1963) )
       else
          !
          ! tsd given by user transport formula
          !
       endif
       hots = wsl/tsd
       sour = rsedeq*hots/h0
       sink = hots/h1
    else
       sink = wsl/h1
    endif
end subroutine soursin_2d

