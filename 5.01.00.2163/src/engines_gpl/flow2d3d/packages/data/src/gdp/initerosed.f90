subroutine initerosed(gdp)
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
!  $Id: initerosed.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/initerosed.f90 $
!!--description-----------------------------------------------------------------
! NONE
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
!! executable statements -------------------------------------------------------
!
    gdp%gderosed%ifirst = 1
    !
    nullify(gdp%gderosed%bc_mor_array)
    !
    nullify(gdp%gderosed%dbodsd)
    nullify(gdp%gderosed%dcwwlc)
    nullify(gdp%gderosed%dg)
    nullify(gdp%gderosed%dgsd)
    nullify(gdp%gderosed%dm)
    nullify(gdp%gderosed%dxx)
    nullify(gdp%gderosed%dzduu)
    nullify(gdp%gderosed%dzdvv)
    !
    nullify(gdp%gderosed%epsclc)
    nullify(gdp%gderosed%epswlc)
    !
    nullify(gdp%gderosed%fixfac)
    nullify(gdp%gderosed%srcmax)
    nullify(gdp%gderosed%frac)
    !
    nullify(gdp%gderosed%hidexp)
    !
    nullify(gdp%gderosed%mudfrac)
    nullify(gdp%gderosed%sandfrac)
    !
    nullify(gdp%gderosed%rsdqlc)
    !
    nullify(gdp%gderosed%sbcu)
    nullify(gdp%gderosed%sbcuu)
    nullify(gdp%gderosed%sbcv)
    nullify(gdp%gderosed%sbcvv)
    nullify(gdp%gderosed%sbuuc)
    nullify(gdp%gderosed%sbvvc)
    nullify(gdp%gderosed%sbwu)
    nullify(gdp%gderosed%sbwuu)
    nullify(gdp%gderosed%sbwv)
    nullify(gdp%gderosed%sbwvv)
    nullify(gdp%gderosed%sddflc)
    nullify(gdp%gderosed%sinkse)
    nullify(gdp%gderosed%sourse)
    nullify(gdp%gderosed%ssuuc)
    nullify(gdp%gderosed%ssvvc)
    nullify(gdp%gderosed%sswu)
    nullify(gdp%gderosed%sswuu)
    nullify(gdp%gderosed%sswv)
    nullify(gdp%gderosed%sswvv)
    nullify(gdp%gderosed%sucor)
    nullify(gdp%gderosed%sutot)
    nullify(gdp%gderosed%svcor)
    nullify(gdp%gderosed%svtot)
    !
    nullify(gdp%gderosed%taurat)
    !
    nullify(gdp%gderosed%umod)
    nullify(gdp%gderosed%ust2)
    nullify(gdp%gderosed%uuu)
    !
    nullify(gdp%gderosed%vvv)
    !
    nullify(gdp%gderosed%wslc)
    !
    nullify(gdp%gderosed%zumod)
    !
end subroutine initerosed
