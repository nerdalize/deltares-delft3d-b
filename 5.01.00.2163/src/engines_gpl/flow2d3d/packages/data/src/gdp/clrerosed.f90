subroutine clrerosed(istat, gdp)
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
!  $Id: clrerosed.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/clrerosed.f90 $
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
! Global variables
!
    integer,intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdp%gderosed%bc_mor_array)) deallocate(gdp%gderosed%bc_mor_array, STAT = istat)
    if (associated(gdp%gderosed%dcwwlc))       deallocate(gdp%gderosed%dcwwlc      , STAT = istat)
    if (associated(gdp%gderosed%epsclc))       deallocate(gdp%gderosed%epsclc      , STAT = istat)
    if (associated(gdp%gderosed%epswlc))       deallocate(gdp%gderosed%epswlc      , STAT = istat)
    if (associated(gdp%gderosed%rsdqlc))       deallocate(gdp%gderosed%rsdqlc      , STAT = istat)
    if (associated(gdp%gderosed%sddflc))       deallocate(gdp%gderosed%sddflc      , STAT = istat)
    if (associated(gdp%gderosed%umod))         deallocate(gdp%gderosed%umod        , STAT = istat)
    if (associated(gdp%gderosed%ust2))         deallocate(gdp%gderosed%ust2        , STAT = istat)
    if (associated(gdp%gderosed%uuu))          deallocate(gdp%gderosed%uuu         , STAT = istat)
    if (associated(gdp%gderosed%vvv))          deallocate(gdp%gderosed%vvv         , STAT = istat)
    if (associated(gdp%gderosed%wslc))         deallocate(gdp%gderosed%wslc        , STAT = istat)
    if (associated(gdp%gderosed%zumod))        deallocate(gdp%gderosed%zumod       , STAT = istat)
    if (associated(gdp%gderosed%dbodsd))       deallocate(gdp%gderosed%dbodsd      , STAT = istat)
    if (associated(gdp%gderosed%fixfac))       deallocate(gdp%gderosed%fixfac      , STAT = istat)
    if (associated(gdp%gderosed%frac))         deallocate(gdp%gderosed%frac        , STAT = istat)
    if (associated(gdp%gderosed%mudfrac))      deallocate(gdp%gderosed%mudfrac     , STAT = istat)
    if (associated(gdp%gderosed%sandfrac))     deallocate(gdp%gderosed%sandfrac    , STAT = istat)
    if (associated(gdp%gderosed%dm))           deallocate(gdp%gderosed%dm          , STAT = istat)
    if (associated(gdp%gderosed%dg))           deallocate(gdp%gderosed%dg          , STAT = istat)
    if (associated(gdp%gderosed%dgsd))         deallocate(gdp%gderosed%dgsd        , STAT = istat)
    if (associated(gdp%gderosed%dxx))          deallocate(gdp%gderosed%dxx         , STAT = istat)
    if (associated(gdp%gderosed%hidexp))       deallocate(gdp%gderosed%hidexp      , STAT = istat)
    if (associated(gdp%gderosed%sbcu))         deallocate(gdp%gderosed%sbcu        , STAT = istat)
    if (associated(gdp%gderosed%sbcv))         deallocate(gdp%gderosed%sbcv        , STAT = istat)
    if (associated(gdp%gderosed%sbcuu))        deallocate(gdp%gderosed%sbcuu       , STAT = istat)
    if (associated(gdp%gderosed%sbcvv))        deallocate(gdp%gderosed%sbcvv       , STAT = istat)
    if (associated(gdp%gderosed%sbuuc))        deallocate(gdp%gderosed%sbuuc       , STAT = istat)
    if (associated(gdp%gderosed%sbvvc))        deallocate(gdp%gderosed%sbvvc       , STAT = istat)
    if (associated(gdp%gderosed%sbwu))         deallocate(gdp%gderosed%sbwu        , STAT = istat)
    if (associated(gdp%gderosed%sbwv))         deallocate(gdp%gderosed%sbwv        , STAT = istat)
    if (associated(gdp%gderosed%sbwuu))        deallocate(gdp%gderosed%sbwuu       , STAT = istat)
    if (associated(gdp%gderosed%sbwvv))        deallocate(gdp%gderosed%sbwvv       , STAT = istat)
    if (associated(gdp%gderosed%srcmax))       deallocate(gdp%gderosed%srcmax      , STAT = istat)
    if (associated(gdp%gderosed%ssuuc))        deallocate(gdp%gderosed%ssuuc       , STAT = istat)
    if (associated(gdp%gderosed%ssvvc))        deallocate(gdp%gderosed%ssvvc       , STAT = istat)
    if (associated(gdp%gderosed%sswu))         deallocate(gdp%gderosed%sswu        , STAT = istat)
    if (associated(gdp%gderosed%sswv))         deallocate(gdp%gderosed%sswv        , STAT = istat)
    if (associated(gdp%gderosed%sswuu))        deallocate(gdp%gderosed%sswuu       , STAT = istat)
    if (associated(gdp%gderosed%sswvv))        deallocate(gdp%gderosed%sswvv       , STAT = istat)
    if (associated(gdp%gderosed%sucor))        deallocate(gdp%gderosed%sucor       , STAT = istat)
    if (associated(gdp%gderosed%svcor))        deallocate(gdp%gderosed%svcor       , STAT = istat)
    if (associated(gdp%gderosed%sutot))        deallocate(gdp%gderosed%sutot       , STAT = istat)
    if (associated(gdp%gderosed%svtot))        deallocate(gdp%gderosed%svtot       , STAT = istat)
    if (associated(gdp%gderosed%dzduu))        deallocate(gdp%gderosed%dzduu       , STAT = istat)
    if (associated(gdp%gderosed%dzdvv))        deallocate(gdp%gderosed%dzdvv       , STAT = istat)
    if (associated(gdp%gderosed%taurat))       deallocate(gdp%gderosed%taurat      , STAT = istat)
end subroutine clrerosed
