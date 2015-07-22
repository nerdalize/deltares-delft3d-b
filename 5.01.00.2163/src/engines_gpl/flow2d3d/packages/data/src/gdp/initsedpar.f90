subroutine initsedpar(gdp)
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
!  $Id: initsedpar.f90 1848 2012-09-14 17:42:05Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/initsedpar.f90 $
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
    include 'sedparams.inc'
!
!! executable statements -------------------------------------------------------
!
    gdp%gdsedpar%mdcuni   = 0.0
    gdp%gdsedpar%nmudfrac = 0
    !
    nullify(gdp%gdsedpar%rhosol)
    !
    nullify(gdp%gdsedpar%logseddia)
    nullify(gdp%gdsedpar%logsedsig)
    nullify(gdp%gdsedpar%sedd10)
    nullify(gdp%gdsedpar%sedd50)
    nullify(gdp%gdsedpar%sedd50fld)
    nullify(gdp%gdsedpar%sedd90)
    !
    nullify(gdp%gdsedpar%cdryb)
    nullify(gdp%gdsedpar%dstar)
    nullify(gdp%gdsedpar%taucr)
    nullify(gdp%gdsedpar%tetacr)
    nullify(gdp%gdsedpar%gamtcr)
    nullify(gdp%gdsedpar%gamflc)
    nullify(gdp%gdsedpar%ws0)
    nullify(gdp%gdsedpar%wsm)
    nullify(gdp%gdsedpar%salmax)
    nullify(gdp%gdsedpar%sdbuni)
    nullify(gdp%gdsedpar%tcrdep)
    nullify(gdp%gdsedpar%tcduni)
    nullify(gdp%gdsedpar%tcrero)
    nullify(gdp%gdsedpar%tceuni)
    nullify(gdp%gdsedpar%tcguni)
    nullify(gdp%gdsedpar%eropar)
    nullify(gdp%gdsedpar%erouni)
    nullify(gdp%gdsedpar%mudcnt)
    nullify(gdp%gdsedpar%sedtrcfac)
    !
    nullify(gdp%gdsedpar%nseddia)
    nullify(gdp%gdsedpar%sedtyp)
    !
    nullify(gdp%gdsedpar%inisedunit)
    nullify(gdp%gdsedpar%namsed)
    nullify(gdp%gdsedpar%flsdbd)
    nullify(gdp%gdsedpar%flstcd)
    nullify(gdp%gdsedpar%flstce)
    nullify(gdp%gdsedpar%flsero)
    nullify(gdp%gdsedpar%flstcg)
    !
    gdp%gdsedpar%flsdia = ' '
    gdp%gdsedpar%flsmdc = ' '
end subroutine initsedpar
