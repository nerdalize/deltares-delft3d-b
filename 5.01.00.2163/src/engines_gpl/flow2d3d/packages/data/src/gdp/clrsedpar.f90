subroutine clrsedpar(istat, gdp)
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
!  $Id: clrsedpar.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/clrsedpar.f90 $
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
    if (associated(gdp%gdsedpar%rhosol))     deallocate(gdp%gdsedpar%rhosol,     STAT = istat)
    !
    if (associated(gdp%gdsedpar%logseddia))  deallocate(gdp%gdsedpar%logseddia,  STAT = istat)
    if (associated(gdp%gdsedpar%logsedsig))  deallocate(gdp%gdsedpar%logsedsig,  STAT = istat)
    if (associated(gdp%gdsedpar%sedd10))     deallocate(gdp%gdsedpar%sedd10,     STAT = istat)
    if (associated(gdp%gdsedpar%sedd50))     deallocate(gdp%gdsedpar%sedd50,     STAT = istat)
    if (associated(gdp%gdsedpar%sedd50fld))  deallocate(gdp%gdsedpar%sedd50fld,  STAT = istat)
    if (associated(gdp%gdsedpar%sedd90))     deallocate(gdp%gdsedpar%sedd90,     STAT = istat)
    !
    if (associated(gdp%gdsedpar%cdryb))      deallocate(gdp%gdsedpar%cdryb,      STAT = istat)
    if (associated(gdp%gdsedpar%dstar))      deallocate(gdp%gdsedpar%dstar,      STAT = istat)
    if (associated(gdp%gdsedpar%taucr))      deallocate(gdp%gdsedpar%taucr,      STAT = istat)
    if (associated(gdp%gdsedpar%tetacr))     deallocate(gdp%gdsedpar%tetacr,     STAT = istat)
    if (associated(gdp%gdsedpar%gamtcr))     deallocate(gdp%gdsedpar%gamtcr,     STAT = istat)
    if (associated(gdp%gdsedpar%gamflc))     deallocate(gdp%gdsedpar%gamflc,     STAT = istat)
    if (associated(gdp%gdsedpar%ws0))        deallocate(gdp%gdsedpar%ws0,        STAT = istat)
    if (associated(gdp%gdsedpar%wsm))        deallocate(gdp%gdsedpar%wsm,        STAT = istat)
    if (associated(gdp%gdsedpar%salmax))     deallocate(gdp%gdsedpar%salmax,     STAT = istat)
    if (associated(gdp%gdsedpar%sdbuni))     deallocate(gdp%gdsedpar%sdbuni,     STAT = istat)
    if (associated(gdp%gdsedpar%tcrdep))     deallocate(gdp%gdsedpar%tcrdep,     STAT = istat)
    if (associated(gdp%gdsedpar%tcduni))     deallocate(gdp%gdsedpar%tcduni,     STAT = istat)
    if (associated(gdp%gdsedpar%tcrero))     deallocate(gdp%gdsedpar%tcrero,     STAT = istat)
    if (associated(gdp%gdsedpar%tceuni))     deallocate(gdp%gdsedpar%tceuni,     STAT = istat)
    if (associated(gdp%gdsedpar%tcguni))     deallocate(gdp%gdsedpar%tcguni,     STAT = istat)
    if (associated(gdp%gdsedpar%eropar))     deallocate(gdp%gdsedpar%eropar,     STAT = istat)
    if (associated(gdp%gdsedpar%erouni))     deallocate(gdp%gdsedpar%erouni,     STAT = istat)
    if (associated(gdp%gdsedpar%mudcnt))     deallocate(gdp%gdsedpar%mudcnt,     STAT = istat)
    !
    if (associated(gdp%gdsedpar%nseddia))    deallocate(gdp%gdsedpar%nseddia,    STAT = istat)
    if (associated(gdp%gdsedpar%sedtyp))     deallocate(gdp%gdsedpar%sedtyp,     STAT = istat)
    !
    if (associated(gdp%gdsedpar%inisedunit)) deallocate(gdp%gdsedpar%inisedunit, STAT = istat)
    if (associated(gdp%gdsedpar%namsed))     deallocate(gdp%gdsedpar%namsed,     STAT = istat)
    if (associated(gdp%gdsedpar%flsdbd))     deallocate(gdp%gdsedpar%flsdbd,     STAT = istat)
    if (associated(gdp%gdsedpar%flstcd))     deallocate(gdp%gdsedpar%flstcd,     STAT = istat)
    if (associated(gdp%gdsedpar%flstce))     deallocate(gdp%gdsedpar%flstce,     STAT = istat)
    if (associated(gdp%gdsedpar%flsero))     deallocate(gdp%gdsedpar%flsero,     STAT = istat)
    if (associated(gdp%gdsedpar%flstcg))     deallocate(gdp%gdsedpar%flstcg,     STAT = istat)
end subroutine clrsedpar
