subroutine clrmorpar(istat, gdp)
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
!  $Id: clrmorpar.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/clrmorpar.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow_tables
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
! Local variables
!
    integer :: i
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdp%gdmorpar%morbnd)) then
       do i = 1, size(gdp%gdmorpar%morbnd)
          if (associated(gdp%gdmorpar%morbnd(i)%idir))      deallocate(gdp%gdmorpar%morbnd(i)%idir,      STAT = istat)
          if (associated(gdp%gdmorpar%morbnd(i)%nm))        deallocate(gdp%gdmorpar%morbnd(i)%nm,        STAT = istat)
          if (associated(gdp%gdmorpar%morbnd(i)%nxmx))      deallocate(gdp%gdmorpar%morbnd(i)%nxmx,      STAT = istat)
          if (associated(gdp%gdmorpar%morbnd(i)%alfa_dist)) deallocate(gdp%gdmorpar%morbnd(i)%alfa_dist, STAT = istat)
          if (associated(gdp%gdmorpar%morbnd(i)%alfa_mag))  deallocate(gdp%gdmorpar%morbnd(i)%alfa_mag,  STAT = istat)
       enddo
       deallocate(gdp%gdmorpar%morbnd, STAT = istat)
    endif
    if (associated(gdp%gdmorpar%cmpbnd))    deallocate(gdp%gdmorpar%cmpbnd,    STAT = istat)
    if (associated(gdp%gdmorpar%xx))        deallocate(gdp%gdmorpar%xx,        STAT = istat)
    if (associated(gdp%gdmorpar%mergebuf))  deallocate(gdp%gdmorpar%mergebuf,  STAT = istat)
    if (associated(gdp%gdmorpar%moroutput)) deallocate(gdp%gdmorpar%moroutput, STAT = istat)
    if (associated(gdp%gdmorpar%mornum))    deallocate(gdp%gdmorpar%mornum,    STAT = istat)
    call cleartable(gdp%gdmorpar%bcmfile)
    call cleartable(gdp%gdmorpar%morfacfile)
end subroutine clrmorpar
