subroutine clrdredge(istat, gdp)
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
!  $Id: clrdredge.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/clrdredge.f90 $
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
    if (associated(gdp%gddredge%link_percentage)) deallocate (gdp%gddredge%link_percentage, STAT = istat)
    if (associated(gdp%gddredge%link_distance))   deallocate (gdp%gddredge%link_distance  , STAT = istat)
    if (associated(gdp%gddredge%link_sum))        deallocate (gdp%gddredge%link_sum       , STAT = istat)
    if (associated(gdp%gddredge%dzdred))          deallocate (gdp%gddredge%dzdred         , STAT = istat)
    if (associated(gdp%gddredge%refplane))        deallocate (gdp%gddredge%refplane       , STAT = istat)
    if (associated(gdp%gddredge%voldred))         deallocate (gdp%gddredge%voldred        , STAT = istat)
    if (associated(gdp%gddredge%totvoldred))      deallocate (gdp%gddredge%totvoldred     , STAT = istat)
    if (associated(gdp%gddredge%globalareadred))  deallocate (gdp%gddredge%globalareadred , STAT = istat)
    if (associated(gdp%gddredge%voldune))         deallocate (gdp%gddredge%voldune        , STAT = istat)
    if (associated(gdp%gddredge%percsupl))        deallocate (gdp%gddredge%percsupl       , STAT = istat)
    if (associated(gdp%gddredge%totvoldump))      deallocate (gdp%gddredge%totvoldump     , STAT = istat)
    if (associated(gdp%gddredge%localareadump))   deallocate (gdp%gddredge%localareadump  , STAT = istat)
    if (associated(gdp%gddredge%globalareadump))  deallocate (gdp%gddredge%globalareadump , STAT = istat)
    if (associated(gdp%gddredge%globaldumpcap))   deallocate (gdp%gddredge%globaldumpcap  , STAT = istat)
    if (associated(gdp%gddredge%voldump))         deallocate (gdp%gddredge%voldump        , STAT = istat)
    !
    if (associated(gdp%gddredge%link_def))        deallocate (gdp%gddredge%link_def       , STAT = istat)
    !
    if (associated(gdp%gddredge%dredge_areas))    deallocate (gdp%gddredge%dredge_areas   , STAT = istat)
    if (associated(gdp%gddredge%dump_areas))      deallocate (gdp%gddredge%dump_areas     , STAT = istat)
    !
    if (associated(gdp%gddredge%dredge_prop)) then
       do i = 1, gdp%gddredge%nadred
          if (associated(gdp%gddredge%dredge_prop(i)%nm))             deallocate (gdp%gddredge%dredge_prop(i)%nm                  , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%inm))            deallocate (gdp%gddredge%dredge_prop(i)%inm                 , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%area))           deallocate (gdp%gddredge%dredge_prop(i)%area                , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%hdune))          deallocate (gdp%gddredge%dredge_prop(i)%hdune               , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%dz_dredge))      deallocate (gdp%gddredge%dredge_prop(i)%dz_dredge           , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%dunetoplevel))   deallocate (gdp%gddredge%dredge_prop(i)%dunetoplevel        , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%triggerlevel))   deallocate (gdp%gddredge%dredge_prop(i)%triggerlevel        , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%bedlevel))       deallocate (gdp%gddredge%dredge_prop(i)%bedlevel            , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%troughlevel))    deallocate (gdp%gddredge%dredge_prop(i)%troughlevel         , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%sedimentdepth))  deallocate (gdp%gddredge%dredge_prop(i)%sedimentdepth       , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%sortvar))        deallocate (gdp%gddredge%dredge_prop(i)%sortvar             , STAT = istat)
          if (associated(gdp%gddredge%dredge_prop(i)%triggered))      deallocate (gdp%gddredge%dredge_prop(i)%triggered           , STAT = istat)
       enddo
       deallocate (gdp%gddredge%dredge_prop    , STAT = istat)
    endif
    !
    if (associated(gdp%gddredge%dump_prop)) then
       do i = 1, gdp%gddredge%nadump
          if (associated(gdp%gddredge%dump_prop(i)%nm))               deallocate (gdp%gddredge%dump_prop(i)%nm                    , STAT = istat)
          if (associated(gdp%gddredge%dump_prop(i)%inm))              deallocate (gdp%gddredge%dump_prop(i)%inm                   , STAT = istat)
          if (associated(gdp%gddredge%dump_prop(i)%area))             deallocate (gdp%gddredge%dump_prop(i)%area                  , STAT = istat)
          if (associated(gdp%gddredge%dump_prop(i)%hdune))            deallocate (gdp%gddredge%dump_prop(i)%hdune                 , STAT = istat)
          if (associated(gdp%gddredge%dump_prop(i)%bedlevel))         deallocate (gdp%gddredge%dump_prop(i)%bedlevel              , STAT = istat)
          if (associated(gdp%gddredge%dump_prop(i)%dz_dump))          deallocate (gdp%gddredge%dump_prop(i)%dz_dump               , STAT = istat)
          if (associated(gdp%gddredge%dump_prop(i)%sortvar))          deallocate (gdp%gddredge%dump_prop(i)%sortvar               , STAT = istat)
       enddo
       deallocate (gdp%gddredge%dump_prop      , STAT = istat)
    endif
    !
    call cleartable(gdp%gddredge%tseriesfile)
end subroutine clrdredge
