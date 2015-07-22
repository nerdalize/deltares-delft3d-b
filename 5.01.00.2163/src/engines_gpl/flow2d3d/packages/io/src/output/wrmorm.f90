subroutine wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                & irequest  ,fds       ,grpnam    ,gdp       )
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
!  $Id: wrmorm.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrmorm.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for the morphological under layers
!              to the sediment group on the NEFIS FLOW MAP file
!
! Method used:
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    type(bedcomp_data) :: gdmorlyr
!
! Global variables
!
    integer       :: fds
    integer       :: lsedtot
    integer       :: lundia
    integer       :: mmax
    integer       :: nmaxus
    integer       :: irequest
    logical       :: error
    character(16) :: grpnam
!
! Local variables
!
    integer                                       :: istat
    integer                             , pointer :: iporos
    integer                             , pointer :: iunderlyr
    integer                             , pointer :: nlyr
    real(prec)       , dimension(:,:)   , pointer :: bodsed
    real(fp)         , dimension(:)     , pointer :: cdryb
    real(fp)         , dimension(:)     , pointer :: rhosol
    real(fp)         , dimension(:)     , pointer :: dpsed
    real(fp)         , dimension(:,:)   , pointer :: svfrac
    real(fp)         , dimension(:,:,:) , pointer :: msed
    real(fp)         , dimension(:,:)   , pointer :: thlyr
!
!! executable statements -------------------------------------------------------
!
    cdryb               => gdp%gdsedpar%cdryb
    rhosol              => gdp%gdsedpar%rhosol
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr,'iunderlyr',iunderlyr)
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Memory problem in WRMORM')
       call d3stop(1, gdp)
    endif
    !
    select case (iunderlyr)
    case (1)
       istat = bedcomp_getpointer_realprec(gdp%gdmorlyr,'bodsed',bodsed)
       if (istat==0) istat = bedcomp_getpointer_realfp(gdp%gdmorlyr,'dpsed',dpsed)
       if (istat/=0) then
          call prterr(lundia, 'U021', 'Memory problem in WRMORM')
          call d3stop(1, gdp)
       endif
       if (.not. parll) then
          call wrmorm1 (lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                      & irequest  ,fds       ,grpnam    ,bodsed    ,dpsed     , &
                      & gdp       )
       else
          call dfwrmorm1 (lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                        & irequest  ,fds       ,grpnam    ,bodsed    ,dpsed     , &
                        & gdp       )
       endif
    case (2)
       istat = bedcomp_getpointer_integer(gdp%gdmorlyr,'nlyr',nlyr)
       if (istat==0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr,'iporosity',iporos)
       if (istat==0) istat = bedcomp_getpointer_realfp(gdp%gdmorlyr,'svfrac',svfrac)
       if (istat==0) istat = bedcomp_getpointer_realfp(gdp%gdmorlyr,'msed',msed)
       if (istat==0) istat = bedcomp_getpointer_realfp(gdp%gdmorlyr,'thlyr',thlyr)
       if (istat/=0) then
          call prterr(lundia, 'U021', 'Memory problem in WRMORM')
          call d3stop(1, gdp)
       endif
       if (.not. parll) then
          call wrmorm2 (lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                      & nlyr      ,irequest  ,fds       ,grpnam    ,msed      , &
                      & thlyr     ,svfrac    ,iporos    ,cdryb     ,rhosol    , &
                      & gdp       )
       else
          call dfwrmorm2 (lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                        & nlyr      ,irequest  ,fds       ,grpnam    ,msed      , &
                        & thlyr     ,svfrac    ,iporos    ,cdryb     ,rhosol    , &
                        & gdp       )
       endif
    case default
    end select
end subroutine wrmorm
