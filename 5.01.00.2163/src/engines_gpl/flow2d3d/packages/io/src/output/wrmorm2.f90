subroutine wrmorm2(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & nlyr      ,irequest  ,fds       ,grpnam    ,msed      , &
                 & thlyr     ,svfrac    ,iporos    ,cdryb     ,rhosol    , &
                 & gdp  )
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
!  $Id: wrmorm2.f90 1200 2012-01-21 11:28:05Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrmorm2.f90 $
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
    use sp_buffer
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer              , pointer :: celidt
    type (nefiselement)  , pointer :: nefiselem
!
! Global variables
!
    integer                                                                :: fds
    integer                                                                :: iporos
    integer                                                                :: irequest
    integer                                                                :: lsedtot
    integer                                                                :: lundia
    integer                                                                :: mmax
    integer                                                                :: nlyr
    integer                                                                :: nmaxus
    logical                                                                :: error
    character(16)                                                          :: grpnam
    real(fp)           , dimension(1:lsedtot)                              :: cdryb
    real(fp)           , dimension(1:lsedtot)                              :: rhosol
    real(fp)           , dimension(1:lsedtot,1:nlyr,gdp%d%nmlb:gdp%d%nmub) :: msed
    real(fp)           , dimension(1:nlyr,gdp%d%nmlb:gdp%d%nmub)           :: thlyr
    real(fp)           , dimension(1:nlyr,gdp%d%nmlb:gdp%d%nmub)           :: svfrac
    type(bedcomp_data)                                                     :: gdmorlyr
!
! Local variables
!
    integer                 :: ierror      ! Local errorflag for NEFIS files
    integer                 :: i
    integer                 :: k
    integer                 :: l
    integer                 :: m
    integer                 :: n
    integer                 :: nm
    integer, external       :: neferr
    integer, external       :: putelt
    integer, dimension(3,5) :: uindex
    real(fp)                :: dens
    character(256)          :: errmsg
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedm)
    celidt  => nefiselem%celidt
    !
    select case (irequest)
    case (1)
       !
       ! Define elements
       !
       call addelm(nefiswrsedm,'MSED',' ','[ KG/M2 ]','REAL',4, &
          & 'Mass of sediment in layer'               , &
          & 4      ,nmaxus ,mmax     ,nlyr     ,lsedtot  ,0      , &
          & lundia ,gdp    )
       call addelm(nefiswrsedm,'LYRFRAC',' ','[   -   ]','REAL',4, &
          & 'Volume fraction of sediment in layer'               , &
          & 4      ,nmaxus ,mmax     ,nlyr     ,lsedtot  ,0      , &
          & lundia ,gdp    )
       call addelm(nefiswrsedm,'THLYR',' ','[   M   ]','REAL',4, &
          & 'Thickness of sediment layer'                      , &
          & 3      ,nmaxus ,mmax     ,nlyr    ,0       ,0      , &
          & lundia ,gdp    )
       if (iporos>0) then
          call addelm(nefiswrsedm,'EPSPOR',' ','[   -   ]','REAL',4, &
             & 'Porosity coefficient'                              , &
             & 3      ,nmaxus ,mmax     ,nlyr    ,0       ,0       , &
             & lundia ,gdp    )
       endif
    case (2)
       !
       ! Write data to file
       !
       uindex (1,1) = celidt
       uindex (2,1) = celidt
       uindex (3,1) = 1 ! increment in time
       !
       ! element 'MSED'
       !
       call sbuff_checksize(lsedtot*nlyr*mmax*nmaxus)
       i = 0
       do l = 1, lsedtot
          do k = 1, nlyr
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(msed(l, k, nm),sp)
                enddo
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'MSED', uindex, 1, sbuff)
       if (ierror /= 0) goto 9999
       !
       ! element 'LYRFRAC'
       !
       call sbuff_checksize(lsedtot*nlyr*mmax*nmaxus)
       i = 0
       do k = 1, nlyr
          do m = 1, mmax
             do n = 1, nmaxus
                do l = 1, lsedtot
                   i        = (l-1)*nlyr*mmax*nmaxus + (k-1)*mmax*nmaxus + (m-1)*nmaxus + n
                   call n_and_m_to_nm(n, m, nm, gdp)
                   if (iporos==0) then
                      dens = cdryb(l)
                   else
                      dens = rhosol(l)
                   endif
                   if (thlyr(k,nm)>0.0_fp) then
                        sbuff(i) = real(msed(l, k, nm)/(dens*svfrac(k, nm)*thlyr(k, nm)),sp)
                   else
                        sbuff(i) = 0.0
                   endif
                enddo
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'LYRFRAC', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'THLYR'
       !
       i = 0
       do k = 1, nlyr
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(thlyr(k, nm),sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'THLYR', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'EPSPOR'
       !
       if (iporos>0) then
          i = 0
          do k = 1, nlyr
             do m = 1, mmax
                do n = 1, nmaxus
                   i        = i+1
                   call n_and_m_to_nm(n, m, nm, gdp)
                   sbuff(i) = real(1.0_fp - svfrac(k, nm),sp)
                enddo
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'EPSPOR', uindex, 1, sbuff)
          if (ierror /= 0) goto 9999
       endif
       !
       ! write error message if error occurred and set error = .true.
       ! the files will be closed in clsnef (called in triend)
       !
 9999  continue
       if (ierror/= 0) then
          ierror = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error = .true.
       endif
    endselect
end subroutine wrmorm2
