subroutine wrmorm1(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & irequest  ,fds       ,grpnam    ,bodsed    ,dpsed     , &
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
!  $Id: wrmorm1.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrmorm1.f90 $
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
    integer                                                         :: fds
    integer                                                         :: irequest
    integer                                                         :: lsedtot
    integer                                                         :: lundia
    integer                                                         :: mmax
    integer                                                         :: nmaxus
    logical                                                         :: error
    character(16)                                                   :: grpnam
    real(prec)         , dimension(1:lsedtot,gdp%d%nmlb:gdp%d%nmub) :: bodsed
    real(fp)           , dimension(gdp%d%nmlb:gdp%d%nmub)           :: dpsed
    type(bedcomp_data)                                              :: gdmorlyr
!
! Local variables
!
    integer                 :: ierror    ! Local errorflag for NEFIS files
    integer                 :: i
    integer                 :: l
    integer                 :: m
    integer                 :: n
    integer                 :: nm
    integer, external       :: neferr
    integer, external       :: putelt
    integer, dimension(3,5) :: uindex
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
       call addelm(nefiswrsedm,'BODSED',' ','[ KG/M2 ]','REAL',4, &
          & 'Available sediment at bed (zeta point)'            , &
          & 3      ,nmaxus ,mmax     ,lsedtot  ,0       ,0      , &
          & lundia ,gdp    )
       call addelm(nefiswrsedm,'DPSED',' ','[   M   ]','REAL',4 , &
          & 'Sediment thickness at bed (zeta point)'            , &
          & 2      ,nmaxus ,mmax     ,0        ,0       ,0      , &
          & lundia ,gdp    )
    case (2)
       !
       ! Write data to file
       !
       uindex (1,1) = celidt
       uindex (2,1) = celidt
       uindex (3,1) = 1 ! increment in time
       !
       ! element 'BODSED'
       !
       call sbuff_checksize(lsedtot*mmax*nmaxus)
       i = 0
       do l = 1, lsedtot
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(bodsed(l, nm),sp)
             enddo
          enddo
       enddo
       ierror = putelt(fds, grpnam, 'BODSED', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! element 'DPSED'
       !
       call sbuff_checksize(mmax*nmaxus)
       i = 0
       do m = 1, mmax
          do n = 1, nmaxus
             i        = i+1
             call n_and_m_to_nm(n, m, nm, gdp)
             sbuff(i) = real(dpsed(nm),sp)
          enddo
       enddo
       ierror = putelt(fds, grpnam,'DPSED', uindex, 1, sbuff)
       if (ierror/= 0) goto 9999
       !
       ! write errormessage if error occurred and set error = .true.
       ! the files will be closed in clsnef (called in triend)
       !
 9999  continue
       if (ierror/= 0) then
          ierror = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error = .true.
       endif
    endselect
end subroutine wrmorm1
