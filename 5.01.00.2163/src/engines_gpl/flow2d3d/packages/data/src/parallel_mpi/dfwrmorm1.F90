subroutine dfwrmorm1(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
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
!  $Id: dfwrmorm1.F90 1977 2012-11-15 16:54:06Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfwrmorm1.F90 $
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
    use dfparall
    use globaldata
    use dffunctionals
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer              , pointer :: celidt
    type (nefiselement)  , pointer :: nefiselem
    integer              , pointer :: mfg
    integer              , pointer :: mlg
    integer              , pointer :: nfg
    integer              , pointer :: nlg
    integer              , pointer :: nmaxgl
    integer              , pointer :: mmaxgl
!
! Global variables
!
    integer                                                :: fds
    integer                                                :: irequest
    integer                                                :: lsedtot
    integer                                                :: lundia
    integer                                                :: mmax
    integer                                                :: nmaxus
    logical                                                :: error
    character(16)                                          :: grpnam
    real(prec), dimension(1:lsedtot,gdp%d%nmlb:gdp%d%nmub) :: bodsed
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)           :: dpsed
    type(bedcomp_data)                                     :: gdmorlyr
!
! Local variables
!
    integer                                 :: ierror       ! Local errorflag for NEFIS files
    integer                                 :: i
    integer                                 :: l
    integer                                 :: m
    integer                                 :: n
    integer                                 :: nm
    integer, external                       :: neferr
    integer, external                       :: putelt
    integer, dimension(3,5)                 :: uindex
    integer , dimension(4,0:nproc-1)        :: iarrc  ! array containing collected grid indices 
    integer                                 :: lenlo  ! length of field of current subdomain
    integer                                 :: lengl  ! length of field containing collected data
    integer , dimension(0:nproc-1)          :: mf     ! first index w.r.t. global grid in x-direction
    integer , dimension(0:nproc-1)          :: ml     ! last index w.r.t. global grid in x-direction
    integer                                 :: msiz   ! size of present subdomain in x-direction
    integer , dimension(0:nproc-1)          :: nf     ! first index w.r.t. global grid in y-direction
    integer , dimension(0:nproc-1)          :: nl     ! last index w.r.t. global grid in y-direction
    integer                                 :: nsiz   ! size of present subdomain in y-direction
    real(fp), dimension(:,:)  , allocatable :: rbuff2
    real(fp), dimension(:,:,:), allocatable :: rbuff3
    character(256)                          :: errmsg
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedm)
    celidt    => nefiselem%celidt
    mfg       => gdp%gdparall%mfg
    mlg       => gdp%gdparall%mlg
    nfg       => gdp%gdparall%nfg
    nlg       => gdp%gdparall%nlg
    mmaxgl    => gdp%gdparall%mmaxgl
    nmaxgl    => gdp%gdparall%nmaxgl
    !
    ierror = 0
    !
    select case (irequest)
    case (1)
       !
       ! Define elements
       !
       call addelm(nefiswrsedm,'BODSED',' ','[ KG/M2 ]','REAL',4, &
          & 'Available sediment at bed (zeta point)'            , &
          & 3      ,nmaxgl ,mmaxgl   ,lsedtot  ,0       ,0      , &
          & lundia ,gdp    )
       call addelm(nefiswrsedm,'DPSED',' ','[   M   ]','REAL',4 , &
          & 'Sediment thickness at bed (zeta point)'            , &
          & 2      ,nmaxgl ,mmaxgl   ,0        ,0       ,0      , &
          & lundia ,gdp    )
    case (2)
       !
       ! allocate data arrays for collection data 
       !
       ! gather LOCAL grid indices of all partitions
       !
       call dfsync(gdp)
       call dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
          &                 nf, nl, mf, ml, iarrc, lengl, lenlo, gdp )
       !
       ! broadcast LOCAL grid indices to ALL partitions
       ! so every partition knows the dimensions and positions
       ! of the other partitions in the global domain
       !
       call dfbroadc ( iarrc, 4*nproc, dfint, gdp )
       call dfbroadc ( nf, nproc, dfint, gdp )
       call dfbroadc ( nl, nproc, dfint, gdp )
       call dfbroadc ( mf, nproc, dfint, gdp )
       call dfbroadc ( ml, nproc, dfint, gdp )
       !
       ! Write data to file
       !
       uindex (1,1) = celidt
       uindex (2,1) = celidt
       uindex (3,1) = 1 ! increment in time
       !
       ! element 'BODSED'
       !
       allocate(rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot))
       rbuff3 = 0.0_fp
       do l = 1, lsedtot
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff3(n,m,l) = real(bodsed(l, nm),fp)
             enddo
          enddo
       enddo
       call dfgather(rbuff3,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff3)
       if (inode == master) then
          ierror = putelt(fds, grpnam, 'BODSED', uindex, 1, glbarr3)
       endif
       if (ierror/= 0) goto 9999
       !
       ! element 'DPSED'
       !
       allocate(rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
       rbuff2 = 0.0_fp
       do m = 1, mmax
          do n = 1, nmaxus
             call n_and_m_to_nm(n, m, nm, gdp)
             rbuff2(n,m) = dpsed(nm)
          enddo
       enddo
       call dfgather(rbuff2,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff2)
       if (inode == master) then
          ierror = putelt(fds, grpnam,'DPSED', uindex, 1, glbarr2)
       endif
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
end subroutine dfwrmorm1
