subroutine wrsedd(lundia    ,error     ,mmax      ,nmaxus    ,irequest  , &
                & fds       ,grpnam    ,gdp       )
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
!  $Id: wrsedd.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrsedd.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for the bedforms to the sediment
!              group on the NEFIS FLOW MAP file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: celidt
    type (nefiselement)                  , pointer :: nefiselem
    real(fp) , dimension(:)              , pointer :: duneheight
    real(fp) , dimension(:)              , pointer :: dunelength
    real(fp) , dimension(:)              , pointer :: rksr
    real(fp) , dimension(:)              , pointer :: rksmr
    real(fp) , dimension(:)              , pointer :: rksd
    logical                              , pointer :: lfbedfrm
    logical                              , pointer :: lfbedfrmrou
!
! Global variables
!
    integer        , intent(in)  :: irequest !! Action flag: 1 = define, 2 = write
    character(16)  , intent(in)  :: grpnam   !!  Group name
    integer                      :: lundia   !  Description and declaration in inout.igs
    integer                      :: mmax     !  Description and declaration in esm_alloc_int.f90
    integer                      :: nmaxus   !  Description and declaration in esm_alloc_int.f90
    logical        , intent(out) :: error    !!  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer                 :: ierror     ! Local errorflag for NEFIS files 
    integer                 :: fds
    integer                 :: i
    integer                 :: m          ! Help var. 
    integer                 :: n          ! Help var. 
    integer                 :: nm         ! Help var.
    integer, dimension(3,5) :: uindex
    integer, external       :: putelt
    integer, external       :: neferr
    character(256)          :: errmsg     ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
!! executable statements -------------------------------------------------------
!
    nefiselem   => gdp%nefisio%nefiselem(nefiswrsedminf)
    celidt      => nefiselem%celidt
    duneheight  => gdp%gdbedformpar%duneheight
    dunelength  => gdp%gdbedformpar%dunelength
    rksr        => gdp%gdbedformpar%rksr
    rksmr       => gdp%gdbedformpar%rksmr
    rksd        => gdp%gdbedformpar%rksd
    lfbedfrm    => gdp%gdbedformpar%lfbedfrm
    lfbedfrmrou => gdp%gdbedformpar%lfbedfrmrou
    !
    ierror = 0
    select case (irequest)
    case (1)
       !
       ! Define elements
       !
       if (lfbedfrm) then
          call addelm(nefiswrsedm,'DUNEHEIGHT',' ','[   M   ]','REAL',4     , &
             & 'Dune height (zeta point)                                    ', &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'DUNELENGTH',' ','[   M   ]','REAL',4     , &
             & 'Dune length (zeta point)                                    ', &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (lfbedfrmrou) then
          call addelm(nefiswrsedm,'KSR',' ','[   M   ]','REAL',4            , &
             & 'Ripple roughness height'                                 , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'KSMR',' ','[   M   ]','REAL',4           , &
             & 'Mega-ripple roughness height'                            , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'KSD',' ','[   M   ]','REAL',4            , &
             & 'Dune roughness height'                                   , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'KS',' ','[   M   ]','REAL',4             , &
             & 'Combined bedform roughness height'                       , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
    case (2)
       !
       ! Write data to file
       !
       uindex (1,1) = celidt
       uindex (2,1) = celidt
       uindex (3,1) = 1 ! increment in time
       !
       if (lfbedfrm) then
          !
          ! element 'DUNEHEIGHT'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                 i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(duneheight(nm) , sp)
             enddo
          enddo
          !
          ierror = putelt(fds, grpnam, 'DUNEHEIGHT', uindex, 1, sbuff)
          if (ierror/= 0) goto 9999
          !
          ! element 'DUNELENGTH'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(dunelength(nm) , sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'DUNELENGTH', uindex, 1, sbuff)
          !
          if (ierror/= 0) goto 9999
       endif
       !
       if (lfbedfrmrou) then
          !
          ! element 'KSR'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(rksr(nm), sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'KSR', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'KSMR'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(rksmr(nm), sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'KSMR', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'KSD'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(rksd(nm), sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'KSD', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
          !
          ! element 'KS'
          !
          call sbuff_checksize(mmax*nmaxus)
          i = 0
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                call n_and_m_to_nm(n, m, nm, gdp)
                sbuff(i) = real(sqrt(rksr(nm)**2 + rksmr(nm)**2 + rksd(nm)**2),sp)
             enddo
          enddo
          ierror = putelt(fds, grpnam, 'KS', uindex, 1, sbuff)
          if (ierror/=0) goto 9999
       endif
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
end subroutine wrsedd
