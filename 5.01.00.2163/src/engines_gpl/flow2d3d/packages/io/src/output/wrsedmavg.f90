subroutine wrsedmavg(lundia    ,error     ,trifil    ,nst       ,mmax      , &
                   & nmaxus    ,lsed      ,lsedtot   ,initi     ,gdp       )
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
!  $Id: wrsedmavg.f90 1677 2012-07-01 21:04:29Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrsedmavg.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for sediment (4 & 5)
!              to the NEFIS MAP-DAT file
!              SOutput is performed conform the times of the map
!              file and only in case lsed > 0.
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
    real(fp), dimension(:)               , pointer :: dm
    real(fp), dimension(:,:)             , pointer :: sbuuc
    real(fp), dimension(:,:)             , pointer :: sbvvc
    real(fp), dimension(:,:)             , pointer :: ssuuc
    real(fp), dimension(:,:)             , pointer :: ssvvc
    real(hp)                             , pointer :: morft
    real(hp)                             , pointer :: morft0
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    type (moroutputtype)                 , pointer :: moroutput
    logical                              , pointer :: first
    integer                              , pointer :: celidt
    type (nefiselement)                  , pointer :: nefiselem
    real(fp)      , dimension(:)         , pointer :: rhosol
    real(fp)      , dimension(:)         , pointer :: cdryb
!
! Global variables
!
    integer        , intent(in)  :: initi
    integer        , intent(in)  :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer        , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                      :: lundia  !  Description and declaration in inout.igs
    integer        , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer        , intent(in)  :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer        , intent(in)  :: nst
    logical        , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    character(60)  , intent(in)  :: trifil  !!  File name for FLOW NEFIS output
                                            !!  files (tri"h/m"-"casl""labl".dat/def)
!
! Local variables
!
    real(hp)                :: dmorft
    real(fp)                :: rhol
    integer                 :: ierror     ! Local errorflag for NEFIS files 
    integer                 :: fds
    integer                 :: i
    integer                 :: l          ! Help var. 
    integer                 :: m          ! Help var. 
    integer                 :: n          ! Help var. 
    integer                 :: nm         ! Help var. 
    integer, dimension(1)   :: idummy     ! Help array to read/write Nefis files 
    integer, dimension(3,5) :: uindex
    integer, external       :: getelt
    integer, external       :: putelt
    integer, external       :: inqmxi
    integer, external       :: clsnef
    integer, external       :: open_datdef
    integer, external       :: neferr
    character(10)           :: transpunit
    character(16)           :: grnam6
    character(16)           :: grnam7
    character(256)          :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(60)           :: filnam      ! Help var. for FLOW file name 
    character(1024)         :: error_string
!
! Data statements
!
    data grnam6/'map-infavg-serie'/
    data grnam7/'map-avg-series'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedmavginf)
    first               => nefiselem%first
    celidt              => nefiselem%celidt
    dm                  => gdp%gderosed%dm
    sbuuc               => gdp%gderosed%sbuuc
    sbvvc               => gdp%gderosed%sbvvc
    ssuuc               => gdp%gderosed%ssuuc
    ssvvc               => gdp%gderosed%ssvvc
    morft               => gdp%gdmorpar%morft
    morft0              => gdp%gdmorpar%morft0
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    moroutput           => gdp%gdmorpar%moroutput
    rhosol              => gdp%gdsedpar%rhosol
    cdryb               => gdp%gdsedpar%cdryb
    !
    !
    ! Initialize local variables
    !
    filnam = trifil(1:3) // 'm' // trifil(5:)
    errmsg = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! Writing of output at the end of the flow run
    ! initi = 4: add new cell
    ! else     : update last cell
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       select case(moroutput%transptype)
       case (0)
          transpunit = '[ KG/S/M]'
       case (1)
          transpunit = '[ M3/S/M]'
       case (2)
          transpunit = '[ M3/S/M]'
       end select
       !
       ! map-infavg-series
       !
       call addelm(nefiswrsedmavginf,'ITAVGS',' ','[   -   ]','INTEGER',4, &
          & 'timestep number (ITAVG*DT*TUNIT := time in sec from ITDATE)', &
          & 1         ,1         ,0         ,0         ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedmavginf,'MFTAVG',' ','[  DAYS ]','REAL',8   , &
          & 'morphological time (days since start of simulation)        ', &
          & 1         ,1         ,0         ,0         ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedmavginf,'MORAVG',' ','[   -   ]','REAL',4   , &
          & 'average MORFAC used during averaging period                ', &
          & 1         ,1         ,0         ,0         ,0         ,0     , &
          & lundia    ,gdp       )
       call defnewgrp(nefiswrsedmavginf ,filnam    ,grnam6   ,gdp)
       !
       ! map-avg-series
       !
       call addelm(nefiswrsedmavg,'SBUUA',' ',transpunit ,'REAL',4, &
          & 'Average bed-load transport u-direction (u point)'    , &
          & 3      ,nmaxus ,mmax    ,lsedtot ,0       ,0          , &
          & lundia ,gdp    )
       call addelm(nefiswrsedmavg,'SBVVA',' ',transpunit ,'REAL',4, &
          & 'Average bed-load transport v-direction (v point)'    , &
          & 3       ,nmaxus ,mmax    ,lsedtot  ,0        ,0       , &
          & lundia  ,gdp    )
       call addelm(nefiswrsedmavg,'SSUUA',' ',transpunit ,'REAL',4  , &
          & 'Average suspended-load transport u-direction (u point)', &
          & 3       ,nmaxus  ,mmax     ,lsed     ,0        ,0       , &
          & lundia  ,gdp     )
       call addelm(nefiswrsedmavg,'SSVVA',' ',transpunit ,'REAL',4  , &
          & 'Average suspended-load transport v-direction (v point)', &
          & 3      ,nmaxus  ,mmax     ,lsed     ,0        ,0        , &
          & lundia ,gdp     )
       call defnewgrp(nefiswrsedmavg ,filnam    ,grnam7   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrsedmavginf)
    first               => nefiselem%first
    celidt              => nefiselem%celidt
       first = .false.
    endif
    ierror = open_datdef(filnam   ,fds      )
    if (ierror/= 0) goto 9999
    ierror = inqmxi(fds, grnam6, celidt)
    if (initi==4 .or. moroutput%cumavg) then
       celidt = celidt + 1
    endif
    idummy(1)   = nst
    uindex(1,1) = celidt
    uindex(2,1) = celidt
    !
    ! group 6: element 'ITAVGS'
    !
    ierror = putelt(fds, grnam6, 'ITAVGS', uindex, 1, idummy)
    if (ierror/=0) goto 9999
    !
    ! group 6: element 'MFTAVG'
    !
    ierror = putelt(fds, grnam6, 'MFTAVG', uindex, 1, morft)
    if (ierror/=0) goto 9999
    !
    dmorft = morft - morft0
    !
    ! group 7: element 'SBUUA'
    !
    i = 0
    do l = 1, lsedtot
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             i        = i+1
             call n_and_m_to_nm(n, m, nm, gdp)             
             if ( dmorft > 0.0_hp ) then
                sbuff(i) = real(sbuuc(nm, l)/rhol/(real(dmorft*86400.0_hp,fp)),sp)
             else
                sbuff(i) =  0.0_sp
             endif
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam7, 'SBUUA', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! group 7: element 'SBVVA'
    !
    i = 0
    do l = 1, lsedtot
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             i        = i+1
             call n_and_m_to_nm(n, m, nm, gdp)
             if ( dmorft > 0.0_hp ) then
                sbuff(i) = real(sbvvc(nm, l)/rhol/(real(dmorft*86400.0_hp,fp)),sp)
             else
                sbuff(i) =  0.0_sp
             endif
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam7, 'SBVVA', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! group 7: element 'SSUUA'
    !
    i = 0
    do l = 1, lsed
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             i        = i+1
             call n_and_m_to_nm(n, m, nm, gdp)             
             if ( dmorft > 0.0_hp ) then
                sbuff(i) = real(ssuuc(nm, l)/rhol/(real(dmorft*86400.0_hp,fp)),sp)
             else
                sbuff(i) =  0.0_sp
             endif
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam7, 'SSUUA', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ! group 7: element 'SSVVA'
    !
    i = 0
    do l = 1, lsed
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             i        = i+1
             call n_and_m_to_nm(n, m, nm, gdp)
             if ( dmorft > 0.0_hp ) then
                sbuff(i) = real(ssvvc(nm, l)/rhol/(real(dmorft*86400.0_hp,fp)),sp)
             else
                sbuff(i) =  0.0_sp
             endif
          enddo
       enddo
    enddo
    ierror = putelt(fds, grnam7, 'SSVVA', uindex, 1, sbuff)
    if (ierror/= 0) goto 9999
    !
    ierror = clsnef(fds)
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
 9999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrsedmavg
