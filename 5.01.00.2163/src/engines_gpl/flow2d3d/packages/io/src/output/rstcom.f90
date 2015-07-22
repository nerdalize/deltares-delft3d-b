subroutine rstcom(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                & kmax      ,nmaxus    ,lstsci    ,lsecfl    ,lsec      , &
                & timrst    ,itlen     ,timcur    ,maxtim    ,s1        , &
                & u1        ,v1        ,r1        ,rbuff     ,gdp       )
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
!  $Id: rstcom.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/rstcom.f90 $
!!--description-----------------------------------------------------------------
!
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                  , pointer :: first
    integer                  , pointer :: celidt
    integer, dimension(:, :) , pointer :: elmdms
    type (nefiselement)      , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 8
!
! Global variables
!
    integer, intent(in)                                                         :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                         :: lsec   !  Description and declaration in dimens.igs
    integer, intent(in)                                                         :: lsecfl !  Description and declaration in dimens.igs
    integer, intent(in)                                                         :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: lundia !  Description and declaration in inout.igs
    integer, intent(in)                                                         :: maxtim !!  Max.nr. of timesteps for the communi-
                                                                                          !!  cation file
    integer                                                                     :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)                                                         :: timrst !!  Restart time in combination with
                                                                                          !!  restart option from comm. file
    integer, dimension(maxtim)                                                  :: timcur !!  Array with time steps on comm. file
                                                                                          !!  for restart option
    logical                                                                     :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmaxus, mmax, kmax, 2)                                  :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                                :: comfil !!  Name for communication file
                                                                                          !!  com-<case><label>
!
! Local variables
!
    integer                         :: i      ! Hulp var. 
    integer                         :: ierr
    integer                         :: kmaxk
    integer                         :: nelmx2
    integer                         :: nhulp  ! Hulp var. 
    integer                         :: nready ! Flag for determination of inter- polation coefficient 
    integer                         :: ntcurr ! Total number of timesteps on com- munication file (to read from) 
    integer                         :: ntimwa ! Time index of first function 
    integer                         :: ntimwb ! Time index of second function 
    integer                         :: tact   ! Actual time step number 
    integer, dimension(1)           :: idummy ! Help array to read/write Nefis files 
    integer, dimension(2)           :: ifcore ! Time indices (cell id's) of the wave functions which are in core available 
    integer, dimension(nelmx)       :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
    integer, external               :: neferr
    logical                         :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)                        :: atimw  ! Interpolation factor for first function 
    real(fp)                        :: btimw  ! Interpolation factor for second function 
    character(10), dimension(nelmx) :: elmunt ! Array with element physical unit 
    character(16)                   :: funam  ! Name of element which has to be read 
    character(16)                   :: grnam1 ! Data-group name defined for the COM-files (CURNT) 
    character(16)                   :: grnam2 ! Data-group name defined for the COM-files (CURTIM) 
    character(16), dimension(nelmx) :: elmnms ! Element name defined for the COM-files 
    character(16), dimension(nelmx) :: elmqty ! Array with element quantity 
    character(16), dimension(nelmx) :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                  :: errmsg
    character(64), dimension(nelmx) :: elmdes ! Array with element description 
!
! Data statements
!
    data grnam1/'CURNT'/
    data grnam2/'CURTIM'/
    data elmnms/'NTCUR', 'TIMCUR', 'QU', 'QV', 'S1', 'U1', 'V1', 'RSP'/
    data elmqty/8*' '/
    data elmunt/'[   -   ]', '[ TSCALE]', '[ M3/S  ]', '[ M3/S  ]', '[   M   ]',&
        & '[  M/S  ]', '[  M/S  ]', '[   -   ]'/
    data elmtps/2*'INTEGER', 6*'REAL'/
    data nbytsg/8*4/
    data elmdes/'Number of current fields in group CURTIM                      '&
       & , 'Time of current field rel.to reference date/time              ',    &
        & 'Time-average over latest interval of discharge in u-point     ',      &
        & 'Time-average over latest interval of discharge in v-point     ',      &
        & 'Water level in zeta point at end of time interval             ',      &
        & 'Velocity in u-point at end of time interval                   ',      &
        & 'Velocity in v-point at end of time interval                   ',      &
        & 'Spiral flow intensity                                         '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefisrstcom)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    ! Initialize local variables
    !
    nelmx2    = nelmx - 1
    ierr      = 0
    wrswch    = .false.
    ifcore(1) = 0
    ifcore(2) = 0
    !
    ! Set up the element dimensions
    ! different element dimensions for 2d and 3d applications
    ! if kmax =1 (2d) then only 2 dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       if (kmax > 1) then
          call filldm(elmdms    ,3         ,3         ,nmaxus    ,mmax      , &
                    & kmax      ,0         ,0         )
          call filldm(elmdms    ,4         ,3         ,nmaxus    ,mmax      , &
                    & kmax      ,0         ,0         )
       else
          call filldm(elmdms    ,3         ,2         ,nmaxus    ,mmax      , &
                    & 0         ,0         ,0         )
          call filldm(elmdms    ,4         ,2         ,nmaxus    ,mmax      , &
                    & 0         ,0         ,0         )
       endif
       call filldm(elmdms    ,5         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
       if (kmax > 1) then
          call filldm(elmdms    ,6         ,3         ,nmaxus    ,mmax      , &
                    & kmax      ,0         ,0         )
          call filldm(elmdms    ,7         ,3         ,nmaxus    ,mmax      , &
                    & kmax      ,0         ,0         )
       else
          call filldm(elmdms    ,6         ,2         ,nmaxus    ,mmax      , &
                    & 0         ,0         ,0         )
          call filldm(elmdms    ,7         ,2         ,nmaxus    ,mmax      , &
                    & 0         ,0         ,0         )
       endif
       call filldm(elmdms    ,8         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
    endif
    !
    ! Read the number of timesteps available at the file.
    !
    celidt = 1
    call putgti(comfil    ,grnam1    ,1         ,elmnms(1) ,elmdms(1, 1)         , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1) , &
              & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
    if (ierr/=0) goto 9999
    ntcurr = idummy(1)
    !
    ! Test if number of time steps on file are inside defined array
    ! boundary maxtim If error occurred then write errormessage to
    ! diagnostic file
    !
    if (ntcurr > maxtim) then
       call prterr(lundia    ,'D008'    ,' '       )
       error = .true.
       goto 9999
    endif
    !
    ! Read the array with time information.
    !
    do celidt = 1, ntcurr
       call putgti(comfil    ,grnam2    ,nelmx2    ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                 & elmnms(2) ,celidt    ,wrswch    ,ierr      ,timcur(celidt)       )
       if (ierr/=0) goto 9999
    enddo
    !
    ! Determination of reduced time and extrapolation coefficient
    !
    nready = 0
    if (itlen>0) then
       !
       ! Periodic functions
       !
       tact = timrst
       if (tact >= timcur(ntcurr)) then
          nhulp = (tact - timcur(ntcurr))/itlen + 1
          tact  = tact - nhulp*itlen
          if (tact < timcur(1)) then
             ntimwb = 1
             ntimwa = ntcurr
             btimw  = real(tact - timcur(ntcurr) + itlen,fp)      &
                    & /real(timcur(1) - timcur(ntcurr) + itlen,fp)
             nready = 1
          endif
       elseif (tact <= timcur(1)) then
          nhulp = (timcur(1) - tact)/itlen + 1
          tact  = tact + nhulp*itlen
          if (tact > timcur(ntcurr)) then
             ntimwb = 1
             ntimwa = ntcurr
             btimw  = real(tact - timcur(ntcurr),fp)              &
                    & /real(timcur(1) - timcur(ntcurr) + itlen,fp)
             nready = 1
          endif
       else
       endif
    else
       !
       ! A-periodic functions
       !
       tact = timrst
       if (tact < timcur(1)) then
          ntimwa = 1
          ntimwb = 2
          tact   = timcur(1)
          btimw  = 0.0
          nready = 1
       elseif (tact > timcur(ntcurr)) then
          ntimwa = ntcurr - 1
          ntimwb = ntcurr
          tact   = timcur(ntcurr)
          btimw  = 1.0
          nready = 1
       else
       endif
    endif
    if (nready == 0) then
       !
       ! Determination of interpolation coefficient
       !
       do i = 2, ntcurr
          if (tact <= timcur(i)) then
             ntimwb = i
             ntimwa = i - 1
             btimw  = real(tact - timcur(i - 1),fp)/real(timcur(i) - timcur(i - 1),fp)
             exit
          endif
       enddo
    endif
    atimw = 1.0 - btimw
    !
    ! Read the two selected timesteps of the functions and perform
    ! the interpolation.
    !
    funam = 'S1'
    kmaxk = 1
    call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grnam2    ,nelmx2    , &
              & elmnms(2) ,elmdms(1, 2)         ,elmqty(2) ,elmunt(2) ,elmdes(2) , &
              & elmtps(2) ,nbytsg(2) ,funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,s1        ,rbuff     ,gdp       )
    if (error) goto 9999
    !
    funam = 'U1'
    kmaxk = kmax
    call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grnam2    ,nelmx2    , &
              & elmnms(2) ,elmdms(1, 2)         ,elmqty(2) ,elmunt(2) ,elmdes(2) , &
              & elmtps(2) ,nbytsg(2) ,funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,u1        ,rbuff     ,gdp       )
    if (error) goto 9999
    !
    funam = 'V1'
    kmaxk = kmax
    call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grnam2    ,nelmx2    , &
              & elmnms(2) ,elmdms(1, 2)         ,elmqty(2) ,elmunt(2) ,elmdes(2) , &
              & elmtps(2) ,nbytsg(2) ,funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,v1        ,rbuff     ,gdp       )
    if (error) goto 9999
    !
    if (lsec>0) then
       funam = 'RSP'
       kmaxk = 1
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grnam2    ,nelmx2    , &
                 & elmnms(2) ,elmdms(1, 2)         ,elmqty(2) ,elmunt(2) ,elmdes(2) , &
                 & elmtps(2) ,nbytsg(2) ,funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,r1(1, -1, 1, lsecfl) ,rbuff     ,gdp       )
       if (error) then
       endif
    endif
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine rstcom
