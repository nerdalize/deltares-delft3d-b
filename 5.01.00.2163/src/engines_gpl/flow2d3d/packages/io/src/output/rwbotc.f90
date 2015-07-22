subroutine rwbotc(comfil    ,lundia    ,error     ,initi     ,itima     , &
                & itcomi    ,mmax      ,nmax      ,nmaxus    ,dp        , &
                & rbuff     ,ite       ,gdp       )
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
!  $Id: rwbotc.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/rwbotc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Write dp array to communication file if initi=1
!              - Read dp array from communication file if
!                initi=2 or 3
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
    integer, parameter :: nelmx = 3
!
! Global variables
!
    integer                                                          , intent(in)  :: initi  !!  Control parameter
                                                                                             !!  =1 initialization
                                                                                             !!  =2 initialization and read restart
                                                                                             !!     information from the communication file
                                                                                             !!  =3 no initialization
    integer                                                          , intent(in)  :: itcomi !  Description and declaration in inttim.igs
    integer                                                          , intent(in)  :: ite
    integer                                                          , intent(in)  :: itima  !!  Time to start simulation (N * tscale)
                                                                                             !!  according to DELFT3D conventions
    integer                                                                        :: lundia !  Description and declaration in inout.igs
    integer                                                                        :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: dp     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nmaxus, mmax)                                          :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                                   :: comfil !!  First part of file name
!
!
! Local variables
!
    integer                                    :: ierr
    integer                                    :: m
    integer                                    :: n
    integer                                    :: nrcel  ! Number of cells written in group BOTTIM (for initi=1 NRCEL=1) 
    integer       , dimension(1)               :: idummy ! Help array to read/write Nefis files 
    integer       , dimension(nelmx)           :: nbytsg ! Array containing the number of by- tes of each single ELMTPS 
    integer                         , external :: neferr
    logical                                    :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(10) , dimension(nelmx)           :: elmunt ! Array with element physical unit 
    character(16)                              :: grnam1
    character(16)                              :: grnam2
    character(16) , dimension(nelmx)           :: elmnms ! Element name defined for the COM-files 
    character(16) , dimension(nelmx)           :: elmqty ! Array with element quantity 
    character(16) , dimension(nelmx)           :: elmtps ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(256)                             :: errmsg
    character(64) , dimension(nelmx)           :: elmdes ! Array with element description 
!
! Data statements
!
    data grnam1/'BOTNT'/
    data grnam2/'BOTTIM'/
    data elmnms/'NTBOT', 'TIMBOT', 'DP'/
    data elmqty/3*' '/
    data elmunt/'[   -   ]', '[ TSCALE]', '[   M   ]'/
    data elmtps/'INTEGER', 'INTEGER', 'REAL'/
    data nbytsg/3*4/
    data elmdes/'Number of bottom fields in group BOTTIM                       '&
       & , 'Communication times bottom fields rel. to reference date/time ',    &
        & 'Bottom depth in bottom points, positive downwards             '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefisrwbotc)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    !-----Initialize local variables
    !
    ierr = 0
    !
    !-----Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,2         ,nmaxus    ,mmax      , &
                 & 0         ,0         ,0         )
    endif
    !
    !-----Initialize local parameter
    !
    celidt = 1
    nrcel = 1
    !
    !-----write nrcel, dp and itstrt to communication file for initi=1
    !     if itcomi > 0
    !
    if (initi==1 .and. itcomi>0) then
       wrswch = .true.
       idummy(1) = nrcel
       !
       call putgti(comfil    ,grnam1    ,1         ,elmnms(1) ,elmdms(1, 1)         , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1) , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
       if (ierr /= 0) goto 9999
       !
       idummy(1) = itima
       call putgti(comfil    ,grnam2    ,2         ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                 & elmnms(2) ,celidt    ,wrswch    ,ierr      ,idummy    )
       if (ierr /= 0) goto 9999
       !
       do n = 1, nmaxus
          do m = 1, mmax
             rbuff(n, m) = dp(n, m)
          enddo
       enddo
       !
       call putgtr(comfil    ,grnam2    ,2         ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                 & elmnms(3) ,celidt    ,.true.    ,ierr      ,rbuff     )
       if (ierr /= 0) goto 9999
    endif
    !
    !-----Read nrcel from communication file for initi=2 or 3
    !
    if (initi==2 .or. initi==3) then
       wrswch = .false.
       call putgti(comfil    ,grnam1    ,1         ,elmnms(1) ,elmdms(1, 1)         , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1) , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
       if (ierr /= 0) goto 9999
       nrcel = idummy(1)
       !
       !--------read dp from communication file
       !
       celidt = nrcel
       call putgtr(comfil    ,grnam2    ,2         ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                 & elmnms(3) ,celidt    ,wrswch    ,ierr      ,rbuff     )
       if (ierr /= 0) goto 9999
       !
       do n = 1, nmaxus
          do m = 1, mmax
             dp(n, m) = rbuff(n, m)
          enddo
       enddo
    endif
    !
    !-----Read nrcel from communication file for initi.ge.4
    !
    if (initi>=4 .and. itcomi>0) then
       wrswch = .false.
       call putgti(comfil    ,grnam1    ,1         ,elmnms(1) ,elmdms(1, 1)         , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1) , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
       if (ierr /= 0) goto 9999
       nrcel = idummy(1)
       !
       !-----write nrcel, dp and itstrt to communication file for initi.ge.4
       !
       celidt = nrcel
       wrswch = .true.
       idummy(1) = nrcel
       !
       call putgti(comfil    ,grnam1    ,1         ,elmnms(1) ,elmdms(1, 1)         , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps(1) ,nbytsg(1) , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierr      ,idummy    )
       if (ierr /= 0) goto 9999
       !
       idummy(1) = ite
       call putgti(comfil    ,grnam2    ,2         ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                 & elmnms(2) ,celidt    ,wrswch    ,ierr      ,idummy    )
       if (ierr /= 0) goto 9999
       !
       do n = 1, nmaxus
          do m = 1, mmax
             rbuff(n, m) = dp(n, m)
          enddo
       enddo
       !
       call putgtr(comfil    ,grnam2    ,2         ,elmnms(2) ,elmdms(1, 2)         , &
                 & elmqty(2) ,elmunt(2) ,elmdes(2) ,elmtps(2) ,nbytsg(2) , &
                 & elmnms(3) ,celidt    ,.true.    ,ierr      ,rbuff     )
       if (ierr /= 0) goto 9999
    endif
    !
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine rwbotc
