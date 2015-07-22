subroutine wrihisdis(lundia    ,error     ,trifil    ,itdate    ,tunit     , &
                   & dt        ,nsrc      ,namsrc    ,gdp       )
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
!  $Id: wrihisdis.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrihisdis.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the initial Discharge group to HIS-DAT
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
    integer, parameter :: nelmx = 4
!
! Global variables
!
    integer                        , intent(in)  :: itdate ! Description and declaration in exttim.igs
    integer                        , intent(in)  :: lundia ! Description and declaration in inout.igs
    integer                        , intent(in)  :: nsrc   ! Description and declaration in dimens.igs        
    real(fp)                       , intent(in)  :: dt     ! Description and declaration in esm_alloc_real.f90    
    real(fp)                       , intent(in)  :: tunit  ! Description and declaration in exttim.igs
    logical                        , intent(out) :: error  ! Description and declaration in tricom.f90
    character(20), dimension(nsrc) , intent(in)  :: namsrc ! Description and declaration in r-i-ch.igs
    character(*)                   , intent(in)  :: trifil ! Description and declaration in trisim.F90
!
! Local variables
!
    integer                                    :: i
    integer                                    :: isrc   ! Index number of discharge location 
    integer                                    :: ierror ! Local errorflag for NEFIS files
    integer       , dimension(2)               :: ival   ! Local array for writing ITDATE and
    integer       , dimension(nelmx)           :: nbytsg ! Help array for name constituents and turbulent quantities Array containing the number of bytes of each single ELMTPS
    integer                         , external :: neferr                                                                                               
    real(fp)      , dimension(1)               :: rdummy ! putgtr expects an array                                                                     
    logical                                    :: wrswch ! Flag to write file
    character(64) , dimension(nelmx)           :: elmdes
    character(16) , dimension(nelmx)           :: elmnms
    character(16) , dimension(nelmx)           :: elmqty
    character(16) , dimension(nelmx)           :: elmtps
    character(10) , dimension(nelmx)           :: elmunt    
    character(256)                             :: errmsg
    character(256)                             :: filnam ! Help var. for file name 
    character(16)                              :: grnam  ! Data-group name defined for the NEFIS-files group 1 
    character(80) , dimension(nsrc)            :: disnam ! Names of discharges extended with 60 spaces
!
! Data statements
!
    data grnam/'his-dis-const'/
    data elmnms/'ITDATE', 'TUNIT', 'DT', 'DISCHARGES'/
    data elmqty/4*' '/
    data elmunt/'[YYYYMMDD]', '[   S   ]', 2*'[   -   ]'/
    data elmtps/'INTEGER', 2*'REAL', 'CHARACTER'/
    data nbytsg/3*4, 80/
    data (elmdes(i), i = 1, nelmx)                                              &
         & /'Initial date (input) & time (default 00:00:00)                ',   &
         &  'Time scale related to seconds                                 ',   &
         &  'Time step (DT*TUNIT sec)                                      ',   &
         &  'Names identifying discharges                                  '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrihisdis)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    !
    ! Initialize local variables
    !
    ierror = 0
    celidt = 1
    !
    filnam = trifil(1:3) // 'h' // trifil(5:)
    errmsg = ' '
    wrswch = .true.
    !
    ! Set up the element dimensions
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,2         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,1         ,nsrc      ,0         , &
                 & 0         ,0         ,0         )
    endif
    !
    ! Element 'ITDATE'
    !
    ival(1) = itdate
    ival(2) = 000000
    call putgti(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(1) ,celidt    ,wrswch    ,ierror   ,ival      )
    if (ierror/= 0) goto 999
    !
    ! Element 'TUNIT'
    !
    rdummy(1) = tunit
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(2) ,celidt    ,wrswch    ,ierror   ,rdummy    )
    if (ierror/= 0) goto 999
    !
    ! Element 'DT'
    !
    rdummy(1) = dt
    call putgtr(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(3) ,celidt    ,wrswch    ,ierror   ,rdummy    )
    if (ierror/= 0) goto 999
    !
    ! Element 'DISCHARGES'
    !
    do isrc = 1, nsrc
        disnam(isrc) = namsrc(isrc)//repeat(' ',60)
    enddo
    call putgtc(filnam    ,grnam     ,nelmx     ,elmnms    ,elmdms       , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg       , &
              & elmnms(4) ,celidt    ,wrswch    ,ierror   ,disnam       )
    if (ierror/= 0) goto 999
    !
    ! Write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrihisdis
