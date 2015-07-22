subroutine wridoc(error, neffil, soort, simdat, runtxt, commrd, gdp)
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
!  $Id: wridoc.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wridoc.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the initial group 4 ('"soort"-version') to
!              "SOORT"-DAT
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
    logical                    , pointer :: first
    integer                    , pointer :: celidt
    integer, dimension(:, :)   , pointer :: elmdms
    integer                    , pointer :: lundia    !  Description and declaration in inout.igs
    integer                    , pointer :: lunprt    !  Description and declaration in inout.igs
    character*131, dimension(:), pointer :: header    !  Description and declaration in postpr.igs
    logical                              :: commrd
    type (nefiselement)        , pointer :: nefiselem
!
! Local parameters
!
    integer, parameter :: nelmx = 4
!
! Global variables
!
    logical          , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    character(*)     , intent(in)  :: neffil !!  File name for FLOW NEFIS output
                                             !!  files: tri"h/m/d"-"casl""labl" or
                                             !!  for Comm. file com-"casl""labl"
    character(16)    , intent(in)  :: simdat !!  Simulation date representing the
                                             !!  flow condition at this date
    character(6)     , intent(in)  :: soort  !!  String containing to which output
                                             !!  file version group or to diagnostic
                                             !!  file should be written
    character(30), dimension(10)   :: runtxt !!  Textual description of model input

!
! Local variables
!
    integer                            :: datlen
    integer                            :: deflen
    integer                            :: fd_nef
    integer                            :: i            ! Help var. 
                                                       ! and last 2 char. (\n) from SYSTXT lines Help var.
    integer                            :: ierror       ! Local errorflag for NEFIS files 
    integer                            :: iheader      ! Loop counter for writing header
    integer                            :: ind
    integer                            :: lrid         ! Help var. to determine the actual length of RUNID 
    integer                            :: lridmx       ! Help var. for lunprt: LRID < 47 
    integer                            :: n
    integer                            :: na
    integer, dimension(nelmx)          :: nbytsg       ! Array containing the number of by- tes of each single ELMTPS 
    integer, dimension(3)              :: uindex
    integer, external                  :: clsnef
    integer, external                  :: crenef
    integer, external                  :: getels
    integer, external                  :: neferr
    logical                            :: wrswch       ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    character(10)                      :: date         ! Date to be filled in the header 
    character(256)                     :: datnam
    character(256)                     :: defnam
    character(10), dimension(nelmx)    :: elmunt       ! Array with element physical unit 
    character(16), dimension(1)        :: cdum16       ! Help array to read/write Nefis files 
    character(16), dimension(nelmx)    :: elmnms       ! Element name defined for the NEFIS-files 
    character(16), dimension(nelmx)    :: elmqty       ! Array with element quantity 
    character(16), dimension(nelmx)    :: elmtps       ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(16)                      :: grnam4       ! Data-group name defined for the NEFIS-files 
    character(256)                     :: filnam       ! Help var. for FLOW file name 
    character(4)                       :: errnr        ! Character var. containing the errormessage number corresponding to errormessage in ERRFIL 
    character(256)                     :: errmsg       ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64), dimension(nelmx)    :: elmdes       ! Array with element description 
    character(20)                      :: rundat       ! Current date and time containing a combination of DATE and TIME 
    character(256)                     :: version_full ! Version nr. of the module of the current package
    character(256), dimension(1)       :: cdumcident   ! Help array to read/write Nefis files 
!
! Data statements
!
    data elmnms/'FLOW-SIMDAT', 'FLOW-SYSTXT', 'FLOW-RUNTXT', 'FILE-VERSION'/
    data elmqty/4*' '/
    data elmunt/4*'[   -   ]'/
    data elmtps/4*'CHARACTER'/
    data nbytsg/16, 256, 30, 16/
    data (elmdes(i), i = 1, nelmx)                                              &
         & /'FLOW Simulation date and time [YYYYMMDD  HHMMSS]              ',    &
         & 'FLOW System description                                       ',     &
         & 'FLOW User defined Model description                           ',     &
         & 'Version number of file                                        '/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswridoc)
    first   => nefiselem%first
    celidt  => nefiselem%celidt
    elmdms  => nefiselem%elmdms
    lundia  => gdp%gdinout%lundia
    lunprt  => gdp%gdinout%lunprt
    header  => gdp%gdpostpr%header
    !
    !
    ! Initialize local variables
    !
    ierror = 0
    celidt = 1
    !
    filnam = neffil
    if (soort(1:3)/='com') filnam = neffil(1:3) // soort(1:1) // neffil(5:)
    grnam4 = soort(1:3) // '-version'
    errmsg = ' '
    wrswch = .true.
    !
    ! Set up the element dimensions
    ! Remove first 4 and last 2 characters from string SYSTXT
    !
    if (first) then
       first = .false.
       call filldm(elmdms    ,1         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,2         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,3         ,1         ,10        ,0         , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,4         ,1         ,1         ,0         , &
                 & 0         ,0         ,0         )
    endif
    !
    ! Write system definition to diagnostic file for SOORT = 'dia'
    ! and skip rest of routine
    !
    version_full  = ' '
    !version_short = ' '
    call getfullversionstring_flow2d3d(version_full)
    !
    if (soort(1:3) == 'dia') then
       ! nothing
    elseif (soort(1:5) == 'ascii') then
        !
        ! write start date and time to LUNPRT
        !
       call noextspaces(gdp%runid, lrid)
       lridmx = min(lrid, 47)
       !
       ! Date and time
       !
       call dattim(rundat)
       date(1:4)  = rundat(1:4)
       date(5:5)  = '-'
       date(6:7)  = rundat(6:7)
       date(8:8)  = '-'
       date(9:10) = rundat(9:10)
       !
       ! Version info
       !
       !
       write (header(1 ), '(131a1)'    ) ('*', na = 1, 131)
       write (header(2 ), '(a,a,a,a,a,a,a,a,a,t129,a)') &
           & '*** Print of ', 'Delft3D-FLOW', ' for Run ', gdp%runid(:lridmx) , ' - Simulation date: ',  &
           & date, ' ', rundat(11:19), '  page     1', '***'
       write (header(3 ), '(2a,t129,a)') '*** ', trim(version_full), '***'
       write (header(4 ), '(a,t129,a)' ) '*** User: Unknown ', '***'
       write (header(5 ), '(131a1)'    ) ('*', na = 1, 131)
       write (header(6 ), '(a)'        )
       write (header(10), '(a)'        )
       !
       do iheader = 1,5
          write (lunprt, '(a)') header(iheader)
       enddo
    else
       !
       ! Write to NEFIS files
       ! group 4, element 'SIMDAT'
       !
       cdum16(1) = simdat
       call putgtc(filnam    ,grnam4    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(1) ,celidt    ,wrswch    ,ierror    ,cdum16    )
       if (ierror/=0) goto 999
       !
       ! group 4, element 'FLOW-SYSTXT'
       !
       cdumcident(1) = trim(version_full)
       call putgtc(filnam    ,grnam4    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(2) ,celidt    ,wrswch    ,ierror    ,cdumcident)
       if (ierror/=0) goto 999
       !
       ! group 4, element 'RUNTXT'
       !
       call putgtc(filnam    ,grnam4    ,nelmx     ,elmnms    ,elmdms    , &
                 & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                 & elmnms(3) ,celidt    ,wrswch    ,ierror    ,runtxt    )
       if (ierror/=0) goto 999
       !
       ! group 5, element 'FILE-VERSION'
       ! drogues file  'd'
       ! history file  'h'
       ! map     file  'f'
       ! comm    file  'c'
       !
       cdum16(1) = '00.00.00.00'
       if (soort(1:1)=='d') then
          call getdrofileversionstring_flow2d3d(cdum16(1))
       elseif (soort(1:1)=='h') then
          call gethisfileversionstring_flow2d3d(cdum16(1))
       elseif (soort(1:1)=='m') then
          call getmapfileversionstring_flow2d3d(cdum16(1))
       elseif (soort(1:1)=='c') then
          call getcomfileversionstring_flow2d3d(cdum16(1))
       else
       endif
       if (soort(1:1)=='c') then
          !
          ! Check if COM-file is a new one or an existing one
          !
          if (.not. commrd) then
             !
             ! COM-file has been newly generated and
             ! does not have a version number
             ! So write it to the COM-file
             !
!          ! First aggregate file names
!          !
!          ind                 = len_trim(filnam)+1
!          datnam              = filnam
!          datnam(ind:ind + 3) = '.dat'
!          call noextspaces(datnam, datlen)
!          !
!          defnam              = filnam
!          defnam(ind:ind + 3) = '.def'
!          call noextspaces(defnam, deflen)
!          !
!          fd_nef      = 0
!          ierror      = 0
!          ierror      = crenef(fd_nef, datnam(1:datlen), defnam(1:deflen), ' ', 'r')
!          uindex(1:3) = 1
!          cdum16(1)   = ' '
!          ierror      = getels(fd_nef, 'com-version', 'FILE-VERSION', uindex, 1, 16, cdum16(1) )
!          ierror      = clsnef(fd_nef)
!          if (cdum16(1) == ' ') then
!             !
!             ! No version number information on COM-file yet: newly written COM-file
!             ! Write current version number to file
!             !
             call putgtc(filnam    ,grnam4    ,nelmx     ,elmnms    ,elmdms    , &
                       & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                       & elmnms(4) ,celidt    ,wrswch    ,ierror    ,cdum16    )
          endif
       else
          call putgtc(filnam    ,grnam4    ,nelmx     ,elmnms    ,elmdms    , &
                    & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                    & elmnms(4) ,celidt    ,wrswch    ,ierror    ,cdum16    )
       endif
       if (ierror/=0) then
       endif
    endif
    !
    ! write errormessage if error occurred and set error = .TRUE.
    ! For all but Comm. file
    !
  999 continue
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wridoc
