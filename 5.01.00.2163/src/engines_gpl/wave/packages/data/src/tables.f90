module tables
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
!  $Id: tables.f90 1652 2012-06-28 10:03:36Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/data/src/tables.f90 $
!!--description-----------------------------------------------------------------
!
! Tables module
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_sp
    use utilities
    !
    private
    public tablefiletype
    !
    public org_readtable
    public org_cleartable
    public org_getntables
    public org_gettable
    public org_checktable
    public org_checktableparnames
    public org_gettablelocation
    public org_gettablentimes
    public org_gettabletimes
    public org_gettabledata
    public org_getfilename
    !
    integer, parameter, public :: MAXTABLECLENGTH = 47

    integer, parameter, public :: CHKTAB_PARNAME  = 1
    integer, parameter, public :: CHKTAB_POSITIVE = 2
    integer, parameter, public :: CHKTAB_BLOCK    = 3
    integer, parameter, public :: CHKTAB_LOGICAL  = 4

    interface org_gettable
       module procedure org_gettable_vector, org_gettable_scalar
    end interface

    interface org_gettabledata
       module procedure org_gettabledata_vector, org_gettabledata_scalar
    end interface

    type tableparametertype
        character(MAXTABLECLENGTH) :: name
        character(MAXTABLECLENGTH) :: unit
        character(MAXTABLECLENGTH) :: interpolation
    end type tableparametertype

    type tabletype
        real(hp)                                          :: timestep
        real(hp)                                          :: timeunit
        real(fp)                ,dimension(3)             :: geocoords
        real(fp)                ,dimension(3)             :: metriccoords
        real(fp)                                          :: reftime
        integer                                           :: refdate
        integer                                           :: layer
        integer                                           :: nrecords
        integer                                           :: nparameters
        type(tableparametertype), dimension(:)  , pointer :: parameters
        real(hp)                , dimension(:)  , pointer :: times
        real(fp)                , dimension(:,:), pointer :: values
        character(MAXTABLECLENGTH)                        :: name
        character(MAXTABLECLENGTH)                        :: contents
        character(MAXTABLECLENGTH)                        :: location
        character(MAXTABLECLENGTH)                        :: interpolation
        character(MAXTABLECLENGTH)                        :: extrapolation
        character(MAXTABLECLENGTH)                        :: timeunitstr
        character(MAXTABLECLENGTH)                        :: timefunction
    end type tabletype

    type tablefiletype
        character(256)                         :: filename = 'UNKNOWN'
        type(tabletype), dimension(:), pointer :: tables => NULL()
    end type tablefiletype
!
!
!
contains
!
!
!===============================================================================
subroutine org_readtable(this, lunbcm, filnam, refjulday, errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Read table files (i.e. BCT/BCC/BCM/BCW)
!
!!------------------------------------------------------------------------------
!
! Local parameters
!
    integer, parameter :: MAXFLD = 200
!
! Global variables
!
    integer            ,intent(in)  :: lunbcm
    integer            ,intent(in)  :: refjulday
    character(*)       ,intent(in)  :: filnam
    character(256)     ,intent(out) :: errorstring
    type(tablefiletype)             :: this
!
! Local variables
!
    integer                                               :: i
    integer                                               :: idd
    integer                                               :: ierr
    integer                                               :: ihh
    integer                                               :: ijuldate
    integer                                               :: imm
    integer                                               :: iss
    integer                                               :: iline
    integer                                               :: ipar
    integer                                               :: ipar0
    integer                                               :: iread_phase
    integer                                               :: irec
    integer                                               :: istat
    integer                                               :: itable
    integer                                               :: itimestep
    integer                                               :: iyyyymmdd
    integer                                               :: nrecords
    integer                                               :: ntables
    integer                                               :: ntoken
    integer                                               :: nvalues
    integer                   , dimension(:), allocatable :: itype
    integer                   , dimension(:), allocatable :: ifield
    integer                   , dimension(:), allocatable :: lenchr
    !
    logical                                               :: feof
    logical                                               :: isdata
    !
    real(fp)                  , dimension(:), allocatable :: rfield
    !
    character(MAXTABLECLENGTH), dimension(:), allocatable :: cfield
    character(10)                                         :: stri1
    character(10)                                         :: stri2
    character(512)                                        :: line
    !
    type(tabletype)           , dimension(:), pointer     :: tables => NULL()
    type(tabletype)                         , pointer     :: table
!
!! executable statements -------------------------------------------------------
!
    istat       = 0
    errorstring = 'org_readtable: memory alloc error'
    !
    open (lunbcm, file = filnam, form = 'formatted', &
        & status = 'old', iostat = istat)
    if (istat /= 0) then
       errorstring = '*** ERROR Error while opening file '//trim(filnam)
       goto 210
    endif
    this%filename = filnam
    !
    ! Allocate scannr arrays
    !
                    allocate(itype(MAXFLD) , stat = istat)
    if (istat == 0) allocate(ifield(MAXFLD), stat = istat)
    if (istat == 0) allocate(rfield(MAXFLD), stat = istat)
    if (istat == 0) allocate(cfield(MAXFLD), stat = istat)
    if (istat == 0) allocate(lenchr(MAXFLD), stat = istat)
    if (istat /= 0) goto 210
    !
    ! Count number of tables in file
    !
    ntables = 0
    do iread_phase = 1,3
       !
       ! Begin of iread_phase loop
       !          1: determine number of tables
       !          2: determine number of parameters and records per table
       !          3: read parameters and data values
       !
       itable = 0
       iline  = 0
       feof   = .false.
       do while (.not.feof)
          !
          ! table loop
          !
          itable = itable + 1
          ipar   = 0
          !
          if (iread_phase > 1) then
             table => tables(itable)
          endif
          !
          if (iread_phase == 3) then
             !
             ! time column should be treated in a different manner
             !
             if (table%timefunction == 'non-equidistant') then
                ipar = -1
             endif
          endif
          !
          isdata = .false.
          !
          nvalues = 0
          !
          do while (.not.isdata)
             !
             ! keyword loop for current table
             !
             ! => process keyword
             !
             if (iline == 0) then
                !
                ! No line read yet
                !
             elseif (itype(1) == 3) then
                !
                call small(cfield(1),len(cfield(1)))
                if (cfield(1)(1:1) == '*' .or. cfield(1)(1:1) == '#' &
                                          .or. cfield(1)(1:1) == ';') then
                   !
                   ! Skip comments and record length
                   !
                elseif (cfield(1) == 'table-name') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3) then
                         errorstring = 'Table name must be a string'
                         goto 200
                      else
                         table%name = cfield(2)
                      endif
                   endif
                elseif (cfield(1) == 'contents') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3) then
                         errorstring = 'Contents must be a string'
                         goto 200
                      else
                         call small(cfield(2),len(cfield(2)))
                         table%contents = cfield(2)
                      endif
                   endif
                elseif (cfield(1)=='geo-coordinates') then
                   !
                   ! geographical co-ordinates
                   !
                   if (iread_phase == 2) then
                      if (ntoken<3) then
                         errorstring = 'Missing coordinate data'
                         goto 200
                      else
                         !
                         ! geo-coordinates <long> <lat> [depth]
                         ! geo-coordinates <long> <lat> layer <layer number>
                         !
                         if (itype(2)==1 .or. itype(2)==2) then
                            table%geocoords(1) = rfield(2)
                         else
                            errorstring = 'Longitude must be a numeric value'
                            goto 200
                         endif
                         if (itype(3)==1 .or. itype(3)==2) then
                            table%geocoords(2) = rfield(3)
                         else
                            errorstring = 'Latitude must be a numeric value'
                            goto 200
                         endif
                         if (ntoken==3) then
                            !
                            ! no optional elevation data
                            !
                         elseif (ntoken==4) then
                            !
                            ! geo-coordinates <long> <lat> [depth]
                            !
                            if (itype(4)==1 .or. itype(4)==2) then
                               table%geocoords(3) = rfield(4)
                            else
                               errorstring = 'Depth must be a numeric value'
                               goto 200
                            endif
                         elseif (ntoken==5) then
                            !
                            ! geo-coordinates <long> <lat> layer <layer number>
                            !
                            call small(cfield(4),len(cfield(4)))
                            if (cfield(4) /= 'layer') then
                               errorstring = 'Expected keyword ''layer'''
                               goto 200
                            elseif (itype(5) /= 1 .or. ifield(5) < 0) then
                               errorstring = 'Layer must be a non-negative integer'
                               goto 200
                            else
                               table%layer = ifield(5)
                            endif
                         else ! if (ntoken > 5) then
                            errorstring = 'Too many arguments on line'
                            goto 200
                         endif
                      endif
                   endif
                elseif (cfield(1) == 'location') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3) then
                         errorstring = 'Location must be a string'
                         goto 200
                      else
                         table%location = cfield(2)
                      endif
                   endif
                elseif (cfield(1) == 'interpolation') then
                   if (ipar <= 0) then
                      if (iread_phase == 2) then
                         if (ntoken < 2) then
                            errorstring = 'Too few arguments on line'
                            goto 200
                         elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                                & cfield(3)(1:1) /= '*' .and. &
                                                & cfield(3)(1:1) /= ';') then
                            errorstring = 'Too many arguments on line'
                            goto 200
                         elseif (itype(2) /= 3) then
                            errorstring = 'Interpolation type must be a string'
                            goto 200
                         else
                            call small(cfield(2),len(cfield(2)))
                            table%interpolation = cfield(2)
                            if (cfield(2) /= 'linear' .and. &
                              & cfield(2) /= 'block') then
                               errorstring = 'Interpolation type must be ''linear'' or ''block'''
                               goto 200
                            endif
                         endif
                      endif
                   else
                      if (iread_phase == 3) then
                         if (ntoken < 2) then
                            errorstring = 'Too few arguments on line'
                            goto 200
                         elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                                & cfield(3)(1:1) /= '*' .and. &
                                                & cfield(3)(1:1) /= ';') then
                            errorstring = 'Too many arguments on line'
                            goto 200
                         elseif (itype(2) /= 3) then
                            errorstring = 'Interpolation type must be a string'
                            goto 200
                         else
                            call small(cfield(2),len(cfield(2)))
                            table%parameters(ipar)%interpolation = cfield(2)
                            if (cfield(2) /= 'linear' .and. &
                              & cfield(2) /= 'block') then
                               errorstring = 'Interpolation type must be ''linear'' or ''block'''
                               goto 200
                            endif
                         endif
                      endif
                   endif
                elseif (cfield(1) == 'extrapolation') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3) then
                         errorstring = 'Extrapolation type must be a string'
                         goto 200
                      else
                         call small(cfield(2),len(cfield(2)))
                         table%extrapolation = cfield(2)
                         if (cfield(2) /= 'periodic' .and. &
                           & cfield(2) /= 'constant' .and. &
                           & cfield(2) /= 'none') then
                            errorstring = 'Extrapolation type must be ''periodic'', ''constant'' or ''none'''
                            goto 200
                         endif
                      endif
                   endif
                elseif (cfield(1) == 'records-in-table') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 1) then
                         errorstring = 'Number of records in table must be positive integer'
                         goto 200
                      else
                         table%nrecords = ifield(2)
                         if (ifield(2) <= 0) then
                            errorstring = 'Number of records in table must be positive integer'
                            goto 200
                         endif
                      endif
                   endif
                elseif (cfield(1) == 'metric') then
                   !
                   ! metric co-ordinates
                   !
                   if (iread_phase == 2) then
                      call small(cfield(2),len(cfield(2)))
                      if (itype(2) /= 3) then
                         errorstring = 'Unknown keyword: '//trim(cfield(1))
                         goto 200
                      elseif (cfield(2) /= 'coordinates') then
                         errorstring = 'Unknown keyword: '//trim(cfield(1))//' '//trim(cfield(2))
                         goto 200
                      elseif (ntoken<4) then
                         errorstring = 'Missing coordinate data'
                         goto 200
                      else
                         !
                         ! metric coordinates <x> <y> [depth]
                         ! metric coordinates <x> <y> layer <layer number>
                         !
                         if (itype(3) /= 3) then
                            table%metriccoords(1) = rfield(3)
                         else
                            errorstring = 'X co-ordinate must be a numeric value'
                            goto 200
                         endif
                         if (itype(4) /= 3) then
                            table%metriccoords(2) = rfield(4)
                         else
                            errorstring = 'Y co-ordinate must be a numeric value'
                            goto 200
                         endif
                         if (ntoken == 4) then
                            !
                            ! no optional elevation data
                            !
                         elseif (ntoken == 5) then
                            !
                            ! metric coordinates <x> <y> [depth]
                            !
                            if (itype(5) /= 3) then
                               table%metriccoords(3) = rfield(5)
                            else
                               errorstring = 'Depth must be a numeric value'
                               goto 200
                            endif
                         elseif (ntoken == 6) then
                            !
                            ! metric coordinates <x> <y> layer <layer number>
                            !
                            call small(cfield(5),len(cfield(5)))
                            if (cfield(5) /= 'layer') then
                               errorstring = 'Expected keyword ''layer'''
                               goto 200
                            elseif (itype(6) /= 1 .or. ifield(6) < 0) then
                               errorstring = 'Layer must be a non-negative integer'
                               goto 200
                            else
                               table%layer = ifield(6)
                            endif
                         else ! if (ntoken > 6) then
                            errorstring = 'Too many arguments on line'
                            goto 200
                         endif
                      endif
                   endif
                elseif (cfield(1) == 'layer') then
                   !
                   ! layer <layer number>
                   !
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 1 .or. ifield(2) < 0) then
                         errorstring = 'Layer must be a non-negative integer'
                         goto 200
                      else
                         table%layer = ifield(2)
                      endif
                   endif
                elseif (cfield(1) == 'time-unit') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3) then
                         errorstring = 'Time unit must be a string'
                         goto 200
                      else
                         call small(cfield(2),len(cfield(2)))
                         table%timeunitstr = cfield(2)
                         if (cfield(2) == 'date' .or. cfield(2) == 'absolute') then
                             table%timeunit = -1.0_hp
                             table%timeunitstr = 'date'
                         elseif (cfield(2) == 'years') then
                             table%timeunit = 365.0_hp
                         elseif (cfield(2) == 'decades') then
                             table%timeunit = 3650.0_hp
                         elseif (cfield(2) == 'days') then
                             table%timeunit = 1.0_hp
                         elseif (cfield(2) == 'hours') then
                             table%timeunit = 1.0_hp / 24.0_hp
                         elseif (cfield(2) == 'minutes') then
                             table%timeunit = 1.0_hp / 1440.0_hp
                         elseif (cfield(2) == 'seconds') then
                             table%timeunit = 1.0_hp / 86400.0_hp
                         elseif (cfield(2) == 'ddhhmmss') then
                             table%timeunit = 1.0_hp
                         else
                            errorstring = 'Time unit must be ''date'', ''years'', ''decades'', ''days'', ''hours'', ''minutes'', ''seconds'', ''ddhhmmss'''
                            goto 200
                         endif
                      endif
                   endif
                elseif (cfield(1) == 'time-step') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3 ) then
                         table%timestep = rfield(2)
                      else
                         errorstring = 'Time step must be a numeric value'
                      endif
                   endif
                elseif (cfield(1) == 'reference-time') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 3 .and. cfield(4)(1:1) /= '#' .and. &
                                             & cfield(4)(1:1) /= '*' .and. &
                                             & cfield(4)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) == 1) then
                         if (ntoken == 2) then
                           !
                           ! <yyyymmdd>
                           !
                           call juldat(ifield(2),table%refdate)
                           table%reftime = 0.0_fp
                         elseif (ntoken == 3 .and. itype(3) == 1) then
                           !
                           ! <yyyymmdd> <hhmmss>
                           !
                           call juldat(ifield(2),table%refdate)
                           ihh = ifield(3) / 10000
                           ifield(3) = ifield(3) - ihh * 10000
                           imm = ifield(3) / 100
                           iss = ifield(3) - imm * 100
                           table%reftime = real(  real(ihh,hp) / 60.0_hp   + &
                                                & real(imm,hp) / 1440.0_hp + &
                                                & real(iss,hp) / 86400.0_hp   ,fp)
                         else
                            errorstring = 'Invalid reference time specification'
                            goto 200
                         endif
                      elseif (itype(2) == 2) then
                         errorstring = 'Invalid reference time specification'
                         goto 200
                      else ! if (itype(2) == 3) then
                         call small(cfield(2),len(cfield(2)))
                         table%refdate = refjulday
                         table%reftime = 0.0_fp
                         if (cfield(2) /= 'from model') then
                            errorstring = 'Reference time must be explicitly specified or it should be ''from model'''
                            goto 200
                         endif
                      endif
                   endif
                elseif (cfield(1) == 'constant') then
                   if (iread_phase == 2) then
                      if (ntoken < 1) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 1 .and. cfield(2)(1:1) /= '#' .and. &
                                             & cfield(2)(1:1) /= '*' .and. &
                                             & cfield(2)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      else
                         table%timefunction = 'constant'
                         table%nrecords = 1
                      endif
                   endif
                elseif (cfield(1) == 'time-function') then
                   if (iread_phase == 2) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 2 .and. cfield(3)(1:1) /= '#' .and. &
                                             & cfield(3)(1:1) /= '*' .and. &
                                             & cfield(3)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3) then
                         errorstring = 'Time function must be a string'
                         goto 200
                      else
                         call small(cfield(2),len(cfield(2)))
                         table%timefunction = cfield(2)
                         if (cfield(2) /= 'astronomic' .and. &
                           & cfield(2) /= 'harmonic' .and. &
                           & cfield(2) /= 'equidistant' .and. &
                           & cfield(2) /= 'non-equidistant') then
                            errorstring = 'Time function must be ''astronomic'', ''harmonic'', ''equidistant'' or ''non-equidistant'''
                            goto 200
                         endif
                      endif
                   endif
                elseif (cfield(1)=='parameter') then
                   ipar = ipar + 1
                   if (iread_phase == 2) then
                      table%nparameters = ipar
                   elseif (iread_phase == 3) then
                      if (ntoken < 2) then
                         errorstring = 'Too few arguments on line'
                         goto 200
                      elseif (ntoken > 4 .and. cfield(5)(1:1) /= '#' .and. &
                                             & cfield(5)(1:1) /= '*' .and. &
                                             & cfield(5)(1:1) /= ';') then
                         errorstring = 'Too many arguments on line'
                         goto 200
                      elseif (itype(2) /= 3) then
                         errorstring = 'Parameter name must be a string'
                         goto 200
                      elseif (ntoken < 4) then
                         errorstring = 'Missing unit string'
                         goto 200
                      elseif (itype(4) /= 3) then
                         errorstring = 'Unit must be a string'
                         goto 200
                      endif
                      if (ipar == 0) then
                         !
                         ! Time column should be skipped; check whether
                         ! the parameter is indeed called 'time'.
                         !
                         call small(cfield(2),MAXTABLECLENGTH)
                         if (cfield(2) /= 'time') then
                            errorstring = 'Parameter in first column should be ''time'''
                            goto 200
                         endif
                      else
                         !
                         ! Normal data column
                         !
                         table%parameters(ipar)%name = cfield(2)
                         table%parameters(ipar)%unit = cfield(4)
                         table%parameters(ipar)%interpolation = table%interpolation
                      endif
                   endif
                else
                   if (cfield(1)(1:1) /= '*' .and. cfield(1)(1:1) /= '#' &
                                          &  .and. cfield(1)(1:1) /= ';') then
                      errorstring = 'Unknown keyword: '//cfield(1)
                      goto 200
                   endif
                endif
             endif
             !
             ! Read new line
             !
             iline = iline+1
             read (lunbcm, '(A)', iostat = istat) line
             if (istat < 0) feof = .true.
             !
             ntoken = 0
             call scannr(   line,       1,        len(line),  ntoken,   itype, &
                       &  ifield,  rfield,  cfield,  lenchr,  MAXFLD,  .true., &
                       & .false., .false.)
             !
             if (ntoken<0) goto 200
             !
             !----------------------------------------------
             !  do i = 1, ntoken
             !     if (itype(i) == 1) then
             !        write(*,*) '>',ifield(i),'<'
             !     elseif (itype(i) == 2) then
             !        write(*,*) '>',rfield(i),'<'
             !     elseif (itype(i) == 3) then
             !        write(*,*) '>',cfield(i),'<'
             !     endif
             !  enddo
             !----------------------------------------------
             !
             if (itype(1) /= 3) then
                if (ipar == 0) then
                   errorstring = 'Data encountered before parameters definition'
                   goto 200
                else
                   !
                   ! Goto data loop
                   !
                   isdata = .true.
                endif
             endif
             !
             ! End of keyword loop for current table
             !
          enddo
          !
          if (iread_phase == 2) then
             if (table%timeunitstr == 'ddhhmmss' .and. &
               & table%timefunction == 'equidistant') then
                if (table%timestep < 0.0_hp) then
                   errorstring = 'Missing time-step in equidistant table'
                   goto 210
                else
                   itimestep = int(table%timestep)
                   iss = mod(itimestep,100)
                   itimestep = (itimestep - iss)/100
                   imm = mod(itimestep,100)
                   itimestep = (itimestep - imm)/100
                   ihh = mod(itimestep,100)
                   idd = (itimestep - ihh)/100
                   !
                   table%timestep = real(idd,hp) + &
                                  & real(ihh,hp) / 60.0_hp + &
                                  & real(imm,hp) / 1440.0_hp + &
                                  & real(iss,hp) / 86400.0_hp
                   table%timeunitstr = 'days'
                endif
             endif
          endif
          !
          irec = 1
          ipar = 1
          !
          if (iread_phase == 3) then
             !
             ! time column should be treated in a different manner
             !
             if (table%timefunction == 'non-equidistant') then
                ipar = 0
             endif
          endif
          !
          ! store initial ipar value for use on each record line
          !
          ipar0 = ipar
          !
          do while (isdata .and. (.not.feof))
             !
             ! data loop for current table
             !
             if (iread_phase >= 2) then
                TOKEN: do i = 1, ntoken
                   !
                   if (itype(i) == 3) then
                      !
                      ! a string in the table is not always an error since
                      ! in case of time-units 'date' the yyyymmddhhmmss
                      ! value cannot be converted to a single precision
                      ! real or long integer
                      !
                      if (cfield(i)(1:1) == '#' .or. cfield(i)(1:1) == '*' .or. &
                        & cfield(i)(1:1) == ';') then
                         !
                         ! skip comments also in table. The whole remainder of
                         ! the line should be interpreted as a comment, so jump
                         ! out of the token loop
                         !
                         exit TOKEN
                      elseif (ipar == 0 .and. table%timeunitstr == 'date') then
                         !
                         ! okay continue
                         !
                      else
                         write(errorstring,*) 'Unexpected string encountered while reading data'
                         goto 200
                      endif
                   endif
                   !
                   if (iread_phase == 2) then
                      !
                      ! count values only, don't put them in the array since that
                      ! has not yet been allocated
                      !
                      nvalues = nvalues + 1
                      !
                   elseif (ipar == 0) then
                      !
                      ! time column
                      ! table%timefunction = 'non-equidistant'
                      !
                      ! copy contents of time column to times array instead of
                      ! copying it to the values array.
                      !
                      select case(table%timeunitstr)
                      case ('date')
                         !
                         ! yyyymmddhhmmss
                         !
                         ! this value may be indicated as string, integer or
                         ! floating point value depending on the kind settings
                         ! always use the string value cfield(i)
                         !
                         read(cfield(i),'(I8,I2,I2,I2)') iyyyymmdd,ihh,imm,iss
                         call juldat(iyyyymmdd,ijuldate)
                         table%times(irec) = real(ijuldate,hp) + &
                                           & real(ihh,hp) / 60.0_hp + &
                                           & real(imm,hp) / 1440.0_hp + &
                                           & real(iss,hp) / 86400.0_hp
                      case default
                         !
                         ! Insert time into table; itype cannot be 3 since
                         ! that exception has already been caught above
                         !
                         table%times(irec) = real(rfield(i),hp) * table%timeunit
                      end select
                      !
                   else
                      !
                      ! data column
                      !
                      if (ipar == 1) then
                         !
                         ! generate time column when not available from file
                         !
                         if (table%timefunction == 'constant') then
                            table%times(irec) = 0.0_hp
                         elseif (table%timefunction == 'equidistant') then
                            table%times(irec) = real(irec-1,hp) * table%timestep &
                                                              & * table%timeunit
                         endif
                      endif
                      !
                      ! Insert value into table; itype cannot be 3 since
                      ! that exception has already been caught above
                      !
                      table%values(irec,ipar) = rfield(i)
                      !
                   endif
                   !
                   ipar = ipar + 1
                   !
                   ! wrap to next row as needed
                   !
                   if (ipar > table%nparameters) then
                      ipar = ipar0
                      irec = irec + 1
                   endif
                enddo TOKEN
             endif
             !
             ! read new line
             !
             iline = iline+1
             read (lunbcm, '(A)', iostat = istat) line
             if (istat<0) feof = .true.
             !
             ntoken = 0
             call scannr(   line,       1,        len(line),  ntoken,   itype, &
                       &  ifield,  rfield,  cfield,  lenchr,  MAXFLD,  .true., &
                       & .false., .false.)
             !
             if (ntoken<0) goto 200
             !
             !----------------------------------------------
             !  do i = 1, ntoken
             !     if (itype(i) == 1) then
             !        write(*,*) '>',ifield(i),'<'
             !     elseif (itype(i) == 2) then
             !        write(*,*) '>',rfield(i),'<'
             !     elseif (itype(i) == 3) then
             !        write(*,*) '>',cfield(i),'<'
             !     endif
             !  enddo
             !----------------------------------------------
             !
             if (itype(1)==3) isdata = .false.
             !
          enddo
          !
          ! Finish reading table (also if we have reached the end of the file !!)
          !
          if (iread_phase == 2) then
             !
             ! Check for consistency of number of values in table versus
             ! specified number of parameters and records
             !
             ! We don't have to check whether table%nparameters == 0
             ! because this check is effectively carried out at the end of the
             ! keyword loop resulting in the error message "Data encountered
             ! before parameters definition"
             !
             nrecords = nvalues/table%nparameters
             if (mod(nvalues,table%nparameters) /= 0) then
                write(stri1,'(I7)') nvalues
                write(stri2,'(I7)') table%nparameters
                errorstring = 'Number of values (' // trim(adjustl(stri1)) // &
                    & ') in table is not multiple of number of parameters (' // &
                    & trim(adjustl(stri2)) // ')'
                goto 200
             elseif (table%nrecords == -999) then
                table%nrecords = nrecords
             elseif (nrecords /= table%nrecords) then
                write(stri1,'(I7)') nrecords
                write(stri2,'(I7)') table%nrecords
                errorstring = 'Actual number of records (' // trim(adjustl(stri1)) // &
                    & ') in table does not match specified number (' // &
                    & trim(adjustl(stri2)) // ')'
                goto 200
             endif
          endif
          !
       enddo
       !
       select case(iread_phase)
       case(1)
          !
          ! Allocate space for the tables
          !
          ntables = itable
          !
          ! write(*,*) 'NTables = ',ntables
          !
          allocate (this%tables(ntables), stat = istat)
          if (istat /= 0) goto 210
          tables => this%tables
          !
          do itable = 1, ntables
             table => tables(itable)
             !
             table%timestep      = -999.0_hp
             table%geocoords     = -999.0_fp
             table%metriccoords  = -999.0_fp
             table%layer         = -999
             table%refdate       = -999
             table%reftime       = -999.0_fp
             table%nrecords      = -999
             table%nparameters   = 0
             write(table%name,'(A,I4,A)') 'Table',itable,' (nameless)'
             table%contents      = ''
             table%location      = ''
             table%interpolation = 'linear'
             table%extrapolation = 'none'
             table%timeunitstr   = 'minutes'
             table%timeunit      = 1.0_hp / 1440.0_hp
             table%timefunction  = 'non-equidistant'
          enddo
          !
          rewind(lunbcm)
       case(2)
          !
          ! Allocate space for the parameters and data
          !
          do itable = 1, ntables
             table => tables(itable)
             !
             if (table%refdate == -999 .and. table%timeunitstr /= 'date') then
                errorstring = 'Missing reference-date record in table ''' // &
                            & trim(table%name) // ''''
                goto 210
             endif
             !
             if (table%timefunction == 'non-equidistant') then
                !
                ! First column (times) will be stored in separate array
                !
                table%nparameters = table%nparameters - 1
             endif
             !
                             allocate(table%parameters(table%nparameters), stat = istat)
             if (istat == 0) allocate(table%times(table%nrecords), stat = istat)
             if (istat == 0) allocate(table%values(table%nrecords,table%nparameters), stat = istat)
             if (istat /= 0) goto 210
          enddo
          !
          rewind(lunbcm)
       case(3)
          !
          ! File read successfully
          !
          close (lunbcm)
       endselect
       !
       ! End of iread_phase loop
       !
    enddo
    !
    ! Deallocate scannr arrays
    !
    deallocate(itype , stat=ierr)
    deallocate(ifield, stat=ierr)
    deallocate(rfield, stat=ierr)
    deallocate(cfield, stat=ierr)
    deallocate(lenchr, stat=ierr)
    !
    ! File read successfully; don't report an error now ...
    !
    errorstring = ' '
    return
    !
200 continue
    if (ntoken < 0) then
       if (ntoken == -2) then
          errorstring = 'Line parse error: too many sub-fields'
       elseif (ntoken == -3) then
          errorstring = 'Line parse error: sub-field string too long'
       elseif (ntoken == -4) then
          errorstring = 'Line parse error: unmatched quotes'
       else
          errorstring = 'Line parse error'
       endif
    endif
    write(errorstring,'(A,A,A,A,I4,A,A,A,A)') &
       & 'Reading ',trim(filnam),char(10), &
       & 'Line ',iline,': ',trim(line),char(10), &
       & trim(errorstring)
210 continue
    !
end subroutine org_readtable


subroutine org_cleartable(this)
!!--description-----------------------------------------------------------------
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)                    :: this
!
! Local variables
!
    integer                                :: i
    integer                                :: istat
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       !
       tables => this%tables
       !
       do i = 1, size(tables)
          if (associated(tables(i)%parameters))  deallocate(tables(i)%parameters,  STAT = istat)
          if (associated(tables(i)%values))      deallocate(tables(i)%values,      STAT = istat)
       enddo
       deallocate(this%tables,  STAT = istat)
    endif
end subroutine org_cleartable


subroutine org_gettabletimes(this       ,itable     ,times      ,refjulday  , &
                       & errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get all times from the specified table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: refjulday
    real(fp), dimension(*) ,intent(out) :: times
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    real(hp)                 :: datediff
    !
    integer                  :: nrec
    type(tabletype), pointer :: table
!
!! executable statements -------------------------------------------------------
!
    table => this%tables(itable)
    errorstring = ' '
    !
    select case(table%timefunction)
    case ('non-equidistant','equidistant','constant')
       !
       ! note: equidistant and constant tables have been
       ! extended with additional time column in org_readtable
       !
       ! times are stored in days and returned in hours
       !
       datediff = real(refjulday-table%refdate,hp)
       do nrec = 1,table%nrecords
          times(nrec) = real(table%times(nrec)-real(datediff,hp),fp)*24.0_fp
       enddo
    case default
       errorstring = 'Time-function '''// &
          & trim(table%timefunction)//''' not yet implemented,'// &
          & ' please contact code supplier'
       return
    endselect
    !
end subroutine org_gettabletimes
!
!
!===============================================================================
subroutine org_gettabledata_vector(this       ,ivec       ,values     , &
                             & timhr      ,refjulday  ,errorstring,extrapol_in)
!!--description-----------------------------------------------------------------
!
!    Function: Get data from table for specified time
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer, dimension(4)               :: ivec
    integer                ,intent(in)  :: refjulday
    real(fp), optional     ,intent(in)  :: extrapol_in
    real(fp)               ,intent(in)  :: timhr
    real(fp), dimension(:) ,intent(out) :: values
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    real(fp)                  :: extrapol
!
!! executable statements -------------------------------------------------------
!
    if (present(extrapol_in)) then
       extrapol = extrapol_in
    else
       extrapol = 0.0_fp
    endif
    call org_gettabledata_scalar(this       ,ivec(1)    ,ivec(2)    , &
               & ivec(3)    ,ivec(4)    ,values     ,timhr      , &
               & refjulday  ,errorstring,extrapol)
end subroutine org_gettabledata_vector
!
!
!===============================================================================
subroutine org_gettabledata_scalar(this       ,itable     ,ipar       , &
                 & npar       ,irec       ,values     ,timhr      , &
                 & refjulday  ,errorstring,extrapol_in)
!!--description-----------------------------------------------------------------
!
!    Function: Get data from table for specified time
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: ipar
    integer                             :: irec
    integer                ,intent(in)  :: npar
    integer                ,intent(in)  :: refjulday
    real(fp), optional     ,intent(in)  :: extrapol_in
    real(fp)               ,intent(in)  :: timhr
    real(fp), dimension(:) ,intent(out) :: values
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    real(hp)                 :: timreq
    real(hp)                 :: t1
    real(hp)                 :: t2
    !
    real(fp)                 :: alpha
    real(fp)                 :: extrapol
    !
    integer                  :: i
    integer                  :: irec2
    integer                  :: j
    integer                  :: datediff
    !
    logical                  :: inrange
    !
    integer, pointer         :: nrec
    type(tabletype), pointer :: table
!
!! executable statements -------------------------------------------------------
!
    table => this%tables(itable)
    errorstring = ' '
    !
    select case(table%timefunction)
    case ('non-equidistant','equidistant','constant')
       !
       ! Search for times in table (note equidistant and
       ! constant tables have been extended with additional
       ! time column in org_readtable)
       !
       timreq = real(timhr,hp) / 24.0_fp
       if (table%timeunitstr == 'date') then
          timreq = timreq + real(refjulday,hp)
       else
          datediff = refjulday - table%refdate
          timreq   = timreq + real(datediff,hp) - table%reftime
       endif
       nrec => table%nrecords
       !
       inrange = .true.
       !
       ! Check whether the data is in the appropriate range
       ! Use comparereal to make sure that in single precision, small differences are allowed
       !
       if (comparereal(real(timreq,fp), real(table%times(1),fp)) == -1) then
          !
          ! requested time before the first time in the table
          !
          select case(table%extrapolation)
          case ('periodic')
             errorstring = 'Periodic boundary conditions not '// &
                & 'yet implemented, please contact code supplier'
             return
          case ('constant')
             irec  = 1
             do i = 1, npar
                j = ipar + i - 1
                values(i) = table%values(irec,j)
             enddo
             inrange = .false.
          case default
             errorstring = 'Requested time lies before available '// &
                & 'times for '//trim(table%parameters(ipar)%name(1:20))// &
                & ' at '//trim(table%location)
             return
          endselect
       elseif (comparereal(real(timreq,fp), real(table%times(nrec),fp)) == 1) then
          !
          ! requested time beyond the last time in the table
          !
          select case(table%extrapolation)
          case ('periodic')
             errorstring = 'Periodic boundary conditions not '// &
                & 'yet implemented, please contact code supplier'
             return
          case ('constant')
             irec  = nrec
             do i = 1, npar
                j = ipar + i - 1
                values(i) = table%values(irec,j)
             enddo
             inrange = .false.
          case default
                if (present(extrapol_in)) then
                   !
                   ! extrapol_in: interval behind table%times(nrec) where extrapolation is used [hr]
                   !
                   extrapol = extrapol_in / 24.0_fp
                else
                   extrapol = 0.0_fp
                endif
                if (comparereal(real(timreq,fp), real(table%times(nrec),fp)+extrapol) /= 1) then
                   irec  = nrec
                   do i = 1, npar
                      j = ipar + i - 1
                      values(i) = table%values(irec,j)
                   enddo
                   inrange = .false.
                else
                   errorstring = 'Requested time lies beyond available '// &
                      & 'times for '//trim(table%parameters(ipar)%name(1:20))// &
                      & ' at '//trim(table%location)
                   return
                endif
          endselect
       endif
       !
       if (inrange) then
          !
          ! reset search index
          !
          if (table%times(irec) > timreq) irec = 1
          !
          ! search time-interval ...
          !
          do while (irec < nrec)
             if (table%times(irec+1) > timreq) exit
             irec = irec + 1
          enddo
          !
          ! time found
          !
          irec2 = irec + 1
          if (irec == nrec) irec2 = irec
          t1    = table%times(irec)
          t2    = table%times(irec2)
          alpha = real((t2 - timreq) / (t2 - t1),fp)
          !
          do i = 1, npar
             j = ipar + i - 1
             if (table%parameters(j)%interpolation == 'block') then
                !
                ! block interpolation
                !
                values(i) = table%values(irec,j)
             else
                !
                ! linear interpolation
                !
                values(i) = table%values(irec,j) * alpha + &
                          & table%values(irec2,j) * (1.0_fp - alpha)
             endif
          enddo
       endif
    case default
       errorstring = 'Time-function '''// &
          & trim(table%timefunction)//''' not yet implemented,'// &
          & ' please contact code supplier'
       return
    endselect
    !
end subroutine org_gettabledata_scalar
!
!
!===============================================================================
subroutine org_gettable_vector(this      ,location  ,parname   ,ivec      , &
                         & nparmin   ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Find table for specified location and quantity
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer, dimension(4)  ,intent(out) :: ivec
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
!! executable statements -------------------------------------------------------
!
    call org_gettable(this      ,location  ,parname   ,ivec(1)   , &
                & ivec(2)   ,ivec(3)   ,nparmin   ,errorstring)
    ivec(4) = 1
end subroutine org_gettable_vector
!
!
!===============================================================================
subroutine org_gettable_scalar(this      ,location  ,parname   ,itable    , &
                         & ipar      ,npar      ,nparmin   ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Find table for specified location and quantity
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                ,intent(out) :: itable
    integer                ,intent(out) :: ipar
    integer                ,intent(out) :: npar
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    type(tablefiletype)    ,intent(in)  :: this
    character(256)         ,intent(out) :: errorstring
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    integer                                :: lpn
    !
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    tables => this%tables
    errorstring = ' '
    itable = -999
    !
    lpn = min(20,len(parname))
    !
    loop_tables: do i = 1, size(tables)
       if (tables(i)%location == location) then
          !
          do j = 1, tables(i)%nparameters
             if (tables(i)%parameters(j)%name(1:lpn) == parname(1:lpn)) then
                itable = i
                ipar = j
                exit loop_tables
             endif
          enddo
       endif
    enddo loop_tables
    !
    if (itable < 0) then
       npar = 0
       if (npar < nparmin) then
          errorstring = 'Missing ''' // trim(parname) // &
             & ''' data for location ''' // trim(location) // &
             & ''' in ' // trim(this%filename)
          return
       else
          return
       endif
    endif
    !
    j = ipar
    do while (tables(itable)%parameters(j)%name(1:lpn) == parname(1:lpn))
       j = j + 1
       if (j > tables(itable)%nparameters) exit
    enddo
    npar = j - ipar
    !
end subroutine org_gettable_scalar


subroutine org_checktable(this      ,itable    ,ipar      , &
                    & npar      ,chktyp    ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Check whether all values in table are
!              * positive              if CHKTAB_POSITIVE
!              * logical               if CHKTAB_LOGICAL
!              * block-wise specified  if CHKTAB_BLOCK
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    integer                      ,intent(in)  :: chktyp
    type(tablefiletype)          ,intent(in)  :: this
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    !
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    tables => this%tables
    errorstring = ' '
    !
    do j = ipar, ipar + npar - 1
       if ((iand(chktyp,CHKTAB_LOGICAL)==1 .or. &
         &  iand(chktyp,CHKTAB_BLOCK)==1).and. &
         & tables(itable)%parameters(j)%interpolation /= 'block') then
          errorstring = 'Interpolation method should be "block" for ''' // &
             & trim(tables(itable)%parameters(j)%name) // ''' at location ''' // &
             & trim(tables(itable)%location) // ''' in ' // &
             & trim(this%filename)
          return
       endif
       do i = 1, tables(itable)%nrecords
          if (iand(chktyp,CHKTAB_POSITIVE)==1 .and. &
             & tables(itable)%values(i,j) < 0.0) then
             errorstring = 'Negative value encountered for ''' // &
                & trim(tables(itable)%parameters(j)%name) // ''' at location ''' // &
                & trim(tables(itable)%location) // ''' in ' // &
                & trim(this%filename)
             return
          endif
          if (iand(chktyp,CHKTAB_LOGICAL)==1 .and. &
            & comparereal(tables(itable)%values(i,j),0.0_fp) /= 0 .and. &
            & comparereal(tables(itable)%values(i,j),1.0_fp) /= 0) then
             errorstring = 'Not all values are 0 or 1 for ''' // &
                & trim(tables(itable)%parameters(j)%name) // ''' at location ''' // &
                & trim(tables(itable)%location) // ''' in ' // &
                & trim(this%filename)
             return
          endif
       enddo
    enddo
    !
end subroutine org_checktable
!
!
!===============================================================================
subroutine org_checktableparnames(this      ,parnames  ,itable    , &
                            & ipar      ,npar      ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Check parameter names in table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    character(*), dimension(npar),intent(in)  :: parnames
    type(tablefiletype)          ,intent(in)  :: this
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    integer                                :: i
    integer                                :: j
    !
    type(tabletype), dimension(:), pointer :: tables
!
!! executable statements -------------------------------------------------------
!
    tables => this%tables
    errorstring = ' '
    !
    i = 0
    do j = ipar, ipar + npar - 1
       i = i + 1
       if (tables(itable)%parameters(j)%name /= parnames(i)) then
          errorstring = 'Expected ''' // trim(parnames(i)) // &
             & ''' but found ''' // trim(tables(itable)%parameters(j)%name) // &
             & ''' for location ''' // trim(tables(itable)%location) // &
             & ''' in ' // trim(this%filename)
          return
       endif
    enddo
    !
end subroutine org_checktableparnames
!
!
!===============================================================================
character(256) function org_getfilename(this    )
!!--description-----------------------------------------------------------------
!
!    Function: Get the name of the file
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    org_getfilename = this%filename
end function org_getfilename
!
!
!===============================================================================
integer function org_getntables(this    ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       errorstring = ' '
       org_getntables = size(this%tables)
    else
       errorstring = 'Tables not yet initialised'
       org_getntables = 0
    endif
end function org_getntables
!
!
!===============================================================================
character(MAXTABLECLENGTH) function org_gettablelocation(this    ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get location name of table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       if (itable<=size(this%tables)) then
          errorstring = ' '
          org_gettablelocation = this%tables(itable)%location
       else
          errorstring = 'Table index is too large'
          org_gettablelocation = ' '
       endif
    else
       errorstring = 'Tables not yet initialised'
       org_gettablelocation = ' '
    endif
end function org_gettablelocation
!
!
!===============================================================================
integer function org_gettablentimes(this    ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of times in table
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletype)          ,intent(in)  :: this
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(this%tables)) then
       if (itable<=size(this%tables)) then
          errorstring = ' '
          org_gettablentimes = this%tables(itable)%nrecords
       else
          errorstring = 'Table index is too large'
          org_gettablentimes = 0
       endif
    else
       errorstring = 'Tables not yet initialised'
       org_gettablentimes = 0
    endif
end function org_gettablentimes

end module tables
