module table_handles
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
!  $Id: table_handles.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/data/src/table_handles.f90 $
!!--description-----------------------------------------------------------------
!
! Handle wrapper for tables module.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_sp
    use tables
    use handles
!
!! -----------------------------------------------------------------------------
!
    private
    public handletype
    public readtable
    public cleartable
    public getntables
    public gettable
    public checktable
    public checktableparnames
    public gettablelocation
    public gettablentimes
    public gettabletimes
    public gettabledata
    public getfilename
    public validtable
    !
    public MAXTABLECLENGTH

    public CHKTAB_PARNAME
    public CHKTAB_POSITIVE
    public CHKTAB_BLOCK
    public CHKTAB_LOGICAL
!
!! -----------------------------------------------------------------------------
!
    interface gettable
       module procedure gettable_vector, gettable_scalar
    end interface

    interface gettabledata
       module procedure gettabledata_vector, gettabledata_scalar
    end interface

    type tablefiletypehandle
       type(tablefiletype), pointer :: this => NULL()
       integer :: htype = -999
    endtype tablefiletypehandle

    integer :: htype = -999

contains

subroutine readtable(handle, lunbcm, filnam, refjulday, errorstring)
!
! Global variables
!
    integer            ,intent(in)  :: lunbcm
    integer            ,intent(in)  :: refjulday
    character(*)       ,intent(in)  :: filnam
    type(handletype)                :: handle
    character(256)     ,intent(out) :: errorstring
!
! Local variables
!
    integer                         :: istat
    type(tablefiletypehandle)       :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    call newtablehandle(tablehandle,errorstring)
    if (errorstring /= '') return
    !
    if (.not.associated(tablehandle%this)) then
       istat = 0
       allocate(tablehandle%this, stat = istat)
       if (istat /= 0) then
          errorstring = 'GETDATAFILE: Memory allocation error'
          return
       endif
    endif
    call org_readtable(tablehandle%this, lunbcm, filnam, refjulday, errorstring)
    if (errorstring /= '') return
    handle = cast_from_tablehandle(tablehandle)
    !
end subroutine readtable


subroutine cleartable(handle)
!!--description-----------------------------------------------------------------
!
!    Function: Deallocate data object
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
!
! Local variables
!
    type(tablefiletypehandle)            :: tablehandle
    integer                              :: istat
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle)) return
    tablehandle = cast_to_tablehandle(handle)
    if (associated(tablehandle%this)) then
       call org_cleartable(tablehandle%this)
       deallocate(tablehandle%this, stat = istat)
    endif
    handle = cast_from_tablehandle(tablehandle)
end subroutine cleartable


subroutine gettable_vector(handle    ,location  ,parname   ,ivec      , &
                         & nparmin   ,errorstring)
!
! Global variables
!
    integer, dimension(4)  ,intent(out) :: ivec
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    call gettable_scalar(handle    ,location  ,parname   , &
                       & ivec(1)    ,ivec(2)   ,ivec(3)   ,nparmin   , &
                       & errorstring)
    ivec(4) = 1
    !
end subroutine gettable_vector


subroutine gettable_scalar(handle    ,location  ,parname   ,itable    , &
                         & ipar      ,npar      ,nparmin   ,errorstring)
!
! Global variables
!
    integer                ,intent(out) :: itable
    integer                ,intent(out) :: ipar
    integer                ,intent(out) :: npar
    integer                ,intent(in)  :: nparmin
    character(*)           ,intent(in)  :: location
    character(*)           ,intent(in)  :: parname
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTable call before initialisation'
    else
       call org_gettable(tablehandle%this      ,location  ,parname   , &
                       & itable     ,ipar      ,npar      ,nparmin   , &
                       & errorstring)
    endif
    !
end subroutine gettable_scalar

subroutine gettabletimes(handle     ,itable     ,times      ,refjulday  , &
                       & errorstring)
!
! Global variables
!
    integer                ,intent(in)  :: itable
    integer                ,intent(in)  :: refjulday
    real(fp), dimension(*) ,intent(out) :: times
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableTimes call before initialisation'
    else
       call org_gettabletimes(tablehandle%this       ,itable     ,times      , &
                            & refjulday  ,errorstring)
    endif
    !
end subroutine gettabletimes


subroutine gettabledata_vector(handle     ,ivec       ,values     , &
                             & timhr      ,refjulday  ,errorstring, extrapol_in)
!
! Global variables
!
    integer, dimension(4)               :: ivec
    integer                ,intent(in)  :: refjulday
    real(fp), optional     ,intent(in)  :: extrapol_in
    real(fp)               ,intent(in)  :: timhr
    real(fp), dimension(:) ,intent(out) :: values
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
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
    call gettabledata_scalar(handle     ,ivec(1)    ,ivec(2)    , &
               & ivec(3)    ,ivec(4)    ,values     ,timhr      , &
               & refjulday  ,errorstring,extrapol   )
    !
end subroutine gettabledata_vector


subroutine gettabledata_scalar(handle     ,itable     ,ipar       , &
                 & npar       ,irec       ,values     ,timhr      , &
                 & refjulday  ,errorstring,extrapol_in)
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
    character(256)         ,intent(out) :: errorstring
    type(handletype)       ,intent(in)  :: handle
!
! Local variables
!
    real(fp)                  :: extrapol
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableData call before initialisation'
    else
       if (present(extrapol_in)) then
          extrapol = extrapol_in
       else
          extrapol = 0.0_fp
       endif
       call org_gettabledata(tablehandle%this       ,itable     ,ipar       , &
                           & npar       ,irec       ,values     ,timhr      , &
                           & refjulday  ,errorstring,extrapol   )
    endif
    !
end subroutine gettabledata_scalar

subroutine checktable(handle    ,itable    ,ipar      , &
                    & npar      ,chktyp    ,errorstring       )
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    integer                      ,intent(in)  :: chktyp
    character(256)         ,intent(out) :: errorstring
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'CheckTable call before initialisation'
    else
       call org_checktable(tablehandle%this        ,itable    ,ipar      , &
                         & npar      ,chktyp    ,errorstring)
    endif
    !
end subroutine checktable


subroutine checktableparnames(handle    ,parnames  ,itable    , &
                            & ipar      ,npar      ,errorstring       )
!
! Global variables
!
    integer                      ,intent(in)  :: itable
    integer                      ,intent(in)  :: ipar
    integer                      ,intent(in)  :: npar
    character(*), dimension(npar),intent(in)  :: parnames
    character(256)               ,intent(out) :: errorstring
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'CheckTableParNames call before initialisation'
    else
       call org_checktableparnames(tablehandle%this        ,parnames  ,itable    , &
                                 & ipar      ,npar      ,errorstring)
    endif
    !
end subroutine checktableparnames


character(256) function getfilename(handle    )
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle,getfilename)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       getfilename = 'NOT YET INITIALISED'
    else
       getfilename = org_getfilename(tablehandle%this)
    endif
    !
end function getfilename


integer function getntables(handle  ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetNTables call before initialisation'
    else
       getntables = org_getntables(tablehandle%this ,errorstring)
    endif
end function getntables


character(MAXTABLECLENGTH) function gettablelocation(handle  ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableLocation call before initialisation'
    else
       gettablelocation = org_gettablelocation(tablehandle%this ,itable, errorstring)
    endif
end function gettablelocation


integer function gettablentimes(handle  ,itable     ,errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Get the number of tables
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)             ,intent(in)  :: handle
    integer                      ,intent(in)  :: itable
    character(256)               ,intent(out) :: errorstring
!
! Local variables
!
    type(tablefiletypehandle)           :: tablehandle
!
!! executable statements -------------------------------------------------------
!
    if (.not.validtable(handle, errorstring)) return
    tablehandle = cast_to_tablehandle(handle)
    if (.not.associated(tablehandle%this)) then
       errorstring = 'GetTableNTimes call before initialisation'
    else
       gettablentimes = org_gettablentimes(tablehandle%this ,itable, errorstring)
    endif
end function gettablentimes


subroutine newtablehandle(tablehandle, errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Create a new table handle object
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(tablefiletypehandle) ,intent(out) :: tablehandle
    character(256)            ,intent(out) :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    errorstring = ''
    if (htype<0) then
       htype = registerhandletype('TableFile')
       if (htype<0) then
          errorstring = 'Unable to register TableFile handle'
          return
       endif
    endif
    nullify(tablehandle%this)
    tablehandle%htype = htype
end subroutine newtablehandle


logical function validtable(handle, errorstring)
!!--description-----------------------------------------------------------------
!
!    Function: Verify whether handle is a tablefiletypehandle
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
    character(256),intent(out), optional :: errorstring
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (htype<0) then
       validtable = .false.
    else
       validtable = handle%htype == htype
    endif
    if (present(errorstring)) then
       if (.not.validtable) then
          errorstring = 'INVALID TableFile handle'
       else
          errorstring = ''
       endif
    endif
end function validtable


function cast_to_tablehandle(handle) result (tablehandle)
!!--description-----------------------------------------------------------------
!
!    Function: Cast from handle to tablefiletypehandle
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
    type(tablefiletypehandle)            :: tablehandle
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    tablehandle = transfer(handle,tablehandle)
end function cast_to_tablehandle


function cast_from_tablehandle(tablehandle) result (handle)
!!--description-----------------------------------------------------------------
!
!    Function: Cast from tablefiletypehandle to handle
!
!!------------------------------------------------------------------------------
!
! Global variables
!
    type(handletype)                     :: handle
    type(tablefiletypehandle)            :: tablehandle
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    handle = transfer(tablehandle,handle)
end function cast_from_tablehandle

end module table_handles
