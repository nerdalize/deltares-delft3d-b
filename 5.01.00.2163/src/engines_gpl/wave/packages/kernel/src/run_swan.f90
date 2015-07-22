subroutine run_swan (casl)
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
!  $Id: run_swan.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/kernel/src/run_swan.f90 $
!!--description-----------------------------------------------------------------
!
!     *** Run swan; produce output file swanout with values on     ***
!     *** swan computational grid                                  ***
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use swan_input
    !
    implicit none
!
! Global variables
!
    integer                 :: ind
    integer                 :: strlen
    integer                 :: ncasl 
    integer                 :: nuerr
    logical                 :: ex
    character(*)            :: casl
    character(4)            :: labl     
    character(4)            :: copy
    character(5)            :: prints
    character(6)            :: append
    character(8)            :: swninp
    character(256)          :: wvsswn
    character(256)          :: string
    character(256)          :: swanCommand
!
!! executable statements -------------------------------------------------------
!
    wvsswn = ' '
    labl   = ' '
    swninp = 'swan.inp'
    write (wvsswn, '(a)') trim(casl)
    ind    = index( wvsswn, ' ')
    ncasl  = ind - 1
    if (ncasl == 0) then
       ncasl = 3
    endif
    wvsswn(ind:) = '.swn'
    copy = 'copy'
    call cp_file( swninp, wvsswn, copy, nuerr)
    if (nuerr > 0) then
       write (*, '(a,i3)') '*** ERROR: While copying swan.inp to waves.swn, errorcode:', nuerr
       stop
    endif
    !
    if (arch == 'linux') then
       write(swanCommand, '(3a)') 'swan.sh', ' ', trim(casl)
       !
       ! SWAN execution
       !
       ! In debug mode, util_system wants spaces at the end...
       !
       call util_system(swanCommand(1:len_trim(swanCommand)+5))
       inquire (file = 'norm_end', exist = ex)
       if (.not. ex) then
          write (*,'(a)') '*** WARNING: unable to run SWAN using "swan.sh". Trying with "swan.bat" ...'
          write(swanCommand,'(3a)') 'swan.bat', ' ', trim(casl)
          !
          ! SWAN execution
          !
          ! In debug mode, util_system wants spaces at the end...
          !
          call util_system(swanCommand(1:len_trim(swanCommand)+5))
       endif
    else
       write(swanCommand,'(3a)') 'swan.bat', ' ', trim(casl)
       !
       ! SWAN execution
       !
       ! In debug mode, util_system wants spaces at the end...
       !
       call util_system(swanCommand(1:len_trim(swanCommand)+5))
    endif
    write(*,'(a)')'>>...End of SWAN run'
    !
    ! Check SWAN output file norm_end
    !
    inquire (file = 'norm_end', exist = ex)
    if (.not. ex) then
       write (*,'(a)') '*** ERROR: file ''norm_end'' expected to signal a correct SWAN calculation'
       stop
    endif
    !
    ! Check SWAN output file PRINT
    !
    call scan_fl(swan_run%checkVersionNumber, swan_run%versionNumberOK)
    prints        = 'PRINT'
    string(1:9)  = 'swn-diag.'
    string(10:)  = casl
    ind = index(string, ' ')
    string(ind:) = labl
    append       = 'append'
    call cp_file(prints     ,string    ,append    ,nuerr             )
    if (nuerr > 0) then
       write (*, '(a,i3)') '*** ERROR: While appending PRINT to diag file'
    endif
    !
    ! Remove SWAN input/output/tmp files (except SWANOUT output data file)
    !
    call rm_del('norm_end')
    call rm_del('INPUT')
    call rm_del('PRINT')
    call rm_del('source')
    call rm_del('temp')
    call rm_del('swaninit')
    call rm_del('INSTR')
    call rm_del('INSTU')
    call rm_del('BOTNOW')
    call rm_del('CURNOW')
    string = casl(1:ncasl) // '.swn'
    call rm_del(string)
    string = casl(1:ncasl) // '.prt'
    call rm_del(string)
    string = 'fname'
    call rm_del(string) 
end subroutine run_swan
