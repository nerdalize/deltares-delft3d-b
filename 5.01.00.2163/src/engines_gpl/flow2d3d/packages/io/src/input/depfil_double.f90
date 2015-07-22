subroutine depfil_double(lundia    ,error     ,fildep    ,fmttmp    ,mmax      , &
                       & nmaxus    ,array     ,nfld      ,ifld      ,gdp       )
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
!  $Id: depfil_double.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/depfil_double.f90 $
!!--description----------------------------------------------------------------- 
! 
!    Function: Reads the depth values from the attribute file 
! Method used: 
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision 
    use globaldata 
    use dfparall 
    ! 
    implicit none 
    ! 
    type(globdat),target :: gdp 
    ! 
    ! The following list of pointer parameters is used to point inside the gdp structure 
    ! 
    integer, pointer :: mfg 
    integer, pointer :: mlg 
    integer, pointer :: nfg 
    integer, pointer :: nlg 
    integer, pointer :: mmaxgl 
    integer, pointer :: nmaxgl 
! 
! Global variables 
! 
    integer                                                            , intent(in)  :: ifld   !  index of field to be read
    integer                                                                          :: lundia !  unit number for diagnostic file
    integer                                                            , intent(in)  :: mmax   !  maximum internal grid index in m dimension
    integer                                                            , intent(in)  :: nfld   !  number of fields
    integer                                                            , intent(in)  :: nmaxus !  maximum internal grid index in n dimension
    logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
    real(hp), dimension(nfld, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: array  !  data array to fill
    character(*)                                                                     :: fildep !  Name of the relevant file 
    character(11)                                                      , intent(in)  :: fmttmp !  Fornat switch for the attribute file 
! 
! Local variables 
! 
    integer                               :: iocond ! Help variable for iostat condition  
    integer                               :: lfile  ! Length of file name  
    integer                               :: luntmp ! Unit number for attribute file  
    integer                               :: m 
    integer                               :: n 
    integer                 , external    :: newlun
    real(hp), dimension(:,:), allocatable :: dtmp   ! Temporary array containing dp of entire domain 
    logical                 , external    :: exifil
    character(300)                        :: errmsg ! Character string containing the errormessage to be written to file. The message depends on the error.  
! 
!! executable statements ------------------------------------------------------- 
! 
    mfg    => gdp%gdparall%mfg 
    mlg    => gdp%gdparall%mlg 
    nfg    => gdp%gdparall%nfg 
    nlg    => gdp%gdparall%nlg 
    mmaxgl => gdp%gdparall%mmaxgl 
    nmaxgl => gdp%gdparall%nmaxgl 
    !
    error = .false.
    ! 
    ! Test file existence, if so read 
    ! 
    lfile = len(fildep) 
    if (exifil(fildep(1:lfile), lundia, 'G004', gdp)) then 
       ! 
       ! File exists 
       ! 
       ! 
       ! allocate temporary array to store data of entire domain read from file 
       ! 
       allocate (dtmp(nmaxgl,mmaxgl)) 
       ! 
       ! the master opens and reads the depth file 
       ! 
       if ( inode /= master ) goto 10 
       ! 
       luntmp = newlun(gdp) 
       open (luntmp, file = fildep(1:lfile), form = fmttmp, status = 'old') 
       ! 
       if (fmttmp(1:2) == 'un') then 
          ! 
          ! unformatted file 
          ! read per nmaxus, mmax values in dtmp array 
          ! NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain) 
          !       in case of parallel runs. Moreover, array is associated with subdomain and 
          !       therefore, data for entire domain is stored in temporary array dtmp 
          ! end of error in file = not ok 
          ! 
          do n = 1, nmaxgl 
             read (luntmp, iostat = iocond) (dtmp(n, m), m = 1, mmaxgl) 
             if (iocond /= 0) then 
                if (iocond < 0) then 
                   call prterr(lundia, 'G006', fildep(1:lfile))
                else 
                   call prterr(lundia, 'G007', fildep(1:lfile))
                endif 
                error = .true. 
                goto 9999 
             endif 
             ! 
             ! If a NaN is read -> error
             ! 
             do m = 1, mmax 
                if ( isnan(dtmp(n, m)) ) then  
                    write(errmsg,'(2a)') 'NaN found in file ', fildep(1:lfile) 
                    call prterr(lundia, 'P004', errmsg)
                    error = .true. 
                    goto 9999 
                endif 
            enddo 
          enddo 
       else 
          ! 
          ! Freeformatted file 
          ! Skip lines starting with a '*' 
          ! 
          call skipstarlines(luntmp) 
          ! 
          ! Read per nmaxus, mmax values in dtmp array 
          ! End of error in file = not ok 
          ! 
          do n = 1, nmaxgl 
             read (luntmp, *, iostat = iocond) (dtmp(n, m), m = 1, mmaxgl) 
             if (iocond /= 0) then 
                if (iocond < 0) then 
                   call prterr(lundia, 'G006', fildep(1:lfile))
                else 
                   call prterr(lundia, 'G007', fildep(1:lfile))
                endif 
                error = .true. 
                goto 9999 
             endif 
             ! 
             ! If a NaN is read -> error
             ! 
             do m = 1, mmax 
                if ( isnan(dtmp(n, m)) ) then  
                    write(errmsg,'(2a)') 'NaN found in file ', fildep(1:lfile) 
                    call prterr(lundia, 'P004', errmsg)
                    error = .true. 
                    goto 9999 
                endif 
            enddo 
          enddo 
       endif 
       ! 
       ! Stop reading file 
       ! 
       9999  continue 
       ! 
       ! Close file 
       ! 
       close (luntmp) 
 10    continue 
       ! 
       ! check whether something went wrong with reading file 
       ! 
       call dfbroadc ( iocond, 1, dfint, gdp ) 
       ! 
       if (iocond /= 0) then 
          if (iocond < 0) then 
             call prterr(lundia    ,'G006'    ,fildep(1:lfile)      )
          else 
             call prterr(lundia    ,'G007'    ,fildep(1:lfile)      )
          endif 
          error = .true. 
       else 
          ! 
          ! scatter array dtmp to all nodes 
          ! 
          call dfbroadc ( dtmp, nmaxgl*mmaxgl, dfdble, gdp ) 
          ! 
          ! put copies of parts of dtmp for each subdomain 
          ! 
          do m = mfg, mlg 
             do n = nfg, nlg 
                array(ifld, n-nfg+1, m-mfg+1) = dtmp(n, m) 
             enddo 
          enddo 
       endif 
       ! 
       deallocate(dtmp) 
    else 
       ! 
       ! File does not exist 
       ! Exifil has produced a nice error message 
       ! 
       call d3stop(1, gdp) 
    endif 
end subroutine depfil_double 
