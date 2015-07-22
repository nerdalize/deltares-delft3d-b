subroutine dimfou(lunmd     ,lundia    ,error     ,nrrec     ,noui      , &
                & filfou    ,nofou     ,gdp       )
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
!  $Id: dimfou.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/dimfou.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Reads the dimension for fourier analysis output
!              the MD-file or from the attribute file for NOUI
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
    integer , pointer :: itis
!
! Global variables
!
    integer                   :: lundia !  Description and declaration in inout.igs
    integer                   :: lunmd  !  Description and declaration in inout.igs
    integer                   :: nofou  !  Description and declaration in dimens.igs
    integer                   :: nrrec  !!  Record counter keeping the track of the last record read
    logical                   :: error  !!  Flag=TRUE if an error is encountered
    logical      , intent(in) :: noui   !!  Flag true if program calling routine is not User Interface
    character(*)              :: filfou !!  Attribute file name for monitoring stations
!
! Local variables
!
    integer            :: lenc    ! Number of char. to read in string 
    integer            :: lfile   ! Number of non blank characters of file name 
    integer            :: luntmp  ! Unit number of FILFOU 
    integer, external  :: newlun
    integer            :: nlook   ! Nr. of values to look for in a record 
    integer            :: ntrec   ! Current record counter. It's value is changed to detect if all records in the MD-file have been read 
    logical, external  :: exifil
    logical            :: found   ! Flag is true if KEYWRD is found 
    logical            :: lerror  ! Flag=TRUE if an local error is encountered For NOUI this can mean error will be set TRUE 
    logical            :: newkw   ! Flag to specify if the keyword to look for is a new keyword 
    character(11)      :: fmtdef  ! Default format of an attribute file = blank 
    character(11)      :: fmttmp  ! Format of FILFOU (UN/FRee formatted) 
    character(12)      :: fildef  ! Default file name = blank 
    character(300)     :: mdfrec  ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)       :: keyw    ! Keyword to look for in the MD-file 
!
!
!! executable statements -------------------------------------------------------
!
!
    itis  => gdp%gdrdpara%itis
    !
    mdfrec = ' '
    luntmp = 88
    fmttmp = ' '
    fildef = ' '
    fmtdef = 'FRformatted'
    lfile  = 12
    nlook  = 1
    lerror = .false.
    newkw  = .true.
    found  = .true.
    !
    ! locate 'Filfou' record for Fourier analysis
    !
    keyw = 'Filfou'
    newkw = .true.
    ntrec = nrrec
    lenc = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lenc      , &
              & 'NO'      )
    !
    ! not found ?
    !
    if (found) then
       nlook = 1
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,filfou    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          if (noui) then
             error = .true.
             goto 9999
          endif
          lerror = .false.
          filfou = fildef
       endif
    endif
    !
    ! skip reading from attribute file for UI
    !
    if (.not.noui) goto 9999
    !
    ! Quantities for Fourier analysis in file
    !
    if (filfou/=fildef) then
       !
       ! define format type
       !
       fmttmp = fmtdef(3:)
       !
       ! test file existence
       !
       lfile = len(filfou)
       !
       if (exifil(filfou(1:lfile), lundia, 'G004', gdp)) then
          !
          ! open input file
          !
          luntmp = newlun(gdp)
          open (luntmp, file = filfou(1:lfile), form = fmttmp, status = 'old')
          !
          ! read data from attribute file
          !
          call fouini(lundia    ,luntmp    ,error     ,nofou     ,gdp       )
          !
          close (luntmp)
       else
          !
          ! file does not exist !!
          !
          error = .true.
       endif
    endif
    !
 9999 continue
end subroutine dimfou
