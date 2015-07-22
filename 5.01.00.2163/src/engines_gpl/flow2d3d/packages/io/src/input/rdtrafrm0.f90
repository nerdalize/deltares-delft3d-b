subroutine rdtrafrm0(error, iform     , npar        , par, flname, &
                   & name , dll_handle, dll_function,dll_usrfil  , gdp)
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
!  $Id: rdtrafrm0.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdtrafrm0.f90 $
!!--description-----------------------------------------------------------------
!
! Reads transport formula and parameters
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lundia
!
! Global variables
!
    logical                      , intent(out) :: error
    integer                                    :: iform
    integer                      , intent(in)  :: npar
    integer(pntrsize)                          :: dll_handle
    real(fp)    , dimension(npar)              :: par
    character(*)                               :: flname
    character(*)                               :: name
    character(*)                               :: dll_function
    character(*)                               :: dll_usrfil
!
! Local variables
!
    integer                     :: i
    integer                     :: inp
    integer                     :: iost
    integer                     :: istat
    integer(pntrsize)           :: istat_ptr
    integer                     :: lfile
    integer          , external :: newlun
    integer(pntrsize), external :: open_shared_library
    logical                     :: lex
    character(3)                :: key
    character(10)               :: versionstring
    character(80)               :: string
    character(256)              :: errmsg
    character(256)              :: rec
    type(tree_data)  , pointer  :: tran_ptr
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    write (lundia, '(a,a)') '    Input file                   : ',trim(flname)
    call noextspaces(flname, lfile)
    !
    inquire (file = flname(1:lfile), exist = lex)
    if (lex) then

       !
       ! Create TransportFormula branch in input tree
       !
       call tree_create_node( gdp%input_tree, 'TransportFormula Input', tran_ptr )
       call tree_put_data( tran_ptr, transfer(trim(flname),node_value), 'STRING' )
       !
       ! Put trafrm-file in input tree
       !
       call prop_file('ini',trim(flname),tran_ptr,istat)
       if (istat /= 0) then
          select case (istat)
          case(1)
             call prterr(lundia, 'G004', trim(flname))
          case(3)
             call prterr(lundia, 'G006', trim(flname))
          case default
             call prterr(lundia, 'G007', trim(flname))
          endselect
          call d3stop(1, gdp)
       endif
       !
       ! Check version number of trafrm-file
       !
       versionstring = '00.00'
       call prop_get_string(tran_ptr,'TransportFormulaFileInformation','FileVersion',versionstring)
       if (trim(versionstring) == '01.00') then
          write (lundia, '(a,a)') '    Versionnumber of input file  : ', trim(versionstring)
          name = ' '
          call prop_get_string(tran_ptr,'TransportFormula','Name',name)
          write (lundia, '(a,a)') '    Formula name                 : ',trim(name)
          rec  = ' '
          call prop_get_string(tran_ptr,'TransportFormula','DLL',rec)
          if (rec /= ' ') then
             iform = 15
             if (gdp%arch == 'win32') then
                rec(len_trim(rec)+1:) = '.dll'
             else
                rec(len_trim(rec)+1:) = '.so'
             endif
             write (lundia, '(a,a)') '    Dynamic library              : ',trim(rec)
             !
             ! Get handle to the DLL
             !
             istat_ptr = 0
             istat_ptr = open_shared_library(dll_handle, rec)
             if (istat_ptr /= 0) then
                write(errmsg,'(a,a)') 'Can not open shared library ', trim(rec)
                call prterr(lundia, 'P004', trim(errmsg))
                call d3stop(1, gdp)
             endif
             dll_function = ' '
             call prop_get_string(tran_ptr,'TransportFormula','function',dll_function)
             write (lundia, '(a,a)') '    Function in DLL              : ',trim(dll_function)
             !
             dll_usrfil = ' '
             call prop_get_string(tran_ptr,'TransportFormula','InputFile',dll_usrfil)
             if (dll_usrfil /= ' ') then
                write (lundia, '(a,a)') '    Input file for function      : ',trim(dll_usrfil)
             endif
          else
             write (lundia, '(a,a)') '    Using default formula'
             call prterr(lundia, 'P004', 'Reading of ini-file formatted input for default transport formulas is not implemented yet')
             call d3stop(1, gdp)
          endif
       else
          ! Disabled confusing (but correct) message
          ! write (lundia, '(a)') '    No versionnumber found in input file.'
          ! write (lundia, '(a)') '         Assuming version 00.00: traditional MOR-format without keywords.'
          write (lundia, '(a)') '    Inputfile in traditional MOR-format.'
          inp = newlun(gdp)
          open (inp, file = flname(1:lfile),status = 'old', iostat = iost)
          if (iost/=0) then
             call prterr(lundia, 'G004', flname(1:lfile))
             call d3stop(1, gdp)
          endif
          string = ' '
          read (inp, '(a)', iostat = iost) string
          do while (index(string, 'IFORM')==0 .and. iost==0)
             read (inp, '(a)', iostat = iost) string
          enddo
          backspace (inp)
          read (inp, *) iform
          write (lundia, '(a,i3)') '    Transport formula used =', iform
          if (iform>0) then
             if (iform<10) then
                write (key, '(a,i1)') '#', iform
             else
                write (key, '(a,i2)') '#', iform
             endif
             read (inp, '(a)', iostat = iost) string
             do while (index(string, key)==0 .and. iost==0)
                read (inp, '(a)', iostat = iost) string
             enddo
             if (iost/=0) then
                call prterr(lundia, 'U021', 'Transport formula parameters not found')
                call d3stop(1, gdp)
             endif
             write (lundia, '(a)') '    Parameters'
             if (iform==1) then
                do i = 11, 12
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
                read(inp,*,iostat=iost) par(13)
                if (iost/=0) then
                   par(13)=0.0_fp
                endif
             elseif (iform==2) then
                do i = 11, 12
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==3) then
                do i = 11, 12
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==4) then
                do i = 11, 15
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==5) then
                do i = 11, 19
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==6) then
                do i = 11, 20
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==7) then
                do i = 11, 14
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==8) then
                do i = 11, 24
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==10) then
                do i = 11, 13
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==11) then
                do i = 11, 13
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==12) then
                do i = 11, 13
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==13) then
                do i = 11, 13
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==14) then
                do i = 11, 15
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==15) then
                write (lundia, '(a,a)') '    Using Dynamic library'
                call prterr(lundia, 'P004', 'Transport formula in dynamic library is only possible when the trafrm-file is in ini-file format.')
                call d3stop(1, gdp)
             elseif (iform==16) then
                do i = 11, 11
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==17) then
                do i = 11, 12
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             elseif (iform==18) then
                do i = 11, 12
                   read (inp, *) par(i)
                   write (lundia, *) '        ',par(i)
                enddo
             else
                error  = .true.
                errmsg = 'Transport formula number is not implemented'
                call prterr(lundia    ,'U021'    ,errmsg    )
             endif
          endif
          close (inp)
       endif
    else
       errmsg = 'TraFrm file '//flname(1:lfile)//' does not exist'
       call prterr(lundia    ,'U021'    ,errmsg    )
       call d3stop(1, gdp)
       goto 9999
    endif
    !
 9999 continue
end subroutine rdtrafrm0
