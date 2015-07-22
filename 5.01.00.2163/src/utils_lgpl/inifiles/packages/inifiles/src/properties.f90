module properties
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: properties.f90 2063 2012-12-17 22:52:39Z baart_f $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/inifiles/packages/inifiles/src/properties.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use tree_structures
    use inifiles_version_module
    !
    implicit none
    !
    integer, private, parameter                   :: max_length = 256
    integer, private, parameter                   :: dp = kind(1.0d00)
    !
    interface max_keylength
       module procedure max_keylength
    end interface
    interface leaf_keylength
       module procedure leaf_keylength
    end interface
    interface print_initree
       module procedure print_initree
    end interface

    interface prop_get
       module procedure prop_file
       module procedure prop_get_string
       module procedure prop_get_integer
       module procedure prop_get_integers
       module procedure prop_get_real
       module procedure prop_get_reals
       module procedure prop_get_logical
       module procedure prop_get_double
       module procedure prop_get_doubles
       module procedure prop_get_versionstring
    end interface

    interface prop_set
       module procedure prop_set_data
       module procedure prop_set_string
       module procedure prop_set_integer
       module procedure prop_set_integers
       module procedure prop_set_double
       module procedure prop_set_doubles
    end interface
    !
contains

subroutine prop_get_versionstring (versionstring)
    character(*) :: versionstring
    call getfullversionstring_inifiles(versionstring)
end subroutine prop_get_versionstring

subroutine prop_file(filetype, filename , tree, error)
    use tree_structures
    !
    implicit none
    !
    ! Parameters
    !
    character(*), intent(in)  :: filetype
    character(*), intent(in)  :: filename
    type(tree_data), pointer  :: tree
    integer     , intent(out) :: error
    !
    ! Local variables
    !
    character(10) :: ftype

    ftype = filetype
    call lowercase(ftype,999)
    select case (trim(ftype))
    case ('ini')
       call prop_inifile(filename, tree, error)
    case ('tekal')
       call prop_tekalfile(filename, tree, error)
    case default
       write(*,*)'file type ',filetype,' not supported'
       error = 5
    endselect
end subroutine prop_file
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_file
!   Author:     Arjen Markus
!   Purpose:    Read the props from file
!   Context:    Called before calls to prop_get
!   Summary:
!               Read the props file, store the lines with
!               chapters and key-value pairs.
!   Arguments:
!   filename    Name of the file to read
!   error       0: no error occured
!               1: file does not exist
!               2: unable to open file
!               3: no properties found in file
!               4: more than max_properties found
!   Restrictions:
!               - One file at a time can be handled
!               - Maximum number of properties in a file: 200
!               - Maximum length of a line in a file    : 256
!   Multi-line values:
!               Lines ending with '\' are continued on the next line.
!   Comment lines:
!               Chapters are recognised by the character "[" in the first column of a line.
!               Keywords are recognised by the character "=" somewhere in the line.
!               Comments behind line continuation *must* be preceded by '#' and may
!               not contain further '#'s. Same for last line of multi-line block.
!               All other lines are assumed to be comments.
! --------------------------------------------------------------------
!
subroutine prop_inifile(filename , tree, error)
    use tree_structures
    !
    implicit none
    !
    ! Parameters
    !
    character(*), intent(in)  :: filename
    type(tree_data), pointer  :: tree
    integer     , intent(out) :: error
    !
    ! Local variables
    !
    integer               :: eof
    integer               :: eqpos, valend
    integer               :: k, k2
    integer               :: lu
    integer               :: lend, lcend, num_bs
    integer               :: iostatus
    logical               :: filestatus
    logical               :: multiple_lines
    character(max_length) :: key
    character(max_length) :: line
    character(max_length) :: linecont !< Placeholder for continued line
    character(max_length) :: value
    type(tree_data), pointer  :: achapter
    type(tree_data), pointer  :: anode

    !
    !! executable statements -------------------------------------------------------
    !
    achapter => tree
    error    = 0
    inquire (file = filename, exist = filestatus)
    if (.not.filestatus) then
       error = 1
       return
    endif
    do lu = 10, 99
       inquire (lu, opened = filestatus)
       if (.not.filestatus) exit
    enddo
    if (lu == 99) then
       error = 2
       return
    endif
    !
    open (lu, file = filename, status = 'old', iostat = iostatus)
    if (iostatus /= 0) then
       error = 2
       return
    endif
    !
    ! To do:
    !   Get rid of leading blanks
    do
        line = ''
        lend = 0
        multiple_lines = .false.
        do ! Check on line continuation
            read (lu, '(a)', iostat = eof) linecont
            linecont = adjustl(linecont)
            lcend = len_trim(linecont)
            if (lcend == 0) then
                ! Empty line, leave continuation loop
                exit
            endif
            if (linecont(1:1)=='#' .or. linecont(1:1) == '*') then
                ! Comment line, leave continuation loop
                exit
            endif
            ! There could be a comment (started by #) after line continuation backslash
            lcend = scan(linecont(1:lcend),'#',.true.) - 1 ! risk: no # allowed in comment text itself
            if (lcend > 0) then
                lcend = len_trim(linecont(1:lcend)) ! Strip off whitespace preceding possible comment
            else
                lcend = len_trim(linecont)
            end if
            if (lcend > 0) then
                num_bs = lcend - verify(linecont(1:lcend),char(92),.true.) ! nr of backslashes at end of line
                if (mod(num_bs, 2) == 1) then ! Odd nr of backslashes, indeed line continuation
                    multiple_lines = .true.
                    lcend = lcend-1 ! Strip off single line cont character
                    goto 700
                else
                    if (.not. multiple_lines) then
                        ! No continuation, so leave possible comment as well
                        lcend = len_trim(linecont)
                    end if
                    goto 800
                end if
            else
                ! Empty line, leave continuation loop
                exit
            end if                

        700 line = line(1:lend)//' '//linecont(1:lcend)
            lend = lend + lcend + 1
            cycle ! Line continuation, proceed to next line
        800 line = line(1:lend)//' '//linecont(1:lcend)
            lend = lend + lcend + 1
            exit  ! No further lines for this value
        end do

       if (eof/=0) exit
       !
       ! Remove carriage returns and tabs
       !
        do k=1,len_trim(line)
            if (line(k:k) == char(13)) then
                line(k:k) = ' '
            else if (line(k:k) == char(9)) then
                line(k:k) = ' '
            end if
        end do
       !
       ! Remove leading spaces, cycle when line is empty
       !
       line = adjustl(line)
       if (len_trim(line) == 0) cycle
       !
       ! Chapters
       !
       if (line(1:1)=='[') then
          k = index(line, ']')
          if (k<=0) then
             cycle
          endif
          !
          call lowercase(line(2:k-1),k-2)
          call tree_create_node( tree, line(2:k-1), achapter)
       else
          !
          ! Key-value pairs
          !
          eqpos = index(line, '=')
          if (eqpos<=0) then
             call tree_create_node( achapter, " ", anode)
             call tree_put_data( anode, transfer(trim(adjustl(line)),node_value), "STRING")
             cycle
          endif
          k = index('ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz0123456789', &
                   & line(1:1))
          if (k<=0) then
             call tree_create_node( achapter, " ", anode)
             call tree_put_data( anode, transfer(trim(adjustl(line)),node_value), "STRING")
             cycle
          endif
          key = adjustl(line(1:eqpos-1))
          call lowercase(key,999)
          
          ! Strip off comments that start with #
          ! To allow lines like: FileName = somefile.ext # Comment text
          ! and prevent that comment text also ends up in file name.
          k = index(line(eqpos+1:), '#')
          if (k > 0) then ! # found, could be first delimiter of #..string..#, OR start of comment
              k2 = index(line(eqpos+k+1:), '#')
              if (k2 > 0) then ! Second # found: ##-delimited value
                  valend = eqpos+k+k2 ! Last value char is second #.
              else             ! No second # found: it was a value followed by # + comment
                  valend = eqpos+k-1  ! Last value char is just before first #.
              end if
          else
              valend = len_trim(line) ! Value is complete line after = char
          end if

          value = adjustl(line(eqpos+1:valend))
          call tree_create_node( achapter, trim(key), anode)
          call tree_put_data( anode, transfer(trim(value),node_value), "STRING")
       endif
       !
       ! Get the next line
       !
    enddo
    !
    ! End of file or procedure
    !
    close (lu)
end subroutine prop_inifile
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_tekal_file
!   Author:     Adri Mourits
!   Purpose:    Read the props from file
!   Context:    Called before calls to prop_get
!   Summary:
!               Read the props file, store the lines with
!               chapters and key-value pairs.
!   Arguments:
!   filename    Name of the file to read
!   error       0: no error occured
!               1: file does not exist
!               2: unable to open file
!               3: no properties found in file
!               4: more than max_properties found
!   Restrictions:
!               - One file at a time can be handled
!               - Maximum number of properties in a file: 200
!               - Maximum length of a line in a file    : 256
!   Comment lines:
!               Chapters are recognised by the character "[" in the first column of a line.
!               Keywords are recognised by the character "=" somewhere in the line.
!               All other lines are assumed to be comments.
! --------------------------------------------------------------------
!
subroutine prop_tekalfile(filename , tree, error)
    use tree_structures
    !
    implicit none
    !
    ! Parameters
    !
    character(*), intent(in)  :: filename
    type(tree_data), pointer  :: tree
    integer     , intent(out) :: error
    !
    ! Local variables
    !
    integer               :: eof
    integer               :: k
    integer               :: lu
    integer               :: iostatus
    integer, dimension(2) :: blockdims
    real   , dimension(:),allocatable :: arow
    logical               :: filestatus
    character(max_length) :: line
    type(tree_data), pointer  :: atekalblock
    type(tree_data), pointer  :: anode
    !
    !! executable statements -------------------------------------------------------
    !
    blockdims = 0
    error    = 0
    inquire (file = filename, exist = filestatus)
    if (.not.filestatus) then
       error = 1
       return
    endif
    do lu = 10, 99
       inquire (lu, opened = filestatus)
       if (.not.filestatus) exit
    enddo
    if (lu == 99) then
       error = 2
       return
    endif
    !
    open (lu, file = filename, status = 'old', iostat = iostatus)
    if (iostatus /= 0) then
       error = 2
       return
    endif
    do
       !
       ! Skip the commentary in the header, lines starting with a '*'
       !
       do
         read (lu, '(a)', iostat = eof) line
         if (eof/=0 .or. line(1:1)/='*') exit
       enddo
       if (eof/=0) exit
       !
       ! Remove carriage returns
       !
       k = index(line, char(13))
       if (k>0) then
          line(k:k) = ' '
       endif
       !
       ! Remove leading spaces
       !
       line = adjustl(line)
       !
       ! Assumption: this line contains the name of a tekal block
       !
       call lowercase(line,999)
       call tree_create_node( tree, trim(line), atekalblock)
       !
       ! Assumption: nest line contains the dimensions of the tekal block
       !
       read (lu, *, iostat = eof) blockdims
       if (eof/=0) exit
       call tree_put_data( atekalblock, transfer(blockdims,node_value), "INTEGER ARRAY")
       allocate(arow(blockdims(2)))
       do k=1,blockdims(1)
          read (lu, *, iostat = eof) arow
          if (eof/=0) exit
          write(line,'(a,i0)')'row_',k
          call tree_create_node( atekalblock, trim(line), anode)
          call tree_put_data( anode, transfer(arow,node_value), "REAL ARRAY")
       enddo
       deallocate(arow)
       !
       ! Get the next tekal block
       !
    enddo
    !
    ! End of file or procedure
    !
    close (lu)
end subroutine prop_tekalfile


!> Writes a property tree to file in ini format.
subroutine prop_write_inifile(mout, tree, error)
    integer,                  intent(in)  :: mout  !< File pointer where to write to.
    type(TREE_DATA), pointer              :: tree  !< Tree to be written.
    integer,                  intent(out) :: error !< Return status.

    character(len=1), allocatable :: lenmaxdata(:)
    logical :: dummylog

    allocate(lenmaxdata(size(transfer(123, node_value)))) ! Fit a single integer into char array (generally 4 bytes)

    error = 0

    ! Determine maximum key stringlength (used for prettyprinting/alignment in print_initree)
    call tree_fold(tree, max_keylength, leaf_keylength, lenmaxdata, dummylog)
    
    ! Print the tree by traversing it depth-first, pass mout and lenmax by transfer into data variable.
    call tree_traverse(tree, print_initree, transfer((/ mout, transfer(lenmaxdata, 123) /), node_value), dummylog)

end subroutine prop_write_inifile

!> Selects the maximum keylength from childdata.
!! to be used in call to tree_fold.
subroutine max_keylength( tree, childdata, data, stop)
    type(TREE_DATA), pointer                        :: tree
    character(len=1), dimension(:,:), intent(in)    :: childdata
    character(len=1), dimension(:),   intent(out)   :: data
    logical,                          intent(inout) :: stop

    integer :: i, lenmax, n
    lenmax = 0
    n = size(childdata, 2)

    do i = 1,size(childdata, 2)
        lenmax = max(lenmax, transfer(childdata(:,i), lenmax))
    end do

    data = transfer(lenmax, data)
end subroutine max_keylength

!> Selects the keylength from a tree leave.
!! to be used in call to tree_fold.
subroutine leaf_keylength( tree, data, stop)
    type(TREE_DATA), pointer                        :: tree
    character(len=1), dimension(:),   intent(out)   :: data
    logical,                          intent(inout) :: stop

    character(len=1), dimension(:),pointer :: data_ptr
    character(len=max_length)              :: string
    character(len=40)                      :: type_string
    integer                                :: keylen

    call tree_get_data_ptr( tree, data_ptr, type_string )

    if (associated(data_ptr)) then ! This is a key-value pair
        keylen = len_trim(tree_get_name(tree))
    else
        keylen = 0 ! Don't include chapter names
    end if

    data = transfer(keylen, data)
end subroutine leaf_keylength


!> Prints the root of a tree (either as chapter or as key-value pair)
!! to be used in call to tree_traverse
subroutine print_initree( tree, data, stop )
    type(TREE_DATA), pointer                   :: tree    !< Tree whose root should be printed.
    character(len=1), dimension(:), intent(in) :: data    !< Help data (max key length, used for alignment).
    logical,                        intent(inout) :: stop !< Whether to continue or stop.

    integer, dimension(2)                  :: inputdata
    integer                                :: mout
    integer                                :: maxkeylength 
    character(len=1), dimension(:),pointer :: data_ptr
    character(len=max_length)              :: string
    character(len=40)                      :: type_string
    logical                                :: success
    integer                                :: level

    inputdata    = transfer(data, inputdata)
    mout         = inputdata(1) !< File pointer
    maxkeylength = inputdata(2)

    level = tree_traverse_level()
    if (level == 0) return

    call tree_get_data_ptr( tree, data_ptr, type_string )
    if (.not. associated(data_ptr)) then
        write(mout, '(a)') ''
        write(mout, '(a,a,a)') '[', trim(tree_get_name(tree)), ']'
        return
    else
        string = tree_get_name(tree)
        write(mout, '(a,a)', advance='no') &
            trim(string), repeat(' ', max(0,maxkeylength-len_trim(string)))
        if (len_trim(string) > 0) then
            write(mout, '(a)', advance='no') ' = '
        else ! For comment lines that appear as key/value with key==''
            write(mout, '(a)', advance='no') '   '
        end if
    end if

    select case (type_string)
    case ('STRING')
        string = '(no data)'
        call tree_get_data_string( tree, string, success )
        write(mout,'(a)') trim(string)
   case default
      string = '(unknown data type)'
      write(mout,'(a,a,a,a)') '# ', trim(string), ' -- ', trim(type_string)
   end select
end subroutine print_initree



!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_string
!   Author:     Arjen Markus
!   Purpose:    Get the string value for a property
!   Context:    Used by applications
!   Summary:
!               Go through the list of props to check the
!               chapter. When the right chapter is found, check
!               for the key.
!               Only set the value if the key matches
!   Arguments:
!   chapter     Name of the chapter (case-insensitive) or "*" to get any key
!   key         Name of the key (case-insensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
!   success     Whether successful or not (optional)
!   Delimiters:
!               If the value starts with the character "#", this character is removed.
!               If a second character "#" is found , this character and everything behind
!               this character is removed.
!   Comments on this line:
!               Use the delimiters "#". Example:
!               StringIn = # AFileName # Comments are allowed behind the second "#"
! --------------------------------------------------------------------
!
subroutine prop_get_string(tree, chapterin ,keyin     ,value, success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer        :: tree
    character(*),intent(in)         :: chapterin
    character(*),intent(in)         :: keyin
    character(*)                    :: value
    logical, optional, intent (out) :: success
    !
    ! Local variables
    !
    logical                   :: ignore
    logical                   :: success_
    integer                   :: free_space ! Length of parameter "value" that is not written yet
    integer                   :: i          ! Childnode number with node_name = key
                                            ! All following child nodes with node_name = " " are also added
    integer                   :: k
    character(80)             :: nodename
    character(255)            :: chapter
    character(255)            :: key
    character(max_length)     :: localvalue
    type(tree_data), pointer  :: thechapter
    type(tree_data), pointer  :: anode
    !
    !! executable statements -------------------------------------------------------
    !
    success_ = .false.
    chapter = chapterin
    key     = keyin
    call lowercase(chapter,999)
    call lowercase(key,999)
    localvalue = ' '
    !
    ! Handle chapters
    !
    ignore = chapter(1:1)=='*'
    !
    ! Find the chapter first
    !
    thechapter => tree
    if (.not.ignore) then
       call tree_get_node_by_name( tree, trim(chapter), thechapter)
       if ( .not. associated(thechapter) ) then
          thechapter => tree
       endif
    endif
    !
    ! Find the key
    ! To do:
    !    Remove leading blanks
    ! Note:
    !    Work around an apparent problem with the SUN Fortran 90
    !    compiler
    !
    call tree_get_node_by_name( thechapter, trim(key), anode, i)
    if ( associated(anode) ) then
        free_space = len(value)
        do
           call tree_get_data_string( anode, localvalue, success_ )
           !
           ! Remove possible delimiters #
           !
           if (localvalue(1:1)=='#') then
              localvalue = localvalue(2:)
              k = index(localvalue, '#')
              if (k>0) then
                 localvalue = localvalue(1:k-1)
              endif
              localvalue = adjustl(localvalue)
           endif
           !
           ! Write to parameter "value"
           !
           if (free_space == len(value)) then
              ! First write to "value": Write as much as possible
              value = ' '
              value = localvalue(:min(free_space,len_trim(localvalue)))
           else
              ! Follow up write to "value": Only add when there is enough free space
              ! This is to avoid "half values" being added
              if (len_trim(localvalue) > free_space - 1) then
                 ! Not enough free space in parameter "value" anymore. Exit this do-loop.
                 exit
              endif
              ! Add a space between the values
              value = trim(value) // ' ' // localvalue(:len_trim(localvalue))
           endif
           free_space = len(value) - len_trim(value)
           !
           ! Check if the next child node has name " "
           !
           i = i + 1
           if (associated(thechapter%child_nodes) .and. i<=size(thechapter%child_nodes)) then
              nodename = "dummy value"
              nodename = tree_get_name( thechapter%child_nodes(i)%node_ptr )
              if (nodename == " ") then
                 ! Yes: add this data string to parameter "value" (in the next do-loop)
                 anode => thechapter%child_nodes(i)%node_ptr
              else
                 ! No: exit do-loop
                 exit
              endif
           else
              exit
           endif
        enddo
    else
        ! Key not found
    endif

    ! success var is not optional in tree_struct, so we used local placeholder first
    if (present(success)) then
        success = success_
    end if
end subroutine prop_get_string
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_integer
!   Author:     Arjen Markus
!   Purpose:    Get the integer value for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to integer.
!   Arguments:
!   chapter     Name of the chapter (case-insensitive) or "*" to get any key
!   key         Name of the key (case-insensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
!   success     Whether successful or not (optional)
!   Comments on this line:
!               Value is set with the first integer found behind the character "=".
!               The following example is allowed:
!               IntegerIn = Index 8, denoting the startpoint for searches
! --------------------------------------------------------------------
!
subroutine prop_get_integer(tree  ,chapter   ,key       ,value     ,success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer    :: tree
    integer     ,intent (inout) :: value
    character(*),intent (in)    :: chapter
    character(*),intent (in)    :: key
    logical, optional, intent (out) :: success
    !
    ! Local variables
    !
    integer, dimension(1) :: valuearray
    !
    !! executable statements -------------------------------------------------------
    !
    valuearray(1) = value
    call prop_get_integers(tree   ,chapter   ,key       ,valuearray,1, success)
    value = valuearray(1)
end subroutine prop_get_integer
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_integers
!   Author:     Adri Mourits
!   Purpose:    Get the array of integer values for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to integers.
!               If the string contains less integers than valuelength,
!               only the integers found are set in value.
!               If the string contains more integers than valuelength,
!               only valuelength integers are set in value
!   Arguments:
!   chapter     Name of the chapter (case-insensitive) or "*" to get any key
!   key         Name of the key (case-insensitive)
!   value       Values of the key (not set if the key is not found,
!               so you can set a default value)
!   success     Whether successful or not (optional)
!   Comments on this line:
!               Everywhere behind the character "=".
!               The following example is allowed:
!               IntegersIn = (n,m): 4,5
! --------------------------------------------------------------------
!
subroutine prop_get_integers(tree   ,chapter   ,key       ,value     ,valuelength, success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer  :: tree
    integer              ,intent (in)  :: valuelength
    integer, dimension(*),intent (out) :: value
    character(*)         ,intent (in)  :: chapter
    character(*)         ,intent (in)  :: key
    logical, optional    ,intent (out) :: success
    !
    ! Local variables
    !
    integer :: i
    integer :: k
    integer :: length
    integer :: valcount
    integer :: ierr
    character(12)  :: intchars = '0123456789-+'
    character(20)  :: fmt
    character(255) :: avalue
    character(255) :: prop_value
    !
    !! executable statements -------------------------------------------------------
    !
    prop_value = ' '
    call prop_get_string(tree   ,chapter   ,key       ,prop_value,success)
    !
    ! Extract the integer part
    ! Using read(prop_value,*,iostat=io) (value(i),i=1,valuelength)
    ! Has another behaviour as the following implementation:
    !
    do valcount = 1, valuelength
       !
       ! Remove everything before the first integer
       !
       k = 0
       do i = 1, len_trim(prop_value)
          k = index(intchars, prop_value(i:i))
          if (k>0) exit
       enddo
       !
       ! k=0: no integer found
       !
       if (k == 0) return
       prop_value = prop_value(i:len(prop_value))
       !
       ! Move the first integer to avalue
       !
       do i = 1, len(prop_value)
          k = index(intchars, prop_value(i:i))
          if (k==0) exit
       enddo
       avalue = prop_value(1:i - 1)
       prop_value = prop_value(i:len(prop_value))
       length = len_trim(avalue)
       if (length/=0) then
          write (fmt, '(a,i5,a)') '(i', length, ')'
          read (avalue, fmt, iostat=ierr) value(valcount)
          if (ierr /= 0) then
             if (present(success)) then
                success = .false.
             endif
             return
          endif
       endif
    enddo
end subroutine prop_get_integers
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_real
!   Author:     Arjen Markus
!   Purpose:    Get the real value for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to real.
!   Arguments:
!   chapter     Name of the chapter (case-insensitive) or "*" to get any key
!   key         Name of the key (case-insensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
!   success     Whether successful or not (optional)
!   Comments on this line:
!               Value is set with the first real found behind the character "=".
!               The following example is allowed:
!               RealIn = Gravity 9.8, m/s*2
! --------------------------------------------------------------------
!
subroutine prop_get_real(tree  ,chapter   ,key       ,value     ,success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer    :: tree
    real        ,intent (inout) :: value
    character(*),intent (in)    :: chapter
    character(*),intent (in)    :: key
    logical, optional, intent(out) :: success
    !
    ! Local variables
    !
    real, dimension(1) :: valuearray
    !
    !! executable statements -------------------------------------------------------
    !
    valuearray(1) = value
    call prop_get_reals(tree  ,chapter   ,key       ,valuearray,1, success)
    value = valuearray(1)
end subroutine prop_get_real
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_reals
!   Author:     Adri Mourits
!   Purpose:    Get the array of real values for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to reals.
!               If the string contains less reals than valuelength,
!               only the reals found are set in value.
!               If the string contains more reals than valuelength,
!               only valuelength reals are set in value
!   Arguments:
!   chapter     Name of the chapter (case-insensitive) or "*" to get any key
!   key         Name of the key (case-insensitive)
!   value       Values of the key (not set if the key is not found,
!               so you can set a default value)
!   success     Whether successful or not (optional)
!   Comments on this line:
!               Everywhere behind the character "=".
!               The following example is allowed:
!               RealsIn = (x,y): 4.5,5.9 Start point
! --------------------------------------------------------------------
!
subroutine prop_get_reals(tree  ,chapter ,key ,value ,valuelength, success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer  :: tree
    integer           , intent (in)  :: valuelength
    real, dimension(*), intent (out) :: value
    character(*)      , intent (in)  :: chapter
    character(*)      , intent (in)  :: key
    logical, optional , intent (out) :: success
    !
    ! Local variables
    !
    integer         :: i
    integer         :: k
    integer         :: length
    integer         :: valcount
    integer         :: ierr
    character(15)   :: realchars = '0123456789-+.eE'
    character(20)   :: fmt
    character(255)  :: avalue
    character(1000) :: prop_value
    logical         :: digitfound
    !
    !! executable statements -------------------------------------------------------
    !
    prop_value = ' '
    call prop_get_string(tree  ,chapter   ,key       ,prop_value,success)
    !
    ! Extract the real part
    ! Using read(prop_value,*,iostat=io) (value(i),i=1,valuelength)
    ! Has another behaviour as the following implementation:
    !
    do valcount = 1, valuelength
       do
          !
          ! Remove everything before the first real
          !
          digitfound = .false.
          k = 0
          length = len_trim(prop_value)
          if (length < 1) exit
          do i = 1, length
             k = index(realchars, prop_value(i:i))
             if (k>0) exit
          enddo
          !
          ! k=0: no real found
          !
          if (k == 0) return
          prop_value = prop_value(i:len(prop_value))
          !
          ! Move the first real to avalue
          !
          length = len_trim(prop_value)
          do i = 1, length
             k = index(realchars, prop_value(i:i))
             if (k==0) exit
             if (k <= 10) digitfound = .true.
          enddo
          avalue = prop_value(1:i - 1)
          prop_value = prop_value(i:len(prop_value))
          length = len_trim(avalue)
          !
          ! if avalue does not contain a digit, scan the rest of prop_value for reals
          !
          if (digitfound .and. length/=0) then
             write (fmt, '(a,i0,a)') '(f', length, '.0)'
             read (avalue, fmt, iostat=ierr) value(valcount)
             if (ierr /= 0) then
                if (present(success)) then
                   success = .false.
                endif
                return
             endif
             exit
          endif
       enddo
    enddo
end subroutine prop_get_reals
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_double
!   Author:     Arjen Markus
!   Purpose:    Get the double-precision real value for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to a double precision real.
!   Arguments:
!   chapter     Name of the chapter (case-insensitive) or "*" to get any key
!   key         Name of the key (case-insensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
!   success     Whether successful or not (optional)
!   Comments on this line:
!               Value is set with the first real found behind the character "=".
!               The following example is allowed:
!               RealIn = Gravity 9.8, m/s*2
! --------------------------------------------------------------------
!
subroutine prop_get_double(tree  ,chapter   ,key       ,value     ,success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer      :: tree
    real(kind=dp) ,intent (inout) :: value
    character(*)  ,intent (in)    :: chapter
    character(*)  ,intent (in)    :: key
    logical, optional, intent(out) :: success
    !
    ! Local variables
    !
    real(kind=dp), dimension(1) :: valuearray
    !
    !! executable statements -------------------------------------------------------
    !
    valuearray(1) = value
    call prop_get_doubles(tree  ,chapter   ,key       ,valuearray,1         ,success)
    value = valuearray(1)
end subroutine prop_get_double
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_doubles
!   Author:     Adri Mourits
!   Purpose:    Get the array of double precision real values for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to double precision reals.
!               If the string contains less reals than valuelength,
!               only the reals found are set in value.
!               If the string contains more reals than valuelength,
!               only valuelength reals are set in value
!   Arguments:
!   chapter      Name of the chapter (case-insensitive) or "*" to get any key
!   key          Name of the key (case-insensitive)
!   value        Values of the key (not set if the key is not found,
!                so you can set a default value)
!   valuelength  Size of the array value
!   success     Whether successful or not (optional)
!   Comments on this line:
!               Everywhere behind the character "=".
!               The following example is allowed:
!               RealsIn = (x,y): 4.5,5.9 Start point
! --------------------------------------------------------------------
!
subroutine prop_get_doubles(tree  ,chapter ,key ,value ,valuelength,success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer  :: tree
    integer                    , intent (in)  :: valuelength
    real(kind=dp), dimension(*), intent (out) :: value
    character(*)               , intent (in)  :: chapter
    character(*)               , intent (in)  :: key
    logical, optional          , intent (out) :: success
    !
    ! Local variables
    !
    integer         :: i
    integer         :: k
    integer         :: length
    integer         :: valcount
    integer         :: ierr
    character(17)   :: realchars = '0123456789-+.eEdD'
    character(20)   :: fmt
    character(255)  :: avalue
    character(1000) :: prop_value
    logical         :: digitfound
    !
    !! executable statements -------------------------------------------------------
    !
    prop_value = ' '
    call prop_get_string(tree  ,chapter   ,key       ,prop_value,success)
    !
    ! Extract the real part
    ! Using read(prop_value,*,iostat=io) (value(i),i=1,valuelength)
    ! Has another behaviour as the following implementation:
    !
    do valcount = 1, valuelength
       do
          !
          ! Remove everything before the first real
          !
          digitfound = .false.
          k = 0
          length = len_trim(prop_value)
          if (length < 1) exit
          do i = 1, length
             k = index(realchars, prop_value(i:i))
             if (k>0) exit
          enddo
          !
          ! k=0: no real found
          !
          if (k == 0) return
          prop_value = prop_value(i:len(prop_value))
          !
          ! Move the first real to avalue
          !
          length = len_trim(prop_value)
          do i = 1, length
             k = index(realchars, prop_value(i:i))
             if (k==0) exit
             if (k <= 10) digitfound = .true.
          enddo
          avalue = prop_value(1:i - 1)
          prop_value = prop_value(i:len(prop_value))
          length = len_trim(avalue)
          !
          ! if avalue does not contain a digit, scan the rest of prop_value for reals
          !
          if (digitfound .and. length/=0) then
             write (fmt, '(a,i0,a)') '(f', length, '.0)'
             read (avalue, fmt, iostat=ierr) value(valcount)
             if (ierr /= 0) then
                if (present(success)) then
                   success = .false.
                endif
                return
             endif
             exit
          endif
       enddo
    enddo
end subroutine prop_get_doubles
!
!
! --------------------------------------------------------------------
!   Subroutine: prop_get_logical
!   Author:     Arjen Markus
!   Purpose:    Get the logical value for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to logical.
!               Allowed strings to detect the value true:
!               Y|YES|yes|Yes|T|TRUE|true|True|J|JA|Ja|ja|W|WAAR|Waar|waar
!               Allowed strings to detect the value false:
!               N|NO|no|No|F|FALSE|false|False|N|NEE|Nee|nee|O|ONWAAR|Onwaar|onwaar
!   Arguments:
!   chapter     Name of the chapter (case-insensitive) or "*" to get any key
!   key         Name of the key (case-insensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
!   success     Whether successful or not (optional)
!   Comments on this line:
!               Not allowed
! --------------------------------------------------------------------
!
subroutine prop_get_logical(tree  ,chapter   ,key       ,value     ,success)
    implicit none
    !
    ! Parameters
    !
    type(tree_data), pointer  :: tree
    character(*),intent (in)  :: chapter
    character(*),intent (in)  :: key
    logical     ,intent (out) :: value
    logical, optional, intent (out) :: success
    !
    ! Local variables
    !
    integer :: k1
    integer :: k2
    integer :: pointpos
    integer :: spacepos
    integer :: vallength
    character(100) :: falsity
    character(100) :: truth
    character(max_length) :: prop_value
    !
    data truth/    &
     & '|Y|y|YES|yes|Yes|T|t|TRUE|true|True|J|j|JA|Ja|ja|W|w|WAAR|Waar|waar|'/
    data falsity/  &
     & '|N|n|NO|no|No|F|f|FALSE|false|False|N|n|NEE|Nee|nee|O|o|ONWAAR|Onwaar|onwaar|'/
    !
    !! executable statements -------------------------------------------------------
    !
    prop_value = ' '
    call prop_get_string(tree  ,chapter   ,key       ,prop_value,success)
    prop_value = adjustl(prop_value)
    if (prop_value(1:1) == '.') prop_value = prop_value(2:)
    vallength = len_trim(prop_value)
    !
    ! Leave immediately in case prop_value is empty
    !
    if (vallength == 0) return
    spacepos = index(prop_value,' ')
    if (spacepos > 0) vallength = min(spacepos - 1, vallength)
    pointpos = index(prop_value,'.')
    if (pointpos > 0) vallength = min(pointpos - 1, vallength)
    !
    ! Extract the logical part
    !
    k1 = index(truth  , prop_value(1:vallength))
    k2 = index(falsity, prop_value(1:vallength))
    !
    ! The value must match a complete word in string truth or falsity, bordered by two '|'s
    !
    if (k1 > 0) then
       if (truth(k1-1:k1-1)=='|' .and. truth(k1+vallength:k1+vallength)=='|') then
          value = .true.
       endif
    endif
    if (k2>0) then
       if (falsity(k2-1:k2-1)=='|' .and. falsity(k2+vallength:k2+vallength)=='|') then
          value = .false.
       endif
    endif
end subroutine prop_get_logical


!> The generic routine for setting key-value data in the tree.
!! The value (of any type) should be transferred into the type of node_value.
subroutine prop_set_data(tree, chapter, key, value, type_string, anno, success)
    type(tree_data), pointer               :: tree        !< The property tree
    character(*),             intent (in)  :: chapter     !< Name of the chapter under which to store the property ('' or '*' for global)
    character(*),             intent (in)  :: key         !< Name of the property
    character(len=1),         intent (in)  :: value(:)    !< Value of the property
    character(*),             intent (in)  :: type_string !< Data type of the property
    character(len=*), optional, intent (in) :: anno       !< Optional annotation/comment
    logical, optional,        intent (out) :: success     !< Returns whether the operation was successful  

    character(len=1), allocatable :: valueline(:)
    logical :: ignore
    logical :: success_
    type(tree_data), pointer  :: thechapter
    type(tree_data), pointer  :: anode
    integer :: i, ianno, nanno, nval, nvalanno

    success_ = .false.

    ignore = len(chapter) == 0
    if (.not. ignore) ignore = chapter(1:1)=='*'

    ! Find the chapter first
    if (ignore) then
        thechapter => tree
    else
       call tree_get_node_by_name( tree, trim(chapter), thechapter)

       ! If chapter does not exist, create it.
       if ( .not. associated(thechapter) ) then
          call tree_create_node( tree, trim(chapter), thechapter)
       endif
    endif

    ! Create the node for key (not looking for earlier definitions)
    call tree_create_node( thechapter, trim(key), anode)
    if ( associated(anode) ) then
        ! Determine value length and optional annotation length
        ! and combine the two in valueline(:) (separated by ' # ')
        nval = size(value)
        if (present(anno)) then
            ianno    = max(20, nval) ! column nr where anno will start
            nanno    = len_trim(anno)
            nvalanno = ianno + nanno + 2 ! Separate by ' # '
        else
            ianno = nval
            nanno = 0
            nvalanno = nval
        end if

        allocate(valueline(nvalanno))
        valueline = ' '

        do i=1,nval
            valueline(i) = value(i)
        end do

        if (nanno > 0) then
            valueline(ianno+1) = '#'
        end if

        do i=1,nanno
            valueline(ianno+2+i) = anno(i:i)
        end do
        call tree_put_data(anode, transfer(valueline, node_value), trim(type_string), success_)
        deallocate(valueline)
    end if

    if (present(success)) then
        success = success_
    end if
end subroutine prop_set_data


!> Sets a string property in the tree.
!! Take care of proper quoting (e.g., by "" or ##) at the call site.
subroutine prop_set_string(tree, chapter, key, value, anno, success)
    type(tree_data),   pointer      :: tree    !< The property tree
    character(*),      intent (in)  :: chapter !< Name of the chapter under which to store the property ('' or '*' for global)
    character(*),      intent (in)  :: key     !< Name of the property
    character(len=*),  intent (in)  :: value   !< Value of the property
    character(len=*), optional, intent (in) :: anno       !< Optional annotation/comment
    logical, optional, intent (out) :: success !< Returns whether the operation was successful
    logical :: success_                         

    if (present(anno)) then
        call prop_set_data(tree, chapter, key, transfer(value, node_value), 'STRING', anno = anno, success = success_)
    else
        call prop_set_data(tree, chapter, key, transfer(value, node_value), 'STRING', success = success_)
    end if

    if (present(success)) then
        success = success_
    end if
end subroutine prop_set_string


!> Sets a double precision array property in the tree.
!! The property value is stored as a string representation.
subroutine prop_set_doubles(tree, chapter, key, value, anno, success)
    type(tree_data),   pointer      :: tree      !< The property tree
    character(*),      intent (in)  :: chapter   !< Name of the chapter under which to store the property ('' or '*' for global)
    character(*),      intent (in)  :: key       !< Name of the property
    real(kind=dp),     intent (in)  :: value(:)  !< Value of the property
    character(len=*), optional, intent (in) :: anno       !< Optional annotation/comment
    logical, optional, intent (out) :: success   !< Returns whether the operation was successful
                                                  
    logical :: success_
    character(len=max_length) :: strvalue
    character(len=24)         :: strscalar
    integer :: i, is, iv, n

    strvalue = ' '
    n = size(value)
    if (n==0) goto 10

    ! Pretty print all doubles into strvalue, separated by single spaces
    call pp_double(value(1), strvalue)
    iv = len_trim(strvalue)
    do i=2,n
        call pp_double(value(i), strscalar)
        is = len_trim(strscalar)
        strvalue(iv+2:iv+is+1) = strscalar(1:is)
        iv  = iv+is+1
    end do
    

 10 continue ! Put the string representation into the tree
    if (present(anno)) then
        call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', anno = anno, success = success_)
    else
        call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', success = success_)
    end if

    if (present(success)) then
        success = success_
    end if
 
end subroutine prop_set_doubles


!> Sets a double precision property in the tree.
!! The property value is stored as a string representation.
subroutine prop_set_double(tree, chapter, key, value, anno, success)
    type(tree_data),   pointer      :: tree     !< The property tree
    character(*),      intent (in)  :: chapter  !< Name of the chapter under which to store the property ('' or '*' for global)
    character(*),      intent (in)  :: key      !< Name of the property
    double precision,  intent (in)  :: value    !< Value of the property
    character(len=*), optional, intent (in) :: anno       !< Optional annotation/comment
    logical, optional, intent (out) :: success  !< Returns whether the operation was successful
 
    logical :: success_
    real(kind=dp) :: valuearray(1)

    valuearray(1) = value

    if (present(anno)) then
        call prop_set_doubles(tree, chapter, key, valuearray, anno = anno, success = success_)
    else
        call prop_set_doubles(tree, chapter, key, valuearray, success = success_)
    end if

    if (present(success)) then
        success = success_
    end if

end subroutine prop_set_double


!> Sets an integer array property in the tree.
!! The property value is stored as a string representation.
subroutine prop_set_integers(tree, chapter, key, value, anno, success)
    type(tree_data),   pointer      :: tree      !< The property tree
    character(*),      intent (in)  :: chapter   !< Name of the chapter under which to store the property ('' or '*' for global)
    character(*),      intent (in)  :: key       !< Name of the property
    integer,           intent (in)  :: value(:)  !< Value of the property
    character(len=*), optional, intent (in) :: anno       !< Optional annotation/comment
    logical, optional, intent (out) :: success   !< Returns whether the operation was successful
                                                  
    logical :: success_
    character(len=max_length) :: strvalue
    character(len=24)         :: strscalar
    integer :: i, is, iv, n

    strvalue = ' '
    n = size(value)
    if (n==0) goto 10

    ! Pretty print all integers into strvalue, separated by single spaces
    write(strvalue, *) value(1)
    strvalue = adjustl(strvalue)
    iv = len_trim(strvalue)
    do i=2,n
        write(strscalar,*) value(i)
        strscalar = adjustl(strscalar)
        is = len_trim(strscalar)
        strvalue(iv+2:iv+is+1) = strscalar(1:is)
        iv  = iv+is+1
    end do

 10 continue ! Put the string representation into the tree
    if (present(anno)) then
        call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', anno = anno, success = success_)
    else
        call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', success = success_)
    end if

    if (present(success)) then
        success = success_
    end if
 
end subroutine prop_set_integers


!> Sets an integer property in the tree.
!! The property value is stored as a string representation.
subroutine prop_set_integer(tree, chapter, key, value, anno, success)
    type(tree_data),   pointer      :: tree     !< The property tree
    character(*),      intent (in)  :: chapter  !< Name of the chapter under which to store the property ('' or '*' for global)
    character(*),      intent (in)  :: key      !< Name of the property
    integer,           intent (in)  :: value    !< Value of the property
    character(len=*), optional, intent (in) :: anno       !< Optional annotation/comment
    logical, optional, intent (out) :: success  !< Returns whether the operation was successful
 
    logical :: success_
    integer :: valuearray(1)

    valuearray(1) = value

    if (present(anno)) then
        call prop_set_integers(tree, chapter, key, valuearray, anno = anno, success = success_)
    else
        call prop_set_integers(tree, chapter, key, valuearray, success = success_)
    end if    

    if (present(success)) then
        success = success_
    end if

end subroutine prop_set_integer


!> Prettyprints a double precision real to a character string
!! Trailing zeros and leading blanks are removed.
subroutine pp_double(value, strvalue)
! A bit ad-hoc prettyprinting, intended for easy readable output in settings files.
    real(kind=dp),    intent(in)  :: value
    character(len=*), intent(out) :: strvalue

    ! adjustl not working in gfortran, so writing to a temp array
    character(len=10000) :: strtmp

    integer :: i, iz, j, n

    write(strtmp,*) value

    i = index(strtmp, '.')
    if (i == 0) then
        strtmp = adjustl(strtmp)
        return
    end if

    n = len_trim(strtmp)
    iz = -1
    do
        i = i+1
        if (i == n+1) then
            ! End of number string, erase any trailing zeros.
            if (iz > 0) then
                strtmp(iz:n) = ' '
            end if
            exit
        end if

        ! Check for a zero, mark position if the previous char wasn't already a zero.
        if (strtmp(i:i) == '0') then
            if (iz < 0) then
                iz = i
            end if
            cycle
        else if (index('EeDd', strtmp(i:i)) > 0) then
            if (iz > 0) then ! Place exponent part over tail of trailing zeros.
                do j=i+2,n
                    if (strtmp(j:j) /= '0') exit
                end do
                if (j==n+1) then ! Entirely remove 'E+000'
                    strtmp(iz:n) = ' '
                else
                    strtmp(iz:iz)       = 'd'
                    strtmp(iz+1:iz+1)   = strtmp(i+1:i+1)
                    strtmp(iz+2:iz+n-j+2) = strtmp(j:n)
                    strtmp(iz+n-j+3:n)  = ' '
                end if
            end if
            exit
        else
            ! No zero nor exponent, unset the trailing zero position
            iz = -1
        end if
    end do

    strvalue = adjustl(trim(strtmp))
end subroutine pp_double
!
!
! --------------------------------------------------------------------
!   Subroutine: lowercase
!   Author:     Cor van der Schelde
!   Purpose:    Convert upper case characters to lower case
!   Context:    This is a copy of subroutine small in Delft3D-FLOW
!               Used inside properties module
!   Summary:
!               Scan string for upper case characters and
!               convert them.
!   Arguments:
!   string      String to be converted
!   lenstr      Length of string to be converted
! --------------------------------------------------------------------
!
subroutine lowercase(string    ,lenstr    )
    implicit none
    !
    ! Global variables
    !
    integer     , intent(in) :: lenstr
    character(*)             :: string
    !
    ! Local variables
    !
    integer :: i
    integer :: j
    integer :: newlen
    !
    !! executable statements -------------------------------------------------------
    !
    newlen = min(lenstr, len(string))
    do i = 1, newlen
       j = ichar(string(i:i))
       if ((j>64) .and. (j<91)) then
          j = j + 32
          string(i:i) = char(j)
       endif
    enddo
end subroutine lowercase

subroutine count_occurrences(input_ptr, group, keyword, npars)
    implicit none
    !
    ! Global variables
    !
    integer                    :: npars
    character(*)               :: group
    character(*)               :: keyword
    type(tree_data), pointer   :: input_ptr
    !
    ! Local variables
    !
    integer                    :: i
    character(80)              :: parname
    type(tree_data), pointer   :: group_ptr
    type(tree_data), pointer   :: node_ptr
    !
    !! executable statements -------------------------------------------------------
    !
    ! Initialise parameters
    !
    i     = 0
    npars = 0
    !
    ! Find the group in the input tree
    !
    call tree_get_node_by_name(input_ptr, group, group_ptr)
    !
    ! Read dimensions from input tree
    !
    if (associated(group_ptr%child_nodes)) then
       do i = 1,size(group_ptr%child_nodes)
          !
          ! Does group_ptr contain one or more children with name keyword?
          !
          node_ptr => group_ptr%child_nodes(i)%node_ptr
          parname = tree_get_name(node_ptr)
          if (parname == keyword) then
             npars = npars + 1
          endif
       enddo
    endif
end subroutine count_occurrences

end module properties
