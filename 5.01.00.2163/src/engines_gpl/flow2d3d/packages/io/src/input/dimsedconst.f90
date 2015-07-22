subroutine dimsedconst(lundia    ,error     ,sedim     ,const     , &
                     & lsed      ,lsedtot   ,lconst    ,gdp       )
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
!  $Id: dimsedconst.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/dimsedconst.f90 $
!!--description-----------------------------------------------------------------
! - Reads sediment dimension from an attribute file
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
! Global variables
!
    integer , intent(out) :: lconst
    integer , intent(out) :: lsed    ! Description and declaration in esm_alloc_int.f90
    integer , intent(out) :: lsedtot ! Description and declaration in esm_alloc_int.f90
    integer               :: lundia  ! Description and declaration in inout.igs
    logical , intent(in)  :: const    
    logical , intent(out) :: error
    logical , intent(out) :: sedim   ! Description and declaration in procs.igs
!
! Local variables
!
    integer                                                :: i
    integer                                                :: istat
    integer                                                :: j
    integer                                                :: lsedbl
    logical                                                :: found         ! File name for sediment parameters Flag is true if KEYWORD is found
    character(6)                                           :: keyword
    character(20)                                          :: versionstring
    character(20)                                          :: namc          ! Name of a constituent as read in md-file
    character(20)   , dimension(:) , allocatable           :: namconst      ! Names of the constituents as read in md-file
    character(20)   , dimension(:) , allocatable           :: namsedim      ! Names of the sediments as read in sed-file
    character(20)                                          :: sedtyptmp     ! Sediment type in sed-file
    character(80)                                          :: parname
    character(256)                                         :: filsed
    character(300)                                         :: message
    type(tree_data)                            , pointer   :: sed_ptr
    type(tree_data)                            , pointer   :: asedblock_ptr
!
!! executable statements -------------------------------------------------------
!
    lconst        = 0
    lsedbl        = 0
    lsed          = 0
    istat         = 0
    sedim         = .false.
    error         = .false.
    !
    ! lsed and lconst are defined by Namc in the md-file
    ! scan Namc## lines for the number of constituents lconst
    !
    keyword= 'Namc  '
    if (const) then
       do i = 1, 99
          if (i < 10) then
             write (keyword(5:5), '(i1)') i
          else
             write (keyword(5:6), '(i2)') i
          endif
          namc = ' '
          call prop_get_string(gdp%mdfile_ptr, '*', keyword, namc)
          if (namc /= ' ') then
             if (i > lconst+1) then
                error   = .true.
                write (lundia,*) keyword,'=#',namc,'#'
                message = 'Namc may not contain constituents behind empty lines'
                call prterr(lundia, 'U021', trim(message))
                call d3stop(1, gdp)
             endif
             lconst = i
          endif
       enddo
    endif
    !
    allocate(namconst(lconst))
    namconst = ' '
    !
    ! check for sediments, determine lsed and lconst are defined by Namc in the md-file
    !
    keyword= 'Namc  '
    do i = 1, lconst
       if (i<10) then
          write (keyword(5:5), '(i1)') i
       else
          write (keyword(5:6), '(i2)') i
       endif
       call prop_get_string(gdp%mdfile_ptr, '*', keyword, namconst(i))
       call small(namconst(i) ,999 )
       namc = namconst(i)
       if (namc(:8) == 'sediment') then
          sedim = .true.
          lsed  = lsed   + 1
          if (i > lsed) then
             error   = .true.
             message = 'Namc may not contain sediments behind non-sediment constituents'
             call prterr(lundia, 'U021', trim(message))
             call d3stop(1, gdp)
          endif
       else
       endif
    enddo
    !
    ! locate 'Filsed' record; file containing sediment parameters
    !
    filsed = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filsed', filsed)
    if (filsed == ' ') then
       if (lsed > 0) then
          error   = .true.
          message = 'Sediments specified in Namc, but no sediment file specified'
          call prterr(lundia, 'U021', trim(message))
          call d3stop(1, gdp)
       else
          !
          ! ok, no sediments
          !
          goto 9999
       endif
    else
       sedim = .true.
    endif
    !
    ! Create Sediment branch in input tree
    !
    call tree_create_node( gdp%input_tree, 'Sediment Input', sed_ptr )
    call tree_put_data( sed_ptr, transfer(trim(filsed),node_value), 'STRING' )
    !
    ! Put sed-file in input tree
    !
    call prop_file('ini', trim(filsed), sed_ptr, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call prterr(lundia, 'G004', trim(filsed))
       case(3)
          call prterr(lundia, 'G006', trim(filsed))
       case default
          call prterr(lundia, 'G007', trim(filsed))
       endselect
       call d3stop(1, gdp)
    endif
    !
    ! Check version number of sed input file
    !
    versionstring = ' '
    call prop_get_string(sed_ptr, 'SedimentFileInformation', 'FileVersion', versionstring)
    if (trim(versionstring) == '02.00') then
       !
       ! Check whether all sediments, specified in namconst in the md-file
       ! have a parameter specification block in the sediment input file
       !
       do i = 1, lsed
          found = .false.
          if ( associated(sed_ptr%child_nodes) ) then
             do j = 1, size(sed_ptr%child_nodes)
                !
                ! Does sed_ptr contain a child with name 'Sediment' (converted to lower case)?
                !
                asedblock_ptr => sed_ptr%child_nodes(j)%node_ptr
                parname = tree_get_name( asedblock_ptr )
                if ( parname == 'sediment') then
                   parname = ' '
                   call prop_get_string(asedblock_ptr, '*', 'Name', parname)
                   call small(parname, len(parname)  )
                   if (trim(parname) == trim(namconst(i))) then
                      found = .true.
                      exit
                   endif
                endif
             enddo
          endif
          if (.not. found) then
             error = .true.
             write(message,'(5a)') 'Sediment #',trim(namconst(i)),          &
                                 & '# is specified in Namc in the md-file', &
                                 & ' but is not found in sediment file ',   &
                                 & trim(filsed)
             call prterr(lundia, 'U021', trim(message))
             call d3stop(1, gdp)
          endif
       enddo
       !
       ! Check uniqueness of all sediments in the sed-file; count number
       ! of sediments not in list of constituents.
       !
       lsedbl   = 0
       allocate(namsedim(size(sed_ptr%child_nodes)))
       namsedim = ' '
       !
       do j = 1, size(sed_ptr%child_nodes)
          !
          ! Does sed_ptr contain a child with name 'Sediment' (converted to lower case)?
          !
          asedblock_ptr => sed_ptr%child_nodes(j)%node_ptr
          parname = tree_get_name( asedblock_ptr )
          if ( parname == 'sediment') then
             parname = ' '
             call prop_get_string(asedblock_ptr, '*', 'Name', parname)
             call small(parname, 999)
             found = .false.
             do i = 1, j-1
                if (trim(parname) == trim(namsedim(i))) then
                   found = .true.
                   exit
                endif
             enddo
             if (found) then
                error = .true.
                write(message,'(5a)') 'Sediment #',trim(parname),         &
                                    & '# is specified more than once in', &
                                    & ' sediment file ',trim(filsed)
                call prterr(lundia, 'U021', trim(message))
                call d3stop(1, gdp)
             endif
             namsedim(j) = parname(1:20)
             !
             ! Count number of sediment fractions not listed as constituent
             !
             found = .false.
             do i = 1, lsed
                if (trim(parname) == trim(namconst(i))) then
                   found = .true.
                   exit
                endif
             enddo
             !
             ! check sedtyp
             !
             sedtyptmp = ' '
             call prop_get_string(asedblock_ptr, '*', 'SedTyp', sedtyptmp)
             call small(sedtyptmp, 999)
             if (sedtyptmp == ' ') then
                error = .true.
                call prterr(lundia, 'U021', 'Missing sediment type for ' &
                          & // trim(parname))
             elseif (found) then
                if (index(sedtyptmp, 'mud') == 1) then
                   !
                   ! if it starts with mud, it's okay
                   !
                elseif (index(sedtyptmp, 'sand') == 1) then
                   !
                   ! if it starts with sand, it's okay
                   !
                else
                   error = .true.
                   call prterr(lundia, 'U007', 'suspended sediment type of ' &
                             & // trim(parname) // ': must start with sand or mud')
                endif
             elseif (.not. found) then
                if (index(sedtyptmp,'bedload') /= 1) then
                   error = .true.
                   call prterr(lundia, 'U007', 'sediment type of ' &
                             & // trim(parname) // ': must start with bedload')
                else
                   lsedbl = lsedbl + 1
                endif
             endif
          endif
       enddo
       deallocate(namsedim, stat=istat)
       write(message,'(i4,2a)') lsedbl, ' bed load fraction(s) found in sediment file: ', &
                              & trim(filsed)
       call prterr(lundia, 'G051', trim(message))
       !
       if (error) then
          call d3stop(1, gdp)
       endif
       !
    else
       !
       ! In versions lower than 02.00, the sediment input file was used to determine lsed.
       ! lsed is now determined by namcon in the md-file
       ! Checking whether all sediments, specified in namcon in the md-file
       ! occur in the sediment input file can only be done for version 02.00 (or higher).
       !
    endif
 9999 continue
    deallocate(namconst, stat=istat)
    lsedtot = lsed + lsedbl
    !
    ! rhosol, namsed and sedtyp must always be allocated
    ! They may already be allocated, because this code
    ! is visited twice (TDATOM and TRISIM)
    !
    if (associated(gdp%gdsedpar%rhosol)) deallocate(gdp%gdsedpar%rhosol, stat=istat)
    if (associated(gdp%gdsedpar%namsed)) deallocate(gdp%gdsedpar%namsed, stat=istat)
    if (associated(gdp%gdsedpar%sedtyp)) deallocate(gdp%gdsedpar%sedtyp, stat=istat)
    istat = 0
    if (istat == 0) allocate (gdp%gdsedpar%rhosol(lsedtot), stat=istat)
    if (istat == 0) allocate (gdp%gdsedpar%namsed(lsedtot), stat=istat)
    if (istat == 0) allocate (gdp%gdsedpar%sedtyp(lsedtot), stat=istat)
    if (istat /= 0) then
       call prterr(lundia, 'P004', "dimsedconst: memory alloc error")
       call d3stop(1, gdp)
    endif
end subroutine dimsedconst
