subroutine rdtrafrm(error, lsedtot, gdp)
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
!  $Id: rdtrafrm.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdtrafrm.f90 $
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
    real(fp)                         , pointer :: rhow
    real(fp)                         , pointer :: ag
    integer                          , pointer :: max_integers
    integer                          , pointer :: max_reals
    integer                          , pointer :: max_strings
    integer                          , pointer :: max_integers_settle
    integer                          , pointer :: max_reals_settle
    integer                          , pointer :: max_strings_settle
    integer                          , pointer :: npar
    character(256)   , dimension(:)  , pointer :: dll_function_settle
    character(256)   , dimension(:)  , pointer :: dll_name_settle
    integer(pntrsize), dimension(:)  , pointer :: dll_handle_settle
    integer          , dimension(:)  , pointer :: dll_integers_settle
    real(hp)         , dimension(:)  , pointer :: dll_reals_settle
    character(256)   , dimension(:)  , pointer :: dll_strings_settle
    character(256)   , dimension(:)  , pointer :: dll_usrfil_settle
    character(256)   , dimension(:)  , pointer :: dll_function
    integer(pntrsize), dimension(:)  , pointer :: dll_handle
    integer          , dimension(:)  , pointer :: dll_integers
    real(hp)         , dimension(:)  , pointer :: dll_reals
    character(256)   , dimension(:)  , pointer :: dll_strings
    character(256)   , dimension(:)  , pointer :: dll_usrfil
    character(256)   , dimension(:)  , pointer :: flstrn
    integer          , dimension(:)  , pointer :: iform
    character(256)   , dimension(:)  , pointer :: name
    real(fp)         , dimension(:,:), pointer :: par
    type (sv_eqtran)                 , pointer :: sveqtran
    integer                          , pointer :: lundia
    include 'trapar.inc'
!
! Global variables
!
    logical, intent(out) :: error
    integer, intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
!
! Local variables
!
    integer           :: i
    integer           :: istat
    integer           :: ll
    character(256)    :: errmsg
    character(256)    :: filtrn
!
!! executable statements -------------------------------------------------------
!
    rhow                 => gdp%gdphysco%rhow
    ag                   => gdp%gdphysco%ag
    max_integers         => gdp%gdeqtran%max_integers
    max_reals            => gdp%gdeqtran%max_reals
    max_strings          => gdp%gdeqtran%max_strings
    max_integers_settle  => gdp%gdeqtran%max_integers_settle
    max_reals_settle     => gdp%gdeqtran%max_reals_settle
    max_strings_settle   => gdp%gdeqtran%max_strings_settle
    npar                 => gdp%gdeqtran%npar
    dll_function         => gdp%gdeqtran%dll_function
    dll_handle           => gdp%gdeqtran%dll_handle
    dll_function_settle  => gdp%gdeqtran%dll_function_settle
    dll_name_settle      => gdp%gdeqtran%dll_name_settle
    dll_handle_settle    => gdp%gdeqtran%dll_handle_settle
    dll_integers_settle  => gdp%gdeqtran%dll_integers_settle
    dll_reals_settle     => gdp%gdeqtran%dll_reals_settle
    dll_strings_settle   => gdp%gdeqtran%dll_strings_settle
    dll_integers         => gdp%gdeqtran%dll_integers
    dll_reals            => gdp%gdeqtran%dll_reals
    dll_strings          => gdp%gdeqtran%dll_strings
    flstrn               => gdp%gdeqtran%flstrn
    iform                => gdp%gdeqtran%iform
    name                 => gdp%gdeqtran%name
    par                  => gdp%gdeqtran%par
    sveqtran             => gdp%gdeqtran
    lundia               => gdp%gdinout%lundia
    !
    istat = 0
    if (.not. associated(sveqtran%par)) then
                     allocate (sveqtran%par    (npar,lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%iform       (lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%flstrn      (lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%name        (lsedtot), stat = istat)
       !
       max_integers = MAX_IP
       max_reals    = MAX_RP
       max_strings  = MAX_SP
       if (istat==0) allocate (sveqtran%dll_handle  (lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%dll_function(lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%dll_usrfil  (lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%dll_integers(max_integers), stat = istat)
       if (istat==0) allocate (sveqtran%dll_reals   (max_reals   ), stat = istat)
       if (istat==0) allocate (sveqtran%dll_strings (max_strings ), stat = istat)
       !
       max_integers_settle =  5
       max_reals_settle    = 21
       max_strings_settle  =  2
       if (istat==0) allocate (sveqtran%dll_handle_settle  (lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%dll_name_settle    (lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%dll_function_settle(lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%dll_usrfil_settle  (lsedtot), stat = istat)
       if (istat==0) allocate (sveqtran%dll_integers_settle(max_integers_settle), stat = istat)
       if (istat==0) allocate (sveqtran%dll_reals_settle   (max_reals_settle   ), stat = istat)
       if (istat==0) allocate (sveqtran%dll_strings_settle (max_strings_settle ), stat = istat)
       !
       if (istat/=0) then
          call prterr(lundia, 'U021', 'RdTraFrm: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       par           => gdp%gdeqtran%par
       iform         => gdp%gdeqtran%iform
       flstrn        => gdp%gdeqtran%flstrn
       name          => gdp%gdeqtran%name
       !
       dll_function  => gdp%gdeqtran%dll_function
       dll_handle    => gdp%gdeqtran%dll_handle
       dll_usrfil    => gdp%gdeqtran%dll_usrfil
       dll_integers  => gdp%gdeqtran%dll_integers
       dll_reals     => gdp%gdeqtran%dll_reals
       dll_strings   => gdp%gdeqtran%dll_strings
       !
       dll_function  = ' '
       dll_handle    = 0
       dll_usrfil    = ' '
       dll_integers  = 0
       dll_reals     = 0.0_hp
       dll_strings   = ' '
       !
       dll_function_settle  => gdp%gdeqtran%dll_function_settle
       dll_name_settle      => gdp%gdeqtran%dll_name_settle
       dll_handle_settle    => gdp%gdeqtran%dll_handle_settle
       dll_usrfil_settle    => gdp%gdeqtran%dll_usrfil_settle
       dll_integers_settle  => gdp%gdeqtran%dll_integers_settle
       dll_reals_settle     => gdp%gdeqtran%dll_reals_settle
       dll_strings_settle   => gdp%gdeqtran%dll_strings_settle
       !
       dll_function_settle  = ' '
       dll_name_settle      = ' '
       dll_handle_settle    = 0
       dll_usrfil_settle    = ' '
       dll_integers_settle  = 0
       dll_reals_settle     = 0.0_hp
       dll_strings_settle   = ' '
       !
       do ll = 1, lsedtot
          !
          ! default is Van Rijn's 1993 formulation (iform=-1)
          !
          iform (ll) = -1
          flstrn(ll) = ' '
          do i = 1,npar
             par(i,ll) = 0.0_fp
          enddo
          !
          par( 1,ll) = ag
          par( 2,ll) = rhow       ! <- physco
          par( 5,ll) = 1.0E-6     ! rnu    uit md-tran.*
          par( 7,ll) = 0.05       ! za1    uit md-tran.*
          par( 8,ll) = 0.0        ! za2    uit md-tran.*
          par( 9,ll) = 1.0        ! da     uit md-tran.*
          par(10,ll) = 1.0        ! alfsta uit md-tran.*
          !
          dll_function_settle(ll) = ' '
          dll_name_settle(ll) = ' '
          dll_handle_settle(ll) = 0
       enddo
    endif
    !
    error = .false.
    !
    ! locate 'TraFrm' record for attribute file containing parameters
    ! for transport formulation
    !
    write (lundia, '(a)') '*** Start of transport formulation'
    !
    filtrn = ' '
    call prop_get_string(gdp%mdfile_ptr,'*','TraFrm',filtrn)
    if (filtrn == ' ') then
       !
       ! keyword not specified
       !
       errmsg = 'No transport formula given. Using Van Rijn 1993 by default.'
       call prterr(lundia    ,'G051'    ,errmsg    )
       goto 9999
    endif
    call rdtrafrm0(error  , iform(1)     , npar           , par(1,1), filtrn, &
                 & name(1), dll_handle(1), dll_function(1), dll_usrfil(1), gdp)
    do ll=2, lsedtot
       iform       (ll) = iform(1)
       name        (ll) = name(1)
       dll_handle  (ll) = dll_handle(1)
       dll_function(ll) = dll_function(1)
       dll_usrfil  (ll) = dll_usrfil(1)
       do i = 1,npar
          par(i,ll) = par(i,1)
       enddo
    enddo
    !
 9999 continue
    !
    write (lundia, '(a)') '*** End   of transport formulation'
    write (lundia, *)
    !
end subroutine rdtrafrm
