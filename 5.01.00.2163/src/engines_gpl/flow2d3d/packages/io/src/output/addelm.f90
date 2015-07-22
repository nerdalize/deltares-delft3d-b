subroutine addelm(nefisgroup ,elmnms_new ,elmqty_new ,elmunt_new ,elmtps_new , &
                & nbytsg_new ,elmdes_new ,dm1        ,dm2        ,dm3        , &
                & dm4        ,dm5        ,dm6        ,lundia     ,gdp)
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
!  $Id: addelm.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/addelm.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Write element dimensions in array elmdms
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer :: nelmx
!
! Global variables
!
    integer     , intent(in)  :: dm1
    integer     , intent(in)  :: dm2
    integer     , intent(in)  :: dm3
    integer     , intent(in)  :: dm4
    integer     , intent(in)  :: dm5
    integer     , intent(in)  :: dm6
    integer     , intent(in)  :: lundia
    integer     , intent(in)  :: nbytsg_new
    integer     , intent(in)  :: nefisgroup
    character(*), intent(in)  :: elmdes_new
    character(*), intent(in)  :: elmnms_new
    character(*), intent(in)  :: elmqty_new
    character(*), intent(in)  :: elmtps_new
    character(*), intent(in)  :: elmunt_new
!
! Local variables
!
    integer  :: ie
    integer  :: istat  ! Help var. memory allocation
!
!! executable statements -------------------------------------------------------
!
    nelmx   => gdp%nefisio%nefiselem(nefisgroup)%nelmx
    !
    ! Do not use local pointers to the gdp structure
    ! when they are reallocated
    !
    !  Allocate memory: Update space allocated and set
    !                   element properties
    !
    istat = 0
    ie    = nelmx + 1
    !
    !  Element dimensions
    !
    call reallocP(gdp%nefisio%nefiselem(nefisgroup)%elmdms, (/ 6, ie /), stat=istat)
    if (istat==0) then
       gdp%nefisio%nefiselem(nefisgroup)%elmdms(1,ie) = dm1
       gdp%nefisio%nefiselem(nefisgroup)%elmdms(2,ie) = dm2
       gdp%nefisio%nefiselem(nefisgroup)%elmdms(3,ie) = dm3
       gdp%nefisio%nefiselem(nefisgroup)%elmdms(4,ie) = dm4
       gdp%nefisio%nefiselem(nefisgroup)%elmdms(5,ie) = dm5
       gdp%nefisio%nefiselem(nefisgroup)%elmdms(6,ie) = dm6
    endif
    !
    !  Element number of bytes
    !
    if (istat==0) call reallocP(gdp%nefisio%nefiselem(nefisgroup)%nbytsg, ie, stat=istat)
    if (istat==0) then
       gdp%nefisio%nefiselem(nefisgroup)%nbytsg(ie) = nbytsg_new
    endif
    !
    !  Element unit
    !
    if (istat==0) call reallocP(gdp%nefisio%nefiselem(nefisgroup)%elmunt, ie, stat=istat)
    if (istat==0) then
       gdp%nefisio%nefiselem(nefisgroup)%elmunt(ie) = elmunt_new
    endif
    !
    !  Element name
    !
    if (istat==0) call reallocP(gdp%nefisio%nefiselem(nefisgroup)%elmnms, ie, stat=istat)
    if (istat==0) then
       gdp%nefisio%nefiselem(nefisgroup)%elmnms(ie) = elmnms_new
    endif
    !
    !  Element quantity
    !
    if (istat==0) call reallocP(gdp%nefisio%nefiselem(nefisgroup)%elmqty, ie, stat=istat)
    if (istat==0) then
       gdp%nefisio%nefiselem(nefisgroup)%elmqty(ie) = elmqty_new
    endif
    !
    !  Element type
    !
    if (istat==0) call reallocP(gdp%nefisio%nefiselem(nefisgroup)%elmtps, ie, stat=istat)
    if (istat==0) then
       gdp%nefisio%nefiselem(nefisgroup)%elmtps(ie) = elmtps_new
    endif
    !
    !  Element description
    !
    if (istat==0) call reallocP(gdp%nefisio%nefiselem(nefisgroup)%elmdes, ie, stat=istat)
    if (istat==0) then
       gdp%nefisio%nefiselem(nefisgroup)%elmdes(ie) = elmdes_new
    endif
    !
    !  Check for memory error
    !
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Addelm: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    !  Update number of elements in group
    !
    nelmx = ie
end subroutine addelm
