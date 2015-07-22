subroutine nefisio_alloc(gdp)
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
!  $Id: nefisio_alloc.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/nefisio_alloc.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
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
!
! Global variables
!
!
! Local variables
!
    integer                          :: i
    integer                          :: m1
    integer                          :: m2
    integer, dimension(nrnefisfiles) :: nelems
!
!! executable statements -------------------------------------------------------
!
    nelems = 0
    nelems(nefisrdtimc)       = 8
    nelems(nefisrdtimw)       = 3
    nelems(nefisrstcom)       = 8
    nelems(nefisrwbotc)       = 3
    nelems(nefiswrboun)       = 8
    nelems(nefiswrcurt)       = 10
    nelems(nefiswrdwqt)       = 8
    nelems(nefiswrgrid)       = 19
    nelems(nefiswribot)       = 4
    nelems(nefiswridoc)       = 4
    nelems(nefiswridro)       = 9
    nelems(nefiswrihis)       = 0
    nelems(nefiswrimap)       = 0
    nelems(nefiswrkenc)       = 3
    nelems(nefiswrkent)       = 4
    nelems(nefiswrparm)       = 9
    nelems(nefiswrplot)       = 4
    nelems(nefiswrrouf)       = 3
    nelems(nefiswrspcp)       = 4
    nelems(nefiswrtdro)       = 2
    nelems(nefiswrthisinf)    = 0
    nelems(nefiswrthis)       = 0
    nelems(nefiswrtmapinf)    = 0
    nelems(nefiswrtmap)       = 0
    nelems(nefissetwav)       = 17
    nelems(nefiswrsedhinf)    = 0
    nelems(nefiswrsedh)       = 0
    nelems(nefiswrsedminf)    = 0
    nelems(nefiswrsedm)       = 0
    nelems(nefiswrsedmavginf) = 0
    nelems(nefiswrsedmavg)    = 0
    nelems(nefischkcom)       = 4
    nelems(nefiswrwavhinf)    = 0
    nelems(nefiswrwavh)       = 0
    nelems(nefiswrrolm)       = 12
    nelems(nefiswrihisdad)    = 0
    nelems(nefiswrthisdad)    = 0
    nelems(nefiswrihisdis)    = 4
    nelems(nefiswrthisdis)    = 17
    nelems(nefiswrcomwind)    = 0
    nelems(nefiswrsedwaqm)    = 0
    !
    allocate (gdp%nefisio)
    do i = 1, nrnefisfiles
       gdp%nefisio%nefiselem(i)%first  = .true.
       gdp%nefisio%nefiselem(i)%celidt = 1
       gdp%nefisio%nefiselem(i)%nelmx  = nelems(i)
       allocate (gdp%nefisio%nefiselem(i)%elmdms(6, nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%nbytsg(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmunt(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmnms(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmqty(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmtps(nelems(i)))
       allocate (gdp%nefisio%nefiselem(i)%elmdes(nelems(i)))
       do m1 = 1, nelems(i)
          do m2 = 1, 2
             gdp%nefisio%nefiselem(i)%elmdms(m2, m1) = 1
          enddo
          do m2 = 3, 6
             gdp%nefisio%nefiselem(i)%elmdms(m2, m1) = 0
          enddo
          gdp%nefisio%nefiselem(i)%nbytsg(m1) = 0
          gdp%nefisio%nefiselem(i)%elmunt(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmnms(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmqty(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmtps(m1) = ' '
          gdp%nefisio%nefiselem(i)%elmdes(m1) = ' '
       enddo
    enddo
end subroutine nefisio_alloc
