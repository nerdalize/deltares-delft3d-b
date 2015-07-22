subroutine rdsed1(nr_sed    ,luninp    ,lundia    ,csoil     ,iopsus    , &
                & facdss    ,sedtyp    ,rhosol    ,seddia    ,salmax    , &
                & ws0       ,wsm       ,tcduni    ,flstcd    ,tceuni    , &
                & flstce    ,erouni    ,flsero    ,sdbuni    ,flsdbd    , &
                & cdryb     ,error     ,gdp       )
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
!  $Id: rdsed1.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rdsed1.f90 $
!!--description-----------------------------------------------------------------
!
! Read  sediment input version 1
! The first line in the file must be:
! * version 1
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
    include 'sedparams.inc'
!
! Global variables
!
    integer                                   :: iopsus
    integer                                   :: lundia !  Description and declaration in inout.igs
    integer                     , intent(in)  :: luninp
    integer                                   :: nr_sed
    logical                     , intent(out) :: error
    real(fp)                                  :: csoil  !  Description and declaration in tricom.igs
    real(fp)      , dimension(*)              :: cdryb  !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: erouni !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: facdss !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: rhosol !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: salmax !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: sdbuni !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: seddia !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: tcduni !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: tceuni !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: ws0    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(*)              :: wsm    !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(*)              :: sedtyp !  sediment type: 0=total/1=noncoh/2=coh
    character(256), dimension(*)              :: flsdbd !  Description and declaration in esm_alloc_char.f90
    character(256), dimension(*)              :: flsero !  Description and declaration in esm_alloc_char.f90
    character(256), dimension(*)              :: flstcd !  Description and declaration in esm_alloc_char.f90
    character(256), dimension(*)              :: flstce !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer       :: iocond
    integer       :: l
    integer       :: lenc
    character(4)  :: sedtype
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    !
    read (luninp, *, iostat = iocond) nr_sed, csoil, iopsus
    !
    lenc = 4
    do l = 1, nr_sed
       flstcd(l) = ' '
       flstce(l) = ' '
       flsero(l) = ' '
       sedtype   = ' '
       read (luninp, '(a)', iostat = iocond) sedtype
       if (iocond == 0) then
          call small(sedtype, lenc)
          if (index(sedtype, 'sand') == 1) then
              sedtyp(l) = SEDTYP_NONCOHESIVE_SUSPENDED
          elseif (index(sedtype, 'mud') == 1) then
              sedtyp(l) = SEDTYP_COHESIVE
          else
             error = .true.
             call prterr(lundia, 'U007', 'sediment type (must start with sand or mud)')
          endif
       endif
       read (luninp, *, iostat = iocond) rhosol(l)
       if (iocond == 0) read (luninp, *, iostat = iocond) seddia(l)
       if (iocond == 0) read (luninp, *, iostat = iocond) salmax(l)
       if (iocond == 0) read (luninp, *, iostat = iocond) ws0(l)
       if (iocond == 0) read (luninp, *, iostat = iocond) wsm(l)
       if (iocond == 0) read (luninp, *, iostat = iocond) tcduni(l)
       if (iocond /= 0) then
          backspace (luninp)
          read (luninp, '(a)', iostat = iocond) flstcd(l)
       endif
       if (iocond == 0) read (luninp, *, iostat = iocond) tceuni(l)
       if (iocond /= 0) then
          backspace (luninp)
          read (luninp, '(a)', iostat = iocond) flstce(l)
       endif
       if (iocond == 0) read (luninp, *, iostat = iocond) erouni(l)
       if (iocond /= 0) then
          backspace (luninp)
          read (luninp, '(a)', iostat = iocond) flsero(l)
       endif
       if (iocond == 0) read (luninp, *, iostat = iocond) cdryb(l)
       if (iocond == 0) read (luninp, *, iostat = iocond) sdbuni(l)
       if (iocond /= 0) then
          backspace (luninp)
          read (luninp, '(a)', iostat = iocond) flsdbd(l)
       endif
       if (iocond == 0) read (luninp, *, iostat = iocond) facdss(l)
    enddo
end subroutine rdsed1
