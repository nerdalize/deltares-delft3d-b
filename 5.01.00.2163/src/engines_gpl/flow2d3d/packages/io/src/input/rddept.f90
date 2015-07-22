subroutine rddept(lundia    ,error     , &
                & fildep    ,fmtdep    ,depuni    ,mmax      , &
                & nmax      ,nmaxus    ,dp        ,gdp       )
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
!  $Id: rddept.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/input/rddept.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Reads the depth records from the MD-file:
!                FILDEP, FMTDEP & DEPUNI
!              - DP-array (r(ja(2))) is filled by either reading
!                the array contents from the FILDEP-file or by
!                filling them with DEPUNI
! Method used:
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
!
! Global variables
!
    integer                                                                        :: lundia !  Description and declaration in inout.igs
    integer                                                                        :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                                        :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                         , intent(out) :: depuni
    real(fp)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: dp     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                   :: fildep !!  File name for variable depth values
    character(2)                                                                   :: fmtdep !!  File format definition for depth file
!
! Local variables
!
    integer       :: m       ! Help loop var. 
    integer       :: n       ! Help loop var.
    real(sp)      :: rval 
    logical       :: lerror  ! Flag=TRUE if a local error is encountered 
    character(11) :: fmttmp  ! Help variable for file format 
!
!! executable statements -------------------------------------------------------
!
    depuni = real(gdp%gdconst%amiss,fp)
    !
    ! locate 'Fildep' record for depth values in extra input file
    !
    fildep = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Fildep', fildep)
    if (fildep /= ' ') then
       !
       ! depth values in file
       ! locate 'Fmtdep' record for format definition of input file
       !
       fmtdep = 'FR'
       call prop_get_string(gdp%mdfile_ptr, '*', 'Fmtdep', fmtdep)
       fmttmp = fmtdep
       call filfmt(lundia    ,'Fmtdep'      ,fmttmp    ,lerror    ,gdp       )
       call depfil(lundia    ,error     ,fildep    ,fmttmp    ,mmax      , &
                 & nmaxus    ,dp        ,1         ,1         ,gdp       )
    else
       !
       ! No depth values in file
       ! locate 'Depuni' record for depuni
       !
       rval = real(gdp%gdconst%amiss,sp)
       call prop_get_real(gdp%mdfile_ptr, '*', 'Depuni', rval) 
       depuni = real(rval,fp)
       if (comparereal(depuni, real(gdp%gdconst%amiss,fp)) == 0) then
          depuni = 0.0_fp
          call prterr(lundia, 'U190', 'No depth specification')
          write(lundia,'(10x,a,f7.3)') 'Using Depuni = ', depuni
       endif
       !
       ! write per nmaxus mmax depuni in dp array only if noui = .true.
       !
       do m = 1, mmax
          do n = 1, nmaxus
             dp(n, m) = depuni
          enddo
       enddo
    endif
end subroutine rddept
