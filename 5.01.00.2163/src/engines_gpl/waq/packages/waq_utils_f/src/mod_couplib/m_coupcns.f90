!!  Copyright(C) Stichting Deltares, 2012.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!-- MODULE m_coupcns ---------------------------------------------------------
!-- DESCRIPTION --------------------------------------------------------------
!
!   Purpose:
!   Declaration of general constants for the CouPLib communication library
!
!-- VERSION HISTORY ----------------------------------------------------------
!
!   $URL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/m_coupcns.f90 $
!   $Revision: 42 $, $Date: 2007-11-26 15:20:20 +0100 (Mon, 26 Nov 2007) $
!
!   Programmer: Edwin Vollebregt (VORtech)
!
!   Version 1.0  30-11-2007  initial version
!-----------------------------------------------------------------------------
module m_coupcns
implicit none
private

! constants

! IDUNNO   general code for unknown information
! IUNITY   code for unity relation table/mapping
! IVOID    general code for "not applicable" or "empty"

integer, parameter, public :: IDUNNO = -111
integer, parameter, public :: IVOID  = -222
integer, parameter, public :: IUNITY = -333

! global communication operations:
! CP_SUM     sum of values per process
! CP_MAX     maximum of input-values per process
! CP_MIN     minimum of input-values per process
! CP_MAX1    for 2D input-data: for each column select the instance with
!            maximum value in 1st entry
integer, parameter, public :: CP_SUM    = -123
integer, parameter, public :: CP_MAX    = -234
integer, parameter, public :: CP_MIN    = -345
integer, parameter, public :: CP_MAX1   = -567

! behaviour w.r.t. error messages:
! IFATAL   abort on errors
! IWARN    warn on errors but continue as much as possible
! INONE    silently ignore errors as much as possible

integer, parameter, public :: INONE  =   0
integer, parameter, public :: IWARN  = -11
integer, parameter, public :: IFATAL = -22

! logical unit-number of stdout (LOUT may be redirected to a file, * is not)
! this variable may be changed by init_couplib
integer,            public :: LOUT   = 6

! length of character strings used in CouPLib:
integer, parameter, public :: STRLEN = 32

! size of a real and double precision variable in number of integers needed
integer, parameter, public :: BYTES_P_INT = 4
integer, parameter, public :: SIZE_REAL = 1
integer, parameter, public :: SIZE_DBLE = 2

! numbering for different communication operations and columns of statistics
! arrays
integer, parameter, public :: IDSTRBC = 1
integer, parameter, public :: IDSTRBX = 2
integer, parameter, public :: ICOLLCT = 3
integer, parameter, public :: IUPDATE = 4
integer, parameter, public :: IACCUM  = 5
integer, parameter, public :: ICOMBIN = 6
integer, parameter, public :: ISYNCHR = 7
integer, parameter, public :: NUMCOMM = ISYNCHR

character(len=STRLEN), dimension(:), public :: namcomm(NUMCOMM) = (/ &
        'distrib(lengt)', 'distrib(ixset)', 'collect_data', 'update_data', &
        'accumulate', 'glob.combine', 'sync_processes' /)

integer, parameter, public :: ICOMMTM = 1
integer, parameter, public :: ISYNCTM = 2

integer, parameter, public :: ISEND   = 1
integer, parameter, public :: IRECV   = 2

! initial length of tables, e.g. table of index-sets; the tables are
! re-allocated with size*GROW_LENGTH each time when their capacity is exceeded.
integer, parameter, public :: INIT_LENGTH=20
real, parameter, public    :: GROW_LENGTH=1.6

! numbers of timers that are used for CouPLib routines
logical,            public :: use_timers = .false.
integer,            public :: itimer_couplib_other = 0
integer,            public :: itimer_commop(2,NUMCOMM) = 0
integer,            public :: itimer_couplib_ixset = 0
integer,            public :: itimer_couplib_intfc = 0
integer,            public :: numtimers_couplib = 2*NUMCOMM + 3
logical,            public :: measure_idletime = .false.

! contains

end module m_coupcns
