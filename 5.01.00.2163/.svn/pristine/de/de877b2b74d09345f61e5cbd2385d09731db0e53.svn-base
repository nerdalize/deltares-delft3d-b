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

!!--version information---------------------------------------------------------
! $Author$
! $Date$
! $Revision$
!!--description-----------------------------------------------------------------
!
!   Delft-FSM (FORTRAN Shared Memory)
!   Definitions for FORTRAN programs
!
!   Irv.Elshoff@deltares.nl
!   1 aug 03
!
!   Jan Mooiman/Arjen Markus wrote:
!
!   Adresses and memory allocation are as follows
!   Memory layout:
!   bit pattern, every <x> is a byte
!   base adress is multiple of
!   memory needed for number (double needs 8 bytes, denoted by -)
!   type
!
!   xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxxx xxxx
!   16   4    8    4    16   4    8    4    16   4    8     4
!   ---- ---- --------- ------------------- --------- ----- ----
!   int  real double    double complex      complex   logi  char
!   4    4    8         16                  8         4     1
!
!
!   base        multiple of
!   ------------------------
!   loc(ibuf)   16              !!!!!!!!! This is a very important assumption
!   loc(rbof)    4
!   loc(dbuf)    8
!   loc(dcbuf)  16
!   loc(cbuf)   16
!   loc(lbuf)    8
!   loc(chbuf)   4
!
!   Example:
!   The length of a doble complex is 16, therefor the base adress for
!   double complex have to be a multiple of 16.
!
!-------------------------------------------------------------------------------


! API Functions

!     integer     makptr
!     external    makptr
!     integer     getptr
!     external    getptr
!     integer     relptr
!     external    relptr

! API Subroutines

    ! external    fsmini
    ! external    fsmend
    ! external    prtkey

! Type-related Definitions

!     integer          ibuf
!     real             rbuf
!     double precision dbuf
!     complex (kind=8) dcbuf
!     complex          cbuf
!     logical          lbuf
!     character*1      chbuf
!
      integer
     +  ityp,
     +  rtyp,
     +  dtyp,
     +  dctyp,
     +  ctyp,
     +  ltyp,
     +  chtyp


      parameter (
     +  ityp  = 1,
     +  rtyp  = 2,
     +  dtyp  = 3,
     +  dctyp = 4,
     +  ctyp  = 5,
     +  ltyp  = 6,
     +  chtyp = 7
     +   )


! Pseudo-storage for each data type

!     common /dynmem/
!    +  ibuf  (0:0),
!    +  rbuf  (0:0),
!    +  dbuf  (0:0),
!    +  dcbuf (0:0),
!    +  cbuf  (0:0),
!    +  lbuf  (0:0),
!    +  chbuf (0:0)


! Flag bits for FSMINI (coupled with definitions in "esm.h" and "fsm.h")

      integer
     +  FSM_SILENT,
     +  FSM_TRACE,
     +  FSM_THREADS,
     +  FSM_NOSHMEM,
     +  FSM_DUPKEYS,
     +  FSM_XXX

      parameter (
     +  FSM_SILENT      = 1,
     +  FSM_TRACE       = 2,
     +  FSM_THREADS     = 4,
     +  FSM_NOSHMEM     = 8,
     +  FSM_DUPKEYS     = 16,
     +  FSM_XXX         = 128
     +  )
