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

!
!     COMMON  /  SYSI  /    Timer characteristics
!
!     ITSTRT  INTEGER  1    Simulation start time ( scu )
!     ITSTOP  INTEGER  1    Simulation stop time ( scu )
!     IDT     INTEGER  1    Simulation time step ( scu )
!     ITFACT  INTEGER  1    factor between time scales
!     ISFLAG  INTEGER  1    flag ( 0 = no , 1 = yes )
!     INTSRT  INTEGER  1    Integration option
!     INTOPT  INTEGER  1    Special options
!     IDSTRT  INTEGER  1    Dump start time ( scu )
!     IDSTOP  INTEGER  1    Dump stop time ( scu )
!     IDSTEP  INTEGER  1    Dump time step ( scu )
!     IHSTRT  INTEGER  1    History start time ( scu )
!     IHSTOP  INTEGER  1    History stop time ( scu )
!     IHSTEP  INTEGER  1    History time step ( scu )
!     IMSTRT  INTEGER  1    Monitoring start time ( scu )
!     IMSTOP  INTEGER  1    Monitoring stop time ( scu )
!     IMSTEP  INTEGER  1    Monitoring time step ( scu )
!     IVFLAG  INTEGER  1    Computed volumes flag ( 0 = no , 1 = yes )
!     ITFLAG  INTEGER  1    flag ( 0 = no , 1 = yes )
!     ILFLAG  INTEGER  1    var. length flag ( 0 = no , 1 = yes )
!     ISFACT  INTEGER  1    System timer in seconds
!     ICFLAG  INTEGER  1    Closure error flag ( 0 = no , 1 = yes )
!     OTIME   REAL*8   1    Time base in Julian time
!     DELTIM  REAL*8   1    Time base in Julian time with time delay
!     TSCALE  REAL*8   1    Scale factor between DELWAQ unit and seconds (T0-string)
!
      INTEGER             IISIZE
      INTEGER             ITSTRT , ITSTOP , IDT    , ITFACT , ISFLAG ,          &
                          INTSRT , INTOPT , IDSTRT , IDSTOP , IDSTEP ,          &
                          IHSTRT , IHSTOP , IHSTEP , IMSTRT , IMSTOP ,          &
                          IMSTEP , IVFLAG , ITFLAG , ILFLAG , ISFACT ,          &
                          ICFLAG
      REAL*8              OTIME  , DELTIM , TSCALE
      COMMON  /  SYSI  /  ITSTRT , ITSTOP , IDT    , ITFACT , ISFLAG ,          &
                          INTSRT , INTOPT , IDSTRT , IDSTOP , IDSTEP ,          &
                          IHSTRT , IHSTOP , IHSTEP , IMSTRT , IMSTOP ,          &
                          IMSTEP , IVFLAG , ITFLAG , ILFLAG , ISFACT ,          &
                          ICFLAG
      COMMON  /  SYSIDR / OTIME  , DELTIM , TSCALE
!
      PARAMETER ( IISIZE  = 21 )

