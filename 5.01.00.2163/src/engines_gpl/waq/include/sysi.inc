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


!     COMMON  /  SYSI  /    Timer characteristics

      INTEGER ITSTRT   !    Simulation start time ( scu )
      INTEGER ITSTOP   !    Simulation stop time ( scu )
      INTEGER IDT      !    Simulation time step ( scu )
      INTEGER ITFACT   !    factor between time scales
      INTEGER ISFLAG   !    date string flag ( 0 = no , 1 = yes )
      INTEGER INTSRT   !    Integration type
      INTEGER INTOPT   !    Integration options
      INTEGER IDSTRT   !    Dump start time ( scu )
      INTEGER IDSTOP   !    Dump stop time ( scu )
      INTEGER IDSTEP   !    Dump time step ( scu )
      INTEGER IHSTRT   !    History start time ( scu )
      INTEGER IHSTOP   !    History stop time ( scu )
      INTEGER IHSTEP   !    History time step ( scu )
      INTEGER IMSTRT   !    Monitoring start time ( scu )
      INTEGER IMSTOP   !    Monitoring stop time ( scu )
      INTEGER IMSTEP   !    Monitoring time step ( scu )
      INTEGER IVFLAG   !    Computed volumes flag ( 0 = no , 1 = yes )
      INTEGER ITFLAG   !    flag ( 0 = no , 1 = yes )
      INTEGER ILFLAG   !    var. length flag ( 0 = no , 1 = yes )
      INTEGER ISFACT   !    System timer in seconds ( if negative fractions of sec. )
      INTEGER ICFLAG   !    Closure error flag ( 0 = no , 1 = yes )

      REAL*8  OTIME    !    Time base in Julian time
      REAL*8  DELTIM   !    Time base in Julian time with time delay
      REAL*8  TSCALE   !    Scale factor between DELWAQ unit and seconds (T0-string)

      INTEGER, parameter :: IISIZE = 21  ! size of the integer common block

      COMMON  /  SYSI  /  ITSTRT , ITSTOP , IDT    , ITFACT , ISFLAG ,
     &                    INTSRT , INTOPT , IDSTRT , IDSTOP , IDSTEP ,
     &                    IHSTRT , IHSTOP , IHSTEP , IMSTRT , IMSTOP ,
     &                    IMSTEP , IVFLAG , ITFLAG , ILFLAG , ISFACT ,
     &                    ICFLAG
      COMMON  /  SYSIDR / OTIME  , DELTIM , TSCALE
