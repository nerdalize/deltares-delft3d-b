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

!     Save the local persistent variables in the derived type for reuse
!
      DLWQD%II        =  II
      DLWQD%IN        =  IN
      DLWQD%ITIME     =  ITIME
      DLWQD%IFFLAG    =  IFFLAG
      DLWQD%IAFLAG    =  IAFLAG
      DLWQD%IBFLAG    =  IBFLAG
      DLWQD%NDDIM     =  NDDIM
      DLWQD%NVDIM     =  NVDIM
      DLWQD%NOSSS     =  NOSSS
      DLWQD%NOQTT     =  NOQTT
      DLWQD%NOQT      =  NOQT
      DLWQD%NOPRED    =  NOPRED
      DLWQD%ITIMEL    =  ITIMEL
      DLWQD%LSTREC    =  LSTREC
      DLWQD%ITHANDL   =  ITHANDL
      DLWQD%LDUMMY    =  LDUMMY
      DLWQD%INWTYP    =  INWTYP

      DLWQD%OTIME     =  OTIME
      DLWQD%DELTIM    =  DELTIM
      DLWQD%TSCALE    =  TSCALE

      DLWQD%NOWARN    =  NOWARN
      DLWQD%IOPTZB    =  IOPTZB
      DLWQD%FORESTER  =  FORESTER
      DLWQD%UPDATR    =  UPDATR

      DLWQD%LLENG     =  LLENG

      DLWQD%TOL       =  TOL

      DLWQD%IWSTKIND  => IWSTKIND
      DLWQD%IEXSEG    => IEXSEG
      DLWQD%IKNMKV    => IKNMKV

!     DLWQD%GRIDPS    - no need!

      call copy_time_data( dlwqd, .true. )
