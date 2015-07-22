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

!     Restore the local persistent variables from the derived type
!
      IN        =  DLWQD%IN
      II        =  DLWQD%II
      ITIME     =  DLWQD%ITIME
      IFFLAG    =  DLWQD%IFFLAG
      IAFLAG    =  DLWQD%IAFLAG
      IBFLAG    =  DLWQD%IBFLAG
      NDDIM     =  DLWQD%NDDIM
      NVDIM     =  DLWQD%NVDIM
      NOSSS     =  DLWQD%NOSSS
      NOQTT     =  DLWQD%NOQTT
      NOQT      =  DLWQD%NOQT
      NOPRED    =  DLWQD%NOPRED
      ITIMEL    =  DLWQD%ITIMEL
      ITHANDL   =  DLWQD%ITHANDL
      LSTREC    =  DLWQD%LSTREC
      LDUMMY    =  DLWQD%LDUMMY
      INWTYP    =  DLWQD%INWTYP

      OTIME     =  DLWQD%OTIME
      DELTIM    =  DLWQD%DELTIM
      TSCALE    =  DLWQD%TSCALE

      NOWARN    =  DLWQD%NOWARN
      IOPTZB    =  DLWQD%IOPTZB
      FORESTER  =  DLWQD%FORESTER
      LLENG     =  DLWQD%LLENG
      UPDATR    =  DLWQD%UPDATR

      TOL       =  DLWQD%TOL

      IWSTKIND  => DLWQD%IWSTKIND
      IEXSEG    => DLWQD%IEXSEG
      IKNMKV    => DLWQD%IKNMKV

      call copy_time_data( dlwqd, .false. )
