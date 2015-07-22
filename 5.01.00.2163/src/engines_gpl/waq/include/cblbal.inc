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

C
C     COMMON BLOCK CBLBAL om te communiceren met de balansen van Jos van Gils
C
C     NTYPM2     dimensies arrays = max aantal algen , koppeling gebruikt 30 , bloom 30 dus maar 30 genomen
C     NTYPA2     actueel aantal algen = copy NTYP_A
C     IBLSUB     stofnummer bloomalgen
C     NCRALG     N-C ratio voor de algen = copy van ALGTYP(4,IALG)
C     PCRALG     P-C ratio voor de algen = copy van ALGTYP(5,IALG)
C
C
      PARAMETER (NTYPM2=30)
      INTEGER    NTYPA2
      INTEGER    IBLSUB(NTYPM2)
      REAL       NCRALG(NTYPM2)
      REAL       PCRALG(NTYPM2)
      COMMON    /CBLBAL/ NTYPA2,IBLSUB,NCRALG,PCRALG
