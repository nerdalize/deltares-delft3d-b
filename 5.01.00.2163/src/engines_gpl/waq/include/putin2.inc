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

      COMMON/PUTIN2/SOLINT(52),TEMP(52),CONNUT(52,MN),BACKGR(52),
     1              DAYLGT(52),ZOOD(52,0:MG),DEPTH(52),IDEC(52)
      REAL*8        SOLINT,TEMP,CONNUT,BACKGR,
     1              DAYLGT,ZOOD,DEPTH
      INTEGER       IDEC
C 0895 MvdV dimension for more than one grazer type added to ZOOD
C Section to be included in the following programs:
C INPUT1
C INPUT3
C RUN
