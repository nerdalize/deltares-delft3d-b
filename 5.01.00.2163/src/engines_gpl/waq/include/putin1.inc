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

      COMMON/PUTIN1/DEATH(52),PHYT(52),BNUT(MN),DNUT(MN),BACKMU,BACKAD,
     1              TEMPMU,TEMPAD,SOLAMU,SOLACO,SOLAAD,DEPTMU,DEPTAD,
     2              DLGTMU,DLGTAD,NPER(10,3),NRUN,IMU,ML,MU,MI,LRUN,
     3              LSTOP,IYEAR
      REAL*8        DEATH,PHYT,BNUT,DNUT,BACKMU,BACKAD,
     1              TEMPMU,TEMPAD,SOLAMU,SOLACO,SOLAAD,DEPTMU,DEPTAD,
     2              DLGTMU,DLGTAD
      INTEGER       NPER,NRUN,IMU,ML,MU,MI,LRUN,LSTOP,IYEAR
      COMMON/PUTIN3/DATE(52),COM(18),CASE(13),CONTRO(20)
      CHARACTER*8 DATE,COM,CASE,CONTRO
C Section to be included in the following programs:
C BLMMAIN
C CHANGE
C CHHELP
C CSPGRO
C INPUT1
C INPUT2
C INPUT3
C MAXGRO
C OPTION
C RUN
C SETABC
