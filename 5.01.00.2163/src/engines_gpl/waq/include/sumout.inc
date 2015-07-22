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

      COMMON/SUMOU1/PARDIS(12),XST(MX),BIOST,XSUM(MT),CHLSUM,OBSSUM,
     1              CHI2
      COMMON/SUMOU2/LIMIT,LIMNAM(MN+3)
      COMMON/SUMOU3/NOBS,NUNU2,NUNU4,NTS6,NTS7,NTS14,INTST,LST,NPRINT,
     1              LDOM,LGBASE,LPRINT,ISDUMP,ISDPER(2)
      REAL*8 PARDIS, XST, BIOST, XSUM, CHLSUM, OBSSUM, CHI2
      INTEGER       NOBS,NUNU2,NUNU4,NTS6,NTS7,NTS14,INTST,LST,NPRINT,
     1              LDOM,LGBASE,LPRINT,ISDUMP,ISDPER

      CHARACTER*18 LIMIT
      CHARACTER*3  LIMNAM
C Section to be included in the following programs:
C BLOOM
C DOMINA
C FIXINF
C HEADIN
C INPUT2
C OPTION
C PRINSU
C PRINT6
C RECORD
C RUN
