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

      SUBROUTINE IBLBAL( NTYP_M, NTYP_A, ALGTYP, IPOINT)
C
C     Function : set common CBLBAL communication with balance routines
C     Jos van Gils, May 2011: bug fix for N-fixers and heterotrophs
C
C     NTYP_M    INPUT   Max number of types
C     NTYP_A    INPUT   Actual number of types
C     ALGTYP    INPUT   Characteristics per algae type
C     IPOINT    INPUT   pointers to bloom algea concentration array
C
      INTEGER         NTYP_M, NTYP_A
      INTEGER         IPOINT(NTYP_A)
      REAL            ALGTYP(0:20,NTYP_M)

C                     index  4 is NC-ratio
C                     index  5 is PC-ratio
C                     index 16 is NC-ratio detritus uptake
C                     index 17 is PC-ratio detritus uptake
C                     index 18 is NC-ratio N fixers
C
      INCLUDE 'cblbal.inc'
      INCLUDE 'sysa.inc'
C
      NTYPA2 = NTYP_A
      DO IALG = 1 , NTYP_A
         IBLSUB(IALG) = IPOINT(IALG) - ICONC + 1
C         NCRALG(IALG) = ALGTYP(4,IALG)
         NCRALG(IALG) = MAX(ALGTYP(4,IALG),0.0)
     J                + MAX(ALGTYP(16,IALG),0.0)
     J                + MAX(ALGTYP(18,IALG),0.0)
C         PCRALG(IALG) = ALGTYP(5,IALG)
         PCRALG(IALG) = MAX(ALGTYP(5,IALG),0.0)
     J                + MAX(ALGTYP(17,IALG),0.0)
      ENDDO
C
      RETURN
      END
      BLOCK DATA BBLBAL
C
C     Function : set common CBLBAL for the case no bloom
C
      INCLUDE 'cblbal.inc'
C
      DATA   NTYPA2 / 0 /
C
      END
