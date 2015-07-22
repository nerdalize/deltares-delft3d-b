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

C    Date:       4 Dec 1989
C    Time:       11:50
C    Program:    CVRBLM   FORTRAN
C    Version:    1.6
C    Programmer: Hans Los
C    Previous version(s):
C    1.5 -- 4 Dec 1989 -- 11:25 -- Operating System: CMS
C    1.4 -- 4 Dec 1989 -- 11:18 -- Operating System: CMS
C    1.3 -- 4 Dec 1989 -- 11:14 -- Operating System: CMS
C    1.2 -- 4 Dec 1989 -- 10:50 -- Operating System: CMS
C    1.1 -- 24 Oct 1989 -- 11:17 -- Operating System: CMS
C    1.0 -- 23 Oct 1989 -- 13:25
C    0.0 -- 3 Oct 1989 --  8:06
C
C  *********************************************************************
C  *  SUBROUTINE TO CONVERT UNITS BETWEEN BLOOM II AND ECOLUMN         *
C  *********************************************************************
C
C  *********************************************************************
C  *      SPECIAL ECOLUMN - BLOOM II PROGRAM VERSION                   *
C  *********************************************************************
C
C  This module converts some of the inputs of BLOOM II to enable the
C  program to use g/m3 rather than mg/m3 as basic concentration unit.
C  Notes:
C  1. No checks are made that all units are indeed correct! Thus it
C     is the user's responsibility to provide a consistent data set.
C  2. The output formats of BLOOM II are not changed. Thus some
C     numbers of some variables are printed in a rather akward format.
C     It is assumed, however, that ECOLUMN itself will handle all
C     essential BLOOM II outputs in the future.
C
      SUBROUTINE CVRBLM
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'dynam.inc'
C
C Assuming that concentration units in the calling program are g/m3,
C where as BLOOM II uses mg/m3, it is necessary to convert
C 1.  the specific extinction coefficients
C 2.  the carbon to chlorophyll ratio (and hence the dry weight to
C     chlorophyll ratio)
C of all phytoplankton types.
C The specific extinction coefficient of detritus.
C The base and top levels of the growth and mortality constraints.
C
      DO 10 I = 1, NUSPEC
         CHLTOC(I) = CHLTOC(I) * 1.0D-3
         CHLR(I)   = CHLTOC(I) * CTODRY(I)
         EKX(I)    = EKX(I)    * 1000.0D0
10    CONTINUE
      SPEXDE = SPEXDE * 1000.0D0
C     BIOBAS = BIOBAS * TSTEP * 1.0D-3
C     TOPLEV = TOPLEV * TSTEP * 1.0D-3
      BIOBAS = BIOBAS * 1.0D-3
      TOPLEV = TOPLEV * 1.0D-3
      RETURN
      END
