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

C    Date:       4 Jan 1994
C    Time:       19:51
C    Program:    PRINUN.FOR
C    Version:    1.3
C    Programmer: Hans Los
C    Previous version(s):
C    1.2 -- 21 Feb 1992 -- 11:52 -- Operating System: DOS
C    1.18 -- 9 Jan 1990 -- 08:08 -- Operating System: DOS
C    1.17 -- 2 Jan 1990 -- 07:41 -- Operating System: DOS
C    1.16 -- 2 Jan 1990 -- 07:39 -- Operating System: DOS
C    1.15 -- 29 Dec 1989 -- 20:22 -- Operating System: DOS
C    1.14 -- 29 Dec 1989 -- 20:16 -- Operating System: DOS
C    1.13 -- 29 Dec 1989 -- 20:14 -- Operating System: DOS
C    1.12 -- 19 Dec 1989 -- 16:42 -- Operating System: DOS
C
C Update 1.3: print names of constrains and detailed limiting factors.
C Update 1.2: write names of species, types and nutrients to output.
C
C  *********************************************************************
C  *   SUBROUTINE TO PRINT RELEVANT MODEL RESULTS IN UNFORMATTED FILE  *
C  *********************************************************************
C
C
      SUBROUTINE PRINUN (CDATE, TOTAL, PHYT, EXTTOT, EXLIVE, EXDEAD,
     1                   EXTB, T, CSOL, DAY, DEP , ZOOD,ZMAX,GRAMX)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'xvect.inc'
      INCLUDE 'postbl.inc'
      REAL*8 TOTAL, EXTTOT, EXLIVE, EXDEAD, ZOOD(0:MG)
      CHARACTER*8 CDATE
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
         FIRST = .FALSE.
         WRITE (IOU(26)) NUSPEC, NUECOG, NUNUCO, NUROWS, NUGRAZ
         WRITE (IOU(26)) (CSTRA(J), J = 1, NUNUCO)
         WRITE (IOU(26)) (SPNAME(J), J = 1, NUSPEC)
         WRITE (IOU(26)) (GRNAME(J), J = 1, NUECOG)
         WRITE (IOU(26)) (CNAMES(J), J = 1, NUROWS)
      END IF

      IF (GRAMX.GT.1000.) GRAMX = -999.
      IF (ZMAX.GT.1000.) THEN
        ZMAX = -999.
      ELSE
        ZMAX = ZMAX * GCTDRY(1)
      ENDIF
      WRITE (IOU(26))    NREP, CDATE, LIMIT,
     1                    (ISPLIM(J), J = 1, NUSPEC),
     1                    (PMAX(J),   J = 1, NUSPEC),
     2                    (RESP(J),   J = 1, NUSPEC),
     3                    (RMORT(J),  J = 1, NUSPEC),
     4                    (AROOT(J),  J = 2, 2*NUSPEC, 2),
     5                    (C(J),      J = 1, NUSPEC),
     6                    (XECO (K),  K = 1, NUECOG),
     7                    (XDEF (J),  J = NUROWS+1, NUROWS+NUSPEC),
     8                    XDEF (NUCOLS+2), TOTAL, PHYT,
     9                    (CONCEN (I), I = 1, NUNUCO),
     A                    (XDEF (I), I = 1, NUNUCO), EXTTOT, EXLIVE,
     B                    EXDEAD, EXTB, T, CSOL, DAY, DEP,
     C                    (ZOOD(IG)*GCTDRY(IG),IG=1,NUGRAZ),
     D                     ZMAX,GRAMX
      RETURN
      END
