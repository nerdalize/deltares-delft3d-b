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

C    Date:       12 Dec 1989
C    Time:       14:08
C    Program:    CALEND.FOR
C    Version:    1.11
C    Programmer: ??????
C    Previous version(s):
C    1.10 -- 12 Dec 1989 -- 11:12 -- Operating System: DOS
C    1.9 -- 12 Dec 1989 -- 11:11 -- Operating System: DOS
C    1.8 -- 12 Dec 1989 -- 11:11 -- Operating System: DOS
C    1.7 -- 12 Dec 1989 -- 11:10 -- Operating System: DOS
C    1.6 -- 12 Dec 1989 -- 11:10 -- Operating System: DOS
C    1.5 -- 12 Dec 1989 -- 11:09 -- Operating System: DOS
C    1.4 -- 12 Dec 1989 -- 11:08 -- Operating System: DOS
C    1.3 -- 12 Dec 1989 -- 11:07 -- Operating System: DOS
C    1.2 -- 12 Dec 1989 -- 11:07 -- Operating System: DOS
C    1.1 -- 12 Dec 1989 -- 10:49 -- Operating System: DOS
C    1.0 -- 12 Dec 1989 -- 10:48 -- Operating System: DOS
C    0.0 -- 12 Dec 1989 -- 10:18 -- Operating System: DOS
C
C  *********************************************************************
C  *    SUBROUTINE TO OBAIN THE CURRENT SYSTEM DATE                    *
C  *********************************************************************
C
      SUBROUTINE CALEND (DATUM,ITERM)
      CHARACTER*1 DATUM(1)
      CHARACTER*8 MMDDYY
      INTEGER STOS, STOSH
C
C Call LAHEY internal routine to get date from DOS.
C
C Note: ITERM is not referenced here, but it is in the mainframe
C version of this routine!
C
c     CALL DATE (MMDDYY)
      MMDDYY=' '
C
C Construct character variable DATUM.
C
      IRC = STOS ('Date:',1,5,datum,lend)
      IRC = STOSH (MMDDYY,1,8,DATUM,LEND+2,LEND2)
      RETURN
      END
