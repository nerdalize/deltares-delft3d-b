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

      INTEGER FUNCTION CMS (CMD, LENGTH)
      CHARACTER*1 CMD(*)
      CHARACTER*8 TOKEN,BLANK
      CHARACTER*3 CMSHLP
      INTEGER LENGTH, GETS, MATCH, IPOS, LPOS, LENTOK
      DATA BLANK /' '/
      DATA CMSHLP /'CMS'/
C
C DOS version of CMS (=system) function.
C If there is a command (CMD), it is stacked (unit 1). A bat program
C RUNCMS.BAT is invoked, which executes the command and stackes
C a carriage return to return to Fortran.
C Note: STACKDRV must be installed! Unit 1 must be assigend to STK!
C If there is no command, temporarily go to DOS and stay there
C until a nul line is entered.
C If first token = "CMS", strip it off.
C
      IPOS = 1
      IF (GETS(CMD,IPOS,LENGTH,8,TOKEN,LENTOK) . EQ. 0) THEN
         IF (MATCH (CMSHLP,1,3,TOKEN,LENTOK,0,NUM) .NE. 1) IPOS = 1
         LPOS = IPOS
         IF (GETS(CMD,IPOS,LENGTH,8,TOKEN,LENTOK) . NE. 0) GO TO 15
         WRITE (1,10) (CMD(I),I=LPOS,LENGTH)
10       FORMAT (' RUNCMS ',80A1)
      END IF
15    PAUSE 'Now you are in DOS ...'
      DO 20 I = 1,LENGTH
20    CMD(I) = BLANK
      CMS = 0
      RETURN
      END
