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

      INTEGER FUNCTION ANSWER (RSPONS,MAXLEN)
      CHARACTER*1 RSPONS(*)
      CHARACTER*3 YESNO(2)
      CHARACTER*255 TOKEN
      INTEGER MAXLEN,GETS,MATCH,UPRCAS,POSIT
      DATA YESNO /'YES','NO '/
C
      IF (MAXLEN .GT. 0 .OR. MAXLEN .LE. 255) GO TO 10
      ANSWER = -1
      RETURN
   10 POSIT = 1
      IF (GETS(RSPONS,POSIT,MAXLEN,255,TOKEN,LENTOK) .EQ. 0) GO TO 20
      ANSWER = -1
      RETURN
   20 CONTINUE
      IRC = UPRCAS (TOKEN,TOKEN,LENTOK)
      IF (MATCH(YESNO,2,3,TOKEN,LENTOK,0,NFND) .NE. 1) NFND = 3
      GO TO (30, 40, 50), NFND
   30 ANSWER = 1
      RETURN
   40 ANSWER = 0
      RETURN
   50 ANSWER = -1
      RETURN
      END
