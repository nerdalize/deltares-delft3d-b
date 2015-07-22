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

      SUBROUTINE ZOEKNX ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : searches a string in an array
C                           searches not case sensetive
C                           Uses ICHAR() and expects ASCII char set
C                           a t/m z have codes 97 t/m 122
C                           A t/m Z have codes 65 t/m 90
C
C     SUBROUTINES CALLED  : -
C
C     PARAMETERS          : 5
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NAAM    CHAR*(*)      1     INPUT   string to be located
C     NOTOT   INTEGER       1     INPUT   number of elements in SYNAME
C     SYNAME  CHAR*(*)  NOTOT     INPUT   array to be searched in
C     NZOEK   INTEGER       1     INPUT   number of characters to be used
C                                         in the comparison
C     IAINDX  INTEGER       1     OUTPUT  index in SYNAME if found,
C                                         -1 if not found
C     Declaration of arguments
C
      INTEGER       NOTOT , NZOEK , IAINDX
      CHARACTER*(*) NAAM
      CHARACTER*(*) SYNAME(NOTOT)
C
      IAINDX = -1
      DO 100 I = 1,NOTOT
          DO 50 K = 1,NZOEK
              I1 = ICHAR(NAAM     (K:K))
              I2 = ICHAR(SYNAME(I)(K:K))
              IF (I1.GE. 97.AND.I1.LE.122) THEN
                  IF (I1.NE.I2.AND.I1.NE.(I2+32)) GOTO 100
              ELSEIF (I1.GE. 65.AND.I1.LE. 90) THEN
                  IF (I1.NE.I2.AND.I1.NE.(I2-32)) GOTO 100
              ELSE
                  IF (I1.NE.I2                  ) GOTO 100
              ENDIF
   50     CONTINUE
          IAINDX = I
          GOTO 200
  100 CONTINUE
      RETURN
  200 CONTINUE
      RETURN
      END
