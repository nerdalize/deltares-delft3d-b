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

C     MODULE ZOEK
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : Search string in character array
C
C     SYMBOLS             : ZOEK  , searches a string in an array
C                           ZOEKNS, searches not case sensetive
C                           ZOEKCS, searches case sensetive
C                           SETZMO, sets search case sensetivety mode
C                           GETZMO, gives search case sensetivety mode
C                           BCZOEK, (block data) sets default search mode
C
      SUBROUTINE ZOEK ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
      USE Timers
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : searches a string in an array
C
C     SUBROUTINES CALLED  : ZOEKNS, searches not case sensetive
C                           ZOEKCS, searches case sensetive
C                           ERRSYS,
C
C     COMMON's            : CZOEK , search settings
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
C     COMMON's
C
      INTEGER          ICASEM
      COMMON / CZOEK / ICASEM
      SAVE   / CZOEK /
      integer(4) ithndl /0/
      if ( timon ) call timstrt( "zoek", ithndl )
C
C     Search with case sensitivity depending on ICASEM
C
      IF ( ICASEM .EQ. 0 ) THEN
         CALL ZOEKNS (NAAM,NOTOT,SYNAME,NZOEK,IAINDX)
      ELSEIF ( ICASEM .EQ. 1 ) THEN
         CALL ZOEKCS (NAAM,NOTOT,SYNAME,NZOEK,IAINDX)
      ELSE
         IAINDX = -1
         CALL ERRSYS ( 'ERROR IN ZOEK : ONBEKENDE MODE ' , 1 )
      ENDIF
C
      if ( timon ) call timstop( ithndl )
      RETURN
      END
C
      SUBROUTINE ZOEKNS ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
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
C
      SUBROUTINE ZOEKCS ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : searches a string in an array
C                           searches case sensetive
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
C
C     Loop over the array elements
C
      DO 100 I = 1,NOTOT
C
C        Direct comparison
C
         IF (NAAM(1:NZOEK).EQ.SYNAME(I)(1:NZOEK)) THEN
            IAINDX = I
            GOTO 200
         ENDIF
  100 CONTINUE
  200 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SETZMO (ICASST)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : sets search mode
C
C     SUBROUTINES CALLED  : -
C
C     COMMON's            : CZOEK , search settings
C
C     PARAMETERS          : 1
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ICASST  INTEGER       1     INPUT   search mode to be set
C
C     Declaration of arguments
C
      INTEGER    ICASST
C
C     COMMON's
C
      INTEGER          ICASEM
      COMMON / CZOEK / ICASEM
      SAVE   / CZOEK /
C
C     set ICASEM
C
      IF ( ICASST .GE. 0 .AND. ICASST .LE. 1 ) THEN
         ICASEM = ICASST
      ELSE
         CALL ERRSYS ( 'ERROR IN SETZMO : ONBEKENDE MODE ' , 1 )
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE GETZMO (ICASGT)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : gives search mode
C
C     SUBROUTINES CALLED  : -
C
C     COMMON's            : CZOEK , search settings
C
C     PARAMETERS          : 1
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ICASGT  INTEGER       1     OUTPUT  actual search mode
C
C     Declaration of arguments
C
      INTEGER    ICASGT
C
C     COMMON's
C
      INTEGER          ICASEM
      COMMON / CZOEK / ICASEM
      SAVE   / CZOEK /
C
      ICASGT = ICASEM
C
      RETURN
      END
C
      BLOCK DATA BCZOEK
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : set default search mode
C
C     COMMON's            : CZOEK , search settings
C
      INTEGER          ICASEM
      COMMON / CZOEK / ICASEM
      SAVE   / CZOEK /
C
      DATA   ICASEM / 0 /
      END
      SUBROUTINE ZOEK20 ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1993 by Jan van Beek
C
C     FUNCTION            : searches a string in an CHAR*20 array
C                           by declaring that array as CHAR*20 and
C                           then calling the normal ZOEK
C
C     SUBROUTINES CALLED  : ZOEK  , searches
C
C     PARAMETERS          : 5
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NAAM    CHAR*(*)      1     INPUT   string to be located
C     NOTOT   INTEGER       1     INPUT   number of elements in SYNAME
C     SYNAME  CHAR*20   NOTOT     INPUT   array to be searched in
C     NZOEK   INTEGER       1     INPUT   number of characters to be used
C                                         in the comparison
C     IAINDX  INTEGER       1     OUTPUT  index in SYNAME if found,
C                                         -1 if not found
C     Declaration of arguments
C
      INTEGER       NOTOT , NZOEK , IAINDX
      CHARACTER*(*) NAAM
      CHARACTER*20  SYNAME(NOTOT)
C
      CALL ZOEK ( NAAM  , NOTOT , SYNAME, NZOEK , IAINDX)
C
      RETURN
      END
