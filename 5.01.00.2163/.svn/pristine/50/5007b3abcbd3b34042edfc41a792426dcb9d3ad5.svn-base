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

      SUBROUTINE OUTHIS ( IOHIS , NAMFIH, ITIME , MONAME, NODUMP,
     +                    IDUMP , DUNAME, NOTOT1, SYNAM1, CONC1 ,
     +                    NOTOT2, SYNAM2, CONC2 , INIT  )
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : may  1993  BY Jan van Beek
C
C     FUNCTION           : Writes history output
C
C     LOGICAL UNITS      : IOHIS = number of history file
C
C     SUBROUTINES CALLED : none
C
C     PARAMETERS         : 19
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     IOHIS   INTEGER  1           INPUT   unit number output file
C     NAMFIH  CHAR*(*) 1           INPUT   name output file
C     ITIME   INTEGER  1           INPUT   present time in clock units
C     MONAME  CHAR*40  4           INPUT   model identhification
C     NODUMP  INTEGER  1           INPUT   number of dump locations
C     IDUMP   INTEGER  NODUMP      INPUT   dump segment numbers
C     DUNAME  CHAR*20  NODUMP      INPUT   names of dump locations
C     NOTOT1  INTEGER  1           INPUT   number of vars in CONC1
C     SYNAM1  CHAR*20  NOTOT1      INPUT   names of vars in CONC1
C     CONC1   REAL     NOTOT1*?    INPUT   values
C     NOTOT2  INTEGER  1           INPUT   number of extra output vars
C     SYNAM2  CHAR*20  NOTOT       INPUT   names of extra vars
C     CONC2   REAL    NOTOT2,NX*NY INPUT   values for extra vars
C     INIT    INTEGER  1           IN/OUT  Initialize flag
C
C     Declaration of arguments
C
      use timers

      INTEGER       IOHIS , ITIME , NODUMP, NOTOT1, NOTOT2,
     +              INIT
      INTEGER       IDUMP(*)
      CHARACTER*(*) MONAME(4), NAMFIH
      CHARACTER*(*) DUNAME(*), SYNAM1(*), SYNAM2(*)
      REAL          CONC1(*) , CONC2(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "outhis", ithandl )
C
C     Initialize file
C
      IF ( INIT .EQ. 1 ) THEN
         INIT = 0
         WRITE (IOHIS) (MONAME(I),I=1,4)
         WRITE (IOHIS)  NOTOT1+NOTOT2,NODUMP
         WRITE (IOHIS) (SYNAM1(I),I=1,NOTOT1),(SYNAM2(I),I=1,NOTOT2)
         WRITE (IOHIS) (I,DUNAME(I),I=1,NODUMP)
      ENDIF
C
C     Perform output
C
      WRITE (IOHIS) ITIME,(
     +              (CONC1(K1+(IDUMP(J)-1)*NOTOT1),K1=1,NOTOT1),
     +              (CONC2(K2+(J-1)*NOTOT2)       ,K2=1,NOTOT2),
     +                                             J=1,NODUMP   )
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
