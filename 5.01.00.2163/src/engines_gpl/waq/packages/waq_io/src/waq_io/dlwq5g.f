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

      SUBROUTINE DLWQ5G ( LUNUT  , IAR    , ITMNR  , NOITM  , IDMNR  ,
     *                    NODIM  , IORDER , IIMAX  , CNAMES , IPOSR  ,
     *                    NPOS   , ILUN   , LCH    , LSTACK , CCHAR  ,
     *                    CHULP  , NOCOL  , DTFLG1 , DTFLG3 , ITFACT ,
     *                    ITYPE  , IHULP  , RHULP  , IERR   , iwar   )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : March '00  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : Checks if collumn header exists
C
C     SUBROUTINES CALLED : none
C
C     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
C                          LUN(29) = unit formatted output file
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LUNUT   INTEGER    1         INPUT   unit number for ASCII output
C     IAR     INTEGER  IIMAX       IN/OUT  integer   workspace
C     ITMNR   INTEGER    1         IN/OUT  nr of items for assignment
C     NOITM   INTEGER    1         IN      nr of items in computational rule
C     NOITS   INTEGER    1         IN      nr of items entries to be filled
C     IDMNR   INTEGER    1         IN/OUT  nr of subst for assignment
C     NODIM   INTEGER    1         IN      nr of subst in computational rule
C     NODIS   INTEGER    1         IN      nr of subst entries to be filled
C     IORDER  INTEGER    1         IN      1 = items first, 2 is subst first
C     IIMAX   INTEGER    1         INPUT   max. int. workspace dimension
C     CNAMES  CHAR*(*)  NITM       INPUT   Items to check for presence
C     IPOSR   INTEGER    1         IN/OUT  Start position on input line
C     NPOS    INTEGER    1         INPUT   nr of significant characters
C     ILUN    INTEGER   LSTACK     INPUT   unitnumb include stack
C     LCH     CHAR*(*)  LSTACK     INPUT   file name stack, 4 deep
C     LSTACK  INTEGER    1         INPUT   include file stack size
C     CCHAR   CHAR*1     1         INPUT   comment character
C     CHULP   CHAR*(*)   1         OUTPUT  space for limiting token
C     NOCOL   INTEGER    1         OUTPUT  number of collums in matrix
C     DTFLG1  LOGICAL    1         INPUT   True if time in 'date' format
C     DTFLG3  LOGICAL    1         INPUT   True if YYetc instead of DDetc
C     ITFACT  INTEGER    1         INPUT   factor between clocks
C     ITYPE   INTEGER    1         OUTPUT  type of info at end
C     IHULP   INTEGER    1         OUTPUT  parameter read to be transferred
C     RHULP   REAL       1         OUTPUT  parameter read to be transferred
C     IERR    INTEGER    1         OUTPUT  actual error indicator
C     Iwar    INTEGER    1         OUTPUT  cumulative warning count
C
C
      use timers       !   performance timers

      INTEGER       IIMAX
      CHARACTER*(*) LCH   (LSTACK) , CHULP , CNAMES(*)
      CHARACTER     CCHAR*1 , STRNG*8
      DIMENSION     IAR(*) , ILUN( LSTACK )
      LOGICAL       DTFLG1 , DTFLG3 , FIRST
      integer ( 8)  ihulp8
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq5g", ithndl )
C
C     Array offsets
C
      IOFFI = ITMNR + NOITM + IDMNR + NODIM
      IF ( IORDER .EQ. 1 ) THEN
         IOFFC = ITMNR + NOITM + IDMNR
         IOFFD = ITMNR + NOITM
         NITM  = NODIM
      ENDIF
      IF ( IORDER .EQ. 2 ) THEN
         IOFFC = IDMNR + NODIM + ITMNR
         IOFFD = IDMNR + NODIM
         NITM  = NOITM
      ENDIF
C
C     Read loop
C
      FIRST = .TRUE.
   20 ITYPE = 0
      CALL RDTOK1 ( LUNUT  , ILUN   , LCH    , LSTACK , CCHAR  ,
     *              IPOSR  , NPOS   , CHULP  , IHULP  , RHULP  ,
     *                                         ITYPE  , IERR   )
C          A read error
      IF ( IERR  .NE. 0 ) goto 9999
C          A string has arrived
      IF ( ITYPE .EQ. 1 ) THEN
         CALL DLWQ0T ( CHULP , ihulp, .FALSE., .FALSE., IERR )
         IF ( IERR .EQ. 0 ) THEN
            IERR = -2
            IF ( FIRST ) THEN
               goto 9999
            ELSE
               GOTO 50
            ENDIF
         ENDIF
         IF ( FIRST ) THEN
            FIRST = .FALSE.
            DO 10 I = 1 , NITM
               IAR(I+IOFFI) = 0
   10       CONTINUE
            NOCOL = 0
            WRITE ( LUNUT ,   *  )
         ENDIF
         NOCOL = NOCOL + 1
         STRNG = 'NOT used'
         DO 30 I = 1 , NITM
            CALL ZOEK(CHULP,1,CNAMES(IOFFC+I),20,IFOUND)
            IF ( IFOUND .GE. 1 ) THEN
               STRNG = 'used'
               IAR(I+IOFFI) = NOCOL
            ENDIF
   30    CONTINUE
   40    WRITE ( LUNUT , 1000 ) NOCOL, CHULP, STRNG
         GOTO 20
      ELSE
         IF ( ITYPE .EQ. 2 ) THEN
            CALL CNVTIM ( ihulp  , ITFACT, DTFLG1 , DTFLG3 )
         ENDIF
         IERR = -1
         IF ( FIRST ) goto 9999
      ENDIF
C
C       Is everything resolved ?
C
   50 ICNT = 0
      IODS = 0
      DO  70 I = 1 , NITM
         K = I - ICNT
         IF ( CNAMES(K+IOFFC) .EQ. '&$&$SYSTEM_NAME&$&$!') GOTO 70
         IF ( IAR(K+IOFFI) .GT. 0 ) GOTO 70
         CALL DLWQ5H ( LUNUT  , IAR    , ITMNR  , NOITM  , IDMNR  ,
     *                 NODIM  , IORDER , CNAMES , IOFFI  , IOFFC  ,
     *                          IODS   , IOFFD  , K      , ICNT   )
         iwar = iwar + 1
         IF ( I + ICNT .GE. NITM ) GOTO 9999
   70 CONTINUE
C
 9999 if (timon) call timstop( ithndl )
      RETURN
C
 1000 FORMAT ( ' Column:',I3,' contains: ',A40,' Status: ',A8)
C
      END
