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

      SUBROUTINE DLWQTK ( LUN    , ITIME  , IKTIM  , IKNMRK , NOSEG  ,
     +                    IS     , LUNTXT , ISFLAG , IFFLAG , IFIOPK )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : december 1994 by Jan van Beek
C
C     FUNCTION            : Updates kenmerk array
C
C     LOGICAL UNITNUMBERS : LUN(IS) - input unit intermediate file
C                           LUN(19) - job-log output file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUN     INTEGER       *     INPUT   unit number intermediate file
C     ITIME   INTEGER       1     INPUT   Model timer
C     IKTIM   INTEGER       *     IN/OUT  Timers in file
C     IKNMRK  INTEGER   NOSEG,*   IN/OUT  Kenmerk array
C     NOSEG   INTEGER       1     INPUT   number of segments
C     IS      INTEGER       1     INPUT   Index number intermediate file
C     LUNTXT  CHAR*(*)      *     INPUT   text with the unit number
C     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
C     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
C     IFIOPK  INTEGER       1     IN/OUT  file option kenmerk array
C
C     DECLARATIONS        :
C
      use timers
      INTEGER       ITIME , NOSEG , IS    , ISFLAG, IFFLAG,
     +              IFIOPK, IKMRK1
      INTEGER       LUN(*)   , IKNMRK(NOSEG,*),
     +              IKTIM(*)

      INTEGER, ALLOCATABLE, DIMENSION(:) :: IOWN

      CHARACTER*(*) LUNTXT(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqtk", ithandl )

C
C     Allocate the work array
C
      ALLOCATE( IOWN(NOSEG) )
C
C
C     If time variable then get variable kenmerk array
C
      IF ( IFIOPK .GT. 0 ) THEN
         LUNOUT = LUN(19)
C
C        if first time open intermediate file and
C        move original kenmerk array (column 1) to constant kenmerk array
C        (column 2)
C
         IF ( IFFLAG .EQ. 1 ) THEN
            CALL DHOPNF ( LUN(IS) , LUNTXT(IS) , IS    , 2     , IERR )
            CALL DHIMOV ( IKNMRK(1,1), IKNMRK(1,2), NOSEG )
         ENDIF
C
C        evaluate file option; read time-dependent kenmerk array into column 3
C
         IF ( IFIOPK .EQ. 1 ) THEN
C
C           one record per time step
C
            CALL DLWQKV(LUN(IS)   , LUNOUT, ITIME , IKNMRK(1,3), NOSEG ,
     +                  LUNTXT(IS), ISFLAG, IFFLAG)
            IF ( IFFLAG .EQ. -1 ) THEN
               IFIOPK =  0
               IFFLAG =  1
               CLOSE ( LUN(IS) )
            ENDIF
C
         ELSEIF ( IFIOPK .EQ. 2 ) THEN
C
C           Block function
C
            CALL DLWQKB ( LUN(IS)    , LUNOUT     ,
     +                    ITIME      , IKTIM(1)   ,
     +                    IKTIM(2)   , IKTIM(3)   ,
     +                    IKNMRK(1,3), IKNMRK(1,4),
     +                    NOSEG      , LUNTXT(IS) ,
     +                    ISFLAG     , IFFLAG     )
C
         ELSE
C
C           Wrong option
C
            WRITE(LUNOUT,2000)
            CALL SRSTOP(1)
C
         ENDIF
C
C        Retrieve the ownership of segments from the constant kenmerk array
C        (column 2)
C
         DO 100 ISEG = 1 , NOSEG
            CALL DHKMRK(4,IKNMRK(ISEG,2),IKMRK4)
            IOWN(ISEG) = IKMRK4
  100    CONTINUE
C
C        Change the time-variable kenmerk-array (column 3) such that it
C        includes ownership of segments in parallel runs
C
         CALL CHKNMR ( LUN(19) , MYPART , NOSEG  , IOWN(1) , IKNMRK(1,3) )

C
C        OR the constant and the time variable array's
C
         DO 200 ISEG = 1 , NOSEG
            IKNMRK(ISEG,1) = IKNMRK(ISEG,2) + IKNMRK(ISEG,3)
  200    CONTINUE
C
      ENDIF
C
      DEALLOCATE( IOWN )

      if ( timon ) call timstop ( ithandl )
      RETURN
C
 2000 FORMAT ('ERROR: wrong file option for kenmerk array')
      END
