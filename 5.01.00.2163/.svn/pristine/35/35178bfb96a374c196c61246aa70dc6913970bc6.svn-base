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

      SUBROUTINE STRIP ( LUNIN  , LFILE  , LUNUT  , LUREP  , NPOS   ,
     *                                              CCHAR  , VRSION )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : march '88  BY  M.E.Sileon
C
C     FUNCTION            : Deletes empty lines and comment in the
C                           DELWAQ input file
C
C     SUBROUTINE CALLED   : SRSTOP
C
C     LOGICAL UNITNUMBERS : LUNIN = unitnumber auxilary input file
C                           LUNUT = unitnumber scratch file
C                           LUREP = unitnumber error messages
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LFILE   CHAR*?     1         INPUT   file name
C     NPOS    INTEGER    1         INPUT   number of significant
C                                          positions in one line
C     CCHAR   CHAR*1     1         INPUT   'comment' indicator
C     VRSION  REAL*4     1         OUTPUT  Version number
C
C
      CHARACTER*2000 CAR
      CHARACTER*1    CCHAR
      CHARACTER*(*)  LFILE
      LOGICAL        EMPTY, STRING
C
      IF ( NPOS .GT. 2000 ) GOTO 60
C
      DO 30 IL = 1 , 1000000
      READ ( LUNIN , '(A)' , END=40 , ERR=50 ) CAR  (1:NPOS)
      EMPTY = .TRUE.
      STRING = .FALSE.
C
C        write line until CCHAR in LUNUT, strip heading spaces
C
      DO 5 I  = 1 , NPOS-19
         IF ( CAR(I:I+14) .EQ. 'DELWAQ_VERSION_' ) THEN
            READ ( CAR(I+15:I+20) , '(F5.0)' ) VERS2
            IF ( VERS2 .GT. VRSION ) THEN
               VRSION = VERS2
               WRITE ( * , '(A,A,F5.3)' ) '       ---------->',
     *                   ' Version number of the input file is: ',VERS2
            ENDIF
            CAR(I:I+19) = ' '
         ENDIF
    5 CONTINUE
C
C        write line until CCHAR in LUNUT, strip heading spaces
C
      IS = 0
      DO 10 I  = 1 , NPOS
      IF ( CAR  (I:I) .EQ. CCHAR ) GOTO 20
      IF ( CAR  (I:I) .NE. ' '  ) EMPTY = .FALSE.
      IF ( .NOT. EMPTY ) THEN
         IF ( CAR  (I:I) .EQ. '''' .OR. CAR  (I:I) .EQ. '"' ) THEN
            IF ( STRING ) THEN
               STRING = .FALSE.
            ELSE
               STRING = .TRUE.
            ENDIF
         ENDIF
         IF ( CAR  (I-1:I) .NE. '  ' .OR. STRING ) THEN
            IS = IS + 1
            CAR  (IS:IS) = CAR  (I:I)
         ENDIF
      ENDIF
   10 CONTINUE
   20 IF ( .NOT. EMPTY ) WRITE( LUNUT , '(A)' )  CAR  (1:IS)
   30 CONTINUE
C
C        end of file encountered
C
   40 REWIND LUNUT
      RETURN
C
C        errors during read
C
   50 WRITE ( LUREP , 2000 ) LUNIN , LFILE
      CALL SRSTOP(1)
C
C        work string to short
C
   60 WRITE ( LUREP , 2001 ) NPOS
      CALL SRSTOP(1)
C
 2000 FORMAT (  ' ERROR, reading file on unit',I3,' !!',
     *         /' Filename is: ',A20,
     *         /' ********** EXECUTION TERMINATED ********' )
 2001 FORMAT (  ' ERROR, number of characters per line exceed maximum',
     *         /' In file ',I4,' , maximum is 1000 ' )
C
      END
