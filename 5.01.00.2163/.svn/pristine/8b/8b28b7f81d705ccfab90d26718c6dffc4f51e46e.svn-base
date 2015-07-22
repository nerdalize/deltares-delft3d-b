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

      SUBROUTINE DHGNAM ( NAME   , CHECK )
C
C     Deltares
C
C     CREATED           : Jan 2003 by Jan van Beek
C
C     FUNCTION          : Reads the input filename from keyboard /command (ini file?)
C
C     SUBROUTINE CALLED : DHGARG
C                         SRSTOP
C
C     PARAMETERS        :
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     NAME    CHAR*(*)   *         OUT     file name input file
C     CHECK   CHAR*(*)   *         IN      if not empty then check existance of file with this extension
C                                          when name is enterd interactive from keyboard
C
      CHARACTER*(*) NAME
      CHARACTER*(*) CHECK
C
C     Local
C
      CHARACTER*4   EXTS
      CHARACTER*9   EXTS9,FILRUN
      CHARACTER*256 CHECK2
      CHARACTER*3   ANSWER
      LOGICAL       EXI  ,LFOUND
      CHARACTER*1   CRJV  , CJVB  , CDUMMY
      CRJV = '/'
      CJVB = '\\'
C
      MAXNAM = LEN(NAME)
C
C     first argument from command line if it is not a option starting with -
C
      CALL DHGARG ( 1     , NAME  )

C     write(*,*) 'AM: ',trim(name)
C
C     Get filename from steering file Delft3D
C
      IF ( NAME .EQ. ' ' .OR. NAME(1:1) .EQ. '-' ) THEN
         FILRUN = ' '
         CALL GETCOM ( '-waq' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.waq'
         ENDIF
         CALL GETCOM ( '-sed' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.sed'
         ENDIF
         CALL GETCOM ( '-eco' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.eco'
         ENDIF
         CALL GETCOM ( '-chem' , 0    , LFOUND, IDUMMY, RDUMMY,
     +              CDUMMY, IERR )
         IF ( LFOUND ) THEN
            FILRUN = 'runid.chm'
         ENDIF
         IF ( FILRUN .NE. ' ' ) THEN
            INQUIRE ( FILE = FILRUN , EXIST = EXI )
            IF ( EXI ) THEN
               OPEN(9,FILE=FILRUN)
               READ(9,'(A)',IOSTAT=IOERR) NAME
               IF ( IOERR .NE. 0 ) NAME = ' '
               CLOSE(9)
            ENDIF
         ENDIF
      ENDIF
C
C     Get filename filename from keyboard
C
      IF ( NAME .EQ. ' ' .OR. NAME(1:1) .EQ. '-' ) THEN
         WRITE (  6  ,  *  ) ' Name of the model files ? '
         WRITE (  6  ,  *  ) ' DELWAQ will provide the extensions. '
         READ  (  5  , '(A)' )   NAME
C
C        remove quotes if they are the first and the last
C
         IF ( NAME(1:1) .EQ. '''' ) THEN
            INDX = INDEX ( NAME(2:),'''' )
            IF ( INDX .NE. 0 ) THEN
               IF ( INDX .EQ. MAXNAM-1 ) THEN
                  NAME(INDX+1:) = ' '
                  NAME = NAME(2:)
               ELSEIF ( NAME(INDX+2:) .EQ. ' ' ) THEN
                  NAME(INDX+1:) = ' '
                  NAME = NAME(2:)
               ENDIF
            ENDIF
         ENDIF
C
C        If empty string then stop
C
         IF ( NAME .EQ. ' ' ) THEN
            WRITE (  6  ,  *  ) ' ERROR no filename entered!'
            CALL SRSTOP(1)
         ENDIF
C
C        check existence of output file
C
         IF ( CHECK .NE. ' ' ) THEN
            INDX  = INDEX ( NAME , ' ' )
            IF ( INDX .EQ. 0 ) INDX = MAXNAM + 1
            CHECK2 = NAME(1:INDX-1)//CHECK
            INQUIRE ( FILE = CHECK2 , EXIST = EXI )
C
            IF ( EXI ) THEN
               WRITE ( 6 , * ) ' File:',CHECK2(1:INDX+3),
     +                         ' already exist.'
               WRITE ( 6 , * ) ' Do you want it to be replaced ? '
               WRITE ( 6 , * ) ' Answer yes or no ? '
               READ  ( 5 , '(A3)' ) ANSWER
               IF ( ANSWER(1:1) .EQ. 'N' .OR.  ANSWER(1:1) .EQ. 'n' )
     *              CALL SRSTOP ( 1 )
            ENDIF
         ENDIF
C
      ELSE
C
C           if there is remove extension
C
         DO 7 INDX1 = MAXNAM , 1 , -1
            IF ( NAME(INDX1:INDX1) .EQ. CRJV) GOTO 8
            IF ( NAME(INDX1:INDX1) .EQ. CJVB) GOTO 8
    7    CONTINUE
    8    CONTINUE
         IF ( INDX1 .EQ. 0 ) INDX1 = 1
         INDX2 = INDEX ( NAME(INDX1:), '.', .TRUE. )
         IF ( INDX2 .GT. 0 ) NAME(INDX1+INDX2-1:MAXNAM) = ' '
C
      ENDIF
C
      RETURN
      END
