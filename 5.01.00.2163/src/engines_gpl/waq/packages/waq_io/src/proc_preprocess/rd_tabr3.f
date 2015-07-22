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

      SUBROUTINE RD_TABR3 ( DEFFDS      ,
     +                      NO_INPU_MAX , NO_INPU     ,
     +                      R3_PID      , R3_IID      ,
     +                      R3_NUMB     , R3_DEFY     ,
     +                      R3_DOC      , R3_SEX      ,
     +                      LUNREP      , IERROR      )
C
C     Deltares
C
C     CREATED            :  june 1999 by Jan van Beek
C
C     FUNCTION           :  Read TABLE_R3 group from NEFIS file
C
C     FILES              :  NEFIS file assumed opened
C
C     SUBROUTINES CALLED :
C
C     ARGUMENTS
C
C     NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     DEFFDS       INT    2993    I/O     Definition file descriptor
C     DATFDS       INT    999     I/O     Data file descriptor
C     NO_INPU      INT            I       number of rows in table r3
C     R3_PID       CHA*10 NO_INPU I       process identification
C     R3_IID       CHA*10 NO_INPU I       item identification
C     R3_NUMB      INT    NO_INPU I       serial number
C     R3_DEFY      CHA*1  NO_INPU I       default y/n
C     R3_DOC       CHA*1  NO_INPU I       documneted y/n
C     R3_SEX       INT    NO_INPU I       segment or exchange
C     LUNREP       INT    1       I       Unit number report file
C     IERROR       INT    1       O       Error
C
C     IMPLICIT NONE for extra compiler checks
C     SAVE to keep the group definition intact
C
      IMPLICIT NONE
      SAVE
C
C     declaration of arguments
C
      INTEGER       NO_INPU_MAX , NO_INPU     ,
     +              LUNREP      , IERROR
      INTEGER       DEFFDS
      CHARACTER*10  R3_PID      (NO_INPU_MAX)
      CHARACTER*10  R3_IID      (NO_INPU_MAX)
      INTEGER       R3_NUMB(NO_INPU_MAX)
      CHARACTER*1   R3_DEFY(NO_INPU_MAX)
      CHARACTER*1   R3_DOC (NO_INPU_MAX)
      INTEGER       R3_SEX (NO_INPU_MAX)
C
C     Local variables
C
C     GRPNAM  CHAR*16     1       LOCAL   group name (table)
C     NELEMS  INTEGER     1       LOCAL   number of elements in group (=cell)
C     ELMNMS  CHAR*16  NELEMS     LOCAL   name of elements on file
C     ELMTPS  CHAR*16  NELEMS     LOCAL   type of elements
C     ELMDMS  INTEGER  6,NELEMS   LOCAL   dimension of elements
C     NBYTSG  INTEGER  NELEMS     LOCAL   length of elements (bytes)
C
      INTEGER       NELEMS
      PARAMETER   ( NELEMS = 7 )
C
      INTEGER       I               , IELM          ,
     +              BUFLEN
      INTEGER       ELMDMS(2,NELEMS), NBYTSG(NELEMS),
     +              UINDEX(3)
      CHARACTER*16  GRPNAM
      CHARACTER*16  ELMNMS(NELEMS)  , ELMTPS(NELEMS)
      CHARACTER*64  ELMDES(NELEMS)
C
C     External NEFIS Functions
C
      INTEGER   GETELS
     +         ,GETELT
      EXTERNAL  GETELS
     +         ,GETELT
C
C     element names
C
      DATA  GRPNAM  /'TABLE_R3'/
      DATA
     + (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I),
     +  I = 1 , NELEMS)
     +/'NO_INPU'  ,'INTEGER'  , 4,1,1,'number of rows  in table R3'    ,
     + 'R3_PID'   ,'CHARACTER',10,1,0,'reference to prcocess'          ,
     + 'R3_IID'   ,'CHARACTER',10,1,0,'reference to item (input)'      ,
     + 'R3_NUMB'  ,'INTEGER'  , 4,1,0,'serial number in process'       ,
     + 'R3_DEFY'  ,'CHARACTER', 1,1,0,'use default yes/no'             ,
     + 'R3_DOC'   ,'CHARACTER', 1,1,0,'documneted yes/no'              ,
     + 'R3_SEX'   ,'INTEGER'  , 4,1,0,'segment/exchange indication'    /
C
C     Read group
C
C     WRITE(LUNREP,*) ' reading GROUP:',GRPNAM
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(1)
      BUFLEN = NBYTSG(1)*ELMDMS(2,1)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(1),
     +                 UINDEX , 1        ,
     +                 BUFLEN , NO_INPU  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_INPU .GT. NO_INPU_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of input items:',NO_INPU
         WRITE(LUNREP,*) 'greater than maximum:',NO_INPU_MAX
         IERROR = 1
         GOTO 900
      ENDIF
C
C     Set dimension of table
C
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_INPU
      ENDDO
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(2)
      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(2),
     +                 UINDEX , 1        ,
     +                 BUFLEN , R3_PID   )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(3)
      BUFLEN = NBYTSG(3)*ELMDMS(2,3)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(3),
     +                 UINDEX , 1        ,
     +                 BUFLEN , R3_IID   )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(4)
      BUFLEN = NBYTSG(4)*ELMDMS(2,4)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(4),
     +                 UINDEX , 1        ,
     +                 BUFLEN , R3_NUMB  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(5)
      BUFLEN = NBYTSG(5)*ELMDMS(2,5)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(5),
     +                 UINDEX , 1        ,
     +                 BUFLEN , R3_DEFY  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(6)
      BUFLEN = NBYTSG(6)*ELMDMS(2,6)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(6),
     +                 UINDEX , 1        ,
     +                 BUFLEN , R3_DOC   )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(7)
      BUFLEN = NBYTSG(7)*ELMDMS(2,7)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(7),
     +                 UINDEX , 1        ,
     +                 BUFLEN , R3_SEX   )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(7)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C
  900 CONTINUE
      RETURN
C
      END
