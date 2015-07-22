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

      SUBROUTINE RD_TABR7 ( DEFFDS      ,
     +                      NO_VSTO_MAX , NO_VSTO     ,
     +                      R7_VID      , R7_SID      ,
     +                      R7_SCAL     , LUNREP      ,
     +                      IERROR      )
C
C     Deltares
C
C     CREATED            :  june 1999 by Jan van Beek
C
C     FUNCTION           :  Read TABLE_R7 group from NEFIS file
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
C     NO_VSTO_MAX  INT            I       maximum number of rows in table r7
C     NO_VSTO      INT            O       number of rows in table r7
C     R7_VID       CHA*10 NO_VSTO O       velocity identification
C     R7_SID       CHA*10 NO_VSTO O       substance identification
C     R7_SCAL      REAL   NO_VSTO O       scale factor
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
      INTEGER       NO_VSTO_MAX , NO_VSTO     ,
     +              LUNREP      , IERROR
      INTEGER       DEFFDS
      CHARACTER*10  R7_VID      (NO_VSTO_MAX)
      CHARACTER*10  R7_SID      (NO_VSTO_MAX)
      REAL          R7_SCAL(NO_VSTO_MAX)
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
      PARAMETER   ( NELEMS = 4 )
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
      DATA  GRPNAM  /'TABLE_R7'/
      DATA
     + (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I),
     +  I = 1 , NELEMS)
     +/'NO_VSTO'  ,'INTEGER'  , 4,1,1,'number of rows in table R7'     ,
     + 'R7_VID'   ,'CHARACTER',10,1,0,'velocity identification'        ,
     + 'R7_SID'   ,'CHARACTER',10,1,0,'substance identification'       ,
     + 'R7_SCAL'  ,'REAL'     , 4,1,0,'scale factor'                   /
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
     +                 BUFLEN , NO_VSTO  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_VSTO .GT. NO_VSTO_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of rows in table R7:',NO_VSTO
         WRITE(LUNREP,*) 'greater than maximum:',NO_VSTO_MAX
         IERROR = 1
         GOTO 900
      ENDIF
C
C     Set dimension of table
C
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_VSTO
      ENDDO
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(2)
      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(2),
     +                 UINDEX , 1        ,
     +                 BUFLEN , R7_VID   )
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
     +                 BUFLEN , R7_SID   )
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
     +                 BUFLEN , R7_SCAL  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C
  900 CONTINUE
      RETURN
C
      END
