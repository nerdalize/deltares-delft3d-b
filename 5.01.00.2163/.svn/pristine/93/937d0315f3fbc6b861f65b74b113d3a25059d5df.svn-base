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

      SUBROUTINE RD_TABP2 ( DEFFDS      ,
     +                      NO_ITEM_MAX , NO_ITEM     ,
     +                      ITEM_ID     , ITEM_NAME   ,
     +                      ITEM_UNIT   , ITEM_DEFAULT,
     +                      ITEM_AGGREGA, ITEM_DISAGGR,
     +                      ITEM_GROUPID, ITEM_SEGX   ,
     +                      ITEM_WK     , LUNREP      ,
     +                      IERROR      )
C
C     Deltares
C
C     CREATED            :  june 1999 by Jan van Beek
C
C     FUNCTION           :  Read TABLE_P2 group from NEFIS file
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
C     NO_ITEM_MAX  INT            I       maximum number of items
C     NO_ITEM      INT            0       number of items
C     ITEM_ID      CHA*10 NO_ITEM 0       unique item identification
C     ITEM_NAME    CHA*50 NO_ITEM 0       item name
C     ITEM_UNIT    CHA*20 NO_ITEM 0       unit
C     ITEM_DEFAULT REA    NO_ITEM 0       default value
C     ITEM_AGGREGA CHA*10 NO_ITEM 0       variable used for aggregation
C     ITEM_DISAGGR CHA*10 NO_ITEM 0       variable used for dis-aggregation
C     ITEM_GROUPID CHA*30 NO_ITEM 0       subtance group ID
C     ITEM_SEGX    CHA*1  NO_ITEM 0       segment / exchange indication
C     ITEM_WK      CHA*1  NO_ITEM 0       active / inactive indication
C     LUNREP       INT    1       I       Unit number report file
C     IERROR       INT    1       0       Error
C
C     IMPLICIT NONE for extra compiler checks
C     SAVE to keep the group definition intact
C
      IMPLICIT NONE
      SAVE
C
C     declaration of arguments
C
      INTEGER       NO_ITEM_MAX , NO_ITEM     ,
     +              LUNREP      , IERROR
      INTEGER       DEFFDS
      CHARACTER*10  ITEM_ID     (NO_ITEM_MAX)
      CHARACTER*50  ITEM_NAME   (NO_ITEM_MAX)
      CHARACTER*20  ITEM_UNIT   (NO_ITEM_MAX)
      REAL          ITEM_DEFAULT(NO_ITEM_MAX)
      CHARACTER*10  ITEM_AGGREGA(NO_ITEM_MAX)
      CHARACTER*10  ITEM_DISAGGR(NO_ITEM_MAX)
      CHARACTER*30  ITEM_GROUPID(NO_ITEM_MAX)
      CHARACTER*1   ITEM_SEGX   (NO_ITEM_MAX)
      CHARACTER*1   ITEM_WK     (NO_ITEM_MAX)
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
      PARAMETER   ( NELEMS = 10 )
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
      DATA  GRPNAM  /'TABLE_P2'/
      DATA
     + (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I),
     +  I = 1 , NELEMS)
     +/'NO_ITEM','INTEGER'  , 4,1,1,'number of items'                  ,
     + 'ITEM_ID','CHARACTER',10,1,0,'unique item identification'       ,
     + 'ITEM_NM','CHARACTER',50,1,0,'item name'                        ,
     + 'UNIT'   ,'CHARACTER',20,1,0,'unit'                             ,
     + 'DEFAULT','REAL'     , 4,1,0,'default value'                    ,
     + 'AGGREGA','CHARACTER',10,1,0,'variable used for aggregation    ',
     + 'DISAGGR','CHARACTER',10,1,0,'variable used for dis-aggregation',
     + 'GROUPID','CHARACTER',30,1,0,'subtance group ID                ',
     + 'SEG_EXC','CHARACTER', 1,1,0,'segment / exchange indication    ',
     + 'WK'     ,'CHARACTER', 1,1,0,'active / inactive indication     '/
C
C     Read all elements
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
     +                 buflen , NO_ITEM  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_ITEM .GT. NO_ITEM_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of items:',NO_ITEM
         WRITE(LUNREP,*) 'greater than maximum:',NO_ITEM_MAX
         IERROR = 1
         GOTO 900
      ENDIF
C
C     Set dimension of table
C
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_ITEM
      ENDDO
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(2)
      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(2),
     +                 UINDEX , 1        ,
     +                 BUFLEN , ITEM_ID  )
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
     +                 BUFLEN , ITEM_NAME)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(4)
      BUFLEN = NBYTSG(4)*ELMDMS(2,4)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(4),
     +                 UINDEX , 1        ,
     +                 BUFLEN , ITEM_UNIT)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(5)
      BUFLEN = NBYTSG(5)*ELMDMS(2,5)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(5),
     +                 UINDEX , 1        ,
     +                 BUFLEN , ITEM_DEFAULT)
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
     +                 BUFLEN , ITEM_DISAGGR)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(7)
      BUFLEN = NBYTSG(7)*ELMDMS(2,7)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(7),
     +                 UINDEX , 1        ,
     +                 BUFLEN , ITEM_AGGREGA)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(7)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(8)
      BUFLEN = NBYTSG(8)*ELMDMS(2,8)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(8),
     +                 UINDEX , 1        ,
     +                 BUFLEN , ITEM_GROUPID)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(8)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(9)
      BUFLEN = NBYTSG(9)*ELMDMS(2,9)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(9),
     +                 UINDEX , 1        ,
     +                 BUFLEN , ITEM_SEGX)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(9)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(10)
      BUFLEN = NBYTSG(10)*ELMDMS(2,10)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(10),
     +                 UINDEX , 1         ,
     +                 BUFLEN , ITEM_WK  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(10)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C
  900 CONTINUE
      RETURN
C
      END
