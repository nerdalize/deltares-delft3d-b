      SUBROUTINE RD_TABM1 ( DEFFDS      ,
     +                      n_old_items_max,
     +                      n_old_items,
     +                      old_items_old_name,
     +                      old_items_new_name,
     +                      old_items_old_default,
     +                      old_items_configuration,
     +                      old_items_serial,
     +                      old_items_action_type,
     +                      LUNREP      , IERROR      )
C
C     Deltares
C
C     CREATED            :  Aug 2012 by Jan van Beek
C
C     FUNCTION           :  Read TABLE_M1 (old_items) group from NEFIS file
C
C     FILES              :  NEFIS file assumed opened
C
C     IMPLICIT NONE for extra compiler checks
C     SAVE to keep the group definition intact
C
      IMPLICIT NONE
      SAVE
C
C     declaration of arguments
C
      integer            :: deffds                                       ! nefis file descriptor
      integer            :: n_old_items_max                              ! maximum number of old items
      integer            :: n_old_items                                  ! number of old items
      character*10       :: old_items_old_name(n_old_items_max)          ! old name (if equal to new name then use old_default if target serial is less then
      character*10       :: old_items_new_name(n_old_items_max)          ! new name
      real               :: old_items_old_default(n_old_items_max)       ! old default value
      character*10       :: old_items_configuration(n_old_items_max)     ! (only use this new name if a specific configuration is used?)
      integer            :: old_items_serial(n_old_items_max)            ! the proces definition serial number up to where this old name, old default was used
      integer            :: old_items_action_type(n_old_items_max)       ! process rename, process parameter rename, default value change
      integer            :: lunrep                                       ! report file
      integer            :: ierror                                       ! error
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
      DATA  GRPNAM  /'TABLE_M1'/
      DATA
     + (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I),
     +  I = 1 , NELEMS)
     +/'n_old_items'  ,'INTEGER'  , 4,1,1,'number of old items',
     + 'old_name'     ,'CHARACTER',10,1,0,'old name',
     + 'new_name'     ,'CHARACTER',10,1,0,'new name',
     + 'old_default'  ,'REAL'     , 4,1,0,'old default value',
     + 'configuration','CHARACTER',10,1,0,'configuration',
     + 'serial'       ,'INTEGER'  , 4,1,0,'serial number when changed',
     + 'action_type'  ,'INTEGER'  , 4,1,0,'type of change'/
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
     +                 buflen , n_old_items)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( n_old_items .GT. n_old_items_max ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of items:',n_old_items
         WRITE(LUNREP,*) 'greater than maximum:',n_old_items_max
         IERROR = 1
         GOTO 900
      ENDIF
C
C     Set dimension of table
C
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = n_old_items
      ENDDO
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(2)
      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(2),
     +                 UINDEX , 1        ,
     +                 BUFLEN ,
     +                 old_items_old_name)
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
     +                 BUFLEN ,
     +                 old_items_new_name)
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
     +                 BUFLEN ,
     +                 old_items_old_default)
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
     +                 BUFLEN ,
     +                 old_items_configuration)
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
     +                 BUFLEN ,
     +                 old_items_serial)
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
     +                 BUFLEN ,
     +                 old_items_action_type)
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
