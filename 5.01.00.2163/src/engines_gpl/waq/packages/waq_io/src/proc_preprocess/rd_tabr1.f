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

      SUBROUTINE RD_TABR1 ( DEFFDS      ,
     +                      NO_C_P_MAX  , NO_CONF     ,
     +                      NO_PROC     , CON_PRO     ,
     +                      LUNREP      , IERROR      )
C
C     Deltares
C
C     CREATED            :  june 1999 by Jan van Beek
C
C     FUNCTION           :  Read TABLE_R1 group from NEFIS file
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
C     NO_C_P_MAX   INT            I       maximum number of configurations * processes
C     NO_CONF      INT            I       number of configurations
C     NO_PROC      INT            I       number of processes
C     CON_PRO      INT    NO_CONF*0       configuration process table
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
      INTEGER       NO_C_P_MAX  , NO_CONF     ,
     +              NO_PROC     , LUNREP      ,
     +              IERROR
      INTEGER       DEFFDS
      INTEGER       CON_PRO(NO_C_P_MAX)
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
      PARAMETER   ( NELEMS = 3 )
C
      INTEGER       I               , IELM          ,
     +              BUFLEN          , NO_CONF_R1    ,
     +              NO_PROC_R1
      INTEGER       ELMDMS(3,NELEMS), NBYTSG(NELEMS),
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
      DATA  GRPNAM  /'TABLE_R1'/
      DATA
     + (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I),
     +  I = 1 , NELEMS)
     +/'NO_CONF_R1'  ,'INTEGER'  , 4,1,1,'number of configurations'    ,
     + 'NO_PROC_R1'  ,'INTEGER'  , 4,1,1,'number of processes'         ,
     + 'CON_PRO'     ,'INTEGER'  , 4,1,0,'unique group identification' /
C
C     Read the group
C
C     WRITE(LUNREP,*) ' reading GROUP:',GRPNAM
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(1)
      BUFLEN = NBYTSG(1)*ELMDMS(2,1)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(1) ,
     +                 UINDEX , 1         ,
     +                 BUFLEN , NO_CONF_R1)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_CONF_R1 .NE. NO_CONF ) THEN
         WRITE(LUNREP,*) 'ERROR number of configuration combinations:',
     +                   NO_CONF_R1
         WRITE(LUNREP,*) 'not equal to number of configurations:',
     +                   NO_CONF
         IERROR = 2
         GOTO 900
      ENDIF
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(2)
      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(2) ,
     +                 UINDEX , 1         ,
     +                 BUFLEN , NO_PROC_R1)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_PROC_R1 .NE. NO_PROC ) THEN
         WRITE(LUNREP,*) 'ERROR number of processes combinations:',
     +                   NO_PROC_R1
         WRITE(LUNREP,*) 'not equal to number of processes:',NO_PROC
         IERROR = 2
         GOTO 900
      ENDIF
      IF ( NO_CONF*NO_PROC .GT. NO_C_P_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*)
     +      'Actual number of configuration-processes combinations:',
     +      NO_CONF*NO_PROC
         WRITE(LUNREP,*) 'greater than maximum:',NO_C_P_MAX
         IERROR = 1
         GOTO 900
      ENDIF
C
C     Set dimension of table
C
      DO IELM = 3 , NELEMS
         ELMDMS(2,IELM) = NO_CONF*NO_PROC
      ENDDO
C     WRITE(LUNREP,*) ' reading ELEMENT:',ELMNMS(3)
      BUFLEN = NBYTSG(3)*ELMDMS(2,3)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(3),
     +                 UINDEX , 1        ,
     +                 BUFLEN , CON_PRO  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C
  900 CONTINUE
      RETURN
C
      END
