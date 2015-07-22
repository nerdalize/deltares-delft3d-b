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

      SUBROUTINE VALPOI ( NOTOT  , NOPA     , NOSFUN , SYNAME , NOCONS ,
     +                    NOFUN  , constants, PANAME , FUNAME , SFNAME ,
     +                    VALNAM , IVALIP   , LINE   )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:    december  1992 by Jan van Beek
C
C     FUNCTION            : sets pointers for process parametrs
C
C     LOGICAL UNITNUMBERS :
C
C     SUBROUTINES CALLED  : ZOEK  , searches a string in an array

      use dlwq_data

C     PARAMETERS          : 13
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     CONAME  CHAR*20   NOCONS    INPUT   Constant names
C     PANAME  CHAR*20   NOPA      INPUT   Parameter names
C     FUNAME  CHAR*20   NOFUN     INPUT   Function names
C     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
C     VALNAM  CHAR*20       1     INPUT   Name of variable in question
C     IVALIP  INTEGER       1     OUTPUT  Pointer in SSA.
C     LINE    CHAR*(*)      1     OUTPUT  Report line
C
      use timers       !   performance timers

      INTEGER       NOTOT , NOPA  , NOSFUN, NOCONS, NOFUN ,
     +              IVALIP
      CHARACTER*(*) VALNAM, LINE
      CHARACTER*(*) SYNAME(*),
     +              PANAME(*), FUNAME(*),
     +              SFNAME(*)
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
C
C     Local
C
      INTEGER       NZOEK
      PARAMETER   ( NZOEK = 20 )
      PARAMETER   ( NOPRED = 6 )
      CHARACTER(NZOEK) PREDEF(NOPRED)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "valpoi", ithndl )
C
      PREDEF(1) = 'VOLUME'
      PREDEF(2) = 'ITIME'
      PREDEF(3) = 'IDT'
      PREDEF(4) = 'DELT'
      PREDEF(5) = 'ITSTRT'
      PREDEF(6) = 'ITSTOP'
C
C
C     determine how VAL is modelled
C
C     Predefined ?
C
      CALL ZOEK ( VALNAM , NOPRED , PREDEF , NZOEK , IVALIP )
      IF ( IVALIP .EQ. 1 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ volume'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 2 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ time'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 3 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ timestep'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 4 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ timestep in days'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 5 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ start time'
         GOTO 800
      ENDIF
      IF ( IVALIP .EQ. 6 ) THEN
         WRITE(LINE,'(A)') '       Using DELWAQ stop time'
         GOTO 800
      ENDIF
C
C     as model variable ?
C
      CALL ZOEK ( VALNAM , NOTOT , SYNAME , NZOEK , ISYS   )
      IF ( ISYS .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using substance nr ',ISYS
         IVALIP = NOPRED + NOCONS + NOPA + NOFUN + NOSFUN + ISYS
         GOTO 800
      ENDIF
C
C     as segment function ?
C
      CALL ZOEK ( VALNAM , NOSFUN, SFNAME , NZOEK , ISFUN  )
      IF ( ISFUN .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using segment function nr',ISFUN
         IVALIP = NOPRED + NOCONS + NOPA + NOFUN + ISFUN
         GOTO 800
      ENDIF
C
C     as function ?
C
      CALL ZOEK ( VALNAM , NOFUN , FUNAME , NZOEK , IFUN   )
      IF ( IFUN .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using function nr',IFUN
         IVALIP = NOPRED + NOCONS + NOPA + IFUN
         GOTO 800
      ENDIF
C
C     as parameter ?
C
      CALL ZOEK ( VALNAM , NOPA  , PANAME , NZOEK , IPA    )
      IF ( IPA .GT. 0 ) THEN
         WRITE(LINE,'(A,I3)') '       Using parameter nr',IPA
         IVALIP = NOPRED + NOCONS + IPA
         GOTO 800
      ENDIF
C
C     as constant ?
C
!jvb  call zoek ( valnam , nocons, coname , nzoek , ico    )
      ico = dlwq_find(constants,valnam)
      if ( ico .gt. 0 ) then
         write(line,'(a,i3,a,g12.6)') '       Using constant nr',ico,' with value:',constants%constant(ico)
         ivalip = nopred + ico
         goto 800
      endif
C
C     not found
C
      IVALIP = -1
C
  800 CONTINUE
C
      if (timon) call timstop( ithndl )
      RETURN
      END
