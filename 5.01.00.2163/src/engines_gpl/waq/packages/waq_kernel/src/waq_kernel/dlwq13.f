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

      SUBROUTINE DLWQ13 ( LUN    , LCHAR  , CONC   , ITIME  , MNAME  ,
     &                    SNAME  , NOTOT  , NOSEG  )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : june 1988  BY L. Postma
C
C     FUNCTION           : gives a complete system dump
C
C     LOGICAL UNITS      : IOUT = number of dump file
C
C     SUBROUTINES CALLED : none
C
C     PARAMETERS         :
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LUN     INTEGER  *           INPUT   unit numbers output files
C     LCHAR   CHAR*(*) *           INPUT   names of output files
C     CONC    REAL     NOTOT*?     INPUT   concentration values
C     ITIME   INTEGER  1           INPUT   present time in clock units
C     MNAME   CHAR*40  4           INPUT   model identhification
C     SNAME   CHAR*20  NOTOT       INPUT   names of substances
C     NOTOT   INTEGER  1           INPUT   total number of systems
C     NOSEG   INTEGER  1           INPUT   total number of segments
C
C
      use timers

      DIMENSION     CONC  ( NOTOT, NOSEG ) , LUN(*)
      CHARACTER*20  SNAME ( * )
      CHARACTER*40  MNAME ( * )
      CHARACTER*(*) LCHAR ( * )
      integer    i
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq13", ithandl )
C
C      check for NaNs
C
      nonan = 0
      do j = 1,noseg
          do i = 1,notot
              if ( conc(i,j) /= conc(i,j) ) then
                  conc(i,j) = 0.0
                  nonan   = nonan + 1
              endif
          enddo
      enddo

      if ( nonan /= 0 ) then
          write (lun(19),*)
     &     ' Corrected concentrations as written to the restart file:'
          write (lun(19),*) ' Number of values reset to zero: ', nonan
          write (lun(19),*) ' Total number in the array:      ',
     &         notot*noseg
          write (lun(19),*)
     &     ' This may indicate that the computation was unstable'
      endif

C
C      write standard restart file
C
      CALL DHOPNF ( LUN(23), LCHAR(23), 23    , 1     , IERR  )
      WRITE ( LUN(23) ) ITIME , CONC
      CLOSE ( LUN(23) )
C
C     write restart file in .map format
C
      DO 10 I=248,1,-1
         IF ( LCHAR(23)(I:I) .EQ. '.' ) THEN
            LCHAR(23)(I:I+7) = "_res.map"
            GOTO 20
         ENDIF
   10 CONTINUE
      WRITE ( * , * ) ' Invalid name of restart MAP file !'
      WRITE (LUN(19),*) ' Invalid name of restart MAP file !'
      CALL SRSTOP(1)
   20 CALL DHOPNF ( LUN(23), LCHAR(23), 23    , 1     , IERR  )
      WRITE ( LUN(23) ) ( MNAME(K) , K=1,4 )
      WRITE ( LUN(23) )   NOTOT    , NOSEG
      WRITE ( LUN(23) ) ( SNAME(K) , K=1,NOTOT )
      WRITE ( LUN(23) ) ITIME , CONC
      CLOSE ( LUN(23) )
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
