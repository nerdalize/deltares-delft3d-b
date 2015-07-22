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

C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            :
C
C     V0.02  230395  Jos van Gils  Modify for hybrid coupling
C     V0.01  040894  Jos van Gils  First version
C
C     MODULE              : CHINPU
C
C     FUNCTION            : Reads input Charon
C                           Coupling Charon - Delwaq 4.0
C
C     SUBROUTINES CALLED  :
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
      SUBROUTINE CHINP2 (RUNNAM, LUIC  , LUOC  )
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     RUNNAM  C*12     1          I       Filename
C     LUIC    I        1          I       Lu input file
C     LUOC    I        1          I       LU output file
C
C     Declarations
C
      CHARACTER*20    C20
      CHARACTER*12    RUNNAM
      INTEGER         LUIC  , LUOC  , I1 , I2 , I , i3(1)
      REAL            R1(1,12)
C
C     Commons CHARON

      INCLUDE 'charon.inc'

C     Initialization of CHARON

      INTITL = 846342

C     Open input files and set LU's for Charon

      NIT    = LUIC
      NOT    = LUOC

      ILEN = LEN(RUNNAM)
C
C     Find last blank
C
      DO INDX = ILEN-4 , 1 , -1
         IF ( RUNNAM(INDX:INDX) .NE. ' ' ) GOTO 5
      ENDDO
C
C     empty string, error
C
      WRITE(*,*) 'ERROR in CHEM coupling, empty CHEM input'
      CALL SRSTOP(1)
    5 CONTINUE
      INDX = INDX + 1
      WRITE (RUNNAM(INDX:),'(''.inp'')')
      OPEN (NIT,FILE=RUNNAM)
      WRITE (RUNNAM(INDX:),'(''.out'')')
      OPEN (NOT,FILE=RUNNAM)

C     Read input file (N.B. Fixed order of input blocks!!!)

      CALL START
      READ (NIT,*)
      CALL ROWS (0)
      READ (NIT,*)
      CALL MATRIX (0,I1,I1,I2,I2,I1,1,I3,I3,I3,R1)
      READ (NIT,*)
c     (this variable switches on dimensionless input for cjor)
      KA(2) = 'NEW   '
      CALL CJCOR
      ITMAX = 200
      READ (NIT,'(A20)') C20
      IF (C20(1:3).EQ.'MES') THEN
          PF = 0
          WRITE (NOT,*) 'MESSAGES'
      ELSEIF (C20(1:3).EQ.'ALL') THEN
          PF = 1
          WRITE (NOT,*) 'ALL MESSAGES'
      ELSE
          PF = -1
          WRITE (NOT,*) 'NO MESSAGES'
      ENDIF

C     New part to read molar mass components

      read (nit,*)
      read (nit,'(12x,f12.0)') (commas(i),i=1,m)
      read (nit,*)
      write (not,'(''COMPONENT MASSES''/(a6,6x,f12.4))')
     j      (nr(i,1),commas(i),i=1,m)

C     New part to read desired list of transported substances

c     read (nit,*,end=40)
c     ntrans = 0
c  10 read (nit,'(a20)') C20
c     if ( c20(1:3) .eq. 'END' ) goto 50
c     ntrans = ntrans + 1
c     read ( c20 , '(6x,a10,4x)' ) varnam(ntrans)
c     goto 10

C     Defaults: all components are transported substances

c  40 continue
c     ntrans = m
c     do 45 i = 1,m
c  45 write ( varnam(i) , '(a6,''_tot'')' ) nr(i,1)

c  50 CONTINUE
c     write (not,'(''TRANSPORTED VECTOR''/(i4,2x,a10))')
c    j      (i, varnam(i),i=1,ntrans)
c     CLOSE (NIT)
      WRITE (NOT,*)'STOP READING INPUT FILE COUPLING DELWAQ-CHARON'

      RETURN
      END
