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

      SUBROUTINE DLWQIB ( LUN    , LUNUT  , A      , J      , MODE   ,
     *                                      IISP   , IRSP   , IERR   )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 1996 by L. Postma
C
C     FUNCTION            : Initialises the complete boundary subsystem
C
C     LOGICAL UNITNUMBERS : LUN   - binary boundary system file
C                           LUNUT - monitoring file
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUN     INTEGER    1        INPUT   unit number input file
C     A       REAL       ?        IN/OUT  Real      boundary workspace
C     J       INTEGER    ?        IN/OUT  Integer   boundary workspace
C     MODE    INTEGER    1        INPUT   File number involved
C     IISP    INTEGER    1        IN/OUT  Integer array space pointer
C     IRSP    INTEGER    1        IN/OUT  Real array space pointer
C     IERR    INTEGER    1        IN/OUT  error count
C
C     Declaration of arguments
C
      use timers

      DIMENSION       A(*)   , J(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqib", ithandl )
C
C         initialise the system
C
      IA = 1
      IJ = 1
C
C       Read the system dimensions
C
      READ ( LUN , END=40 , ERR=110 )  J(IJ), J(IJ+1)
      NOITM = J(IJ)
      NOSYS = J(IJ+1)
      IJ = 3
C
C       Read the pointers for this block of data
C
   10 READ ( LUN , END=40 , ERR=110 )  J(IJ),
     *                NPNT, ( J(IJ+K) , K=2,NPNT+1 )  ,
     *                NDIM, ( J(IJ+NPNT+2+K) , K = 1, NDIM+2 )
      J(IJ     +1) = NPNT
      J(IJ+NPNT+2) = NDIM
      IJ = IJ + NPNT + NDIM + 5
C        default for all items
      IF ( NPNT .EQ. 0 ) NPNT = 1
C
C       3 and 4 are harmonics
C
      IOPT = J(IJ-2)
      NTAL = 0
      IF ( IOPT .EQ. 3 .OR. IOPT .EQ. 4 ) NTAL = 1
C
C       Nr of breakpoints or harmonics
C
      READ ( LUN , END=100 , ERR=110 ) NOBRK
      J(IJ) = NOBRK
      IJ = IJ + 1
C
      DO 20 I=1,NOBRK
         READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *               ( A(IA+K) , K=0,NPNT*NDIM-1+NTAL )
         IJ = IJ + 1
         IA = IA + NPNT*NDIM + NTAL
   20 CONTINUE
C
C       Return until finished
C
      GOTO 10
C
C       Update linear pointers in array
C
   40 IISP = IISP + IJ-1
      IRSP = IRSP + IA-1
      goto 9999  !   RETURN
C
  100 WRITE ( LUNUT , '(A,I3)' ) ' END-OF-FILE mode:',MODE
      IERR = IERR + 1
      goto 9999  !   RETURN
  110 WRITE ( LUNUT , '(A,I3)' ) ' ERROR-ON-FILE mode:',MODE
      IERR = IERR + 1
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
