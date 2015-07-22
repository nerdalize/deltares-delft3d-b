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

      MODULE DHMMAR_MOD
      CONTAINS
      SUBROUTINE DHMMAR(LUNREP,J,C,PART)
C
C     Deltares
C
C     CREATED             : Jun. 1998 by Jan van Beek
C
C     FUNCTION            : Sets the array pointers for the array
C                           administration array's.
C                           Declares memory.
C
C     LOGICAL UNITNUMBERS : LUNREP - monitoring output file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution

      use partition_arrays ! module for computing the pointers into the arrays
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUNREP  INTEGER       1     INPUT   logical unitnumber output file
C     J       INTEGER       *     OUTPUT  integer workspace array
C     C       CHAR*20       *     OUTPUT  character workspace array
C
C     Declaration of arguments
C
      INTEGER       LUNREP
      INTEGER       J(:)
      CHARACTER*(*) C(:)
      type(memory_partition), intent(inout) :: part ! Private variables for MAKPTR

C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI   /   Timer characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSA   /   Pointers in real array workspace
C
      INCLUDE 'sysa.inc'
C
C     COMMON  /  SYSJ   /   Pointers in integer array workspace
C
      INCLUDE 'sysj.inc'
C
C     COMMON  /  SYSC   /   Pointers in character array workspace
C
      INCLUDE 'sysc.inc'
C
C     Local declarations
C
      CHARACTER(LEN=20 ) :: ARRNAM   ! name of the array for esmfsm
!     CHARACTER(LEN=256) :: ERRSTR   ! error string from esmfsm routines
C
      IIAPOI =  IASIZE + 1
      IIATYP =  IASIZE + 2
      IIABYT =  IASIZE + 3
      IIALEN =  IASIZE + 4
      IIAKND =  IASIZE + 5
      IIADM1 =  IASIZE + 6
      IIADM2 =  IASIZE + 7
      IIADM3 =  IASIZE + 8
C
      IIANAM =  IASIZE + IJSIZE + 1
C
C     First set and declare memory for array administration
C     directly use the array with pointers
C
      ARRNAM = 'ARRPOI'
      IAPOI  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IAPOI .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IAPOI = IAPOI + 1
C
      ARRNAM = 'ARRTYP'
      IATYP  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IATYP .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IATYP = IATYP + 1
C
      ARRNAM = 'ARRBYT'
      IABYT  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IABYT .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IABYT = IABYT + 1
C
      ARRNAM = 'ARRLEN'
      IALEN  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IALEN .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IALEN = IALEN + 1
C
      ARRNAM = 'ARRKND'
      IAKND  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IAKND .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IAKND = IAKND + 1
C
      ARRNAM = 'ARRDM1'
      IADM1  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IADM1 .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IADM1 = IADM1 + 1
C
      ARRNAM = 'ARRDM2'
      IADM2  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IADM2 .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IADM2 = IADM2 + 1
C
      ARRNAM = 'ARRDM3'
      IADM3  = MAKPTR(PART, ARRNAM,ITYP ,NOARR)
      IF ( IADM3 .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IADM3 = IADM3 + 1
C
      ARRNAM = 'ARRNAM'
      IANAM  = MAKPTR(PART, ARRNAM,CHTYP ,NOARR*20)
      IF ( IANAM .EQ. 0 ) THEN
!        CALL FSM_ERROR(ERRSTR)
         WRITE(LUNREP,2010)
         WRITE(LUNREP,2020) ARRNAM
         WRITE(LUNREP,2030) NOARR*20
!        WRITE(LUNREP,2040) ERRSTR
         CALL SRSTOP(1)
      ENDIF
      IANAM = IANAM + 1
C
C
C     Fill the array's themselves
C
      J(IAPOI+IIAPOI-1) = IAPOI
      J(IAPOI+IIATYP-1) = IATYP
      J(IAPOI+IIABYT-1) = IABYT
      J(IAPOI+IIALEN-1) = IALEN
      J(IAPOI+IIAKND-1) = IAKND
      J(IAPOI+IIADM1-1) = IADM1
      J(IAPOI+IIADM2-1) = IADM2
      J(IAPOI+IIADM3-1) = IADM3
      J(IAPOI+IIANAM-1) = IANAM
C
      J(IATYP+IIAPOI-1) = ITYP
      J(IATYP+IIATYP-1) = ITYP
      J(IATYP+IIABYT-1) = ITYP
      J(IATYP+IIALEN-1) = ITYP
      J(IATYP+IIAKND-1) = ITYP
      J(IATYP+IIADM1-1) = ITYP
      J(IATYP+IIADM2-1) = ITYP
      J(IATYP+IIADM3-1) = ITYP
      J(IATYP+IIANAM-1) = CHTYP
C
      J(IABYT+IIAPOI-1) = 4
      J(IABYT+IIATYP-1) = 4
      J(IABYT+IIABYT-1) = 4
      J(IABYT+IIALEN-1) = 4
      J(IABYT+IIAKND-1) = 4
      J(IABYT+IIADM1-1) = 4
      J(IABYT+IIADM2-1) = 4
      J(IABYT+IIADM3-1) = 4
      J(IABYT+IIANAM-1) = 20
C
      J(IALEN+IIAPOI-1) = NOARR
      J(IALEN+IIATYP-1) = NOARR
      J(IALEN+IIABYT-1) = NOARR
      J(IALEN+IIALEN-1) = NOARR
      J(IALEN+IIAKND-1) = NOARR
      J(IALEN+IIADM1-1) = NOARR
      J(IALEN+IIADM2-1) = NOARR
      J(IALEN+IIADM3-1) = NOARR
      J(IALEN+IIANAM-1) = NOARR
C
      J(IAKND+IIAPOI-1) = 1
      J(IAKND+IIATYP-1) = 1
      J(IAKND+IIABYT-1) = 1
      J(IAKND+IIALEN-1) = 1
      J(IAKND+IIAKND-1) = 1
      J(IAKND+IIADM1-1) = 1
      J(IAKND+IIADM2-1) = 1
      J(IAKND+IIADM3-1) = 1
      J(IAKND+IIANAM-1) = 1
C
      J(IADM1+IIAPOI-1) = NOARR
      J(IADM1+IIATYP-1) = NOARR
      J(IADM1+IIABYT-1) = NOARR
      J(IADM1+IIALEN-1) = NOARR
      J(IADM1+IIAKND-1) = NOARR
      J(IADM1+IIADM1-1) = NOARR
      J(IADM1+IIADM2-1) = NOARR
      J(IADM1+IIADM3-1) = NOARR
      J(IADM1+IIANAM-1) = NOARR
C
      J(IADM2+IIAPOI-1) = 1
      J(IADM2+IIATYP-1) = 1
      J(IADM2+IIABYT-1) = 1
      J(IADM2+IIALEN-1) = 1
      J(IADM2+IIAKND-1) = 1
      J(IADM2+IIADM1-1) = 1
      J(IADM2+IIADM2-1) = 1
      J(IADM2+IIADM3-1) = 1
      J(IADM2+IIANAM-1) = 1
C
      J(IADM3+IIAPOI-1) = 1
      J(IADM3+IIATYP-1) = 1
      J(IADM3+IIABYT-1) = 1
      J(IADM3+IIALEN-1) = 1
      J(IADM3+IIAKND-1) = 1
      J(IADM3+IIADM1-1) = 1
      J(IADM3+IIADM2-1) = 1
      J(IADM3+IIADM3-1) = 1
      J(IADM3+IIANAM-1) = 1
C
      C(IIAPOI) = 'ARRPOI'
      C(IIATYP) = 'ARRTYP'
      C(IIABYT) = 'ARRBYT'
      C(IIALEN) = 'ARRLEN'
      C(IIAKND) = 'ARRKND'
      C(IIADM1) = 'ARRDM1'
      C(IIADM2) = 'ARRDM2'
      C(IIADM3) = 'ARRDM3'
      C(IIANAM) = 'ARRNAM'
C
      RETURN
 2010 FORMAT ( ' ERROR  : allocating administration array')
 2020 FORMAT ( ' name   : ',A)
 2030 FORMAT ( ' length : ',I12)
 2040 FORMAT ( ' error  : ',A)
      END SUBROUTINE
      END MODULE DHMMAR_MOD
