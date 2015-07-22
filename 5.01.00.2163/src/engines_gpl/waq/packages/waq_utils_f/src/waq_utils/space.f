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

      MODULE WORKSPACE

      USE PARTITION_ARRAYS
      USE DHMMAR_MOD
      USE DHMMCA_MOD
      USE DHMMJA_MOD
      USE DHMMRA_MOD

      CONTAINS

      SUBROUTINE SPACE  ( LUNREP , L_DECL , A      , J      , C      ,
     +                    IMAXA  , IMAXI  , IMAXC  )
C
C     Deltares
C
C     CREATED             : april- 8-1988 by L. Postma
C
C     FUNCTION            : Sets the array pointers in the
C                           SYSA, SYSI and SYSC common blocks.
C                           This is the only place where these
C                           common blocks are changed.
C                           WARNING: The order in the common block
C                           must be the same as the order in which the
C                           pointers are set.
C
C     LOGICAL UNITNUMBERS : LUNREP- monitoring output file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUNREP  INTEGER       1     INPUT   logical unitnumber output file
C     L_DECL  LOGICAL       1     INPUT   Declare memory y/n
C     A       INTEGER       *     OUTPUT  real workspace array
C     J       INTEGER       *     OUTPUT  integer workspace array
C     C       CHAR*20       *     OUTPUT  character workspace array
C     IMAXA   INTEGER       1     INPUT   Maximum real  array space
C     IMAXI   INTEGER       1     INPUT   Maximum integer array space
C     IMAXC   INTEGER       1     INPUT   Maximum character array space
C
      INTEGER       LUNREP, IMAXA  , IMAXI  , IMAXC
      LOGICAL       L_DECL
      REAL, DIMENSION(:), POINTER             :: A
      INTEGER, DIMENSION(:), POINTER          :: J
      CHARACTER(LEN=*), DIMENSION(:), POINTER :: C

      INTEGER, DIMENSION(:), POINTER               :: JNEW
      CHARACTER(LEN=LEN(C)), DIMENSION(:), POINTER :: CNEW
      CHARACTER(LEN=20),     DIMENSION(:), ALLOCATABLE :: CNAME

      INTEGER                :: K1, K2
      INTEGER*8              :: ITOT

      TYPE(MEMORY_PARTITION) :: PART
C
C     COMMON  /  SYSI   /   Timer characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
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
C     Allocate initial space
C
      NOARR  = IASIZE + IJSIZE + ICSIZE

      IF ( ASSOCIATED(J)    ) DEALLOCATE( J )
      IF ( ASSOCIATED(C)    ) DEALLOCATE( C )
      IF ( ALLOCATED(CNAME) ) DEALLOCATE( CNAME )

      ALLOCATE( J(     IASIZE + 1 + IJSIZE + ICSIZE + 1 + 8 * NOARR  ) )
      ALLOCATE( C( 20*(IASIZE + 1 + IJSIZE + ICSIZE + 1) ) )
      ALLOCATE( CNAME (IASIZE + 1 + IJSIZE + ICSIZE + 1) )

      J = 0
      C = ' '
      CNAME = ' '

C
C     Total number of "separate" variables
C
      NOVAR = 5 + NOCONS + NOPA   + NOFUN  + NOSFUN + NOTOT + NOTOT +
     +            NOTOT  + NODISP + NOVELO + NODEF  + NOLOC + NDSPX +
     +            NVELX  + NLOCX  + NFLUX
C
C     Sets the array pointers for the array administration array's.
C
      CALL DHMMAR( LUNREP  , J       , CNAME   , PART )
C
C     Set the real array workspace
C
      CALL DHMMRA( LUNREP   ,L_DECL   ,J(IAPOI:), J(IATYP:), J(IABYT:),
     +             J(IALEN:),J(IAKND:),J(IADM1:), J(IADM2:), J(IADM3:),
     +             CNAME, ITOTA, PART )
C
C     Set the integer array workspace
C
      CALL DHMMJA( LUNREP   ,L_DECL   ,J(IAPOI:), J(IATYP:), J(IABYT:),
     +             J(IALEN:),J(IAKND:),J(IADM1:), J(IADM2:), J(IADM3:),
     +             CNAME, ITOTI, PART )
C
C     Set the integer array workspace
C
      CALL DHMMCA( LUNREP   ,L_DECL   ,J(IAPOI:), J(IATYP:), J(IABYT:),
     +             J(IALEN:),J(IAKND:),J(IADM1:), J(IADM2:), J(IADM3:),
     +             CNAME, ITOTC, PART )
C
C     messages and tests on array space
C
      ITOT = INT8(ITOTA+ITOTI+ITOTC)*4_2
      WRITE ( LUNREP, 2000 ) ITOTA, ITOTI, ITOTC, ITOT/4,
     &                       ITOT/1000000000,
     &                       MOD(ITOT,1000000000)/1000000,
     &                       MOD(ITOT,   1000000)/   1000,
     &                       MOD(ITOT,      1000)
      IEFLAG = 0
      IF ( ITOTA .GT. IMAXA .AND. IMAXA .NE. 0 .AND. L_DECL ) THEN
           WRITE ( LUNREP, 2010 ) ITOTA, IMAXA
           IEFLAG = 1
      ENDIF
      IF ( ITOTI .GT. IMAXI .AND. IMAXI .NE. 0 .AND. L_DECL ) THEN
           WRITE ( LUNREP, 2020 ) ITOTI, IMAXI
           IEFLAG = 1
      ENDIF
      IF ( ITOTC .GT. IMAXC .AND. IMAXC .NE. 0 .AND. L_DECL ) THEN
           WRITE ( LUNREP, 2030 ) ITOTC, IMAXC
           IEFLAG = 1
      ENDIF
      IF ( IEFLAG .EQ.    1 ) THEN
           WRITE ( LUNREP, 2040 )
           CALL SRSTOP(1)
      ENDIF
      IMAXA = ITOTA
      IMAXI = ITOTI
      IMAXC = ITOTC

C
C     Allocate the arrays, first C then J then A for least memory requiremnt
C

      IF ( L_DECL ) THEN

          DEALLOCATE( A )                ! It was allocated at the start

          ALLOCATE( CNEW(part%cpoint) )
          CNEW = ' '
          DO K2 = 1,SIZE(CNAME)
              DO K1 = 1,20
                  CNEW(1+K1+(K2-1)*20) = CNAME(K2)(K1:K1)
              ENDDO
          ENDDO
          DEALLOCATE( C )
          C => CNEW

          ALLOCATE( JNEW(part%jpoint) )
          JNEW = 0
          JNEW(1:SIZE(J)) = J
          DEALLOCATE( J )
          J => JNEW

          ALLOCATE( A(part%apoint) )
          A    = 0.0

      ENDIF

      RETURN
C
C         output formats
C
 2000 FORMAT ( ' total real      array space: ',I10,/
     *         ' total integer   array space: ',I10,/
     *         ' total character array space: ',I10,/
     *         ' grand total in 4-byte words: ',I10,
     *         ' = ',i3,'-GB ',i3,'-MB ',i3'-KB ',i3,'-Byte.' )
 2010 FORMAT ( ' ERROR. Real      array space exceeded !!! ',/,
     *         ' total real    array space: ',I10,', allowed = ',I10)
 2020 FORMAT ( ' ERROR. Integer   array space exceeded !!! ',/,
     *         ' total integer array space: ',I10,', allowed = ',I10)
 2030 FORMAT ( ' ERROR. Character array space exceeded !!! ',/,
     *         ' total Character*20  space: ',I10,', allowed = ',I10)
 2040 FORMAT ( ' EXECUTION HALTED, CONSULT YOUR SYSTEM MANAGER !!!')
C
      END SUBROUTINE

      END MODULE WORKSPACE
