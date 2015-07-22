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

      SUBROUTINE RDWRKO ( LUNWRO, LCH   , LUREP , NOUTP , NRVART,
     +                    NBUFMX, IOUTPS, IOPOIN, OUNAM , VERSIO,
     +                    NOWARN, IERR  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : november 1994 by Jan van Beek
C
C     FUNCTION            : Reads output work file.
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : LUNWRO, Proces work file
C                           LUREP , Monitoring file
C
C     PARAMETERS          : 11
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUNWRO  INTEGER       1     INPUT   Output work file
C     LCH     CHA*(*)       1     INPUT   Name output work file
C     LUREP   INTEGER       1     INPUT   Monitoring file
C     NOUTP   INTEGER       1     INPUT   Number of output files
C     NRVART  INTEGER       1     INPUT   Number of extra output vars
C     NBUFMX  INTEGER       1     INPUT   length of output buffer
C     IOUTPS  INTEGER 7*NOUTP    OUTPUT   Output structure
C                                            index 1 = start time
C                                            index 2 = stop time
C                                            index 3 = time step
C                                            index 4 = number of vars
C                                            index 5 = kind of output
C                                            index 6 = format of output
C                                            index 7 = initialize flag
C     IOPOIN  INTEGER  NRVART    OUTPUT   Pointer to DELWAQ array's
C     OUNAM   CHAR*(*) NRVART    OUTPUT   name of output variable
C     NOWARN  INTEGER       1    IN/OUT   Cummulative warning count
C     IERR    INTEGER       1    IN/OUT   cummulative error count
C
C     Declaration of arguments
C
      use timers       !   performance timers

      INTEGER       LUNWRO, LUREP , NOUTP , NRVART, NBUFMX,
     +              NOWARN, IERR
      INTEGER       IOUTPS(7,*)   , IOPOIN(*)
      REAL          VERSIO
      CHARACTER*(*) LCH
      CHARACTER*(*) OUNAM(*)
C
C     Local declarations
C
      PARAMETER   ( VERSI1 = 0.0 , VERSI2 = 0.1 )
      INTEGER       NOUTPD, NRVARD, NBUFMD
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "rdwrko", ithndl )
C
C     read and check version number
C
      READ (LUNWRO, ERR=900, END=900) VERSIO
C
C     less than lowest supported version, ERROR
C
      IF ( VERSIO .LT. VERSI1 ) THEN
         WRITE ( LUREP, 2000 ) VERSIO , VERSI1
         CALL SRSTOP(1)
      ENDIF
C
C     greater than this version, WARNING
C
      IF ( VERSIO .GT. VERSI2 ) THEN
         NOWARN = NOWARN + 1
         WRITE ( LUREP, 2010 ) VERSIO , VERSI2
      ENDIF
C
C     read and check dimensions
C
      READ (LUNWRO, ERR=900, END=900) NOUTPD, NRVARD, NBUFMD
      IF ( NOUTPD .NE. NOUTP  ) THEN
         WRITE ( LUREP, 2020 ) NOUTPD, NOUTP
         IERR = IERR + 1
      ENDIF
      IF ( NRVARD .NE. NRVART ) THEN
         WRITE ( LUREP, 2030 ) NRVARD, NRVART
         IERR = IERR + 1
      ENDIF
      IF ( NBUFMD .NE. NBUFMX ) THEN
         WRITE ( LUREP, 2040 ) NBUFMD, NBUFMX
         IERR = IERR + 1
      ENDIF
      IF ( IERR .GT. 0 ) GOTO 910
C
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(1,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(2,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(3,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(4,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(5,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(6,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOPOIN(K)   , K = 1 , NRVART)
      READ (LUNWRO, ERR=900, END=900) ( OUNAM (K)   , K = 1 , NRVART)
C
      if (timon) call timstop( ithndl )
      RETURN
C
C     unsuccessful read
C
  900 CONTINUE
      WRITE ( LUREP   , 2050 ) LCH, LUNWRO
      IDUM = IDUM + 1
C
  910 CONTINUE
      if (timon) call timstop( ithndl )
      RETURN
C
C     output formats
C
 2000 FORMAT ( ' ERROR  : version output intput ',F5.2,' NOT supported'
     +        /'          by OUTPUT sytem version,',F5.2)
 2010 FORMAT ( ' WARNING: version output intput ',F5.2,' greater than'
     +        /'          OUTPUT sytem version,',F5.2)
 2020 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     +        /'          DELWAQ boot file for NOUTP',
     +        /'          ',I6,' in output,',I6,' in boot file.')
 2030 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     +        /'          DELWAQ boot file for NRVART',
     +        /'          ',I6,' in output,',I6,' in boot file.')
 2040 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     +        /'          DELWAQ boot file for NBUFMX',
     +        /'          ',I6,' in output,',I6,' in boot file.')
 2050 FORMAT ( ' ERROR  : Reading output work file;',A,
     +        /'          on unit number ',I3)
C
      END
