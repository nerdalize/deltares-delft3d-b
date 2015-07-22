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

      SUBROUTINE DLWQIO ( LUNWRO, LCH   , LUREP , NOUTP , NRVART,
     +                    NBUFMX, IOUTPS, IOPOIN, OUNAM , LUN   ,
     +                    LCHAR , MYPART, IERR  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : may 1993 by Jan van Beek
C
C     FUNCTION            : Initialisation of OUTPUT system.
C                           Reads output work file.
C
C     SUBROUTINES CALLED  : DHOPNF, Opens files
C
C     FILES               : LUNWRO, Proces work file
C                           LUREP , Monitoring file
C
C     PARAMETERS          : 12
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
C     LUN     INTEGER    *        INPUT   array with unit numbers
C     LCHAR   CHAR*(*)   *        INPUT   filenames
C     MYPART  INTEGER       1     INPUT   subdomain number in parallel run
C     IERR    INTEGER       1    IN/OUT   cummulative error count
C
C     Declaration of arguments
C
      use timers
      INTEGER       LUNWRO, LUREP , NOUTP , NRVART, NBUFMX,
     +              IERR
      INTEGER       IOUTPS(7,*)   , IOPOIN(*)     , LUN(*)
      CHARACTER*(*) LCH           , LCHAR(*)
      CHARACTER*20  OUNAM(*)
C
C     Local declarations
C
      PARAMETER   ( VERSI1 = 0.0 , VERSI2 = 0.1 )
      PARAMETER   ( IMON = 1 , IMO2 = 2 , IDMP = 3 , IDM2 = 4 ,
     +              IHIS = 5 , IHI2 = 6 , IMAP = 7 , IMA2 = 8 ,
     +              IBAL = 9 , IHNF =10 , IHN2 =11 , IMNF =12 ,
     +              IMN2 =13 , IMO3 =14 , IMO4 =15 , IHI3 =16 ,
     +              IHI4 =17 , IHN3 =18 , IHN4 =19 , IBA2 =20 )
      PARAMETER   ( LUOFF = 18 )
      PARAMETER   ( LUOFF2= 36 )
      INTEGER       NOUTPD, NRVARD, NBUFMD
      REAL          VERSIO
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqio", ithandl )
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
      IF (NRVART.GT.0) THEN
         READ (LUNWRO, ERR=900, END=900) ( IOPOIN(K)   , K = 1 , NRVART)
         READ (LUNWRO, ERR=900, END=900) ( OUNAM (K)   , K = 1 , NRVART)
      ENDIF
C
C     Set initialize flag, open files: only on first subdomain
C
      IF (MYPART.EQ.1) THEN
         DO 10 K = 1,NOUTP
            ISRTOU = IOUTPS(5,K)
            IF ( K .LE. 4 ) THEN
               IFI = K + LUOFF
            ELSEIF ( K .LE. 7 ) THEN
               IFI = K + LUOFF2 - 4
            ELSE
               IFI = K + LUOFF2 - 2
            ENDIF
C
C           Open the output-file in the correct way, depending on type of output
C
            IOUTPS(7,K) = 1
            IF ( ISRTOU .EQ. IMON .OR. ISRTOU .EQ. IMO2 .OR.
     +           ISRTOU .EQ. IMO3 .OR. ISRTOU .EQ. IMO4 ) THEN
C
C              Do not open the normal monitor file
C
               IF ( K .NE. 1 ) THEN
                  CALL DHOPNF ( LUN(IFI), LCHAR(IFI), 19   , 1    , IDUM )
               ENDIF
            ELSEIF ( ISRTOU .EQ. IDMP .OR. ISRTOU .EQ. IDM2 ) THEN
               CALL DHOPNF ( LUN(IFI), LCHAR(IFI), 20    , 1     , IDUM  )
            ELSEIF ( ISRTOU .EQ. IHIS .OR. ISRTOU .EQ. IHI2 .OR.
     +               ISRTOU .EQ. IHI3 .OR. ISRTOU .EQ. IHI4 ) THEN
               CALL DHOPNF ( LUN(IFI), LCHAR(IFI), 21    , 1     , IDUM  )
            ELSEIF ( ISRTOU .EQ. IMAP .OR. ISRTOU .EQ. IMA2 ) THEN
               CALL DHOPNF ( LUN(IFI), LCHAR(IFI), 22    , 1     , IDUM  )
            ELSEIF ( ISRTOU .EQ. IBAL .OR. ISRTOU .EQ. IBA2 ) THEN
               CALL DHOPNF ( LUN(IFI), LCHAR(IFI), 37    , 1     , IDUM  )
            ENDIF
   10    CONTINUE
      ENDIF ! MYPART.EQ.1
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
C     unsuccessful read
C
  900 CONTINUE
      WRITE ( LUREP   , 2050 ) LCH, LUNWRO
      IERR = IERR + 1
C
  910 CONTINUE
      RETURN
C
C     output formats
C
 2000 FORMAT ( ' ERROR  : version output intput ',F5.2,' NOT supported'
     &        /'          by OUTPUT sytem version,',F5.2)
 2010 FORMAT ( ' WARNING: version output intput ',F5.2,' greater than'
     &        /'          OUTPUT sytem version,',F5.2)
 2020 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NOUTP',
     &        /'          ',I6,' in output,',I6,' in boot file.')
 2030 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NRVART',
     &        /'          ',I6,' in output,',I6,' in boot file.')
 2040 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NBUFMX',
     &        /'          ',I6,' in output,',I6,' in boot file.')
 2050 FORMAT ( ' ERROR  : Reading output work file;',A,
     &        /'          on unit number ',I3)
C
      END
