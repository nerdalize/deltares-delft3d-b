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

      SUBROUTINE DLWQIP ( LUNWRP, LCH   , LUREP , NOTOT , NIPMSA,
     +                    NPROC , NOLOC , NFLUX , NODEF , PRVNIO,
     +                    IFLUX , PRVVAR, PRVTYP, DEFAUL, STOCHI,
     +                    PRONAM, IMODU , IERR  , IPBLOO, IPCHAR,
     +                    IOFFBL, IOFFCH, NOSYS , NDSPX , NVELX ,
     +                    DSTO  , VSTO  , NDSPN , IDPNW , NVELN ,
     +                    IVPNW , NLOCX , PROGRD, PRONDT, NOVAR ,
     +                    VARARR, VARIDX, VARTDA, VARDAG, VARTAG,
     +                    VARAGG, nrref , proref, prvpnt)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : april 1993 by Jan van Beek

!     Modified            : December 2009 by Leo Postma, addition of
!                           proref array to test resolved input with parallelism

C     FUNCTION            : Initialisation of PROCES system.
C                           Reads proces work file.
C
C     SUBROUTINES CALLED  : PRONRS, gives module number
C                           PROPOI, computes absolute pointers, increments
C
C     FILES               : LUNWRP, Proces work file
C                           LUREP , Monitoring file
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUNWRP  INTEGER       1     INPUT   Proces work file
C     LCH     CHA*(*)       1     INPUT   Name proces work file
C     LUREP   INTEGER       1     INPUT   Monitoring file
C     NOTOT   INTEGER       1     INPUT   Number of substances
C     NIPMSA  INTEGER       1     INPUT   Length IPMSA
C     NPROC   INTEGER       1     INPUT   Number of called processes
C     NOLOC   INTEGER       1     INPUT   Number of local proces params
C     NFLUX   INTEGER       1     INPUT   total number of fluxes
C     NODEF   INTEGER       1     INPUT   Number of used defaults
C     PRVNIO  INTEGER       *     OUTPUT  Number of variables per proces
C     IFLUX   INTEGER       *     OUTPUT  Pointer in FLUX per proces inst.
C     IPMSA   INTEGER       *     OUTPUT  Pointer in SSA per proces inst.
C     IPSSA   INTEGER       *     OUTPUT  Pointer to SSA per proces inst.
C     DEFAUL  REAL          *     OUTPUT  Default proces parameters
C     STOCHI  REAL          *     OUTPUT  Proces stochiometry
C     PRONAM  CHA*(*)       *     OUTPUT  Name of called module
C     IMODU   INTEGER       *     OUTPUT  Module number proces
C     IERR    INTEGER       1     IN/OUT  Error count
C     IPBLOO  INTEGER       1     INPUT   Number of Bloom module (if >0)
C     IPCHAR  INTEGER       1     INPUT   Number of Charon module (if >0)
C     IOFFBL  INTEGER       1     INPUT   Offset in IPMSA for Bloom
C     IOFFCH  INTEGER       1     INPUT   Offset in IPMSA for Charon
C     NOSYS   INTEGER       1     INPUT   Number of active substances
C     NDSPX   INTEGER       1     INPUT   Number of extra dispersion array
C     NVELX   INTEGER       1     INPUT   Number of extra velocity array
C     DSTO    INTEGER NOSYS,*     OUTPUT  dispersion stochi matrix
C     VSTO    INTEGER NOSYS,*     OUTPUT  velocity stochi matrix
C     NDSPN   INTEGER       1     INPUT   Number of new dispersion array
C     IDPNW   INTEGER   NOSYS     OUTPUT  Pointers to new dispersion array
C     NVELN   INTEGER       1     INPUT   Number of new velocity array
C     IVPNW   INTEGER   NOSYS     OUTPUT  Pointers to new velocity array
C     PROGRD  INTEGER   NPROC     OUTPUT  Grid number for process
C     PRONDT  INTEGER   NPROC     OUTPUT  Fractional step for process
C
C     Declaration of arguments
C
      use timers

      INTEGER       LUNWRP, LUREP , NOTOT , NIPMSA, NPROC ,
     +              NOLOC , NFLUX , NODEF , IPBLOO, IPCHAR,
     +              IOFFBL, IOFFCH, NOSYS , NDSPX , NVELX ,
     +              NDSPN , NVELN , NOVAR , nrref
      INTEGER       PRVNIO(*)     , IFLUX(*)      , PRVVAR(*)    ,
     +              PRVTYP(*)     , IMODU(*)      , IDPNW(*)     ,
     +              IVPNW(*)      , PROGRD(*)     , PRONDT(*)    ,
     +              VARARR(*)     , VARIDX(*)     , VARTDA(*)    ,
     +              VARDAG(*)     , VARTAG(*)     , VARAGG(*)    ,
     &              proref(*)     , prvpnt(*)
      REAL          DEFAUL(*)     , STOCHI(*)     , DSTO(*)      ,
     +              VSTO(*)
      CHARACTER*(*) LCH
      CHARACTER*20  PRONAM(*)
C
C     Local declarations
C
      PARAMETER   ( VERSI1 = 4.51  , VERSI2 = 4.52 )
      INTEGER       NIPMSD, NPROCD, NOLOCD, NFLUXD, NODEFD,
     +              NOTOTD, IOFF
      REAL          VERSIO
C
Cjvb  Store fractional step flag in common CFRACS
C
      COMMON /CFRACS/ IFRACS
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqip", ithandl )
Cjvb
C
C     read and check version number
C
      READ (LUNWRP, ERR=900, END=900) VERSIO
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
      READ (LUNWRP, ERR=900, END=900) NIPMSD, NPROCD, NFLUXD,
     +                                NOLOCD, NODEFD, NOTOTD,
     +                                NOSYSD, NDSPXD, NVELXD,
     +                                NLOCXD, NDSPND, NVELND,
     +                                NOVARD, nrrefD
      IF ( NIPMSD .NE. NIPMSA ) THEN
         WRITE ( LUREP, 2020 ) NIPMSD, NIPMSA
         IERR = IERR + 1
      ENDIF
      IF ( NPROCD .NE. NPROC  ) THEN
         WRITE ( LUREP, 2030 ) NPROCD, NPROC
         IERR = IERR + 1
      ENDIF
      IF ( NFLUXD .NE. NFLUX  ) THEN
         WRITE ( LUREP, 2040 ) NFLUXD, NFLUX
         IERR = IERR + 1
      ENDIF
      IF ( NOLOCD .NE. NOLOC  ) THEN
         WRITE ( LUREP, 2050 ) NOLOCD, NOLOC
         IERR = IERR + 1
      ENDIF
      IF ( NODEFD .NE. NODEF  ) THEN
         WRITE ( LUREP, 2060 ) NODEFD, NODEF
         IERR = IERR + 1
      ENDIF
      IF ( NOTOTD .NE. NOTOT  ) THEN
         WRITE ( LUREP, 2070 ) NOTOTD, NOTOT
         IERR = IERR + 1
      ENDIF
      IF ( NOSYSD .NE. NOSYS  ) THEN
         WRITE ( LUREP, 2120 ) NOSYSD, NOSYS
         IERR = IERR + 1
      ENDIF
      IF ( NDSPXD .NE. NDSPX  ) THEN
         WRITE ( LUREP, 2130 ) NDSPXD, NDSPX
         IERR = IERR + 1
      ENDIF
      IF ( NVELXD .NE. NVELX  ) THEN
         WRITE ( LUREP, 2140 ) NVELXD, NVELX
         IERR = IERR + 1
      ENDIF
      IF ( NLOCXD .NE. NLOCX  ) THEN
         WRITE ( LUREP, 2150 ) NLOCXD, NLOCX
         IERR = IERR + 1
      ENDIF
      IF ( NDSPND .NE. NDSPN  ) THEN
         WRITE ( LUREP, 2160 ) NDSPND, NDSPN
         IERR = IERR + 1
      ENDIF
      IF ( NVELND .NE. NVELN  ) THEN
         WRITE ( LUREP, 2170 ) NVELND, NVELN
         IERR = IERR + 1
      ENDIF
      IF ( NOVARD .NE. NOVAR  ) THEN
         WRITE ( LUREP, 2190 ) NOVARD, NOVAR
         IERR = IERR + 1
      ENDIF
      IF ( nrrefD .NE. nrref  ) THEN
         WRITE ( LUREP, 2200 ) nrrefd, nrref
         IERR = IERR + 1
      ENDIF
      IF ( IERR .GT. 0 ) GOTO 910
C
      READ (LUNWRP, ERR=900, END=900) ( PRVNIO(K), K = 1 , NPROC )
      READ (LUNWRP, ERR=900, END=900) ( IFLUX(K) , K = 1 , NPROC )
      READ (LUNWRP, ERR=900, END=900) ( PRVVAR(K), K = 1 , NIPMSA)
      READ (LUNWRP, ERR=900, END=900) ( PRVTYP(K), K = 1 , NIPMSA)
      READ (LUNWRP, ERR=900, END=900) ( DEFAUL(K), K = 1 , NODEF )
      READ (LUNWRP, ERR=900, END=900) ( STOCHI(K), K = 1 , NOTOT*NFLUX )
      READ (LUNWRP, ERR=900, END=900) ( DSTO(K)  , K = 1 , NOSYS*NDSPX )
      READ (LUNWRP, ERR=900, END=900) ( VSTO(K)  , K = 1 , NOSYS*NVELX )
      IF ( NDSPN .GT. 0 ) THEN
         READ (LUNWRP, ERR=900, END=900) ( IDPNW(K)  , K = 1 , NOSYS )
      ENDIF
      IF ( NVELN .GT. 0 ) THEN
         READ (LUNWRP, ERR=900, END=900) ( IVPNW(K)  , K = 1 , NOSYS )
      ENDIF
      READ (LUNWRP, ERR=900, END=900) ( PRONAM(K)(1:6), K = 1 , NPROC )
      READ (LUNWRP, ERR=900, END=900) ( PROGRD(K)  , K = 1 , NPROC )
      READ (LUNWRP, ERR=900, END=900) ( PRONDT(K)  , K = 1 , NPROC )
      READ (LUNWRP, ERR=900, END=900) ( VARARR(K)  , K = 1 , NOVAR )
      READ (LUNWRP, ERR=900, END=900) ( VARIDX(K)  , K = 1 , NOVAR )
      READ (LUNWRP, ERR=900, END=900) ( VARTDA(K)  , K = 1 , NOVAR )
      READ (LUNWRP, ERR=900, END=900) ( VARDAG(K)  , K = 1 , NOVAR )
      READ (LUNWRP, ERR=900, END=900) ( VARTAG(K)  , K = 1 , NOVAR )
      READ (LUNWRP, ERR=900, END=900) ( VARAGG(K)  , K = 1 , NOVAR )
      read (lunwrp, err=900, end=900) ( proref(k)  , k = 1 , nproc*nrref )
      k = 1
      do iproc = 1, nproc
         prvpnt(iproc) = k
         k = k + prvnio(iproc)
      enddo
C
C     Set module numbers
C
      DO 20 K = 1,NPROC
         CALL PRONRS ( PRONAM(K), IMODU(K) )
c        IF ( IMODU(K) .EQ. 0 ) THEN
c           WRITE ( LUREP, 2080 ) PRONAM(K)
c           IERR = IERR + 1
c        ENDIF
   20 CONTINUE
C
C     Report on process decomposition
C
      IFRACS = 0
      IPDGRD = 0
      DO K = 1,NPROC
         IF ( PRONDT(K) .GT. 1 ) THEN
            IFRACS = 1
         ENDIF
         IF ( PROGRD(K) .GT. 1 ) THEN
            IPDGRD = 1
         ENDIF
      ENDDO
      IF ( IFRACS .EQ. 0 .AND. IPDGRD .EQ. 0 ) THEN
         WRITE(LUREP,3010)
      ELSE
         WRITE(LUREP,3020)
         DO K = 1,NPROC
            WRITE(LUREP,3000) PRONAM(K),PROGRD(K),PRONDT(K)
         ENDDO
      ENDIF
C
C     Check for Bloom and Charon connection
C
      IPBLOO = 0
      IPCHAR = 0
      IOFFBL = 0
      IOFFCH = 0
      IOFF   = 1
      DO 30 K = 1,NPROC
         IF ( PRONAM(K)(1:6) .EQ. 'D40BLO' ) THEN
            IPBLOO = K
            IOFFBL = IOFF
            WRITE ( LUREP, 2100 )
         ENDIF
         IF ( PRONAM(K)(1:6) .EQ. 'D40CHA' ) THEN
            IPCHAR = K
            IOFFCH = IOFF
            WRITE ( LUREP, 2110 )
         ENDIF
         IOFF = IOFF + PRVNIO(K)
   30 CONTINUE
C
      goto 9999  !    RETURN
C
C     unsuccessful read
C
  900 CONTINUE
      WRITE ( LUREP   , 2090 ) LCH, LUNWRP
      IERR = IERR + 1
C
  910 CONTINUE
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
C     output formats
C
 2000 FORMAT ( ' ERROR  : version proces intput ',F5.2,' NOT supported'
     &        /'          by PROCES system version,',F5.2)
 2010 FORMAT ( ' WARNING: version proces intput ',F5.2,' greater than'
     &        /'          PROCES system version,',F5.2)
 2020 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NIPMSA',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2030 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NPROC ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2040 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NFLUX ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2050 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NOLOC ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2060 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NODEF ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2070 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NOTOT ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2080 FORMAT ( ' ERROR  : Unidentified module requested;',A20)
 2090 FORMAT ( ' ERROR  : Reading proces work file;',A,
     &        /'          on unit number ',I3)
 2100 FORMAT ( ' MESSAGE: Bloom fractional step switched on')
 2110 FORMAT ( ' MESSAGE: Charon fractional step switched on')
 2120 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NOSYS ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2130 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NDSPX ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2140 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NVELX ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2150 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NLOCX ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2160 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NDSPN ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2170 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NVELN ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2180 FORMAT ( ' Process with IO on exchanges set to base grid :',A)
 2190 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NOVAR ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 2200 FORMAT ( ' ERROR  : Proces work file doesn''t match dimensions in'
     &        /'          DELWAQ boot file for NRREF ',
     &        /'          ',I6,' in proces,',I6,' in boot file.')
 3000 FORMAT (/' MODULE :',A,' on grid ',I3,', timestep multiplier:',I3)
 3010 FORMAT (/' No process decomposition active')
 3020 FORMAT (/' Process decomposition active')
C
      END
