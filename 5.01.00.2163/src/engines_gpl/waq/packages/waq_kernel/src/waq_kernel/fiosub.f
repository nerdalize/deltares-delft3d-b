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

      SUBROUTINE FIOSUB (OUTVAL, IOPOIN, NRVAR , NOCONS, NOPA  ,
     +                   NOFUN , NOSFUN, NOTOT , CONC  , SEGFUN,
     +                   FUNC  , PARAM , CONS  , IDT   , ITIME ,
     +                   VOLUME, NOSEG , NOSYS , NDMPAR, IPDMP ,
     +                   BOUND , NOLOC , PROLOC, NODEF , DEFAUL,
     +                   NCOUT , NTDMPQ, paname, sfname)
      use timers
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : may 1993 by Jan van Beek
C
C     FUNCTION            : Fills output buffer OUTVAL on sub grid.
C
C     SUBROUTINES CALLED  : ZERO  , zero's a real array
C
C     FILES               : -
C
C     PARAMETERS          : 27
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     OUTVAL  REAL  NCOUT+NRVAR,* OUTPUT  Values for vars on output grid
C     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
C     NRVAR   INTEGER       1     INPUT   Number of extra output vars
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
C     SEGFUN  REAL   NOSEG,NOSFUN INPUT   Segment functions at ITIME
C     FUNC    REAL          *     INPUT   Model functions at ITIME
C     PARAM   REAL    NOPA,NOSEG  INPUT   Model parameters
C     CONS    REAL          *     INPUT   Model constants
C     IDT     INTEGER       1     INPUT   Simulation timestep
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     VOLUME  REAL      NOSEG     INPUT   Segment volumes
C     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
C     NOSYS   INTEGER       1     INPUT   Number of active substances
C     NDMPAR  INTEGER       1     INPUT   number of dump locations
C     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
C     BOUND   REAL     NOTOT*?    INPUT   boundary      values
C     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
C     PARAM   REAL   NOLOC,NOSEG  INPUT   Parameters local in PROCES system
C     NODEF   INTEGER       1     INPUT   Number of used defaults
C     DEFAUL  REAL          *     INPUT   Default proces parameters
C     NCOUT   INTEGER       1     INPUT   number of conc in output
C     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
C
C     Declaration of arguments
C
      INTEGER    NRVAR , NOCONS, NOPA  , NOFUN , NOSFUN,
     +           NOTOT , IDT   , ITIME , NOSEG , NOSYS ,
     +           NDMPAR, NOLOC , NODEF , NCOUT , NTDMPQ
      INTEGER    IOPOIN(*)      , IPDMP(*)
      REAL       OUTVAL(*)      , CONC(NOTOT,*),
     +           segfun(noseg,nosfun), FUNC(*) ,
     +           param (nopa ,noseg ), CONS(*) ,
     +           VOLUME(*)      , BOUND(*)     ,
     +           PROLOC(*)      , DEFAUL(*)
      character(20) paname(*) , sfname(*)
C
C     Local
C
      PARAMETER ( RMISS = -999. )
      PARAMETER ( NOPRED= 6     )
      INTEGER     IOPA  , IOFUNC, IOSFUN, IOCONC, IOLOC ,
     +            IODEF , IP    , IP1   , IP2   , ITEL2 ,
     +            ISYS  , IVAR  , IDUMP , ISC   , ISEG  ,
     +            NSC   , IOFDMP, IOCONS, IIP   , IIDUMP
      REAL        HLPVAR, HLPCUM, VALCUM, VALVAR
      logical     parm
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "fiosub", ithandl )
C
C     Pointer offsets
C
      IOCONS = NOPRED + 1
      IOPA   = IOCONS + NOCONS
      IOFUNC = IOPA   + NOPA
      IOSFUN = IOFUNC + NOFUN
      IOCONC = IOSFUN + NOSFUN
      IOLOC  = IOCONC + NOTOT
      IODEF  = IOLOC  + NOLOC
C
C     Zero the output buffer for it is used as accumulation variable.
C
      CALL ZERO(OUTVAL,(NCOUT+NRVAR)*NDMPAR)
C
C     Fill the output buffer OUTVAL
C
      IP1   = NDMPAR + NTDMPQ
      ITEL2 = NDMPAR + NTDMPQ + NDMPAR
C
C     Loop over the dump area's
C
      DO 300 IDUMP = 1 , NDMPAR
C
C        the segment contributes
C
         NSC = IPDMP(IP1+IDUMP)
         IOFDMP = (IDUMP-1)*(NCOUT+NRVAR)
C
C        If one segment don't bother to calculate mean value
C
         IF ( NSC .EQ. 1 ) THEN
            ITEL2 = ITEL2 + 1
            ISEG  = IPDMP(ITEL2)
C
C           The substances
C
            IF ( NCOUT .GT. 0 ) THEN
               IF ( ISEG .LT. 0 ) THEN
                  DO 10 ISYS = 1 , NOSYS
                     IIDUMP = IOFDMP+ISYS
                     IIP = (-ISEG-1)*NOSYS + ISYS
                     OUTVAL(IIDUMP) = BOUND(IIP)
  10              CONTINUE
                  DO 20 ISYS = NOSYS+1 , NOTOT
                     IIDUMP = IOFDMP+ISYS
                     OUTVAL(IIDUMP) = RMISS
  20              CONTINUE
               ELSEIF ( ISEG .EQ. 0 ) THEN
                  DO 30 ISYS = 1 , NOTOT
                     IIDUMP = IOFDMP+ISYS
                     OUTVAL(IIDUMP) = RMISS
  30              CONTINUE
               ELSE
                  DO 40 ISYS = 1 , NOTOT
                     IIDUMP = IOFDMP+ISYS
                     OUTVAL(IIDUMP) = CONC(ISYS,ISEG)
  40              CONTINUE
               ENDIF
            ENDIF
C
C           The extra variables
C
            DO 50 IVAR = 1 , NRVAR
               IP = IOPOIN(IVAR)
               IIDUMP = IOFDMP+NCOUT+IVAR
               IF ( ISEG .LT. 0 ) THEN
                  IF ( IP .GE. IOCONC .AND. IP .LT. IOCONC+NOSYS ) THEN
                     IIP = (-ISEG-1)*NOSYS + IP-IOCONC+1
                     OUTVAL(IIDUMP) = BOUND(IIP)
                  ELSE
                     OUTVAL(IIDUMP) = RMISS
                  ENDIF
               ELSEIF ( ISEG .EQ. 0 ) THEN
                  OUTVAL(IIDUMP) = RMISS
               ELSE
                  IF ( IP .GE. IODEF  ) THEN
                     OUTVAL(IIDUMP) = DEFAUL(IP-IODEF+1)
                  ELSEIF ( IP .GE. IOLOC  ) THEN
                     IIP = (ISEG-1)*NOLOC + IP-IOLOC+1
                     OUTVAL(IIDUMP) = PROLOC(IIP)
                  ELSEIF ( IP .GE. IOCONC ) THEN
                     OUTVAL(IIDUMP) = CONC(IP-IOCONC+1,ISEG)
                  ELSEIF ( IP .GE. IOSFUN ) THEN
                     OUTVAL(IIDUMP) = SEGFUN(ISEG,IP-IOSFUN+1)
                  ELSEIF ( IP .GE. IOFUNC ) THEN
                     OUTVAL(IIDUMP) = FUNC(IP-IOFUNC+1)
                  ELSEIF ( IP .GE. IOPA ) THEN
                     outval(iidump) = param(ip-iopa+1,iseg)
                  ELSEIF ( IP .GE. IOCONS ) THEN
                     OUTVAL(IIDUMP) = CONS(IP-IOCONS+1)
                  ELSEIF ( IP .EQ. 3 ) THEN
                     OUTVAL(IIDUMP) = REAL(IDT)
                  ELSEIF ( IP .EQ. 2 ) THEN
                     OUTVAL(IIDUMP) = REAL(ITIME)
                  ELSEIF ( IP .EQ. 1 ) THEN
                     OUTVAL(IIDUMP) = VOLUME(ISEG)
                  ELSEIF ( IP .LE. 0 ) THEN
                     OUTVAL(IIDUMP) = RMISS
                  ENDIF
               ENDIF
C
  50        CONTINUE
C
         ELSE
C
C           The substances ( if asked ) in one loop
C
            IF ( NCOUT .GT. 0 ) THEN
C
C              Zero the accummulative variables, OUTVAL already done.
C
               HLPCUM = 0.0
               if ( nosys .ne. notot ) then
                  cumsrf = 1.0
                  call zoek20 ( 'SURF      ', nopa, paname, 10, indx )
                  if  ( indx .gt. 0 ) then
                     cumsrf = 0.0
                     parm = .true.
                  else
                     call zoek20 ( 'SURF      ', nosfun, sfname, 10, indx )
                     if  ( indx .gt. 0 ) then
                        cumsrf = 0.0
                        parm = .false.
                     endif
                  endif
               endif
C
               DO 120 ISC = 1 , NSC
                  ISEG  = IPDMP(ITEL2+ISC)
C
C                 Accumulate VOLUME, substances in OUTVAL
C
                  IF ( ISEG .GT. 0 ) THEN
                     HLPVAR = VOLUME(ISEG)
                     HLPCUM = HLPCUM + HLPVAR
C
C                    All active substances weighted by volume
C
                     DO 100 ISYS = 1 , NOSYS
                        IIDUMP = IOFDMP+ISYS
                        OUTVAL(IIDUMP) = OUTVAL(IIDUMP) +
     +                                   CONC(ISYS,ISEG)*HLPVAR
  100                CONTINUE

!       take care of mass/m2 for inactive substances if possible

                     if ( nosys .ne. notot ) then
                        if  ( indx .gt. 0 ) then
                           if ( parm ) then
                              srf = param (indx,iseg)
                           else
                              srf = segfun(iseg,indx)
                           endif
                           cumsrf = cumsrf + srf
                           do isys = nosys+1, notot
                              iidump = iofdmp+isys
                              outval(iidump) = outval(iidump)+conc(isys,iseg)*srf
                           enddo
                        else
                           do isys = nosys+1, notot
                              iidump = iofdmp+isys
                              outval(iidump) = outval(iidump)+conc(isys,iseg)
                           enddo
                        endif
                     endif
                  ENDIF
C
  120          CONTINUE
C
C              Calculate mean for then active and inactive substances
C
               IF ( ABS(HLPCUM) .GT. 1.0E-20 ) THEN
                  DO 130 ISYS = 1 , NOSYS
                     IIDUMP = IOFDMP+ISYS
                     OUTVAL(IIDUMP) = OUTVAL(IIDUMP) / HLPCUM
  130             CONTINUE
               ENDIF
               if ( abs(cumsrf) .gt. 1.0e-20 ) then
                  do isys = nosys+1, notot
                     iidump = iofdmp+isys
                     outval(iidump) = outval(iidump) / cumsrf
                  enddo
               endif
C
            ENDIF
C
C           The extra output variables each in a seperate loop
C
            DO 210 IVAR = 1 , NRVAR
C
C              Accumulate
C
               IP  = IOPOIN(IVAR)
               IP2 = IOPOIN(NRVAR+IVAR)
               VALCUM = 0.0
               HLPCUM = 0.0
               DO 200 ISC = 1 , NSC
                  ISEG  = IPDMP(ITEL2+ISC)
C
C                 The output variable
C
                  IF ( ISEG .GT. 0 ) THEN
                     IP = IOPOIN(IVAR)
                     IF ( ISEG .LT. 0 ) THEN
                        IF ( IP.GE.IOCONC .AND. IP.LT.IOCONC+NOSYS ) THEN
                           IIP = (-ISEG-1)*NOSYS + IP-IOCONC+1
                           VALVAR = BOUND(IIP)
                        ELSE
                           VALVAR = 0.0
                        ENDIF
                     ELSEIF ( ISEG .EQ. 0 ) THEN
                        VALVAR = 0.0
                     ELSE
                        IF ( IP .GE. IODEF  ) THEN
                           VALVAR = DEFAUL(IP-IODEF+1)
                        ELSEIF ( IP .GE. IOLOC  ) THEN
                           IIP = (ISEG-1)*NOLOC + IP-IOLOC+1
                           VALVAR = PROLOC(IIP)
                        ELSEIF ( IP .GE. IOCONC ) THEN
                           VALVAR = CONC(IP-IOCONC+1,ISEG)
                        ELSEIF ( IP .GE. IOSFUN ) THEN
                           VALVAR = SEGFUN(ISEG,IP-IOSFUN+1)
                        ELSEIF ( IP .GE. IOFUNC ) THEN
                           VALVAR = FUNC(IP-IOFUNC+1)
                        ELSEIF ( IP .GE. IOPA ) THEN
                           valvar = param(ip -iopa+1,iseg)
                        ELSEIF ( IP .GE. IOCONS ) THEN
                           VALVAR = CONS(IP-IOCONS+1)
                        ELSEIF ( IP .EQ. 3 ) THEN
                           VALVAR = REAL(IDT)
                        ELSEIF ( IP .EQ. 2 ) THEN
                           VALVAR = REAL(ITIME)
                        ELSEIF ( IP .EQ. 1 ) THEN
                           VALVAR = VOLUME(ISEG)
                        ELSEIF ( IP .LE. 0 ) THEN
                           VALVAR = RMISS
                        ENDIF
                     ENDIF
C
C                    The weigth variable
C
                     IF ( ISEG .LT. 0 ) THEN
                        HLPVAR = 1.0
                     ELSEIF ( ISEG .EQ. 0 ) THEN
                        HLPVAR = 0.0
                     ELSE
                        IF ( IP2 .GE. IODEF  ) THEN
                           HLPVAR = DEFAUL(IP2-IODEF+1)
                        ELSEIF ( IP2 .GE. IOLOC  ) THEN
                           IIP = (ISEG-1)*NOLOC + IP2-IOLOC+1
                           HLPVAR = PROLOC(IIP)
                        ELSEIF ( IP2 .GE. IOCONC ) THEN
                           HLPVAR = CONC(IP2-IOCONC+1,ISEG)
                        ELSEIF ( IP2 .GE. IOSFUN ) THEN
                           HLPVAR = SEGFUN(ISEG,IP2-IOSFUN+1)
                        ELSEIF ( IP2 .GE. IOFUNC ) THEN
                           HLPVAR = FUNC(IP2-IOFUNC+1)
                        ELSEIF ( IP2 .GE. IOPA ) THEN
                           hlpvar = param(ip2-iopa+1,iseg)
                        ELSEIF ( IP2 .GE. IOCONS ) THEN
                           HLPVAR = CONS(IP2-IOCONS+1)
                        ELSEIF ( IP2 .EQ. 3 ) THEN
                           HLPVAR = REAL(IDT)
                        ELSEIF ( IP2 .EQ. 2 ) THEN
                           HLPVAR = REAL(ITIME)
                        ELSEIF ( IP2 .EQ. 1 ) THEN
                           HLPVAR = VOLUME(ISEG)
                        ELSEIF ( IP2 .EQ. 0 ) THEN
                           HLPVAR = 0.0
                        ELSEIF ( IP2 .LT. 0 ) THEN
                           HLPVAR = 1.
                        ENDIF
                     ENDIF
C
                     IF ( IP2 .EQ. 0 ) THEN
                        VALCUM = VALCUM + VALVAR
                     ELSE
                        VALCUM = VALCUM + VALVAR * HLPVAR
                        HLPCUM = HLPCUM + HLPVAR
                     ENDIF
                  ENDIF
C
  200          CONTINUE
C
C              Calculate mean , HLPCUM = 0.0 has a double meaning
C              1. only accumulation, 2. no divide by zero HLPCUM
C
               IIDUMP = IOFDMP+NCOUT+IVAR
C
               IF ( ABS(HLPCUM) .GT. 1.0E-20 ) THEN
                  OUTVAL(IIDUMP) = VALCUM / HLPCUM
               ELSE
                  OUTVAL(IIDUMP) = VALCUM
               ENDIF
C
  210       CONTINUE
C
            ITEL2 = ITEL2 + NSC
         ENDIF
C
  300 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
