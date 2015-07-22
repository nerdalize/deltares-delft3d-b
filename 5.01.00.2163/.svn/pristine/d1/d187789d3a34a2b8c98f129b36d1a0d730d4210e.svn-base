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

      SUBROUTINE FIOUTV (OUTVAL, IOPOIN, NRVAR , NOCONS, NOPA  ,
     +                   NOFUN , NOSFUN, NOTOT , CONC  , SEGFUN,
     +                   FUNC  , PARAM , CONS  , IDT   , ITIME ,
     +                   VOLUME, NOSEG , NOSYS , NODUMP, IDUMP ,
     +                   NX    , NY    , LGRID , IGRID , BOUND ,
     +                   NOLOC , PROLOC, NODEF , DEFAUL)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : may 1993 by Jan van Beek
C
C     FUNCTION            : Fills output buffer OUTVAL.
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     PARAMETERS          : 29
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     OUTVAL  REAL    NRVAR,*     OUTPUT  Values for vars on output grid
C     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
C     NRVAR   INTEGER       1     INPUT   Number of output vars
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
C     SEGFUN  REAL   NOSEG,NOSFUN IN/OUT  Segment functions at ITIME
C     FUNC    REAL          *     IN/OUT  Model functions at ITIME
C     PARAM   REAL    NOPA,NOSEG  IN/OUT  Model parameters
C     CONS    REAL          *     IN/OUT  Model constants
C     IDT     INTEGER       1     INPUT   Simulation timestep
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     VOLUME  REAL      NOSEG     INPUT   Segment volumes
C     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
C     NOSYS   INTEGER       1     INPUT   Number of active substances
C     NODUMP  INTEGER       1     INPUT   number of dump locations
C     IDUMP   INTEGER    NODUMP   INPUT   dump segment numbers
C     NX      INTEGER       1     INPUT   Width of output grid
C     NY      INTEGER       1     INPUT   Depth of output grid
C     LGRID   INTEGER     NX*NY   INPUT   grid-layout
C     IGRID   INTEGER       1     INPUT   Output grid indication
C     BOUND   REAL     NOTOT*?    INPUT   boundary      values
C     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
C     PARAM   REAL   NOLOC,NOSEG  INPUT   Parameters local in PROCES system
C     NODEF   INTEGER       1     INPUT   Number of used defaults
C     DEFAUL  REAL          *     INPUT   Default proces parameters
C
C     Declaration of arguments
C
      use timers

      INTEGER    NRVAR , NOCONS, NOPA  , NOFUN , NOSFUN,
     +           NOTOT , IDT   , ITIME , NOSEG , NOSYS ,
     +           NODUMP, NX    , NY    , IGRID , NOLOC ,
     +           NODEF
      INTEGER    IOPOIN(*)     , IDUMP(*)      ,
     +           LGRID(*)
      REAL       OUTVAL(*)      , CONC(NOTOT,*),
     +           SEGFUN(NOSEG,*), FUNC(*)      ,
     +           PARAM(*)       , CONS(*)      ,
     +           VOLUME(*)      , BOUND(*)     ,
     +           PROLOC(*)      , DEFAUL(*)
C
C     Local
C
      PARAMETER ( IGSEG = 1 , IGMON = 2 , IGGRD = 3 , IGSUB = 4 )
      PARAMETER ( RMISS = -999. )
      PARAMETER ( NOPRED= 6 )
      INTEGER     IOPA  , IOFUNC, IOSFUN, IOCONC, IOLOC ,
     +            IODEF , IP
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "fioutv", ithandl )
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
C     GRID
C
      IF ( IGRID .EQ. IGSEG ) THEN
         NOCEL = NOSEG
      ELSEIF ( IGRID .EQ. IGMON ) THEN
         NOCEL = NODUMP
      ELSEIF ( IGRID .EQ. IGGRD ) THEN
         NOCEL = NX*NY
      ENDIF
C
C     FILL OUTVAL
C
      DO 200 ICEL = 1 , NOCEL
C
C        What segment ?
C
         IF ( IGRID .EQ. IGSEG ) THEN
            ISEG = ICEL
         ELSEIF ( IGRID .EQ. IGMON ) THEN
            ISEG = IDUMP(ICEL)
         ELSEIF ( IGRID .EQ. IGGRD ) THEN
            ISEG = LGRID(ICEL)
         ENDIF

         DO 100 I = 1 , NRVAR
            IICEL = (ICEL-1)*NRVAR+I
            IP = IOPOIN(I)
C
C           What value
C
            IF ( ISEG .LT. 0 ) THEN
               IF ( IP .GE. IOCONC .AND. IP .LT. IOCONC+NOSYS ) THEN
                  IIP = (-ISEG-1)*NOSYS + IP-IOCONC+1
                  OUTVAL(IICEL) = BOUND(IIP)
               ELSE
                  OUTVAL(IICEL) = RMISS
               ENDIF
            ELSEIF ( ISEG .EQ. 0 ) THEN
               OUTVAL(IICEL) = RMISS
            ELSE
               IF ( IP .GE. IODEF  ) THEN
                  OUTVAL(IICEL) = DEFAUL(IP-IODEF+1)
               ELSEIF ( IP .GE. IOLOC  ) THEN
                  IIP = (ISEG-1)*NOLOC + IP-IOLOC+1
                  OUTVAL(IICEL) = PROLOC(IIP)
               ELSEIF ( IP .GE. IOCONC ) THEN
                  OUTVAL(IICEL) = CONC(IP-IOCONC+1,ISEG)
               ELSEIF ( IP .GE. IOSFUN ) THEN
                  OUTVAL(IICEL) = SEGFUN(ISEG,IP-IOSFUN+1)
               ELSEIF ( IP .GE. IOFUNC ) THEN
                  OUTVAL(IICEL) = FUNC(IP-IOFUNC+1)
               ELSEIF ( IP .GE. IOPA ) THEN
                  IIP = (ISEG-1)*NOPA + IP-IOPA+1
                  OUTVAL(IICEL) = PARAM(IIP)
               ELSEIF ( IP .GE. IOCONS ) THEN
                  OUTVAL(IICEL) = CONS(IP-IOCONS+1)
               ELSEIF ( IP .EQ. 3 ) THEN
                  OUTVAL(IICEL) = REAL(IDT)
               ELSEIF ( IP .EQ. 2 ) THEN
                  OUTVAL(IICEL) = REAL(ITIME)
               ELSEIF ( IP .EQ. 1 ) THEN
                  OUTVAL(IICEL) = VOLUME(ISEG)
               ELSEIF ( IP .LE. 0 ) THEN
                  OUTVAL(IICEL) = RMISS
               ENDIF
            ENDIF
  100    CONTINUE
  200 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
