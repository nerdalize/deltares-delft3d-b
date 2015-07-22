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

      SUBROUTINE DLWQTR ( NOTOT  , NOSYS  , NOSEG  , NOQ    , NOQ1   ,
     +                    NOQ2   , NOQ3   , NOPA   , NOSFUN , NODISP ,
     +                    NOVELO , IPOINT , VOLUME , AREA   , FLOW   ,
     +                    ALENG  , CONC   , DISP   , CONS   , PARAM  ,
     +                    FUNC   , SEGFUN , DISPER , VELO   , ITIME  ,
     +                    IDT    , SYNAME , NOCONS , NOFUN  , CONAME ,
     +                    PANAME , FUNAME , SFNAME , UPDATR , ILFLAG ,
     +                    NPARTp )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:                 by L.Postma
C     REVISED:    august  1997 by Jan van Beek, Delft3D-WAQ functonality
C
C     FUNCTION            : reads SURFACE from coupling
C                           Sets dispersion length in vertical
C
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOSYS   INTEGER       1     INPUT   number of active substances
C     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
C     NOQ     INTEGER       1     INPUT   Total number of exchanges
C     NOQ1    INTEGER       1     INPUT   Nr. of exchanges direction 1
C     NOQ2    INTEGER       1     INPUT   Nr. of exchanges direction 2
C     NOQ3    INTEGER       1     INPUT   Nr. of exchanges direction 3
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     NODISP  INTEGER       1     INPUT   Number of user-dispersions
C     NOVELO  INTEGER       1     INPUT   Number of user-flows
C     IPOINT  INTEGER   4*NOQ     INPUT   1= "From"   segment pointers
C                                 INPUT   2= "To"     segment pointers
C                                 INPUT   3= "From-1" segment pointers
C                                 INPUT   4= "To+1"   segment pointers
C     VOLUME  REAL      NOSEG     INPUT   Segment volumes
C     AREA    REAL        NOQ     INPUT   Exchange surfaces
C     FLOW    REAL        NOQ     INPUT   Flows
C     ALENG a)REAL      2*NOQ     INPUT   1= Length to "From" surface
C                                         2= Length to "To"   surface
C           b)REAL        3       INPUT   3 lengthes in the grid
C     CONC    REAL   NOTOT*NOSEG  INPUT   Model concentrations
C     DISP    REAL        3       IN/OUT  Dispersion in 3 directions
C     CONS    REAL          *     IN/OUT  Model constants
C     PARAM   REAL    NOPA*NOSEG  IN/OUT  Model parameters
C     FUNC    REAL          *     IN/OUT  Model functions at ITIME
C     SEGFUN  REAL   NOSEG*NOSFUN IN/OUT  Segment functions at ITIME
C     DISPER  REAL   NODISP*NOQ   OUTPUT  User defined dispersion
C     VELO    REAL   NOVELO*NOQ   OUTPUT  User defined flows
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     IDT     INTEGER       1     INPUT   Time step system clock units
C     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     CONAME  CHAR*20   NOCONS    INPUT   Constant names
C     PANAME  CHAR*20   NOPA      INPUT   Parameter names
C     FUNAME  CHAR*20   NOFUN     INPUT   Function names
C     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
C     UPDATR  LOGICAL       1     IN/OUT  Flag indicating if the transport
C                                         matrix is changed. The user should
C                                         set this flag to .T. if he alters
C                                         part of the matrix and uses integratio
C                                         option 10.xx .
C     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
C     NPARTp  INTEGER     1       INPUT   number of subdomains in parallel run
C
C     ==================================================================
C
C     Save for all the local index pointers and switches
C
      SAVE
C
      DIMENSION    IPOINT(4,NOQ)
      DIMENSION    VOLUME(NOSEG)     , AREA(NOQ)         ,
     +             FLOW(NOQ)         , ALENG (2,NOQ)     ,
     +             CONC(NOTOT,NOSEG) , DISP(3)           ,
     +             CONS(*)           , PARAM (NOPA,NOSEG),
     +             FUNC(*)           , SEGFUN(NOSEG,*)   ,
     +             VELO(*)           , DISPER(*)
      CHARACTER*20 SYNAME (NOTOT)    , CONAME (*)        ,
     +             PANAME (*)        , FUNAME (*)        ,
     +             SFNAME (*)
      LOGICAL      UPDATR
C
C     Local
C
      PARAMETER  ( LCCCO  = 98 )
      LOGICAL    FIRST ,  LINIT , LEXI
      DATA       FIRST / .TRUE. /
      DATA       LINIT / .FALSE. /
C
C          check usage w.r.t. parallel computing
C
!          AM:
!          I removed this check, as all the computations set up using
!          the Delft3D user-interface have the SURF parameter.
!          Even if not, then the file should be available on all
!          nodes, as they share the directory.
!
!     IF ( NPARTp .GT. 1 ) THEN
!        WRITE(LUNREP,2060) NPARTp
!        CALL SRSTOP(1)
!     ENDIF
C
C          check number of parameters
C
C     Initialisation set index pointers, read surface areas
C
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         IER   = 0
         CALL GETMLU(LUNREP)
         WRITE(LUNREP,*)
         WRITE(LUNREP,2000)
C
C        Set pointers in param array
C
         CALL ZOEK ( 'SURF      ', NOPA  , PANAME , 10    , ISURF  )
C
C          read surface areas
C
         IF ( ISURF .GT. 0 ) THEN
            IF ( ILFLAG .EQ. 1 .AND. NOQ3 .GT. 0 ) THEN
               LINIT = .TRUE.
               WRITE(LUNREP,2040)
            ENDIF
            INQUIRE  ( FILE='areachar.dat', EXIST = LEXI )
            IF ( .NOT. LEXI ) THEN
C
C
C              It is assumed the SURF parameter has been set in the input
C
C              WRITE (LUNREP,2020)
C              WRITE (  *   ,2020)
            ELSE
               OPEN ( LCCCO, FILE='areachar.dat', FORM  ='UNFORMATTED',
     +                       STATUS='OLD'       , IOSTAT=IER2         )
               IF ( IER2 .NE. 0 ) THEN
                  WRITE (LUNREP,2010)
                  WRITE ( *    ,2010)
                  IER = IER + 1
               ELSE
                  WRITE(LUNREP,2030)
                  READ ( LCCCO ) NMAXA, MMAXA, NMA, NMA, NMA, IDUMMY
                  LAYT = NOSEG/NMA
                  NMT = NMA*LAYT
                  IF ( NMT .NE. NOSEG ) THEN
                     WRITE (LUNREP,2050) NMA,LAYT,NMT,NOSEG
                     WRITE (  *   ,2050) NMA,LAYT,NMT,NOSEG
                     IER = IER + 1
                  ENDIF
                  IF ( IER .EQ. 0 ) THEN
                     READ ( LCCCO ) (PARAM(ISURF,K),K=1,NMA)
                     DO 40 ILAY = 2, LAYT
                        DO 45 ISEG = 1, NMA
                           IPOS = (ILAY-1)*NMA + ISEG
                           PARAM(ISURF,IPOS) = PARAM(ISURF,ISEG)
45                      CONTINUE
40                   CONTINUE
                  ENDIF
                  CLOSE ( LCCCO )
               ENDIF
            ENDIF
C
            IF ( IER .NE. 0 ) THEN
               CALL SRSTOP(1)
            ENDIF
         ENDIF
C
         WRITE(LUNREP,2070)
C
      ENDIF
C
C     adapt the length for the third direction
C
      IF ( LINIT ) THEN
         DO 60 IQ = NOQ1 + NOQ2 + 1, NOQ
              IFROM = IPOINT(1,IQ)
              ITO   = IPOINT(2,IQ)
              IF ( IFROM .GT. 0 ) THEN
                 IF ( PARAM(ISURF,IFROM) .GT. 1.0E-15 ) THEN
                      ALENG(1,IQ) = VOLUME(IFROM)/PARAM(ISURF,IFROM)/2.
                 ENDIF
              ENDIF
              IF ( ITO   .GT. 0 ) THEN
                 IF ( PARAM(ISURF,IFROM) .GT. 1.0E-15 ) THEN
                      ALENG(2,IQ) = VOLUME(ITO)/PARAM(ISURF,IFROM)/2.
                 ENDIF
              ENDIF
60       CONTINUE
      ENDIF
C
C     end of the subroutine
C
      RETURN
C
C     Output formats
C
 2000 FORMAT (' Extra functionality DLWQTR')
 2010 FORMAT (' ERROR: opening file <areachar.dat> !')
 2020 FORMAT (' WARNING: no <areachar.dat> file!',/,
     +        '          rectilinear simulation assumed!',/,
     +        '          param [SURF] must have been set by the',/,
     +        '          user, this is not checked !!!!')
 2030 FORMAT (' Surface area''s will be read from file <areachar.dat>')
 2040 FORMAT (' Dispersion length in third dir. will be calculated')
 2050 FORMAT (' ERROR: File areachar.dat does not match.',
     +        ' NMA = ',I8,' LAYT= ',I8,' NMT = ',I8,' NOSEG=',I8)
 2060 FORMAT (' ERROR: User-supplied transport processes (DLWQTR) may n
     +ot be used',/,
     +        '        in parallel runs (NPART=',i3,').')
 2070 FORMAT (' End extra functionality DLWQTR')
C
      END
