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
     *                    NOQ2   , NOQ3   , NOPA   , NOSFUN , NODISP ,
     *                    NOVELO , IPOINT , VOLUME , AREA   , FLOW   ,
     *                    ALENG  , CONC   , DISP   , CONS   , PARAM  ,
     *                    FUNC   , SEGFUN , DISPER , VELO   , ITIME  ,
     *                    IDT    , SYNAME , NOCONS , NOFUN  , CONAME ,
     *                    PANAME , FUNAME , SFNAME , UPDATR , ILFLAG ,
     *                    NPARTp )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:    march 1988 by L.Postma
C
C     FUNCTION            : Parameter list and header for user supplied
C                           subroutine for TRANSPORT processes.
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
C     ALENG   REAL      2*NOQ     INPUT   1= Length to "From" surface
C                                         2= Length to "To"   surface
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
C                                         option 5 )
C     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
C     NPARTp  INTEGER     1       INPUT   number of subdomains in parallel run
C
C     ==================================================================
C
      DIMENSION  IPOINT( 4,NOQ )
      DIMENSION  VOLUME(NOSEG) , AREA(NOQ) , FLOW(NOQ) ,
     &           ALENG (2,NOQ) , CONC  (NOTOT,NOSEG)       , DISP( 3 ) ,
     &           CONS  (  *  ) , PARAM (  NOPA,NOSEG )     , FUNC( * ) ,
     &           SEGFUN( NOSEG , NOSFUN ) ,
     &           DISPER( NODISP,  NOQ) , VELO  (NOVELO,NOQ)
      CHARACTER*20 SYNAME (NOTOT), CONAME (*), PANAME (*),
     &             FUNAME (*)    , SFNAME (*)
      LOGICAL    UPDATR
C
C          check usage w.r.t. parallel computing
C          activate this check when your routine is not parallellized.
C
C     IF ( NPARTp .GT. 1 ) THEN
C        WRITE(LUNREP,2060) NPARTp
C        CALL SRSTOP
C     ENDIF
C
C
      RETURN
C
C     Output formats
C
 2060 FORMAT (' ERROR: User-supplied transport processes (DLWQTR) may not be used',/,
     +        '        in parallel runs (NPART=',i3,').')
      END
