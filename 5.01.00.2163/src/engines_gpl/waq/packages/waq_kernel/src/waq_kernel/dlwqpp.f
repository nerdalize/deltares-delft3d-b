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

      SUBROUTINE DLWQPP ( NOTOT  , NOSYS  , NOSEG  , NOPA   , NOSFUN ,
     *                    ITIME  , IMFLAG , IDFLAG , IHFLAG , MONAME ,
     *                    SYNAME , DUNAME , WANAME , IDUMP  , NODUMP ,
     *                    IWASTE , NOWAST , CONC   , CONS   , PARAM  ,
     *                    FUNC   , SEGFUN , VOLUME , WASTE  , BOUND  ,
     *                    NOBND  , ITSTRT , ITSTOP , NX     , NY     ,
     *                    LGRID  , NODISP , NOVELO , NOQ    , NOQ1   ,
     *                    NOQ2   , NOQ3   , DISPER , VELO   , ASMASS ,
     *                    IBFLAG , NOCONS , NOFUN  , CONAME , PANAME ,
     *                    FUNAME , SFNAME , BONAME )
C
C
C     Deltares      SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : june 1988 by L. Postma
C
C     FUNCTION            : Parameter list and header for user supplied
C                           subroutine for POSTPROCESSING.
C
C     LOGICAL UNITS       : none explicitly, the user may use any unit
C                           number if NOT between 10 and 35 !!!!!!!!!!
C
C     SUBROUTINES CALLED  : none explicitly
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOSYS   INTEGER       1     INPUT   Number of active substances
C     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     IMFLAG  LOGICAL       1     INPUT   .TRUE. if DELWAQ sets a
C                                                     monitoring step
C     IDFLAG  LOGICAL       1     INPUT   as IMFLAG for dump actions
C     IHFLAG  LOGICAL       1     INPUT   as IMFLAG for history actions
C     MONAME  CHAR*40       4     INPUT   Model and run names
C     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
C     DUNAME  CHAR*20    NODUMP   INPUT   names of dump locations
C     WANAME  CHAR*20    NOWAST   INPUT   names of waste locations
C     IDUMP   INTEGER    NODUMP   INPUT   dump segment numbers
C     NODUMP  INTEGER       1     INPUT   number of dump locations
C     IWASTE  INTEGER    NOWAST   INPUT   waste segment numbers
C     NOWAST  INTEGER       1     INPUT   number of waste locations
C     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
C     CONS    REAL          *     IN/OUT  Model constants
C     PARAM   REAL    NOPA,NOSEG  IN/OUT  Model parameters
C     FUNC    REAL          *     IN/OUT  Model functions at ITIME
C     SEGFUN  REAL   NOSEG,NOSFUN IN/OUT  Segment functions at ITIME
C     VOLUME  REAL      NOSEG     INPUT   Segment volumes
C     WASTE   REAL NOTOT+1,NOWAST INPUT   waste loads
C     BOUND   REAL   NOSYS,NOBND  INPUT   boundary concentrations
C     NOBND   INTEGER     1       INPUT   number of boundary conditions
C     ITSTRT  INTEGER     1       INPUT   model start time in units
C     ITSTOP  INTEGER     1       INPUT   model stop  time in units
C     NX      INTEGER     1       INPUT   width of grid
C     NY      INTEGER     1       INPUT   depth of grid
C     LGRID   INTEGER     NX*NY   INPUT   grid-layout
C     NOQ     INTEGER       1     INPUT   Total number of exchanges
C     NOQ1    INTEGER       1     INPUT   number of exchanges 1st direction
C     NOQ2    INTEGER       1     INPUT   number of exchanges 2nd direction
C     NOQ3    INTEGER       1     INPUT   number of exchanges 3rd direction
C     NOVELO  INTEGER       1     INPUT   Number of user-flows
C     NODISP  INTEGER       1     INPUT   Number of user-dispersions
C     DISPER  REAL   NODISP*NOQ   OUTPUT  User defined dispersion
C     VELO    REAL   NOVELO*NOQ   OUTPUT  User defined flows
C     ASMASS  REAL  NOTOT*NOSEG*? IN/OUT  mass balance per comp. elems.
C                                         if IBFLAG = 1
C     IBFLAG  INTEGER    1        INPUT   if 1 then mass balance p.c.e.
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     CONAME  CHAR*20   NOCONS    INPUT   Constant names
C     PANAME  CHAR*20   NOPA      INPUT   Parameter names
C     FUNAME  CHAR*20   NOFUN     INPUT   Function names
C     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
C     BONAME  CHAR*20   NOBND     INPUT   Boundary names
C
C     ==================================================================
C
C
C
      CHARACTER*40 MONAME(4)
      CHARACTER*20 SYNAME (NOTOT), DUNAME (NODUMP), WANAME (NOWAST),
     &             CONAME (*)    , PANAME (*)     , FUNAME (*)     ,
     &             SFNAME (*)    , BONAME (*)
      DIMENSION    IDUMP (NODUMP), IWASTE (NOWAST)
      DIMENSION    VOLUME (NOSEG), CONS        (*),FUNC        (*),
     &             CONC   (NOTOT,NOSEG)           ,PARAM   (NOPA,NOSEG),
     &             WASTE (0:NOTOT,NOWAST)         ,SEGFUN(NOSEG,NOSFUN),
     &             BOUND ( NOSYS, NOBND )           ,LGRID (NX    ,NY ),
     &             DISPER( NODISP, NOQ  )           ,VELO  (NOVELO,NOQ),
     &             ASMASS( NOTOT,NOSEG,*)
      LOGICAL IMFLAG, IDFLAG, IHFLAG
C
C
      RETURN
      END
