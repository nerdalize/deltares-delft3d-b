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

      SUBROUTINE DLWQWQ ( NOTOT  , NOSYS  , NOSEG  , NOPA   , NOSFUN ,
     *                    VOLUME , CONC   , CONS   , PARAM  , FUNC   ,
     *                    SEGFUN , DERIV  , ITIME  , IDT    , ASMASS ,
     *                    IBFLAG , SYNAME , NOCONS , NOFUN  , CONAME ,
     *                    PANAME , FUNAME , SFNAME , NODUMP , IDUMP  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:    march 1988 by L.Postma
C
C     FUNCTION            : WATERQUALITY subroutine
C
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
C     VOLUME  REAL      NOSEG     INPUT   Segment volumes
C     CONC    REAL   NOTOT*NOSEG  INPUT   Model concentrations, first
C                                         NOSYS are active and expressed
C                                         as concentrations, others are
C                                         inactive, expressed in MASS!!!
C     CONS    REAL          *     IN/OUT  Model constants
C     PARAM   REAL    NOPA*NOSEG  IN/OUT  Model parameters
C     FUNC    REAL          *     IN/OUT  Model functions at ITIME
C     SEGFUN  REAL   NOSEG*NOSFUN IN/OUT  Segment functions at ITIME
C     DERIV   REAL   NOTOT*NOSEG  OUTPUT  Model derivatives
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     IDT     INTEGER       1     INPUT   Time step system clock units
C     ASMASS  REAL  NOTOT*NOSEG*? IN/OUT  mass balance per comp. elems.
C                                         if IBFLAG = 1
C     IBFLAG  INTEGER    1        INPUT   if 1 then mass balance p.c.e.
C     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     CONAME  CHAR*20   NOCONS    INPUT   Constant names
C     PANAME  CHAR*20   NOPA      INPUT   Parameter names
C     FUNAME  CHAR*20   NOFUN     INPUT   Function names
C     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
C     NODUMP  INTEGER       1     INPUT   Number of monitor points
C     IDUMP   INTEGER   NODUMP    INPUT   Segment numbers monitor points
C
C     ==================================================================
C
C          DIMENSION  PARAM (NOPA,NOSEG) , SEGFUN(NOSEG,NOSFUN)
C
C     NOTE that previous dimension statement is only correct if
C          PARAM and NOSFUN are larger than zero, which is not
C          always the case. The user may insert 2-D dimensioning
C          for applications with non-zero PARAM and/or NOSFUN.
C
      CHARACTER*20 SYNAME (NOTOT), CONAME (*), PANAME (*),
     &             FUNAME (*)    , SFNAME (*)
      DIMENSION    VOLUME(NOSEG) , CONC  (NOTOT,NOSEG) , CONS(*) ,
     &             FUNC  (  *  ) , DERIV (NOTOT,NOSEG) ,
     &             PARAM (NOPA,NOSEG) , SEGFUN(NOSEG,NOSFUN),
     &             ASMASS(NOTOT,NOSEG,*), IDUMP(*)
C
C          user-defined waterquality processes
C                -  expressed in mass per time
C                -  positive derivative is growth
C                -  DERIV is zero at entrance
C                -  CONC   may be changed but it has no effect
C                -  VOLUME may NEVER be changed
C
      RETURN
      END
