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

      SUBROUTINE ACTLOC (IOPOIN, NRVAR , NOCONS, NOPA  , NOFUN ,
     +                   NOSFUN, NOTOT , NOSEG , NOLOC , NOGRID,
     +                   NOVAR , VARARR, VARIDX, VARTDA, VARDAG,
     +                   ARRKND, ARRPOI, ARRDM1, ARRDM2, VGRSET,
     +                   GRDNOS, GRDSEG, A     )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : may 1993 by Jan van Beek
C
C     FUNCTION            : Sets all variable from the LOCAL array used
C                           for output actual for the base grid.
C                           (ouput always uses the value from base grid)
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
C     NRVAR   INTEGER       1     INPUT   Number of output vars
C     NOCONS  INTEGER       1     INPUT   Number of constants used
C     NOPA    INTEGER       1     INPUT   Number of parameters
C     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
C     NOSFUN  INTEGER       1     INPUT   Number of segment functions
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
C     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
C     NOGRID  INTEGER       1     INPUT   Number of grids
C     NOVAR   INTEGER       1     INPUT   Number of variables
C     VARARR  INTEGER   NOVAR     INPUT   Variable array number
C     VARIDX  INTEGER   NOVAR     INPUT   Variable index in array
C     VARTDA  INTEGER   NOVAR     INPUT   Type of disaggregation
C     VARDAG  INTEGER   NOVAR     INPUT   Variable disaggr. weight var.
C     ARRKND  INTEGER   NOARR     INPUT   Kind of array
C     ARRPOI  INTEGER   NOARR     INPUT   Array pointer in A
C     ARRDM1  INTEGER   NOARR     INPUT   First dimension
C     ARRDM2  INTEGER   NOARR     INPUT   Second dimension
C     VGRSET  INTEGER   NOVAR,*   IN/OUT  Actual indication
C     GRDNOS  INTEGER   NOGRID    INPUT   Number of segments in grid
C     GRDSEG  INTEGER   NOGRID    INPUT   Segment pointering
C     A       REAL      *         IN/OUT  Real array work space
C
C     Declaration of arguments
C
      use timers

      INTEGER    NRVAR , NOCONS, NOPA  , NOFUN , NOSFUN,
     +           NOTOT , NOSEG , NOLOC , NOGRID, NOVAR
      INTEGER    IOPOIN(NRVAR) , VARARR(NOVAR) ,
     +           VARIDX(NOVAR) , VARTDA(NOVAR) ,
     +           VARDAG(NOVAR) , ARRKND(*)     ,
     +           ARRPOI(*)     , ARRDM1(*)     ,
     +           ARRDM2(*)     , VGRSET(NOVAR,*),
     +           GRDNOS(NOGRID), GRDSEG(NOSEG,NOGRID)
      REAL       A(*)
C
C     Local
C
      PARAMETER ( NOPRED= 6 )
      INTEGER     IOPA  , IOFUNC, IOSFUN, IOCONC, IOLOC ,
     +            IODEF , IP
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "actloc", ithandl )
C
C     If no locals get out of here
C
      IF ( NOLOC .EQ. 0 ) RETURN
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
      IA_LOC = 33
      IX_HLP = 1
      IA_HLP = 33
      CALL DHGVAR( IA_HLP, IX_HLP, IV_HLP)
      IK_HLP = ARRKND(IA_HLP)
      IP_HLP = ARRPOI(IA_HLP)
      ID1HLP = ARRDM1(IA_HLP)
      ID2HLP = ARRDM2(IA_HLP)
C
      DO I = 1 , NRVAR
         IP = IOPOIN(I)
C
C        Is it a local value
C
         IF ( IP .LT. IODEF .AND. IP .GE. IOLOC ) THEN
C
C           Get variable number
C
            ILOC = IP-IOLOC+1
            CALL DHGVAR( IA_LOC, ILOC  , IVAR  )
C
C           Check is variable is active for base grid
C
            IF ( VGRSET(IVAR,1) .EQ. 0 ) THEN
C
               IARR   = IA_LOC
               IV_IDX = VARIDX(IVAR)
               IARKND = ARRKND(IARR)
               IP_ARR = ARRPOI(IARR)
               IDIM1  = ARRDM1(IARR)
               IDIM2  = ARRDM2(IARR)
C
C              Set variable
C
               DO IGRID = 2 , NOGRID
                  IF ( VGRSET(IVAR,IGRID) .EQ. 1 ) THEN
                     NOSEG2 = GRDNOS(IGRID)
C
C                    Determine characteristics of variable
C
                     CALL DHGPOI( IVAR  , IARR  ,
     +                            IARKND, IV_IDX,
     +                            IDIM1 , IDIM2 ,
     +                            IP_ARR, IGRID ,
     +                            ISYSI , NOTOTI,
     +                            IP_ARI)
                     CALL DHGPOI( IVAR  , IARR  ,
     +                            IARKND, IV_IDX,
     +                            IDIM1 , IDIM2 ,
     +                            IP_ARR, 1     ,
     +                            ISYSO , NOTOTO,
     +                            IP_ARO)
C
C                    Determine characteristics of WEIGHT variable
C                    ( Don't mind if this one is actuel ? )
C
                     IDATYP = VARTDA(IVAR)
                     IF ( IDATYP .EQ. 2 ) THEN
                        IV_DA  = VARDAG(IVAR)
                        IA_DA  = VARARR(IV_DA)
                        IK_DA  = ARRKND(IA_DA)
                        IF ( IK_DA .EQ. 1 ) THEN
C
C                          Not variable in space use help var
C
                           IDATYP = 3
                           IV_DA  = IV_HLP
                           IA_DA  = VARARR(IV_DA)
                           IK_DA  = ARRKND(IA_DA)
                        ENDIF
                        IX_DA  = VARIDX(IV_DA)
                        IP_DA  = ARRPOI(IA_DA)
                        ID1_DA = ARRDM1(IA_DA)
                        ID2_DA = ARRDM2(IA_DA)
                        CALL DHGPOI( IV_DA , IA_DA ,
     +                               IK_DA , IX_DA ,
     +                               ID1_DA, ID2_DA,
     +                               IP_DA , 1     ,
     +                               ISYSW , NOTOTW,
     +                               IP_ARW)
                        CALL DHGPOI( IV_HLP, IA_HLP,
     +                               IK_HLP, IX_HLP,
     +                               ID1HLP, ID2HLP,
     +                               IP_HLP, IGRID ,
     +                               ISYSH , NOTOTH,
     +                               IP_ARH)
                     ELSEIF ( IDATYP .EQ. 3 ) THEN
                        IV_DA  = IV_HLP
                        IA_DA  = VARARR(IV_DA)
                        IK_DA  = ARRKND(IA_DA)
                        IX_DA  = VARIDX(IV_DA)
                        IP_DA  = ARRPOI(IA_DA)
                        ID1_DA = ARRDM1(IA_DA)
                        ID2_DA = ARRDM2(IA_DA)
                        CALL DHGPOI( IV_DA , IA_DA ,
     +                               IK_DA , IX_DA ,
     +                               ID1_DA, ID2_DA,
     +                               IP_DA , 1     ,
     +                               ISYSW , NOTOTW,
     +                               IP_ARW)
                        CALL DHGPOI( IV_HLP, IA_HLP,
     +                               IK_HLP, IX_HLP,
     +                               ID1HLP, ID2HLP,
     +                               IP_HLP, IGRID ,
     +                               ISYSH , NOTOTH,
     +                               IP_ARH)
                     ELSE
C
C                       Weight and help array's dummy's
C                       so set to the variable itself
C
                        ISYSW  = ISYSO
                        ISYSH  = ISYSI
                        NOTOTW = NOTOTO
                        NOTOTH = NOTOTI
                        IP_ARW = IP_ARO
                        IP_ARH = IP_ARI
C
                     ENDIF
C
                     ISWCUM = 0
                     CALL DHDAGG( NOSEG          , NOSEG2   ,
     +                            NOTOTI         , NOTOTW   ,
     +                            NOTOTH         , NOTOTO   ,
     +                            ISYSI          , ISYSW    ,
     +                            ISYSH          , ISYSO    ,
     +                            GRDSEG(1,IGRID), IDATYP   ,
     +                            A(IP_ARI)      , A(IP_ARW),
     +                            ISWCUM         , A(IP_ARH),
     +                            A(IP_ARO))
                     VGRSET(IVAR,1) = 1
                  ENDIF
               ENDDO
C
            ENDIF
C
         ENDIF
C
      ENDDO
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
