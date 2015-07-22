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

      SUBROUTINE RBPDF2 ( LUPDEF, LUREP , NBPR  , NBPRM , BPRNAM,
     +                    BPRTXT, MODNAM, NSVAI , VAINAM, VAITXT,
     +                    VAIDEF, NSVAO , VAONAM, VAOTXT, NBFL  ,
     +                    BFLNAM, BFLTXT, NBST  , GENBST, FLXBST,
     +                    STOBST, VERSIO, SERIAL, IPVAI , IPVAO ,
     +                    IPBFL , IPBST , MAXVAI, MAXVAO, MAXBFL,
     +                    MAXBST, MAXVXI, NSVXI , IPVXI , VXINAM,
     +                    VXITXT, VXIDEF, MAXVXO, NSVXO , IPVXO ,
     +                    VXONAM, VXOTXT, MAXDST, NDST  , IPDST ,
     +                    GENDST, OUTDST, STODST, MAXVST, NVST  ,
     +                    IPVST , GENVST, OUTVST, STOVST, ISWITR,
     +                    NSVICH, NSVOCH, NFLCH , NSTCH )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: nov -1992 by Jan van Beek
C
C     FUNCTION            : Reads the binary proces definition file
C
C     LOGICAL UNITNUMBERS : LUPDEF  - proces definition file
C                         : LUREP   - report file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUPDEF  INTEGER       1     INPUT   Porces definition file
C     LUREP   INTEGER       1     INPUT   Report file
C     NBPR    INTEGER       1     OUTPUT  Number of processes in def file
C     NBPRM   INTEGER       1     INPUT   Max number of processes
C     BPRNAM  CHARACTER*(*) *     OUTPUT  Name of processes
C     BPRTXT  CHARACTER*(*) *     OUTPUT  Text of processes
C     MODNAM  CHARACTER*(*) *     OUTPUT  Name of module of processes
C     NSVAI   INTEGER       *     OUTPUT  No of input vars per proces
C     VAINAM  CHARACTER*(*) *     OUTPUT  Name of input variable
C     VAITXT  CHARACTER*(*) *     OUTPUT  Text of input variable
C     VAIDEF  REAL          *,*   OUTPUT  Default values input variables
C     NSVAO   INTEGER       *     OUTPUT  No of output vars per proces
C     VAONAM  CHARACTER*(*) *     OUTPUT  Name of input variable
C     VAOTXT  CHARACTER*(*) *     OUTPUT  Text of output variable
C     NBFL    INTEGER       *     OUTPUT  No of basic fluxes per proces
C     BFLNAM  CHARACTER*(*) *     OUTPUT  Name of basix fluxe
C     BFLTXT  CHARACTER*(*) *     OUTPUT  Text of basix fluxe
C     NBST    INTEGER       *     OUTPUT  No of basic stochis per proces
C     GENBST  CHARACTER*(*) *,*   OUTPUT  Name of substance in stochi
C     FLXBST  CHARACTER*(*) *,*   OUTPUT  Name of flux in stochi
C     STOBST  REAL          *,*   OUTPUT  Stochimetric factor
C     IPVAI   INTEGER       *     OUTPUT  Pointers for arrays on VAI
C     IPVAO   INTEGER       *     OUTPUT  Pointers for arrays on VAO
C     IPBFL   INTEGER       *     OUTPUT  Pointers for arrays on BFL
C     IPBST   INTEGER       *     OUTPUT  Pointers for arrays on BST
C     MAXVAI  INTEGER       1     INPUT   Maximum number of input vars
C     MAXVAO  INTEGER       1     INPUT   Maximum number of output vars
C     MAXBFL  INTEGER       1     INPUT   Maximum number of fluxes
C     MAXBST  INTEGER       1     INPUT   Maximum number of stochio's
C     MAXVXI  INTEGER       1     INPUT   Maximum number of input x vars
C     NSVXI   INTEGER       *     OUTPUT  No of input vars X per proces
C     IPVXI   INTEGER       *     OUTPUT  Pointers for arrays on VXI
C     VXINAM  CHARACTER*(*) *     OUTPUT  Name of input variable X
C     VXITXT  CHARACTER*(*) *     OUTPUT  Text of input variable X
C     VXIDEF  REAL          *     OUTPUT  Default values input X variables
C     MAXVXO  INTEGER       1     INPUT   Maximum number of output x vars
C     NSVXO   INTEGER       *     OUTPUT  No of output vars X per proces
C     IPVXO   INTEGER       *     OUTPUT  Pointers for arrays on VXO
C     VXONAM  CHARACTER*(*) *     OUTPUT  Name of output variable X
C     VXOTXT  CHARACTER*(*) *     OUTPUT  Text of output variable X
C     MAXDST  INTEGER       1     INPUT   Max. number of dispersion rules
C     NSDST   INTEGER       *     OUTPUT  No of dispersion rules p.proces
C     IPDST   INTEGER       *     OUTPUT  Pointers for arrays on DST
C     GENDST  CHARACTER*(*) *     OUTPUT  Name of substance in disp rule
C     OUTDST  CHARACTER*(*) *     OUTPUT  Name of output item in disp rule
C     STOVST  REAL          *     OUTPUT  factor in dispersion rule
C     MAXVST  INTEGER       1     INPUT   Max. number of velocity rules
C     NSVST   INTEGER       *     OUTPUT  No of velocity rules p.proces
C     IPVST   INTEGER       *     OUTPUT  Pointers for arrays on VST
C     GENVST  CHARACTER*(*) *     OUTPUT  Name of substance in velo rule
C     OUTVST  CHARACTER*(*) *     OUTPUT  Name of output item in velo rule
C     STOVST  REAL          *     OUTPUT  factor in velocity rule
C     ISWITR  INTEGER       *     OUTPUT  Target dimension indicator
C     NSVICH  INTEGER       1     INPUT   No of extra input variables for charon
C     NSVOCH  INTEGER       1     INPUT   No of extra output variables for charon
C     NFLCH   INTEGER       1     INPUT   No of fluxes for charon
C     NSTCH   INTEGER       1     INPUT   No of stochiometric terms for charon
C
C     Declaration of arguments
C

      use timers       !   performance timers

      INTEGER        LUPDEF          , LUREP           ,
     +               NBPR            , NBPRM           ,
     +               SERIAL          , MAXVAI          ,
     +               MAXVAO          , MAXBFL          ,
     +               MAXBST          , NSVICH          ,
     +               NSVOCH          , NFLCH           ,
     +               NSTCH
      INTEGER        NSVAI(*)        , NSVAO(*)        ,
     +               NBFL(*)         , NBST(*)         ,
     +               IPVAI(*)        , IPVAO(*)        ,
     +               IPBFL(*)        , IPBST(*)        ,
     +               NSVXI(*)        , IPVXI(*)        ,
     +               NSVXO(*)        , IPVXO(*)        ,
     +               NDST(*)         , IPDST(*)        ,
     +               NVST(*)         , IPVST(*)        ,
     +               ISWITR(*)
      REAL           VERSIO
      REAL           VAIDEF(*)       , STOBST(*)       ,
     +               VXIDEF(*)       , STODST(*)       ,
     +               STOVST(*)
      CHARACTER*(*)  BPRNAM(*)       , MODNAM(*)       ,
     +               VAINAM(*)       , VAONAM(*)       ,
     +               BFLNAM(*)       , GENBST(*)       ,
     +               FLXBST(*)       , VXINAM(*)       ,
     +               VXONAM(*)       , GENDST(*)       ,
     +               OUTDST(*)       , GENVST(*)       ,
     +               OUTVST(*)
      CHARACTER*(*)  BPRTXT(*)       , VAITXT(*)       ,
     +               VAOTXT(*)       , BFLTXT(*)       ,
     +               VXITXT(*)       , VXOTXT(*)
C
C     Local
C
      INTEGER        NSV   , NFL   , NST
      LOGICAL        CHARON
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "rdpdf2", ithndl )
C
C     Read version number and serial number
C
      READ ( LUPDEF ) VERSIO, SERIAL
C
C     Read NBPR  number of proces modules
C
      READ ( LUPDEF ) NBPR
      IF ( NBPR  .LE. 0 .OR. NBPR  .GT. NBPRM  ) THEN
         WRITE ( LUREP,* ) ' ERROR : Reading proces definition file'
         WRITE ( LUREP,* ) '         wrong number of processes',NBPR
         CALL SRSTOP(1)
      ENDIF
C
C     Read name , input vars, output vars and fluxes
C
      IIVAI = 1
      IIVXI = 1
      IIVAO = 1
      IIVXO = 1
      IIBFL = 1
      IIBST = 1
      IIDST = 1
      IIVST = 1
C
      DO 700 IP = 1 , NBPR
C
C        Read proces naam , module name
C
         READ ( LUPDEF ) BPRNAM(IP),BPRTXT(IP)
         READ ( LUPDEF ) MODNAM(IP)
         IF ( MODNAM(IP) .EQ. 'D40CHA' ) THEN
            CHARON = .TRUE.
         ELSE
            CHARON = .FALSE.
         ENDIF
C
C        Read the xD target dimension indicator
C
         READ ( LUPDEF ) ISWITR(IP)
C
C        Read input variables VAI with defaults
C
         READ ( LUPDEF ) NSV
         IF ( CHARON ) THEN
            NSVAI(IP) = NSV + NSVICH
            IPVAI(IP) = IIVAI
            IIVAI     = IIVAI + NSV + NSVICH
         ELSE
            NSVAI(IP) = NSV
            IPVAI(IP) = IIVAI
            IIVAI     = IIVAI + NSV
         ENDIF
         IF ( IIVAI .GT. MAXVAI ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of input variables'
            WRITE (LUREP,*) '         exceed maximum:,',MAXVAI
            CALL SRSTOP(1)
         ENDIF
         DO 100 IV = 1 , NSV
            IPV = IPVAI(IP) + IV - 1
            READ ( LUPDEF )
     +           VAINAM(IPV), VAIDEF(IPV),VAITXT(IPV)
  100    CONTINUE
C
C        Read input variables VXI with defaults
C
         READ ( LUPDEF ) NSV
         NSVXI(IP) = NSV
         IPVXI(IP) = IIVXI
         IIVXI     = IIVXI + NSV
         IF ( IIVXI .GT. MAXVXI ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of input X variables'
            WRITE (LUREP,*) '         exceed maximum:,',MAXVXI
            CALL SRSTOP(1)
         ENDIF
         DO 150 IV = 1 , NSV
            IPV = IPVXI(IP) + IV - 1
            READ ( LUPDEF )
     +           VXINAM(IPV), VXIDEF(IPV),VXITXT(IPV)
  150    CONTINUE
C
C        Read output variables VAO
C
         READ ( LUPDEF ) NSV
         IF ( CHARON ) THEN
            NSVAO(IP) = NSV + NSVOCH
            IPVAO(IP) = IIVAO
            IIVAO     = IIVAO + NSV + NSVOCH
         ELSE
            NSVAO(IP) = NSV
            IPVAO(IP) = IIVAO
            IIVAO     = IIVAO + NSV
         ENDIF
         IF ( IIVAO .GT. MAXVAO ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of output variables'
            WRITE (LUREP,*) '         exceed maximum:,',MAXVAO
            CALL SRSTOP(1)
         ENDIF
         DO 200 IVAO = 1 , NSV
            IPV = IPVAO(IP) + IVAO - 1
            READ ( LUPDEF )
     +           VAONAM(IPV),VAOTXT(IPV)
  200    CONTINUE
C
C        Read output variables VXO
C
         READ ( LUPDEF ) NSV
         NSVXO(IP) = NSV
         IPVXO(IP) = IIVXO
         IIVXO     = IIVXO + NSV
         IF ( IIVXO .GT. MAXVXO ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of outputX variables'
            WRITE (LUREP,*) '         exceed maximum:,',MAXVXO
            CALL SRSTOP(1)
         ENDIF
         DO 250 IVAO = 1 , NSV
            IPV = IPVXO(IP) + IVAO - 1
            READ ( LUPDEF )
     +           VXONAM(IPV),VXOTXT(IPV)
  250    CONTINUE
C
C        Read basis fluxes  BFL
C
         READ ( LUPDEF ) NFL
         IF ( CHARON ) THEN
            NBFL(IP) = NFL + NFLCH
            IPBFL(IP) = IIBFL
            IIBFL     = IIBFL + NFL + NFLCH
         ELSE
            NBFL(IP) = NFL
            IPBFL(IP) = IIBFL
            IIBFL     = IIBFL + NFL
         ENDIF
         IF ( IIBFL .GT. MAXBFL ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of fluxes'
            WRITE (LUREP,*) '         exceed maximum:,',MAXBFL
            CALL SRSTOP(1)
         ENDIF
         DO 300 IFLX = 1 , NFL
            IPV = IPBFL(IP) + IFLX - 1
            READ ( LUPDEF )
     +             BFLNAM(IPV),BFLTXT(IPV)
  300    CONTINUE
C
C        Read basis stochiometry  BST
C
         READ ( LUPDEF ) NST
         IF ( CHARON ) THEN
            NBST(IP) = NST + NSTCH
            IPBST(IP) = IIBST
            IIBST     = IIBST + NST + NSTCH
         ELSE
            NBST(IP) = NST
            IPBST(IP) = IIBST
            IIBST     = IIBST + NST
         ENDIF
         IF ( IIBST .GT. MAXBST ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of stochio terms'
            WRITE (LUREP,*) '         exceed maximum:,',MAXBST
            CALL SRSTOP(1)
         ENDIF
         DO 400 IST = 1 , NST
            IPV = IPBST(IP) + IST - 1
            READ ( LUPDEF ) GENBST(IPV), FLXBST(IPV),
     +                      STOBST(IPV)
  400    CONTINUE
C
C        Read basis dispersion rules
C
         READ ( LUPDEF ) NST
         NDST(IP) = NST
         IPDST(IP) = IIDST
         IIDST     = IIDST + NST
         IF ( IIDST .GT. MAXDST ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of dispersion rules'
            WRITE (LUREP,*) '         exceed maximum:,',MAXDST
            CALL SRSTOP(1)
         ENDIF
         DO 500 IST = 1 , NST
            IPV = IPDST(IP) + IST - 1
            READ ( LUPDEF ) GENDST(IPV), OUTDST(IPV),
     +                      STODST(IPV)
  500    CONTINUE
C
C        Read basis velocity rules
C
         READ ( LUPDEF ) NST
         NVST(IP) = NST
         IPVST(IP) = IIVST
         IIVST     = IIVST + NST
         IF ( IIVST .GT. MAXVST ) THEN
            WRITE (LUREP,*) ' ERROR : Reading proces definition file'
            WRITE (LUREP,*) '         total number of velocity rules'
            WRITE (LUREP,*) '         exceed maximum:,',MAXVST
            CALL SRSTOP(1)
         ENDIF
         DO 600 IST = 1 , NST
            IPV = IPVST(IP) + IST - 1
            READ ( LUPDEF ) GENVST(IPV), OUTVST(IPV),
     +                      STOVST(IPV)
  600    CONTINUE
C
  700 CONTINUE
C
      if (timon) call timstop( ithndl )
      RETURN
      END
