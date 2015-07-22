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

      SUBROUTINE WAPDEF ( LUPDEF, LUREP , NBPR  , NBPRM , BPRNAM,
     +                    BPRTXT, MODNAM, NSVAI , VAINAM, VAITXT,
     +                    VAIDEF, NSVAO , VAONAM, VAOTXT, NBFL  ,
     +                    BFLNAM, BFLTXT, NBST  , GENBST, FLXBST,
     +                    STOBST, IPVAI , IPVAO , IPBFL , IPBST ,
     +                    NSVXI , IPVXI , VXINAM, VXITXT, VXIDEF,
     +                    NSVXO , IPVXO , VXONAM, VXOTXT, NDST  ,
     +                    IPDST , GENDST, OUTDST, STODST, NVST  ,
     +                    IPVST , GENVST, OUTVST, STOVST, ISWITR)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: nov -1992 by Jan van Beek
C
C     FUNCTION            : Writes the ascii proces definition file
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
C     NBPR    INTEGER       1     INPUT   Number of processes in def file
C     NBPRM   INTEGER       1     INPUT   Max number of processes
C     BPRNAM  CHARACTER*(*) *     INPUT   Name of processes
C     BPRTXT  CHARACTER*(*) *     INPUT   Text of processes
C     MODNAM  CHARACTER*(*) *     INPUT   Name of module of processes
C     NSVAI   INTEGER       *     INPUT   No of input vars per proces
C     VAINAM  CHARACTER*(*) *     INPUT   Name of input variable
C     VAITXT  CHARACTER*(*) *     INPUT   Text of input variable
C     VAIDEF  REAL          *,*   INPUT   Default values input variables
C     NSVAO   INTEGER       *     INPUT   No of output vars per proces
C     VAONAM  CHARACTER*(*) *     INPUT   Name of input variable
C     VAOTXT  CHARACTER*(*) *     INPUT   Text of output variable
C     NBFL    INTEGER       *     INPUT   No of basic fluxes per proces
C     BFLNAM  CHARACTER*(*) *     INPUT   Name of basix fluxe
C     BFLTXT  CHARACTER*(*) *     INPUT   Text of basix fluxe
C     NBST    INTEGER       *     INPUT   No of basic stochis per proces
C     GENBST  CHARACTER*(*) *,*   INPUT   Name of substance in stochi
C     FLXBST  CHARACTER*(*) *,*   INPUT   Name of flux in stochi
C     STOBST  REAL          *,*   INPUT   Stochimetric factor
C     IPVAI   INTEGER       *     INPUT   Pointers for arrays on VAI
C     IPVAO   INTEGER       *     INPUT   Pointers for arrays on VAO
C     IPBFL   INTEGER       *     INPUT   Pointers for arrays on BFL
C     IPBST   INTEGER       *     INPUT   Pointers for arrays on BST
C     NSVXI   INTEGER       *     INPUT   No of input vars X per proces
C     IPVXI   INTEGER       *     INPUT   Pointers for arrays on VXI
C     VXINAM  CHARACTER*(*) *     INPUT   Name of input variable X
C     VXITXT  CHARACTER*(*) *     INPUT   Text of input variable X
C     VXIDEF  REAL          *     INPUT   Default values input X variables
C     NSVXO   INTEGER       *     INPUT   No of output vars X per proces
C     IPVXO   INTEGER       *     INPUT   Pointers for arrays on VXO
C     VXONAM  CHARACTER*(*) *     INPUT   Name of output variable X
C     VXOTXT  CHARACTER*(*) *     INPUT   Text of output variable X
C     NDST    INTEGER       *     INPUT   No of dispersion rules p.proces
C     IPDST   INTEGER       *     INPUT   Pointers for arrays on DST
C     GENDST  CHARACTER*(*) *     INPUT   Name of substance in disp rule
C     OUTDST  CHARACTER*(*) *     INPUT   Name of output item in disp rule
C     STOVST  REAL          *     INPUT   factor in dispersion rule
C     NVST    INTEGER       *     INPUT   No of velocity rules p.proces
C     IPVST   INTEGER       *     INPUT   Pointers for arrays on VST
C     GENVST  CHARACTER*(*) *     INPUT   Name of substance in velo rule
C     OUTVST  CHARACTER*(*) *     INPUT   Name of output item in velo rule
C     STOVST  REAL          *     INPUT   factor in velocity rule
C     ISWITR  INTEGER       *     INPUT   Target dimension indicator
C
C     Declaration of arguments
C
      INTEGER        LUPDEF          , LUREP           ,
     +               NBPR            , NBPRM
      INTEGER        NSVAI(*)        , NSVAO(*)        ,
     +               NBFL(*)         , NBST(*)         ,
     +               IPVAI(*)        , IPVAO(*)        ,
     +               IPBFL(*)        , IPBST(*)        ,
     +               NSVXI(*)        , IPVXI(*)        ,
     +               NSVXO(*)        , IPVXO(*)        ,
     +               NDST(*)         , IPDST(*)        ,
     +               NVST(*)         , IPVST(*)        ,
     +               ISWITR(*)
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
C
C     Write NBPR  number of proces modules
C
      WRITE ( LUPDEF , '(I3)' ) NBPR
C
C     Write name , input vars, output vars and fluxes
C
      DO 700 IP = 1 , NBPR
C
C        Write proces name , module name
C
         WRITE ( LUPDEF , '(A10,20X,A50)' )
     +         BPRNAM(IP),BPRTXT(IP)
         WRITE ( LUPDEF , '(A)' ) MODNAM(IP)
C
C        Write the xD target dimension indicator
C
         WRITE ( LUPDEF , '(I10)' ) ISWITR(IP)
C
C        Write input variables VAI with defaults
C
         NSV = NSVAI(IP)
         WRITE ( LUPDEF , '(I3)' ) NSV
         DO 100 IV = 1 , NSV
            IPV = IPVAI(IP) + IV - 1
            WRITE ( LUPDEF, '(A10,E20.8,A50)' )
     +           VAINAM(IPV), VAIDEF(IPV),VAITXT(IPV)
  100    CONTINUE
C
C        Write input variables VXI with defaults
C
         NSV = NSVXI(IP)
         WRITE ( LUPDEF , '(I3)' ) NSV
         DO 150 IV = 1 , NSV
            IPV = IPVXI(IP) + IV - 1
            WRITE ( LUPDEF, '(A10,E20.8,A50)' )
     +           VXINAM(IPV), VXIDEF(IPV),VXITXT(IPV)
  150    CONTINUE
C
C        Write output variables VAO
C
         NSV = NSVAO(IP)
         WRITE ( LUPDEF , '(I3)' ) NSV
         DO 200 IVAO = 1 , NSV
            IPV = IPVAO(IP) + IVAO - 1
            WRITE ( LUPDEF , '(A10,20X,A50)'  )
     +           VAONAM(IPV),VAOTXT(IPV)
  200    CONTINUE
C
C        Write output variables VXO
C
         NSV = NSVXO(IP)
         WRITE ( LUPDEF , '(I3)' ) NSV
         DO 250 IVAO = 1 , NSV
            IPV = IPVXO(IP) + IVAO - 1
            WRITE ( LUPDEF , '(A10,20X,A50)'  )
     +           VXONAM(IPV),VXOTXT(IPV)
  250    CONTINUE
C
C        Write basis fluxes  BFL
C
         NFL = NBFL(IP)
         WRITE ( LUPDEF , '(I3)' ) NFL
         DO 300 IFLX = 1 , NFL
            IPV = IPBFL(IP) + IFLX - 1
            WRITE ( LUPDEF , '(A10,20X,A50)'  )
     +             BFLNAM(IPV),BFLTXT(IPV)
  300    CONTINUE
C
C        Write basis stochiometry  BST
C
         NST = NBST(IP)
         WRITE ( LUPDEF , '(I3)' ) NST
         DO 400 IST = 1 , NST
            IPV = IPBST(IP) + IST - 1
            WRITE ( LUPDEF , '(A10,2X,A10,2X,E12.6)') GENBST(IPV),
     +                                                FLXBST(IPV),
     +                                                STOBST(IPV)
  400    CONTINUE
C
C        Write dispersion rules DST
C
         NST = NDST(IP)
         WRITE ( LUPDEF , '(I3)' ) NST
         DO 500 IST = 1 , NST
            IPV = IPDST(IP) + IST - 1
            WRITE ( LUPDEF , '(A10,2X,A10,2X,E12.6)') GENDST(IPV),
     +                                                OUTDST(IPV),
     +                                                STODST(IPV)
  500    CONTINUE
C
C        Write velocity rules VST
C
         NST = NVST(IP)
         WRITE ( LUPDEF , '(I3)' ) NST
         DO 600 IST = 1 , NST
            IPV = IPVST(IP) + IST - 1
            WRITE ( LUPDEF , '(A10,2X,A10,2X,E12.6)') GENVST(IPV),
     +                                                OUTVST(IPV),
     +                                                STOVST(IPV)
  600    CONTINUE
C
C        Write end line
C
         WRITE ( LUPDEF , '(A)' ) 'END'
  700 CONTINUE
C
      RETURN
      END
