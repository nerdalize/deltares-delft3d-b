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

!
!     COMMON  /  SYSJ   /   Pointers in integer array workspace
!
!     IAPOI   INTEGER  1    pointer to ARRPOI, Pointer in workarray/FMM reference pointer
!     IATYP   INTEGER  1    pointer to ARRTYP, array type see FMM
!     IABYT   INTEGER  1    pointer to ARRBYT, number off bytes per array element
!     IALEN   INTEGER  1    pointer to ARRLEN, Array length
!     IAKND   INTEGER  1    pointer to ARRKND, Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR)
!     IADM1   INTEGER  1    pointer to ARRDM1, Array dimension 1
!     IADM2   INTEGER  1    pointer to ARRDM2, Array dimension 2
!     IADM3   INTEGER  1    pointer to ARRDM3, Array dimension 3
!     IXPNT   INTEGER  1    pointer to IPOINT, exchange pointers
!     IDUMP   INTEGER  1    pointer to IIDUMP, monitoring segments
!     IBPNT   INTEGER  1    pointer to IBPNT , boundary concitions admini
!     IWAST   INTEGER  1    pointer to IWAST , waste load locations
!     IDPNW   INTEGER  1    pointer to IDPNEW, new dispersion pointer for subst
!     IDPNT   INTEGER  1    pointer to IDPNT , dispersion pointer for subst
!     IVPNW   INTEGER  1    pointer to IVPNEW, new velocity pointer for subst
!     IVPNT   INTEGER  1    pointer to IVPNT , velocity pointer for subst
!     INRHA   INTEGER  1    pointer to IHARM , harmonic time space
!     INRH2   INTEGER  1    pointer to NRHARM, nr of harmonic records
!     INRFT   INTEGER  1    pointer to NRFTOT, breakpoint record lengths
!     IBULK   INTEGER  1    pointer to IPOINT, pointers to functions
!     ILP     INTEGER  1    pointer to IP    , paging structure monitor file
!     IGRID   INTEGER  1    pointer to LGRID , grid layout
!     INSVA   INTEGER  1    pointer to NSVAR , nr of state vars per proces
!     IIFLU   INTEGER  1    pointer to IFLUX , pointers in FLUX array
!     IIPMS   INTEGER  1    pointer to IPMSA , pointers from SSA to PMSA
!     IIPSS   INTEGER  1    pointer to IPSSA , pointers from PMSA to SSA
!     IIMOD   INTEGER  1    pointer to IMODU , module number per proces
!     IIOUT   INTEGER  1    pointer to IOUTPS, output structure
!     IIOPO   INTEGER  1    pointer to IOPOIN, pointer to delwaq array's
!     IKNMR   INTEGER  1    pointer to IKNMRK, kenmerk array
!     IKTIM   INTEGER  1    pointer to IKTIM , timers of time var kenmerk
!     IQDMP   INTEGER  1    pointer to IQDMP , pointer from exchange to DMPQ
!     ISDMP   INTEGER  1    pointer to ISDMP , pointer from segment to DMPS
!     IPDMP   INTEGER  1    pointer to IPDMP , pointer structure dump areas
!     IORAA   INTEGER  1    pointer to IORAAI, output option raai
!     NQRAA   INTEGER  1    pointer to NQRAAI, number of exchanges per raai
!     IQRAA   INTEGER  1    pointer to IQRAAI, index of the exchanges in raai
!     INISP   INTEGER  1    pointer to INWISP, new time series int space
!     INTYP   INTEGER  1    pointer to INTYPE, array with types of items
!     IWORK   INTEGER  1    pointer to IWORK , work array
!     JTRAC   INTEGER  1    pointer to ITRACE, lengte van matrix rows
!     IMAT    INTEGER  1    pointer to IMATRX, matrix van pointers fst.sol
!     IWRK    INTEGER  1    pointer to IWRK  , work array
!     ISYSN   INTEGER  1    pointer to ISYSN , Copy of the SYSN common block
!     ISYSI   INTEGER  1    pointer to ISYSI , Copy of the SYSI common block
!     IKFU    INTEGER  1    pointer to KFU   , mask array for u-velocity
!     IKFV    INTEGER  1    pointer to KFV   , mask array for v-velocity
!     IKCS    INTEGER  1    pointer to KCS   , mask array for water elevation
!     IKFS    INTEGER  1    pointer to KFS   , mask array for water elevation
!     ILGRA   INTEGER  1    pointer to LGRACT, active grid table
!     IKBND   INTEGER  1    pointer to IKBND , original cell number for boundaries
!     IPGRD   INTEGER  1    pointer to PROGRD, Grid number for this process
!     IPNDT   INTEGER  1    pointer to PRONDT, Number of timesteps in fractional step
!     IPVAR   INTEGER  1    pointer to PRVVAR, Variable number stacked list
!     IPTYP   INTEGER  1    pointer to PRVTYP, Variable type stacked list
!     IVARR   INTEGER  1    pointer to VARARR, Variable array number
!     IVIDX   INTEGER  1    pointer to VARIDX, Index variable in array
!     IVTDA   INTEGER  1    pointer to VARTDA, Dis-aggregation type for this variable
!     IVDAG   INTEGER  1    pointer to VARDAG, Dis-aggregation weight variable number
!     IVTAG   INTEGER  1    pointer to VARTDA, Aggregation type for this variable
!     IVAGG   INTEGER  1    pointer to VARDAG, Aggregation weight variable number
!     IVSET   INTEGER  1    pointer to VGRSET, Indication if variable is actual set for this grid
!     IGNOS   INTEGER  1    pointer to GRDNOS, Number of segments for grid
!     IGREF   INTEGER  1    pointer to GRDREF, Reference number for grid
!     IGSEG   INTEGER  1    pointer to GRDSEG, pointers to base grid
!     IOWNS   INTEGER  1    pointer to OWNERS, partitioning of segments for parallel computing
!     IOWNQ   INTEGER  1    pointer to OWNERQ, partitioning of exchanges for parallel computing
!     ipror   integer  1    pointer to proref, references to processes for input per process
!     iprvpt  integer  1    pointer to prvpnt, cumulative startpointer of variables per process
!     iprdon  integer  1    pointer to prdone, help array to see whether processs is done
!     IDMPB   INTEGER  1    pointer to DMPBAL, if dump area is included in balance
!
      INTEGER         IJSIZE
      INTEGER         IAPOI  , IATYP  , IABYT  , IALEN  , IAKND  ,
     &                IADM1  , IADM2  , IADM3  , IXPNT  , IDUMP  ,
     &                IBPNT  , IWAST  , IDPNW  , IDPNT  , IVPNW  ,
     &                IVPNT  , INRHA  , INRH2  , INRFT  , IBULK  ,
     &                ILP    , IGRID  , INSVA  , IIFLU  , IIPMS  ,
     &                IIPSS  , IIMOD  , IIOUT  , IIOPO  , IKNMR  ,
     &                IKTIM  , IQDMP  , ISDMP  , IPDMP  , IORAA  ,
     &                NQRAA  , IQRAA  , INISP  , INTYP  , IWORK  ,
     &                JTRAC  , IMAT   , IWRK   , ISYSN  , ISYSI  ,
     &                IKFU   , IKFV   , IKCS   , IKFS   , ILGRA  ,
     &                IKBND  , IPGRD  , IPNDT  , IPVAR  , IPTYP  ,
     &                IVARR  , IVIDX  , IVTDA  , IVDAG  , IVTAG  ,
     &                IVAGG  , IVSET  , IGNOS  , IGREF  , IGSEG  ,
     &                IOWNS  , IOWNQ  , ipror  , iprvpt , iprdon ,
     &                IDMPB
      COMMON / SYSJ / IAPOI  , IATYP  , IABYT  , IALEN  , IAKND  ,
     &                IADM1  , IADM2  , IADM3  , IXPNT  , IDUMP  ,
     &                IBPNT  , IWAST  , IDPNW  , IDPNT  , IVPNW  ,
     &                IVPNT  , INRHA  , INRH2  , INRFT  , IBULK  ,
     &                ILP    , IGRID  , INSVA  , IIFLU  , IIPMS  ,
     &                IIPSS  , IIMOD  , IIOUT  , IIOPO  , IKNMR  ,
     &                IKTIM  , IQDMP  , ISDMP  , IPDMP  , IORAA  ,
     &                NQRAA  , IQRAA  , INISP  , INTYP  , IWORK  ,
     &                JTRAC  , IMAT   , IWRK   , ISYSN  , ISYSI  ,
     &                IKFU   , IKFV   , IKCS   , IKFS   , ILGRA  ,
     &                IKBND  , IPGRD  , IPNDT  , IPVAR  , IPTYP  ,
     &                IVARR  , IVIDX  , IVTDA  , IVDAG  , IVTAG  ,
     &                IVAGG  , IVSET  , IGNOS  , IGREF  , IGSEG  ,
     &                IOWNS  , IOWNQ  , ipror  , iprvpt , iprdon ,
     &                IDMPB
!
      PARAMETER ( IJSIZE  = 71 )
