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

      SUBROUTINE PROCES ( NOTOT , NOSEG , CONC  , VOLUME, ITIME ,
     +                    IDT   , DERIV , NDMPAR, NPROC , NOFLUX,
     +                    IPMSA , PRVNIO, PROMNR, IFLUX , INCREM,
     +                    FLUX  , FLXDMP, STOCHI, IBFLAG, IPBLOO,
     +                    IPCHAR, IOFFBL, IOFFCH, AMASS , NOSYS ,
     +                    ITFACT, AMASS2, IAFLAG, INTOPT, FLXINT,
     +                    IEXPNT, IKNMRK, NOQ1  , NOQ2  , NOQ3  ,
     +                    NOQ4  , NDSPN , IDPNEW, DISPNW, NODISP,
     +                    IDPNT , DISPER, NDSPX , DSPX  , DSTO  ,
     +                    NVELN , IVPNEW, VELONW, NOVELO, IVPNT ,
     +                    VELO  , NVELX , VELX  , VSTO  , DMPS  ,
     +                    ISDMP , IPDMP , NTDMPQ, DEFAUL, PRONDT,
     +                    PROGRD, PRVVAR, PRVTYP, VARARR, VARIDX,
     +                    VARTDA, VARDAG, VARTAG, VARAGG, ARRPOI,
     +                    ARRKND, ARRDM1, ARRDM2, VGRSET, GRDNOS,
     +                    GRDSEG, NOVAR , A     , NOGRID, NDMPS ,
     +                    PRONAM, INTSRT, OWNERS, OWNERQ, MYPART,
     +                    prvpnt, done  , nrref , proref, nodef ,
     +                    surfac, LUNREP)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : november 1992 by Jos van Gils and Jan van Beek
C
C     FUNCTION            : Control routine of PROCES system.
C                           Proces sub-sytem of DELWAQ waterquality
C                           modelling system.
C
C     SUBROUTINES CALLED  : PROCAL, Compute fluxes with call to a module
C                           PRODER, Make derivatives, store fluxes
C                           DHDAGG, De-aggrgation off a variable
C                           DLWQ14, set deriv array
C                           DLWQP0, set a step
C                           PROINT, integrate fluxes at dump segments
C                           PROVEL, calculate new velocities/dispersions
C                           DHAGGR, aggrgation off a variable
C                           GETMLU, get unit number monitor file
C                           SRSTOP, stops execution
C                           ZERO  , Zero's an array
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
C     CONC    REAL   NOTOT*NOSEG  INPUT   Model concentrations
C     VOLUME  REAL      NOSEG     INPUT   Segment volumes
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     IDT     INTEGER       1     INPUT   Time step system clock units
C     DERIV   REAL   NOTOT*NOSEG  OUTPUT  Model derivatives
C     NDMPAR  INTEGER       1     INPUT   Number of dump areas
C     NPROC   INTEGER       1     INPUT   Number of processes
C     NOFLUX  INTEGER       1     INPUT   Number of fluxes
C     IPMSA   INTEGER       *     INPUT   Direct pointer in DELWAQ arrays
C     PRVNIO  INTEGER       *     INPUT   Nr. of state variables per proces
C     PROMNR  INTEGER       *     INPUT   Proces module number per proces
C     IFLUX   INTEGER       *     INPUT   Offset in flux array per proces
C     INCREM  INTEGER       *     INPUT   Direct increment in DELWAQ arrays
C     FLUX    REAL          *     LOCAL   Proces fluxes
C     FLXDMP  REAL   NOFLX*NDMPS  LOCAL   Fluxes at dump segments
C     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
C     IBFLAG  INTEGER    1        INPUT   if 1 then mass balance output
C     IPBLOO  INTEGER    1        INPUT   Number of Bloom module (if >0)
C     IPCHAR  INTEGER    1        INPUT   Number of Charon module (if >0)
C     IOFFBL  INTEGER    1        INPUT   Offset in IPMSA for Bloom
C     IOFFCH  INTEGER    1        INPUT   Offset in IPMSA for Charon
C     AMASS   REAL   NOTOT*NOSEG  IN/OUT  mass array to be updated
C     NOSYS   INTEGER     1       INPUT   number of active substances
C     ITFACT  INTEGER     1       INPUT   scale factor
C     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
C     IAFLAG  INTEGER     1       INPUT   if 1 then accumulation
C     INTOPT  INTEGER     1       INPUT   Integration suboptions
C     FLXINT  REAL  NOFLUX*NDMPAR IN/OUT  Integrated fluxes at dump areas
C     IEXPNT
C     IKNMRK
C     NOQ1
c     NOQ2
C     NOQ3
c     NOQ4
C     NDSPN   INTEGER        1    INPUT   Nr. of new dispersion array's
C     IDPNEW  INTEGER    NOSYS    INPUT   Pointer to new disp array
C     DISPNW  REAL     NVELN,*    OUTPUT  New dispersion array
C     NODISP  INTEGER        1    INPUT   Nr. of original dispersion
C     IDPNT   INTEGER    NOSYS    INPUT   pointer to original dispersion
C     DISPER  REAL    NOVELO,*    INPUT   Original dispersions
C     DSPX    REAL     NVELX,*    LOCAL   Calculated dispersions
C     NDSPX   INTEGER        1    INPUT   Nr. of calculated dispersions
C     DSTO    REAL    NOSYS,NVELX INPUT   Factor for calc. dispersions
C     NVELN   INTEGER        1    INPUT   Nr. of new velocity array's
C     IVPNEW  INTEGER    NOSYS    INPUT   Pointer to new velo array
C     VELONW  REAL     NVELN,*    OUTPUT  New velocity array
C     NOVELO  INTEGER        1    INPUT   Nr. of original velocities
C     IVPNT   INTEGER    NOSYS    INPUT   pointer to original velo
C     VELO    REAL    NOVELO,*    INPUT   Original velocities
C     NVELX   INTEGER        1    INPUT   Nr. of calculated velocities
C     VELX    REAL     NVELX,*    LOCAL   Calculated velocities
C     VSTO    REAL    NOSYS,NVELX INPUT   Factor for velocitie
C     DMPS    REAL        *       IN/OUT  dumped segment fluxes
C     ISDMP   INTEGER  NOSEG      INPUT   pointer dumped segments
C     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
C     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
C     DEFAUL  REAL          *     IN/OUT  Default proces parameters
C     PRONDT
C     PROGRD
C     PRVVAR
C     PRVTYP
C     VARARR
C     VARIDX
C     VARTDA
C     VARDAG
C     VARTAG
C     VARAGG
C     ARRPOI
C     ARRKND
C     ARRDM1
C     ARRDM2
C     VGRSET
C     GRDNOS
C     GRDSEG
C     NOVAR
C     A
C     NOGRID
C     NDMPS   INTEGER       1     INPUT   Number of segments in dump area
C     PRONAM  CHA*(*)       *     INPUT   Name of called module
C     INTSRT  INTEGER       1     INPUT   Number of integration routine used
C     OWNERS  INTEGER   NOSEG     INPUT   Ownership array for segments
C     OWNERQ  INTEGER     NOQ     INPUT   Ownership array for exchanges
C     MYPART  INTEGER       1     INPUT   Number of current part/subdomain
C     NODEF   INTEGER       1     INPUT   Number of values in the deafult array
C     LUNREP  INTEGER       1     INPUT   Logical unit number of report-file
C
      use timers
!     use m_timers_waq
      use m_couplib
!$    include "omp_lib.h"
C
C     Declaration of arguments
C
      INTEGER NOTOT , NOSEG , ITIME , IDT   , NDMPAR,
     +        NPROC , NOFLUX, IBFLAG, NOSYS , ITFACT,
     +        IAFLAG, INTOPT, IPBLOO, IPCHAR, IOFFBL,
     +        IOFFCH, NOQ1  , NOQ2  , NOQ3  , NOQ4 ,
     +        NDSPN , NODISP, NDSPX , NVELN , NOVELO,
     +        NVELX , NTDMPQ, NOVAR , NOGRID, NDMPS,
     +        INTSRT, MYPART, NODEF , LUNREP
      INTEGER IPMSA(*)      , PRVNIO(*)     , PROMNR(*)     ,
     +        IFLUX(*)      , INCREM(*)     , IEXPNT(*)     ,
     +        IKNMRK(*)     , IDPNEW(*)     , IDPNT(*)      ,
     +        IVPNEW(*)     , IVPNT(*)      , ISDMP(*)      ,
     +        IPDMP(*)      , PRONDT(*)     , PROGRD(*)     ,
     +        PRVVAR(*)     , PRVTYP(*)     , VARARR(*)     ,
     +        VARIDX(*)     , VARTDA(*)     , VARDAG(*)     ,
     +        VARTAG(*)     , VARAGG(*)     , ARRPOI(*)     ,
     +        ARRKND(*)     , ARRDM1(*)     , ARRDM2(*)     ,
     +        VGRSET(NOVAR,*),GRDNOS(*)     , GRDSEG(NOSEG,*),
     +        OWNERS(*)     , OWNERQ(*)
      REAL    CONC(*)       , VOLUME(*)     , DERIV(*),
     +        FLUX(*)       , FLXDMP(*)     , STOCHI(*)     ,
     +        AMASS(*)      , AMASS2(*)     , FLXINT(*)     ,
     +        DISPNW(*)     , DISPER(*)     , DSPX(*)       ,
     +        DSTO(*)       , VELONW(*)     , VELO(*)       ,
     +        VELX(*)       , VSTO(*)       , DMPS(*)       ,
     +        DEFAUL(*)     , A(*)
      CHARACTER*20  PRONAM(*)
      integer(4), intent(in   ) :: prvpnt(nproc)       ! entry in process pointers OMP
      integer(4)                   done  (nproc)       ! flag whether a process has ran
      integer(4), intent(in   ) :: nrref               ! maximum nr of back references
      integer(4), intent(in   ) :: proref(nrref,nproc) ! the back references
      integer                   :: actually_done
      real(4)   , intent(in   ) :: surfac(noseg)       ! horizontal surface
C
C     Local declarations
C
C     ISTEP   INTEGER     1       LOCAL   counter for timesteps (-)
C
      integer                    :: idtpro    ! fractional step idt
      integer(4)                 :: ipp_idt    ! pointer in default array to process specific idt
      integer(4)                 :: ipp_delt   ! pointer in default array to process specific delt
      INTEGER ISTEP, NOQ, IERR
      integer, allocatable, save :: velndt(:) ! fractional step per velocity
      integer, allocatable, save :: dspndt(:) ! fractional step per dispersion
      integer                    :: open_shared_library
      integer                    :: perf_function
      integer, save              :: ifirst = 1
      integer, save              :: dll_opb     ! open proces library dll handle
      character(len=256)         :: shared_dll
      logical                    :: lfound
      integer                    :: idummy
      real                       :: rdummy
      integer                    :: ierror
      integer                    :: ierr2


C     LOGICAL PROFLG
      SAVE    ISTEP
      DATA    ISTEP  / 0 /
C     DATA    PROFLG / .TRUE. /
C
Cjvb  Store fractional step flag in common CFRACS
C
      COMMON /CFRACS/ IFRACS
      logical                 run              ! lp for OMP
      integer                 aproc            ! lp for OMP

      logical timon_old
      integer(4) ithandl /0/
      integer(4) ithand2 /0/
      if ( timon ) call timstrt ( "proces", ithandl )
Cjvb
C
C     If no processes, get out of here
C
      IF ( NPROC .EQ. 0 ) goto 9999

      ! open openpb dll

      if ( ifirst .eq. 1 ) then
         call getmlu(lunrep)
         call getcom ( '-openpb', 3, lfound, idummy, rdummy, shared_dll, ierr2)
         if ( lfound ) then
            if ( ierr2.eq. 0 ) then
               write(lunrep,*) ' -openpb command line argument found'
               write(lunrep,*) ' using dll : ',trim(shared_dll)
            else
               shared_dll = 'd3dwaq_openpb.dll'
               write(lunrep,*) ' WARNING : -openpb command line argument without filename'
               write(lunrep,*) ' using default dll : ',trim(shared_dll)
            endif
            l_stop =.true.
         else
            shared_dll = 'd3dwaq_openpb.dll'
            l_stop =.false.
            write(lunrep,*) ' using default dll : ',trim(shared_dll)
         endif
         ierror = open_shared_library(dll_opb, shared_dll)
         if ( ierror .ne. 0 .and. l_stop ) then
            write(*,*) 'ERROR : opening process library DLL'
            write(*,*) 'DLL   : ',trim(shared_dll)
            write(lunrep,*) 'ERROR : opening process library DLL'
            write(lunrep,*) 'DLL   : ',trim(shared_dll)
            call srstop(1)
         endif
         ifirst = 0
      endif
C
C     Count calls of this module
C
      ISTEP = ISTEP + 1
      done = 0                                           ! this zeros the whole array
C
C     Start timings
C
!     call timer_start(timer_proces)
C
C     allocate velndt, dspndt
C
      if ( .not. allocated(velndt) ) then
         allocate(velndt(nvelx))
         velndt = 1
      endif
      if ( .not. allocated(dspndt) ) then
         allocate(dspndt(ndspx))
         dspndt = 1
      endif

C
CJVB
C     TEMPORARY HERE ?
C
C     aggregate kenmerk array
C
Cgrd  only if there is a process on a higher grid
      MAXGRID= MAXVAL(PROGRD(1:NPROC))
      IASIZE = 78
      IIKNMR = IASIZE + 30
      IF ( MAXGRID .GT. 1 ) THEN
      NODIM2 = ARRDM2(IIKNMR)
         CALL DHAGKM ( NOSEG , NODIM2, NOGRID, IKNMRK, GRDNOS,
     +                 GRDSEG)
      ENDIF
C
C     get the general local work array, first index of LOCAL array
C
      IX_HLP = 1
      IA_HLP = 33
      CALL DHGVAR( IA_HLP, IX_HLP, IV_HLP)
      IK_HLP = ARRKND(IA_HLP)
      IP_HLP = ARRPOI(IA_HLP)
      ID1HLP = ARRDM1(IA_HLP)
      ID2HLP = ARRDM2(IA_HLP)
C
C     Fill some specific variables absolute in the real array
C
      DEFAUL(2) = FLOAT(ITIME)
      NOQ = NOQ1 + NOQ2 + NOQ3 + NOQ4
C
C     BLOOM fractional step (derivs assumed zero at entry)
C     Check presence of module
C
      IF ( IPBLOO .GT. 0 ) THEN
         IVAR   = PRVVAR(IOFFBL)
         IARR   = VARARR(IVAR)
         IV_IDX = VARIDX(IVAR)
         IP_ARR = ARRPOI(IARR)
         IPNDT  = IP_ARR + IV_IDX - 1
         NDTBLO = NINT( A(IPNDT) )
         PRONDT(IPBLOO) = NDTBLO
C
C        This timestep fractional step ?
C
         IF ( MOD(ISTEP-1,NDTBLO) .EQ. 0 ) THEN
C
C           Set CONC actual on bloom grid
C
!           call timer_start(timer_proces_grids)
            IGRBLO = PROGRD(IPBLOO)
            IF ( IGRBLO .GT. 1 ) THEN
               IX_CNC = 1
               IA_CNC = 6
               CALL DHGVAR( IA_CNC, IX_CNC, IV_CNC)
               NOSEG2 = GRDNOS(IGRBLO)
               IP_CNC = (IGRBLO-1)*NOTOT*NOSEG + 1
               CALL DHGPOI( IV_HLP, IA_HLP,
     +                      IK_HLP, IX_HLP,
     +                      ID1HLP, ID2HLP,
     +                      IP_HLP, IGRBLO,
     +                      ISYSH , NOTOTH,
     +                      IP_ARH)
C
C              actives
C
               CALL DHAGG2( NOSEG          , NOSEG2          ,
     +                      NOTOT          , 1               ,
     +                      NOTOTH         , NOTOT           ,
     +                      1              , 1               ,
     +                      ISYSH          , 1               ,
     +                      NOSYS          , GRDSEG(1,IGRBLO),
     +                      3              , CONC            ,
     +                      VOLUME         , A(IP_ARH)       ,
     +                      CONC(IP_CNC)   )
C
C              inactives
C
               IF ( NOTOT - NOSYS .GT. 0 ) THEN
                  CALL DHAGG2( NOSEG          , NOSEG2          ,
     +                         NOTOT          , 1               ,
     +                         NOTOTH         , NOTOT           ,
     +                         NOSYS + 1      , 1               ,
     +                         ISYSH          , NOSYS + 1       ,
     +                         NOTOT-NOSYS    , GRDSEG(1,IGRBLO),
     +                         3              , CONC            ,
     +                         surfac         , A(IP_ARH)       ,
     +                         CONC(IP_CNC)   )
               ENDIF
               DO ISYS = 1 , NOTOT
                  IVAR = IV_CNC + ISYS - 1
                  VGRSET(IVAR,IGRBLO) = 1
               ENDDO
            ENDIF
C
            CALL ZERO ( FLUX , NOFLUX*NOSEG*NOGRID )
            IF ( IBFLAG .GT. 0 ) THEN
               CALL ZERO ( FLXDMP, NOFLUX*NDMPS )
            ENDIF
!           set idt and delt, bloom itself will multiply with prondt
            IDTPRO    = PRONDT(IPBLOO)*IDT
            ipp_idt    = nodef - 2*nproc + ipbloo
            ipp_delt   = nodef -   nproc + ipbloo
            DEFAUL(ipp_idt)  = FLOAT(IDT)
            DEFAUL(ipp_delt) = FLOAT(IDT)/FLOAT(ITFACT)
!           call timer_stop(timer_proces_grids)
      if ( timon ) call timstrt ( "onepro", ithand2 )
            CALL ONEPRO ( IPBLOO, IOFFBL, IDT   , ITFACT, PROGRD,
     +                    GRDNOS, PRVNIO, PRVTYP, PRVVAR, VARARR,
     +                    VARIDX, ARRKND, ARRPOI, ARRDM1, ARRDM2,
     +                    VGRSET, NOGRID, VARTDA, VARDAG, NOSEG ,
     +                    GRDSEG, A     , VARAGG, IPMSA , INCREM,
     +                    NOFLUX, IFLUX , PROMNR, FLUX  , IEXPNT,
     +                    IKNMRK, NOQ1  , NOQ2  , NOQ3  , NOQ4  ,
     +                    NPROC , NOTOT , DERIV , STOCHI, VOLUME,
     +                    PRONDT, IBFLAG, ISDMP , FLXDMP, NOVAR ,
     +                    VARTAG, IIKNMR, PRONAM, OWNERS, MYPART,
     +                    dspndt, velndt, dll_opb)
            done( ipbloo ) = 1
      if ( timon ) call timstop ( ithand2 )
            IGRID = PROGRD(IPBLOO)
            NOSEG2 = GRDNOS(IGRID)
            IF ( IPBLOO .NE. NPROC ) THEN
               NFLUXP = IFLUX(IPBLOO+1) - IFLUX(IPBLOO)
            ELSE
               NFLUXP = NOFLUX - IFLUX(IPBLOO) + 1
            ENDIF
            IF ( NFLUXP .GT. 0 ) THEN
C
C        If necessary set volume for this grid
C        Volume is always variable 1
C
               IF ( VGRSET(1,IGRID) .NE. 1 ) THEN
                  IPVGR  = (IGRID-1)*NOSEG + 1
                  CALL DHAGGR( NOSEG          , NOSEG2       ,
     +                         1              , 1            ,
     +                         1              , 1            ,
     +                         1              , 1            ,
     +                         1              , 1            ,
     +                         GRDSEG(1,IGRID), 1            ,
     +                         VOLUME         , VOLUME       ,
     +                         VOLUME         , VOLUME(IPVGR))
                  VGRSET(1,IGRID) = 1
               ENDIF
C
C        Construct derivatives for these fluxes on this grid
C
               IPFLUX = (IGRID-1)*NOFLUX*NOSEG + 1
               IPVGR  = (IGRID-1)*NOSEG + 1
               IPDGR  = NOTOT*NOSEG*(IGRID-1) + 1
               CALL PRODR2 ( DERIV(IPDGR) , NOTOT        ,
     +                       NOFLUX       , STOCHI       ,
     +                       IFLUX(IPBLOO) , NFLUXP       ,
     +                       FLUX(IPFLUX) , NOSEG2       ,
     +                       VOLUME(IPVGR), PRONDT(IPBLOO),
     +                       OWNERS       , MYPART       )
C
C        For balances store FLXDMP
C
               IF ( IBFLAG .GT. 0 ) THEN
C
                  CALL PROFLD ( NOFLUX       , IFLUX(IPBLOO)  ,
     +                          NFLUXP       , IGRID         ,
     +                          NOSEG2       , NOSEG         ,
     +                          PRONDT(IPBLOO), ISDMP         ,
     +                          GRDSEG       , FLUX(IPFLUX)  ,
     +                          VOLUME       , FLXDMP        )
C
               ENDIF
            ENDIF
C
C        If processes on other grid convert derivs to base grid
C
            IGRBLO = PROGRD(IPBLOO)
            IF ( NOFLUX .GT. 0 .AND. IGRBLO .GT. 1 ) THEN
!              call timer_start(timer_proces_grids)
C
               ISWCUM = 1
               NOSEG2 = GRDNOS(IGRBLO)
               IPGR   = NOTOT*NOSEG*(IGRBLO-1) + 1
               CALL DHDAG2( NOSEG         , NOSEG2          ,
     +                      NOTOT         , NOTOT           ,
     +                      NOTOT         , NOTOT           ,
     +                      1             , 1               ,
     +                      1             , 1               ,
     +                      NOTOT         , GRDSEG(1,IGRBLO),
     +                      2             , DERIV(IPGR)     ,
     +                      AMASS         , ISWCUM          ,
     +                      AMASS(IPGR)   , DERIV           )
C
C              Zero derivs higher grids
C
               CALL ZERO ( DERIV(IPGR), NOTOT*NOSEG )
!              call timer_stop(timer_proces_grids)
            ENDIF
C
C           Scale fluxes and update "processes" accumulation arrays
C
!           call timer_start(timer_proces_fluxes)
            CALL DLWQ14 ( DERIV  , NOTOT  , NOSEG  , ITFACT , AMASS2 ,
     *                    IDT    , IAFLAG , DMPS   , INTOPT , ISDMP  ,
     *                    OWNERS , MYPART )
C
C           Integration (derivs are zeroed)
C
            CALL DLWQP0 ( CONC   , AMASS  , DERIV  , VOLUME , IDT     ,
     *                    NOSYS  , NOTOT  , NOSEG  , 0      , 0       ,
     *                    OWNERS , MYPART , surfac )
C
C           Integrate the fluxes at dump segments
C
            IF ( IBFLAG .GT. 0 ) THEN
               CALL PROINT ( NOFLUX, NDMPAR, IDT   , ITFACT, FLXDMP,
     +                       FLXINT, ISDMP , IPDMP , NTDMPQ)
               CALL ZERO ( FLXDMP, NOFLUX*NDMPS )
            ENDIF
C
C           Set CONC not actual for higer grids
C
            IX_CNC = 1
            IA_CNC = 6
            CALL DHGVAR( IA_CNC, IX_CNC, IV_CNC)
            DO IGRID = 2 , NOGRID
               DO ISYS = 1 , NOTOT
                  IVAR = IV_CNC + ISYS - 1
                  VGRSET(IVAR,IGRID) = 0
               ENDDO
            ENDDO
!           call timer_stop(timer_proces_fluxes)
C
         else
            done ( ipbloo ) = 1
         ENDIF
C
      ENDIF
C
C     Charon fractional step
C
      IF ( IPCHAR .GT. 0 ) THEN
         IVAR   = PRVVAR(IOFFCH)
         IARR   = VARARR(IVAR)
         IV_IDX = VARIDX(IVAR)
         IP_ARR = ARRPOI(IARR)
         IPNDT  = IP_ARR + IV_IDX - 1
         NDTCHA = NINT( A(IPNDT) )
         PRONDT(IPCHAR) = NDTCHA
C
C        This timestep fractional step ?
C
         IF ( MOD(ISTEP-1,NDTCHA) .EQ. 0 ) THEN
C
C           Set CONC actual on bloom grid
C
            IGRCHA = PROGRD(IPCHAR)
            IF ( IGRCHA .GT. 1 ) THEN
!              call timer_start(timer_proces_grids)
               IX_CNC = 1
               IA_CNC = 6
               CALL DHGVAR( IA_CNC, IX_CNC, IV_CNC)
               NOSEG2 = GRDNOS(IGRCHA)
               IP_CNC = (IGRCHA-1)*NOTOT*NOSEG + 1
               CALL DHGPOI( IV_HLP, IA_HLP,
     +                      IK_HLP, IX_HLP,
     +                      ID1HLP, ID2HLP,
     +                      IP_HLP, IGRCHA,
     +                      ISYSH , NOTOTH,
     +                      IP_ARH)
C
C              actives
C
               CALL DHAGG2( NOSEG          , NOSEG2          ,
     +                      NOTOT          , 1               ,
     +                      NOTOTH         , NOTOT           ,
     +                      1              , 1               ,
     +                      ISYSH          , 1               ,
     +                      NOSYS          , GRDSEG(1,IGRCHA),
     +                      3              , CONC            ,
     +                      VOLUME         , A(IP_ARH)       ,
     +                      CONC(IP_CNC)   )
C
C              inactives
C
               IF ( NOTOT - NOSYS .GT. 0 ) THEN
                  CALL DHAGG2( NOSEG          , NOSEG2          ,
     +                         NOTOT          , 1               ,
     +                         NOTOTH         , NOTOT           ,
     +                         NOSYS + 1      , 1               ,
     +                         ISYSH          , NOSYS + 1       ,
     +                         NOTOT-NOSYS    , GRDSEG(1,IGRCHA),
     +                         3              , CONC            ,
     +                         surfac         , A(IP_ARH)       ,
     +                         CONC(IP_CNC)   )
               ENDIF
               DO ISYS = 1 , NOTOT
                  IVAR = IV_CNC + ISYS - 1
                  VGRSET(IVAR,IGRCHA) = 1
               ENDDO
!              call timer_stop(timer_proces_grids)
            ENDIF
C
            CALL ZERO ( FLUX , NOFLUX*NOSEG*NOGRID )
            IF ( IBFLAG .GT. 0 ) THEN
               CALL ZERO ( FLXDMP, NOFLUX*NDMPS )
            ENDIF
!           set idt and delt
            ipp_idt    = nodef - 2*nproc + ipchar
            ipp_delt   = nodef -   nproc + ipchar
            IDTPRO    = PRONDT(IPCHAR)*IDT
            DEFAUL(ipp_idt)  = FLOAT(IDTPRO)
            DEFAUL(ipp_delt) = FLOAT(IDTPRO)/FLOAT(ITFACT)
      if ( timon ) call timstrt ( "onepro", ithand2 )
            CALL ONEPRO ( IPCHAR, IOFFCH, IDT   , ITFACT, PROGRD,
     +                    GRDNOS, PRVNIO, PRVTYP, PRVVAR, VARARR,
     +                    VARIDX, ARRKND, ARRPOI, ARRDM1, ARRDM2,
     +                    VGRSET, NOGRID, VARTDA, VARDAG, NOSEG ,
     +                    GRDSEG, A     , VARAGG, IPMSA , INCREM,
     +                    NOFLUX, IFLUX , PROMNR, FLUX  , IEXPNT,
     +                    IKNMRK, NOQ1  , NOQ2  , NOQ3  , NOQ4  ,
     +                    NPROC , NOTOT , DERIV , STOCHI, VOLUME,
     +                    PRONDT, IBFLAG, ISDMP , FLXDMP, NOVAR ,
     +                    VARTAG, IIKNMR, PRONAM, OWNERS, MYPART,
     +                    dspndt, velndt, dll_opb)
            done( ipchar ) = 1
      if ( timon ) call timstop ( ithand2 )
            IGRID = PROGRD(IPCHAR)
            NOSEG2 = GRDNOS(IGRID)
            IF ( IPCHAR .NE. NPROC ) THEN
               NFLUXP = IFLUX(IPCHAR+1) - IFLUX(IPCHAR)
            ELSE
               NFLUXP = NOFLUX - IFLUX(IPCHAR) + 1
            ENDIF
            IF ( NFLUXP .GT. 0 ) THEN
C
C        If necessary set volume for this grid
C        Volume is always variable 1
C
               IF ( VGRSET(1,IGRID) .NE. 1 ) THEN
                  IPVGR  = (IGRID-1)*NOSEG + 1
                  CALL DHAGGR( NOSEG          , NOSEG2       ,
     +                         1              , 1            ,
     +                         1              , 1            ,
     +                         1              , 1            ,
     +                         1              , 1            ,
     +                         GRDSEG(1,IGRID), 1            ,
     +                         VOLUME         , VOLUME       ,
     +                         VOLUME         , VOLUME(IPVGR))
                  VGRSET(1,IGRID) = 1
               ENDIF
C
C        Construct derivatives for these fluxes on this grid
C
               IPFLUX = (IGRID-1)*NOFLUX*NOSEG + 1
               IPVGR  = (IGRID-1)*NOSEG + 1
               IPDGR  = NOTOT*NOSEG*(IGRID-1) + 1
               CALL PRODR2 ( DERIV(IPDGR) , NOTOT        ,
     +                       NOFLUX       , STOCHI       ,
     +                       IFLUX(IPCHAR) , NFLUXP       ,
     +                       FLUX(IPFLUX) , NOSEG2       ,
     +                       VOLUME(IPVGR), PRONDT(IPCHAR),
     +                       OWNERS       , MYPART       )
C
C        For balances store FLXDMP
C
               IF ( IBFLAG .GT. 0 ) THEN
C
                  CALL PROFLD ( NOFLUX       , IFLUX(IPCHAR)  ,
     +                          NFLUXP       , IGRID         ,
     +                          NOSEG2       , NOSEG         ,
     +                          PRONDT(IPCHAR), ISDMP         ,
     +                          GRDSEG       , FLUX(IPFLUX)  ,
     +                          VOLUME       , FLXDMP        )
C
               ENDIF
            ENDIF
C
C           If processes on other grid convert derivs to base grid
C
            IGRCHA = PROGRD(IPCHAR)
            IF ( NOFLUX .GT. 0 .AND. IGRCHA .GT. 1 ) THEN
!              call timer_start(timer_proces_grids)
C
               ISWCUM = 1
               NOSEG2 = GRDNOS(IGRCHA)
               IPGR   = NOTOT*NOSEG*(IGRCHA-1) + 1
               CALL DHDAG2( NOSEG         , NOSEG2          ,
     +                      NOTOT         , NOTOT           ,
     +                      NOTOT         , NOTOT           ,
     +                      1             , 1               ,
     +                      1             , 1               ,
     +                      NOTOT         , GRDSEG(1,IGRCHA),
     +                      2             , DERIV(IPGR)     ,
     +                      AMASS         , ISWCUM          ,
     +                      AMASS(IPGR)   , DERIV           )
C
C              Zero derivs higher grids
C
               CALL ZERO ( DERIV(IPGR), NOTOT*NOSEG )
!              call timer_stop(timer_proces_grids)
            ENDIF
C
C           Scale fluxes and update "processes" accumulation arrays
C
!           call timer_start(timer_proces_fluxes)
            CALL DLWQ14 ( DERIV  , NOTOT  , NOSEG  , ITFACT , AMASS2 ,
     *                    IDT    , IAFLAG , DMPS   , INTOPT , ISDMP  ,
     *                    OWNERS , MYPART )
C
C           Integration (derivs are zeroed)
C
            CALL DLWQP0 ( CONC   , AMASS  , DERIV  , VOLUME , IDT    ,
     *                    NOSYS  , NOTOT  , NOSEG  , 0      , 0      ,
     *                    OWNERS , MYPART , surfac )
C
C           Integrate the fluxes at dump segments
C
            IF ( IBFLAG .GT. 0 ) THEN
               CALL PROINT ( NOFLUX, NDMPAR, IDT   , ITFACT, FLXDMP,
     +                       FLXINT, ISDMP , IPDMP , NTDMPQ)
               CALL ZERO ( FLXDMP, NOFLUX*NDMPS )
            ENDIF
C
C           Set CONC not actual for higer grids
C
            IX_CNC = 1
            IA_CNC = 6
            CALL DHGVAR( IA_CNC, IX_CNC, IV_CNC)
            DO IGRID = 2 , NOGRID
               DO ISYS = 1 , NOTOT
                  IVAR = IV_CNC + ISYS - 1
                  VGRSET(IVAR,IGRID) = 0
               ENDDO
            ENDDO
!           call timer_stop(timer_proces_fluxes)
C
         else
            done ( ipchar ) = 1
         ENDIF
C
      ENDIF
C
C     see if converting CONC in one step speeds up
C     only in case of no fractional step
C
      IF ( IFRACS .EQ. 0 .AND. MAXGRID .GT. 1 ) THEN
         IX_CNC = 1
         IA_CNC = 6
         CALL DHGVAR( IA_CNC, IX_CNC, IV_CNC)
!        call timer_start(timer_proces_grids)
         DO IGRID = 2 , NOGRID
            NOSEG2 = GRDNOS(IGRID)
            IP_CNC = (IGRID-1)*NOTOT*NOSEG + 1
            CALL DHGPOI( IV_HLP, IA_HLP,
     +                   IK_HLP, IX_HLP,
     +                   ID1HLP, ID2HLP,
     +                   IP_HLP, IGRID ,
     +                   ISYSH , NOTOTH,
     +                   IP_ARH)
C
C     actives
C
            CALL DHAGG2( NOSEG          , NOSEG2         ,
     +                   NOTOT          , 1              ,
     +                   NOTOTH         , NOTOT          ,
     +                   1              , 1              ,
     +                   ISYSH          , 1              ,
     +                   NOSYS          , GRDSEG(1,IGRID),
     +                   3              , CONC           ,
     +                   VOLUME         , A(IP_ARH)      ,
     +                   CONC(IP_CNC)   )
C
C     inactives
C
            IF ( NOTOT - NOSYS .GT. 0 ) THEN
               CALL DHAGG2( NOSEG          , NOSEG2         ,
     +                      NOTOT          , 1              ,
     +                      NOTOTH         , NOTOT          ,
     +                      NOSYS + 1      , 1              ,
     +                      ISYSH          , NOSYS + 1      ,
     +                      NOTOT-NOSYS    , GRDSEG(1,IGRID),
     +                      3              , CONC           ,
     +                      surfac         , A(IP_ARH)      ,
     +                      CONC(IP_CNC)   )
            ENDIF
            DO ISYS = 1 , NOTOT
               IVAR = IV_CNC + ISYS - 1
               VGRSET(IVAR,IGRID) = 1
            ENDDO
         ENDDO
!        call timer_stop(timer_proces_grids)
      ENDIF
C
CJVB
C
C     The processes fractional step
C
      CALL ZERO ( FLUX , NOFLUX*NOSEG*NOGRID )
      IF ( IBFLAG .GT. 0 ) THEN
         CALL ZERO ( FLXDMP, NOFLUX*NDMPS )
      ENDIF

      if ( timon ) call timstrt ( "onepro", ithand2 )

      timon_old = timon
      if ( OMP_GET_NUM_THREADS() > 1 ) then
          timon = .false.
      endif
!$OMP PARALLEL

!$OMP DO  PRIVATE(run,idtpro,k,nfluxp,ipp_idt,ipp_delt)  SCHEDULE(DYNAMIC)
      do iproc = 1,nproc

!        NOT bloom and charon

         if ( iproc .ne. ipbloo .and. iproc .ne. ipchar ) then

!           Check fractional step

            if ( mod( istep-1, prondt(iproc) ) .eq. 0 ) then
               run = .false.                             ! to get the loop running
               do while ( .not. run )                    ! wait untill all input is resolved
                  run = .true.                           ! we are optimistic
                  do k = 1, nrref                        ! maximum number of references / proc
                     if ( proref(k,iproc) .eq. 0 ) exit        ! no references left

                     !
                     ! Flush the array done:
                     ! The Intel Fortran compiler seems to optimise this
                     ! loop too aggressively under Linux, if we use the array done()
                     ! directly.
                     !

                     !$omp flush(done)

                     if ( done(proref(k,iproc)) .eq. 0 ) then  ! an unresolved one found
                        run = .false.                          ! so no run yet
                        exit
                     endif                                     ! everything is resolved
                  enddo                                        ! for this processs
               enddo

               ! set idt and delt for this process in the default array
               ipp_idt    = nodef - 2*nproc + iproc
               ipp_delt   = nodef -   nproc + iproc
               IDTPRO    = PRONDT(IPROC)*IDT
               DEFAUL(ipp_idt)  = FLOAT(IDTPRO)
               DEFAUL(ipp_delt) = FLOAT(IDTPRO)/FLOAT(ITFACT)

               call onepro ( iproc   , prvpnt(iproc), idt     , itfact  , progrd  ,
     &                       grdnos  , prvnio       , prvtyp  , prvvar  , vararr  ,
     &                       varidx  , arrknd       , arrpoi  , arrdm1  , arrdm2  ,
     &                       vgrset  , nogrid       , vartda  , vardag  , noseg   ,
     &                       grdseg  , a            , varagg  , ipmsa   , increm  ,
     &                       noflux  , iflux        , promnr  , flux    , iexpnt  ,
     &                       iknmrk  , noq1         , noq2    , noq3    , noq4    ,
     &                       nproc   , notot        , deriv   , stochi  , volume  ,
     &                       prondt  , ibflag       , isdmp   , flxdmp  , novar   ,
     &                       vartag  , iiknmr       , pronam  , owners  , mypart  ,
     &                       dspndt  , velndt       , dll_opb )

               done(iproc) = 1                           ! this process has resolved its output
               !$omp flush(done)

            endif
         endif
      enddo
!$OMP END DO
!$OMP END PARALLEL

      timon = timon_old
      if ( timon ) call timstop ( ithand2 )

!           Now update the derivatives and the dumps of the fluxes from
!              all processes together outside of the parallel region

      call twopro ( nproc  , nogrid , noflux , novar  , noseg  ,
     &              notot  , progrd , grdnos , iflux  , vgrset ,
     &              grdseg , volume , deriv  , stochi , flux   ,
     &              prondt , ibflag , isdmp  , flxdmp , owners ,
     &              mypart , ipbloo , ipchar , istep  )

C
C     Store fluxes and elaborate mass balances set fractional step
C     Vraag , doen we nu altijd fractional step? of moeten we als we geen
C     processen hebben met een grotere tijdstap de integratie samen met het
C     transport doen.
C
Cgrd  IF ( NOFLUX .GT. 0 .AND. NOGRID .GT. 1 ) THEN
      IF ( NOFLUX .GT. 0 .AND. MAXGRID .GT. 1 ) THEN
!        call timer_start(timer_proces_grids)
         DO IGRD = 2 , NOGRID
c           DO ISYS = 1 , NOTOT
c              ISWCUM = 1
c              NOSEG2 = GRDNOS(IGRD)
c              IPGR   = NOTOT*NOSEG*(IGRD-1) + 1
c              CALL DHDAGG( NOSEG         , NOSEG2     ,
c    +                      NOTOT         , NOTOT      ,
c    +                      NOTOT         , NOTOT      ,
c    +                      ISYS          , ISYS       ,
c    +                      ISYS          , ISYS       ,
c    +                      GRDSEG(1,IGRD), 2          ,
c    +                      DERIV(IPGR)   , AMASS      ,
c    +                      ISWCUM        , AMASS(IPGR),
c    +                      DERIV         )
c           ENDDO
            ISWCUM = 1
            NOSEG2 = GRDNOS(IGRD)
            IPGR   = NOTOT*NOSEG*(IGRD-1) + 1
            CALL DHDAG2( NOSEG         , NOSEG2        ,
     +                   NOTOT         , NOTOT         ,
     +                   NOTOT         , NOTOT         ,
     +                   1             , 1             ,
     +                   1             , 1             ,
     +                   NOTOT         , GRDSEG(1,IGRD),
     +                   2             , DERIV(IPGR)   ,
     +                   AMASS         , ISWCUM        ,
     +                   AMASS(IPGR)   , DERIV         )
         ENDDO
C
C        Zero derivs higher grids
C
         IPGR   = NOTOT*NOSEG + 1
         CALL ZERO ( DERIV(IPGR), NOTOT*NOSEG*(NOGRID-1) )
!        call timer_stop(timer_proces_grids)
      ENDIF
C
C     Set fractional step
C
      IF ( NOFLUX .GT. 0 .AND. IFRACS .EQ. 1 ) THEN

         ! no fluxes at first step of fractional step

         IF ( ISTEP .EQ. 1 ) THEN
            ! just zero the deriv
            CALL ZERO(DERIV,NOTOT*NOSEG)
            IF ( IBFLAG .GT. 0 ) THEN
               CALL ZERO ( FLXDMP, NOFLUX*NDMPS )
            ENDIF
         ELSE

C
C           Scale fluxes and update "processes" accumulation arrays
C
!           call timer_start(timer_proces_fluxes)
            CALL DLWQ14 ( DERIV  , NOTOT  , NOSEG  , ITFACT , AMASS2 ,
     *                    IDT    , IAFLAG , DMPS   , INTOPT , ISDMP  ,
     *                    OWNERS , MYPART )
C
C           Integration (derivs are zeroed)
C
            CALL DLWQP0 ( CONC   , AMASS  , DERIV  , VOLUME , IDT    ,
     *                    NOSYS  , NOTOT  , NOSEG  , 0      , 0      ,
     *                    OWNERS , MYPART , surfac )
C
C           Integrate the fluxes at dump segments
C
            IF ( IBFLAG .GT. 0 ) THEN
               CALL PROINT ( NOFLUX , NDMPAR , IDT    , ITFACT , FLXDMP ,
     +                       FLXINT , ISDMP  , IPDMP  , NTDMPQ )
               CALL ZERO ( FLXDMP, NOFLUX*NDMPS )
            ENDIF
!           call timer_stop(timer_proces_fluxes)
         ENDIF
      ENDIF
C
C     Calculate new dispersions
C
      IF ( NDSPN  .GT. 0 ) THEN
 !       call timer_start(timer_proces_disper)
         CALL PROVEL (DISPNW, NDSPN , IDPNEW, DISPER, NODISP,
     +                IDPNT , DSPX  , NDSPX , DSTO  , NOSYS ,
     +                NOQ   , OWNERQ, MYPART, DSPNDT, ISTEP )
  !      call timer_stop(timer_proces_disper)
      ENDIF
C
C     Calculate new velocities
C
      IF ( NVELN  .GT. 0 ) THEN
 !       call timer_start(timer_proces_disper)
         CALL PROVEL (VELONW, NVELN , IVPNEW, VELO  , NOVELO,
     +                IVPNT , VELX  , NVELX , VSTO  , NOSYS ,
     +                NOQ   , OWNERQ, MYPART, VELNDT, ISTEP )
 !       call timer_stop(timer_proces_disper)
      ENDIF
C
C     Update output-data between neighbouring processors
C
!     call timer_start(timer_proces_comm)
      call update_data(deriv, notot, 'noseg', 1, 'stc1', ierr)
      call update_data(amass, notot, 'noseg', 1, 'stc1', ierr)
      if (ndspn.gt.0)
     +   call update_data(dispnw, ndspn,'noq',1, 'exchg_for_ownseg', ierr)
      if (nveln.gt.0)
     +   call update_data(velonw, nveln,'noq',1, 'exchg_for_ownseg', ierr)
!     call timer_stop(timer_proces_comm)
C
C     Stop timing for cpu/wall-clock time for process library
C
!     call timer_stop(timer_proces)

 9999 if ( timon ) call timstop ( ithandl )
      RETURN
 2000 FORMAT ( ' ERROR: undefined kind of array in PROCES :',I8 )
      END
C
      SUBROUTINE ONEPRO ( IPROC , K     , IDT   , ITFACT, PROGRD,
     +                    GRDNOS, PRVNIO, PRVTYP, PRVVAR, VARARR,
     +                    VARIDX, ARRKND, ARRPOI, ARRDM1, ARRDM2,
     +                    VGRSET, NOGRID, VARTDA, VARDAG, NOSEG ,
     +                    GRDSEG, A     , VARAGG, IPMSA , INCREM,
     +                    NOFLUX, IFLUX , PROMNR, FLUX  , IEXPNT,
     +                    IKNMRK, NOQ1  , NOQ2  , NOQ3  , NOQ4  ,
     +                    NPROC , NOTOT , DERIV , STOCHI, VOLUME,
     +                    PRONDT, IBFLAG, ISDMP , FLXDMP, NOVAR ,
     +                    VARTAG, IIKNMR, PRONAM, OWNERS, MYPART,
     +                    DSPNDT, VELNDT, dll_opb)
C
      use timers
!     use m_timers_waq
      use m_couplib
c
      INTEGER             IPROC , K     , IDT   , ITFACT, NOGRID,
     +                    NOSEG , NOFLUX, NOQ1  , NOQ2  , NOQ3  ,
     +                    NOQ4  , NPROC , NOTOT , IBFLAG, NOVAR ,
     +                    IIKNMR, MYPART
      INTEGER             PROGRD(*)      , GRDNOS(*)      ,
     +                    PRVNIO(*)      , PRVTYP(*)      ,
     +                    PRVVAR(*)      , VARARR(*)      ,
     +                    VARIDX(*)      , ARRKND(*)      ,
     +                    ARRPOI(*)      , ARRDM1(*)      ,
     +                    ARRDM2(*)      , VGRSET(NOVAR,*),
     +                    VARTDA(*)      , VARDAG(*)      ,
     +                    GRDSEG(NOSEG,*), VARAGG(*)      ,
     +                    IPMSA (*)      , INCREM(*)      ,
     +                    IFLUX (*)      , PROMNR(*)      ,
     +                    IEXPNT(*)      , IKNMRK(*)      ,
     +                    PRONDT(*)      , ISDMP (*)      ,
     +                    VARTAG(*)      , OWNERS(*)      ,
     +                    DSPNDT(*)      , VELNDT(*)
      REAL                A(*)           , FLUX(*)        ,
     +                    DERIV(*)       , STOCHI(*)      ,
     +                    VOLUME(*)      , FLXDMP(*)
      CHARACTER*20        PRONAM(*)
      integer      , intent(in   ) :: dll_opb     ! open proces library dll handle
C
C     Local
C
      INTEGER             IDTPRO, ITYP
C
C     get the general local work array, first index of LOCAL array
C
!     call timer_start(timer_proces_grids)
      IX_HLP = 1
      IA_HLP = 33
      CALL DHGVAR( IA_HLP, IX_HLP, IV_HLP)
      IK_HLP = ARRKND(IA_HLP)
      IP_HLP = ARRPOI(IA_HLP)
      ID1HLP = ARRDM1(IA_HLP)
      ID2HLP = ARRDM2(IA_HLP)
C
C     Which grid
C
      IGRID = PROGRD(IPROC)
      NOSEG2 = GRDNOS(IGRID)
C
C     Set the variable for this grid
C

      DO IVARIO = 1 , PRVNIO(IPROC)
         ITYP   = PRVTYP(K+IVARIO-1)
         IVAR   = PRVVAR(K+IVARIO-1)
         IARR   = VARARR(IVAR)
         IV_IDX = VARIDX(IVAR)
         IARKND = ARRKND(IARR)
         IP_ARR = ARRPOI(IARR)
         IDIM1  = ARRDM1(IARR)
         IDIM2  = ARRDM2(IARR)
         IF ( ITYP .EQ. 1 ) THEN
C
C           Only for space varying array's
C
            IF ( IARKND .GE. 2 ) THEN
C
C              Only if variable isn't actual set for this grid
C
               IF ( VGRSET(IVAR,IGRID) .EQ. 0 ) THEN
C
C                 Set variable for base grid
C
                  IF ( VGRSET(IVAR,1) .EQ. 0 ) THEN
                     DO IGR3 = 2 , NOGRID
                        IF ( VGRSET(IVAR,IGR3) .EQ. 1 ) THEN
                           NOSEG3 = GRDNOS(IGR3)
C
C                          Determine characteristics of variable
C
                           CALL DHGPOI( IVAR  , IARR  ,
     +                                  IARKND, IV_IDX,
     +                                  IDIM1 , IDIM2 ,
     +                                  IP_ARR, IGR3  ,
     +                                  ISYSI , NOTOTI,
     +                                  IP_ARI)
                           CALL DHGPOI( IVAR  , IARR  ,
     +                                  IARKND, IV_IDX,
     +                                  IDIM1 , IDIM2 ,
     +                                  IP_ARR, 1     ,
     +                                  ISYSO , NOTOTO,
     +                                  IP_ARO)
C
C                          Determine characteristics of WEIGHT variable
C                          ( Don't mind if this one is actual ? )
C
                           IDATYP = VARTDA(IVAR)
                           IF ( IDATYP .EQ. 2 ) THEN
                              IV_DA  = VARDAG(IVAR)
                              IA_DA  = VARARR(IV_DA)
                              IK_DA  = ARRKND(IA_DA)
                              IF ( IK_DA .EQ. 1 ) THEN
C
C                                Not variable in space use help var
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
     +                                     IK_DA , IX_DA ,
     +                                     ID1_DA, ID2_DA,
     +                                     IP_DA , 1     ,
     +                                     ISYSW , NOTOTW,
     +                                     IP_ARW)
                              CALL DHGPOI( IV_HLP, IA_HLP,
     +                                     IK_HLP, IX_HLP,
     +                                     ID1HLP, ID2HLP,
     +                                     IP_HLP, IGR3  ,
     +                                     ISYSH , NOTOTH,
     +                                     IP_ARH)
                           ELSEIF ( IDATYP .EQ. 3 ) THEN
                              IV_DA  = IV_HLP
                              IA_DA  = VARARR(IV_DA)
                              IK_DA  = ARRKND(IA_DA)
                              IX_DA  = VARIDX(IV_DA)
                              IP_DA  = ARRPOI(IA_DA)
                              ID1_DA = ARRDM1(IA_DA)
                              ID2_DA = ARRDM2(IA_DA)
                              CALL DHGPOI( IV_DA , IA_DA ,
     +                                     IK_DA , IX_DA ,
     +                                     ID1_DA, ID2_DA,
     +                                     IP_DA , 1     ,
     +                                     ISYSW , NOTOTW,
     +                                     IP_ARW)
                              CALL DHGPOI( IV_HLP, IA_HLP,
     +                                     IK_HLP, IX_HLP,
     +                                     ID1HLP, ID2HLP,
     +                                     IP_HLP, IGR3  ,
     +                                     ISYSH , NOTOTH,
     +                                     IP_ARH)
                           ELSE
C
C                             Weight and help array's dummy's
C                             so set to the variable itself
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
                           CALL DHDAGG( NOSEG         , NOSEG3   ,
     +                                  NOTOTI        , NOTOTW   ,
     +                                  NOTOTH        , NOTOTO   ,
     +                                  ISYSI         , ISYSW    ,
     +                                  ISYSH         , ISYSO    ,
     +                                  GRDSEG(1,IGR3), IDATYP   ,
     +                                  A(IP_ARI)     , A(IP_ARW),
     +                                  ISWCUM        , A(IP_ARH),
     +                                  A(IP_ARO))
                           VGRSET(IVAR,1) = 1
                        ENDIF
                     ENDDO
                  ENDIF
C
C                 Set the variable for this grid
C
                  IF ( IGRID .NE. 1 ) THEN
C
C                    Determine characteristics of variable
C
                     CALL DHGPOI( IVAR  , IARR  ,
     +                            IARKND, IV_IDX,
     +                            IDIM1 , IDIM2 ,
     +                            IP_ARR, 1     ,
     +                            ISYSI , NOTOTI,
     +                            IP_ARI)
                     CALL DHGPOI( IVAR  , IARR  ,
     +                            IARKND, IV_IDX,
     +                            IDIM1 , IDIM2 ,
     +                            IP_ARR, IGRID ,
     +                            ISYSO , NOTOTO,
     +                            IP_ARO)
C
C                    Determine characteristics of WEIGHT variable
C
                     IAGTYP = VARTAG(IVAR)
                     IF ( IAGTYP .EQ. 2 .OR. IAGTYP .EQ. 3) THEN
                        IV_AG  = VARAGG(IVAR)
                        IA_AG  = VARARR(IV_AG)
                        IX_AG  = VARIDX(IV_AG)
                        IK_AG  = ARRKND(IA_AG)
                        IP_AG  = ARRPOI(IA_AG)
                        ID1_AG = ARRDM1(IA_AG)
                        ID2_AG = ARRDM2(IA_AG)
                        CALL DHGPOI( IV_AG , IA_AG ,
     +                               IK_AG , IX_AG ,
     +                               ID1_AG, ID2_AG,
     +                               IP_AG , 1     ,
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
                     CALL DHAGGR( NOSEG          , NOSEG2   ,
     +                            NOTOTI         , NOTOTW   ,
     +                            NOTOTH         , NOTOTO   ,
     +                            ISYSI          , ISYSW    ,
     +                            ISYSH          , ISYSO    ,
     +                            GRDSEG(1,IGRID), IAGTYP   ,
     +                            A(IP_ARI)      , A(IP_ARW),
     +                            A(IP_ARH)      , A(IP_ARO))
                     VGRSET(IVAR,IGRID) = 1
                  ENDIF
C
               ENDIF
            ENDIF
         ENDIF
C
C        Zet pointer structuur voor procesmodule, dit hoeft eigenlijk maar 1 keer
C
         IF ( IARKND .EQ. 1 ) THEN
            IPMSA (K+IVARIO-1) = IP_ARR + IV_IDX - 1
            INCREM(K+IVARIO-1) = 0
         ELSEIF ( IARKND .EQ. 2 ) THEN
            IPMSA (K+IVARIO-1) = IP_ARR + (IGRID-1)*IDIM1*IDIM2 +
     +                           IV_IDX - 1
            INCREM(K+IVARIO-1) = IDIM1
         ELSEIF ( IARKND .EQ. 3 ) THEN
            IPMSA (K+IVARIO-1) = IP_ARR + (IGRID-1)*IDIM1*IDIM2 +
     +                           (IV_IDX-1)*IDIM1
            INCREM(K+IVARIO-1) = 1
         ENDIF
C
      ENDDO
!     call timer_stop(timer_proces_grids)
C
C     compute fluxes
C
      IPFLUX = (IGRID-1)*NOFLUX*NOSEG + IFLUX(IPROC)
      IPKNMR = (IGRID-1)*ARRDM1(IIKNMR)*ARRDM2(IIKNMR) + 1
      CALL PROCAL (A        , PROMNR(IPROC), FLUX(IPFLUX), IPMSA(K)      , INCREM(K)    ,
     &             NOSEG2   , NOFLUX       , IEXPNT      , IKNMRK(IPKNMR), NOQ1         ,
     &             NOQ2     , NOQ3         , NOQ4        , PRONAM(IPROC) , PRVNIO(IPROC),
     &             PRVTYP(K), iproc        , dll_opb     )
C
C     the used grid is now the only actual value for the output
C
      DO IVARIO = 1 , PRVNIO(IPROC)
         ITYP   = PRVTYP(K+IVARIO-1)
         IF ( ITYP .EQ. 3 .OR. ITYP .EQ. 4 .OR. ITYP .EQ. 5 ) THEN
            IVAR   = PRVVAR(K+IVARIO-1)
            IARR   = VARARR(IVAR)
            IARKND = ARRKND(IARR)
C
C           Only for space varying array's
C
            IF ( IARKND .GE. 2 ) THEN
               DO IGR2 = 1 , NOGRID
                  VGRSET(IVAR,IGR2) = 0
               ENDDO
               VGRSET(IVAR,IGRID) = 1
            ENDIF
         ENDIF

         ! set fractional step array for dispersion and velocities from the processes

         IF ( ITYP .EQ. 4 ) THEN
            IARR   = VARARR(IVAR)
            IF ( IARR .EQ. 40 ) THEN
               IV_IDX = VARIDX(IVAR)
               IF ( IV_IDX .GT. 0 ) THEN
                  DSPNDT(IV_IDX) = PRONDT(IPROC)
               ENDIF
            ENDIF
            IF ( IARR .EQ. 41 ) THEN
               IV_IDX = VARIDX(IVAR)
               IF ( IV_IDX .GT. 0 ) THEN
                  VELNDT(IV_IDX) = PRONDT(IPROC)
               ENDIF
            ENDIF
         ENDIF

      ENDDO
C
C     Scale fluxes with fractional step
C
C
C     Dis-aggregate fluxes to base grid
C     Dit is niet volgens ontwerp, flux zou op stof moeten werken
C     En de stof daarna herverdeeld over volgens de oude verdeling.
C     dus is deze herverdeling voor twee stoffen anders dus kunnen
C     we niet eerst de flux op het basis nivo brengen
C
C     Oplossing zou zijn de flux voor iedere stof te disaggregeren
C     met als gewicht de massa van die stof en dan meteen de deriv
C     voor die stof met de fractional step te vullen. Vervolgens
C     voor de volgende stof hetzelfde geintje te herhalen. Dus een
C     loop over de stochi's toe te voegen. Op deze manier loop je voor een
C     stof met veel fluxen wel eindeloss te aggregeren natuurlijk.
C
C     Dus zou je ook eerst deriv op geaggregeerd grid kunnen cummuleren en
C     daarna eenmaal naar het basis grid. Dit betekend dat ook DERIV
C     voor alle grids gedefinieerd moet zijn. We moeten er nu wel voor zorgen
C     dat alle derivs voor een stof op hetzelfde grid komt of dat we alle
C     derivs bij disaggregatie mee moeten nemen. Als we dan een vlaggetje
C     per stof per grid meenemen of de deriv gevuld is doen we geen extra werk.
C     Kunnen we de disaggregatie zo maken dat deze cummuleert in de variable op
C     het target grid. PRODER vervalt hiermee, wat doen we met met de FLXDMP
C     functionaliteit van PRODER hier is theoretisch nog een probleem .
C     verder is nodig STOCHI, DTSTEP, VOLUME
C
C     NOG checken of VOLUME OP HET GRID ACTUEEL IS !!! PROBLEEM we
C     weten het variable nummer van vol niet.
C
C

      RETURN
      END

      subroutine twopro ( nproc  , nogrid , noflux , novar  , noseg  ,
     &                    notot  , progrd , grdnos , iflux  , vgrset ,
     &                    grdseg , volume , deriv  , stochi , flux   ,
     &                    prondt , ibflag , isdmp  , flxdmp , owners ,
     &                    mypart , ipbloo , ipchar , istep  )

!     Deltares - Delft Software Department

!     Created   : Dec. 2009 by Leo Postma

!     Function  : This routine has been split off from the 'onepro' routine and in that sense
!                 Jan van Beek is the author of this code since somewhere 1992.
!                 Onepro is used in a parallel setting in such a way that previous processes
!                 always have completed the generation of input for the following processes.
!                 Conflicts nevertheless arose because more parallel instances of 'onepro'
!                 could together want to update the same derivative array. This is prevented
!                 by isolation of the update of the derivative array for all processes together
!                 in this separate routine outside of the parallel region of 'onepro'.

!     Modified  :

!     Subroutines called :  dhaggr - fills a variable on a specific grid from its values on another grid
!                           prodr2 - updates the derivatives from the fluxes
!                           profld - fills the dump array for fluxes used in a mass balance

      use timers
      implicit none

!     Arguments           :

!     Kind        Function         Name   Dimensions                 Description

      integer(4), intent(in   ) :: nproc                           ! Total number of processes
      integer(4), intent(in   ) :: nogrid                          ! Total number of grids
      integer(4), intent(in   ) :: noflux                          ! Total number of fluxes
      integer(4), intent(in   ) :: novar                           ! Total number of variables
      integer(4), intent(in   ) :: noseg                           ! Total number of computational volumes
      integer(4), intent(in   ) :: notot                           ! Total number of substances
      integer(4), intent(in   ) :: progrd(nproc )                  ! The grid number of each process
      integer(4), intent(in   ) :: grdnos(nogrid)                  ! The nummber of volumes in each grid
      integer(4), intent(in   ) :: iflux (nproc )                  ! Offset in the flux array per process
      integer(4), intent(inout) :: vgrset(novar         , nogrid)  ! Indicates whether a variable for a grid is set
      integer(4), intent(in   ) :: grdseg(noseg         , nogrid)  ! Probably the aggregation pointer of the grids
      real   (4), intent(inout) :: volume(noseg         , nogrid)  ! Computational volumes
      real   (4), intent(inout) :: deriv (notot , noseg , nogrid)  ! Array with derivatives
      real   (4), intent(in   ) :: stochi(notot , noflux)          ! Stoichiometric factors per flux
      real   (4), intent(in   ) :: flux  (noflux, noseg , nogrid)  ! Process fluxes
      integer(4), intent(in   ) :: prondt(nproc )                  ! Time step size of the process
      integer(4), intent(in   ) :: ibflag                          ! If > 0 then balances are required
      integer(4), intent(in   ) :: isdmp (noseg )                  ! Segment to dumped segment pointer
      real   (4), intent(inout) :: flxdmp(noflux, *     )          ! Dumped fluxes
      integer(4), intent(in   ) :: owners(noseg )                  ! MPI array for parallelism owning nodes of the volumes
      integer(4), intent(in   ) :: mypart                          ! MPI calling node number
      integer(4), intent(in   ) :: ipbloo                          ! The BLOOM  process if any
      integer(4), intent(in   ) :: ipchar                          ! The CHARON process if any
      integer(4), intent(in   ) :: istep                           ! Time step nr.

!     Local

      integer(4)                :: iproc                           ! Loop counter over processes
      integer(4)                :: igrid                           ! Grid nr of this process
      integer(4)                :: noseg2                          ! Number of computational volumes in this grid
      integer(4)                :: nfluxp                          ! Number of fluxes in this process
      integer(4), save          :: ithandl = 0
      if ( timon ) call timstrt ( "twopro", ithandl )

      do iproc = 1, nproc
         if ( iproc .eq. ipbloo .or. iproc .eq. ipchar ) cycle
         if ( mod( istep-1, prondt(iproc) ) .ne. 0 ) cycle

!        See if this process produces fluxes

         if ( iproc .ne. nproc ) then
            nfluxp = iflux(iproc+1) - iflux(iproc)
         else
            nfluxp = noflux - iflux(iproc) + 1
         endif
         if ( nfluxp .eq. 0 ) cycle

!        If necessary set volume for this grid.

         igrid  = progrd(iproc)
         noseg2 = grdnos(igrid)
         if ( vgrset(1,igrid) .ne. 1 ) then  !
            call dhaggr( noseg          , noseg2  , 1       , 1       , 1       ,
     &                   1              , 1       , 1       , 1       , 1       ,
     &                   grdseg(1,igrid), 1       , volume  , volume  , volume  ,
     &                   volume(1,igrid))
            vgrset(1,igrid) = 1              !  Volume is always variable 1
         endif

!        Construct derivatives from these fluxes on this grid

         call prodr2 ( deriv(1,1,igrid), notot          , noflux , stochi         , iflux (iproc),
     &                 nfluxp          , flux(1,1,igrid), noseg2 , volume(1,igrid), prondt(iproc),
     &                 owners          , mypart         )

!        For the use in balances, store fluxes in 'flxdmp' using aggregation pointer 'isdmp'

         if ( ibflag .gt. 0 ) then
            call profld ( noflux  , iflux (iproc), nfluxp  , igrid   , noseg2         ,
     &                    noseg   , prondt(iproc), isdmp   , grdseg  , flux(1,1,igrid),
     &                    volume  , flxdmp       )
         endif

      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end
