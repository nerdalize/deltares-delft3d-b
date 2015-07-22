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

      subroutine dlwqni ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Iterative stationary vertical central method (18)
!>
!>                         Performs substance by substance a stationairy solution.\n
!>                         Uses the GMRES method with Krilov sub-spaces.\n
!>                         Horizontal fluxes are discretized upwind.\n
!>                         Vertical fluxes are discretized central.\n

C     CREATED            : june 1988 by L. Postma
C
C     MODIFIED           : feb 1997, by RJ Vos; like 6 but steady state solver
C                          is GMRES and compact storage from option 16
C
C     LOGICAL UNITS      : LUN(19) , output, monitoring file
C                          LUN(20) , output, formatted dump file
C                          LUN(21) , output, unformatted hist. file
C                          LUN(22) , output, unformatted dump file
C                          LUN(23) , output, unformatted restart file
C
C     SUBROUTINES CALLED : DLWQTR, user transport routine
C                          DLWQWQ, user waterquality routine
C                          DLWQPP, user postprocessing routine
C                          DLWQO2, DELWAQ4 output routine
C                          DLWQ13, system postpro-dump routine
C                          DLWQ15, wasteload routine
C                          DLWQ60, scales water quality
C                          DLWQH1, set diagonal and deriv in deriv(1)
C                          DLWQG3, fills the matrix (except diagonal)
C                          DLWQ63, stores the results
C                          DLWQ64, performs mass balance computation
C                          DLWQ65, computes closure error
C                          DLWQ66, makes masses
C                          DLWQH3, check diagonal on zero's
C                          SGMRES, fast solver
C                          DHOPNF, opens files
C                          MOVE  , copy an array
C
C      NOTE             :   " DELWAQ FASTSOLVERS 2 " (R.J.Vos, M.Borsboom and K.
C       Newton-Krylov methods for solving linear and non-linear equations
C       report T1596, January 1996, Deltares
C                       :   NSYS = 1 always
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
C     ---------------------------------------------------------
C     A       REAL       *      LOCAL  real      workspace array
C     J       INTEGER    *      LOCAL  integer   workspace array
C     C       CHARACTER  *      LOCAL  character workspace array
C     LUN     INTEGER    *      INPUT  array with unit numbers
C     LCHAR   CHARACTER  *      INPUT  filenames
C
C     Declaration of arguments
C
      use grids
      use timers
      use waqmem
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress

      implicit none

      include 'actions.inc'
C
C     Declaration of arguments
C
      REAL, DIMENSION(*)          :: A
      INTEGER, DIMENSION(*)       :: J
      INTEGER, DIMENSION(*)       :: LUN
      CHARACTER*(*), DIMENSION(*) :: C
      CHARACTER*(*), DIMENSION(*) :: LCHAR
      INTEGER                     :: ACTION
      TYPE(DELWAQ_DATA)           :: DLWQD
      type(GridPointerColl)       :: GridPs               ! collection off all grid definitions

!$    include "omp_lib.h"

C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI  /    Timer characteristics
C
      INCLUDE 'sysi.inc'
C
C     COMMON  /  SYSA   /   Pointers in real array workspace
C
      INCLUDE 'sysa.inc'
C
C     COMMON  /  SYSJ   /   Pointers in integer array workspace
C
      INCLUDE 'sysj.inc'
C
C     COMMON  /  SYSC   /   Pointers in character array workspace
C
      INCLUDE 'sysc.inc'
C
C     Local declarations
C
      LOGICAL          IMFLAG , IDFLAG , IHFLAG
      LOGICAL          LDUMMY , LSTREC , LREWIN
      LOGICAL          LITREP , UPDATE
      real(8)          tol

      INTEGER         ITIME
      INTEGER         ITIMEL
      INTEGER         IFFLAG
      INTEGER         IAFLAG
      INTEGER         IBFLAG
      INTEGER         NDDIM
      INTEGER         NVDIM
      INTEGER         ISYS
      INTEGER         ICSYS
      INTEGER         NSYS
      INTEGER         INWTYP
      INTEGER         ISTEP
      INTEGER         ITH
      INTEGER         I
      INTEGER         ISCALE
      INTEGER         NOPRED
      INTEGER         ITER
      INTEGER         IOPTPC
      INTEGER          :: NOSSS
      INTEGER          :: NOQTT

      integer       :: ithandl
      integer, save :: ithand1 = 0 ! Leave local

      include 'state_data.inc'

      if ( action == action_initialisation  .or.
     &     action == action_finalisation           ) then
          return
      endif

C
C          some initialisation for fast solver
C
C       IOPTPC = preconditioner switch [0 = none, 1 = GS (L), 2 = GS (U),
C       3 = SSOR], ITER = maximum number of iterations [ > 0],
C       TOL = relative tolerance [10^-3, 10^-10], ISCALE = row scaling
C       of system of equations [0 = no, 1 =yes], KLAT = number of
C       layers in preconditioner [1,KMAX]
C
      ithandl = 0
      if ( timon ) call timstrt ( "dlwqni", ithandl )

      call dlwqf5 ( lun(19) , nocons  , c(icnam), a(icons), ioptpc  ,
     &              iter    , tol     , iscale  , litrep  , noseg   ,
     &              noq3    , noq     , nobnd   , novec   , nomat   ,
     &             nolay   , intsrt  , intopt  )

      itime   = itstrt+idt
      ifflag  = 0
      iaflag  = 0
      ibflag  = 0
      if ( mod(intopt,16) .ge. 8 ) ibflag = 1
      if ( ndspn .eq. 0 ) then
         nddim = nodisp
      else
         nddim = ndspn
      endif
      if ( nveln .eq. 0 ) then
         nvdim = novelo
      else
         nvdim = nveln
      endif
      lstrec = icflag .eq. 1
      nosss  = noseg + nseg2
      noqtt  = noq + noq4
      inwtyp = intyp + nobnd

!      Initialize pointer matices for fast solvers

      call dlwqf1 ( noseg   , nobnd   , noq     , noq1    , noq2    ,
     &              nomat   , j(ixpnt), j(iwrk) , j(imat) , rowpnt  ,
     &              fmat    , tmat    )

      iexseg = 1     !  There is nothing to mask. This array is meant for method 21

C
C======================= simulation loop ============================
C
C          make closure error correction
C
      if ( j(inrh2+1) .ge. 0 .and. ivflag .eq. 0 .and.
     &      idt       .gt. 0 .and. lstrec               ) then
         call dlwq41 ( lun     , itstrt+idt, itstrt  , a(iharm), a(ifarr),
     &                 j(inrha), j(inrh2)  , j(inrft), noseg   , a(ivol2),
     &                 j(ibulk), lchar     , ftype   , isflag  , ivflag  ,
     &                 update  , j(inisp)  , a(inrsp), j(intyp), j(iwork),
     &                 lstrec  , lrewin    , a(ivoll), mypart  , dlwqd   )
         call dlwq65 ( a(ivol2), a(ivol)   , idt     , noseg   )
      else
         call zero   ( a(ivol2), noseg     )
      endif

!        Determine the volumes and areas that ran dry,
!        They cannot have explicit processes during this time step

         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , nopa     ,
     &                 c(ipnam) , a(iparm) , nosfun   , c(isfna) , a(isfun) ,
     &                 j(iknmr) , iknmkv   )

!          do the user transport processes

      CALL DLWQTR ( NOTOT   , NOSYS   , NOSEG   , NOQ     , NOQ1    ,
     &              NOQ2    , NOQ3    , NOPA    , NOSFUN  , NODISP  ,
     &              NOVELO  , J(IXPNT), A(IVOL) , A(IAREA), A(IFLOW),
     &              A(ILENG), A(ICONC), A(IDISP), A(ICONS), A(IPARM),
     &              A(IFUNC), A(ISFUN), A(IDIFF), A(IVELO), itstrt  ,
     &              IDT     , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     &              C(IPNAM), C(IFNAM), C(ISFNA), UPDATE  , ILFLAG  ,
     &              NPARTp  )
Cjvb
C     Temporary ? set the variables grid-setting for the DELWAQ variables
C
      CALL SETSET ( LUN(19), NOCONS, NOPA  , NOFUN   , NOSFUN,
     +              NOSYS  , NOTOT , NODISP, NOVELO  , NODEF ,
     +              NOLOC  , NDSPX , NVELX , NLOCX   , NFLUX ,
     +              NOPRED , NOVAR , NOGRID, J(IVSET))
Cjvb
C
C          call PROCES subsystem
C
      CALL PROCES ( NOTOT   , NOSEG   , A(ICONC), A(IVOL) , ITIME   ,
     +              IDT     , A(IDERV), NDMPAR  , NPROC   , NFLUX   ,
     +              J(IIPMS), J(INSVA), J(IIMOD), J(IIFLU), J(IIPSS),
     +              A(IFLUX), A(IFLXD), A(ISTOC), IBFLAG  , IPBLOO  ,
     +              IPCHAR  , IOFFBL  , IOFFCH  , A(IMASS), NOSYS   ,
     +              ITFACT  , A(IMAS2), IAFLAG  , INTOPT  , A(IFLXI),
     +              J(IXPNT), J(IKNMR), NOQ1    , NOQ2    , NOQ3    ,
     +              NOQ4    , NDSPN   , J(IDPNW), A(IDNEW), NODISP  ,
     +              J(IDPNT), A(IDIFF), NDSPX   , A(IDSPX), A(IDSTO),
     +              NVELN   , J(IVPNW), A(IVNEW), NOVELO  , J(IVPNT),
     +              A(IVELO), NVELX   , A(IVELX), A(IVSTO), A(IDMPS),
     +              J(ISDMP), J(IPDMP), NTDMPQ  , A(IDEFA), J(IPNDT),
     +              J(IPGRD), J(IPVAR), J(IPTYP), J(IVARR), J(IVIDX),
     +              J(IVTDA), J(IVDAG), J(IVTAG), J(IVAGG), J(IAPOI),
     +              J(IAKND), J(IADM1), J(IADM2), J(IVSET), J(IGNOS),
     +              J(IGSEG), NOVAR   , A       , NOGRID  , NDMPS   ,
     +              C(IPRNA), INTSRT  , J(IOWNS), J(IOWNQ), MYPART  ,
     +              j(iprvpt), j(iprdon), nrref , j(ipror), nodef   ,
     +              surface  ,lun(19) )

!          loop over the systems

      ith = 1
      IAFLAG = 1
      DO 10 ISYS = 1 , NOSYS

!          do the user water quality processes

         ICSYS = ISYS
         CALL DLWQWQ ( NOTOT   , NOSYS   , NOSEG   , NOPA    , NOSFUN  ,
     &                 A(IVOL) , A(ICONC), A(ICONS), A(IPARM), A(IFUNC),
     &                 A(ISFUN), A(IDERV), ICSYS   , IDT     , A(ISMAS),
     &                 IBFLAG  , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     &                 C(IPNAM), C(IFNAM), C(ISFNA), NODUMP  , J(IDUMP))
         CALL DLWQ60 ( A(IDERV), A(ICONC), NOTOT   , NOSEG   , ITFACT  ,
     &                 A(IMAS2), ISYS    , 1       , A(IDMPS), INTOPT  ,
     &                 J(ISDMP))

!          add the waste loads

         call dlwq15 ( nosys     , notot    , noseg    , noq      , nowst    ,
     &                 nowtyp    , ndmps    , intopt   ,     1    , itime    ,
     &                 iaflag    , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &                 a(iflow ) , j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &                 j(inwtyp) , j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &                 iknmkv    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna ) , a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &                 a(iwdmp)  , isys     , 1        , j(iowns ), mypart   )

!          fill the diagonal of the matrix, with conc-array and closure error

         call dlwqh1 ( noseg         , notot         , nobnd         , isys          , gm_diag(1,ith),
     &                 a(ivol2)      , a(iconc)      )

!          build rest of matrix like in option 16

         call dlwqg3 ( noseg         , nobnd         , noq1          , noq2          , noq           ,
     &                 j(ixpnt)      , nddim         , nvdim         , j(idpnw)      , j(ivpnw)      ,
     &                 a(iarea)      , a(iflow)      , a(ileng)      , a(idisp)      , a(idnew)      ,
     &                 a(ivnew)      , isys          , intopt        , ilflag        , nomat         ,
     &                 gm_amat(1,ith), j(imat)       , rowpnt        , gm_diag(1,ith),gm_diac(1,ith),
     &                 iscale        , fmat          , tmat          )

!          initial guess : take rhs / diagonal

         call dlwqh3 ( noseg         , nosys         , notot         , nobnd         , isys          ,
     &                 a(iderv)      , a(iboun)      , gm_rhs(1,ith) , gm_diac(1,ith), gm_sol (1,ith))

!          solve linear system of equations
!          note that RHS is in A(IDERV) for steady state otpions

         call sgmres ( noseg+nobnd   , gm_rhs (1,ith), gm_sol (1,ith), novec         , gm_work(1,ith),
     &                 noseg+nobnd   , gm_hess(1,ith), novec+1       , iter          , tol           ,
     &                 nomat         , gm_amat(1,ith), j(imat)       , gm_diag(1,ith), rowpnt        ,
     &                 nolay         , ioptpc        , nobnd         , gm_trid(1,ith), iexseg (1,ith),
     &                 lun(19)       , litrep        )

!           copy solution for this substance into concentration array, note that the array for
!                                                                      segment dumps is not filled yet

         call dlwqh6 ( noseg        , notot   , isys    , 1       , a(iconc),
     &                 gm_sol(1,ith), a(imas2), a(idmps), intopt  , j(isdmp))

   10 continue
C
C          mass balance
C
      IAFLAG = 1
      CALL DLWQ64 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *              A(IVNEW), A(ICONC), A(IBOUN), J(IXPNT), NOSYS   ,
     *              NOTOT   , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *              NVDIM   , J(IDPNW), J(IVPNW), INTOPT  , A(IMAS2),
     *              ILFLAG  , A(IDMPQ), NDMPQ   , J(IQDMP))
      CALL DLWQ66 ( A(IDERV), A(IVOL) , A(ICONC), NOTOT   , NOSEG   )
C
C     Call OUTPUT system ( note that mass is in A(IDERV) )
C
      CALL DLWQO2 ( NOTOT   , NOSEG   , NOPA    , NOSFUN  , ITSTRT  ,
     +              C(IMNAM), C(ISNAM), C(IDNAM), J(IDUMP), NODUMP  ,
     +              A(ICONC), A(ICONS), A(IPARM), A(IFUNC), A(ISFUN),
     +              A(IVOL) , NOCONS  , NOFUN   , 1       , NOUTP   ,
     +              LCHAR   , LUN     , J(IIOUT), J(IIOPO), A(IRIOB),
     +              C(IONAM), NX      , NY      , J(IGRID), C(IEDIT),
     +              NOSYS   , A(IBOUN), J(ILP)  , A(IDERV), A(IMAS2),
     +              A(ISMAS), NFLUX   , A(IFLXI), ISFLAG  , IAFLAG  ,
     +              IBFLAG  , IMSTRT  , IMSTOP  , IMSTEP  , IDSTRT  ,
     +              IDSTOP  , IDSTEP  , IHSTRT  , IHSTOP  , IHSTEP  ,
     +              IMFLAG  , IDFLAG  , IHFLAG  , NOLOC   , A(IPLOC),
     +              NODEF   , A(IDEFA), ITSTRT  , ITSTOP  , NDMPAR  ,
     +              C(IDANA), NDMPQ   , NDMPS   , J(IQDMP), J(ISDMP),
     +              J(IPDMP), A(IDMPQ), A(IDMPS), A(IFLXD), NTDMPQ  ,
     +              C(ICBUF), NORAAI  , NTRAAQ  , J(IORAA), J(NQRAA),
     +              J(IQRAA), A(ITRRA), C(IRNAM), A(ISTOC), NOGRID  ,
     +              NOVAR   , J(IVARR), J(IVIDX), J(IVTDA), J(IVDAG),
     +              J(IAKND), J(IAPOI), J(IADM1), J(IADM2), J(IVSET),
     +              J(IGNOS), J(IGSEG), A       , NOBND   , NOBTYP  ,
     +              C(IBTYP), J(INTYP), C(ICNAM), NOQ     , J(IXPNT),
     +              INTOPT  , C(IPNAM), C(IFNAM), C(ISFNA), J(IDMPB),
     +              NOWST   , NOWTYP  , C(IWTYP), J(IWAST), J(INWTYP),
     +              A(IWDMP), iknmkv  , J(IOWNS), MYPART  )
C
C
C          printout of results
C
c     CALL DLWQ10 ( LUN(19) , J(IDUMP), A(IDERV), A(ICONC), A(IMAS2),
c    *              ITSTRT  ,    1    , ITSTRT  , ITSTRT+1,    1    ,
c    *              C(IDNAM), C(ISNAM), C(IMNAM), NODUMP  , NOTOT   ,
c    *              NOSEG   , J(ILP)  , ISFLAG  , IAFLAG  , IMFLAG  ,
c    *              A(ISMAS), INTOPT  , NDMPAR  , C(IDANA))
C
C          close files, except monitor file
C

      call CloseHydroFiles( dlwqd%collcoll )
      call close_files( lun )
C
C          write restart file
C
      CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITSTRT, C(IMNAM) ,
     *              C(ISNAM) , NOTOT , NOSEG    )
C
C          user output routine
C
      CALL DLWQPP ( NOTOT   , NOSYS   , NOSEG   , NOPA    , NOSFUN  ,
     *              ITSTRT  , IMFLAG  , IDFLAG  , IHFLAG  , C(IMNAM),
     *              C(ISNAM), C(IDNAM), C(IWSID), J(IDUMP), NODUMP  ,
     *              J(IWAST), NOWST   , A(ICONC), A(ICONS), A(IPARM),
     *              A(IFUNC), A(ISFUN), A(IVOL ), A(IWSTE), A(IBOUN),
     *              NOBND   , ITSTRT  , ITSTOP  , NX      , NY      ,
     *              J(IGRID), NODISP  , NOVELO  , NOQ     , NOQ1    ,
     *              NOQ2    , NOQ3    , A(IDISP), A(IVELO), A(ISMAS),
     *              IBFLAG  , NOCONS  , NOFUN   , C(ICNAM), C(IPNAM),
     *              C(IFNAM), C(ISFNA), C(IBNID))
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
