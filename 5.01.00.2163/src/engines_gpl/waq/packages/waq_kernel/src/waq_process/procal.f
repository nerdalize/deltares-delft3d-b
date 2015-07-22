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

      subroutine procal (pmsa   , imodul , flux   , ipoint , increm ,                             ! &
     &                   noseg  , noflux , iexpnt , iknmrk , noq1   ,                             ! &
     &                   noq2   , noq3   , noq4   , pronam , pronvr ,                             ! &
     &                   prvtyp , iproc  , dll_opb)
!>\file
!>       Calls the process modules

!     Deltares Software Centre

!     Created   : November 1992 by Jos van Gils and Jan van Beek

!     Modified  : ????? 2007, Jan van Beek : implemented user defined processes through dll
!                 ????? 2009, Leo Postma   : implemented timers
!                 March 2010, Leo Postma   : implemented parallel processing

!     use m_timers_waq
      use timers
      implicit none

!     parameters          :

!     kind           function         name          description

      real   (4)   , intent(inout) :: pmsa  ( * ) ! Process module status array
      integer      , intent(in   ) :: imodul      ! Process module number
      real   (4)   , intent(  out) :: flux  ( * ) ! Process fluxes
      integer      , intent(in   ) :: ipoint( * ) ! Pointer to process data
      integer      , intent(in   ) :: increm( * ) ! Increment in pointer process data
      integer      , intent(in   ) :: noseg       ! Number of computational volumes
      integer      , intent(in   ) :: noflux      ! Number of process fluxes
      integer      , intent(in   ) :: iexpnt(4,*) ! Exchange pointers
      integer      , intent(in   ) :: iknmrk( * ) ! Tag array
      integer      , intent(in   ) :: noq1        ! Number of exchanges in first direction
      integer      , intent(in   ) :: noq2        ! Number of exchanges in second direction
      integer      , intent(in   ) :: noq3        ! Number of exchanges in third direction
      integer      , intent(in   ) :: noq4        ! Number of exchanges in the water bed
      character(20), intent(in   ) :: pronam      ! Name of this process
      integer      , intent(in   ) :: pronvr      ! Not used
      integer      , intent(in   ) :: prvtyp( * ) ! Not used
      integer      , intent(in   ) :: iproc       ! Process number
      integer      , intent(in   ) :: dll_opb     ! open proces library dll handle

!     local

      integer             :: perf_function
      integer             :: lunrep
      integer             :: ierror

      integer(4), parameter :: nomax = 500
      integer(4), save      :: ithand(nomax) = 0 !  timer handles
      if ( timon ) then
         if ( iproc .le. nomax ) call timstrt ( pronam, ithand(iproc) )
      endif

!     call timer_start(timer_offs_proces0+imodul)
      select case ( imodul )
         case (1 ) ;  call DDEPTH ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (2 ) ;  call DSURF ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (3 ) ;  call TOTDEP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (4 ) ;  call EMERSI ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (5 ) ;  call METEO ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (6 ) ;  call HEATFL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (7 ) ;  call DAYRAD ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (8 ) ;  call TEMPER ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (9 ) ;  call VARSAL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (10 ) ;  call VELOC ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (11 ) ;  call RESTIM ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (12 ) ;  call STOX3D ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (13 ) ;  call HDISP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (14 ) ;  call HDISPV ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (15 ) ;  call WATAGE ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (16 ) ;  call INTPOL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (17 ) ;  call CALCHZ ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (18 ) ;  call CALWAV ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (19 ) ;  call CALTAU ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (20 ) ;  call SIMPH ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (21 ) ;  call SPCARB ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (22 ) ;  call EXTINA ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (23 ) ;  call EXTINC ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (24 ) ;  call CLCRAD ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (25 ) ;  call DAYL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (26 ) ;  call DEPAVE ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (27 ) ;  call VTRANS ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (28 ) ;  call D40BLO ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (29 ) ;  call PHCOMB ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (30 ) ;  call MAKPOC ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (31 ) ;  call PHCOMP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (32 ) ;  call SEDCOM ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (33 ) ;  call WKCOMP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (34 ) ;  call DMVOL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (35 ) ;  call BACMRT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (36 ) ;  call SATCO2 ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (37 ) ;  call REAR ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (38 ) ;  call ADSPO4 ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (39 ) ;  call DENSED ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (40 ) ;  call DENWAT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (41 ) ;  call NITRIF ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (42 ) ;  call SATOXY ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (43 ) ;  call VAROXY ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (44 ) ;  call BOTMIN ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (45 ) ;  call BODCOD ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (46 ) ;  call DECBOD ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (47 ) ;  call DECPC5 ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (48 ) ;  call VIVIAN ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (49 ) ;  call DISSI ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (50 ) ;  call SEDOX ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (51 ) ;  call TFALG ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (52 ) ;  call DLALG ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (53 ) ;  call NLALG ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (54 ) ;  call RADALG ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (55 ) ;  call RDBALG ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (56 ) ;  call PRIPRO ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (57 ) ;  call SDPPRO ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (58 ) ;  call PPRLIM ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (59 ) ;  call NUTUPT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (60 ) ;  call NUTREL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (61 ) ;  call NRALGS ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (62 ) ;  call OXYMIN ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (63 ) ;  call CSELAC ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (64 ) ;  call EBUCH4 ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (65 ) ;  call SATCH4 ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (66 ) ;  call SULFID ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (67 ) ;  call SULFOX ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (68 ) ;  call SULFPR ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (69 ) ;  call METHOX ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (70 ) ;  call SPECFE ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (71 ) ;  call IRONOX ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (72 ) ;  call SULPHO ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (73 ) ;  call IRONRE ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (74 ) ;  call PRIRON ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (75 ) ;  call CALSED ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (76 ) ;  call SEDCAR ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (77 ) ;  call SEDNUT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (78 ) ;  call SEDSOD ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (79 ) ;  call SSEDPH ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (80 ) ;  call SOMSED ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (81 ) ;  call SEDAAP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (82 ) ;  call RESDM ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (83 ) ;  call BURIAL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (84 ) ;  call DIGGIN ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (85 ) ;  call ADVTRA ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (86 ) ;  call DSPTRA ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (87 ) ;  call RFPART ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (88 ) ;  call PARTMP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (89 ) ;  call TRASE2 ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (90 ) ;  call ULFIX ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (91 ) ;  call CONSBL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (92 ) ;  call SWOXY ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (93 ) ;  call TRCOEF ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (94 ) ;  call VERVLU ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (95 ) ;  call DEGMP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (96 ) ;  call SEDHM ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (97 ) ;  call SEDOMV ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (98 ) ;  call ATMDEP ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (99 ) ;  call NH3FRE ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (100 ) ;  call POSOXY ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (101 ) ;  call SECCHI ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (102 ) ;  call PTEWOR ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (103 ) ;  call STREAR ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (104 ) ;  call TRSOXY ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (105 ) ;  call APATIT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (106 ) ;  call HARVES ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (107 ) ;  call VEG2DN ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (108 ) ;  call VBSTAT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (109 ) ;  call VBGRO ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (110 ) ;  call VBMRT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (111 ) ;  call VEG3DX ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (112 ) ;  call VBUPT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (113 ) ;  call VEG3DU ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (114 ) ;  call SALCHL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (115 ) ;  call DECDET ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (116 ) ;  call S12TRA ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (117 ) ;  call RESANT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (118 ) ;  call STADAY ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (119 ) ;  call STADPT ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (120 ) ;  call STADSC ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (121 ) ;  call STAGEO ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (122 ) ;  call STAPRC ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )
         case (123 ) ;  call STAQTL ( pmsa   , flux   , ipoint , increm , noseg  ,               ! &
     &                                 noflux , iexpnt , iknmrk , noq1   , noq2   ,               ! &
     &                                 noq3   , noq4   )

         case default

!     assumed from dll

!JVB  WRITE(32,*) 'calling from dll:',PRONAM(1:6)
            ierror = perf_function(dll_opb, pronam(1:6),                                          ! &
     &                             pmsa   , flux   , ipoint , increm , noseg  ,                   ! &
     &                             noflux , iexpnt , iknmrk , noq1   , noq2   ,                   ! &
     &                             noq3   , noq4   )
            if ( ierror .ne. 0 ) then
               call getmlu(lunrep)
               write(*,*) 'ERROR : requested module not in process library DLL'
               write(*,*) 'module: ', pronam(1:6)
               write(lunrep,*) 'ERROR     : requested module not in process library DLL'
               write(lunrep,*) 'module    : ', pronam(1:6)
               write(lunrep,*) 'dll handle: ', dll_opb
                  call srstop(1)
            endif

      end select
!
!     Voor debuggen van in/output per proces.
!     Print voor ieder proces dat berekend is de gebruikte/gewijzigd waardes
!     uit pmsa, voor segment "mydbgseg" van de horizontaal, "mynolay" lagen
!     Gaat helaas alleen goed voor arrays die voor segmenten werken.
!
!     mynoseg  = 13860
!     mynolay  =   5
!     mynosegh = mynoseg / mynolay
!     mydbgseg = 1509
!     call getmlu(lunrep)
!     write(lunrep,*) 'arr-end of procal for module=',imodul,', debug-seg=',
!    +   mydbgseg
!     do iarr = 1, pronvr
!        write(lunrep,'(a,i2,a,i1,a,15g14.7)') 'arr-',iarr,', type=',
!    +      prvtyp(iarr),':',
!    +      ( pmsa(ipoint(iarr) + increm(iarr)* (mydbgseg-1+(ilay-1)*mynosegh) ),ilay=1,mynolay)
!     enddo

!     call timer_stop(timer_offs_proces0+imodul)

      if ( timon ) then
         if ( iproc .le. nomax ) call timstop ( ithand(iproc) )
      endif

      return
      end
