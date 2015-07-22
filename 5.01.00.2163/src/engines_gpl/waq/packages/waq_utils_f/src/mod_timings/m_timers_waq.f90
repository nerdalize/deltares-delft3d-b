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

module m_timers_waq
!!--version information---------------------------------------------------------
! $Date: 2007-11-26 15:18:25 +0100 (Mon, 26 Nov 2007) $
! $Revision: 41 $
!!--description-----------------------------------------------------------------
!
! Adding a timer:
! 1) Add an identification name (module parameter)
!    Example:
!    integer, parameter :: timer_new          = 12
! 2) Increase num_timers (module parameter)
!    Example:
!    integer, parameter :: num_timers         = 12
! 3) Add the name/description of the timer (module subroutine timers_init)
!    Maximum of 20 characters
!    Example:
!    gdp%gdtimers%names(timer_new)       = 'My own new timer'
! 4) Add an explanation (module subroutine timers_finish)
!    Example:
!    write(LOUTTM,'(a)') '|Momentum eq.   : Part of Simulation ("UZD")|'
! 5) Add in the source code calls to timer_start and timer_stop around the
!    code you want to time
!    Example:
!    call timer_start(timer_new, 1)
!    ... code to be timed ...
!    call timer_stop(timer_new, 1)
! Remarks/restrictions:
! - The subroutines containing calls to timer_start/timer_stop must contain
!   the line:
!   use m_timers_waq
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use m_timings
implicit none
!
public timer_start
public timer_stop
!
integer, parameter :: timer_total          = 1
integer, parameter :: timer_init           = timer_total + 1
!
! timers per integration routine:
integer, parameter :: timer_offs_intsrt    = timer_init
integer, parameter :: intsrt_num_timers    = 20
!
! timers per chunk of each integration routine:
integer, parameter :: timer_proces         = timer_offs_intsrt + &
                                                        intsrt_num_timers + 1
integer, parameter :: timer_transport      = timer_proces + 1
integer, parameter :: timer_transp_comm    = timer_transport + 1
integer, parameter :: timer_mass_balnc     = timer_transp_comm + 1
integer, parameter :: timer_readdata       = timer_mass_balnc + 1
integer, parameter :: timer_user           = timer_readdata + 1
integer, parameter :: timer_bound          = timer_user + 1
integer, parameter :: timer_wastes         = timer_bound + 1
integer, parameter :: timer_output         = timer_wastes + 1
integer, parameter :: timer_close          = timer_output + 1
!
! timers per process of process-library:
integer, parameter :: timer_proces_grids   = timer_close + 1
integer, parameter :: timer_proces_fluxes  = timer_proces_grids + 1
integer, parameter :: timer_proces_disper  = timer_proces_fluxes + 1
integer, parameter :: timer_proces_comm    = timer_proces_disper + 1
integer, parameter :: timer_offs_proces0   = timer_proces_comm
integer, parameter :: max_num_processes    = 200
!
! timers for CouPLib communication library:
integer, parameter :: timer_start_couplib  = timer_offs_proces0 + &
                                                        max_num_processes + 1
integer, parameter :: couplib_max_timers   = 20
!
! timers for temporary performance-debugging, used on ad-hoc basis:
integer, parameter :: timer_temp0          = timer_start_couplib + &
                                                        couplib_max_timers
integer, parameter :: max_temp_timers      = 20

! Total number of timers
integer, parameter :: num_timers           = timer_temp0 + max_temp_timers
contains
!
!
!==============================================================================
subroutine timers_waq_init(lunrep)
!
! Parameters
!
   integer, intent(in) :: lunrep        ! logical unit of stdout
!
! Local variables
!
   integer           :: istat, i
   character(len=20) :: tmpstr
   character(len=20) :: process_names(max_num_processes) = (/ &
      'Proces   1: "ADSPO4"',  &
      'Proces   2: "ALGMRT"',  &
      'Proces   3: "ATMDEP"',  &
      'Proces   4: "BACMRT"',  &
      'Proces   5: "BOD"   ',  &
      'Proces   6: "BODCOD"',  &
      'Proces   7: "BOTMIN"',  &
      'Proces   8: "BURCAR"',  &
      'Proces   9: "BURHM" ',  &
      'Proces  10: "BURIAL"',  &
      'Proces  11: "BURNUT"',  &
      'Proces  12: "BUROMV"',  &
      'Proces  13: "CALCHZ"',  &
      'Proces  14: "CALSED"',  &
      'Proces  15: "CALTAU"',  &
      'Proces  16: "CHLOR" ',  &
      'Proces  17: "CLCRAD"',  &
      'Proces  18: "CONSBL"',  &
      'Proces  19: "D40BLO"',  &
      'Proces  20: "D40CHA"',  &
      'Proces  21: "D40SWI"',  &
      'Proces  22: "DAYL"  ',  &
      'Proces  23: "DDELT" ',  &
      'Proces  24: "DDEPTH"',  &
      'Proces  25: "DENSED"',  &
      'Proces  26: "DENWAT"',  &
      'Proces  27: "DIGCAR"',  &
      'Proces  28: "DIGFIX"',  &
      'Proces  29: "DIGGIN"',  &
      'Proces  30: "DIGHM" ',  &
      'Proces  31: "DIGNUT"',  &
      'Proces  32: "DIGOMV"',  &
      'Proces  33: "DLALG" ',  &
      'Proces  34: "DSURF" ',  &
      'Proces  35: "EXPLFL"',  &
      'Proces  36: "EXTINA"',  &
      'Proces  37: "EXTINC"',  &
      'Proces  38: "FEADSO"',  &
      'Proces  39: "GMREED"',  &
      'Proces  40: "GRD"   ',  &
      'Proces  41: "INTPOL"',  &
      'Proces  42: "KSORHM"',  &
      'Proces  43: "KSOROM"',  &
      'Proces  44: "LINPOL"',  &
      'Proces  45: "DEGMP" ',  &
      'Proces  46: "DECFSN"',  &
      'Proces  47: "MAKPOC"',  &
      'Proces  48: "METRES"',  &
      'Proces  49: "MINLIM"',  &
      'Proces  50: "SUMCOL"',  &
      'Proces  51: "NITRIF"',  &
      'Proces  52: "NLALG" ',  &
      'Proces  53: "NRALGS"',  &
      'Proces  54: "NUTREL"',  &
      'Proces  55: "NUTUPT"',  &
      'Proces  56: "OXYMIN"',  &
      'Proces  57: "PHCOMP"',  &
      'Proces  58: "POSOXY"',  &
      'Proces  59: "POCOMP"',  &
      'Proces  60: "PPRLIM"',  &
      'Proces  61: "VAROXY"',  &
      'Proces  62: "PRIPRO"',  &
      'Proces  63: "CSELAC"',  &
      'Proces  64: "EBUCH4"',  &
      'Proces  65: "PARTMP"',  &
      'Proces  66: "SATCH4"',  &
      'Proces  67: "RADALG"',  &
      'Proces  68: "RDBALG"',  &
      'Proces  69: "REAR"  ',  &
      'Proces  70: "RFPART"',  &
      'Proces  71: "RESALG"',  &
      'Proces  72: "RESANT"',  &
      'Proces  73: "RESCAR"',  &
      'Proces  74: "RESDM" ',  &
      'Proces  75: "RESFIX"',  &
      'Proces  76: "RESHM" ',  &
      'Proces  77: "RESNUT"',  &
      'Proces  78: "RESOMV"',  &
      'Proces  79: "RESTIM"',  &
      'Proces  80: "S2X"   ',  &
      'Proces  81: "SALIN" ',  &
      'Proces  82: "SATCO2"',  &
      'Proces  83: "SATOXY"',  &
      'Proces  84: "SDPPRO"',  &
      'Proces  85: "SECCHI"',  &
      'Proces  86: "SEDAAP"',  &
      'Proces  87: "SEDCAR"',  &
      'Proces  88: "SEDCOM"',  &
      'Proces  89: "SEDFIX"',  &
      'Proces  90: "SEDHM" ',  &
      'Proces  91: "SEDNAL"',  &
      'Proces  92: "SEDNUT"',  &
      'Proces  93: "SEDOMV"',  &
      'Proces  94: "SEDOX" ',  &
      'Proces  95: "SEDPHY"',  &
      'Proces  96: "SEDSOD"',  &
      'Proces  97: "SOMRES"',  &
      'Proces  98: "SOMSED"',  &
      'Proces  99: "SSEDPH"',  &
      'Proces 100: "STOX3D"',  &
      'Proces 101: "STVAR" ',  &
      'Proces 102: "SULOXI"',  &
      'Proces 103: "SULSED"',  &
      'Proces 104: "SWBUR" ',  &
      'Proces 105: "SWBURA"',  &
      'Proces 106: "SWBURN"',  &
      'Proces 107: "SWOXY" ',  &
      'Proces 108: "SWSEDN"',  &
      'Proces 109: "TEMPER"',  &
      'Proces 110: "TFALG" ',  &
      'Proces 111: "TOTDEP"',  &
      'Proces 112: "TRCOEF"',  &
      'Proces 113: "VDISP" ',  &
      'Proces 114: "VELOC" ',  &
      'Proces 115: "VERVLU"',  &
      'Proces 116: "WATAGE"',  &
      'Proces 117: "WATMIN"',  &
      'Proces 118: "WEIRCH"',  &
      'Proces 119: "WEIROX"',  &
      'Proces 120: "WKCOMP"',  &
      'Proces 121: "XTOS3D"',  &
      'Proces 122: "XVELOC"',  &
      'Proces 123: "ZOODYN"',  &
      'Proces 124: "SULFID"',  &
      'Proces 125: "NORHM" ',  &
      'Proces 126: "NH3FRE"',  &
      'Proces 127: "SIMPH" ',  &
      'Proces 128: "SULFOX"',  &
      'Proces 129: "SULFPR"',  &
      'Proces 130: "CALWAV"',  &
      'Proces 131: "ULFIX" ',  &
      'Proces 132: "VELOCV"',  &
      'Proces 133: "CALSND"',  &
      'Proces 134: "CEC"   ',  &
      'Proces 135: "DECFST"',  &
      'Proces 136: "DECREF"',  &
      'Proces 137: "DECSLW"',  &
      'Proces 138: "DISSI" ',  &
      'Proces 139: "VIVIAN"',  &
      'Proces 140: "GEMCMP"',  &
      'Proces 141: "MFBNUT"',  &
      'Proces 142: "GEMMFB"',  &
      'Proces 143: "GEMMIN"',  &
      'Proces 144: "GEMMND"',  &
      'Proces 145: "GEMNLM"',  &
      'Proces 146: "DECSLN"',  &
      'Proces 147: "DECREN"',  &
      'Proces 148: "GEMTMP"',  &
      'Proces 149: "MFBLLM"',  &
      'Proces 150: "MNDINI"',  &
      'Proces 151: "MNDLLM"',  &
      'Proces 152: "ADVTRA"',  &
      'Proces 153: "CALVS" ',  &
      'Proces 154: "DSPTRA"',  &
      'Proces 155: "TRASE2"',  &
      'Proces 156: "VIVIA2"',  &
      'Proces 157: "VTRANS"',  &
      'Proces 158: "DEPAVE"',  &
      'Proces 159: "DMVOL" ',  &
      'Proces 160: "METHOX"',  &
      'Proces 161: "PTEWOR"',  &
      'Proces 162: "BLUETD"',  &
      'Proces 163: "NUTCNK"',  &
      'Proces 164: "OYSTER"',  &
      'Proces 165: "VERTNK"',  &
      'Proces 166: "STADAY"',  &
      'Proces 167: "STADPT"',  &
      'Proces 168: "STADSC"',  &
      'Proces 169: "STAGEO"',  &
      'Proces 170: "STAPRC"',  &
      'Proces 171: "STAQTL"',  &
      'Proces 172: "FERDOM"',  &
      'Proces 173: "GROAB" ',  &
      'Proces 174: "GROHB" ',  &
      'Proces 175: "HYDPOM"',  &
      'Proces 176: "LYSIS" ',  &
      'Proces 177: "DECBOD"',  &
      'Proces 178: "STREAR"',  &
      'Proces 179: "HDISP" ',  &
      'Proces 180: "METEO" ',  &
      'Proces 181: "HEATFL"',  &
      'Proces 182: "HDISPV"',  &
      'Proces 183: "VARSAL"',  &
      'Proces 184:         ',  &
      'Proces 185:         ',  &
      'Proces 186:         ',  &
      'Proces 187:         ',  &
      'Proces 188:         ',  &
      'Proces 189:         ',  &
      'Proces 190:         ',  &
      'Proces 191:         ',  &
      'Proces 192:         ',  &
      'Proces 193:         ',  &
      'Proces 194:         ',  &
      'Proces 195:         ',  &
      'Proces 196:         ',  &
      'Proces 197:         ',  &
      'Proces 198:         ',  &
      'Proces 199:         ',  &
      'Proces 200:         '   &
                                       /)
!
!! executable statements -------------------------------------------------------
!
!  set unit-number of stdout for this application:
!
   LOUTTM = lunrep
!
!  set size of timer-table for this application:
!
   call timer_table_size(num_timers)
!
!  initialize names of all timers
!
   do i = 1, num_timers
      write(tmpstr,'(a,i4)') 'timer', i
      call timer_name(i, namtmr=tmpstr)
   enddo
!
!  set application-specific names for timers:
!
   call timer_name(timer_total     , namtmr='Total'                 )
!
!  main chunks of application:
!
   call timer_name(timer_init      , namtmr='Initialization'        )
   do i = 1, intsrt_num_timers
      write(tmpstr,'(a,i3)') 'Integrat.routine', i
      call timer_name(timer_offs_intsrt+i, namtmr=tmpstr)
   enddo
!
!  main chunks per integration routine:
!
   call timer_name(timer_proces     , namtmr='Proces library'        )
   call timer_name(timer_transport  , namtmr='Transport eq.'         )
   call timer_name(timer_transp_comm, namtmr='Comm. transp.eq.'      )
   call timer_name(timer_mass_balnc , namtmr='Mass balnc calc/comm'  )
   call timer_name(timer_readdata   , namtmr='Read input data'       )
   call timer_name(timer_user       , namtmr='User routines'         )
   call timer_name(timer_bound      , namtmr='Boundaries'            )
   call timer_name(timer_wastes     , namtmr='Wasteloads'            )
   call timer_name(timer_output     , namtmr='Output'                )
   call timer_name(timer_close      , namtmr='Close and stop'        )
!
!  all the processes of the process-library:
!
   call timer_name(timer_proces_grids    , namtmr='Grid manipulations' )
   call timer_name(timer_proces_fluxes   , namtmr='Flux manipulations' )
   call timer_name(timer_proces_disper   , namtmr='Dispersion calc.  ' )
   call timer_name(timer_proces_comm     , namtmr='Communications    ' )
   do i = 1, max_num_processes
      call timer_name(timer_offs_proces0+i, namtmr=process_names(i))
   enddo
!
!  names for timers for CouPLib are set by couplib_timers_init
!
!  names for ad-hoc performance timers:
!
   do i = 1, max_temp_timers
      write(tmpstr,'(a,i3)') 'temp-timer', i
      call timer_name(timer_temp0+i, namtmr=tmpstr)
   enddo

end subroutine timers_waq_init
!
!
!
!==============================================================================
subroutine timers_waq_print
!
! Local variables
!
   integer                                :: i, ireftm, num_used
   real(kind=8), dimension(2)             :: cum_time
   real(kind=8)                           :: total_cpu, total_wall
   character(len=20)                      :: namtmr
   integer                                :: numtms
   real(kind=8)                           :: cputim, waltim
!
!! executable statements ------------------------------------------------------
!
!------------------------------------------------------------------------------
!  1) Write header and total time
!------------------------------------------------------------------------------
!
   write(LOUTTM,'(a)') 'Performance timers:'
   write(LOUTTM, 333)
   write(LOUTTM,'(a)') '|Timer name                     |   cpu time        |    wall clock      |'
   write(LOUTTM,'(a)') '|                               |-------------------|--------------------|'
   write(LOUTTM,'(a)') '|                     | #times  |   sec     |  %    |    sec     |  %    |'
   write(LOUTTM, 333)
!
!  obtain total cpu-time and wall-clock time for computing percentages
!
   call timer_read(timer_total, 1, namtmr, numtms, total_cpu, total_wall)
   total_cpu = max(1e-6, total_cpu)
   total_wall = max(1e-6, total_wall)

   write(LOUTTM,111) namtmr, numtms, total_cpu, 100., total_wall, 100.
!
!------------------------------------------------------------------------------
!  2) Write overview for entire simulation.
!------------------------------------------------------------------------------
!
   write(LOUTTM, 222) 'In init, simulation:                          '

   cum_time = 0
   do i = timer_init, timer_offs_intsrt + intsrt_num_timers

      call timer_read(i, 1, namtmr, numtms, cputim, waltim)

      if (numtms.gt.0) write(LOUTTM,111) namtmr, numtms, &
                                        cputim, 100.*cputim/total_cpu, &
                                        waltim, 100.*waltim/total_wall
      cum_time(1) = cum_time(1) + cputim
      cum_time(2) = cum_time(2) + waltim
   enddo

   write(LOUTTM, 112) 'Total:              ', &
                     cum_time(1), 100.*cum_time(1)/total_cpu, &
                     cum_time(2), 100.*cum_time(2)/total_wall

!------------------------------------------------------------------------------
!  3) Write breakdown of time for integration routine
!------------------------------------------------------------------------------

   write(LOUTTM, 222) 'In integration routine:                       '

   cum_time = 0
   do i = timer_proces, timer_close
      call timer_read(i, 1, namtmr, numtms, cputim, waltim)

      if (numtms.gt.0) write(LOUTTM,111) namtmr, numtms, &
                                        cputim, 100.*cputim/total_cpu, &
                                        waltim, 100.*waltim/total_wall
      cum_time(1) = cum_time(1) + cputim
      cum_time(2) = cum_time(2) + waltim
   enddo

   write(LOUTTM, 112) 'Total Integration:  ', &
                     cum_time(1), 100.*cum_time(1)/total_cpu, &
                     cum_time(2), 100.*cum_time(2)/total_wall

!------------------------------------------------------------------------------
!  4) Write breakdown of time for process library
!------------------------------------------------------------------------------

   write(LOUTTM, 222) 'In processes library:                         '

   call timer_read(timer_proces, 1, namtmr, numtms, total_cpu, total_wall)
   total_cpu  = max(1e-6, total_cpu)
   total_wall = max(1e-6, total_wall)

   cum_time = 0
   do i = timer_proces_grids, timer_offs_proces0 + max_num_processes
      call timer_read(i, 1, namtmr, numtms, cputim, waltim)

      if (numtms.gt.0) write(LOUTTM,111) namtmr, numtms, &
                                        cputim, 100.*cputim/total_cpu, &
                                        waltim, 100.*waltim/total_wall
      cum_time(1) = cum_time(1) + cputim
      cum_time(2) = cum_time(2) + waltim
   enddo

   write(LOUTTM, 112) 'Total Processes:    ', &
                     cum_time(1), 100.*cum_time(1)/total_cpu, &
                     cum_time(2), 100.*cum_time(2)/total_wall

!------------------------------------------------------------------------------
!  5) Write timings for CouPLib communications library
!------------------------------------------------------------------------------

   write(LOUTTM, 222) 'In CouPLib communications library:            '

   call timer_read(timer_total, 1, namtmr, numtms, total_cpu, total_wall)
   total_cpu  = max(1e-6, total_cpu)
   total_wall = max(1e-6, total_wall)

   cum_time = 0
   do i = timer_start_couplib, timer_start_couplib + couplib_max_timers - 1
      call timer_read(i, 1, namtmr, numtms, cputim, waltim)

      if (numtms.gt.0) write(LOUTTM,111) namtmr, numtms, &
                                        cputim, 100.*cputim/total_cpu, &
                                        waltim, 100.*waltim/total_wall
      cum_time(1) = cum_time(1) + cputim
      cum_time(2) = cum_time(2) + waltim
   enddo

   write(LOUTTM, 112) 'Total Communication:', &
                     cum_time(1), 100.*cum_time(1)/total_cpu, &
                     cum_time(2), 100.*cum_time(2)/total_wall

!------------------------------------------------------------------------------
!  6) Write temporary performance timings, when used
!------------------------------------------------------------------------------

!  temporary timers are printed relative to "reference timer" ireftm:

   ireftm = 1
   call timer_read(timer_total, ireftm, namtmr, numtms, total_cpu, total_wall)
   total_cpu  = max(1e-6, total_cpu)
   total_wall = max(1e-6, total_wall)

!  count number of temporary timers used, for suppressing header when num==0

   num_used = 0

   do i = timer_temp0 + 1, timer_temp0 + max_temp_timers
      call timer_read(i, 1, namtmr, numtms, cputim, waltim)

      if (numtms.gt.0) then
         num_used = num_used + 1
         if (num_used.eq.1) &
            write(LOUTTM, 222) 'Temporary performance timers:                 '
            write(LOUTTM,111) namtmr, numtms, &
                                        cputim, 100.*cputim/total_cpu, &
                                        waltim, 100.*waltim/total_wall
      endif
   enddo

!------------------------------------------------------------------------------
!  Close table
!------------------------------------------------------------------------------

   write(LOUTTM, 333)

!------------------------------------------------------------------------------
!  Format statements
!------------------------------------------------------------------------------

 111 format('|',a,' |',i8,' |',2(f10.1,' | ',f5.1,' | '))
 112 format('|',a,' |',8x,' |',2(f10.1,' | ',f5.1,' | '))

 222 format('|------------------------------------------------------------------------|', /, &
            '| *** ', a, '                     |', /, &
            '|------------------------------------------------------------------------|')

 333 format('|------------------------------------------------------------------------|')

end subroutine timers_waq_print

end module m_timers_waq
