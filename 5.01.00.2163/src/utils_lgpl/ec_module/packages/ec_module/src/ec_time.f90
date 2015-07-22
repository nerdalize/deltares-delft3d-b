module ec_time
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: ec_time.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_time.f90 $
!!--description-----------------------------------------------------------------
!
! see ec_module.f90
! More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!
!!--pseudo code and references--------------------------------------------------
!
! adri.mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
use precision
!
implicit none
!
! types
! order inside a type: doubles, reals, integers, pointers, logicals, characters
!
  type tTime
    real(hp) :: time    ! modified Julian Date
    integer  :: iDate   ! yyyymmdd
    integer  :: iTime   ! hhmmsscc
  end type tTime
!
! interfaces
!
!
! public entities
!
contains
!
!
!==============================================================================
! Set subroutines
!==============================================================================
function setTime(time) result(timeStruc)
  !
  ! result
  type(tTime) :: timeStruc
  !
  ! arguments
  real(hp), intent(in) :: time
  !
  ! body
  timeStruc%time = time
  call timeToITime(timeStruc)
end function setTime
!
!
!==============================================================================
function setITime(iDate, iTime) result(timeStruc)
  !
  ! result
  type(tTime) :: timeStruc
  !
  ! arguments
  integer, intent(in) :: iDate
  integer, intent(in) :: iTime
  !
  ! body
  timeStruc%iDate = iDate
  timeStruc%iTime = iTime
  call iTimeToTime(timeStruc)
end function setITime
!
!
!==============================================================================
! Convert subroutines
!==============================================================================
subroutine timeToITime(timeStruc)
  !
  ! This subroutine is based on:
  ! - DIO-time-support, DioJulian2DioTime
  !
  ! arguments
  type(tTime) :: timeStruc
  !
  ! locals
  integer  :: iyear
  integer  :: imonth
  integer  :: iday
  integer  :: ihour
  integer  :: imin
  integer  :: isec
  integer  :: icentiSec
  real(hp) :: dsec      ! seconds and centiSeconds as one real
  !
  ! body
  call Gregor(timeStruc%time, iyear , imonth, iday  , ihour , imin, isec, dsec)
  icentiSec       = nint((dsec - real(isec,hp)) * 100.0_hp)
  timeStruc%idate = iyear*10000 + imonth*100 + iday
  timeStruc%itime = ihour*1000000 + imin*10000 + isec*100 + icentiSec
end subroutine timeToITime
!
!
!==============================================================================
subroutine iTimeToTime(timeStruc)
  !
  ! This subroutine is based on:
  ! - DIO-time-support, DioTimeString2Julian
  ! - rr_rtc_tools    , Modified_Julian_fromJulian
  !
  ! arguments
  type(tTime) :: timeStruc
  !
  ! locals
  integer          :: iyear, imonth, iday, ihour, imin, isec, icentiSec, absDate
  integer          :: monlen(12)
  real(hp)  :: temp1 , temp2
  !
  ! Initialize lenghts of months :
  !
  data monlen / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
  !
  ! body
  timeStruc%time = -1.0_hp
  icentiSec      = mod(timeStruc%itime,100)
  isec           = mod(timeStruc%itime,10000)/100
  imin           = mod(timeStruc%itime,1000000)/10000
  ihour          =     timeStruc%itime/1000000
  absdate        = abs(timeStruc%idate)
  iday           = mod(absdate,100)
  imonth         = mod(absdate,10000)/100
  iyear          =     absdate/10000
  iyear          = sign(iyear, timeStruc%idate)

  if (( iyear  .lt. -4713 ) .or. ( imonth .lt.  1 ) .or.    &
      ( imonth .gt.    12 ) .or. ( iday   .lt.  1 ) .or.    &
      ( iday   .gt. monlen(imonth) ) .or.                   &
      ( ihour  .lt.     0 ) .or. ( ihour  .gt. 23 ) .or.    &
      ( imin   .lt.     0 ) .or. ( imin   .gt. 59 ) .or.    &
      ( isec   .lt.     0 ) .or. ( isec   .gt. 59 )) then
     timeStruc%time = -1.0_hp
  else
     temp1  = int (( real(imonth,hp)-14.0_hp) / 12.0_hp )
     !-----debug---------------
     !    write(*,*) 'temp1 : ', temp1
     !    write(*,*) 'DBG 1 : ', &
     !           int ( 1461.0 * ( iyear + 4800.0 + temp1 ) / 4.0 )
     !    write(*,*) 'DBG 2 : ', &
     !           int ( 367.0 * ( imonth - 2.0 - temp1 * 12.0 ) / 12.0 )
     !    write(*,*) 'DBG 3 : ', &
     !           int ( 3.0 * int ( ( iyear + 4900.0 + temp1 ) / 100.0 ) / &
     !           4.0 )
     !-------------------------
     temp2  = real(iday,hp) - 32075.0_hp                                        &
              + real(int(1461.0_hp * ( real(iyear,hp) + 4800.0_hp + temp1 ) / 4.0_hp ))       &
              + real(int( 367.0_hp * ( real(imonth,hp) - 2.0_hp - temp1 * 12.0_hp ) / 12.0_hp ))  &
              - real(int(   3.0_hp * real(int( ( real(iyear,hp) + 4900.0_hp + temp1 ) / 100.0_hp )) /  4.0_hp ))
     temp1  =   real(ihour,hp) * 3600.0_hp                                   &
              + real(imin ,hp) *   60.0_hp                                   &
              + real(isec ,hp) + ( real(icentiSec,hp) / 100.0_hp ) - 43200.0_hp
     !
     ! Julian day
     !
     timeStruc%time = temp2 + ( temp1 / 86400.0_hp )
     !
     ! Modified Julian day
     !
     timeStruc%time = timeStruc%time - 2400000.5_hp
  endif
end subroutine iTimeToTime
!
!
!==============================================================================
subroutine checkTime
  !
  ! locals
  integer     :: testIDate
  integer     :: testITime
  real(hp)    :: testTime
  type(tTime) :: timeStruc
  !
  ! body
  write(*,*) 'ec_time::testTime start ...'
  ! set an arbitrary test date/time
  testIDate = 19690911
  testITime = 12345067
  ! itime -> time
  timeStruc = setITime(testIDate, testITime)
  testTime  = timeStruc%time
  ! time -> itime
  timeStruc = setTime(testTime)
  write(*,*) testIDate, ' =?= ', timeStruc%idate
  write(*,*) testITime, ' =?= ', timeStruc%itime
  write(*,*) '... ec_time::testTime end'
end subroutine checkTime


!***********************************************************************
!***********************************************************************
!    Date:       29 Apr 1993 / 11 Dec 2002 (adjusted)
!    Time:       09:35
!    Program:    GREGOR.FOR
!    Version:    1.01
!    Programmer: Andre Hendriks
!    Previous version(s):
!    1.00 -- 29 Apr 1993 -- 09:23 -- Operating System: DOS
!    0.0 -- 29 Apr 1993 --  9:22 -- Operating System: DOS
!    Project:    T1234.56
!    Module:     GREGOR
!    Function:
!    Comment:
!    Reference:
!    Review:
      SUBROUTINE GREGOR ( JULIAN, IYEAR , IMONTH, IDAY  , IHOUR , &
                          IMIN  , ISEC  , DSEC   )
!
!     +----------------------------------------------------------------+
!     |    W A T E R L O O P K U N D I G   L A B O R A T O R I U M     |
!     |               Sector Waterbeheer & Milieu                      |
!     +----------------------------------------------------------------+
!
!***********************************************************************
!
!     Project : T0467
!     Author  : Andre Hendriks
!     Date    : 891215             Version : 1.00
!
!     Changes in this module :
!
!     Date    Author          Description
!     ------  --------------  -----------------------------------
!     ......  ..............  ..............................
!     891215  Andre Hendriks  Version 1.00
!
!***********************************************************************
!
!     Description of module :
!
!        This functions returns the Gregorian date and the time of a so
!        called Julian day, or iyear -9999 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differces between dates. ( See DOUBLE PRECISION
!        FUNCTION JULIAN for the reverse proces ).
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     JULIAN real*8   in     -               Julian day
!     IYEAR  integer  out    -               Year   ( -4713-.. )
!     IMONTH integer  out    -               Month  ( 1-12 )
!     IDAY   integer  out    -               Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  out    -               Hour   ( 0-23 )
!     IMIN   integer  out    -               Minute ( 0-59 )
!     ISEC   integer  out    -               Second ( 0-59 )
!     DSEC   real*8   out    -               Second as double
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     TEMP3  real*8   -      Temporary variable
!     TEMP4  real*8   -      Temporary variable, JULIAN
!     TEMP5  real*8   -      Temporary variable, fractional part JULIAN
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      INTEGER           :: IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC
      DOUBLE PRECISION  :: JULIAN, TEMP1 , TEMP2 , TEMP3 , TEMP4 , TEMP5
      DOUBLE PRECISION  :: DSEC
      DOUBLE PRECISION  :: myJULIAN, delta
      integer           :: nTry
!
!***********************************************************************
!
!
!
      delta = 0.0D+00

      IF ( JULIAN .LT. 0.0 ) THEN
         IYEAR = -9999
      ELSE
         nTry = 1
         DO WHILE ( nTry <= 2 )
             myJULIAN= JULIAN + delta
             TEMP4 = myJULIAN
             TEMP5 = DMOD ( myJULIAN, 1.0D0 )
             IF ( TEMP5 .LT. 0.5 ) THEN
                TEMP3  = 0.5 + TEMP5
                TEMP4  = DINT ( TEMP4 )
             ELSE
                TEMP3  = TEMP5 - 0.5
                TEMP4  = DINT ( TEMP4 ) + 1.0
             ENDIF
             TEMP1  = TEMP4 + 68569.0
             TEMP2  = DINT  ( 4.0 * TEMP1 / 146097.0 )
             TEMP1  = TEMP1 - DINT ( ( 146097.0 * TEMP2 + 3.0 ) / 4.0 )
             IYEAR  = INT   ( 4000.0 * ( TEMP1 + 1.0 ) / 1461001.0 )
             TEMP1  = TEMP1 - DINT ( (1461.0D0 * IYEAR) / 4.0 ) + 31.0
             IMONTH = INT   ( 80.0 * TEMP1 / 2447.0 )
             IDAY   = INT   ( TEMP1 - AINT ( 2447.0 * IMONTH / 80.0 ) )
             TEMP1  = DINT  ( dble(IMONTH / 11.0D0) )
             IMONTH = INT   ( IMONTH + 2.0 - 12.0 * TEMP1 )
             IYEAR  = INT   ( 100.0 * ( TEMP2 - 49.0 ) + IYEAR + TEMP1 )
             IHOUR  = INT   ( TEMP3 * 24.0 )
             IMIN   = INT   ( TEMP3 * 1440.0 - 60.0 * IHOUR )
             DSEC   =         TEMP3 * 86400.0 - 3600.0 * IHOUR - 60.0*IMIN
             ISEC   = NINT  ( DSEC )

             if ( isec >= 60 ) then
                 if ( nTry < 2 ) then
                     delta = 0.49999D+00 / 86400.0D+00
                     nTry = nTry + 1
                 else
                     IYEAR = -9999
                     exit
                 endif
             else
                 exit
             endif
         ENDDO

      ENDIF

      END SUBROUTINE GREGOR



end module ec_time
