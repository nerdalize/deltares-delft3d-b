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

      DOUBLE PRECISION FUNCTION JULIAN ( IDATE , ITIME )
C
C     +----------------------------------------------------------------+
C     |    W A T E R L O O P K U N D I G   L A B O R A T O R I U M     |
C     |               Sector Waterbeheer & Milieu                      |
C     +----------------------------------------------------------------+
C
C***********************************************************************
C
C     Project : T0467
C     Author  : Andre Hendriks
C     Date    : 891215             Version : 1.00
C
C     Changes in this module :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     891215  Andre Hendriks  Version 1.00
C
C***********************************************************************
C
C     Description of module :
C
C        This functions returns the so called Julian day of a date, or
C        the value -1.0 if an error occurred.
C
C        The Julian day of a date is the number of days that has passed
C        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
C        to compute differces between dates. ( See SUBROUTINE GREGOR
C        for the reverse proces ).
C
C***********************************************************************
C
C     Arguments :
C
C     Name   Type     In/Out Size            Description
C     ------ -----    ------ -------         ---------------------------
C     IDATE  integer  in     -               Date as YYYYMMDD
C     ITIME  integer  in     -               Time as HHMMSS
C
C     Local variables :
C
C     Name   Type     Size   Description
C     ------ -----    ------ ------------------------
C     TEMP1  real*8   -      Temporary variable
C     TEMP2  real*8   -      Temporary variable
C     IYEAR  integer  -      Year   ( -4713-.. )
C     IMONTH integer  -      Month  ( 1-12 )
C     IDAY   integer  -      Day    ( 1-28,29,30 or 31 )
C     IHOUR  integer  -      Hour   ( 0-23 )
C     IMIN   integer  -      Minute ( 0-59 )
C     ISEC   integer  -      Second ( 0-59 )
C     MONLEN integer  12     Length of month in days
C
C     Calls to : none
C
C***********************************************************************
C
C     Variables :
C
      INTEGER          IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC  ,
     1                 IDATE , ITIME , MONLEN(12)
      DOUBLE PRECISION TEMP1 , TEMP2
      CHARACTER*48     LINE
C
C***********************************************************************
C
C     Initialize lenghts of months :
C
      DATA MONLEN / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
C
C***********************************************************************
C
C
C
      IYEAR  = IDATE/10000
      IMONTH = IDATE/100 - IYEAR*100
      IDAY   = IDATE - IYEAR*10000 - IMONTH*100
      IHOUR  = ITIME/10000
      IMIN   = ITIME/100 - IHOUR*100
      ISEC   = ITIME - IHOUR*10000 - IMIN*100
      IF (( IYEAR  .LT. -4713 ) .OR. ( IMONTH .LT.  1 ) .OR.
     1    ( IMONTH .GT.    12 ) .OR. ( IDAY   .LT.  1 ) .OR.
     2    ( IDAY   .GT. MONLEN(IMONTH) ) .OR.
     3    ( IHOUR  .LT.     0 ) .OR. ( IHOUR  .GT. 24 ) .OR.
     4    ( IMIN   .LT.     0 ) .OR. ( IMIN   .GT. 60 ) .OR.
     5    ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 60 )) THEN
         JULIAN = -1.0
         WRITE(LINE,'(A33,I8,''-'',I6)') 'ERROR in JULIAN interpeting time:',IDATE,ITIME
         CALL MONSYS(LINE,1)
         GOTO 999
      ELSE
         TEMP1  = INT (( IMONTH-14.0) / 12.0 )
*-----------------------------
c        WRITE(*,*) 'TEMP1 : ', TEMP1
c        WRITE(*,*) 'dbg 1 : ',
c    1          INT ( 1461.0 * ( IYEAR + 4800.0 + TEMP1 ) / 4.0 )
c        WRITE(*,*) 'dbg 2 : ',
c    2          INT ( 367.0 * ( IMONTH - 2.0 - TEMP1 * 12.0 ) / 12.0 )
c        WRITE(*,*) 'dbg 3 : ',
c    1          INT ( 3.0 * INT ( ( IYEAR + 4900.0 + TEMP1 ) / 100.0 ) /
c    2          4.0 )
*-----------------------------
         TEMP2  = IDAY - 32075.0 +
     1          INT ( 1461.0 * ( IYEAR + 4800.0 + TEMP1 ) / 4.0 ) +
     2          INT ( 367.0 * ( IMONTH - 2.0 - TEMP1 * 12.0 ) / 12.0 ) -
     3          INT ( 3.0 * INT ( ( IYEAR + 4900.0 + TEMP1 ) / 100.0 ) /
     4          4.0 )
         TEMP1  = FLOAT ( IHOUR ) * 3600.0 +
     1            FLOAT ( IMIN  ) *   60.0 +
     2            FLOAT ( ISEC  ) - 43200.0
         JULIAN = TEMP2 + ( TEMP1 / 86400.0 )
      ENDIF
  999 RETURN
      END
