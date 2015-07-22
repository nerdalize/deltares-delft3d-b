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

      SUBROUTINE PRONRS ( PRONAM, IMODUL)
!>\file
!>       Returns number of the process routine

C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : jan -1994 by Jan van Beek
C
C     FUNCTION            : Returns module number
C                           NOTE the numbers in this subroutine must have
C                           an 1 to 1 relation with the labels in the
C                           subroutine PROCEZ.
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     PRONAM  CHA*(*)       1     INPUT   Name of called module
C     IMODUL  INTEGER       1     OUTPUT  Module number proces
C
C     Declaration of arguments
C
      INTEGER       IMODUL
      CHARACTER*(*) PRONAM
C
C     Local declarations
C
      PARAMETER   ( NOMODU = 123)
      CHARACTER*6   MODNAM(NOMODU)
      SAVE          MODNAM
      DATA MODNAM /
     +   'DDEPTH',
     +   'DSURF',
     +   'TOTDEP',
     +   'EMERSI',
     +   'METEO',
     +   'HEATFL',
     +   'DAYRAD',
     +   'TEMPER',
     +   'VARSAL',
     +   'VELOC',
     +   'RESTIM',
     +   'STOX3D',
     +   'HDISP',
     +   'HDISPV',
     +   'WATAGE',
     +   'INTPOL',
     +   'CALCHZ',
     +   'CALWAV',
     +   'CALTAU',
     +   'SIMPH',
     +   'SPCARB',
     +   'EXTINA',
     +   'EXTINC',
     +   'CLCRAD',
     +   'DAYL',
     +   'DEPAVE',
     +   'VTRANS',
     +   'D40BLO',
     +   'PHCOMB',
     +   'MAKPOC',
     +   'PHCOMP',
     +   'SEDCOM',
     +   'WKCOMP',
     +   'DMVOL',
     +   'BACMRT',
     +   'SATCO2',
     +   'REAR',
     +   'ADSPO4',
     +   'DENSED',
     +   'DENWAT',
     +   'NITRIF',
     +   'SATOXY',
     +   'VAROXY',
     +   'BOTMIN',
     +   'BODCOD',
     +   'DECBOD',
     +   'DECPC5',
     +   'VIVIAN',
     +   'DISSI',
     +   'SEDOX',
     +   'TFALG',
     +   'DLALG',
     +   'NLALG',
     +   'RADALG',
     +   'RDBALG',
     +   'PRIPRO',
     +   'SDPPRO',
     +   'PPRLIM',
     +   'NUTUPT',
     +   'NUTREL',
     +   'NRALGS',
     +   'OXYMIN',
     +   'CSELAC',
     +   'EBUCH4',
     +   'SATCH4',
     +   'SULFID',
     +   'SULFOX',
     +   'SULFPR',
     +   'METHOX',
     +   'SPECFE',
     +   'IRONOX',
     +   'SULPHO',
     +   'IRONRE',
     +   'PRIRON',
     +   'CALSED',
     +   'SEDCAR',
     +   'SEDNUT',
     +   'SEDSOD',
     +   'SSEDPH',
     +   'SOMSED',
     +   'SEDAAP',
     +   'RESDM',
     +   'BURIAL',
     +   'DIGGIN',
     +   'ADVTRA',
     +   'DSPTRA',
     +   'RFPART',
     +   'PARTMP',
     +   'TRASE2',
     +   'ULFIX',
     +   'CONSBL',
     +   'SWOXY',
     +   'TRCOEF',
     +   'VERVLU',
     +   'DEGMP',
     +   'SEDHM',
     +   'SEDOMV',
     +   'ATMDEP',
     +   'NH3FRE',
     +   'POSOXY',
     +   'SECCHI',
     +   'PTEWOR',
     +   'STREAR',
     +   'TRSOXY',
     +   'APATIT',
     +   'HARVES',
     +   'VEG2DN',
     +   'VBSTAT',
     +   'VBGRO',
     +   'VBMRT',
     +   'VEG3DX',
     +   'VBUPT',
     +   'VEG3DU',
     +   'SALCHL',
     +   'DECDET',
     +   'S12TRA',
     +   'RESANT',
     +   'STADAY',
     +   'STADPT',
     +   'STADSC',
     +   'STAGEO',
     +   'STAPRC',
     +   'STAQTL'
     +    /
C
C     Set module number
C
      IMODUL = 0
      DO 10 J = 1,NOMODU
         IF (PRONAM(1:6).EQ.MODNAM(J)) IMODUL = J
   10 CONTINUE
C
      RETURN
      END
