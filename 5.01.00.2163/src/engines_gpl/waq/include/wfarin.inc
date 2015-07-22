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

C     WRITE THE VARIABLES IN THE ARRAY
      WRITE(IDEBUG,*) 'ISEG           = ',ISEG
      WRITE(IDEBUG,*) 'BT             = ',BT
      WRITE(IDEBUG,*) 'BEDEK          = ',BEDEK
      WRITE(IDEBUG,*) 'CA0            = ',CA0
      WRITE(IDEBUG,*) 'CN0            = ',CN0
      WRITE(IDEBUG,*) 'CO0            = ',CO0
      WRITE(IDEBUG,*) 'CP0            = ',CP0
      WRITE(IDEBUG,*) 'FOP0           = ',FOP0
      WRITE(IDEBUG,*) 'CS0            = ',CS0
      WRITE(IDEBUG,*) 'DB             = ',DB
      WRITE(IDEBUG,*) 'DH             = ',DH
      WRITE(IDEBUG,*) 'DIFFUS(1)      = ',DIFFUS(1)
      WRITE(IDEBUG,*) 'DIFFUS(2)      = ',DIFFUS(2)
      WRITE(IDEBUG,*) 'DIFFUS(3)      = ',DIFFUS(3)
      WRITE(IDEBUG,*) 'DIFFUS(4)      = ',DIFFUS(4)
      WRITE(IDEBUG,*) 'DIFFUS(5)      = ',DIFFUS(5)
      WRITE(IDEBUG,*) 'DS             = ',DS
      WRITE(IDEBUG,*) 'DT             = ',DT
      WRITE(IDEBUG,*) 'FAB            = ',FAB
      WRITE(IDEBUG,*) 'FDB1           = ',FDB1
      WRITE(IDEBUG,*) 'FDB4           = ',FDB4
      WRITE(IDEBUG,*) 'FDBN1          = ',FDBN1
      WRITE(IDEBUG,*) 'FDBN4          = ',FDBN4
      WRITE(IDEBUG,*) 'FDBP1          = ',FDBP1
      WRITE(IDEBUG,*) 'FDBP4          = ',FDBP4
      WRITE(IDEBUG,*) 'FNB            = ',FNB
      WRITE(IDEBUG,*) 'FOB            = ',FOB
      WRITE(IDEBUG,*) 'FPB            = ',FPB
      WRITE(IDEBUG,*) 'FPS            = ',FPS
      WRITE(IDEBUG,*) 'FR             = ',FR
      WRITE(IDEBUG,*) 'FRO            = ',FRO
      WRITE(IDEBUG,*) 'FS             = ',FS
      WRITE(IDEBUG,*) 'FSB            = ',FSB
      WRITE(IDEBUG,*) 'LDISP          = ',LDISP
      WRITE(IDEBUG,*) 'POROSH         = ',POROSH
      WRITE(IDEBUG,*) 'POROSL         = ',POROSL
      WRITE(IDEBUG,*) 'TEMP           = ',TEMP
      WRITE(IDEBUG,*) 'VZ             = ',VZ
      WRITE(IDEBUG,*) 'FM             = ',FM
      WRITE(IDEBUG,*) 'FRF            = ',FRF
      WRITE(IDEBUG,*) 'AA             = ',AA
      WRITE(IDEBUG,*) 'AC             = ',AC
      WRITE(IDEBUG,*) 'AN             = ',AN
      WRITE(IDEBUG,*) 'AP             = ',AP
      WRITE(IDEBUG,*) 'CAC_O          = ',CAC_O
      WRITE(IDEBUG,*) 'CAC_R          = ',CAC_R
      WRITE(IDEBUG,*) 'CNC            = ',CNC
      WRITE(IDEBUG,*) 'COPS           = ',COPS
      WRITE(IDEBUG,*) 'CSS            = ',CSS
      WRITE(IDEBUG,*) 'DM             = ',DM
      WRITE(IDEBUG,*) 'KCH20          = ',KCH20
      WRITE(IDEBUG,*) 'KCL20          = ',KCL20
      WRITE(IDEBUG,*) 'KD20           = ',KD20
      WRITE(IDEBUG,*) 'KN20           = ',KN20
      WRITE(IDEBUG,*) 'KO20           = ',KO20
      WRITE(IDEBUG,*) 'KP20           = ',KP20
      WRITE(IDEBUG,*) 'KS20           = ',KS20
      WRITE(IDEBUG,*) 'KSP20          = ',KSP20
      WRITE(IDEBUG,*) 'KT_CH          = ',KT_CH
      WRITE(IDEBUG,*) 'KT_D           = ',KT_D
      WRITE(IDEBUG,*) 'KT_N           = ',KT_N
      WRITE(IDEBUG,*) 'KT_O           = ',KT_O
      WRITE(IDEBUG,*) 'KT_P           = ',KT_P
      WRITE(IDEBUG,*) 'KT_S           = ',KT_S
      WRITE(IDEBUG,*) 'KT_SP          = ',KT_SP
      WRITE(IDEBUG,*) 'D1PO4          = ',D1PO4
      WRITE(IDEBUG,*) 'CNH4KW         = ',CNH4KW
      WRITE(IDEBUG,*) 'CPO4KW         = ',CPO4KW
      WRITE(IDEBUG,*) 'CSO4KW         = ',CSO4KW
      WRITE(IDEBUG,*) 'BTHICK(1)      = ',BTHICK(1)
      WRITE(IDEBUG,*) 'BTHICK(2)      = ',BTHICK(2)
      WRITE(IDEBUG,*) 'BTHICK(3)      = ',BTHICK(3)
      WRITE(IDEBUG,*) 'BTHICK(4)      = ',BTHICK(4)
      WRITE(IDEBUG,*) 'FPP1           = ',FPP1
      WRITE(IDEBUG,*) 'FPP3           = ',FPP3
      WRITE(IDEBUG,*) 'FPP4           = ',FPP4
      WRITE(IDEBUG,*) 'FMP1           = ',FMP1
      WRITE(IDEBUG,*) 'FMP3           = ',FMP3
      WRITE(IDEBUG,*) 'FMP4           = ',FMP4
      WRITE(IDEBUG,*) 'CA1            = ',CA1
      WRITE(IDEBUG,*) 'CA2            = ',CA2
      WRITE(IDEBUG,*) 'CA4            = ',CA4
      WRITE(IDEBUG,*) 'CDET1          = ',CDET1
      WRITE(IDEBUG,*) 'CDET4          = ',CDET4
      WRITE(IDEBUG,*) 'CREF1          = ',CREF1
      WRITE(IDEBUG,*) 'CDTN1          = ',CDTN1
      WRITE(IDEBUG,*) 'CDTN4          = ',CDTN4
      WRITE(IDEBUG,*) 'CRFN1          = ',CRFN1
      WRITE(IDEBUG,*) 'CDTP1          = ',CDTP1
      WRITE(IDEBUG,*) 'CDTP4          = ',CDTP4
      WRITE(IDEBUG,*) 'CRFP1          = ',CRFP1
      WRITE(IDEBUG,*) 'CN1            = ',CN1
      WRITE(IDEBUG,*) 'CP1            = ',CP1
      WRITE(IDEBUG,*) 'CP3            = ',CP3
      WRITE(IDEBUG,*) 'CP4            = ',CP4
      WRITE(IDEBUG,*) 'CS1            = ',CS1
      WRITE(IDEBUG,*) 'CPP1           = ',CPP1
      WRITE(IDEBUG,*) 'CPP3           = ',CPP3
      WRITE(IDEBUG,*) 'CPP4           = ',CPP4
      WRITE(IDEBUG,*) 'CMP1           = ',CMP1
      WRITE(IDEBUG,*) 'CMP3           = ',CMP3
      WRITE(IDEBUG,*) 'CMP4           = ',CMP4
