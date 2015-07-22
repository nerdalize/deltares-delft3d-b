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

C     FILL THE OUTPUT ARRAY
      AROUT(LFOP1  ) = FOP1
      AROUT(LFOP3  ) = FOP3
      AROUT(LFOP4  ) = FOP4
      AROUT(LFPP1  ) = FPP1
      AROUT(LFPP3  ) = FPP3
      AROUT(LFPP4  ) = FPP4
      AROUT(LFMP1  ) = FMP1
      AROUT(LFMP3  ) = FMP3
      AROUT(LFMP4  ) = FMP4
      AROUT(LDTSW  ) = DTA
      AROUT(LBTH1  ) = BTHICK(1)
      AROUT(LBTH2  ) = BTHICK(2)
      AROUT(LBTH3  ) = BTHICK(3)
      AROUT(LBTH4  ) = BTHICK(4)
      AROUT(LCA1   ) = CA1
      AROUT(LCA2   ) = CA2
      AROUT(LCA4   ) = CA4
      AROUT(LCDET1 ) = CDET1
      AROUT(LCDET4 ) = CDET4
      AROUT(LCREF1 ) = CREF1
      AROUT(LCDTN1 ) = CDTN1
      AROUT(LCDTN4 ) = CDTN4
      AROUT(LCRFN1 ) = CRFN1
      AROUT(LCDTP1 ) = CDTP1
      AROUT(LCDTP4 ) = CDTP4
      AROUT(LCRFP1 ) = CRFP1
      AROUT(LCN1   ) = CN1
      AROUT(LCP1   ) = CP1
      AROUT(LCP3   ) = CP3
      AROUT(LCP4   ) = CP4
      AROUT(LCPP1  ) = CPP1
      AROUT(LCPP3  ) = CPP3
      AROUT(LCPP4  ) = CPP4
      AROUT(LCMP1  ) = CMP1
      AROUT(LCMP3  ) = CMP3
      AROUT(LCMP4  ) = CMP4
      AROUT(LCS1   ) = CS1

c     Additional output items by Jos van Gils 040800
c     Dissolved phosphorus concentrations
      AROUT(LCOP1  ) = FOP1 * CP1 / POROSH
      AROUT(LCOP3  ) = FOP3 * CP3 / POROSH
      AROUT(LCOP4  ) = FOP4 * CP4 / POROSL
c     Fluxes to and from water column
      AROUT(LSA0DI ) = FLX(MSA0DI)
      AROUT(LSA0Z  ) = FLX(MSA0Z )
      AROUT(LSN0DI ) = FLX(MSN0DI)
      AROUT(LSOXY  ) = FLX(MSOXY )
      AROUT(LSP0DI ) = FLX(MSP0DI)
      AROUT(LSP0RE ) = FLX(MSP0RE)
      AROUT(LSP0Z  ) = FLX(MSP0Z )
      AROUT(LSS0DI ) = FLX(MSS0DI)
      AROUT(LSS0Z  ) = FLX(MSS0Z )
