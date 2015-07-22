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

C     FILL THE VARIABLES FROM THE ARRAY
      BT     = ARIN(IBT     )
      BEDEK  = ARIN(IBEDEK  )
      CA0    = ARIN(ICA0    )
      CN0    = ARIN(ICN0    )
      CO0    = ARIN(ICO0    ) * ARIN(IFOX)
      CP0    = ARIN(ICP0    ) + ARIN(ICAAP0  ) + ARIN(ICPAP0)
      IF (CP0.GT.0.0) THEN
        FOP0   = ARIN(ICP0    ) / CP0
      ELSE
        FOP0 = 0.0
      ENDIF
      CS0    = ARIN(ICS0    )
      DB     = ARIN(IDB     )
      DIFFUS(1) = ARIN(IDIFNH4 )
      DIFFUS(2) = ARIN(IDIFNO3 )
      DIFFUS(3) = ARIN(IDIFOXY )
      DIFFUS(4) = ARIN(IDIFPO4 )
      DIFFUS(5) = ARIN(IDIFSIO )
      DS     = ARIN(IDS     )
      DT     = ARIN(IDT     )
      FAB    = ARIN(IFAB    ) + ARIN(IOFAB) - ARIN(IFMFBN) + ARIN(IDFAB)
      FDB1   = MAX(0.,ARIN(IFDB1   )) + MAX(0.,ARIN(IFDTBC))
      FDB4   = MAX(0.,ARIN(IFDB4   ))
      FDBN1  = MAX(0.,ARIN(IFDBN1  )) + MAX(0.,ARIN(IFDTBN))
      FDBN4  = MAX(0.,ARIN(IFDBN4  ))
      FDBP1  = MAX(0.,ARIN(IFDBP1  )) + MAX(0.,ARIN(IFDTBP))
      FDBP4  = MAX(0.,ARIN(IFDBP4  ))
      FNB    = ARIN(IFNB    )
      FOB    = (ARIN(IFOB    ) + ARIN(IOFOB)) * ARIN(IAC     )
      FPB    = ARIN(IFPB    ) + ARIN(IOFPB) - ARIN(IFMFBP) + ARIN(IDFPB)
      FPS    = ARIN(IFPS    ) + ARIN(IFPSA)
      FR     = ARIN(IFR     )
      FRO    = ARIN(IFRO    )
      FS     = ARIN(IFS     )
      FSB    = ARIN(IFSB    ) + ARIN(IOFSB) - ARIN(IFMFBS)
      LDISP   = ARIN(IDISPL  )
      POROSH = ARIN(IPORH   )
      POROSL = ARIN(IPORL   )
      TEMP   = ARIN(ITEMP   )
      IF (ARIN(IDW).GT.0.0) THEN
        VZ     = - ARIN(IDW     )
      ELSE
        VZ     = ARIN(IUW     )
      ENDIF
      FM     = ARIN(IFM     )
      FRF    = ARIN(IFRF    )
      AA     = ARIN(IAA     )
      AC     = ARIN(IAC     )
      AN     = ARIN(IAN     )
      AP     = ARIN(IAP     )
      CAC_O  = ARIN(ICAC_O  )
      CAC_R  = ARIN(ICAC_R  )
      CNC    = ARIN(ICNC    )
      COPS   = ARIN(ICOPS   )
      CSS    = ARIN(ICSS    )
      DM     = ARIN(IDM     )
      KCH20  = ARIN(IKCH20  )
      KCL20  = ARIN(IKCL20  )
      KD20   = ARIN(IKD20   )
      KN20   = ARIN(IKN20   )
      KO20   = ARIN(IKO20   )
      KP20   = ARIN(IKP20   )
      KS20   = ARIN(IKS20   )
      KSP20  = ARIN(IKSP20  )
      KT_CH  = ARIN(IKT_CH  )
      KT_D   = ARIN(IKT_D   )
      KT_N   = ARIN(IKT_N   )
      KT_O   = ARIN(IKT_O   )
      KT_P   = ARIN(IKT_P   )
      KT_S   = ARIN(IKT_S   )
      KT_SP  = ARIN(IKT_SP  )
      D1PO4  = ARIN(ID1PO4  )
      CNH4KW = ARIN(ICNH4KW )
      CPO4KW = ARIN(ICPO4KW )
      CSO4KW = ARIN(ICSO4KW )
C     SURF   = ARIN(ISURF   )
C     VOLUME = ARIN(IVOL    )
      BTHICK(1) = ARIN(IBTH1   )
      BTHICK(2) = ARIN(IBTH2   )
      BTHICK(3) = ARIN(IBTH3   )
      BTHICK(4) = ARIN(IBTH4   )
      DH     = BTHICK(1) + BTHICK(2) + BTHICK(3)
      FPP1   = MAX(MIN(ARIN(IFPP1   ),1.0),0.0)
      FPP3   = MAX(MIN(ARIN(IFPP3   ),1.0),0.0)
      FPP4   = MAX(MIN(ARIN(IFPP4   ),1.0),0.0)
      FMP1   = MAX(MIN(ARIN(IFMP1   ),1.0),0.0)
      FMP3   = MAX(MIN(ARIN(IFMP3   ),1.0),0.0)
      FMP4   = MAX(MIN(ARIN(IFMP4   ),1.0),0.0)
      CA1    = ARIN(ICA1    )
      CA2    = ARIN(ICA2    )
      CA4    = ARIN(ICA4    )
      CDET1  = ARIN(ICDET1  )
      CDET4  = ARIN(ICDET4  )
      CREF1  = ARIN(ICREF1  )
      CDTN1  = ARIN(ICDTN1  )
      CDTN4  = ARIN(ICDTN4  )
      CRFN1  = ARIN(ICRFN1  )
      CDTP1  = ARIN(ICDTP1  )
      CDTP4  = ARIN(ICDTP4  )
      CRFP1  = ARIN(ICRFP1  )
      CN1    = ARIN(ICN1    )
      CP1    = ARIN(ICP1    )
      CP3    = ARIN(ICP3    )
      CP4    = ARIN(ICP4    )
      CS1    = ARIN(ICS1    )

      CPP1 = FPP1 * CP1
      CPP3 = FPP3 * CP3
      CPP4 = FPP4 * CP4

      CMP1 = FMP1 * CP1
      CMP3 = FMP3 * CP3
      CMP4 = FMP4 * CP4
