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

C BT       iDiffusie factor bioturbatie (-)
C BEDEK    iBedekkingsgraad afsluitende complex-laag (-)
C CA0      iAmmoniumconcentratie in waterkolom [ g N/m3 ]
C CN0      iNitraatconcentratie in waterkolom [ g N/m3 ]
C CO0      iZuurstofconcentratie in waterkolom [ g O/m3 ]
C CP0      iTotaal fosfaat concentratie in waterkolom [ g O/m3 ]
C CPAP0    iParticulair fosfaat concentratie in waterkolom [ g O/m3 ]
C CAAP0    iParticulair fosfaat concentratie in waterkolom [ g O/m3 ]
C CS0      iSilicaat concentratie in waterkolom [ g Si/m3)
C DB       iBioturbatie dispersiecoefficient (m2/d)
C DH       iDikte bodemlaag met hoge porositeit ( = 1 + 2 + 3) [ m ]
C DIFNH4   iMoleculaire diffusiecoefficient [ m2/dag ]
C DIFNO3   iMoleculaire diffusiecoefficient [ m2/dag ]
C DIFOXY   iMoleculaire diffusiecoefficient [ m2/dag ]
C DIFPO4   iMoleculaire diffusiecoefficient [ m2/dag ]
C DIFSIO   iMoleculaire diffusiecoefficient [ m2/dag ]
C DS       iDichtheid bodemsediment (alle lagen) [ kg dw/m3 ]
C DT       iTijdstap in simulatie (d)
C FAB      iNetto ammoniumflux detrituslaag naar laag 1 [ g N/m2/dag
C OFAB     iNetto ammoniumflux OOC     laag naar laag 1 [ g N/m2/dag
C FDB1     iIncorporatie detritus uit detrituslaag [ g C/m2/dag ]
C FDB4     iIncorporatie OOC uit detrituslaag [ g C/m2/dag ]
C FDBN1    iIncorporatie detritus uit detrituslaag [ g N/m2/dag ]
C FDBN4    iIncorporatie OOC uit detrituslaag [ g N/m2/dag ]
C FDBP1    iIncorporatie detritus uit detrituslaag [ g P/m2/dag ]
C FDBP4    iIncorporatie OOC uit detrituslaag [ g P/m2/dag ]
C FNB      iNitraatflux vanuit detrituslaag [ g N/m2/dag ]
C FOB      iFlux netto zuurstofverbruik detrituslaag [ g O2/m2/dag ]
C OFOB     iFlux netto zuurstofverbruik OOC     laag [ g O2/m2/dag ]
C FPB      iNetto fosfaatflux van detrituslaag (naar laag 1) [ g P/m2/dag ]
C OFPB     iNetto fosfaatflux van OOC     laag (naar laag 1) [ g P/m2/dag ]
C FPS      iSedimentatieflux van geadsorbeerd fosfaat [ g P/m2/dag ]
C FR       iResuspensieflux gebaseerd op bodemvolume [ m3/m2/dag ]
C FRO      iFractie potentiele oxidatie [ - ]
C FS       iSedimentatieflux gebaseerd bodemvolume [ m3/m2/dag ]
C FSB      iNetto opgelost silicaatflux uit detrituslaag naar laag 1 t/m 4[ g
C OFSB     iNetto opgelost silicaatflux uit OOC     laag naar laag 1 t/m 4[ g
C DISPL    iMengweglengte in waterkolom [ m ]
C PORH     iPorositeit laag 1 t/m 3 [ m3 pw/m3 b ]
C PORL     iPorositeit laag 4 [ m3 pw/m3 b ]
C TEMP     iTemperatuur bodem = temp. waterkolom boven bodem [ C ]
C VZ       iWegzijging ( <0) of kwel ( >0) [ m/dag ]
C FM       iFractie naar vast geprecipiteerd fosfaat [ - ]
C FRF      iFractie refractair detritus [ - ]
C AA       iStiochiometrische constante voor stikstof in detritus [ g N/g C ]
C AC       iStiochiometrische constante [ g O2/g C ]
C AN       iStiochiometrische constante [ g O2/g N ]
C AP       iStiochiometrische constante voor fosfor in detritus [ g P/g C ]
C CAC_O    iAdsorptiecapaciteit voor fosfaat in geoxideerd sediment [ g P/
C CAC_R    iAdsorptiecapaciteit voor fosfaat in gereduceerd sediment [ g P (indien < 0 nwe formulering)
C CNC      iCritische nitraatconcentratie [ g N/m3 pw ]
C COPS     iVerzadigingsconcentratie opgelost fosfaat [ g P/m3 pw ]
C CSS      iVerzadigingsconcentratie opgelost silicaat [ g Si/m3 pw ]
C DM       iMinimale dikte aerobe bodemlaag [ m ]
C KCH20    iMineralisatiesnelheid in laag 1 t/m 3 bij 20 øC [ /dag ]
C KCL20    iMineralisatiesnelheid in laag 4 bij 20 øC [ /dag ]
C KD20     iDenitrificatiesnelheid bij 20 øC [ /dag ]
C KN20     iNitrificatiesnelheid bij 20 øC [ /dag ]
C KO20     iSnelheidsconstante voor oplossen van geprecip. fosfaat bij 20 øC [ /da
C KP20     iSnelheidsconstante voor precipitatie van fosfaat bij 20 øC [ /dag ]
C KS20     iOplossnelheid opaal silicaat bij 20 øC [ /dag ]
C KSP20    iHalve verzadigingsconcentratie fosfaat (alle lagen) bij 20 øC [ g P/
C KT_CH    iTemperatuur afhankelijkheid mineralisatiesnelheid [ /øC ]
C KT_D     iTemperatuur afhankelijkheid denitrificatiesnelheid [ /øC ]
C KT_N     iTemperatuur afhankelijkheid nitrificatiesnelheid [ /øC ]
C KT_O     iTemperatuur afhankelijkheid oplossen van fosfaat [ /øC ]
C KT_P     iTemperatuur afhankelijkheid precipitatie van fosfaat [ /øC ]
C KT_S     iTemperatuur afhankelijkheid oplossnelheid opaal silicaat [ /øC ]
C KT_SP    iTemperatuur afhankelijkheid verzadigingsconcentratie fosfaat [ øC
C D1PO4    iKritische aerobe laagdikte voor gereduceerde of geoxideerde partitie fosfaat (m)
C CNH4KW   iKwel randconcentratie NH4 [g/m3]
C CPO4KW   iKwel randconcentratie PO4 [g/m3]
C CSO4KW   iKwel randconcentratie SiO4 [g/m3]
C SURF     ioppervlakte (m2)
C VOLUME   ivolume (m3)
C BTH1     oDikte laag 1 [ m ]
C BTH2     oDikte laag 2 [ m ]
C BTH3     oDikte laag 3 [ m ]
C BTH4     oDikte laag 4 [ m ]
C FPP1     ofractie geprecipiteerd fosfaat (vivianiet) in laag 1 [ g P/m3 b ]
C FPP3     ofractie geprecipiteerd fosfaat (vivianiet) in laag 3 [ g P/m3 b ]
C FPP4     ofractie geprecipiteerd fosfaat (vivianiet) in laag 4 [ g P/m3 b ]
C FMP1     ofractie vast geprecipiteerd fosfaat in laag 1-2 [ g P/m3 b ]
C FMP3     ofractie vast geprecipiteerd fosfaat in laag 3 [ g P/m3 b ]
C FMP4     ofractie vast geprecipiteerd fosfaat in laag 4 [ g P/m3 b ]
C CA1      oAmmoniumconcentratie in laag 1 [ g N/m3 pw ]
C CA2      oAmmoniumconcentratie in laag 2 [ g N/m3 pw ]
C CA4      oAmmoniumconcentratie in laag 4 [ g N/m3 pw ]
C CDET1    oDetritusconcentratie in laag 1 ( t/m 3) [ g C/m3 b ]
C CDET4    oDetritusconcentratie in laag 4 [ g C/m3 b ]
C CREF1    oRefractaire detritusconcentratie in laag 1 ( t/m 3) [ g C/m3 b ]
C CDTN1    oDetritusconcentratie in laag 1 ( t/m 3) [ g N/m3 b ]
C CDTN4    oDetritusconcentratie in laag 4 [ g N/m3 b ]
C CRFN1    oRefractaire detritusconcentratie in laag 1 ( t/m 3) [ g N/m3 b ]
C CDTP1    oDetritusconcentratie in laag 1 ( t/m 3) [ g P/m3 b ]
C CDTP4    oDetritusconcentratie in laag 4 [ g P/m3 b ]
C CRFP1    oRefractaire detritusconcentratie in laag 1 ( t/m 3) [ g P/m3 b ]
C CDTS1    oDetritusconcentratie in laag 1 ( t/m 3) [ g Si/m3 b ]
C CN1      oNitraat concentratie in laag 1 [ g N/m3 pw ]
C CP1      oConcentratie totaal anorganisch fosfaat in laag 1 en 2 [ g P/m3 b ]
C CP3      oConcentratie totaal anorganisch fosfaat in laag 3 [ g P/m3 b ]
C CP4      oConcentratie totaal anorganisch fosfaat in laag 4 [ g P/m3 b ]
C CS1      oConcentratie opgelost silicaat in laag 1 t/m 4 [ g Si/m3 pw ]
C SZSW     iSediment zonering SWITCH (-)
C ISEGB    iSegment for detail mass balances output
C IBALFR   iFrequency for balance (nr of time steps)

      INTEGER IBT,IBEDEK,ICA0,ICN0,ICO0,ICP0,ICS0,IDB,IDIFNH4,
     +        IDIFNO3,IDIFOXY,IDIFPO4,IDIFSIO,IDS,IDT,IFAB,IFDB1,
     +        IFDB4,IFDBN1,IFDBN4,IFDBP1,IFDBP4,IFNB,
     +        IFOB,IFPB,IFPS,IFR,IFRO,IFS,IFSB,IDISPL,IPORH,IOFOB,
     +        IPORL,ITEMP,IDW,IUW,IBTH1,IBTH2,IBTH3,IBTH4,ICA1,IOFAB,
     +        ICA2,ICA4,ICDET1,ICDET4,ICREF1,ICDTN1,ICDTN4,ICRFN1,
     +        ICDTP1,ICDTP4,ICRFP1,ICN1,ICP1,ICP3,ICP4,IFPP1,
     +        IFPP3,IFPP4,IFMP1,IFMP3,IFMP4,ICS1,IFM,IFRF,IAA,IAC,IAN,
     +        IAP,ICAC_O,ICAC_R,ICNC,ICOPS,ICSS,IDM,IKCH20,IKCL20,IKD20,
     +        IKN20,IKO20,IKP20,IKS20,IKSP20,IKT_CH,IKT_D,IKT_N,IKT_O,
     +        IKT_P,IKT_S,IKT_SP,ID1PO4,ICNH4KW,ICPO4KW,ICSO4KW,INLAST,
     +        ISURF,IVOL,ICAAP0,IOFPB,IOFSB,ISZSW,NOSTAT,NOSUBS,ICPAP0,
     +        IFPSA,IFDTBC,IFMFBN,IFMFBP,IFMFBS,IFDTBN,IFDTBP,IFOX,
     +        IDFAB,IDFPB, IISEGB, IIBALF

      PARAMETER ( IISEGB  = 1          ,
     +            IIBALF  = IISEGB  + 1,
     +            ICA0    = IIBALF  + 1,
     +            ICN0    = ICA0    + 1,
     +            ICO0    = ICN0    + 1,
     +            ICP0    = ICO0    + 1,
     +            ICS0    = ICP0    + 1,
     +            ICPAP0  = ICS0    + 1,
     +            ICAAP0  = ICPAP0  + 1,
     +            IBT     = ICAAP0  + 1,
     +            IBEDEK  = IBT     + 1,
     +            IDB     = IBEDEK  + 1,
     +            IDIFNH4 = IDB     + 1)
      PARAMETER ( IDIFNO3 = IDIFNH4 + 1,
     +            IDIFOXY = IDIFNO3 + 1,
     +            IDIFPO4 = IDIFOXY + 1,
     +            IDIFSIO = IDIFPO4 + 1,
     +            IDS     = IDIFSIO + 1,
     +            IDT     = IDS     + 1,
     +            IFAB    = IDT     + 1,
     +            IOFAB   = IFAB    + 1,
     +            IDFAB   = IOFAB   + 1,
     +            IFDB1   = IDFAB   + 1,
     +            IFDB4   = IFDB1   + 1,
     +            IFDBN1  = IFDB4   + 1)
      PARAMETER ( IFDBN4  = IFDBN1  + 1,
     +            IFDBP1  = IFDBN4  + 1,
     +            IFDBP4  = IFDBP1  + 1,
     +            IFDTBC  = IFDBP4  + 1,
     +            IFDTBN  = IFDTBC  + 1,
     +            IFDTBP  = IFDTBN  + 1,
     +            IFMFBN  = IFDTBP  + 1,
     +            IFMFBP  = IFMFBN  + 1,
     +            IFMFBS  = IFMFBP  + 1,
     +            IFNB    = IFMFBS  + 1,
     +            IFOB    = IFNB    + 1,
     +            IOFOB   = IFOB    + 1,
     +            IFPB    = IOFOB   + 1,
     +            IOFPB   = IFPB    + 1,
     +            IDFPB   = IOFPB   + 1,
     +            IFPSA   = IDFPB   + 1,
     +            IFPS    = IFPSA   + 1)
      PARAMETER ( IFSB    = IFPS    + 1,
     +            IOFSB   = IFSB    + 1,
     +            IFR     = IOFSB   + 1,
     +            IFRO    = IFR     + 1,
     +            IFS     = IFRO    + 1,
     +            IDISPL  = IFS     + 1,
     +            IPORH   = IDISPL  + 1,
     +            IPORL   = IPORH   + 1,
     +            ITEMP   = IPORL   + 1,
     +            IDW     = ITEMP   + 1,
     +            IUW     = IDW     + 1,
     +            IFM     = IUW     + 1)
      PARAMETER ( IFRF    = IFM     + 1,
     +            IFOX    = IFRF    + 1,
     +            IAA     = IFOX    + 1,
     +            IAC     = IAA     + 1,
     +            IAN     = IAC     + 1,
     +            IAP     = IAN     + 1,
     +            ICAC_O  = IAP     + 1,
     +            ICAC_R  = ICAC_O  + 1,
     +            ICNC    = ICAC_R  + 1,
     +            ICOPS   = ICNC    + 1,
     +            ICSS    = ICOPS   + 1)
      PARAMETER ( IDM     = ICSS    + 1,
     +            IKCH20  = IDM     + 1,
     +            IKCL20  = IKCH20  + 1,
     +            IKD20   = IKCL20  + 1,
     +            IKN20   = IKD20   + 1,
     +            IKO20   = IKN20   + 1,
     +            IKP20   = IKO20   + 1,
     +            IKS20   = IKP20   + 1,
     +            IKSP20  = IKS20   + 1,
     +            IKT_CH  = IKSP20  + 1)
      PARAMETER ( IKT_D   = IKT_CH  + 1,
     +            IKT_N   = IKT_D   + 1,
     +            IKT_O   = IKT_N   + 1,
     +            IKT_P   = IKT_O   + 1,
     +            IKT_S   = IKT_P   + 1,
     +            IKT_SP  = IKT_S   + 1,
     +            ID1PO4  = IKT_SP  + 1,
     +            ICNH4KW = ID1PO4  + 1,
     +            ICPO4KW = ICNH4KW + 1,
     +            ICSO4KW = ICPO4KW + 1)
      PARAMETER ( ISURF   = ICSO4KW + 1,
     +            IVOL    = ISURF   + 1,
     +            IBTH4   = IVOL    + 1,
     +            ISZSW   = IBTH4   + 1,
     +            IBTH1   = ISZSW   + 1,
     +            IBTH2   = IBTH1   + 1,
     +            IBTH3   = IBTH2   + 1,
     +            IFPP1   = IBTH3   + 1,
     +            IFPP3   = IFPP1   + 1,
     +            IFPP4   = IFPP3   + 1,
     +            IFMP1   = IFPP4   + 1)
      PARAMETER ( IFMP3   = IFMP1   + 1,
     +            IFMP4   = IFMP3   + 1,
     +            ICA1    = IFMP4   + 1,
     +            ICA2    = ICA1    + 1,
     +            ICA4    = ICA2    + 1,
     +            ICDET1  = ICA4    + 1,
     +            ICDET4  = ICDET1  + 1,
     +            ICREF1  = ICDET4  + 1,
     +            ICDTN1  = ICREF1  + 1,
     +            ICDTN4  = ICDTN1  + 1)
      PARAMETER ( ICRFN1  = ICDTN4  + 1,
     +            ICDTP1  = ICRFN1  + 1,
     +            ICDTP4  = ICDTP1  + 1,
     +            ICRFP1  = ICDTP4  + 1,
     +            ICN1    = ICRFP1  + 1,
     +            ICP1    = ICN1    + 1,
     +            ICP3    = ICP1    + 1,
     +            ICP4    = ICP3    + 1,
     +            ICS1    = ICP4    + 1,
     +            INLAST  = ICS1       )

      PARAMETER ( NOSTAT = IFMP4 - IBTH1 + 1,
     +            NOSUBS = ICS1  - ICA1  + 1)
