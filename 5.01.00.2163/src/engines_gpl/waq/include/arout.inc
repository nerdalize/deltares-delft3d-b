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

C FOP1     uFractie opgelost fosfaat in laag 1 [ - ]
C FOP3     uFractie opgelost fosfaat in laag 3 [ - ]
C FOP4     uFractie opgelost fosfaat in laag 4 [ - ]
C FPP1     uFractie geprecipiteerd fosfaat (vivianiet) in laag 1 [ - ]
C FPP3     uFractie geprecipiteerd fosfaat (vivianiet) in laag 3 [ - ]
C FPP4     uFractie geprecipiteerd fosfaat (vivianiet) in laag 4 [ - ]
C FMP1     uFractie vast geprecipiteerd fosfaat in laag 1-2 [ - ]
C FMP3     uFractie vast geprecipiteerd fosfaat in laag 3 [ - ]
C FMP4     uFractie vast geprecipiteerd fosfaat in laag 4 [ - ]
C DTSW     uTijdstap SWITCH (d)
C BTHCK1   oDikte laag 1 [ m ]
C BTHCK2   oDikte laag 2 [ m ]
C BTHCK3   oDikte laag 3 [ m ]
C BTHCK4   oDikte laag 4 [ m ]
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
C CPP1     oConcentratie geprecipiteerd fosfaat (vivianiet) in laag 1 [ g P/m3 b ]
C CPP3     oConcentratie geprecipiteerd fosfaat (vivianiet) in laag 3 [ g P/m3 b ]
C CPP4     oConcentratie geprecipiteerd fosfaat (vivianiet) in laag 4 [ g P/m3 b ]
C CMP1     oConcentratie vast geprecipiteerd fosfaat in laag 1-2 [ g P/m3 b ]
C CMP3     oConcentratie vast geprecipiteerd fosfaat in laag 3 [ g P/m3 b ]
C CMP4     oConcentratie vast geprecipiteerd fosfaat in laag 4 [ g P/m3 b ]
C CS1      oConcentratie opgelost silicaat in laag 1 t/m 4 [ g Si/m3 pw ]

      INTEGER LFOP1,LFOP3,LFOP4,LFPP1,LFPP3,LFPP4,LFMP1,LFMP3,LFMP4,
     +        LDTSW,LBTH1,LBTH2,LBTH3,LBTH4,LCA1,LCA2,LCA4,
     +        LCDET1,LCDET4,LCREF1,LCDTN1,LCDTN4,LCRFN1,LCDTP1,LCDTP4,
     +        LCRFP1,LCN1,LCP1,LCP3,LCP4,LCPP1,LCPP3,LCPP4,LCMP1,
     +        LCMP3,LCMP4,LCS1,LOLAST,
     +        LCOP1,LCOP3,LCOP4,LSA0DI,LSA0Z,LSN0DI,LSOXY,LSP0DI,LSP0RE,
     +        LSP0Z,LSS0DI,LSS0Z

      PARAMETER ( LBTH1   =  1             ,
     +            LBTH2   =  LBTH1    + 1  ,
     +            LBTH3   =  LBTH2    + 1  ,
     +            LFPP1   =  LBTH3    + 1  ,
     +            LFPP3   =  LFPP1    + 1  ,
     +            LFPP4   =  LFPP3    + 1  ,
     +            LFMP1   =  LFPP4    + 1  ,
     +            LFMP3   =  LFMP1    + 1  ,
     +            LFMP4   =  LFMP3    + 1  )
      PARAMETER ( LCA1    =  LFMP4    + 1  ,
     +            LCA2    =  LCA1     + 1  ,
     +            LCA4    =  LCA2     + 1  ,
     +            LCDET1  =  LCA4     + 1  ,
     +            LCDET4  =  LCDET1   + 1  ,
     +            LCREF1  =  LCDET4   + 1  ,
     +            LCDTN1  =  LCREF1   + 1  ,
     +            LCDTN4  =  LCDTN1   + 1  ,
     +            LCRFN1  =  LCDTN4   + 1  ,
     +            LCDTP1  =  LCRFN1   + 1  )
      PARAMETER ( LCDTP4  =  LCDTP1   + 1  ,
     +            LCRFP1  =  LCDTP4   + 1  ,
     +            LCN1    =  LCRFP1   + 1  ,
     +            LCP1    =  LCN1     + 1  ,
     +            LCP3    =  LCP1     + 1  ,
     +            LCP4    =  LCP3     + 1  ,
     +            LCS1    =  LCP4     + 1  )
      PARAMETER ( LDTSW   =  LCS1     + 1  ,
     +            LFOP1   =  LDTSW    + 1  ,
     +            LFOP3   =  LFOP1    + 1  ,
     +            LFOP4   =  LFOP3    + 1  ,
     +            LBTH4   =  LFOP4    + 1  )
      PARAMETER ( LCPP1   =  LBTH4    + 1  ,
     +            LCPP3   =  LCPP1    + 1  ,
     +            LCPP4   =  LCPP3    + 1  ,
     +            LCMP1   =  LCPP4    + 1  ,
     +            LCMP3   =  LCMP1    + 1  ,
     +            LCMP4   =  LCMP3    + 1  )
      PARAMETER ( LCOP1   =  LCMP4    + 1  ,
     +            LCOP3   =  LCOP1    + 1  ,
     +            LCOP4   =  LCOP3    + 1  )
      PARAMETER ( LSA0DI  =  LCOP4    + 1  ,
     +            LSA0Z   =  LSA0DI   + 1  ,
     +            LSN0DI  =  LSA0Z    + 1  ,
     +            LSOXY   =  LSN0DI   + 1  ,
     +            LSP0DI  =  LSOXY    + 1  ,
     +            LSP0RE  =  LSP0DI   + 1  ,
     +            LSP0Z   =  LSP0RE   + 1  ,
     +            LSS0DI  =  LSP0Z    + 1  ,
     +            LSS0Z   =  LSS0DI   + 1  )
      PARAMETER ( LOLAST  =  LSS0Z         )
