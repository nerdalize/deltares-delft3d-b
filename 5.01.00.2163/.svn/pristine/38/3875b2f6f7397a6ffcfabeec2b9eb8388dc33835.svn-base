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

C     MNITB      NITRI.BOT.     Nitrificatie bodem
C     MSA0DI     DISP.NH4.0     Dispersief NH4 uit bodem
C     MSA0Z      AUTO.NH4.0     Autonome flux uit bodem (kwel/wegzijging)
C     MSA1DI     DISP.NH4.1     Dispersief NH4 van bodemlaag 2 naar 1
C     MSA1Z      AUTO.NH4.1
C     MSA3DI     DISP.NH4.3     Dispersief NH4 van bodemlaag 4 naar 3
C     MSA3Z      AUTO.NH4.3
C     MSA4Z      AUTO.NH4.4
C     MSD1MI     MIN.BDETR.1    Mineralisatie bodem detr.C in laag 1 (Switch)
C     MSD2MI     MIN.BDETR.2    Mineralisatie bodem detr.C in laag 2 (Switch)
C     MSD3BI     BIO.DET.41    Bioturbatie bodemdetr.C over grens 3 (Switch)
C     MSD3MI     MIN.BDETR.3    Mineralisatie bodem detr.C in laag 3 (Switch)
C     MSD3SR     BUR.D.34      Netto burial detritus C over grens 3 (Switch)
C     MSD4MI     MIN.BDETR.4    Mineralisatie bodem detr.C in laag 4 (Switch)
C     MSD4RF     REF.BDETR.4    Refractair worden bodem detr.C in laag 4 (Switch)
C     MSD4SR     BUR.D.4UIT    Netto burial detritus C over grens 4 (Switch)
C     MSDN1M     MIN.BDET.N.1   Mineralisatie bodem detr.N in laag 1 (Switch)
C     MSDN2M     MIN.BDET.N.2   Mineralisatie bodem detr.N in laag 2 (Switch)
C     MSDN3B     BIO.DETN.41   Bioturbatie bodemdetr.N over grens 3 (Switch)
C     MSDN3M     MIN.BDET.N.3   Mineralisatie bodem detr.N in laag 3 (Switch)
C     MSDN3S     BUR.DN.34     Netto burial detritus N over grens 3 (Switch)
C     MSDN4M     MIN.BDET.N.4   Mineralisatie bodem detr.N in laag 4 (Switch)
C     MSDN4R     REF.BDET.N.4   Refractair worden bodem detr.N in laag 4 (Switch)
C     MSDN4S     BUR.DN.4UIT   Netto burial detritus N over grens 4 (Switch)
C     MSDP1M     MIN.BDET.P.1   Mineralisatie bodem detr.P in laag 1 (Switch)
C     MSDP2M     MIN.BDET.P.2   Mineralisatie bodem detr.P in laag 2 (Switch)
C     MSDP3B     BIO.DETP.41   Bioturbatie bodemdetr.P over grens 3 (Switch)
C     MSDP3M     MIN.BDET.P.3   Mineralisatie bodem detr.P in laag 3 (Switch)
C     MSDP3S     BUR.DP.34     Netto burial detritus P over grens 3 (Switch)
C     MSDP4M     MIN.BDET.P.4   Mineralisatie bodem detr.P in laag 4 (Switch)
C     MSDP4R     REF.BDET.P.4   Refractair worden bodem detr.P in laag 4 (Switch)
C     MSDP4S     BUR.DP.4UIT   Netto burial detritus P over grens 4 (Switch)
C     MSN0DI     DISP.NO3.0
C     MSN1DI     DISP.NO3.1     Dispersief NO3 van bodemlaag 2 naar 1
C     MSOXY      BOTTOM.OXY
C     MSP0DI     DISP.P.0
C     MSP0RE     RES.PHOSP.     Resuspensie part.anorg.fosfaat
C     MSP0Z      AUTO.PO4.0     Autonome flux uit bodem (kwel/wegzijging)
C     MSP2BI     BIO.PHOSP.2
C     MSP2DI     DISP.P.2       Dispersief P van bodemlaag 3 naar 2
C     MSP2SR     NET.BUR.P.2
C     MSP2Z      AUTO.PO4.2     Autonome flux uit bodem (kwel/wegzijging)
C     MSP3BI     BIO.PHOSP.3
C     MSP3DI     DISP.P.3       Dispersief P van bodemlaag 4 naar 3
C     MSP3SR     NET.BUR.P.3    Netto begraving P van bodemlaag 3 naar 4
C     MSP3Z      AUTO.PO4.3     Autonome flux uit bodem (kwel/wegzijging)
C     MSP4SR     NET.BUR.P.4    Netto begraving P van bodemlaag 4 naar buiten
C     MSP4Z      AUTO.PO4.4     Autonome flux uit bodem (kwel/wegzijging)
C     MSPP2L     P.LAAGDIKTE    Flux door veranderen laagdikte (Switch)
C     MSPP1O     PP.OPLOS.1     Oplossen gepre.fosfaat laag 1+2 (Switch)
C     MSPP3O     PP.OPLOS.3     Oplossen gepre.fosfaat laag 3 (Switch)
C     MSPP4O     PP.OPLOS.4     Oplossen gepre.fosfaat laag 4 (Switch)
C     MSS0DI     DISP.SI.0      Dispersief Si van bodem naar water
C     MSS0Z      AUTO.SIO4.0    Autonome flux uit bodem (kwel/wegzijging)
C     MSS1OP     DISS.SI        Oplossen Si in bodem
C     MSS4Z      AUTO.SIO4.4    Autonome flux uit bodem (kwel/wegzijging)

      INTEGER MSA0DI,MSA0Z,MSA1DI,MSA1Z,MSA3DI,MSA3Z,MSA4Z,MNITB,MSD1MI,
     +        MSD2MI,MSD3BI,MSD3MI,MSD3SR,MSD4MI,MSD4RF,MSD4SR,MSDN1M,
     +        MSDN2M,MSDN3B,MSDN3M,MSDN3S,MSDN4M,MSDN4R,MSDN4S,MSDP1M,
     +        MSDP2M,MSDP3B,MSDP3M,MSDP3S,MSDP4M,MSDP4R,MSDP4S,MSN0DI,
     +        MSP0RE,MSN1DI,MSOXY,MSP0DI,MSP0Z,MSP2BI,MSP2DI,MSP2SR,
     +        MSP2Z,MSP3BI,MSP3DI,MSP3SR,MSP3Z,MSP4SR,MSP4Z,MSPP2L,
     +        MSS0DI,MSS0Z,MSS1OP,MSS4Z,MFLAST

      PARAMETER ( MNITB  = 1           ,
     +            MSA0DI = MNITB   + 1 ,
     +            MSA0Z  = MSA0DI  + 1 ,
     +            MSA1DI = MSA0Z   + 1 ,
     +            MSA1Z  = MSA1DI  + 1 ,
     +            MSA3DI = MSA1Z   + 1 ,
     +            MSA3Z  = MSA3DI  + 1 ,
     +            MSA4Z  = MSA3Z   + 1 ,
     +            MSD1MI = MSA4Z   + 1 ,
     +            MSD2MI = MSD1MI  + 1 )
      PARAMETER ( MSD3BI = MSD2MI  + 1 ,
     +            MSD3MI = MSD3BI  + 1 ,
     +            MSD3SR = MSD3MI  + 1 ,
     +            MSD4MI = MSD3SR  + 1 ,
     +            MSD4RF = MSD4MI  + 1 ,
     +            MSD4SR = MSD4RF  + 1 ,
     +            MSDN1M = MSD4SR  + 1 ,
     +            MSDN2M = MSDN1M  + 1 ,
     +            MSDN3B = MSDN2M  + 1 ,
     +            MSDN3M = MSDN3B  + 1 )
      PARAMETER ( MSDN3S = MSDN3M  + 1 ,
     +            MSDN4M = MSDN3S  + 1 ,
     +            MSDN4R = MSDN4M  + 1 ,
     +            MSDN4S = MSDN4R  + 1 ,
     +            MSDP1M = MSDN4S  + 1 ,
     +            MSDP2M = MSDP1M  + 1 ,
     +            MSDP3B = MSDP2M  + 1 ,
     +            MSDP3M = MSDP3B  + 1 ,
     +            MSDP3S = MSDP3M  + 1 ,
     +            MSDP4M = MSDP3S  + 1 )
      PARAMETER ( MSDP4R = MSDP4M  + 1 ,
     +            MSDP4S = MSDP4R  + 1 ,
     +            MSN0DI = MSDP4S  + 1 ,
     +            MSN1DI = MSN0DI  + 1 ,
     +            MSOXY  = MSN1DI  + 1 ,
     +            MSP0DI = MSOXY   + 1 ,
     +            MSP0RE = MSP0DI  + 1 ,
     +            MSP0Z  = MSP0RE  + 1 ,
     +            MSP2BI = MSP0Z   + 1 ,
     +            MSP2DI = MSP2BI  + 1 )
      PARAMETER ( MSP2SR = MSP2DI  + 1 ,
     +            MSP2Z  = MSP2SR  + 1 ,
     +            MSP3BI = MSP2Z   + 1 ,
     +            MSP3DI = MSP3BI  + 1 ,
     +            MSP3SR = MSP3DI  + 1 ,
     +            MSP3Z  = MSP3SR  + 1 ,
     +            MSP4SR = MSP3Z   + 1 ,
     +            MSP4Z  = MSP4SR  + 1 ,
     +            MSPP2L = MSP4Z   + 1 ,
     +            MSS0DI = MSPP2L  + 1 )
      PARAMETER ( MSS0Z  = MSS0DI  + 1 ,
     +            MSS1OP = MSS0Z   + 1 ,
     +            MSS4Z  = MSS1OP  + 1 ,
     +            MFLAST = MSS4Z       )
