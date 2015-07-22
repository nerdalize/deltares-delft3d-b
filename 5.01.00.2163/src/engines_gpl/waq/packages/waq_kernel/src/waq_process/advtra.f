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

      subroutine advtra ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Advective transport, velocities and fluxes, of solids in sediment

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Upgrade DELWAQ-G
C     Author  : Jos van Gils
C     Date    : 011220             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     011220  Jos van Gils    Created from Restra and Burtra (v4.40)
C     021202  Jos van Gils    Column admin updated
C     021202  Jos van Gils    Seepage added (disabled!)
C
C***********************************************************************
C
C     Description of the module :
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

C     IMPLICIT REAL (A-H,J-Z)
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C Coll Struct 1  O    Structure with collection of bottom collumn info
C                  Contains:
C    type(BotColmn), pointer :: set(:)  ! array with info for all bottom collumns
C    integer                 :: maxsize ! maximum size of the current array
C    integer                 :: cursize ! filled up to this size
C BotColm Struct 1   O  Structure with bottom collumn info
C                  Contains:
C    integer :: fstwatsed  ! first water sediment exchange number
C    integer :: lstwatsed  ! last  water sediment exchange number
C    integer :: topsedsed  ! first within collumn exchange number
C    integer :: botsedsed  ! last exchange of collumn to deeper bnd
C
      USE BottomSet     !  Module with derived types and add function

c     type ( BotColmnColl ) :: Coll  <= is defined in the module

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

C     Locals

      INTEGER  IWA1  , IWA2  , ITOP  , IBOT  , IOFFSE, IQ    , ISEG  ,
     J         IKMRK1, IK
      REAL     TOTSED, SW    , VBUR  , DELT  , SEEP  , PORACT
      INTEGER  IPDM  , IPSFLX, IPZRES, IPVBUR, IPTAU , IPTCRR, IPSURF,
     J         IPDEPT, IPMDEP, IPACTH, IPMNTH, IPMXTH, IPSW  , IPDELT,
     J         IPRFLX, IPPRES, IPTFLX, IPBFLX, IPRVEL, IPSVEL, IPBVEL,
     J         IPRVOL, IPSVOL, IPBVOL, IPPORI, IPPORA,
     J         IPDVOL, IPDVEL, IPSEEP
      INTEGER  INDM  , INSFLX, INZRES, INVBUR, INTAU , INTCRR, INSURF,
     J         INDEPT, INMDEP, INACTH, INMNTH, INMXTH, INSW  , INDELT,
     J         INRFLX, INPRES, INTFLX, INBFLX, INRVEL, INSVEL, INBVEL,
     J         INRVOL, INSVOL, INBVOL, INPORI, INPORA,
     J         INDVOL, INDVEL, INSEEP

c     Include column structure
c     we define a double column structure, one for downward,
c     and one for upward transport

      CALL MAKKO2 ( IEXPNT , IKNMRK , NOQ1   , NOQ2   , NOQ3   ,
     +              NOQ4   )
C
      IPDM   = IPOINT( 1)
      IPSFLX = IPOINT( 2)
      IPZRES = IPOINT( 3)
      IPVBUR = IPOINT( 4)
      IPTAU  = IPOINT( 5)
      IPTCRR = IPOINT( 6)
      IPSURF = IPOINT( 7)
      IPDEPT = IPOINT( 8)
      IPMDEP = IPOINT( 9)
      IPACTH = IPOINT(10)
      IPMNTH = IPOINT(11)
      IPMXTH = IPOINT(12)
      IPSW   = IPOINT(13)
      IPDELT = IPOINT(14)
      IPPORA = IPOINT(15)
      IPPORI = IPOINT(16)
      IPSEEP = IPOINT(17)
      IPSWRE = IPOINT(18)
      IPALPH = IPOINT(19)
      IPCFLX = IPOINT(20)

      IPRFLX = IPOINT(21)
      IPPRES = IPOINT(22)
      IPTFLX = IPOINT(23)
      IPBFLX = IPOINT(24)

      IPRVEL = IPOINT(25)
      IPSVEL = IPOINT(26)
      IPBVEL = IPOINT(27)
      IPDVEL = IPOINT(28)
      IPRVOL = IPOINT(29)
      IPSVOL = IPOINT(30)
      IPBVOL = IPOINT(31)
      IPDVOL = IPOINT(32)

      INDM   = INCREM( 1)
      INSFLX = INCREM( 2)
      INZRES = INCREM( 3)
      INVBUR = INCREM( 4)
      INTAU  = INCREM( 5)
      INTCRR = INCREM( 6)
      INSURF = INCREM( 7)
      INDEPT = INCREM( 8)
      INMDEP = INCREM( 9)
      INACTH = INCREM(10)
      INMNTH = INCREM(11)
      INMXTH = INCREM(12)
      INSW   = INCREM(13)
      INDELT = INCREM(14)
      INPORA = INCREM(15)
      INPORI = INCREM(16)
      INSEEP = INCREM(17)
      INSWRE = INCREM(18)
      INALPH = INCREM(19)
      INCFLX = INCREM(20)

      INRFLX = INCREM(21)
      INPRES = INCREM(22)
      INTFLX = INCREM(23)
      INBFLX = INCREM(24)

      INRVEL = INCREM(25)
      INSVEL = INCREM(26)
      INBVEL = INCREM(27)
      INDVEL = INCREM(28)
      INRVOL = INCREM(29)
      INSVOL = INCREM(30)
      INBVOL = INCREM(31)
      INDVOL = INCREM(32)
C
      do ISEG = 1 , NOSEG

C     Zero output quantities on segment level

         CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
         IF (IKMRK1.EQ.3) THEN
            PMSA ( IPCFLX ) =  0.0
            PMSA ( IPRFLX ) =  0.0
            PMSA ( IPPRES ) =  0.0
            PMSA ( IPTFLX ) =  0.0
            PMSA ( IPBFLX ) =  0.0
         ENDIF

         IPCFLX= IPCFLX+INCFLX
         IPRFLX= IPRFLX+INRFLX
         IPPRES= IPPRES+INPRES
         IPTFLX= IPTFLX+INTFLX
         IPBFLX= IPBFLX+INBFLX
C
      enddo
C
c.....Zero output quantities on exchange level
      do IQ=1,NOQ1+NOQ2+NOQ3+NOQ4
         PMSA(IPRVEL) = 0.0
         PMSA(IPSVEL) = 0.0
         PMSA(IPBVEL) = 0.0
         PMSA(IPDVEL) = 0.0
         PMSA(IPRVOL) = 0.0
         PMSA(IPSVOL) = 0.0
         PMSA(IPBVOL) = 0.0
         PMSA(IPDVOL) = 0.0
         IPRVEL = IPRVEL + INRVEL
         IPSVEL = IPSVEL + INSVEL
         IPBVEL = IPBVEL + INBVEL
         IPDVEL = IPDVEL + INDVEL
         IPRVOL = IPRVOL + INRVOL
         IPSVOL = IPSVOL + INSVOL
         IPBVOL = IPBVOL + INBVOL
         IPDVOL = IPDVOL + INDVOL
      enddo

      IPCFLX = IPOINT(20)
      IPRFLX = IPOINT(21)
      IPPRES = IPOINT(22)
      IPTFLX = IPOINT(23)
      IPBFLX = IPOINT(24)

      IPRVEL = IPOINT(25)
      IPSVEL = IPOINT(26)
      IPBVEL = IPOINT(27)
      IPDVEL = IPOINT(28)
      IPRVOL = IPOINT(29)
      IPSVOL = IPOINT(30)
      IPBVOL = IPOINT(31)
      IPDVOL = IPOINT(32)

c.....Loop over kolommen
      DO 7000 IK = 1 , Coll%cursize

c         Select first column of exchanges for DOWNWARD advection

          IWA1 = Coll%set(IK)%fstwatsed
          IWA2 = Coll%set(IK)%lstwatsed
          ITOP = Coll%set(IK)%topsedsed
          IBOT = Coll%set(IK)%botsedsed

C         Take uniform quantities from top sediment layer

          ISEG = IEXPNT(1,ITOP)
          SW   = PMSA(IPSW  +(ISEG-1)*INSW  )
          VBUR = PMSA(IPVBUR+(ISEG-1)*INVBUR)
          DELT = PMSA(IPDELT+(ISEG-1)*INDELT)
          SEEP = PMSA(IPSEEP+(ISEG-1)*INSEEP)

c         Set sedimentation flux and associated volume velocities

          CALL SEDKOL ( IWA1  , IWA2  , IEXPNT, SW    , PMSA  ,
     J                  IPSFLX, INSFLX, IPDM  , INDM  ,
     J                  IPSVEL, INSVEL, IPSVOL, INSVOL, TOTSED)

c         Set burial fluxes and associated velocities
c             fixed layers: equal to sedimentation
c             variable layers: constant velocity if Th > MaxTh

          CALL BURKOL ( ITOP  , IBOT  , IEXPNT, SW    , PMSA  ,
     J                  IPACTH, INACTH, IPMXTH, INMXTH,
     J                  IPMNTH, INMNTH,
     J                  IPDM  , INDM  , IPBFLX, INBFLX, DELT  ,
     J                  IPBVEL, INBVEL, IPBVOL, INBVOL, VBUR  ,
     J                  IPPORA, INPORA, IPPORI, INPORI, TOTSED)

c         Apply digging (negative burial) to get to specified
c             porosity if layer is not dense enough

          CALL DIGKOL ( ITOP  , IBOT  , IEXPNT, DELT  ,
     J                  PMSA  , IPACTH, INACTH, IPDM  , INDM  ,
     J                  IPBFLX, INBFLX, IPBVEL, INBVEL, IPPORA,
     J                  INPORA, IPPORI, INPORI)

c         Add INFILTRATION (defined in m/d!)

          DO IQ = IWA1,IBOT

              IF ( SEEP .LT. 0.0 ) THEN
C                 Find upwind porosity!
                  ISEG = IEXPNT(1,IQ)
                  PORACT = PMSA (IPPORA+(ISEG-1)*INPORA)
                  PMSA(IPDVEL+(iq-1)*indvel) = -SEEP/86400./PORACT
                  PMSA(IPDVOL+(iq-1)*indvol) = -SEEP/86400.
              ENDIF
          ENDDO
c
c         Set resuspension fluxes and associated velocity
c         Select second column of exchanges for UPWARD advection

          IOFFSE = IBOT - (IWA1-1)
          IWA1 = Coll%set(IK)%fstwatsed+IOFFSE
          IWA2 = Coll%set(IK)%lstwatsed+IOFFSE
          ITOP = Coll%set(IK)%topsedsed+IOFFSE
          IBOT = Coll%set(IK)%botsedsed+IOFFSE
          CALL RESKOL ( IWA1  , IWA2  , IEXPNT, SW    , PMSA  ,
     J                  ITOP  , IBOT  , IPSURF, INSURF,
     J                  IPACTH, INACTH, IPMNTH, INMNTH,
     J                  IPDM  , INDM  , IPRFLX, INRFLX, DELT  ,
     J                  IPRVEL, INRVEL, IPRVOL, INRVOL,
     J                  IPZRES, INZRES, IPTAU , INTAU ,
     J                  IPDEPT, INDEPT, IPMDEP, INMDEP,
     J                  IPTCRR, INTCRR, IPTFLX, INTFLX,
     J                  IPPRES, INPRES, IPSWRE, INSWRE,
     J                  IPALPH, INALPH, IPCFLX, INCFLX)

c         Add seepage

          DO IQ = IWA1,IBOT

              IF ( SEEP .GT. 0.0 ) THEN
C                 Find upwind porosity!
                  IF ( IQ .EQ. IBOT ) THEN
                      ISEG = IEXPNT(1,IQ)
                  ELSE
                      ISEG = IEXPNT(2,IQ)
                  ENDIF
                  PORACT = PMSA (IPPORA+(ISEG-1)*INPORA)
                  PMSA(IPDVEL+(iq-1)*indvel) = -SEEP/86400./PORACT
                  PMSA(IPDVOL+(iq-1)*indvol) = -SEEP/86400.
              ENDIF
          ENDDO

 7000 CONTINUE

      RETURN
C
      END
      SUBROUTINE BURKOL ( ITOP  , IBOT  , IEXPNT, SW    , PMSA  ,
     J                    IPACTH, INACTH, IPMXTH, INMXTH,
     J                    IPMNTH, INMNTH,
     J                    IPDM  , INDM  , IPBFLX, INBFLX, DELT  ,
     J                    IPBVEL, INBVEL, IPBVOL, INBVOL, VBUR  ,
     J                    IPPORA, INPORA, IPPORI, INPORI, TOTSED)
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        SETS BURIAL FLUXES AND ASSOCIATED VELOCITIES
c             fixed layers: equal to sedimentation
c             variable layers: constant velocity if Th > MaxTh
C
C
C Name    T      L  I/O Description
C ----    ---    -   -  ------------
C ITOP    I*4    1   I  first exchange number sediment collumn
C IBOT    I*4    1   I  last  exchange number sediment collumn
C IEXPNT  I*4 4,NOQT I  exchange pointers
C SW      R*4    1   I  Switch, > 0.5 then computation changes volume
C PMSA    R*4    *  I/O real input output array whole process system
C IPACTH  I*4    1   I  Pointer in PMSA where actual layer thickness
C INACTH  I*4    1   I  Increment of IPACTH in the PMSA array
C IPMXTH  I*4    1   I  Pointer in PMSA where maximum layer thickness
C INMXTH  I*4    1   I  Increment of IPMXTH in the PMSA array
C IPMNTH  I*4    1   I  Pointer in PMSA where minimum layer thickmess
C INMNTH  I*4    1   I  Increment of IPMNTH in the PMSA array
C IPDM    I*4    1   I  Pointer in PMSA where dry matter mass starts
C INDM    I*4    1   I  Increment of IPDM   in the PMSA array
C IPBFLX  I*4    1   I  Pointer in PMSA where burial flows
C INBFLX  I*4    1   I  Increment of IPBFLX in the PMSA array
C DELT    R*4    1   O  Time step size
C IPBVEL  I*4    1   I  Pointer in PMSA where burial velocity
C INBVEL  I*4    1   I  Increment of IPBVEL in the PMSA array
C IPBVOL  I*4    1   I  Pointer in PMSA where burial volume
C INBVOL  I*4    1   I  Increment of IPBVOL in the PMSA array
C VBUR    R*4    1   I  Burial velocity
C IPPORA  I*4    1   I  Pointer in PMSA where actual porosity starts
C INPORA  I*4    1   I  Increment of IPPORA in the PMSA array
C IPPORI  I*4    1   I  Pointer in PMSA where given porosity starts
C INPORI  I*4    1   I  Increment of IPPORI in the PMSA array
C TOTSED  R*4    1   O  Summed sedimentation flux
C
C
      INTEGER    ITOP  , IBOT  ,
     J           IPACTH, INACTH, IPMXTH, INMXTH, IPMNTH, INMNTH,
     J           IPDM  , INDM  , IPBFLX, INBFLX,
     J           IPBVEL, INBVEL, IPBVOL, INBVOL,
     J           IPPORA, INPORA, IPPORI, INPORI
      REAL       TOTSED, SW    , VBUR  , DELT  , PMSA(*)
      INTEGER    IEXPNT( 4,* )

C     Local variables

      INTEGER    IQ    , IBODEM
      REAL       TOTBUR, ACTH  , MAXTH , DM    , MINTH ,
     J           PORACT, PORINP, CORBUR, MXRCOR
      PARAMETER (MXRCOR = 1.0)
CJVB  PARAMETER (MXRCOR = 0.5)

      CORBUR = 0.0
      DO 10 IQ = ITOP,IBOT

C         SW and VBUR are fixed per column
          IBODEM = IEXPNT(1,IQ)
          ACTH   = PMSA (IPACTH+(IBODEM-1)*INACTH)
          MAXTH  = PMSA (IPMXTH+(IBODEM-1)*INMXTH)
          MINTH  = PMSA (IPMNTH+(IBODEM-1)*INMNTH)
          DM     = PMSA (IPDM  +(IBODEM-1)*INDM  )
          PORACT = PMSA (IPPORA+(IBODEM-1)*INPORA)
          PORINP = PMSA (IPPORI+(IBODEM-1)*INPORI)

C         Compute burial

          IF ( SW .LT. 0.5 ) THEN

C ---     First option (fixed layer thickness)
C         burial flux equals sedimentation flux
              TOTBUR = TOTSED

          ELSE

C ---     Second option (variable layer maximum thickness)
C         burial flux if layer is too thick, velocity in m/d bulk
              TOTBUR = 0.0
              IF ( ACTH .GT. MINTH ) THEN
                  TOTBUR = VBUR*DM
              ENDIF
              IF ( ACTH .GT. MAXTH ) THEN
                  TOTBUR = TOTBUR + MAX ( 0.0 , (ACTH-MAXTH)*DM/DELT )
              ENDIF
          ENDIF

c         Correction for inhomogeneous porosity
c         formula tries to maintain PORINP
C         burial if layer is too dense (porosity too low)
C         the inverse case is treated as DIGGING (see RESKOL)
c         numerical parameter determines max.rel.corr. per time step

cjvb      CORBUR = 0.0
          IF ( PORINP .GT. 0.0001 ) then
c         IF ( PORACT .LT. PORINP-0.001 ) THEN
          IF ( PORACT .LT. PORINP ) THEN
cjvb          CORBUR = ACTH/DELT*DM*(PORINP-PORACT)/(1.0-PORACT)*MXRCOR
              CORBUR = CORBUR + ACTH/DELT*DM*(PORINP-PORACT)/(1.0-PORACT)*MXRCOR
          ENDIF
          ENDIF
c         write (78,*) 'iq ',iq
c         write (78,*) 'porinp ',porinp
c         write (78,*) 'poract ',poract
c         write (78,*) 'acth   ',acth
c         write (78,*) 'delt   ',delt
c         write (78,*) 'dm     ',dm
c         write (78,*) 'mxrcor ',mxrcor
c         write (78,*) 'corbur ',corbur

C         Flux from segment

          PMSA(IPBFLX+(IBODEM-1)*INBFLX) = TOTBUR + CORBUR

C         Velocity for use in TRASE2

          IF ( DM .GT. 1E-20 )
     J    PMSA(IPBVEL+(IQ-1)*INBVEL) = (TOTBUR+CORBUR)/DM/86400.
c         WRITE(78,*) ' SED_BUR ',IQ,TOTBUR/DM/86400.

c         Velocity for volume change, variable thickness ONLY,
C         correction flux for porosity EXCLUDED
          IF ( SW .GE. 0.5 ) THEN
              IF ( DM .GT. 1E-20 )
     J        PMSA(IPBVOL+(IQ-1)*INBVOL) = TOTBUR/DM/86400.
          ENDIF

   10 CONTINUE

      RETURN
      END

      SUBROUTINE RESKOL ( IWA1  , IWA2  , IEXPNT, SW    , PMSA  ,
     J                    ITOP  , IBOT  , IPSURF, INSURF,
     J                    IPACTH, INACTH, IPMNTH, INMNTH,
     J                    IPDM  , INDM  , IPRFLX, INRFLX, DELT  ,
     J                    IPRVEL, INRVEL, IPRVOL, INRVOL,
     J                    IPZRES, INZRES, IPTAU , INTAU ,
     J                    IPDEPT, INDEPT, IPMDEP, INMDEP,
     J                    IPTCRR, INTCRR, IPTFLX, INTFLX,
     J                    IPPRES, INPRES, IPSWRE, INSWRE,
     J                    IPALPH, INALPH, IPCFLX, INCFLX)
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
c        SETS RESUSPENSION FLUXES AND ASSOCIATED VELOCITIES
c             Operates on second column of exchanges
c             for the UPWARD advection
C
C
C Name    T      L  I/O Description
C ----    ---    -   -  ------------
C IWA1    I*4    1   I  first exchange number water bottom exchange
C IWA2    I*4    1   I  first exchange number water bottom exchange
C IEXPNT  I*4 4,NOQT I  exchange pointers
C SW      R*4    1   I  Switch, > 0.5 then computation changes volume
C PMSA    R*4    *  I/O real input output array whole process system
C ITOP    I*4    1   I  first exchange number sediment collumn
C IBOT    I*4    1   I  last  exchange number sediment collumn
C IPSURF  I*4    1   I  Pointer in PMSA where
C INSURF  I*4    1   I  Increment of IPSURF in the PMSA array
C IPACTH  I*4    1   I  Pointer in PMSA where actual layer thickness
C INACTH  I*4    1   I  Increment of IPACTH in the PMSA array
C IPMNTH  I*4    1   I  Pointer in PMSA where minimum layer thickmess
C INMNTH  I*4    1   I  Increment of IPMNTH in the PMSA array
C IPDM    I*4    1   I  Pointer in PMSA where dry matter mass starts
C INDM    I*4    1   I  Increment of IPDM   in the PMSA array
C IPRFLX  I*4    1   I  Pointer in PMSA where burial flows
C INRFLX  I*4    1   I  Increment of IPRFLX in the PMSA array
C DELT    R*4    1   O  Time step size
C IPRVEL  I*4    1   I  Pointer in PMSA where burial velocity
C INRVEL  I*4    1   I  Increment of IPRVEL in the PMSA array
C IPRVOL  I*4    1   I  Pointer in PMSA where burial volume
C INRVOL  I*4    1   I  Increment of IPRVOL in the PMSA array
C IPDEPT  I*4    1   I  Pointer in PMSA where
C INDEPT  I*4    1   I  Increment of IPDEPT in the PMSA array
C IPMDEP  I*4    1   I  Pointer in PMSA where
C INMDEP  I*4    1   I  Increment of IPMDEP in the PMSA array
C IPTCRR  I*4    1   I  Pointer in PMSA where
C INTCRR  I*4    1   I  Increment of IPTCRR in the PMSA array
C IPTFLX  I*4    1   I  Pointer in PMSA where
C INTFLX  I*4    1   I  Increment of IPTFLX in the PMSA array
C IPPREP  I*4    1   I  Pointer in PMSA where
C INPRES  I*4    1   I  Increment of IPPRES in the PMSA array
C IPSWRE  I*4    1   I  Pointer in PMSA for swith for resuspension
C INSWRE  I*4    1   I  Increment of IPPRES in the PMSA array
C IPALPH  I*4    1   I  Pointer in PMSA for alpha in Rayleigh distribution
C INALPH  I*4    1   I  Increment of IPPRES in the PMSA array
C IPCFLX  I*4    1   I  Pointer in PMSA for communication TDM flux
C INCFLX  I*4    1   I  Increment of IPPRES in the PMSA array
C
C
      INTEGER    IWA1  , IWA2  , IPZRES, INZRES, IPTAU , INTAU ,
     J           IPDEPT, INDEPT, IPMDEP, INMDEP, ITOP  , IBOT  , IPACTH,
     J           INACTH, IPMNTH, INMNTH, IPTCRR, INTCRR, IPDM  , INDM  ,
     J           IPSURF, INSURF, IPPRES, INPRES, IPRFLX, INRFLX,
     J           IPRVEL, INRVEL, IPTFLX, INTFLX, IPRVOL, INRVOL,
     J           IPSWRE, INSWRE, IPALPH, INALPH, IPCFLX, INCFLX
      INTEGER    IEXPNT( 4,* )
      REAL       PMSA(*), DELT , SW

C     Local variables

      INTEGER    IWATER, IQ    , IQ2   , IBODEM, IBODE2, IQ3
      LOGICAL    GONE
      REAL       ZRES  , DEPTH , MINDEP, TAU   , ACTH  , MINTH ,
     J           FLRES , PRES  , TCRR  , RESTH , RESMX , SURFW ,
     J           SURFB , ZRESLE, VELRES, DM    , DMTOP
      LOGICAL    SW_PARTHENIADES

C     Resuspension submodel for DELWAQ-G, equals Restra, bug fixed ZRESLE

c.....Loop over water_sediment_exchanges in kolom
c     note: more than 1 sediment water exchange may exist on top of
c           one colun of sediment layers

      DO 20 IQ = IWA1,IWA2

C         Water-sediment interface, set physical parameters

          IWATER  = IEXPNT(1,IQ)

          DEPTH   = PMSA(IPDEPT+(IWATER-1)*INDEPT)
          MINDEP  = PMSA(IPMDEP+(IWATER-1)*INMDEP)
          TAU     = PMSA(IPTAU +(IWATER-1)*INTAU )
          SURFW   = PMSA(IPSURF+(IWATER-1)*INSURF)

          IBODEM  = IEXPNT(2,IQ)

          DMTOP   = PMSA(IPDM  +(IBODEM-1)*INDM  )

C         Skip if resuspension is zero
C         IF ( ZRES .LE. 0.0 ) GOTO 20
C         No resuspensie if depth is below minimum value
          IF ( DEPTH .LT. MINDEP ) GOTO 20

C         Underlying sediment-sediment interfaces: ITOP,IBOT
C         Compute actual resuspension per layer
C         GONE is TRUE as long as the layer right above is eroded away
C         ZRESLE is part of resuspension flux left over after eroding the
c         layer(s) above

          TIME_LEFT = 1.0
          GONE   = .TRUE.
          DO 10 IQ2 = ITOP,IBOT

              IBODEM  = IEXPNT(1,IQ2)
              DM      = PMSA(IPDM  +(IBODEM-1)*INDM  )
              TCRR    = PMSA(IPTCRR+(IBODEM-1)*INTCRR)
              SURFB   = PMSA(IPSURF+(IBODEM-1)*INSURF)
              ACTH    = PMSA(IPACTH+(IBODEM-1)*INACTH)
              MINTH   = PMSA(IPMNTH+(IBODEM-1)*INMNTH)
              ZRES    = PMSA(IPZRES+(IBODEM-1)*INZRES)
              SW_PARTHENIADES = NINT(PMSA(IPSWRE+(IBODEM-1)*INSWRE)) .EQ. 0
              ALPHA   = PMSA(IPALPH+(IBODEM-1)*INALPH)

              IF ( ACTH .LE. MINTH ) THEN
c                 Layer was already gone
                  FLRES  = 0.0
                  GONE = .TRUE.
              ELSE
C                 Layer (still) exists, compute resuspension probability
                  IF (TAU .EQ. -1.0) THEN
                      PRES = 1.0
                  ELSE
                      IF ( SW_PARTHENIADES ) THEN
                          PRES = MAX ( 0.0, (TAU/TCRR - 1.0) )
                      ELSE
                          IF ( TAU .LT. 1.E-10 ) THEN
                              PRES = 0.0
                          ELSE
                              PRES = EXP(-ALPHA*(TCRR/TAU)**2)
                          ENDIF
                      ENDIF
                  ENDIF
                  RESTH = PRES * ZRES * TIME_LEFT
                  RESMX = MAX (0.0, DM*(ACTH-MINTH)*TIME_LEFT/DELT )
                  IF ( RESTH .GT. 1E-20 .AND. RESTH .GE. RESMX ) THEN
                      FLRES = RESMX
                      GONE = .TRUE.
                      TIME_LEFT = TIME_LEFT*(1.-RESMX/RESTH)
                  ELSE
                      FLRES = RESTH
                      GONE = .FALSE.
                      TIME_LEFT = 0.0
                  ENDIF
              ENDIF

C             Resuspension probability (average for sediment column)
              PMSA(IPPRES+(IBODEM-1)*INPRES) =
     J        PMSA(IPPRES+(IBODEM-1)*INPRES) + PRES*SURFW/SURFB

              IF ( FLRES .GT. 1E-20 ) THEN

C                 Net resuspension flux (average for sediment column)
                  PMSA(IPRFLX+(IBODEM-1)*INRFLX) =
     J            PMSA(IPRFLX+(IBODEM-1)*INRFLX) + FLRES*SURFW/SURFB

C                 Resuspension velocity and volume change for
c                 sediment-water interface

                  VELRES = FLRES/DMTOP
                  PMSA(IPRVEL+(IQ-1)*INRVEL) =
     J            PMSA(IPRVEL+(IQ-1)*INRVEL) - VELRES/86400.
                  IF ( SW .GE. 0.5 ) THEN
                      PMSA(IPRVOL+(IQ-1)*INRVOL) =
     J                PMSA(IPRVOL+(IQ-1)*INRVOL) - VELRES/86400.
                  ENDIF

C                 Set total resuspension fluxes
c                 set associated velocities in sediment column
c                 Depends on fixed (SW = 0) or variable (SW = 1) layers

                  IF ( SW .GE. 0.5 ) THEN

C                     Variable layer thicknes: only layers on top are affected

C                     Total resuspension flux, and the communication flux which must be handled the same way
                      DO IQ3 = ITOP,IQ2
                          IBODE2  = IEXPNT(1,IQ3)
                          PMSA(IPTFLX+(IBODE2-1)*INTFLX) =
     J                    PMSA(IPTFLX+(IBODE2-1)*INTFLX) + FLRES
                          PMSA(IPCFLX+(IBODE2-1)*INCFLX) =
     J                    PMSA(IPCFLX+(IBODE2-1)*INCFLX) + FLRES
                      ENDDO

C                     Resuspension velocity and volume change
                      DO IQ3 = ITOP,IQ2-1
                          IBODE2  = IEXPNT(2,IQ3)
                          DM      = PMSA(IPDM  +(IBODE2-1)*INDM  )
                          VELRES  = FLRES/DM
                          PMSA(IPRVEL+(IQ3-1)*INRVEL) =
     J                    PMSA(IPRVEL+(IQ3-1)*INRVEL) - VELRES/86400.
                          PMSA(IPRVOL+(IQ3-1)*INRVOL) =
     J                    PMSA(IPRVOL+(IQ3-1)*INRVOL) - VELRES/86400.
                      ENDDO
                  ELSE

C                     Fixed layer thicknes: whole column is affected

C                     Total resuspension flux, and the communication flux which must be handled the same way
                      DO IQ3 = ITOP,IBOT
                          IBODE2  = IEXPNT(1,IQ3)
                          PMSA(IPTFLX+(IBODE2-1)*INTFLX) =
     J                    PMSA(IPTFLX+(IBODE2-1)*INTFLX) + FLRES
                          PMSA(IPCFLX+(IBODE2-1)*INCFLX) =
     J                    PMSA(IPCFLX+(IBODE2-1)*INCFLX) + FLRES
                      ENDDO

C                     Resuspension velocity, relate to density of lower layer,
c                     except at the deep sediment boundary!
                      DO IQ3 = ITOP,IBOT
                          IF ( IQ3 .EQ. IBOT ) THEN
                              IBODE2  = IEXPNT(1,IQ3)
                          ELSE
                              IBODE2  = IEXPNT(2,IQ3)
                          ENDIF
                          DM      = PMSA(IPDM  +(IBODE2-1)*INDM  )
                          VELRES  = FLRES/DM
                          PMSA(IPRVEL+(IQ3-1)*INRVEL) =
     J                    PMSA(IPRVEL+(IQ3-1)*INRVEL) - VELRES/86400.
                      ENDDO
                  ENDIF

              ENDIF

C             Stop als huidige laag niet helemaal weg is!
              IF ( .NOT. GONE ) GOTO 20

   10     CONTINUE
   20 CONTINUE

      RETURN
      END

      SUBROUTINE SEDKOL ( IQ1   , IQ2   , IEXPNT, SW    , PMSA  ,
     J                    IPSFLX, INSFLX, IPDM  , INDM  ,
     J                    IPSVEL, INSVEL, IPSVOL, INSVOL, TOTSED)
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        SETS SEDIMENTATION FLUX AND ASSOCIATED VOLUME VELOCITIES
C
C Name    T      L  I/O Description
C ----    ---    -   -  ------------
C IQ1     I*4    1   I  first exchange number water bottom exchange
C IQ2     I*4    1   I  last  exchange number water bottom exchange
C IEXPNT  I*4 4,NOQT I  exchange pointers
C SW      R*4    1   I  Switch, > 0.5 then computation changed volume
C PMSA    R*4    *  I/O real input output array whole process system
C IPSFLX  I*4    1   I  Pointer in PMSA where flows start
C INSFLX  I*4    1   I  Increment of IPSFLX in the PMSA array
C IPDM    I*4    1   I  Pointer in PMSA where dry matter mass starts
C INDM    I*4    1   I  Increment of IPDM   in the PMSA array
C IPSVEL  I*4    1   I  Pointer in PMSA where sedimentation velocity
C INSVEL  I*4    1   I  Increment of IPSVEL in the PMSA array
C IPSVOL  I*4    1   I  Pointer in PMSA where sedimentation volume
C INSVOL  I*4    1   I  Increment of IPSVOL in the PMSA array
C TOTSED  R*4    1   O  Summed sedimentation flux
C     NB IPSVEL and IPSVOL point in the EXCHANGE space
C        IPSFLX and IPDM   point in the SEGMENT  space
C
C
      INTEGER    IQ1   , IQ2
     J           IPSFLX, INSFLX, IPDM  , INDM  ,
     J           IPSVEL, INSVEL, IPSVOL, INSVOL
      INTEGER    IEXPNT( 4,* )
      REAL       SW    , TOTSED, PMSA(*)

C     Local variables

      INTEGER    IQ    , IVAN  , INAAR
      REAL       SEDFLX, DM    , DVOL

      TOTSED = 0.0
C     Loop over bodem-water uitwisselingen voor huidige kolom
      DO IQ = IQ1,IQ2
          IVAN  = IEXPNT(1,IQ)
          INAAR = IEXPNT(2,IQ)
C         Totale sedimentatieflux
          SEDFLX = PMSA(IPSFLX+(IVAN-1)*INSFLX)
          TOTSED = TOTSED + SEDFLX
C         Sedimentation velocity
C         Massa in upwind segment
          DM     = PMSA(IPDM+(IVAN-1)*INDM)
          DVOL   = 0.0
          IF (DM .GT. 0.0) DVOL = SEDFLX/DM/86400.
          PMSA (IPSVEL+(IQ-1)*INSVEL) = DVOL
c          WRITE(*,*) ' SED_VEL ',IQ,DVOL
          IF ( SW .GE. 0.5 ) THEN
C             Compute volume change velocity
C             Massa in DOWNwind segment!!!!!!!!!!!!!!!!!!!!!!
              DM     = PMSA(IPDM+(INAAR-1)*INDM)
              DVOL   = 0.0
              IF (DM .GT. 0.0) DVOL = SEDFLX/DM/86400.
              PMSA (IPSVOL+(IQ-1)*INSVOL) = DVOL
          ENDIF
      enddo

      RETURN
      END
C
      SUBROUTINE MAKKO2 ( IEXPNT , IKNMRK , NOQ1   , NOQ2   , NOQ3   ,
     +                    NOQ4   )
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        SETS COLUMN STRUCTURE OF SEDIMENT
C
C        Deze module bouwt de kolomstructuur:
C        -  boven elke kolom bodemsegmenten kunnen meerdere water
C           segmenten liggen (som AREAs watersegmenten moet gelijk
C           zijn aan AREA bodemsegmenten)
C        De pointertabel bevat in de vierde richting blokjes die
C        er als volgt uitzien:
C        watersegment A -> bodemsegment 1  \
C        watersegment B -> bodemsegment 1  | dit kan onbegrensd herhaald
C        .... etc                          /
C        bodemsegment 1 -> bodemsegment 2  \
C        bodemsegment 2 -> bodemsegment 3  | dit kan ook onbegrensd
C        .... etc                          | herhaald
C        bodemsegment N -> rand            /
C
C        Bodemsegmenten hebben een eerste kenmerk gelijk aan 2!
C
C
C Name    T      L  I/O Description
C ----    ---    -   -  ------------
C IEXPNT  I*4 4,NOQT I  exchange pointers for flows
C IKNMRK  I*4  NOSEG I  feature array
C NOQ's   I*4    1   I  number of exchanges NOQ4 is within bottom
C Coll    Struct 1   O  Structure with collection of bottom collumn info
C                  Contains:
C    type(BotColmn), pointer :: set(:)  ! array with info for all bottom collumns
C    integer                 :: maxsize ! maximum size of the current array
C    integer                 :: cursize ! filled up to this size
C BotColm Struct 1   O  Structure with bottom collumn info
C                  Contains:
C    integer :: fstwatsed  ! first water sediment exchange number
C    integer :: lstwatsed  ! last  water sediment exchange number
C    integer :: topsedsed  ! first within collumn exchange number
C    integer :: botsedsed  ! last exchange of collumn to deeper bnd
C
      USE BottomSet     !  Module with derived types and add function

c     type ( BotColmnColl ) :: Coll  <= is defined in the module
      type ( BotColmn )     :: set   !  makes code more readable

      INTEGER  IEXPNT(4,*), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

C     Local variables

      LOGICAL          KOLOM
      logical, save :: FIRST
      INTEGER  IK    , IQ    , ivan  , inaar , ikmrkv, ikmrkn, ik1   ,
     j         lenkol , nkolom
      DATA FIRST / .true. /

C     Check for bottom collumns anyway
      if ( NOQ4 .eq. 0 ) return

C     Check for first call
      if ( .NOT. FIRST ) return
      FIRST = .false.
      Coll%cursize = 0
      Coll%maxsize = 0

c.....Exchangeloop over de verticale richting

      KOLOM = .FALSE.
      DO 10 IQ = NOQ1+NOQ2+NOQ3+1 , NOQ1+NOQ2+NOQ3+NOQ4

         IVAN  = IEXPNT(1,IQ)
         INAAR = IEXPNT(2,IQ)

C        Illegal boundary

         IF ( IVAN  .LT. 0 ) GOTO 9004

C        Zoek eerste kenmerk van- en naar-segmenten

         IKMRKV = -1
         IF ( IVAN  .GT. 0 ) CALL DHKMRK(1,IKNMRK(IVAN ),IKMRKV)
         IKMRKN = -1
         IF ( INAAR .GT. 0 ) CALL DHKMRK(1,IKNMRK(INAAR),IKMRKN)

C        Bottom-water exchange, the collumn starts

         if (( IKMRKV.EQ.1 .AND. IKMRKN.EQ.3 )  .or.
     +       ( IKMRKV.EQ.0 .AND. IKMRKN.EQ.3 )) then
            if ( .NOT. KOLOM ) then   !  first detected
               KOLOM = .TRUE.
               IW1   = IQ
               IST   =  0
            endif
         endif

C        Bottom-bottom exchange, the collumn continues

         if ( IKMRKV.EQ.3. AND. IKMRKN.EQ.3 ) then
            if ( .NOT. KOLOM ) goto 9002
            if (  IW1 .LE. 0 ) goto 9003
            if (  IST .EQ. 0 ) then
               IW2 = IQ-1           !  previous was last water exch
               IST = IQ             !  this is the first bottom exch
            endif
         endif

C        Deep sediment boundary

         if ( INAAR .LT. 0 ) then
             if ( .NOT. KOLOM ) goto 9000
             KOLOM = .false.
             if ( IW1 .LT. 0 ) goto 9001
             if ( IST .LT. 0 ) then   ! this is true
                 IW2 = IQ-1           ! for 1 bottom layer
                 IST = IQ
             endif
             ISB = IQ                 ! last exchange of collumn
             nkolom = BotColmnCollAdd ( Coll , IW1 , IW2 , IST , ISB )
         endif

   10 CONTINUE

C     Check and remove copies

      if ( mod(nkolom,2) .ne. 0 ) goto 9005
      do ik = 1,nkolom,2
          set = Coll%set(ik)
          lenkol = set%botsedsed - (set%fstwatsed-1)
          do iq = set%fstwatsed , set%botsedsed
              if ( iexpnt(1,iq) .ne. iexpnt(1,iq+lenkol) ) goto 9006
              if ( iexpnt(2,iq) .ne. iexpnt(2,iq+lenkol) ) goto 9006
          enddo
          Coll%set((ik+1)/2) = set
      enddo
      Coll%cursize = nkolom/2
      RETURN
C
 9000 STOP 'MAKKOL(9000): Illegal structure of pointer table'
 9001 STOP 'MAKKOL(9001): Illegal structure of pointer table'
 9002 STOP 'MAKKOL(9002): Illegal structure of pointer table'
 9003 STOP 'MAKKOL(9003): Illegal structure of pointer table'
 9004 STOP 'MAKKOL(9004): Illegal structure of pointer table'
 9005 STOP 'MAKKOL(9005): Illegal structure of pointer table'
 9006 STOP 'MAKKOL(9006): Illegal structure of pointer table'
C
      END

      SUBROUTINE DIGKOL ( ITOP  , IBOT  , IEXPNT, DELT  ,
     +                    PMSA  , IPACTH, INACTH, IPDM  , INDM  ,
     +                    IPBFLX, INBFLX, IPBVEL, INBVEL, IPPORA,
     +                    INPORA, IPPORI, INPORI )
C
      INTEGER    ITOP  , IBOT  , IPACTH, INACTH,
     +           IPDM  , INDM  , IPBFLX, INBFLX,
     +           IPBVEL, INBVEL, IPPORA, INPORA,
     +           IPPORI, INPORI
      REAL       DELT  , PMSA(*)
      INTEGER    IEXPNT( 4,* )

C     Local variables

      INTEGER    IQ    , IBODEM
      REAL       ACTH  , DM    , PORACT, PORINP, CORDIG,
     +           MXRCOR
      PARAMETER (MXRCOR = 1.0)
CJVB  PARAMETER (MXRCOR = 0.5)

      CORDIG = 0.0
      DO IQ = ITOP,IBOT

          IBODEM = IEXPNT(1,IQ)
          ACTH   = PMSA (IPACTH+(IBODEM-1)*INACTH)
          DM     = PMSA (IPDM  +(IBODEM-1)*INDM  )
          PORACT = PMSA (IPPORA+(IBODEM-1)*INPORA)
          PORINP = PMSA (IPPORI+(IBODEM-1)*INPORI)
          PORACT = MIN(PORACT,0.999)

c         Correction for inhomogeneous porosity
c         formula tries to maintain PORINP
C         digging if layer is not dense enough (porosity too HIGH)
c         numerical parameter determines max.rel.corr. per time step

          IF ( PORINP .GT. 0.0001 ) THEN
c             IF ( PORACT .GT. PORINP+0.001 ) THEN
              IF ( PORACT .GT. PORINP ) THEN
                  CORDIG = CORDIG+ACTH/DELT*DM*(PORINP-PORACT)/(1.0-PORACT)*MXRCOR
              ENDIF
          ENDIF

C         Flux from segment

          PMSA(IPBFLX+(IBODEM-1)*INBFLX) = PMSA(IPBFLX+(IBODEM-1)*INBFLX) + CORDIG

C         Velocity for use in TRASE2

          IF ( DM .GT. 1E-20 ) PMSA(IPBVEL+(IQ-1)*INBVEL) = PMSA(IPBVEL+(IQ-1)*INBVEL) + CORDIG/DM/86400.

      ENDDO

      RETURN
      END
