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

c    23-12-97: Store computed biomass in BIOMAS array for use in D40BLO
C    Version 0.2 22 July 1994
C    Version 0.1 7 Januari 1994
C    Program:    BLPRIM.FOR
C    Programmer: Jos van Gils
C
C    Compute primary production and associated fluxes
C
C    Called by: BLOOMC
C    Calls    : BVECT , DYNRUN

      SUBROUTINE BLPRIM (BIOMAS, CNH4  , CNO3  , CPO4  , CSIO  , CDETN ,
     M                   CDETP ,
     J                   FLMORA, FLDETN, TSTEPI, EXTOT , EXALG , TEMP  ,
     J                   RAD   , DEPTH , DAYL  , ID    , LCOUPL, NSET  ,
     J                   DEAT4 , TOTNUT, CHLTOT, FLPRPA, FLUPTN, FACLIM,
     J                   UPTNIT, FRACAM, FBOD5 , RATGRO, RATMOR, ALGDM ,
     J                   ISEG  , CGROUP)
C
C     Arguments
C
C     Name    Type  Length   I/O  Description
C
C     BIOMAS  R*4   NUSPEC   I    Biomass (gC/m3)
C     CNH4    R*4   1        I    Concentration NH4 (gN/m3)
C     CNO3    R*4   1        I    Concentration NO3 (gN/m3)
C     CPO4    R*4   1        I    Concentration PO4 (gP/m3)
C     CSIO    R*4   1        I    Concentration SIO (gSi/m3)
C     FLMORA  R*4   NUSPEC   I/O  Mortality fluxes (gC/m3/d)
C     FLDETN  R*4   4        I/O  Detritus production (g/m3/d)
C     TSTEPI  R*4   1        I    Time step (d)
C     EXTOT   R*4   1        I    Total extinction (1/m)
C     EXALG   R*4   1        I    Extinction from living algae (1/m)
C     TEMP    R*4   1        I    Temperature (deg.C)
C     RAD     R*4   1        I    Radiation (J/cm2/week)
C     DEPTH   R*4   1        I    Depth (m)
C     DAYL    R*4   1        I    Length of light period (h)
C     ID      I     1        I    Week number (1 to 52)
C     LCOUPL  I     1        I    Coupling flag
C     NSET    I     1        I    Counter
C     DEAT4   R*4   1        I    ??
C     TOTNUT  R*4   4        O    Total C,N,P,Si in algae (g/m3)
C     CHLTOT  R*4   1        O    Total chlorophyl in algae (mgChl/m3)
C     FLPRPA  R*4   NUSPEC   O    Primary production fluxes (gC/m3/d)
C     FLUPTN  R*4   5        O    Uptake fluxes (g/m3/d)
C     FACLIM  R*4   6        O    Limiting factors (-)
C     UPTNIT  R*4   1        O    Nitrogen uptake per day
C     FRACAM  R*4   1        O    Fraction NH4 of N uptake
C     FBOD5   R*4   1        O    BOD5/BODinf in algae
C     RATGRO  R*4   NUECOG   O    Effective growth rate per group (1/day)
C     RATMOR  R*4   NUECOG   O    Effective mortality per group (1/day)
C     ALGDM   R*4   1        O    Dry matter in algae (gDM/m3)
C     ISEG    I     1        I    Segment number
C
      REAL            BIOMAS(*), CNH4, CNO3, CPO4, CSIO, RATGRO(*),
     J                FLMORA(*), FLDETN(*), TSTEPI, EXTOT, EXALG, TEMP,
     J                RAD, DEPTH, DAYL, DEAT4, TOTNUT(*), CHLTOT, FBOD5,
     J                FLPRPA(*), FLUPTN(*), FACLIM(*), UPTNIT, FRACAM,
     J                RATMOR(*), ALGDM, CGROUP(*)
      INTEGER         LCOUPL, NSET, ID, ISEG

C     Common block variables used
C
C     Name    Type  Length   I/O  Inc-file  Description
C
C     NUSPEC  I     1        I    phyt2     Number of types
C     NUECOG  I     1        I    phyt2     Number of groups
C     NUROWS  I     1        I    phyt2
C     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
C     CTODRY  R*8   MT       I    size      Conversion (gDW/gC)
C     NREP    I     1        I/O  phyt2     Counter
C     IT2     I     MS,2     I    phyt2     Administration of groups/types
C     BIOBAS  R*8   1        I    size      Base level per group (gDW/m3)
C     TOPLEV  R*8   1        I    size      Base level per type (gDW/m3)
C     CONCEN  R*8   3        I/O  phyt1     Available nutrients (g/m3)
C     IOU     I     99       I    ioblck    Logical unit numbers
C     ISDPER  I     2        I    sumout    First and last week selective dump
C     XDEF    R*8   MX+1     I    xvect     System vector of Bloom
C     LIMIT   C*18  1        I    sumout    Limiting factors
C     XINIT   R*8   MS       O    xvect     Initial biomass per group
C     TSTEP   R*8   1        O    dynam     Time step
C     RMORT   R*8   MT       I    size      Mortality rate (1/day)
C
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'xvect.inc'
      INCLUDE 'dynam.inc'
C
C     Local variables
C
C     Name    Type  Length   I/O  Description
C
C     CMORT   R*4   1             Additional mortality flux (gDW/m3/d)
C     X       R*8   MT            Remaining after mortality (gDW/m3)
C     XJ      R*8   1             Biomass per group (gDW/m3)
C     J       I     1
C     IHULP   I     1
C     IERROR  I     1             Present number of mass errors
C     MERROR  I     1             Maximum number of mass errors
C     EXTOT8  R*8   1             Real*8 version of input parameter
C     EXBAC8  R*8   1             ..
C     TEMP8   R*8   1             ..
C     RAD8    R*8   1             ..
C     DEPTH8  R*8   1             ..
C     DAYL8   R*8   1             ..
C     CHLOR8  R*8   1             ..
C     EXTLIM  R*8   1             ??
C     DEAT    R*8   1             ??
C     TOTCHL  R*8   1             Real*8 version of output parameter
C     TOTDRY  R*8   1             Real*8 version of output parameter
C     TOTCAR  R*8   1             Real*8 version of output parameter
C     UPTAKE  R*4   1             Nitrogen uptake (gN/m3/d)
C     I       I     1
C     CDETUP  R*4   1             Uptake of detritus C by heterotrophy
C     FRMIX   R*4   1             Fraction of mixotrophy in production

      REAL*8          EXTOT8, EXBAC8, TEMP8, RAD8, DAYL8, CHLOR8, XJ,
     J                DEPTH8, EXTLIM, DEAT, TOTCHL, TOTDRY, TOTCAR,
     J                X(MT), AUTO(3), FIXINF
      REAL            CMORT , UPTAKE, CDETUP, FRMIX
      INTEGER         IERROR, MERROR, J, I, IHULP
      PARAMETER      (MERROR = 100,FIXINF=1.0D+03)
      SAVE IERROR
      DATA IERROR /0/
C
C Increase BLOOM II indicator for the number of runs.
C
      NREP = NREP + 1
C
C  Transfer actual time step to Bloom (through common variable TSTEP)
C
      TSTEP = DBLE(TSTEPI)
C
C  Compute totals per species group (XINIT)
C
C  Do loop over groups

      DO 210 J=1,NUECOG
         XJ = 0D0

C Do loop over species within group

         DO 200 I = IT2(J,1), IT2(J,2)
 200     XJ = XJ + DBLE(BIOMAS(I)) * CTODRY(I)

C Fill XINIT, with lower limit BIOBAS

         IF (XJ .GT. BIOBAS) THEN
            XINIT(J) = XJ
         ELSE
            XINIT(J) = BIOBAS
         END IF
 210  CONTINUE
C
C  Compute available nutrients (CONCEN) using dissolved nutrients
C
      CONCEN (1) = DBLE(CNO3 + CNH4)
      CONCEN (2) = DBLE(CPO4)
      CONCEN (3) = DBLE(CSIO)
C     N-FIXATION
      IF (NUNUCO.EQ.4) CONCEN(4) = FIXINF
      IF (NUNUCO.EQ.6) CONCEN(6) = FIXINF
C     MIXOTROPHY
      IF (NUNUCO.GE.5) THEN
        CONCEN(4) = DBLE(CDETN)
        CONCEN(5) = DBLE(CDETP)
      ENDIF

      AUTO(1) = 0.D0
      AUTO(2) = 0.D0
      AUTO(3) = 0.D0
C
C Check whether biomass minus mortality is reasonably large.
C If the value has dropped below TOPLEV, then set the biomass to
C zero and correct the detritus fluxes.
C Otherwise add nutrients in live phytoplankton to CONCEN.
C
C Loop over algae species

      DO 250 J=1,NUSPEC

C Compute remaining biomass after mortality

         IF (BIOMAS (J) .LT. 0.0) THEN
            X (J) = BIOMAS (J)
            GO TO 245
         END IF
         X(J) = DBLE( BIOMAS(J) - TSTEPI * FLMORA(J) ) * CTODRY(J)
C
C  Add nutrients in autolyses to CONCEN.
C
C  Loop over nutrients
         DO 241 I=1,3
            AUTNUT  = DBLE( TSTEPI * FLMORA(J)) * CTODRY(J) *
     1                (1.D0 - AVAILN) * AA(I,J)

C           N-FIXATION AND/OR MIXOTROPHY
            IF ((NUNUCO.GE.4).AND.(I.EQ.1)) AUTNUT  = AUTNUT +
     1        DBLE(TSTEPI*FLMORA(J))*CTODRY(J) * (1.D0-AVAILN) * AA(4,J)
            IF ((NUNUCO.GE.5).AND.(I.EQ.2)) AUTNUT  = AUTNUT +
     1        DBLE(TSTEPI*FLMORA(J))*CTODRY(J) * (1.D0-AVAILN) * AA(5,J)
            IF ((NUNUCO.EQ.6).AND.(I.EQ.1)) AUTNUT  = AUTNUT +
     1        DBLE(TSTEPI*FLMORA(J))*CTODRY(J) * (1.D0-AVAILN) * AA(6,J)

            AUTO(I) = AUTO(I) + AUTNUT
            CONCEN(I) = CONCEN(I) + AUTNUT
  241    CONTINUE
C
C Negative biomass after mortality? Message!
C
         IF (X(J) .LT. -1.0D-2) THEN
            IERROR = IERROR + 1
            WRITE (IOU(61), 1050) IERROR,J,BIOMAS(J), ISEG, X(J)
            IF (IERROR .EQ. MERROR) GOTO 901
         END IF
 1050    FORMAT ( ' Integration error number ',I3,/,
     &            ' Current biomass of type ',I3,' = ',E15.5,/,
     &            ' in segment',I8,/,
     &            ' Remaining after mortality = ',E15.5,/,
     &            ' Serious error: time step is too big to model',
     &            ' mortality')

C No actions for neglectible biomasses

         IF (X(J) .LT. 1.0D-6) GO TO 245

         IF (X(J) .LT. TOPLEV) THEN
C
C  Set small biomasses to zero, putting mass into the detritus pools,
C  by increasing the mortality and detritus production fluxes
C
            CMORT = SNGL(X(J))/TSTEPI
            FLMORA(J) = FLMORA(J) + CMORT/SNGL(CTODRY(J))
            FLDETN(1) = FLDETN(1) + CMORT/SNGL(CTODRY(J))
            FLDETN(2) = FLDETN(2) + CMORT*SNGL(AA(1,J))
            FLDETN(3) = FLDETN(3) + CMORT*SNGL(AA(2,J))
            FLDETN(4) = FLDETN(4) + CMORT*SNGL(AA(3,J))
C           N-FIXATION AND/OR MIXOTROPHY
            IF (NUNUCO.GE.4) FLDETN(2) = FLDETN(2) + CMORT*SNGL(AA(4,J))
            IF (NUNUCO.GE.5) FLDETN(3) = FLDETN(3) + CMORT*SNGL(AA(5,J))
            IF (NUNUCO.EQ.6) FLDETN(2) = FLDETN(2) + CMORT*SNGL(AA(6,J))
            X(J) = 0.0D0
         ELSE
C
C  Add nutrients in live phytoplankton to CONCEN.
C
C  Loop over nutrients

            DO 240 I=1,3
  240       CONCEN(I) = CONCEN(I) + AA(I,J) * X(J)

C           N-FIXATION AND/OR MIXOTROPHY
            IF (NUNUCO.GE.4) CONCEN(1) = CONCEN(1) + AA(4,J) * X(J)
            IF (NUNUCO.GE.5) CONCEN(2) = CONCEN(2) + AA(5,J) * X(J)
            IF (NUNUCO.EQ.6) CONCEN(1) = CONCEN(1) + AA(6,J) * X(J)

C  End of conditional increase of available nutrients with algae content

         END IF
  245    CONTINUE

C  End of loop over algae species

  250 CONTINUE
C
C Call BVECT to set the mortality constraints.
C
      CALL BVECT (X, XDEF)
C
C  Call DYNRUN. This routine calls the actual BLOOM II module
C
      EXTOT8 = DBLE(EXTOT)
      EXBAC8 = DBLE(EXTOT-EXALG)
      TEMP8  = DBLE(TEMP)
      RAD8   = DBLE(RAD)
      DEPTH8 = DBLE(DEPTH)
      DAYL8  = DBLE(DAYL)
      CHLOR8 = -1D0
      EXTLIM = 0D0
      DEAT   = DBLE(DEAT4)

      CALL DYNRUN (EXTOT8, EXBAC8, TEMP8 , RAD8  , DEPTH8, DAYL8 ,
     J             CHLOR8, ID    , ISEG  , LCOUPL, NSET  , EXTLIM,
     J             DEAT  , TOTCHL, TOTDRY, TOTCAR)
C
C Store total carbon and chlorophyll concentration
C
      TOTNUT(1) = SNGL(TOTCAR)
      CHLTOT    = SNGL(TOTCHL)
      ALGDM     = SNGL(TOTDRY)
C
C Compute gross production: computed biomass by Bloom is in XDEF(NUROWS+J)

C Loop over algae species, compute production per species and total pr.pr.
C Added: computation of total nutrients

      TOTNUT(2) = 0.0
      TOTNUT(3) = 0.0
      TOTNUT(4) = 0.0
      FBOD5     = 0.0
      FLUPTN(1) = 0.0
      FLUPTN(9) = 0.0
      DO 501 J = 1, NUSPEC
         FLPRPA(J) = SNGL( (XDEF(J+NUROWS)-X(J)) / CTODRY(J) ) / TSTEPI
         FLUPTN(1) = FLUPTN(1) + FLPRPA(J)
         TOTNUT(2) = TOTNUT(2) + SNGL( XDEF(J+NUROWS)*AA(1,J) )
         TOTNUT(3) = TOTNUT(3) + SNGL( XDEF(J+NUROWS)*AA(2,J) )
         TOTNUT(4) = TOTNUT(4) + SNGL( XDEF(J+NUROWS)*AA(3,J) )
C        N-FIXATION AND / OR MIXOTROPHY
         CDETUP = 0.0
         IF (NUNUCO.GE.4) THEN
           TOTNUT(2) = TOTNUT(2) + SNGL( XDEF(J+NUROWS)*AA(4,J) )
C          FRACTION MIXOTROPHY IN PRODUCTION
           FRMIX = AA(4,J) / (AA(1,J) + AA(4,J))
           CDETUP = MAX(CDETUP,
     1              FRMIX * SNGL( (XDEF(J+NUROWS)-X(J)) / CTODRY(J) ))
         ENDIF
         IF (NUNUCO.GE.5) THEN
           TOTNUT(3) = TOTNUT(3) + SNGL( XDEF(J+NUROWS)*AA(5,J) )
C          FRACTION MIXOTROPHY IN PRODUCTION
           FRMIX = AA(5,J) / (AA(2,J) + AA(5,J))
           CDETUP = MAX(CDETUP,
     1              FRMIX * SNGL( (XDEF(J+NUROWS)-X(J)) / CTODRY(J) ))
         ENDIF
         IF (NUNUCO.EQ.6) TOTNUT(2) = TOTNUT(2) +
     1                      SNGL( XDEF(J+NUROWS)*AA(6,J) )
         FBOD5     = FBOD5     + SNGL( XDEF(J+NUROWS)/CTODRY(J) )
     J             * ( 1. - EXP(-5.*RMORT(J)) )
         FLUPTN(9) = FLUPTN(9) + CDETUP
  501 CONTINUE
      IF (TOTNUT(1) .GT. 1E-30) THEN
          FBOD5 = FBOD5/TOTNUT(1)
      ELSE
          FBOD5 = 1.0
      ENDIF
C
C Compute uptake of nutrients: remaining N, P and Si in XDEF(1) to (3)
C  For nitrogen, we must, however, first determine how much NH4 and
C  how much NO3 is used by phytoplankton. Assume that
C  phytoplankton first depletes ammonia (completely) and then
C  switches to nitrate. If the total dissolved N concentration XDEF(1)
C  is less than the (initial) NO3 concentration, NH4 is completely
C  depleted and some (or all) NO3 must have been used.
C
C  Compute uptake of NH4, NO3, PO4, SIOx

      FLUPTN(2) = (CNO3 + CNH4 + SNGL( AUTO(1) - XDEF(1))) / TSTEPI
      FLUPTN(3) = 0.0
      FLUPTN(4) = (CPO4        + SNGL( AUTO(2) - XDEF(2))) / TSTEPI
      FLUPTN(5) = (CSIO        + SNGL( AUTO(3) - XDEF(3))) / TSTEPI
      IF (NUNUCO.EQ.3) THEN
        FLUPTN(6) = 0.0
        FLUPTN(7) = 0.0
        FLUPTN(8) = 0.0
      ELSEIF (NUNUCO.EQ.4) THEN
        FLUPTN(6) = 0.0
        FLUPTN(7) = 0.0
        FLUPTN(8) = (FIXINF      - SNGL( XDEF(4))) / TSTEPI
      ELSEIF (NUNUCO.EQ.5) THEN
        FLUPTN(6) = (CDETN       - SNGL( XDEF(4))) / TSTEPI
        FLUPTN(7) = (CDETP       - SNGL( XDEF(5))) / TSTEPI
        FLUPTN(8) = 0.0
      ELSE
        FLUPTN(6) = (CDETN       - SNGL( XDEF(4))) / TSTEPI
        FLUPTN(7) = (CDETP       - SNGL( XDEF(5))) / TSTEPI
        FLUPTN(8) = (FIXINF      - SNGL( XDEF(6))) / TSTEPI
      ENDIF

C  Correct for depletion of NH4

      IF (XDEF(1) .LE. CNO3) THEN
         UPTAKE = FLUPTN(2)
         FLUPTN(2) = (CNH4 + AUTO(1)) / TSTEPI
         FLUPTN(3) = UPTAKE - FLUPTN(2)
      ENDIF
      UPTNIT = FLUPTN(2) + FLUPTN(3)
      IF (UPTNIT .GT. 1E-30) THEN
          FRACAM = FLUPTN(2)/UPTNIT
      ELSE
          FRACAM = 1.0
      ENDIF

C Find limiting factors

      I = 0
      DO 450 J = 1,NUNUCO + 3
          READ ( LIMIT((2*J-1):(2*J)),'(I2)') IHULP
          IF ((J.LE.3).OR.(J.GT.NUNUCO)) THEN
            I = I + 1
            FACLIM(I) = REAL(IHULP)
          ENDIF
  450 CONTINUE
C
C Loop to compute effective growth and mortality rates
C
      DO 460 J=1,NUECOG
          RATGRO(J) = 0.0
          RATMOR(J) = 0.0
          CGROUP(J) = 0.0
          DO 455 I = IT2(J,1), IT2(J,2)
              RATGRO(J) = RATGRO(J)
     J              + SNGL( XDEF(I+NUROWS)-X(I) )
              RATMOR(J) = RATMOR(J)
     J              + SNGL(CTODRY(I))*BIOMAS(I) - SNGL(X(I))
              BIOMAS(I) = SNGL(XDEF(I+NUROWS)/CTODRY(I))
              CGROUP(J) = CGROUP(J) + BIOMAS(I)
  455     CONTINUE
          RATGRO(J) = RATGRO(J) / SNGL(XINIT(J)) / TSTEPI
          RATMOR(J) = RATMOR(J) / SNGL(XINIT(J)) / TSTEPI
  460 CONTINUE
C
C Exit
C
      RETURN

C     $Neatly process this error
  901 STOP 'Fatal ERROR in Bloom module: time step too big'

      END

