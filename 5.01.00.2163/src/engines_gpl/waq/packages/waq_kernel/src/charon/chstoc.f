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

C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            :
C
C     V0.02  240395  Jos van Gils  Modify for hybrid coupling
C     V0.01  040894  Jos van Gils  First version
C
C     MODULE              : CHSTOC
C
C     FUNCTION            : Manipulates stoichiometric coefficients
C                           of Delwaq 4.0 for use with Charon
C
C     SUBROUTINES CALLED  :
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C
      SUBROUTINE CHSTOC (LUREP,NAIJ2)
C
C     Declarations
C
      INTEGER      LUIN  , LUOUT , I     , ICOMP , ISPEC , IS    ,
     J             LUREP , NSTOC , NSTOCN, NSTOCM, ISTOC , NSKIP,
     J             INDEX , NAIJ2
      PARAMETER   (NSTOCM = 100)
      CHARACTER*10 C10
      CHARACTER*12 ACTSUB, ACTFLU, SUBARR(NSTOCM), FLUARR(NSTOCM)
      CHARACTER*80 REGEL
      REAL         ACTSTO, RMSPEC, RMCOMP, STOCHI, STOARR(NSTOCM)

      INCLUDE 'charon.inc'

      DATA LUIN  /21/
      DATA LUOUT /22/

C     Open pdf input and output file

      OPEN (LUIN,FILE='PROCES.ASC')
      OPEN (LUOUT,FILE='PROCES_C.ASC')

C     Read and copy first line (nr. of processes)

      READ  (LUIN  ,'(A80)',END=100) REGEL
      WRITE (LUOUT ,'(A80)') REGEL

C     Loop over processes in file

   10 CONTINUE

C     Skip name process, name module and TRSwitch

      READ  (LUIN ,'(A80)',END=100) REGEL
      WRITE (LUOUT ,'(A80)') REGEL
      WRITE (LUREP ,'(A80)') REGEL
      READ  (LUIN ,'(A80)') REGEL
      WRITE (LUOUT ,'(A80)') REGEL
      READ  (LUIN ,'(A80)') REGEL
      WRITE (LUOUT ,'(A80)') REGEL

C     Skip input parameters segments

      CALL SKIBLO (LUIN,LUOUT)

C     Skip input parameters exchanges

      CALL SKIBLO (LUIN,LUOUT)

C     Skip output parameters segments

      CALL SKIBLO (LUIN,LUOUT)

C     Skip output parameters exchanges

      CALL SKIBLO (LUIN,LUOUT)

C     Skip fluxes

      CALL SKIBLO (LUIN,LUOUT)

C     Read number of stoichiometry lines, initialize new number of lines

      READ (LUIN,'(I10)') NSTOC
      NSTOCN = 0

C     Loop over stoichiometry lines

      DO 90 IS = 1,NSTOC
          READ (LUIN,'(2A12,F10.0)') ACTSUB, ACTFLU, ACTSTO

C         Solve problem with double meaning of PO4 and NH4

          IF ( ACTSUB .EQ. 'NH4         ' ) ACTSUB = 'NH4+        '
          IF ( ACTSUB .EQ. 'PO4         ' ) ACTSUB = 'PO4---      '

C         Is this a CHARON species?? Is it transported??

          CALL ZOEK (ACTSUB,N,KN,6,ISPEC)
          WRITE ( C10 , '(A6,''    '')' ) ACTSUB
          CALL ZOEK (C10,NTRANS,VARNAM,10,INDEX)
          IF (ISPEC .GT. 0 .AND. INDEX .LE. 0) THEN

C             Charon-species, which is not transported

              WRITE (LUREP,'(''Remove :'',2A12,F10.3)')
     J        ACTSUB, ACTFLU, ACTSTO

C             Find molar mass to make flux in moles

              RMSPEC = GFW(ISPEC)

C             Loop over non-zero matrix-entries CHARON

              DO 20 I = 1,NAIJ
                  IF ( JCOL(I) .EQ. ISPEC ) THEN

C                     This entry concerns the right species!!

                      ICOMP = IROW(I)

C                     Find molar mass of component

                      RMCOMP = COMMAS(ICOMP)

C                     Find component in transported vector

                      IF ( I .LE. NAIJ2 ) THEN
                          WRITE (C10,'(A6,''_dis'')') NR(ICOMP,1)
                      ELSE
                          WRITE (C10,'(A6,''_par'')') NR(ICOMP,1)
                      ENDIF
                      CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
                      IF ( INDEX .LE. 0 ) THEN
                          WRITE (C10,'(A6,''_tot'')') NR(ICOMP,1)
                          CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
                      ENDIF
                      IF ( INDEX .LE. 0 ) STOP 'CHSTOC: 001'

C                     Component has been found: INDEX

                      WRITE (ACTSUB,'(A10,''  '')') VARNAM(INDEX)

C                     Compute new stochi coefficient

                      STOCHI = ACTSTO * AIJ(I) * RMCOMP / RMSPEC

C                     New stoichiometry line found
C                     Check if the affected substance is a new one
C                     if not, increase stoch. coefficient
C                             and skip creating new line

                      DO 15 ISTOC = 1,NSTOCN
                      IF ( SUBARR(ISTOC) .EQ. ACTSUB .AND.
     J                     FLUARR(ISTOC) .EQ. ACTFLU ) THEN
                          STOARR(ISTOC) = STOARR(ISTOC) + STOCHI
                          GOTO 18
                      ENDIF
   15                 CONTINUE

C                     copy to block of new lines

                      NSTOCN = NSTOCN + 1
                      IF ( NSTOCN .GT. NSTOCM ) GOTO 900
                      SUBARR(NSTOCN) = ACTSUB
                      FLUARR(NSTOCN) = ACTFLU
                      STOARR(NSTOCN) = STOCHI

   18                 CONTINUE


C                 End block for right component

                  ENDIF

C                 End loop non-zero matrix-entries CHARON

   20         CONTINUE

C             End block of not-transported affected CHARON-species

          ELSE

C             Stoichiometry line does not concern CHARON species
C             copy to block of new lines and give message

              WRITE (LUREP,'(''Accept :'',2A12,F10.3)')
     J        ACTSUB, ACTFLU, ACTSTO
              NSTOCN = NSTOCN + 1
              IF ( NSTOCN .GT. NSTOCM ) GOTO 900
              SUBARR(NSTOCN) = ACTSUB
              FLUARR(NSTOCN) = ACTFLU
              STOARR(NSTOCN) = ACTSTO
          ENDIF

C         End loop over old STOCHI lines

   90 CONTINUE

C     Check STOCHI lines, SKIP if coefficient is low

      NSKIP = 0
      DO 92 I = 1,NSTOCN
   92 IF ( ABS(STOARR(I)) .LT. 0.005 ) NSKIP = NSKIP + 1

C     Write new block of STOCHI lines, SKIP if coefficient is low

      WRITE ( LUOUT , '(I3,9X,''; Basis stochiometrie'')' ) NSTOCN-NSKIP
      DO 95 I = 1,NSTOCN
          IF ( ABS(STOARR(I)) .GT. 0.005 ) THEN
              WRITE(LUREP,'(''Final  :'',2A12,F10.3)')
     J        SUBARR(I), FLUARR(I), STOARR(I)
              WRITE (LUOUT,'(2A12,F10.3)')
     J        SUBARR(I), FLUARR(I), STOARR(I)
          ELSE
              WRITE(LUREP,'(''Neglect:'',2A12,F10.3)')
     J        SUBARR(I), FLUARR(I), STOARR(I)
          ENDIF
   95 CONTINUE

C     Skip 2 more blocks and END line
      CALL SKIBLO(LUIN,LUOUT)
      CALL SKIBLO(LUIN,LUOUT)
      READ  (LUIN ,'(A80)') REGEL
      WRITE (LUOUT ,'(A80)') REGEL

C     End loop processes

      GOTO 10

C     Close file

  100 CONTINUE
      CLOSE (LUIN)
      CLOSE (LUOUT)

      RETURN
  900 STOP 'Dimension error in CHSTOC'
      END

      SUBROUTINE SKIBLO (LUIN,LUOUT)
      INTEGER  LUIN, I, IHULP, LUOUT
      CHARACTER*80 REGEL
      READ (LUIN,'(I10)') IHULP
      BACKSPACE (LUIN)
      DO 10 I = 1,IHULP+1
      READ (LUIN,'(A80)') REGEL
   10 WRITE (LUOUT,'(A80)') REGEL
      RETURN
      END
