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

C    Date:       5 Augustus 1994
C    Program:    GETCOE.FOR
C    Version:    0.1
C    Programmer: Jos van Gils
C
C    Get characteristices of algae
C
C    Called by: PDFBLO
C    Calls    : -

      SUBROUTINE GETCOE (NTYPE , DMCRAT, NCRAT , PCRAT , SCRAT , SPEXT )
C
C     Arguments
C
C     Name    Type  Length   I/O  Description
C
C     NTYPE   I     1        I    Number of species
C     DMCRAT  R     NTYPE    O    DM/C Ratio
C     NCRAT   R     NTYPE    O    N/C Ratio
C     PCRAT   R     NTYPE    O    P/C Ratio
C     SCRAT   R     NTYPE    O    SI/C Ratio
C     SPEXT   R     NTYPE    O    Specific extinction (m2/gC)
C
      INTEGER         NTYPE
      REAL            DMCRAT(NTYPE), NCRAT(NTYPE), PCRAT(NTYPE),
     J                SCRAT(NTYPE), SPEXT(NTYPE)

C     Common block variables used
C
C     Name    Type  Length   I/O  Inc-file  Description
C
C     MN      I     1        I    blmdim    Max. number of nutrients
C     MT      I     1        I    blmdim    Max. number of species
C     NUSPEC  I     1        I    phyt2     Actual number of species
C     MS      I     1        I    blmdim    Max. number of groups
C     NUECOG  I     1        I    phyt2     Actual number of groups
C     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
C     CTODRY  R*8   MT       I    size      Conversion (gDW/gC)
C     EKX     R*8   MT       I    phyt1     Specific extinction
C                                           (1/m/(gDW/m3))
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'

C     Local variables

      INTEGER     I

      IF (NUSPEC.NE.NTYPE )
     JSTOP 'Getcoe: Error 001, consult system manager.'

      DO 10 I = 1,NUSPEC
         DMCRAT(I) =           CTODRY(I)
         NCRAT(I)  = AA(1,I) * CTODRY(I)
         PCRAT(I)  = AA(2,I) * CTODRY(I)
         SCRAT(I)  = AA(3,I) * CTODRY(I)
         SPEXT(I)  = EKX(I)  * CTODRY(I)
   10 CONTINUE

      RETURN

      END
