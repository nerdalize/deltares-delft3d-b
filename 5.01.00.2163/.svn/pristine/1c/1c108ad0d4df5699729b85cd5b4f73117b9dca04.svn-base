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
C     V0.02  230395  Jos van Gils  Modify for hybrid coupling
C     V0.01  040894  Jos van Gils  First version
C
C     MODULE              : CHMAPD
C
C     FUNCTION            : Maps Delwaq and Charon administration
C
C     SUBROUTINES CALLED  :
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NAIJ2   I        1          I       index last dissolved component
C     MMAX    I        1          I       max.number of components
C     NMAX    I        1          I       max.number of species
C     N1MAX   I        1          I       max.value NTTOB
C     N2MAX   I        1          I       max.value NCTOT
C     NTTOB   I        1          O       length of adm.array
C     ITTTOB  I        NTTOB      O       index in transport vector
C     IBTTOB  I        NTTOB      O       index in component vector
C     ABTTOB  R        NTTOB      O       stoch.coefficient
C     RMTTOB  R        NTTOB      O       molar mass
C     NCTOT   I        1          O       length of adm.array
C     ICCTOT  I        NCTOT      O       index in species vector
C     ITCTOT  I        NCTOT      O       index in transport vector
C     ABCTOT  R        NCTOT      O       stoch.coefficient
C     RMCTOT  R        NCTOT      O       molar mass
C     IOUTP   I        NMAX       O       =0: species in transport vector
C                                         >0: index in output array
C     NNOTRA  I        1          O       nr. of species in output vector
C     NTOX    I        1          O       nr. of identified metals
C     ICTOX   I        MMAX       O       index: pos. in list id. metals
C                                         value: pos. in charon comp.vect.
C     ALITOX  C*10     2,NALTOX   I       aliases heavy metals
C     NALTOX  I        1          I       length of ALITOX

      SUBROUTINE  CHMAPD ( NAIJ2 , MMAX  , NMAX  , N1MAX , N2MAX ,
     J                     NTTOB , ITTTOB, IBTTOB, ABTTOB, RMTTOB,
     J                     NCTOT , ICCTOT, ITCTOT, ABCTOT, RMCTOT,
     J                     IOUTP , NNOTRA,
     J                     NALTOX, NTOX  , ICTOX , ALITOX)

      INTEGER         NAIJ2 , MMAX  , I     , J     , INDEX , NTOX  ,
     J                NALTOX, NTTOB , NCTOT , N1MAX , N2MAX ,
     J                NMAX  , NNOTRA
      CHARACTER*10    ALITOX(2,NALTOX), C10
      LOGICAL         GOTCHA
      INTEGER         ICTOX(MMAX), ITTTOB(N1MAX), IBTTOB(N1MAX),
     J                ICCTOT(N2MAX), ITCTOT(N2MAX), IOUTP(NMAX)
      REAL            ABTTOB(N1MAX), RMTTOB(N1MAX),
     J                ABCTOT(N2MAX), RMCTOT(N2MAX)

      INCLUDE 'charon.inc'

C     Check consistency of T

      DO 50 I = 1,NAIJ

C         Check if species is transported Yes or No

          WRITE ( C10 , '(A6,''    '')' ) KN(JCOL(I))
          CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
          IF ( INDEX .LT. 0 ) THEN

C             Component!! Look for _dis/_par , or _tot

              IF ( I .LE. NAIJ2 ) THEN
                  WRITE ( C10 , '(A6,''_dis'')' ) NR(IROW(I),1)
              ELSE
                  WRITE ( C10 , '(A6,''_par'')' ) NR(IROW(I),1)
              ENDIF
              CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
              IF ( INDEX .LE. 0 ) THEN
                  WRITE ( C10 , '(A6,''_tot'')' ) NR(IROW(I),1)
                  CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
              ENDIF

              IF ( INDEX .LE. 0 ) THEN
                  WRITE (*,*) ' Component ',
     J            NR(IROW(I),1), ' in species ', KN(JCOL(I)),
     J            ' not found in transported vector!'
              ENDIF
          ENDIF
   50 CONTINUE

C     Construct arrays to convert T(ransported vector) to B
C     Loop over elements in T

      NTTOB = 0
      DO 100 I = 1,NTRANS

C         Check component or species

          IF ( VARNAM(I)(7:10) .EQ. '    ') THEN

C             Species!! Loop to find relevant components

              GOTCHA = .FALSE.
              DO 10 J = 1,NAIJ
                  IF ( KN(JCOL(J)) .EQ. VARNAM(I)(1:6) ) THEN
                      GOTCHA = .TRUE.
                      NTTOB = NTTOB + 1
                      IF ( NTTOB .GT. N1MAX ) GOTO 901
                      ITTTOB(NTTOB) = I
                      IBTTOB(NTTOB) = IROW(J)
                      ABTTOB(NTTOB) = AIJ(J)
                      RMTTOB(NTTOB) = GFW(JCOL(J))
                  ENDIF
   10         CONTINUE
              IF ( .NOT. GOTCHA ) WRITE (*,*) ' Species ',
     J        VARNAM(I)(1:6),' in transported vector not recognized!'

          ELSE

C             Component!!

              CALL ZOEK ( VARNAM(I)(1:6),M,NR,6,INDEX)
              IF ( INDEX .LE. 0 ) THEN
                 WRITE (*,*) ' Component ',
     J           VARNAM(I)(1:6),' in transported vector not recognized!'
              ELSE
                 NTTOB = NTTOB + 1
                 IF ( NTTOB .GT. N1MAX ) GOTO 901
                 ITTTOB(NTTOB) = I
                 IBTTOB(NTTOB) = INDEX
                 ABTTOB(NTTOB) = 1.0
                 RMTTOB(NTTOB) = COMMAS(INDEX)
              ENDIF

          ENDIF
  100 CONTINUE

C     Fill array that indicates whether species are output variables

      NNOTRA = 0
      DO 200 I = 1,N
          WRITE ( C10 , '(A6,''    '')' ) KN(I)
          CALL ZOEK (C10,NTRANS,VARNAM,10,INDEX)
          IF ( INDEX .LE. 0 ) THEN
              NNOTRA = NNOTRA + 1
              IOUTP(I) = NNOTRA
          ELSE
              IOUTP(I) = 0
          ENDIF
  200 CONTINUE

C     Construct arrays to convert X to T

      NCTOT = 0
      DO 300 I = 1,N

C         Check if species is transported Yes or No

          WRITE ( C10 , '(A6,''    '')' ) KN(I)
          CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
          IF ( INDEX .GT. 0 ) THEN

C             Yes!! Add species.

              NCTOT = NCTOT + 1
              IF ( NCTOT .GT. N2MAX ) GOTO 901
              ICCTOT(NCTOT) = I
              ITCTOT(NCTOT) = INDEX
              ABCTOT(NCTOT) = 1.0
              RMCTOT(NCTOT) = GFW(I)

          ELSE

C             Add components

              DO 210 J = 1,NAIJ
                  IF ( JCOL(J) .EQ. I ) THEN
                      IF ( J .LE. NAIJ2 ) THEN
                          WRITE ( C10 , '(A6,''_dis'')' ) NR(IROW(J),1)
                      ELSE
                          WRITE ( C10 , '(A6,''_par'')' ) NR(IROW(J),1)
                      ENDIF
                      CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
                      IF ( INDEX .LE. 0 ) THEN
                          WRITE ( C10 , '(A6,''_tot'')' ) NR(IROW(J),1)
                          CALL ZOEK ( C10,NTRANS,VARNAM,10,INDEX)
                      ENDIF
                      IF ( INDEX .GT. 0 ) THEN
                          NCTOT = NCTOT + 1
                          IF ( NCTOT .GT. N2MAX ) GOTO 901
                          ICCTOT(NCTOT) = I
                          ITCTOT(NCTOT) = INDEX
                          ABCTOT(NCTOT) = AIJ(J)
                          RMCTOT(NCTOT) = COMMAS(IROW(J))
                      ENDIF
                  ENDIF
  210         CONTINUE

          ENDIF

  300 CONTINUE

C     Determine actual list of present h.m.'s

      NTOX = 0
      DO 400 I = 1,NALTOX
          CALL ZOEK (ALITOX(1,I),M,NR,6,INDEX)
          IF (INDEX .GT. 0) THEN
              NTOX = NTOX + 1
              ICTOX(NTOX) = INDEX
          ENDIF
  400 CONTINUE

      RETURN
  901 STOP 'PDFCHA: Dimension error'
      END
