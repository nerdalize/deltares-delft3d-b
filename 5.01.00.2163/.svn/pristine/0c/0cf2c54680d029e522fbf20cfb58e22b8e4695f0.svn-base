C
C     ZOEK IS NIET AFHANKELIJK VAN UPPERCASE/LOWERCASE
C
C     De routine maakt gebruik van ICHAR()
C     a t/m z hebben codes 97 t/m 122
C     A t/m Z hebben codes 65 t/m 90
C
      SUBROUTINE ZOEK (NAAM,NOTOT,SYNAME,NZOEK,INDEX)
      INTEGER NOTOT, NZOEK, INDEX
      CHARACTER*(*) NAAM
      CHARACTER*(*) SYNAME(NOTOT)
      INTEGER I,K,I1,I2
      INDEX = -1
c     WRITE (*,'(A)') NAAM(1:NZOEK)
      DO 100 I = 1,NOTOT
c         WRITE (*,'(I5,A)') I,SYNAME(I)(1:NZOEK)
          DO 50 K = 1,NZOEK
              I1 = ICHAR(NAAM     (K:K))
              I2 = ICHAR(SYNAME(I)(K:K))
              IF (I1.GE. 97.AND.I1.LE.122) THEN
                  IF (I1.NE.I2.AND.I1.NE.(I2+32)) GOTO 100
              ELSEIF (I1.GE. 65.AND.I1.LE. 90) THEN
                  IF (I1.NE.I2.AND.I1.NE.(I2-32)) GOTO 100
              ELSE
                  IF (I1.NE.I2                  ) GOTO 100
              ENDIF
   50     CONTINUE
          INDEX = I
          GOTO 200
C         IF (NAAM(1:NZOEK).EQ.SYNAME(I)(1:NZOEK)) GOTO 200
  100 CONTINUE
c     WRITE (*,*) ' ZOEK:',INDEX
      RETURN
  200 CONTINUE
c     WRITE (*,*) ' ZOEK:',INDEX
      RETURN
      END
