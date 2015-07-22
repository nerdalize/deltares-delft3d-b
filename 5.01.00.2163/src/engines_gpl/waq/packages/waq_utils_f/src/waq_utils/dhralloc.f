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

      MODULE DHRALLOC
C
      INTERFACE
      SUBROUTINE DHRALLOC_REAL ( PREAL , NEW_LENGTH, OLD_LENGTH )
      REAL        , POINTER :: PREAL(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE
      SUBROUTINE DHRALLOC_INT ( PINT  , NEW_LENGTH, OLD_LENGTH )
      INTEGER     , POINTER :: PINT(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE
      SUBROUTINE DHRALLOC_CH10 ( PCH10 , NEW_LENGTH, OLD_LENGTH )
      CHARACTER*10, POINTER :: PCH10(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE
      SUBROUTINE DHRALLOC_CH20 ( PCH20 , NEW_LENGTH, OLD_LENGTH )
      CHARACTER*20, POINTER :: PCH20(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE
      SUBROUTINE DHRALLOC_CH50 ( PCH50 , NEW_LENGTH, OLD_LENGTH )
      CHARACTER*50, POINTER :: PCH50(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE
      SUBROUTINE DHRALLOC_LOGI ( PLOGI , NEW_LENGTH, OLD_LENGTH )
      LOGICAL     , POINTER :: PLOGI(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
      END SUBROUTINE
C
      END INTERFACE
C
      END MODULE
      SUBROUTINE DHRALLOC_REAL ( PREAL , NEW_LENGTH, OLD_LENGTH )
C
C     Declaration of arguments
C
      REAL        , POINTER :: PREAL(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
C
      LOGICAL       L_ASSOCIATED
      REAL        , POINTER :: P_HULP(:)
C
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
C
      L_ASSOCIATED = ASSOCIATED(PREAL)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PREAL(1:MIN_LENGTH)
            DEALLOCATE(PREAL)
            PREAL => P_HULP
         ELSE
            DEALLOCATE(PREAL)
            ALLOCATE(PREAL(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PREAL(NEW_LENGTH))
      ENDIF
C
      END
      SUBROUTINE DHRALLOC_INT ( PINT  , NEW_LENGTH, OLD_LENGTH )
C
C     Declaration of arguments
C
      INTEGER     , POINTER :: PINT(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
C
      LOGICAL       L_ASSOCIATED
      INTEGER     , POINTER :: P_HULP(:)
C
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
C
      L_ASSOCIATED = ASSOCIATED(PINT)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PINT(1:MIN_LENGTH)
            DEALLOCATE(PINT)
            PINT => P_HULP
         ELSE
            DEALLOCATE(PINT)
            ALLOCATE(PINT(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PINT(NEW_LENGTH))
      ENDIF
C
      END
      SUBROUTINE DHRALLOC_CH10( PCH10  , NEW_LENGTH, OLD_LENGTH )
C
C     Declaration of arguments
C
      CHARACTER*10, POINTER :: PCH10(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
C
      LOGICAL       L_ASSOCIATED
      CHARACTER*10, POINTER :: P_HULP(:)
C
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
C
      L_ASSOCIATED = ASSOCIATED(PCH10)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PCH10(1:MIN_LENGTH)
            DEALLOCATE(PCH10)
            PCH10 => P_HULP
         ELSE
            DEALLOCATE(PCH10)
            ALLOCATE(PCH10(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PCH10(NEW_LENGTH))
      ENDIF
C
      END
      SUBROUTINE DHRALLOC_CH20( PCH20  , NEW_LENGTH, OLD_LENGTH )
C
C     Declaration of arguments
C
      CHARACTER*20, POINTER :: PCH20(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
C
      LOGICAL       L_ASSOCIATED
      CHARACTER*20, POINTER :: P_HULP(:)
C
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
C
      L_ASSOCIATED = ASSOCIATED(PCH20)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PCH20(1:MIN_LENGTH)
            DEALLOCATE(PCH20)
            PCH20 => P_HULP
         ELSE
            DEALLOCATE(PCH20)
            ALLOCATE(PCH20(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PCH20(NEW_LENGTH))
      ENDIF
C
      END
      SUBROUTINE DHRALLOC_CH50( PCH50  , NEW_LENGTH, OLD_LENGTH )
C
C     Declaration of arguments
C
      CHARACTER*50, POINTER :: PCH50(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
C
      LOGICAL       L_ASSOCIATED
      CHARACTER*50, POINTER :: P_HULP(:)
C
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
C
      L_ASSOCIATED = ASSOCIATED(PCH50)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PCH50(1:MIN_LENGTH)
            DEALLOCATE(PCH50)
            PCH50 => P_HULP
         ELSE
            DEALLOCATE(PCH50)
            ALLOCATE(PCH50(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PCH50(NEW_LENGTH))
      ENDIF
C
      END
      SUBROUTINE DHRALLOC_LOGI( PLOGI  , NEW_LENGTH, OLD_LENGTH )
C
C     Declaration of arguments
C
      LOGICAL     , POINTER :: PLOGI(:)
      INTEGER       OLD_LENGTH, NEW_LENGTH
C
      LOGICAL       L_ASSOCIATED
      LOGICAL     , POINTER :: P_HULP(:)
C
      MIN_LENGTH = MIN(OLD_LENGTH,NEW_LENGTH)
C
      L_ASSOCIATED = ASSOCIATED(PLOGI)
      IF (  L_ASSOCIATED ) THEN
         IF ( MIN_LENGTH .GT. 0 ) THEN
            ALLOCATE(P_HULP(NEW_LENGTH))
            P_HULP(1:MIN_LENGTH) = PLOGI(1:MIN_LENGTH)
            DEALLOCATE(PLOGI)
            PLOGI => P_HULP
         ELSE
            DEALLOCATE(PLOGI)
            ALLOCATE(PLOGI(NEW_LENGTH))
         ENDIF
      ELSE
         ALLOCATE(PLOGI(NEW_LENGTH))
      ENDIF
C
      END
