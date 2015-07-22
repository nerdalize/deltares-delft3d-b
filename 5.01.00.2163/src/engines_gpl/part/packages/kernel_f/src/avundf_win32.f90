! --------------------------------------------------------------------------
! Routine ad hoc:
! Avoid a nasty problem with underflows on Windows 95/98.
!
! Usage:
! Call this routine once early in the program, for instance just after 
! start-up.
!
! Note:
! It contains statements specific for Digital/Compaq Visual Fortran.
! This means that under UNIX you will need to comment out most of the 
! code, an empty routine will suffice.
!
! Note:
! It even contains some extensions defined by Digital Visual Fortran
! --------------------------------------------------------------------------
!
      SUBROUTINE AVUNDF
!
      USE DFLIB
      INTEGER(2) STS
!
! ---------- Get the current settings of the mathematical coprocessor
!            and add zaa flag for treating underflows "benevolently"
!
      CALL GETCONTROLFPQQ( STS )
      STS = STS .OR. FPCW$UNDERFLOW
      CALL SETCONTROLFPQQ( STS )
!
! ---------- That was all. Return
!
      RETURN
      END
