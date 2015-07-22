      integer function GTCPNT(pntnam)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: memlib.f90 1703 2012-07-13 14:34:14Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd1/packages/nesthd1/src/memlib.f90 $
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION GTCPNT
!           Function: Gets pointer PNTNAM for character array with the
!                     dynamic array declaration routines
!                     In case of an negative exit code the error
!                     handling is executed here (barely)
!                     exit value: -10 mlj Pointer name not found
!                               : -30 mlj Pointer name to long
!        Method used:
!               Date: 20-08-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various
!-----------------------------------------------------------------------
!   Called  routines:              GETPRT
!                                  ERRPNT
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      save
!
      integer       getptr,ind   ,errpnt
!
      character*(*) pntnam
!
      external      GETPTR,ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam, ' ')
      if (ind   .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function GETPTR to get requested
!     array pointer
!----------------------------------------------------------------------
      gtcpnt = GETPTR(pntnam(:ind-1))
!----------------------------------------------------------------------
!-----Test exit code = 0, with means error has occured
!----------------------------------------------------------------------
      if (gtcpnt .eq. 0) then
         gtcpnt = ERRPNT(pntnam,'character','getptr')
      endif
!
      end
!***********************************************************************
      integer function GTIPNT(pntnam)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION GTIPNT
!           Function: Gets pointer PNTNAM for integer array with the
!                     dynamic array declaration routines
!                     In case of an negative exit code the error
!                     handling is executed here (barely)
!                     exit value: -10 mlj Pointer name not found
!                               : -30 mlj Pointer name to long
!        Method used:
!               Date: 20-08-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header:
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various
!-----------------------------------------------------------------------
!   Called  routines:              GETPRT
!                                  ERRPNT
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      save
!
      integer       getptr,ind   ,errpnt
!
      character*(*) pntnam
!
      external      GETPTR,ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam,' ')
      if (ind    .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function GETPTR to get requested
!     array pointer
!----------------------------------------------------------------------
      gtipnt = GETPTR(pntnam(:ind-1))
!----------------------------------------------------------------------
!-----Test exit code = 0, with means error has occured
!----------------------------------------------------------------------
      if (gtipnt .eq. 0) then
         gtipnt = ERRPNT(pntnam,'integer','getptr')
      endif
!
      end
!***********************************************************************
      integer function GTLPNT(pntnam)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION GTLPNT
!           Function: Gets pointer PNTNAM for logical array with the
!                     dynamic array declaration routines
!                     In case of an negative exit code the error
!                     handling is executed here (barely)
!                     exit value: -10 mlj Pointer name not found
!                               : -30 mlj Pointer name to long
!        Method used:
!               Date: 28-07-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header:
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various
!-----------------------------------------------------------------------
!   Called  routines:              GETPRT
!                                  ERRPNT
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      save
!
      integer       getptr,ind   ,errpnt
!
      character*(*) pntnam
!
      external      GETPTR,ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam,' ')
      if (ind    .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function GETPTR to get requested
!     array pointer
!----------------------------------------------------------------------
      gtlpnt = GETPTR(pntnam(:ind-1))
!----------------------------------------------------------------------
!-----Test exit code = 0, with means error has occured
!----------------------------------------------------------------------
      if (gtlpnt .eq. 0) then
         gtlpnt = ERRPNT(pntnam,'logical','getptr')
      endif
!
      end
!***********************************************************************
      integer function GTRPNT(pntnam)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION GTRPNT
!           Function: Gets pointer PNTNAM for real array with the
!                     dynamic array declaration routines
!                     In case of an negative exit code the error
!                     handling is executed here (barely)
!                     exit value: -10 mlj Pointer name not found
!                               : -30 mlj Pointer name to long
!        Method used:
!               Date: 20-08-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header:
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various
!-----------------------------------------------------------------------
!   Called  routines:              GETPRT
!                                  ERRPNT
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      save
!
      integer       getptr,ind   ,errpnt
!
      character*(*) pntnam
!
      external      GETPTR,ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam,' ')
      if (ind   .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function GETPTR to get requested
!     array pointer
!----------------------------------------------------------------------
      gtrpnt = GETPTR(pntnam(:ind-1))
!----------------------------------------------------------------------
!-----Test exit code = 0, with means error has occured
!----------------------------------------------------------------------
      if (gtrpnt .eq. 0) then
         gtrpnt = ERRPNT(pntnam,'real','getptr')
      endif
!
      end
!***********************************************************************
      integer function MKCPNT(pntnam    ,length    )
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION MKCPNT
!           Function: Request memory space with the dynamic array
!                     declaration routines to create a pointer for the
!                     character array with pointer name PNTNAM
!                     In case of a new array the requested memory is
!                     initialized
!                     In case of a negative exit code the error
!                     handling is left for the calling module
!                     exit value: -10 mlj Not enough memory
!                               : -20 mlj Wrong type specified
!                               : -30 mlj Pointer name to long
!                               : -40 mlj Pointer already exists and
!                                         length equals defined length
!                                         will be redefined in -1
!                               : -41 mlj Pointer already exists, but of
!                                         wrong type
!                               : -42 mlj Pointer already exists, but
!                                         length longer than defined
!                               : -43 mlj Pointer already exists, but
!                                         length smaller than defined
!                     When -1 occures go on, al the others exit program
!        Method used:
!               Date: 20-08-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various (esm_alloc_char.f90)
!-----------------------------------------------------------------------
!   Called  routines:              CHNULL
!                                  ERRPNT
!                                  MAKPTR
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LENGTH  I   I*4                  Total required array length
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!   Common variables:
!   -----------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! CHBUF      CH*1  (0:0)           Complete character array for dynamic
!                                  array declaration
! CHTYP       I*4                  Type number for characters
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      use precision
      save
!
      include      'fsm.i'
!
      integer       length,ind   ,errpnt
!
      character*(*) pntnam
!
      external      ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam,' ')
      if (ind   .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function MAKPTR for pointer name
!     and required array length. Because length 0 is not permitted
!     a minimum length of 1 is always given
!----------------------------------------------------------------------
      mkcpnt = MAKPTR(pntnam(:ind-1),chtyp ,MAX (1,length))
!----------------------------------------------------------------------
!-----Initialize requested memory space if output of function MAKPTR
!     is greater then 0
!----------------------------------------------------------------------
      if (mkcpnt .ne. 0) then
         call    CHNULL(chbuf (mkcpnt),length)
         mkcpnt=1
!----------------------------------------------------------------------
!-----Return error code for MAKPTR <> 0
!----------------------------------------------------------------------
      else
         mkcpnt = ERRPNT(pntnam,'character','makptr')
      endif
!
      end
!***********************************************************************
      integer function MKIPNT(pntnam,length)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION MKIPNT
!           Function: Request memory space with the dynamic array
!                     declaration routines to create a pointer for the
!                     integer array with pointer name PNTNAM
!                     In case of a new array the requested memory is
!                     initialized
!                     In case of a negative exit code the error
!                     handling is left for the calling module
!                     exit value: -10 mlj Not enough memory
!                               : -20 mlj Wrong type specified
!                               : -30 mlj Pointer name to long
!                               : -40 mlj Pointer already exists and
!                                         length equals defined length
!                                         will be redefined in -1
!                               : -41 mlj Pointer already exists, but of
!                                         wrong type
!                               : -42 mlj Pointer already exists, but
!                                         length longer than defined
!                               : -43 mlj Pointer already exists, but
!                                         length smaller than defined
!                     When -1 occures go on, al the others exit program
!        Method used:
!               Date: 20-08-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various
!-----------------------------------------------------------------------
!   Called  routines:              ERRPNT
!                                  INULL 
!                                  MAKPTR
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LENGTH  I   I*4                  Total required array length
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!   Common variables:
!   -----------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! IBUF        I*4  (0:0)           Complete integer array for dynamic
!                                  array declaration
! ITYP        I*4                  Type number for integer
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      use precision
      save
!
      include      'fsm.i'
!
      integer       length,ind   ,errpnt
!
      character*(*) pntnam
!
      external      ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam,' ')
      if (ind .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function MAKPTR for pointer name
!     and required array length. Because length 0 is not permitted
!     a minimum length of 1 is always given
!----------------------------------------------------------------------
      mkipnt = MAKPTR(pntnam(:ind-1),ityp  ,MAX (1,length))
!----------------------------------------------------------------------
!-----Initialize requested memory space if output of function MAKPTR
!     is greater then 0
!----------------------------------------------------------------------
      if (mkipnt .ne. 0) then
         call    INULL (ibuf  (mkipnt),length)
         mkipnt=1
!----------------------------------------------------------------------
!-----Return error code for MAKPTR <> 0
!----------------------------------------------------------------------
      else
         mkipnt = ERRPNT(pntnam,'integer','makptr')
      endif
!
      end
!***********************************************************************
      integer function MKLPNT(pntnam,length)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION MKLPNT
!           Function: Request memory space with the dynamic array
!                     declaration routines to create a pointer for the
!                     logical array with pointer name PNTNAM
!                     In case of a new array the requested memory is
!                     initialized
!                     In case of a negative exit code the error
!                     handling is left for the calling module
!                     exit value: -10 mlj Not enough memory
!                               : -20 mlj Wrong type specified
!                               : -30 mlj Pointer name to long
!                               : -40 mlj Pointer already exists and
!                                         length equals defined length
!                                         will be redefined in -1
!                               : -41 mlj Pointer already exists, but of
!                                         wrong type
!                               : -42 mlj Pointer already exists, but
!                                         length longer than defined
!                               : -43 mlj Pointer already exists, but
!                                         length smaller than defined
!                     When -1 occures go on, al the others exit program
!        Method used:
!               Date: 20-08-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various
!-----------------------------------------------------------------------
!   Called  routines:              ERRPNT
!                                  LNULL
!                                  MAKPTR
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LENGTH  I   I*4                  Total required array length
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!   Common variables:
!   -----------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LBUF        L*4  (0:0)           Complete logical array for dynamic
!                                  array declaration
! LTYP        I*4                  Type number for logical
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      use precision
      save
!
      include      'fsm.i'
!
      integer       length,ind   ,errpnt
!
      character*(*) pntnam
!
      external      ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam,' ')
      if (ind   .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function MAKPTR for pointer name
!     and required array length. Because length 0 is not permitted
!     a minimum length of 1 is always given
!----------------------------------------------------------------------
      mklpnt = MAKPTR(pntnam(:ind-1),ltyp  ,MAX (1,length))
!----------------------------------------------------------------------
!-----Initialize requested memory space if output of function MAKPTR
!     is greater then 0
!----------------------------------------------------------------------
      if (mklpnt .ne. 0) then
         call    LNULL (lbuf  (mklpnt),length)
         mklpnt=1
!----------------------------------------------------------------------
!-----Return error code for MAKPTR <> 0
!----------------------------------------------------------------------
      else
         mklpnt = ERRPNT(pntnam,'logical','makptr')
      endif
!
      end
!***********************************************************************
      integer function MKRPNT(pntnam,length)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION MKRPNT
!           Function: Request memory space with the dynamic array
!                     declaration routines to create a pointer for the
!                     real array with pointer name PNTNAM
!                     In case of a new array the requested memory is
!                     initialized
!                     In case of a negative exit code the error
!                     handling is left for the calling module
!                     exit value: -10 mlj Not enough memory
!                               : -20 mlj Wrong type specified
!                               : -30 mlj Pointer name to long
!                               : -40 mlj Pointer already exists and
!                                         length equals defined length
!                                         will be redefined in -1
!                               : -41 mlj Pointer already exists, but of
!                                         wrong type
!                               : -42 mlj Pointer already exists, but
!                                         length longer than defined
!                               : -43 mlj Pointer already exists, but
!                                         length smaller than defined
!                     When -1 occures go on, al the others exit program
!                     returns 1 if sucessfull
!        Method used:
!               Date: 20-08-1998
!         Programmer: W. t. Horst, H.H. Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              various
!-----------------------------------------------------------------------
!   Called  routines:              ERRPNT
!                                  MAKPTR
!                                  RNULL
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LENGTH  I   I*4                  Total required array length
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
!-----------------------------------------------------------------------
!   Common variables:
!   -----------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! RBUF        R*4  (0:0)           Complete integer array for dynamic
!                                  array declaration
! RTYP        I*4                  Type number for real
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IND         I*4                  Actual length of character string
!                                  PNTNAM
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      use precision
      save
!
      include      'fsm.i'
!
      integer       length,ind   ,errpnt
!
      character*(*) pntnam
!
      external      ERRPNT
!----------------------------------------------------------------------
!-----Define length of pointer name (array name)
!----------------------------------------------------------------------
      ind    = INDEX (pntnam,' ')
      if (ind   .eq. 0) ind    = LEN (pntnam) + 1
!----------------------------------------------------------------------
!-----Call dynamic array declaration function MAKPTR for pointer name
!     and required array length. Because length 0 is not permitted
!     a minimum length of 1 is always given
!----------------------------------------------------------------------
      mkrpnt = MAKPTR(pntnam(:ind-1),rtyp  ,MAX (1,length))
!----------------------------------------------------------------------
!-----Initialize requested memory space if output of function MAKPTR
!     is greater then 0
!----------------------------------------------------------------------
      if (mkrpnt .ne. 0) then
         call    RNULL (rbuf  (mkrpnt),length)
         mkrpnt=1
!----------------------------------------------------------------------
!-----Return error code for MAKPTR <> 0
!----------------------------------------------------------------------
      else
        mkrpnt = ERRPNT(pntnam,'real','makptr')
      endif
!
      end
!***********************************************************************
      integer function ERRPNT(pntnam,soort,callty)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: FUNCTION ERRPNT
!           Function: Get the current error value from a failed
!                     call to MAKPTR or GETPTR or RELPTR and translate
!                     it to the old style  return value for the MK?PNT
!                     series handling is left to the calling module
!                     exit value: -10 mlj Not enough memory
!                               : -20 mlj Wrong type specified
!                               : -30 mlj Pointer name to long
!                               : -40 mlj Pointer already exists and
!                                         length equals defined length
!                                         will be redefined in -1
!                               : -41 mlj Pointer already exists, but of
!                                         wrong type
!                               : -42 mlj Pointer already exists, but
!                                         length longer than defined
!                               : -43 mlj Pointer already exists, but
!                                         length smaller than defined
!                     Old exit values -42 mlj & -43 mlj not able to
!                     reproduce
!        Method used:
!               Date: 20-07-2000
!         Programmer: S.Lambrechtsen
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              MK?PNT
!                                  GT?PNT
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! CALLTY  I  CH*(*)                Type of call 'getprt' or 'makptr'
! PNTNAM  I  CH*(*)                Character string containing array
!                                  name (hence max 6 characters).
! SOORT   I  CH*(*)                Character string containing type of
!                                  pointer to be made
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IERR        I*4                  Error code number as output from
!                                  function ERRPRT
! LUNDIA      I*4                  Unit number of diagnostic file
!                                  =0 No sub-system defined or diagno-
!                                     stic file is not open
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      integer        errptr,ierr  ,lundia
!
      character*(*)  pntnam,soort ,callty
!
      external       ERRPTR
!-----------------------------------------------------------------------
!-----Get error message error code
!-----------------------------------------------------------------------
       ierr =0
!      ierr   = ERRPTR()
!-----------------------------------------------------------------------
!-----Write appropriate error message to screen or diagnostic file
!-----------------------------------------------------------------------
      if (ierr   .eq. 14) then
!-----------------------------------------------------------------------
!--------IERR = 14: pointer exists with same length and type
!-----------------------------------------------------------------------
         errpnt = -40000000
!-----------------------------------------------------------------------
!--------This is ok pointer is already made, return value ERRPNT will
!        be re-defined to -1 sothat the MK?PNT routines return an
!        expected value For CALLTY = 'getprt' exit
!-----------------------------------------------------------------------
         if (callty .eq. 'makptr') then
           errpnt = -1
         else
!-----------------------------------------------------------------------
!----------Get diagnostic unit
!-----------------------------------------------------------------------
           call    GETLUN(lundia)
!-----------------------------------------------------------------------
!----------Write message to screen (LUNDIA=0) or diagnostic (LUNDIA<>0)
!-----------------------------------------------------------------------
           if (lundia .eq. 0) then
             write(*,*)      '*** ERROR FMM error ',ierr , ' occured ?! '
             write(*,*)      '          Contact Deltares'
           else
             write(lundia,*) '*** ERROR FMM error ',ierr , ' occured ?! '
             write(lundia,*) '          Contact Deltares'
           endif
           call    CSTOP (1         ,CHAR (0))
         endif
      else if (ierr   .eq. 8) then
!-----------------------------------------------------------------------
!--------IERR =  8 : cannot mallocate bytes
!-----------------------------------------------------------------------
         errpnt = -10000000
!-----------------------------------------------------------------------
!--------Get diagnostic unit
!-----------------------------------------------------------------------
         call    GETLUN(lundia)
!-----------------------------------------------------------------------
!--------Write message to screen (LUNDIA=0) or diagnostic (LUNDIA<>0)
!-----------------------------------------------------------------------
         if (lundia .eq. 0) then
            write(*,*)      '*** ERROR Not enough memory to define ', soort ,' array'
            write(*,*)      '          Contact Deltares'
         else
            write(lundia,*) '*** ERROR Not enough memory to define ', soort ,' array'
            write(lundia,*) '          Contact Deltares'
         endif
         call    CSTOP (1         ,CHAR (0))
      else if (ierr   .eq. 25) then
!-----------------------------------------------------------------------
!-------IERR = 25: wrong type specified
!-----------------------------------------------------------------------
         errpnt = -20000000
!-----------------------------------------------------------------------
!--------Get diagnostic unit
!-----------------------------------------------------------------------
         call    GETLUN(lundia)
!-----------------------------------------------------------------------
!--------Write message to screen (LUNDIA=0) or diagnostic (LUNDIA<>0)
!-----------------------------------------------------------------------
         if (lundia .eq. 0) then
            write(*,*)      '*** ERROR Pointer for ',soort , ' array of wrong type'
            write(*,*)      '          Contact Deltares'
         else
            write(lundia,*) '*** ERROR Pointer for ',soort , ' array of wrong type'
            write(lundia,*) '          Contact Deltares'
         endif
         call    CSTOP (1         ,CHAR (0))
      else if (ierr   .eq. 6) then
!-----------------------------------------------------------------------
!--------IERR = 6: key too long
!-----------------------------------------------------------------------
         errpnt = -30000000
!-----------------------------------------------------------------------
!--------Get diagnostic unit
!-----------------------------------------------------------------------
         call    GETLUN(lundia)
!-----------------------------------------------------------------------
!--------Write message to screen (LUNDIA=0) or diagnostic (LUNDIA<>0)
!-----------------------------------------------------------------------
         if (lundia .eq. 0) then
            write(*,*)      '*** ERROR Pointer name of ',soort , ' array to long'
            write(*,*)      '          Contact Deltares'
         else
            write(lundia,*) '*** ERROR Pointer name of ',soort , ' array to long'
            write(lundia,*) '          Contact Deltares'
         endif
         call    CSTOP (1         ,CHAR (0))
      else if (ierr   .eq. 7) then
!-----------------------------------------------------------------------
!--------IERR = 7: pointer exists with different length and type
!-----------------------------------------------------------------------
         errpnt = -41000000
!-----------------------------------------------------------------------
!--------Get diagnostic unit
!-----------------------------------------------------------------------
         call    GETLUN(lundia)
!-----------------------------------------------------------------------
!--------Write message to screen (LUNDIA=0) or diagnostic (LUNDIA<>0)
!-----------------------------------------------------------------------
         if (lundia .eq. 0) then
            write(*,*)      '*** ERROR Pointer for ',pntnam, ' array of wrong type'
            write(*,*)      '          Contact Deltares'
         else
            write(lundia,*) '*** ERROR Pointer for ',pntnam, ' array of wrong type'
            write(lundia,*) '          Contact Deltares'
         endif
         call    CSTOP (1         ,CHAR (0))
      else if (ierr  .ne. 0) then
!-----------------------------------------------------------------------
!--------Get diagnostic unit
!-----------------------------------------------------------------------
         call    GETLUN(lundia)
!-----------------------------------------------------------------------
!--------Write message to screen (LUNDIA=0) or diagnostic (LUNDIA<>0)
!-----------------------------------------------------------------------
         if (lundia .eq. 0) then
            write(*,*)      '*** ERROR Unexpected memory error: ', ierr  ,' from FMM routine ERRPRT'
            write(*,*)      '          Contact Deltares'
         else
            write(lundia,*) '*** ERROR Unexpected memory error: ', ierr  ,' from FMM routine ERRPRT'
            write(lundia,*) '          Contact Deltares'
         endif
         call    CSTOP (1         ,CHAR (0))
      else
!-----------------------------------------------------------------------
!--------IERR = 0, should not be possible for GETPRT calls
!-----------------------------------------------------------------------
         if (callty .eq. 'getprt') then
!-----------------------------------------------------------------------
!-----------Get diagnostic unit
!-----------------------------------------------------------------------
            call GETLUN(lundia)
!-----------------------------------------------------------------------
!-----------Write message to screen (LUNDIA=0) or diagnostic (LUNDIA<>0)
!-----------------------------------------------------------------------
            if (lundia .eq. 0) then
               write(*,*)      '*** ERROR Unexpected memory error: ', ierr  ,' from FMM routine ERRPRT'
               write(*,*)      '          Contact Deltares'
            else
               write(lundia,*) '*** ERROR Unexpected memory error: ', ierr  ,' from FMM routine ERRPRT'
               write(lundia,*) '          Contact Deltares'
            endif
            call CSTOP (1         ,CHAR (0))
         endif
      endif
!-----------------------------------------------------------------------
      end
!***********************************************************************
      subroutine CHNULL(charr     ,length      )
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: SUBROUTINE CHNULL
!           Function: Initialize Character array with blanks
!        Method used:
!               Date: 20-08-1998
!         Programmer: Heleen Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              MKCPNT
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! CHARR   IO CH*1  LENGTH          Character array to initialize
! LENGTH  I   I*4                  Total required array length
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IC          I*4                  Loop variable
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      integer     length,ic
!
      character*1 charr
!
      dimension   charr (length)
!
      do 110 ic = 1,length
         charr (ic) = ' '
  110 continue
!-----------------------------------------------------------------------
      end
!***********************************************************************
      subroutine INULL (iarr      ,length      )
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: SUBROUTINE INULL
!           Function: Initialize Integer array with zero
!        Method used:
!               Date: 20-08-1998
!         Programmer: Heleen Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              MKIPNT
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! IARR    IO  I*4  LENGTH          Integer array to initialize
! LENGTH  I   I*4                  Total required array length
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! II          I*4                  Loop variable
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      integer     length,ii
      integer     iarr
!
      dimension   iarr  (length)
!
      do 110 ii = 1,length
         iarr  (ii) = 0
  110 continue
!-----------------------------------------------------------------------
      end
!***********************************************************************
      subroutine LNULL (larr      ,length      )
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: SUBROUTINE LNULL
!           Function: Initialise Logical array for .false.
!        Method used:
!               Date: 20-08-1998
!         Programmer: Heleen Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              MKLPNT
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LARR    IO  L*4  LENGTH          Logical array to initialize
! LENGTH  I   I*4                  Total required array length
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IL          I*4                  Loop variable
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      integer     length,il
!
      logical     larr
!
      dimension   larr  (length)
!
      do 110 il = 1,length
         larr  (il) = .false.
  110 continue
!-----------------------------------------------------------------------
      end
!***********************************************************************
      subroutine RNULL (rarr      ,length      )
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: SUBROUTINE RNULL
!           Function: Initialise Real array for zero
!        Method used:
!               Date: 20-08-1998
!         Programmer: Heleen Leepel
!         CVS header
!            $Author: Koster $
!              $Date: 22-01-01 15:36 $
!            $Source: /u/trisula/cvsroot/trisula/main/memlib.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routine :              MKRPNT
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! LENGTH  I   I*4                  Total required array length
! RARR    IO  R*4  LENGTH          Real array to initialize
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IR          I*4                  Loop variable
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      integer     length,ir
!
      real        rarr
!
      dimension   rarr  (length)
!
      do 110 ir = 1,length
         rarr  (ir) = 0.0
  110 continue
!-----------------------------------------------------------------------
      end
