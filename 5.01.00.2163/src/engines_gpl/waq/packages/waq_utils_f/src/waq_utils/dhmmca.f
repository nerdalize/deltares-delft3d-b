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

      module dhmmca_mod
      contains
      subroutine dhmmca ( lunrep, l_decl, arrpoi, arrtyp, arrbyt,
     &                    arrlen, arrknd, arrdm1, arrdm2, arrdm3,
     &                    arrnam, itotc,  part )

!     Deltares Software Centre

!     Function            : Sets the array pointers in the SYSC common
!                           block. Gives array space of the kind C(pointer)
!                           Declares memory through C-interface if asked
!                           (routine is also called by preprocessor)

!     Created             : Feb. 1997 by Jan van Beek
!     Modified            : May  2010 by Leo Postma
!                           Routine brought in line with real and integer version

!     Files               : LUNREP - monitoring output file

!     Routines            : SRSTOP, stops execution (on error)

      use partition_arrays ! module for computing the pointers into the arrays

      implicit none

!     Parameters          :

!     kind     function         name        description

      integer      , intent(in   ) :: lunrep    ! logical unitnumber output file
      logical      , intent(in   ) :: l_decl    ! Declare memory y/n
!     integer      , intent(in   ) :: noarr     ! dimension of parameter arrays, contained in 'sysn.inc'
      integer      , intent(inout) :: arrpoi(:) ! Pointer in workarray/FMM reference pointer
      integer      , intent(inout) :: arrtyp(:) ! Array type ( INT=,REAL=,CHAR= ), see FMM/NEFIS
      integer      , intent(inout) :: arrbyt(:) ! Number of bytes per element, see FMM/NEFIS
      integer      , intent(inout) :: arrlen(:) ! Length off array
      integer      , intent(inout) :: arrknd(:) ! Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR)
      integer      , intent(inout) :: arrdm1(:) ! dimension 1
      integer      , intent(inout) :: arrdm2(:) ! dimension 2
      integer      , intent(inout) :: arrdm3(:) ! dimension 3 ( number of grids mostly )
      character(20), intent(inout) :: arrnam(:) ! Array name
      integer      , intent(inout) :: itotc     ! Required array space
      type(memory_partition), intent(inout) :: part ! Private variables for MAKPTR

!     COMMON  /  SYSN   /   System characteristics

      INCLUDE 'sysn.inc'

!     COMMON  /  SYSI   /   Timer characteristics

      INCLUDE 'sysi.inc'

!     COMMON  /  SYSA   /   Pointers in real array workspace

      INCLUDE 'sysa.inc'

!     COMMON  /  SYSJ   /   Pointers in integer array workspace

      INCLUDE 'sysj.inc'

!     COMMON  /  SYSC   /   Pointers in character array workspace

      INCLUDE 'sysc.inc'

!     Local declarations

      character*20       :: namarr                      ! help variable for array name
      integer, parameter :: nr_car = icsize             ! total number of arrays
      integer            :: ip_car(nr_car)              ! help array to fill the common block / SYSA /
      equivalence   ( ianam  , ip_car(1) )              ! first entry equivalences with first entry common block

      integer            :: iianam, iimnam, iisnam, iidnam, iibnid,
     &                      iibnam, iibtyp, iiwsid, iiwnam, iiwtyp,
     &                      iicnam, iipnam, iifnam, iisfna, iiedit,
     &                      iiprna, iionam, iidina, iivnam, iidana,
     &                      iirnam, iicbuf, iilunt
      integer            :: i_car,  iartyp, iarlen, ip
C
      IIANAM = IASIZE + IJSIZE +  1
      IIMNAM = IASIZE + IJSIZE +  2
      IISNAM = IASIZE + IJSIZE +  3
      IIDNAM = IASIZE + IJSIZE +  4
      IIBNID = IASIZE + IJSIZE +  5
      IIBNAM = IASIZE + IJSIZE +  6
      IIBTYP = IASIZE + IJSIZE +  7
      IIWSID = IASIZE + IJSIZE +  8
      IIWNAM = IASIZE + IJSIZE +  9
      IIWTYP = IASIZE + IJSIZE + 10
      IICNAM = IASIZE + IJSIZE + 11
      IIPNAM = IASIZE + IJSIZE + 12
      IIFNAM = IASIZE + IJSIZE + 13
      IISFNA = IASIZE + IJSIZE + 14
      IIEDIT = IASIZE + IJSIZE + 15
      IIPRNA = IASIZE + IJSIZE + 16
      IIONAM = IASIZE + IJSIZE + 17
      IIDINA = IASIZE + IJSIZE + 18
      IIVNAM = IASIZE + IJSIZE + 19
      IIDANA = IASIZE + IJSIZE + 20
      IIRNAM = IASIZE + IJSIZE + 21
      IICBUF = IASIZE + IJSIZE + 22
      IILUNT = IASIZE + IJSIZE + 23
C
C     Set defaults, no name no length
C     Don't declare the first array , ARRNAM
C
      DO I_CAR = IASIZE + IJSIZE + 1 , IASIZE + IJSIZE + NR_CAR
         ARRNAM(I_CAR) = ' '
         ARRTYP(I_CAR) = CHTYP
         ARRBYT(I_CAR) = 4
         ARRKND(I_CAR) = 1
         ARRDM1(I_CAR) = 0
         ARRDM2(I_CAR) = 1
         ARRDM3(I_CAR) = 1
         ARRLEN(I_CAR) = 0
      ENDDO
C
C     Set the characteristics
C
      ARRNAM(IIMNAM) = 'MNAME '
      ARRDM1(IIMNAM) = 8
C
      ARRNAM(IISNAM) = 'SNAME '
      ARRDM1(IISNAM) = NOTOT
C
      ARRNAM(IIDNAM) = 'DNAME '
      ARRDM1(IIDNAM) = NODUMP
C
      ARRNAM(IIBNID) = 'BNDID '
      ARRDM1(IIBNID) = NOBND
C
      ARRNAM(IIBNAM) = 'BNAME '
      ARRDM1(IIBNAM) = NOBND*2
C
      ARRNAM(IIBTYP) = 'BNTYP '
      ARRDM1(IIBTYP) = NOBTYP
C
      ARRNAM(IIWSID) = 'WASTID'
      ARRDM1(IIWSID) = NOWST
C
      ARRNAM(IIWNAM) = 'WNAME '
      ARRDM1(IIWNAM) = NOWST*2
C
      ARRNAM(IIWTYP) = 'WTYPE '
      ARRDM1(IIWTYP) = NOWTYP
C
      ARRNAM(IICNAM) = 'CONAM '
      ARRDM1(IICNAM) = NOCONS
C
      ARRNAM(IIPNAM) = 'PANAM '
      ARRDM1(IIPNAM) = NOPA
C
      ARRNAM(IIFNAM) = 'FUNAM '
      ARRDM1(IIFNAM) = NOFUN
C
      ARRNAM(IISFNA) = 'SFNAM '
      ARRDM1(IISFNA) = NOSFUN
C
      ARRNAM(IIEDIT) = 'CGRID '
      ARRDM1(IIEDIT) = NY*6
C
      ARRNAM(IIPRNA) = 'PRNAM '
      ARRDM1(IIPRNA) = NPROC
C
      ARRNAM(IIONAM) = 'OUNAM '
      ARRDM1(IIONAM) = NRVART
C
      ARRNAM(IIDINA) = 'DINAM '
      ARRDM1(IIDINA) = NODISP
C
      ARRNAM(IIVNAM) = 'VENAM '
      ARRDM1(IIVNAM) = NOVELO
C
      ARRNAM(IIDANA) = 'DANAM '
      ARRDM1(IIDANA) = NDMPAR
C
      ARRNAM(IIRNAM) = 'RANAM '
      ARRDM1(IIRNAM) = NORAAI
C
      ARRNAM(IICBUF) = 'CBUFF '
      ARRDM1(IICBUF) = NCBUFM
C
      ARRNAM(IILUNT) = 'LUNT  '
      ARRDM1(IILUNT) = NUFIL*10

!     the total array length

      if ( .not. l_decl ) then
         write ( 328, '(/a/a/)' ) "  => CHARACTER arrays 4-byte words <=",
     &                            "  nr array name            array size"
      endif

!     Variables are 'CHARACTER(20), we count characters in 4 byte words, FMM in 1

      itotc = 0
      do i_car = iasize + ijsize + 1 , iasize + ijsize + nr_car
         arrlen(i_car) = arrdm1(i_car)*arrdm2(i_car)*arrdm3(i_car)*5
         if ( .not. l_decl ) write ( 328, 2040 ) i_car-iasize-ijsize, arrnam(i_car), arrlen(i_car)
         itotc         = itotc + arrlen(i_car)
         arrlen(i_car) = arrlen(i_car)*4
      enddo
cjvb                                     ??lp
c     raaien behind the dump areas
c
      ARRLEN(IIDANA) = ARRLEN(IIDANA) + ARRLEN(IIRNAM)
      ARRLEN(IIRNAM) = 0

!     Allocate array's, set pointers in common block
!     Don't declare the first array , ARRNAM

      if ( l_decl ) then
         do i_car = iasize + ijsize + 2 , iasize + ijsize + nr_car
            iartyp = arrtyp(i_car)
            iarlen = arrlen(i_car)
            namarr = arrnam(i_car)
            if ( iarlen .gt. 0 ) then
               ip = makptr(part, namarr, iartyp ,iarlen)
               if ( ip .eq. 0 ) then
                  write(lunrep,2010) namarr
                  call srstop(1)
               endif
!jvb           itotc = itotc + arrlen(i_car)
            else
               ip = 0
            endif

!           Add one extra because of the shift between ibuf(0) and j(1)

            ip = ip + 1
            ip_car(i_car-iasize-ijsize) = ip
            arrpoi(i_car)               = ip
         enddo
      endif
      if ( .not. l_decl ) write ( 328, '(/5x,a20,i12)' ) "Total (4 byte words)",itotc
cjvb                                     ??lp
c     raaien behind the dump areas
c
      IP_CAR(IIRNAM-IASIZE-IJSIZE)=IP_CAR(IIDANA-IASIZE-IJSIZE)+
     +                             NDMPAR*20
      ARRPOI(IIRNAM) = ARRPOI(IIDANA) + NDMPAR*20
      ITOTC = ITOTC + NDMPAR
cjvb
C
      return

 2000 format ( ' total character array space: ',I8)
 2010 format ( ' ERROR  : allocating character array. Name   : ',A)
 2040 format (   i4,1x,a20,i12 )

      end subroutine
      end module dhmmca_mod
