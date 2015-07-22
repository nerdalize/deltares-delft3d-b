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

      subroutine pdfch1 ( lurep   , runnam, nosys , syname, procha,
     +                    allitems)

      ! defines variable part of CHARON coupling interface

      use processet
      implicit none

      ! declaration of arguments

      integer               :: lurep               ! unit number report file
      integer               :: nosys               ! number of transportable substances
      character(len=*)      :: runnam              ! Charon run name (input file)
      character(len=20)     :: syname(*)           ! substance names
      type(ProcesProp)      :: procha              ! charon process definition
      type(itempropcoll)    :: allitems            ! all items of the proces system
C
C     Local declarations
C
      INTEGER         LUPDF , MMAX  , NAIJ2 , NMAX  , NNOTRA,
     J                LUIC  , LUOC  , I     , NALTOX,
     J                NTOX  , NALADS, NALSPE, NALSP2, N1MAX , N2MAX
      PARAMETER      (MMAX = 1000, N1MAX = 1000, NMAX = 1000,
     J                N2MAX = 1000)
      PARAMETER      (NALTOX = 7, NALADS = 6, NALSPE = 3, NALSP2 = 2)
      CHARACTER*6     VERSIO, NAMTOX(MMAX)
      CHARACTER*10    ALIADS(2,NALADS),ALISPE(2,NALSPE),
     J                ALISP2(2,NALSP2),ALITOX(2,NALTOX)
      INTEGER         ICTOX(MMAX)
      INTEGER         ITTTOB(N1MAX), IBTTOB(N1MAX),
     J                ICCTOT(N2MAX), ITCTOT(N2MAX), IOUTP(NMAX)
      REAL            ABTTOB(N1MAX), RMTTOB(N1MAX),
     J                ABCTOT(N2MAX), RMCTOT(N2MAX)
      type(ItemProp)        :: aItemProp           ! one item
      type(IOitemProp)      :: aIOitemProp         ! one IOitem
      type(IOitemPropColl)  :: input_item          ! one collection of items
      type(IOitemPropColl)  :: output_item         ! one collection of items
      type(IOitemPropColl)  :: FluxOutput          ! one collection of items
      type(StochiProp)      :: aStochiProp         ! one Stochi
      type(StochiPropColl)  :: FluxStochi          ! one collection of Stochis
      integer               :: mlevel              ! monitoring level
      integer               :: indx                ! index in item list
      integer               :: iret                ! return code
      integer               :: ip                  ! index in io list
      integer               :: nttob               ! nttob
      integer               :: nctot               ! nctot

      INCLUDE 'charon.inc'

C     Definitie unitnrs files

      DATA LUPDF,LUIC,LUOC / 13,11,12 /

C     Aliases toxical substances:
C          if first name occurs in CHARON component vector
C          it will be remembered in order to write dissolved
C          concentrations and quality of solid phase in DELWAQ,
C          second name is DW4 equivalent,
C          also used to manipulate species-array

      DATA ALITOX /'CD++      ','CD        ','CU++      ','CU        ',
     J             'ZN++      ','ZN        ','NI++      ','NI        ',
     J             'CR+++     ','CR        ','HG        ','HG        ',
     J             'PB++      ','PB        '/

C     Aliases for adsorbents:
C          if first name occurs in TRANSPORTED VECTOR
C          it will be replaced by the second one for DELWAQ
C          in order to find masses in TRANSPORTED VECTOR
c
c     DATA ALIADS /'CEC   _par','IM1       ','CEC   _tot','IM1       ',
c    J             'FEADS _par','AdsFer    ','FEADS _par','AdsFer    ',
c    J             'HHUM  _dis','DOC       ','HHUM  _dis','DOC       '/
c
C     Aliases for species
C          if first name occurs in CHARON species vector
C          it will be replaced by the second one for DELWAQ
C          in order to define substances that influence DW4 processes
C          (species molar mass in CHARON input will be applied!!)

      DATA ALISPE/ 'O2        ','OXY       ','CL-       ','CL        ',
     J             'NO3-      ','NO3       '/
C    J             'NO3-      ','NO3       ','PARTP     ','AAP       '/

C     Aliases for species NH4 and PO4
C          to be used after writing the process definition files
C          species NH4 and PO4 are explicitly computed by process CHARON
C          but NH4 and PO4 stoichiometry terms should be recognized!!
C
C     DATA ALISP2/ 'NH4+      ','NH4       ','PO4---    ','PO4       '/

C
C     get monitoring level
C
      CALL GETMMO(MLEVEL)
      WRITE(LUREP,*)
      WRITE(LUREP,*) ' Determinig the relation between the substances',
     +               ' and the CHEM species'

C     Read input of CHARON (also opening of files)

      CALL CHINP2 (RUNNAM, LUIC  , LUOC  )
C
C     Set DELWAQ active substances as transport vector, skip non charon components or species
C
      IF ( NOSYS .GT. MAXN ) THEN
         WRITE(LUREP,*)
     +    'ERROR: number of DELWAQ active substances exceeds maximum:',
     +    NOSYS,'>',MAXN
         WRITE(LUREP,*)
     +    'reduce substances or contact WL|Deltares'
         CALL SRSTOP(1)
      ENDIF
      NTRANS = 0
      IF (MLEVEL.GE.10) WRITE(LUREP,*) 'TRANSPORTED VECTOR'
      DO I = 1 , NOSYS
         VARNAM(NTRANS+1) = SYNAME(I)
c        CALL CHALI2 ( 1 , VARNAM(NTRANS+1) , 10 , NALADS , ALIADS )
         CALL CHALI2 ( 1 , VARNAM(NTRANS+1) , 10 , NALSPE , ALISPE )
         IF ( VARNAM(NTRANS+1)(7:10) .EQ. '    ') THEN
C
C           I am unsure of this one, see chmapd
C
            CALL ZOEK ( VARNAM(NTRANS+1)(1:6),N,KN,6,INDX)
         ELSE
            CALL ZOEK ( VARNAM(NTRANS+1)(1:6),M,NR,6,INDX)
         ENDIF
         IF ( INDX .GT. 0 ) THEN
            NTRANS = NTRANS + 1
            IF (MLEVEL.GE.10)
     +         WRITE(LUREP,'(I4,2X,A10)') NTRANS,VARNAM(NTRANS)
         ENDIF
      ENDDO
C
C     Write transport vector after charon.inp
C
      WRITE ( LUIC , '(A)' ) 'TRANSPORT VECTOR'
      DO I = 1 , NTRANS
         WRITE ( LUIC , '(6X,A10)' ) VARNAM(I)
      ENDDO
      WRITE ( LUIC , '(A)' ) 'END'
      CLOSE(LUIC)

C     Check number of components for local arrays

      IF ( M .GT. MMAX ) THEN
         WRITE(LUREP,*) 'ERROR: M exceeds maximum:',M,'>',MMAX
         WRITE(LUREP,*) 'local dimensioning error, reduce M or contact'
         WRITE(LUREP,*) 'WL|Deltares'
         CALL SRSTOP(1)
      ENDIF
      IF ( N .GT. NMAX ) THEN
         WRITE(LUREP,*)
     +    'ERROR: number of Charon species exceeds maximum:',N,'>',NMAX
         WRITE(LUREP,*)
     +    'reduce species or contact WL|Deltares'
         CALL SRSTOP(1)
      ENDIF

C     Find last index of first phase in arrays with dimension NAIJ
C     (arrays with all non-zero entries in stoichiometric matrix)

      CALL CHPHAS ( NAIJ2 )

C     Define mapping of Charon-Delwaq systems,
C     administration arrays used in writing PDF for CHARON
C     and in coupling CHARON and DELWAQ (see module for definition)

      CALL  CHMAPD ( NAIJ2 , MMAX  , NMAX  , N1MAX , N2MAX ,
     J               NTTOB , ITTTOB, IBTTOB, ABTTOB, RMTTOB,
     J               NCTOT , ICCTOT, ITCTOT, ABCTOT, RMCTOT,
     J               IOUTP , NNOTRA, NALTOX, NTOX  , ICTOX , ALITOX)

C     Replace names of adsorbents by DELWAQ-equivalents

c     CALL CHALIA ( NTRANS, VARNAM , 10 , NALADS , ALIADS )

C     Put DELWAQ-aliasses in species names insofar as they influence
C     DW4 processes (exclude NH4 and PO4 because they are output CHARON)
C     Put DELWAQ-aliasses in TRANSPORTED VECTOR as well

      CALL  CHALIA ( N      , KN     , 6  , NALSPE , ALISPE )
      CALL  CHALIA ( NTRANS , VARNAM , 10 , NALSPE , ALISPE )

C     Write CHARON process

      DO 15 I =1,NTOX
   15 NAMTOX(I) = NR(ICTOX(I),1)
c
c     test call old routine
c
c     CALL WRICHA ( LUPDF , NTRANS, VARNAM, NNOTRA, IOUTP ,
c    J              N     , KN    , NTOX  , NAMTOX)
C
C     Check local maxima with interface maxima
C
C
C     Map local variables to interface variables
C
      ! Getransporteerde vector

      input_item%maxsize  = 0
      input_item%cursize  = 0
      do i = 1 , ntrans
         aitemprop%name         = varnam(i)
         aitemprop%text         = varnam(i)//' element of transport vector    (g/m3)'
         iret = ItemPropCollAdd( AllItems, aItemProp )
         aioitemprop%item => allitems%itemproppnts(iret)%pnt
         aioitemprop%name   = varnam(i)
         aioitemprop%actdef = -999.
         aioitemprop%indx   = i
         aioitemprop%ip_val = 0
         aioitemprop%type = IOTYPE_SEGMENT_INPUT
         indx = i
         iret = ioitempropcolladdindx( input_item , aioitemprop , indx )
      enddo

      ! CHARON compounds

      output_item%maxsize  = 0
      output_item%cursize  = 0
      ip = 0
      do i = 1,n
         if ( ioutp(i) .gt. 0 ) then
            ip = ip + 1
            aitemprop%name         = kn(i)
            aitemprop%text         = kn(i)//' Charon species                 (g/m3)'
            iret = ItemPropCollAdd( AllItems, aItemProp )
            aioitemprop%item => allitems%itemproppnts(iret)%pnt
            aioitemprop%name   = kn(i)
            aioitemprop%indx   = ip
            aioitemprop%ip_val = 0
            aioitemprop%type = IOTYPE_SEGMENT_OUTPUT
            indx = ip
            iret = ioitempropcolladdindx( output_item, aioitemprop , indx )
         endif
      enddo

      ! Concentration of solid metals

      do i = 1,ntox
         ip = i + nnotra
         aitemprop%name         = 'Q'//namtox(i)(1:2)//'IM1'
         aitemprop%text         = namtox(i)(1:2)//' quality of solid phase  (g/gDM)'
         iret = ItemPropCollAdd( AllItems, aItemProp )
         aioitemprop%item => allitems%itemproppnts(iret)%pnt
         aioitemprop%name   = 'Q'//namtox(i)(1:2)//'IM1'
         aioitemprop%indx   = ip
         aioitemprop%ip_val = 0
         aioitemprop%type = IOTYPE_SEGMENT_OUTPUT
         indx = ip
         iret = ioitempropcolladdindx( output_item, aioitemprop , indx )
      ENDDO

      ! Dissolved fraction metals

      do i = 1,ntox
         ip = i + ntox + nnotra
         aitemprop%name         = 'Fr'//namtox(i)(1:2)//'Dis'
         aitemprop%text         = namtox(i)(1:2)//' dissolved fraction          (-)'
         iret = ItemPropCollAdd( AllItems, aItemProp )
         aioitemprop%item => allitems%itemproppnts(iret)%pnt
         aioitemprop%name   = 'Fr'//namtox(i)(1:2)//'Dis'
         aioitemprop%indx   = ip
         aioitemprop%ip_val = 0
         aioitemprop%type = IOTYPE_SEGMENT_OUTPUT
         indx = ip
         iret = ioitempropcolladdindx( output_item, aioitemprop , indx )
      enddo

      ! Dissolved fraction metals

      do i = 1,ntox
         ip = i + 2*ntox + nnotra
         aitemprop%name         = 'Kd'//namtox(i)(1:2)//'CEC'
         aitemprop%text         = namtox(i)(1:2)//' partition coefficient   (m3/kg)'
         iret = ItemPropCollAdd( AllItems, aItemProp )
         aioitemprop%item => allitems%itemproppnts(iret)%pnt
         aioitemprop%name   = 'Fr'//namtox(i)(1:2)//'Dis'
         aioitemprop%indx   = ip
         aioitemprop%ip_val = 0
         aioitemprop%type = IOTYPE_SEGMENT_OUTPUT
         indx = ip
         iret = ioitempropcolladdindx( output_item, aioitemprop , indx )
      enddo

      ! Fluxes

      fluxoutput%maxsize  = 0
      fluxoutput%cursize  = 0
      do i = 1,ntrans
         aitemprop%name         = 'c'//varnam(i)(1:6)//varnam(i)(8:10)
         aitemprop%text         = 'c'//varnam(i)(1:6)//varnam(i)(8:10)//' Charon derivative            (g/m3/d)'
         iret = ItemPropCollAdd( AllItems, aItemProp )
         aioitemprop%item => allitems%itemproppnts(iret)%pnt
         aioitemprop%name   = 'c'//varnam(i)(1:6)//varnam(i)(8:10)
         aioitemprop%indx   = i
         aioitemprop%ip_val = 0
         aioitemprop%type = IOTYPE_SEGMENT_OUTPUT
         indx = i
         iret = ioitempropcolladdindx( fluxoutput, aioitemprop , indx )
      enddo

      ! Stochiometry

      fluxstochi%maxsize  = 0
      fluxstochi%cursize  = 0
      do i = 1,ntrans
         aStochiProp%type      = STOCHITYPE_FLUX
         aStochiProp%ioitem    = 'c'//varnam(i)(1:6)//varnam(i)(8:10)
         aStochiProp%substance = varnam(i)
         aStochiProp%subindx   = 0
         aStochiProp%scale     = 1.0
         iret = StochiPropCollAdd( FluxStochi , aStochiProp )
      enddo

      procha%name         = 'Charon'
      procha%text         = 'Charon'
      procha%routine      = 'D40CHA'
      procha%swtransp     = 123
      procha%linvok       = .false.
      procha%active       = .false.
      procha%no_input     = input_item%cursize
      procha%input_item   =>input_item%IOitemProps
      procha%no_output    = output_item%cursize
      procha%output_item  =>output_item%IOitemProps
      procha%no_FluxOutput= FluxOutput%cursize
      procha%FluxOutput   =>FluxOutput%IOitemProps
      procha%no_FluxStochi= FluxStochi%cursize
      procha%FluxStochi   =>FluxStochi%StochiProps
      procha%no_DispStochi= 0
      procha%no_VeloStochi= 0
C
C     Report on coupling
C
      WRITE ( LUREP , '(/
     J ''Arrays describing composition of CHARON B-vector:''/
     J ''B[mol] = B[mol] + T[g] * A[mol/mol] / RM[g/mol]''/
     J ''T          B              A        RM''             )' )
      WRITE ( LUREP , '(A10,1X,A6,2F10.3)' )
     J    ( VARNAM(ITTTOB(I)),
     J      NR(IBTTOB(I),1),
     J      ABTTOB(I),
     J      RMTTOB(I),             I = 1,NTTOB   )

      WRITE ( LUREP , '(/
     J ''Arrays describing reconstr. of T-vector:''/
     J ''T[g] = T[g] + X[mol] * A[mol/mol] * RM[g/mol]''/
     J ''X      T                  A        RM''              )' )
      WRITE ( LUREP , '(A6,1X,A10,2F10.3)' )
     J    ( KN(ICCTOT(I)),
     J      VARNAM(ITCTOT(I)),
     J      ABCTOT(I),
     J      RMCTOT(I),              I = 1,NCTOT   )

C     Put DELWAQ-aliasses in species names and transported vector
C     insofar as they are used to process stoichiometry
C     Include NH4 and PO4, Yes or No?????

      CALL  CHALIA ( N      , KN     , 6  , NALTOX , ALITOX )
      CALL  CHALIA ( NTRANS , VARNAM , 10 , NALTOX , ALITOX )
C
      RETURN
      END
