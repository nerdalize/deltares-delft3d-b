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

      SUBROUTINE CNVPRO ( LUREP , procesdef, NBPRX , BPRNAM, BPRTXT,
     +                    MODNAM, NSVAI    , INS_ID, INS_NM, INS_VA,
     +                    NSVAO , OUS_ID   , OUS_NM, NBFL  , FLU_ID,
     +                    FLU_NM, NBST     , STO_SU, STO_FL, STO_SC,
     +                                       IPVAI , IPVAO , IPBFL ,
     +                    IPBST , NSVXI    , IPVXI , INE_ID, INE_NM,
     +                    INE_VA, NSVXO    , IPVXO , OUE_ID, OUE_NM,
     +                    NDST  , IPDST    , DIS_SU, DIS_IT, DIS_SC,
     +                    NVST  , IPVST    , VEL_SU, VEL_IT, VEL_SC,
     +                    ISWITR, NSVICH   , NSVOCH, NFLCH , NSTCH ,
     +                            NOINFO   , NOWARN, IERROR, VAIORD,
     +                    VXIORD, VAOORD   , VXOORD)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: nov -1992 by Jan van Beek
C
C     FUNCTION            : convert procesdef structure to old structure
C
C     LOGICAL UNITNUMBERS : LUREP   - report file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUREP   INTEGER       1     INPUT   Report file
C     ProcesDef                   INPUT   the proces definition
C     NBPR    INTEGER       1     OUTPUT  Number of processes in def file
C     BPRNAM  CHARACTER*(*) *     OUTPUT  Name of processes
C     BPRTXT  CHARACTER*(*) *     OUTPUT  Text of processes
C     MODNAM  CHARACTER*(*) *     OUTPUT  Name of module of processes
C     NSVAI   INTEGER       *     OUTPUT  No of input vars per proces
C     INS_ID  CHARACTER*(*) *     OUTPUT  Name of input variable
C     INS_NM  CHARACTER*(*) *     OUTPUT  Text of input variable
C     INS_VA  REAL          *,*   OUTPUT  Default values input variables
C     NSVAO   INTEGER       *     OUTPUT  No of output vars per proces
C     OUS_ID  CHARACTER*(*) *     OUTPUT  Name of input variable
C     OUS_NM  CHARACTER*(*) *     OUTPUT  Text of output variable
C     NBFL    INTEGER       *     OUTPUT  No of basic fluxes per proces
C     FLU_ID  CHARACTER*(*) *     OUTPUT  Name of basix fluxe
C     FLU_NM  CHARACTER*(*) *     OUTPUT  Text of basix fluxe
C     NBST    INTEGER       *     OUTPUT  No of basic stochis per proces
C     STO_SU  CHARACTER*(*) *,*   OUTPUT  Name of substance in stochi
C     STO_FL  CHARACTER*(*) *,*   OUTPUT  Name of flux in stochi
C     STO_SC  REAL          *,*   OUTPUT  Stochimetric factor
C     IPVAI   INTEGER       *     OUTPUT  Pointers for arrays on VAI
C     IPVAO   INTEGER       *     OUTPUT  Pointers for arrays on VAO
C     IPBFL   INTEGER       *     OUTPUT  Pointers for arrays on BFL
C     IPBST   INTEGER       *     OUTPUT  Pointers for arrays on BST
C     NSVXI   INTEGER       *     OUTPUT  No of input vars X per proces
C     IPVXI   INTEGER       *     OUTPUT  Pointers for arrays on VXI
C     INE_ID  CHARACTER*(*) *     OUTPUT  Name of input variable X
C     INE_NM  CHARACTER*(*) *     OUTPUT  Text of input variable X
C     INE_VA  REAL          *     OUTPUT  Default values input X variables
C     NSVXO   INTEGER       *     OUTPUT  No of output vars X per proces
C     IPVXO   INTEGER       *     OUTPUT  Pointers for arrays on VXO
C     OUE_ID  CHARACTER*(*) *     OUTPUT  Name of output variable X
C     OUE_NM  CHARACTER*(*) *     OUTPUT  Text of output variable X
C     NSDST   INTEGER       *     OUTPUT  No of dispersion rules p.proces
C     IPDST   INTEGER       *     OUTPUT  Pointers for arrays on DST
C     DIS_SU  CHARACTER*(*) *     OUTPUT  Name of substance in disp rule
C     DIS_IT  CHARACTER*(*) *     OUTPUT  Name of output item in disp rule
C     VEL_SC  REAL          *     OUTPUT  factor in dispersion rule
C     NSVST   INTEGER       *     OUTPUT  No of velocity rules p.proces
C     IPVST   INTEGER       *     OUTPUT  Pointers for arrays on VST
C     VEL_SU  CHARACTER*(*) *     OUTPUT  Name of substance in velo rule
C     VEL_IT  CHARACTER*(*) *     OUTPUT  Name of output item in velo rule
C     VEL_SC  REAL          *     OUTPUT  factor in velocity rule
C     ISWITR  INTEGER       *     OUTPUT  Target dimension indicator
C     NSVICH  INTEGER       1     INPUT   No of extra input variables for charon
C     NSVOCH  INTEGER       1     INPUT   No of extra output variables for charon
C     NFLCH   INTEGER       1     INPUT   No of fluxes for charon
C     NSTCH   INTEGER       1     INPUT   No of stochiometric terms for charon
C     NOINFO  INTEGER       1     IN/OUT  Cummulative information count
C     NOWARN  INTEGER       1     IN/OUT  Cummulative warning count
C     IERROR  INTEGER       1     OUTPUT  Error indicatior
C
C
      USE ProcesSet ! all definitions of structures used
C
C     IMPLICIT NONE for extra compiler checks
C
      use timers       !   performance timers

      IMPLICIT NONE
C
C     Declaration of arguments
C
      type(ProcesPropColl)  :: procesdef           ! the proces definition
      INTEGER                          LUREP           ,
     +               NBPRX           ,
     +               NSVICH          , NSVOCH          ,
     +               NFLCH           , NSTCH           ,
     +               NOINFO          , NOWARN          ,
     +               IERROR
      INTEGER      :: NSVAI(*)
      INTEGER      :: NSVAO(*)
      INTEGER      :: NBFL(*)
      INTEGER      :: NBST(*)
      INTEGER      :: IPVAI(*)
      INTEGER      :: IPVAO(*)
      INTEGER      :: IPBFL(*)
      INTEGER      :: IPBST(*)
      INTEGER      :: NSVXI(*)
      INTEGER      :: IPVXI(*)
      INTEGER      :: NSVXO(*)
      INTEGER      :: IPVXO(*)
      INTEGER      :: NDST(*)
      INTEGER      :: IPDST(*)
      INTEGER      :: NVST(*)
      INTEGER      :: IPVST(*)
      INTEGER      :: ISWITR(*)
      REAL         :: INS_VA(*)
      REAL         :: STO_SC(*)
      REAL         :: INE_VA(*)
      REAL         :: DIS_SC(*)
      REAL         :: VEL_SC(*)
      CHARACTER*10 :: BPRNAM(*)
      CHARACTER*10 :: MODNAM(*)
      CHARACTER*20 :: INS_ID(*)
      CHARACTER*20 :: OUS_ID(*)
      CHARACTER*10 :: FLU_ID(*)
      CHARACTER*10 :: STO_SU(*)
      CHARACTER*10 :: STO_FL(*)
      CHARACTER*20 :: INE_ID(*)
      CHARACTER*20 :: OUE_ID(*)
      CHARACTER*10 :: DIS_SU(*)
      CHARACTER*10 :: DIS_IT(*)
      CHARACTER*10 :: VEL_SU(*)
      CHARACTER*10 :: VEL_IT(*)
      CHARACTER*50 :: BPRTXT(*)
      CHARACTER*50 :: INS_NM(*)
      CHARACTER*50 :: OUS_NM(*)
      CHARACTER*50 :: FLU_NM(*)
      CHARACTER*50 :: INE_NM(*)
      CHARACTER*50 :: OUE_NM(*)
      INTEGER      :: VAIORD(*)
      INTEGER      :: VXIORD(*)
      INTEGER      :: VAOORD(*)
      INTEGER      :: VXOORD(*)
C
C     Local declarations
C
      INTEGER        INS   , INE   , OUS   , OUE   , FLU   ,
     +               STO   , DIS   , VEL
      integer      :: nproc           ! number of processes
      integer      :: iproc           ! loop counter processes
      integer      :: i_input         ! loop counter input items
      integer      :: i_output        ! loop counter output items
      integer      :: i_disp          ! loop counter dispersion stochi
      integer      :: i_velo          ! loop counter velocity stochi
      integer      :: i_flux          ! loop counter fluxes
      integer      :: i_flst          ! loop counter flux stochi
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "cnvpro", ithndl )
C
C     Some init
C
      IERROR = 0
      ins = 0
      ine = 0
      ous = 0
      oue = 0
      flu = 0
      sto = 0
      dis = 0
      vel = 0

      ! loop over all processes

      nproc = procesdef%cursize
      do iproc=1,nproc

         ! names and offsets

         BPRNAM(iproc) = procesdef%procesprops(iproc)%name
         BPRTXT(iproc) = procesdef%procesprops(iproc)%text
         MODNAM(iproc) = procesdef%procesprops(iproc)%routine
         ISWITR(iproc) = procesdef%procesprops(iproc)%swtransp
         IPVAI(iproc)  = ins + 1
         IPVXI(iproc)  = ine + 1
         IPVAO(iproc)  = ous + 1
         IPVXO(iproc)  = oue + 1
         IPBFL(iproc)  = flu + 1
         IPBST(iproc)  = sto + 1
         IPDST(iproc)  = dis + 1
         IPVST(iproc)  = vel + 1

         ! input items on segment level/exchange level

         do i_input = 1 , procesdef%procesprops(iproc)%no_input
            if ( procesdef%procesprops(iproc)%input_item(i_input)%type .eq. IOTYPE_SEGMENT_INPUT .or.
     +           procesdef%procesprops(iproc)%input_item(i_input)%type .eq. IOTYPE_SEGMENT_WORK       ) then
               ins   = ins + 1
               ins_id(ins) = procesdef%procesprops(iproc)%input_item(i_input)%name
               ins_nm(ins) = procesdef%procesprops(iproc)%input_item(i_input)%item%text
               ins_va(ins) = procesdef%procesprops(iproc)%input_item(i_input)%actdef
               vaiord(ins) = i_input
            elseif ( procesdef%procesprops(iproc)%input_item(i_input)%type .eq. IOTYPE_EXCHANG_INPUT .or.
     +               procesdef%procesprops(iproc)%input_item(i_input)%type .eq. IOTYPE_EXCHANG_WORK       ) then
               ine = ine + 1
               ine_id(ine) = procesdef%procesprops(iproc)%input_item(i_input)%name
               ine_nm(ine) = procesdef%procesprops(iproc)%input_item(i_input)%item%text
               ine_va(ine) = procesdef%procesprops(iproc)%input_item(i_input)%actdef
               vxiord(ine) = i_input
            else
               ! unknown ( a hole created by a delete, should we handle this? )
               ins   = ins + 1
               ins_id(ins) = 'dummy'
               ins_nm(ins) = 'dummy'
               ins_va(ins) = 0.0
               vaiord(ins) = i_input
            endif
         enddo

         ! output items on segment level/exchange level

         do i_output = 1 , procesdef%procesprops(iproc)%no_output
            if ( procesdef%procesprops(iproc)%output_item(i_output)%type .eq. IOTYPE_SEGMENT_OUTPUT .or.
     +           procesdef%procesprops(iproc)%output_item(i_output)%type .eq. IOTYPE_SEGMENT_WORK       ) then
               ous   = ous + 1
               ous_id(ous) = procesdef%procesprops(iproc)%output_item(i_output)%name
               ous_nm(ous) = procesdef%procesprops(iproc)%output_item(i_output)%item%text
               vaoord(ous) = i_output
            elseif ( procesdef%procesprops(iproc)%output_item(i_output)%type .eq. IOTYPE_EXCHANG_OUTPUT .or.
     +               procesdef%procesprops(iproc)%output_item(i_output)%type .eq. IOTYPE_EXCHANG_WORK       ) then
               oue = oue + 1
               oue_id(oue) = procesdef%procesprops(iproc)%output_item(i_output)%name
               oue_nm(oue) = procesdef%procesprops(iproc)%output_item(i_output)%item%text
               vxoord(oue) = i_output
            else
               ! unknown ( a hole created by a delete, should we handle this? )
               ous   = ous + 1
               ous_id(ous) = 'dummy'
               ous_nm(ous) = 'dummy'
               vaoord(ous) = i_output
            endif
         enddo

         ! dispersion stochi

         do i_disp = 1 , procesdef%procesprops(iproc)%no_dispstochi
            dis = dis + 1
            dis_su(dis) = procesdef%procesprops(iproc)%dispstochi(i_disp)%substance
            dis_it(dis) = procesdef%procesprops(iproc)%dispstochi(i_disp)%ioitem
            dis_sc(dis) = procesdef%procesprops(iproc)%dispstochi(i_disp)%scale
         enddo

         ! velocity stochi

         do i_velo = 1 , procesdef%procesprops(iproc)%no_velostochi
            vel = vel + 1
            vel_su(vel) = procesdef%procesprops(iproc)%velostochi(i_velo)%substance
            vel_it(vel) = procesdef%procesprops(iproc)%velostochi(i_velo)%ioitem
            vel_sc(vel) = procesdef%procesprops(iproc)%velostochi(i_velo)%scale
         enddo

         ! flux

         do i_flux = 1 , procesdef%procesprops(iproc)%no_fluxoutput
            if ( procesdef%procesprops(iproc)%fluxoutput(i_flux)%type .eq. IOTYPE_FLUX ) then
               flu = flu + 1
               flu_id(flu) = procesdef%procesprops(iproc)%fluxoutput(i_flux)%name
               flu_nm(flu) = procesdef%procesprops(iproc)%fluxoutput(i_flux)%item%text
            else
               ! unknown ( a hole created by a delete, should we handle this? )
               flu = flu + 1
               flu_id(flu) = 'dummy flux'
               flu_nm(flu) = 'dummy flux'
            endif
         enddo

         ! flux stochi

         do i_flst = 1 , procesdef%procesprops(iproc)%no_fluxstochi
            sto = sto + 1
            sto_su(sto) = procesdef%procesprops(iproc)%fluxstochi(i_flst)%substance
            sto_fl(sto) = procesdef%procesprops(iproc)%fluxstochi(i_flst)%ioitem
            sto_sc(sto) = procesdef%procesprops(iproc)%fluxstochi(i_flst)%scale
         enddo

         ! reserve for extra input/output if process is charon

         if ( procesdef%procesprops(iproc)%routine .eq. 'D40CHA' ) then
            ins = ins + nsvich
            ous = ous + nsvoch
            flu = flu + nflch
            sto = sto + nstch
         endif

         ! store totals for this process

         NSVAI(iproc) = ins - IPVAI(iproc) + 1
         NSVXI(iproc) = ine - IPVXI(iproc) + 1
         NSVAO(iproc) = ous - IPVAO(iproc) + 1
         NSVXO(iproc) = oue - IPVXO(iproc) + 1
         NBFL(iproc)  = flu - IPBFL(iproc) + 1
         NBST(iproc)  = sto - IPBST(iproc) + 1
         NDST(iproc)  = dis - IPDST(iproc) + 1
         NVST(iproc)  = vel - IPVST(iproc) + 1

      enddo

      if (timon) call timstop( ithndl )
      return
      end
