      subroutine set_fraction( lurep    , notot   , syname, nomult, imultp,
     +                         procesdef, allitems, no_act, actlst, nbpr  )

!     Deltares Software Centre

!>/File
!>      set the substance fractions intothe processes structure

!     Created   : Aug   2012 by Jan van Beek

      use timers         !< performance timers
      use processet      !< use processet definitions
      implicit none

      ! arguments

      integer             , intent(in   ) :: lurep                  !< unit number report file
      integer             , intent(in   ) :: notot                  !< number of substances
      character(len=*)    , intent(in   ) :: syname(*)              !< substance names
      integer             , intent(in   ) :: nomult                 !< number of multiple substances
      integer             , intent(in   ) :: imultp(2,nomult)       !< multiple substance administration
      type(procespropcoll), intent(inout) :: procesdef              !< the proces definition
      type(itempropcoll)  , intent(inout) :: allitems               !< all items of the proces system
      integer             , intent(inout) :: no_act                 !< number of activated processes
      character(len=*)    , intent(inout) :: actlst(*)              !< list of activated processes
      integer             , intent(inout) :: nbpr                   !< number of processes

      ! local

      type(sfracsprop)                    :: sfracs          ! substance fraction properties
      integer(4)                          :: ithndl = 0      ! handle for performance timer
      if (timon) call timstrt( "set_fractions", ithndl )

      ! set the fractions structure

      call get_sfrac ( lurep , notot , syname, nomult, imultp,
     +                 sfracs)

      ! add atributes to processes ( and sfracs ?)

      call add_atrfrc( lurep , procesdef, sfracs)

      ! add calculation of the sum of the fractions

      call add_sumfrc( lurep , procesdef, allitems, sfracs, no_act,
     +                 actlst, nbpr     )

      ! add the fluxes to the fractions by expanding processes

      call expand_frc( lurep , procesdef, allitems, sfracs)

      ! add the fluxes to the fractions by copying processes

      call add_prcfrc( lurep , procesdef, allitems, sfracs, no_act,
     +                 actlst, nbpr     )

      ! add the fluxes to the fractions by adding a distribution process

      call add_flxfrc( lurep , procesdef, allitems, sfracs, no_act,
     +                 actlst, nbpr     )

      ! add the dispersion stochi

      call add_dspfrc( lurep , procesdef, sfracs)

      if (timon) call timstop( ithndl )
      return
      end
