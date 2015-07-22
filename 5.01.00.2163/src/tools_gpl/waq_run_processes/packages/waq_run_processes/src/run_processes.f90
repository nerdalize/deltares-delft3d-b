! run_processes.f90 --
!     Run the various process routines with the correct input
!     and check the output
!
!     Note:
!     The program assumes a number of things about the tables,
!     namely that they are grouped on process name.
!
!     TODO:
!     - Items that have no default value
!     - Items that are actually switches
!     - Instrument the code for test coverage
!
program run_processes

    implicit none

    integer, parameter :: max_number_parameters = 5000
    integer, parameter :: max_number_fluxes     = 5000
    integer, parameter :: max_number_segments   = 12

    real, parameter    :: missing     = (1.0 -       epsilon(1.0)) * huge(1.0)
    real, parameter    :: unimportant = (1.0 - 2.0 * epsilon(1.0)) * huge(1.0)

    real, dimension(max_number_parameters,max_number_segments) :: pmsa_original
    real, dimension(max_number_parameters,max_number_segments) :: pmsa_input
    real, dimension(max_number_parameters,max_number_segments) :: pmsa

    real, dimension(max_number_fluxes,max_number_segments)     :: fl

    integer, dimension(max_number_parameters)              :: ipoint
    integer, dimension(max_number_parameters)              :: ipoint_original
    integer, dimension(max_number_parameters)              :: increm
    integer, dimension(max_number_parameters)              :: increm_original
    integer, dimension(4,max_number_segments)              :: iexpnt
    integer, dimension(max_number_segments)                :: iknmrk

    integer            :: noseg
    integer            :: noq1
    integer            :: noq2
    integer            :: noq3
    integer            :: noq4
    integer            :: i
    integer            :: j
    integer            :: k
    integer            :: l
    integer            :: number_inputs
    integer            :: number_outputs
    integer            :: number_fluxes

    integer            :: luproces  = 11
    integer            :: luinputs  = 12
    integer            :: luoutputs = 13
    integer            :: luoutpflx = 14
    integer            :: luitems   = 15
    integer            :: ierr

    integer            :: number_items

    integer            :: dims
    character(len=20)  :: process_name
    character(len=20)  :: process_routine
    character(len=80)  :: description

    type item_type
        character(len=20) :: name
        integer           :: type
        real              :: value
        real              :: minimum
        real              :: maximum
    end type item_type

    type(item_type), pointer, dimension(:) :: item_data

    character(len=256) :: cident
    character(len=10)  :: version
    character(len=100) :: modnam

    cident = ' '
    call getfullversionstring_waq_run_processes(cident)
    call getfeaturenumberstring_waq_run_processes(version)
    modnam = 'DHS_Delft3D_WAQ_RUN_PROCESSES'

    !
    ! Schematisation
    !
    noseg  = 12
    noq1   = 0
    noq2   = 0
    noq3   = 0
    noq4   = 0
    iexpnt = 0  ! TODO

    increm = max_number_parameters
    iknmrk = (/ 1, 0, 0, 0, 21, 11, 11, 31, 21, 11, 11, 31 /)


    !
    ! Open the table files
    !
    open( luproces,  file = 'waq_tables/proces.csv'  )
    open( luinputs,  file = 'waq_tables/inputs.csv'  )
    open( luoutputs, file = 'waq_tables/outputs.csv' )
    open( luoutpflx, file = 'waq_tables/outpflx.csv' )
    open( luitems,   file = 'waq_tables/items.csv'   )

    call load_items( luitems, item_data )

    close( luitems )

    !
    ! Loop over the processes
    !
    read( luproces, '(a)' )

    do
        read( luproces, *, iostat = ierr ) process_name, dims, process_routine, description

        if ( ierr /= 0 ) exit

        write(*,*) ' '
        write(*,*) trim(process_name), ' - ', trim(process_routine), ' - ', trim(description)

        increm_original = max_number_parameters

        call identify_in_out( process_name, item_data, number_inputs, number_outputs, number_fluxes, &
                ipoint_original, increm_original, pmsa_original )

        if ( number_inputs + number_outputs == 0 ) then
            write(*,*) '   ==> process skipped: no input or output'
        endif

        if ( dims /= 123 ) cycle ! For the moment at least
        !
        ! Run the process routine
        !
        ! Note: the shifts do not seem to be very useful, so omit them
        !
        !do i = 1,number_inputs+number_outputs
        do i = 1,1
            do j = 1,max_number_segments
                if ( .not. btest(iknmrk(j),0) ) then
                    fl(1:number_fluxes,j) = unimportant
                    pmsa_original(2*number_inputs+1:2*(number_inputs+number_outputs),j) = unimportant
                endif
            enddo

            fl         = missing
            pmsa_input = missing

            pmsa_input(1:2*(number_inputs+number_outputs),:)   = &
                cshift( pmsa_original(1:2*(number_inputs+number_outputs),:), 2*(i-1), 1 )

            ipoint(1:number_inputs+number_outputs) = &
                1 + modulo( ipoint_original(1:number_inputs+number_outputs) - 2*(i-1)-1, &
                2*(number_inputs+number_outputs) )

            increm(1:number_inputs+number_outputs) = &
                cshift( increm_original(1:number_inputs+number_outputs), 2*(i-1), 1 )

            pmsa = pmsa_input

            call report_results( pmsa, pmsa_original, fl, number_inputs, number_outputs, number_fluxes )

            call select_routine( process_routine, pmsa, fl, ipoint, increm, &
                     noseg, max_number_fluxes, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4 )

            call report_results( pmsa, pmsa_original, fl, number_inputs, number_outputs, number_fluxes )

            !
            ! Checks (shift the array of inputs and outputs back)
            !
            pmsa(1:2*(number_inputs+number_outputs),:)   = &
                cshift( pmsa(1:2*(number_inputs+number_outputs),:), -2*(i-1), 1 )

            if ( any( pmsa_original(1:2*number_inputs,:) /= pmsa(1:2*number_inputs,:) ) ) then
                write(*,*) trim(process_name) // ': input parameters have been changed'
                write(*,'(2i5,2e14.5)') ((k, l, pmsa_original(k,l), pmsa(k,l), k = 1,2*number_inputs),l=1,noseg)
            endif
            if ( any( pmsa(2*number_inputs+1:2*number_inputs+number_outputs,:) == missing ) ) then
                write(*,*) trim(process_name) // ': one or more output parameters have not been set'
            endif
            if ( any( fl(1:number_fluxes,:) == missing ) ) then
                write(*,*) trim(process_name) // ': one or more fluxes have not been set'
            endif
            if ( any( fl(number_fluxes+1:,:) /= missing ) ) then
                write(*,*) trim(process_name) // ': one or more fluxes outside the valid range have been changed'
            endif

            ! Attribute array must be taken into account!

        enddo
    enddo

    write(*,*) 'Test completed'
contains

subroutine load_items( luitems, item_data )

    integer                                :: luitems
    type(item_type), dimension(:), pointer :: item_data

    character(len=20)                      :: name
    character(len=20)                      :: dummy
    integer                                :: idx
    integer                                :: ierr
    real                                   :: value

    type(item_type), dimension(:), pointer :: copy_items
    character(len=100)                     :: line
    character(len=20)                      :: keyword
    integer                                :: k
    real                                   :: minimum
    real                                   :: maximum
    real                                   :: step

    idx = 0

    allocate( item_data(100) )

    read( luitems, '(a)' )

    do
        read( luitems, *, iostat = ierr ) name, dummy, dummy, value
        if ( ierr /= 0 ) exit

        idx = idx + 1
        if ( idx > size(item_data) ) then
            allocate( copy_items(size(item_data)) )

            copy_items = item_data

            deallocate( item_data )
            allocate( item_data(size(copy_items)+100) )
            item_data%name = '?'
            item_data(1:size(copy_items)) = copy_items
            deallocate( copy_items )
        endif

        item_data(idx)%name    = name
        item_data(idx)%value   = value
        item_data(idx)%minimum = -999.0
        item_data(idx)%maximum = -999.0
    enddo

    close( luitems )

    !
    ! Read the non-default values
    !
    open( luitems, file = 'run_processes.inp' )

    do
         read( luitems, '(a)', iostat = ierr ) line
         if ( ierr /= 0 ) exit

         k = index( line, '#' )
         if ( k > 0 ) then
             line = line(1:k-1)
         endif

         if ( line == ' ' ) then
             cycle
         endif

         line(100:100) = '/'

         maximum = -999.0
         read( line, * ) name, keyword, minimum, maximum, step

         call find_item( item_data, name, idx )

         if ( idx > 0 ) then
             if ( maximum /= -999.0 ) then
                 item_data(idx)%value   = minimum
                 item_data(idx)%minimum = minimum
                 item_data(idx)%maximum = maximum
             else
                 item_data(idx)%value   = minimum
             endif
             select case( keyword )
                 case( 'parameter' )
                     item_data(idx)%type = 1
                 case( 'constant' )
                     item_data(idx)%type = 2
                 case( 'switch' )
                     item_data(idx)%type = 3
                 case default
                     write(*,*) 'Keyword in run_processes.inp unknown: ', trim(keyword)
             endselect
         else
             write(*,*) 'Item in run_processes.inp unknown: ', trim(name)
         endif
     enddo

     close( luitems )

end subroutine load_items

subroutine find_item( item_data, name, idx )

    type(item_type), dimension(:) :: item_data
    character(len=20)             :: name
    integer                       :: idx

    integer                       :: i

    idx = -1

    do i = 1,size(item_data)
        if ( item_data(i)%name == name ) then
            idx = i
            exit
        endif
    enddo

end subroutine find_item

subroutine identify_in_out( process_name, item_data, number_inputs, number_outputs, number_fluxes, &
                            ipoint, increm, pmsa )
    character(len=*)              :: process_name
    type(item_type), dimension(:) :: item_data
    integer                       :: number_inputs
    integer                       :: number_outputs
    integer                       :: number_fluxes
    integer, dimension(:)         :: ipoint
    integer, dimension(:)         :: increm
    real, dimension(:,:)          :: pmsa

    integer                       :: ierr
    character(len=80), save       :: line_inputs
    character(len=80), save       :: line_outputs
    character(len=80), save       :: line_fluxes
    character(len=20)             :: name
    character(len=20)             :: item
    integer                       :: idx
    integer                       :: idxn

    logical, save                 :: first = .true.

    ! TODO

    number_inputs  = 0
    number_outputs = 0
    number_fluxes  = 0

    if ( first ) then
        first = .false.
        read( luinputs,  '(a)' )
        read( luoutputs, '(a)' )
        read( luoutpflx, '(a)' )

        read( luinputs,  '(a)' ) line_inputs
        read( luoutputs, '(a)' ) line_outputs
        read( luoutpflx, '(a)' ) line_fluxes
    endif

    do
        read( line_inputs, * ) name

        if ( name == process_name ) then
            read( line_inputs, * ) name, item, idx
            number_inputs = number_inputs + 1
            call find_item( item_data, item, idxn )
            if ( idxn > 0 ) then
                ipoint(idx)  = 2*number_inputs-1
                pmsa(2*number_inputs-1,:) = item_data(idxn)%value

                if ( item_data(idxn)%type == 2 .or. item_data(idxn)%type == 3 ) then
                    write(*,*) 'CONSTANT: ', item_data(idxn)%name
                    increm(idx) = 0
                endif
            else
                write(*,*) 'Input item not found: ', trim(item)
            endif

            read( luinputs, '(a)', iostat = ierr ) line_inputs
            if ( ierr /= 0 ) exit

        else
            exit
        endif
    enddo

    do
        read( line_outputs, * ) name

        if ( name == process_name ) then
            read( line_outputs, * ) name, item, idx
            number_outputs = number_outputs + 1
            call find_item( item_data, item, idxn )
            if ( idxn > 0 ) then
                ipoint(number_inputs+idx)            = 2*number_inputs + 2*number_outputs-1
                pmsa(2*number_inputs+2*number_outputs-1,:) = item_data(idxn)%value
            else
                write(*,*) 'Output item not found: ', trim(item)
            endif

            read( luoutputs,  '(a)', iostat = ierr ) line_outputs
            if ( ierr /= 0 ) exit
        else
            exit
        endif
    enddo

    do
        read( line_fluxes, *, iostat = ierr ) name

        if ( name == process_name ) then
            read( line_fluxes, * ) name, item, idx
            number_fluxes = number_fluxes + 1

            read( luoutpflx, '(a)', iostat = ierr ) line_fluxes
            if ( ierr /= 0 ) exit

        else
            exit
        endif
    enddo

end subroutine identify_in_out

subroutine report_results( pmsa, pmsa_original, fl, number_inputs, number_outputs, number_fluxes )
    real, dimension(:,:) :: pmsa
    real, dimension(:,:) :: pmsa_original
    real, dimension(:,:) :: fl
    integer              :: number_inputs
    integer              :: number_outputs
    integer              :: number_fluxes

    integer              :: i
    logical              :: changed

    write(*,*) 'Input items:'
    do i = 1,number_inputs
        changed = any( pmsa(i,:) /= pmsa_original(i,:) )
        write(*,'(i5,a,100e12.4)') i, merge('*', ' ', changed), pmsa(i,:)
    enddo

    write(*,*) 'Output items:'
    do i = number_inputs+1,number_inputs+number_outputs
        changed = any( pmsa(i,:) /= missing )
        write(*,'(i5,a,100e12.4)') i-number_inputs, merge(' ', '*', changed), pmsa(i,:)
    enddo

    write(*,*) 'Fluxes:'
    do i = 1,number_fluxes
        changed = any( fl(i,:) /= missing )
        write(*,'(i5,a,100e12.4)') i, merge(' ', '*', changed), fl(i,:)
    enddo

end subroutine report_results


subroutine select_routine( process_routine, pmsa, fl, ipoint, increm, &
               noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
               noq3, noq4 )
    character(len=*), intent(in) :: process_routine

    real     :: pmsa  ( * ) , fl    (*)
    integer  :: ipoint( * ) , increm(*) , noseg , noflux, &
                iexpnt(4,*) , iknmrk(*) , noq1, noq2, noq3, noq4

    select case( process_routine )
    case( 'DDEPTH' )
        call ddepth( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DSURF'  )
        call dsurf( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'TOTDEP' )
        call totdep( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'EMERSI' )
        !call emersi( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping EMERSI - no proper layer information'
    case( 'METEO' )
        !call meteo( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping METEO - problem with number of stations'
    case( 'HEATFL')
        call heatfl( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'TEMPER')
        call temper( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CEC')
        !call cec( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping CEC - deleted from library for open source release'
    case( 'D40CHA')
        !call d40cha( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping D40CHA: array bounds incorrectly reported as being overflowed'
    case( 'VARSAL')
        call varsal( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SALIN')
        !call salin( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SALIN - deleted from library for open source release'
    case( 'CHLOR')
        !call chlor( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping CHLOR - deleted from library for open source release'
    case( 'VELOC')
        !call veloc( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping VELOC: issues with work arrays or INCREM being zero?'
    case( 'GRD')
        !call grd( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GRD - deleted from library for open source release'
    case( 'RESTIM')
        call restim( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'S2X')
        !call s2x( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping S2X - deleted from library for open source release'
    case( 'VDISP')
        !call vdisp( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping VDISP - deleted from library for open source release'
    case( 'STOX3D')
        call stox3d( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'HDISP')
        call hdisp( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'HDISPV')
        call hdispv( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'XTOS3D')
        !call xtos3d( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping XTOS3D - deleted from library for open source release'
    case( 'WATAGE')
        call watage( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'INTPOL')
        call intpol( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CALCHZ')
        call calchz( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CALWAV')
        call calwav( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CALTAU')
        call caltau( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'EXTINA')
        call extina( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'EXTINC')
        call extinc( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CLCRAD')
        call clcrad( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DAYL')
        call dayl( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DEPAVE')
        call depave( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'VTRANS')
        call vtrans( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'D40BLO')
        !call d40blo( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping D40BLO: array bounds incorrectly reported as being overflowed or some issue with pointering'
    case( 'MAKPOC')
        call makpoc( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'PHCOMP')
        call phcomp( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'POCOMP')
        !call pocomp( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping POCOMP - deleted from library for open source release'
    case( 'SEDCAR')
        call sedcar( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'RESCAR')
        !call rescar( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping RESCAR - deleted from library for open source release'
    case( 'RESALG')
        !call resalg( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping RESALG - deleted from library for open source release'
    case( 'RESNUT')
        !call resnut( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping RESNUT - deleted from library for open source release'
    case( 'SEDCOM')
        call sedcom( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'WKCOMP')
        call wkcomp( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SUMCOL')
        !call sumcol( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SUMCOL - deleted from library for open source release'
    case( 'DMVOL')
        call dmvol( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SIMPH')
        call simph( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'BACMRT')
        call bacmrt( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SATCO2')
        call satco2( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'REAR')
        call rear( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'ADSPO4')
        call adspo4( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'EXPLFL')
        !call explfl( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping EXPLFL - deleted from library for open source release'
    case( 'DENSED')
        call densed( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DENWAT')
        call denwat( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'NITRIF')
        call nitrif( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SATOXY')
        call satoxy( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'VAROXY')
        call varoxy( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'WATMIN')
        !call watmin( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping WATMIN - deleted from library for open source release'
    case( 'BOTMIN')
        call botmin( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'MINLIM')
        !call minlim( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping MINLIM - deleted from library for open source release'
    case( 'BODCOD')
        call bodcod( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DECBOD')
        call decbod( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DECFSN')
        !call decfsn( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DECFSN - deleted from library for open source release'
    case( 'DECSLN')
        !call decsln( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DECSLN - deleted from library for open source release'
    case( 'DECREN')
        !call decren( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DECREN - deleted from library for open source release'
    case( 'DECREF')
        !call decref( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DECREF - deleted from library for open source release'
    case( 'DECFST')
        !call decfst( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DECFST - deleted from library for open source release'
    case( 'DECSLW')
        !call decslw( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DECSLW - deleted from library for open source release'
    case( 'VIVIAN')
        call vivian( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'VIVIA2')
        !call vivia2( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping VIVIA2 - deleted from library for open source release'
    case( 'DISSI')
        call dissi( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SEDOX')
        call sedox( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'TFALG')
        call tfalg( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DLALG')
        call dlalg( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'NLALG')
        call nlalg( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'RADALG')
        call radalg( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'RDBALG')
        call rdbalg( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'PRIPRO')
        call pripro( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SDPPRO')
        call sdppro( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'PPRLIM')
        call pprlim( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'NUTUPT')
        call nutupt( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'NUTREL')
        call nutrel( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'ALGMRT')
        !call algmrt( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping ALGMRT - deleted from library for open source release'
    case( 'NRALGS')
        call nralgs( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'OXYMIN')
        call oxymin( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'MFBNUT')
        !call mfbnut( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping MFBNUT - deleted from library for open source release'
    case( 'GEMTMP')
        !call gemtmp( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GEMTMP - deleted from library for open source release'
    case( 'MFBLLM')
        !call mfbllm( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping MFBLLM - deleted from library for open source release'
    case( 'GEMNLM')
        !call gemnlm( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GEMNLM - deleted from library for open source release'
    case( 'GEMMFB')
        !call gemmfb( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GEMMFB - deleted from library for open source release'
    case( 'MNDINI')
        !call mndini( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping MNDINI - deleted from library for open source release'
    case( 'MNDLLM')
        !call mndllm( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping MNDLLM - deleted from library for open source release'
    case( 'GEMMND')
        !call gemmnd( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GEMMND - deleted from library for open source release'
    case( 'CSELAC')
        call cselac( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'EBUCH4')
        call ebuch4( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SATCH4')
        call satch4( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SULFID')
        call sulfid( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SULFOX')
        call sulfox( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SULFPR')
        call sulfpr( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'METHOX')
        call methox( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CALSED')
        call calsed( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CALVS')
        !call calvs( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GEMMND - deleted from library for open source release'
    case( 'SEDNUT')
        call sednut( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SEDNAL')
        !call sednal( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SEDNAL - deleted from library for open source release'
    case( 'SEDSOD')
        call sedsod( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SSEDPH')
        if ( process_name == 'SedPhBlo_P' ) then
            write(*,*) 'Skipping SedPhBlo_P - conflict with SedPhBlo wrt input'
            return
        endif
        call ssedph( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SEDAAP')
        call sedaap( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SWSEDN')
        !call swsedn( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SWSEDN - deleted from library for open source release'
    case( 'SOMSED')
        call somsed( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SOMRES')
        !call somres( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SOMRES - deleted from library for open source release'
    case( 'RESDM')
        call resdm( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'RESANT')
        call resant( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'BURIAL')
        call burial( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'BURCAR')
        !call burcar( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping BURCAR - deleted from library for open source release'
    case( 'BURNUT')
        !call burnut( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping BURNUT - deleted from library for open source release'
    case( 'SWBUR')
        !call swbur( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SWBUR - deleted from library for open source release'
    case( 'SWBURA')
        !call swbura( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SWBURA - deleted from library for open source release'
    case( 'SWBURN')
        !call swburn( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping SWBURN - deleted from library for open source release'
    case( 'DIGGIN')
        call diggin( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DIGCAR')
        !call digcar( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DIGCAR - deleted from library for open source release'
    case( 'DIGNUT')
        !call dignut( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DIGNUT - deleted from library for open source release'
    case( 'ADVTRA')
        call advtra( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DSPTRA')
        call dsptra( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'RFPART')
        call rfpart( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'PARTMP')
        call partmp( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'TRASE2')
        call trase2( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'ULFIX')
        !call ulfix( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping ULFIX - no proper layer information'
    case( 'CONSBL')
        !call consbl( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping CONSBL - many process parameters must be constant'
    case( 'ZOODYN')
        !call zoodyn( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping ZOODYN - deleted from library for open source release'
    case( 'D40SWI')
        !call d40swi( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping D40SWI - problem with internal state parameters'
    case( 'SWOXY')
        call swoxy( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'TRCOEF')
        call trcoef( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'VERVLU')
        call vervlu( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'DEGMP')
        call degmp( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SEDHM')
        call sedhm( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'RESHM')
        !call reshm( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping RESHM - deleted from library for open source release'
    case( 'BURHM')
        !call burhm( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping BURHM - deleted from library for open source release'
    case( 'DIGHM')
        !call dighm( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DIGHM - deleted from library for open source release'
    case( 'SEDOMV')
        call sedomv( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'RESOMV')
        !call resomv( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping RESOMV - deleted from library for open source release'
    case( 'BUROMV')
        !call buromv( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping BUROMV - deleted from library for open source release'
    case( 'DIGOMV')
        !call digomv( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping DIGOMV - deleted from library for open source release'
    case( 'ATMDEP')
        call atmdep( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'NH3FRE')
        call nh3fre( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'BOD')
        !call bod( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping BOD - deleted from library for open source release'
    case( 'POSOXY')
        call posoxy( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'SECCHI')
        call secchi( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'CALSND')
        !call calsnd( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping CALSND - deleted from library for open source release'
    case( 'NUTCNK')
        !call nutcnk( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping NUTCNK - deleted from library for open source release'
    case( 'VERTNK')
        !call vertnk( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping VERTNK - deleted from library for open source release'
    case( 'BLUETD')
        !call bluetd( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping BLUETD - deleted from library for open source release'
    case( 'OYSTER')
        !call oyster( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping OYSTER - problem with parameter "coupling water-sediment"'
    case( 'PTEWOR')
        call ptewor( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )
    case( 'FERDOM')
        !call ferdom( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping FERDOM - deleted from library for open source release'
    case( 'GROAB')
        !call groab( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GROAB - deleted from library for open source release'
    case( 'GROHB')
        !call grohb( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping GROHB - deleted from library for open source release'
    case( 'HYDPOM')
        !call hydpom( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping HYDPOM - deleted from library for open source release'
    case( 'LYSIS')
        !call lysis( pmsa, fl, ipoint, increm, &
        !         noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
        !         noq3, noq4 )
        write(*,*) 'Skipping LYSIS - deleted from library for open source release'
    case( 'STREAR')
        call strear( pmsa, fl, ipoint, increm, &
                 noseg, noflux, iexpnt, iknmrk, noq1, noq2, &
                 noq3, noq4 )

    case default
        write(*,*) 'Unknown process routine: ', process_routine
    end select
end subroutine select_routine

end program
