      subroutine finuni ( itemd, ihulp )
      character*50 itemd
      integer      ihulp , j     , nhaak
      logical      unit

c         write (*,*) ' FINUNI: '
c         write (*,*) '[',itemd,']'
      unit = .false.
      nhaak = 0
      ihulp = 0
      do 100 j = 50,1,-1
          if ( itemd(j:j) .eq. '(' .or.
     j         itemd(j:j) .eq. '['      ) then
              nhaak = nhaak-1
          endif
          if ( itemd(j:j) .eq. ')' .or.
     j         itemd(j:j) .eq. ']'      ) then
              nhaak = nhaak+1
              unit = .true.
          endif
          if ( nhaak .lt. 0 ) then
              write (*,*) ' ITEM: ',itemd
              stop 'FINUNI 001'
          endif
c         write (*,*) ' j    : ',j
c         write (*,*) ' nhaak: ',nhaak
c         write (*,*) ' unit : ',unit
          if ( nhaak .eq. 0 .and. unit ) then
              ihulp = j
              goto 200
          endif
  100 continue
  200 continue
      return
      end
