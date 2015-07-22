      subroutine ch1to1 (lu_mes)

      include 'data.inc'

      integer lu_mes
      logical item_produced_by_process(nitemm)
      integer iconf, item, iproc, ioutp
      character*10 last_process

      do iconf = 1,nconf

c         Zero check list

          do item = 1,nitem
              item_produced_by_process(item) = .false.
          enddo

c         Loop over output items
   
          last_process = 'notyetdone'
          do ioutp=1,noutp
              if ( outppr(ioutp) .ne. last_process ) then
                  last_process = outppr(ioutp)
                  call zoek (last_process,nproc,procid,10,iproc)
                  if ( iproc .le. 0 ) then
                     stop 'CH1TO1 BUG 01'
                  endif
              endif
              if (conpro(iconf,iproc)) then

c                 Process in current configuration
                  if ( outpdo(ioutp) .ne. ' ' ) then
                      call zoek (outpit(ioutp),nitem,itemid,10,item)
                      if ( item .le. 0 ) stop 'CH1TO1 BUG 02'
                      if ( item_produced_by_process(item) ) then
                          write (lu_mes,'(''Item '',a10,
     j                           '' produced twice in conf '',a10)')
     j                    itemid(item),confid(iconf)
                      else
                          item_produced_by_process(item) = .true.
                      endif
                  endif
              endif
          enddo
      enddo

      return
      end
