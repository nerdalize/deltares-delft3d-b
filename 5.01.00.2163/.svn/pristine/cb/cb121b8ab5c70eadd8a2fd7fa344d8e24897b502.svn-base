      subroutine makind
c
c     Create indices for Nefis file
c
c     Table R2: index in ITEMS is r2_iin
c     Table R3: index in ITEMS is inpuii
c     Table R3: index in PROCS is inpupi
c     Table R4: index in ITEMS is outpii
c     Table R4: index in PROCS is outppi

c     Include data structures for tables
      include 'data.inc'

      integer icnsb, iinpu, ioutp, iitem, iproc

      do 10 icnsb = 1,ncnsb
          call zoek (r2_sid(icnsb),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop 'MAKIND: BUG 001'
          r2_iin(icnsb) = iitem-1
   10 continue

      do 20 iinpu = 1,ninpu
          call zoek (inpuit(iinpu),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop 'MAKIND: BUG 002'
          inpuii(iinpu) = iitem-1

          call zoek (inpupr(iinpu),nproc,procid,10,iproc)
          if ( iproc .le. 0 ) stop 'MAKIND: BUG 003'
          inpupi(iinpu) = iproc-1
   20 continue

      do 30 ioutp = 1,noutp
          call zoek (outpit(ioutp),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) stop 'MAKIND: BUG 004'
          outpii(ioutp) = iitem-1

          call zoek (outppr(ioutp),nproc,procid,10,iproc)
          if ( iproc .le. 0 ) stop 'MAKIND: BUG 005'
          outppi(ioutp) = iproc-1
   30 continue

      return
      end
