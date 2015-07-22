      subroutine wripdf ( procid, procnm, procco, procfo, lun   )

      character*50 procnm
      character*10 procid, procfo
      integer      procco, lun   , i
      include 'pdf.inc'

      write ( lun , '(a10,20x,a50)' ) procid,procnm
      write ( lun , '(a10,''; naam module'')' ) procfo
      write ( lun , '(i3,''       ; waarde van TRswitch'')' ) procco

      write ( lun , '(i10,
     j      ''; aantal invoer grootheden op segment niveau'')' ) ins
      if ( ins .gt. 0 )
c    jwrite ( lun , '(a10,2x,g15.6,3x,a50)' )
c    j(ins_id(i),ins_va(i),ins_nm(i),i=1,ins)
     jwrite ( lun , '(a10,2x,g15.6,1x,a1,1x,a50)' )
     j(ins_id(i),ins_va(i),ins_do(i),ins_nm(i),i=1,ins)

      write ( lun , '(i10,
     j      ''; aantal invoer items op exchange niveau'')' ) ine
      if ( ine .gt. 0 )
c    jwrite ( lun , '(a10,2x,g15.6,3x,a50)' )
c    j(ine_id(i),ine_va(i),ine_nm(i),i=1,ine)
     jwrite ( lun , '(a10,2x,g15.6,1x,a1,1x,a50)' )
     j(ine_id(i),ine_va(i),ine_do(i),ine_nm(i),i=1,ine)

      write ( lun , '(i10,
     j      ''; aantal uitvoer grootheden op segment niveau'')' ) ous
      if ( ous .gt. 0 )
c    jwrite ( lun , '(a10,20x,a50)' )
c    j(ous_id(i),ous_nm(i),i=1,ous)
     jwrite ( lun , '(a10,18x,a1,1x,a50)' )
     j(ous_id(i),ous_do(i),ous_nm(i),i=1,ous)

      write ( lun , '(i10,
     j      ''; aantal uitvoer items op exchange niveau'')' ) oue
      if ( oue .gt. 0 )
c    jwrite ( lun , '(a10,20x,a50)' )
c    j(oue_id(i),oue_nm(i),i=1,oue)
     jwrite ( lun , '(a10,18x,a1,1x,a50)' )
     j(oue_id(i),oue_do(i),oue_nm(i),i=1,oue)

      write ( lun , '(i10,''; aantal fluxen'')' ) flu
      if ( flu .gt. 0 )
c    jwrite ( lun , '(a10,20x,a50)' )
c    j(flu_id(i),flu_nm(i),i=1,flu)
     jwrite ( lun , '(a10,18x,a1,1x,a50)' )
     j(flu_id(i),flu_do(i),flu_nm(i),i=1,flu)

      write ( lun , '(i10,''; aantal basis stochiometrie termen'')' )sto
      if ( sto .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(sto_su(i),sto_fl(i),sto_sc(i),i=1,sto)

      write ( lun , '(i10,
     j   ''; aantal basis stochiometrie termen dispersie-arra'')' ) dis
      if ( dis .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(dis_su(i),dis_it(i),dis_sc(i),i=1,dis)

      write ( lun , '(i10,
     j ''; aantal basis stochiometrie termen velocity-array'')' ) vel
      if ( vel .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(vel_su(i),vel_it(i),vel_sc(i),i=1,vel)

      write ( lun , '(''END'')')
      return
      end

      subroutine exppdfnalg ( nalgold, nalgnew)
      include 'pdf.inc'

      integer nalgold, nalgnew
      real    rdum(1)
      character*1 cdum

c     Deze routine scant de pdf op regels die herhaald worden voor 
c     nalgold typen, en past e.e.a. aan voor nalgnew typen

c     Een scan van de PB leert me dat in alle gevallen het Id Alg01 bevat
c     behalve in CONSBL waar ALGFF01 staat or ALGPR14
c     en UlvaFix waar dsedres01 staat
c     ... bovendien zijn er processen die herhaald moeten worden ...


c     invoer grootheden op segment niveau

      write (*,*) 'ins'
      call upgrade ( ins,ins_id,ins_nm,ins_va,ins_do,insmax,
     j               .true.,.true.,.true.,nalgold,nalgnew )

c     invoer exchange

      write (*,*) 'ine'
      call upgrade ( ine,ine_id,ine_nm,ine_va,ine_do,inemax,
     j               .true.,.true.,.true.,nalgold,nalgnew )

c     uitvoer segment

      write (*,*) 'ous'
      call upgrade ( ous,ous_id,ous_nm,rdum(1),ous_do,ousmax,
     j               .true.,.false.,.true.,nalgold,nalgnew )

c     uitvoer exchange

      write (*,*) 'oue'
      call upgrade ( oue,oue_id,oue_nm,rdum,oue_do,ouemax,
     j               .true.,.false.,.true.,nalgold,nalgnew )

c     fluxen

      write (*,*) 'flu'
      call upgrade ( flu,flu_id,flu_nm,rdum,flu_do,flumax,
     j               .true.,.false.,.true.,nalgold,nalgnew )

c     bas stoch

      write (*,*) 'sto'
      call upgrade ( sto,sto_su,sto_fl,sto_sc,cdum,stomax,
     j               .true.,.true.,.false.,nalgold,nalgnew )

c     disp stoch

      write (*,*) 'dis'
      call upgrade ( dis,dis_su,dis_it,dis_sc,cdum,dismax,
     j               .true.,.true.,.false.,nalgold,nalgnew )

c     vel stoch

      call upgrade ( vel,vel_su,vel_it,vel_sc,cdum,velmax,
     j               .true.,.true.,.false.,nalgold,nalgnew )

      return
      end
      subroutine upgrade ( nin,id,nm,va,ddo,ninmax,
     j                     donm,dova,dodo,nalgold,nalgnew )
      integer nin,ninmax,nalgold,nalgnew
      character*(*) id(*)
      character*(*) nm(*)
      real         va(*)
      character*1  ddo(*) 
      logical donm,dova,dodo

      integer in, ialg,nmodif, nmodim, imodif, nadd, iline
      parameter (nmodim =100)
      integer linemodif(nmodim)
      logical success

c     Loop over items

      ialg = 0
      nmodif = 0
      do in = 1,nin

c         recognize algae and retrieve number

          call recalg (ialg+1,id(in),success)
          if (success) then
c             In sequence
              ialg = ialg + 1
              if ( ialg.eq.nalgold ) then
                  if (nmodif+1.gt.nmodim) stop 'Bug6789'
                  nmodif = nmodif + 1
                  linemodif(nmodif) = in
                  ialg = 0
              endif
          else
c             Sequence finished
              ialg = 0
          endif
      enddo

c     Modifications from bottom to top

      nadd = nalgnew-nalgold
      do imodif=nmodif,1,-1
         iline = linemodif(imodif)
c         write (*,*) 'expand ',iline,id(iline)

c        Check space
         if (nin+nadd.gt.ninmax) then
             write (*,*) nin+nadd
             stop 'Bug1234'
         endif

c        Move all following items
         do in = nin,iline+1,-1
             id(in+nadd) = id(in)
             if (donm) nm(in+nadd) = nm(in)
             if (dova) va(in+nadd) = va(in)
             if (dodo) ddo(in+nadd) = ddo(in)
         enddo
c         write (*,*) 'expand ',iline,id(iline)

c        Add new items
         do in = 1,nadd
             id(iline+in) = id(iline)
c             write(*,*) 'updating id'
             call update (id(iline+in),nalgold,nalgold+in)
             if (donm) then
                 nm(iline+in) = nm(iline)
                 call update (nm(iline+in),nalgold,nalgold+in)
             endif
             if (dova) va(iline+in) = va(iline)
             if (dodo) ddo(iline+in) = ddo(iline)
         enddo

c        Increase total number
         nin = nin + nadd
      enddo
      return
      end
      subroutine recalg (ialg,id,success)
      integer ialg
      character*10 id
      logical success

      integer nmatch, i, j
      parameter (nmatch = 6)
      character*10 matchstrings(nmatch), match
      data matchstrings /
     j 'Algxx     ',
     j 'algxx     ',
     j 'ALGxx     ',
     j 'ALGFFxx   ',
     j 'ALGPRxx   ',
     j 'dSedResxx '/

c     Een scan van de PB leert me dat in alle gevallen het Id Alg01 bevat
c     behalve in CONSBL waar ALGFF01 staat or ALGPR14
c     en UlvaFix waar dsedres01 staat

      success = .false.
      do i = 1,nmatch
          match = matchstrings(i)
          j = index(match,'xx')
          if (j.le.0) then
              write (*,*) 'matching ',match
              stop 'Bugsieflapstaart'
          endif
          write (match(j:j+1),'(i2.2)') ialg
          j = index(id,trim(match))
          if (j.gt.0) then
              success = .true.
              goto 991
          endif
      enddo

  991 return
      end
      subroutine update (str,ialgin,ialgout)
      character*(*) str
      integer ialgin,ialgout
      
      character*2 c2
      integer j
    
c      write(*,'(''UPDATE'')')
c      write(*,'(''['',a,'']'')') str
      write (c2,'(i2.2)') ialgin
      j = index (str,c2)
      if (j.gt.0) 
     jwrite (str(j:j+1),'(i2.2)') ialgout
    
      return
      end
      subroutine reppdfnalg ( nalgold, nalgnew, 
     j                        procid, newprocid, 
     j                        procnam, newprocnam, 
     j                        repeat )
      integer nalgold, nalgnew
      logical repeat
      character*(*) procid, newprocid
      character*(*) procnam, newprocnam

      include 'pdf.inc'

      logical busy
      integer lastcopy
      logical success
      data busy /.false./
      save busy, lastcopy

c     Check procesnaam en laatste kopie

      repeat = .false.
      if ( .not.busy ) then
        call recalg (nalgold,procid,success)
        if (success) then
          busy = .true.
          lastcopy = nalgold+1
          repeat = .true.
        endif
      else
        if (lastcopy+1.le.nalgnew) then
          lastcopy = lastcopy + 1
          repeat = .true.
        else
          busy = .false.
        endif
      endif

      if ( .not.repeat ) return

c     invoer grootheden op segment niveau

      newprocid  = procid
      newprocnam = procnam
      call update (newprocid ,nalgold,lastcopy)
      call update (newprocnam,nalgold,lastcopy)

      write (*,*) 'ins'
      call upgrad2 ( ins,ins_id,ins_nm,
     j               .true.,lastcopy-1,lastcopy)

c     invoer exchange

      write (*,*) 'ine'
      call upgrad2 ( ine,ine_id,ine_nm,
     j               .true.,lastcopy-1,lastcopy)

c     uitvoer segment

      write (*,*) 'ous'
      call upgrad2 ( ous,ous_id,ous_nm,
     j               .true.,lastcopy-1,lastcopy)

c     uitvoer exchange

      write (*,*) 'oue'
      call upgrad2 ( oue,oue_id,oue_nm,
     j               .true.,lastcopy-1,lastcopy)

c     fluxen

      write (*,*) 'flu'
      call upgrad2 ( flu,flu_id,flu_nm,
     j               .true.,lastcopy-1,lastcopy)

c     bas stoch

      write (*,*) 'sto'
      call upgrad2 ( sto,sto_su,sto_fl,
     j               .true.,lastcopy-1,lastcopy)

c     disp stoch

      write (*,*) 'dis'
      call upgrad2 ( dis,dis_su,dis_it,
     j               .true.,lastcopy-1,lastcopy)

c     vel stoch

      call upgrad2 ( vel,vel_su,vel_it,
     j               .true.,lastcopy-1,lastcopy)

      return
      end
      subroutine upgrad2 ( nin,id,nm,
     j                     donm,ialgold,ialgnew)
      integer nin,ialgold,ialgnew
      character*(*) id(*)
      character*(*) nm(*)
      logical donm, success

      integer in

c     Loop over items

      do in = 1,nin

c         recognize algae and retrieve number

          call recalg (ialgold,id(in),success)
          if (success) then
             call update (id(in),ialgold,ialgnew)
             if (donm) then
                 call update (nm(in),ialgold,ialgnew)
             endif
          endif
      enddo
      return
      end
