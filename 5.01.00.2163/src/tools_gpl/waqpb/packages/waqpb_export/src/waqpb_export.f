c
c     Program to compose a PROCES.ASC file from tables
c
c     This program cocnsists of the following parts:
c     - Reading of tables holding PROCLIB data structure
c     - Finding out which processes need to be included
c     - Loop over the processes:
c       - Empty PDF structure
c       - Construct and write PDF structure
c       - Dump structure to PROCES.ASC
c     JvG optionally txt file used for update NAlg

c     Include data structures for tables and PDF-file
      include 'data.inc'
      include 'pdf.inc'
      integer      jndex , iproc , iinpu , iitem , ioutp , idisp ,
     j             ioutf , isubs , naanta, ioffse, ioffs2, ivelo ,
     j             istoc , iconf , npdf  , iflag , naant2, serial,
     j             ierror, icnsb , imodv , i
      integer      nalgnew, nalgold
      logical      switui, swit2d, proswi(nprocm), itmswi(nitemm),
     j             repeat
      character*10 c10   , locid
      character*50 adduni, locnam
      real         actdef, versio
      integer      lu_inp, lu_mes
      data         lu_inp /14/
      data         lu_mes /11/

      do iitem = 1,nitemm
          itmswi(iitem) = .false.
      enddo

      open ( lu_mes , file = 'waqpb_export.log' )

      write (*,'('' Reading data......'')')

c----------------------------------------------------------------------c
c     READ DATABASE
c----------------------------------------------------------------------c

      call readdb ( lu_inp, lu_mes )

c     Check validity of table R9

      do 10 imodv = 1,nmodv
          call zoek (modvci(imodv),nconf,confid,10,iconf)
          call zoek (modvit(imodv),nitem,itemid,10,iitem)
          if ( iconf .le. 0 .or. iitem .le. 0 )
     j        write ( lu_mes , '(''Undefined item in TABLE5: '',a10,1x,
     j                            a10)') modvci(imodv),modvit(imodv)
   10 continue

c     Create auxiliary table of substances

      nsubs = 0
      do 30 icnsb = 1,ncnsb
          c10 = r2_sid(icnsb)

c         Lookup substance in item array
          call zoek (c10,nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) then
              write (*,*) ' ITEM: ',c10
              STOP 'Unknown substance in R2 table'
          endif

c         Add to substances array
          call zoek (c10,nsubs,subsid,10,isubs)
          if ( isubs .le. 0 ) then
              if ( nsubs+1 .gt. nsubsm ) STOP 'Dimension NSUBSM'
              nsubs = nsubs+1
              subsid(nsubs) = c10
          endif
   30 continue

c     TEMPORARY CHECK on multiple processes producing ONE ITEM

      call ch1to1 (lu_mes)

c     Temporary dump of FIRST configuration TO AVOID BUG
c     which I DO NOT UNDERSTAND
      write (11,'(''Processes in FIRST configuration:'')')
      write (11,'(''Process '',a10,1x,l10)')
     j      (procid(iproc),conpro(1,iproc),iproc=1,nproc)

c     Dump TRM tables

      write (*,*) 'Do you want TRM tables?? (0/1)'
      read (*,*) iflag
      if ( iflag .eq. 1 ) then
          write (*,'('' Writing TRM tables......'')')
          call writrm
      endif
      write (*,'('' Writing TRM tables for LaTeX......'')')
      call writex
c----------------------------------------------------------------------c
c     SET VERSION, SERIAL AND WRITE NEFIS FILE
c----------------------------------------------------------------------c

      write (*,'('' Configuring......'')')
      write (11,'(/''Configuring......'')')
      do 200 iproc=1,nproc
  200 proswi(iproc) = .false.

      write (*,*) 'Version number ProcLib?? (real)'
      read (*,*) versio

      write (*,*) 'Serial number ProcLib?? (integer)'
      read (*,*) serial

      write (*,*) 'Make NEFIS process definition file?? (0/1)'
      read (*,*) iflag
      if ( iflag .eq. 1 ) then
          write (11,'(''Writing NEFIS process definition file'')')
          call makind()
          call pdfnef(11    , serial, versio, ierror)
          if ( ierror .ne. 0 ) then
             write (11,'(''ERROR writing NEFIS file'')')
             write (*,'(''ERROR writing NEFIS file, see report file'')')
          endif
      endif

c----------------------------------------------------------------------c
c     INTERACTIVE PART TO FIND OUT WHICH PROCESSES ARE REQUIRED
c----------------------------------------------------------------------c

      switui = .false.
c     write (*,*) 'Make files for ProcEdit version 1.07?? (0/1)'
c     read (*,*) iflag
c     if ( iflag .eq. 1 ) then
c         switui = .true.
c         write (11,'(''ProcEdit V1.07 mode'')')
c     endif

      swit2d = .false.
c     write (*,*) 'Make files for 2D use only?? (0/1)'
c     read (*,*) iflag
c     if ( iflag .eq. 1 ) then
c         swit2d = .true.
c         write (11,'(''2D mode'')')
c     endif

c     Back for next assignment

  210 write (*,*) 'Requested process or configuration ?? (ALL/STOP)'
      read (*,'(a)') c10

c     Check for STOP
      call zoek (c10,1,'stop',4,jndex)
      if (jndex .eq. 1) then
          write (11,'(''STOP command received'')')
          goto 220
      endif

c     Check for ALL
      call zoek (c10,1,'all',3,jndex)
      if (jndex .eq. 1) then
          do 212 iproc=1,nproc
  212     proswi(iproc) = .true.
          write (11,'(''ALL command received'')')
          goto 220
      endif

c     Check for Configuration
      call zoek (c10,nconf,confid,10,iconf)
      if (iconf .gt. 0) then
          write (11,'(''Configuration '',a10,'' activated'')') c10
          do 214 iproc=1,nproc
              if ( conpro(iconf,iproc) ) then
                  proswi(iproc) = .true.
                  write (11,'(''Process '',a10,'' switched on'')')
     j                                     procid(iproc)
              endif
  214     continue
          goto 210
      endif

c     Check for Process
      call zoek (c10,nproc,procid,10,iproc)
      if (iproc .gt. 0) then
          proswi(iproc) = .true.
          write (11,'(''Process       '',a10,'' activated'')') c10
          goto 210
      endif

      write (11,'(''UNKNOWN!!!    '',a10)') c10
      goto 210

  220 continue

c----------------------------------------------------------------------c
c     LOOP OVER PROCESSES
c----------------------------------------------------------------------c

      npdf = 0
      do 300 iproc=1,nproc
          if ( proswi(iproc) ) npdf = npdf + 1
  300 continue
      if ( npdf .eq. 0 ) goto 900

      write (*,'('' Making PROCES.ASC......'')')
      write (*,*)
      open ( 15 , file = 'procesm.asc' )
      open ( 16 , file = 'proclib.txt' )

      write ( 15 , '(i10,50x,f8.2,2x,i10)' ) npdf,versio,serial

      nalgold = 0
      open (789,file='expand_algae',status='old',err=987)
      read (789,*) nalgold,nalgnew
      if (nalgnew.lt.nalgold) stop 'Option not implemented'
  987 continue

      if (nalgold.gt.0) then
      write ( 16 , '(i10,50x,f8.2,2x,i10)' ) npdf,versio,serial
      else
      write ( 16 , '(/''HELP FILE FOR DELWAQ PROCESSES LIBRARY''/
     j                ''version:  '',f10.2/
     j                ''serialnr: '',i10///)' ) versio,serial
      endif

      do 800 iproc=1,nproc

          if ( proswi(iproc) ) then
          write (*,'(''+Process: '',a10)') procid(iproc)

c----------------------------------------------------------------------c
c         CONSTRUCT PROCESS
c----------------------------------------------------------------------c

c         Clear PDF structure

          ins = 0
          ine = 0
          ous = 0
          oue = 0
          flu = 0
          sto = 0
          dis = 0
          vel = 0

c         Fill PDF structure

C         INPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

c         scan input items table for FIRST occurence of proces
          call zoek ( procid(iproc), ninpu, inpupr, 10, ioffse )
          naanta = 0
          if ( ioffse .gt. 0 ) then

c             loop over all INPU rows related to this process

  410         continue
              naanta = naanta + 1

c             Process current row

c             Lookup item in items table
              iinpu = ioffse + naanta-1
              call zoek ( inpuit(iinpu), nitem, itemid, 10, iitem)
              if ( iitem .le. 0 ) stop 'unknown ITEM'

c             Documented items are marked for COEFEDIT.DAT
              if ( inpudo(iinpu) .eq. 'x' ) itmswi(iitem) = .true.

c             Find item properties and store in PDF structure
              if ( inpude(iinpu) .eq. 'Y' ) then
                  actdef = itemde(iitem)
              elseif ( inpude(iinpu) .eq. 'G' ) then
                  actdef = -888.
              elseif ( inpude(iinpu) .eq. 'B' ) then
                  actdef = -101.
              elseif ( inpude(iinpu) .eq. 'M' ) then
                  actdef = -11.
              elseif ( inpude(iinpu) .eq. 'O' ) then
                  actdef = -1.
              else
                  actdef = -999.
              endif
              if ( inpusx(iinpu) .eq. 1 ) then
                  ins = ins + 1
                  if ( ins .gt. insmax ) stop 'DIMENSION insmax'
                  ins_id(ins) = itemid(iitem)
                  ins_nm(ins) = adduni(itemnm(iitem),itemun(iitem))
                  if ( switui ) then
                      ins_va(ins) = itemde(iitem)
                  else
                      ins_va(ins) = actdef
                  endif
                  ins_do(ins) = inpudo(iinpu)
              else
                  ine = ine + 1
                  if ( ine .gt. inemax ) stop 'DIMENSION inemax'
                  ine_id(ine) = itemid(iitem)
                  ine_nm(ine) = adduni(itemnm(iitem),itemun(iitem))
                  if ( switui ) then
                      ine_va(ine) = itemde(iitem)
                  else
                      ine_va(ine) = actdef
                  endif
                  ine_do(ine) = inpudo(iinpu)
              endif

c             Back for next row in table INPU,
c             if it still matches current proces

              if ( (iinpu+1) .le. ninpu ) then
                  call zoek ( procid(iproc), 1, inpupr(iinpu+1),
     j                  10, jndex )
                  if ( jndex .gt. 0 ) goto 410
              endif
          endif

C         OUTPUT ITEMS ON SEGMENT LEVEL/EXCHANGE LEVEL

c         scan output items table for FIRST occurence of proces
          call zoek ( procid(iproc), noutp, outppr, 10, ioffse )
          naanta = 0
          if ( ioffse .gt. 0 ) then

c             loop over all OUTP rows related to this process

  440         continue
              naanta = naanta + 1

c             Process current row

c             Lookup item in items table
              ioutp = ioffse + naanta-1
              call zoek ( outpit(ioutp), nitem, itemid, 10, iitem)
              if ( iitem .le. 0 ) stop 'unknown ITEM'

c             Find item properties and store in PDF structure
              if ( outpsx(ioutp) .eq. 1 ) then
                  ous = ous + 1
                  if ( ous .gt. ousmax ) stop 'DIMENSION ousmax'
                  ous_id(ous) = itemid(iitem)
                  ous_nm(ous) = adduni(itemnm(iitem),itemun(iitem))
                  ous_do(ous) = outpdo(ioutp)
              else
                  oue = oue + 1
                  if ( oue .gt. ouemax ) stop 'DIMENSION ouemax'
                  oue_id(oue) = itemid(iitem)
                  oue_nm(oue) = adduni(itemnm(iitem),itemun(iitem))
                  oue_do(oue) = outpdo(ioutp)

c                 SCAN VELO and DISP TABLES FOR LINES ASSOCIATED WITH
c                 CURRENT OUTPUT ITEM ON EXCHANGE LEVEL

c                 scan dispersion lines table for FIRST occurence of item
                  call zoek ( itemid(iitem), ndisp, dispit, 10, ioffs2)
                  naant2 = 0
                  if ( ioffs2 .gt. 0 .and. .not.swit2d ) then

c                     loop over all DISP rows related to this item

  450                 continue
                      naant2 = naant2+1
                      dis = dis + 1
                      if ( dis .gt. dismax ) stop 'dimension DISMAX'

c                     Process current row

                      idisp = ioffs2 + naant2-1
                      dis_su(dis) = dispsu(idisp)
                      dis_it(dis) = dispit(idisp)
                      dis_sc(dis) = dispsc(idisp)

c                     Back for next row in table DISP,
c                     if it still matches current item

                      if ( (idisp+1) .le. ndisp ) then
                          call zoek ( itemid(iitem), 1, dispit(idisp+1),
     j                          10, jndex )
                          if ( jndex .gt. 0 ) goto 450
                      endif
                  endif

c                 scan velocity lines table for FIRST occurence of item
                  call zoek ( itemid(iitem), nvelo, veloit, 10, ioffs2)
                  naant2 = 0
                  if ( ioffs2 .gt. 0 .and. .not.swit2d ) then

c                     loop over all VELO rows related to this item

  460                 continue
                      naant2 = naant2+1
                      vel = vel + 1
                      if ( vel .gt. velmax ) stop 'dimension VELMAX'

c                     Process current row

                      ivelo = ioffs2 + naant2-1
                      vel_su(vel) = velosu(ivelo)
                      vel_it(vel) = veloit(ivelo)
                      vel_sc(vel) = velosc(ivelo)

c                     Back for next row in table VELO,
c                     if it still matches current item

                      if ( (ivelo+1) .le. nvelo ) then
                          call zoek ( itemid(iitem), 1, veloit(ivelo+1),
     j                          10, jndex )
                          if ( jndex .gt. 0 ) goto 460
                      endif
                  endif

c                 END of processing output item on exchange level!

              endif

c             Back for next row in table OUTP,
c             if it still matches current proces

              if ( (ioutp+1) .le. noutp ) then
                  call zoek ( procid(iproc), 1, outppr(ioutp+1),
     j                  10, jndex )
                  if ( jndex .gt. 0 ) goto 440
              endif
          endif

C         FLUXES

c         scan output fluxes table for FIRST occurence of proces
          call zoek ( procid(iproc), noutf, outfpr, 10, ioffse )
          if ( ioffse .gt. 0 ) then

c             loop over all FLUX rows related to this process

  470         continue
              flu = flu + 1
              if ( flu .gt. flumax ) stop 'dimension FLUMAX'

c             Process current row

c             Lookup flux in items table
              ioutf = ioffse + flu-1
c             write (11,*) ' flu ',flu,' ioutf ', ioutf
              call zoek ( outffl(ioutf), nitem, itemid, 10, iitem)
              if ( iitem .le. 0 ) stop 'unknown FLUX'

c             Find and store flux properties
              flu_id(flu) = itemid(iitem)
              flu_nm(flu) = adduni(itemnm(iitem),itemun(iitem))
              flu_do(flu) = outfdo(ioutf)

c             SCAN STOCHI TABLE FOR LINES ASSOCIATED WITH PRESENT FLUX

c             scan stochi lines table for FIRST occurence of flux
              call zoek ( itemid(iitem), nstoc, stocfl, 10, ioffs2)
              naant2 = 0
              if ( ioffs2 .gt. 0 ) then

c                 loop over all STOC rows related to this flux

  480             continue
                  naant2 = naant2+1
                  sto = sto + 1
                  if ( sto .gt. stomax ) stop 'dimension STOMAX'

c                 Process current row

                  istoc = ioffs2 + naant2-1
c             write (11,*) ' sto ',sto,' istoc ', istoc
                  sto_su(sto) = stocsu(istoc)
                  sto_fl(sto) = stocfl(istoc)
                  sto_sc(sto) = stocsc(istoc)

c                 Back for next row in table STOC,
c                 if it still matches current flux

                  if ( (istoc+1) .le. nstoc ) then
                      call zoek ( itemid(iitem), 1, stocfl(istoc+1),
     j                      10, jndex )
                      if ( jndex .gt. 0 ) goto 480
                  endif
              endif

c             Back for next row in table OUTF,
c             if it still matches current proces

              if ( (ioutf+1) .le. noutf ) then
                  call zoek ( procid(iproc), 1, outfpr(ioutf+1),
     j                  10, jndex )
                  if ( jndex .gt. 0 ) goto 470
              endif
          endif

c----------------------------------------------------------------------c
c         WRITE PROCESS
c----------------------------------------------------------------------c

c         Write PDF file (formats as in HARMONIZE to allow comparison)

          call wripdf ( procid(iproc), procnm(iproc), procco(iproc),
     j                  procfo(iproc), 15 )

          if ( nalgold.gt.0 ) then
          call exppdfnalg ( nalgold, nalgnew )
          call wripdf ( procid(iproc), procnm(iproc), procco(iproc),
     j                  procfo(iproc), 16 )
          repeat = .true.
          do while (repeat)
            call reppdfnalg ( nalgold, nalgnew,
     j                    procid(iproc), locid,
     j          procnm(iproc), locnam, repeat )
            if (repeat)
     j      call wripdf ( locid, locnam, procco(iproc),
     j                    procfo(iproc), 16 )
          enddo
          else
c         Write documentation

          call wridoc ( procid(iproc), procnm(iproc), procco(iproc),
     j                  procfo(iproc), 16 )
          endif

c         End if Proswi

          endif

  800 continue
      close (15)
      close (16)

c     Write all active coefficients to COEFEDIT.DAT
c     in the Sobek-format
      call coefed(serial,itmswi)

  900 continue
      close (11)

      stop 'Normal end'
      end
      function adduni(name,unit)
      character*50 adduni, name
      character*20 unit

      integer      lennam, lenuni, i, ihulp

c     find length of name and unit

      lennam = -1
      do 10 i = 50,1,-1
          if ( name(i:i) .ne. ' ' ) then
              lennam = i
              goto 11
          endif
   10 continue
   11 continue
      if ( lennam .le. 1 ) then
          write (*,*) ' ',name
          stop 'ADDUNI Fatal Error 1'
      endif

      lenuni = 0
      if ( unit(2:3) .eq. 'no' .and.
     j     unit(5:8) .eq. 'unit' ) then
          lenuni = 0
      else
          do 20 i = 20,1,-1
              if ( unit(i:i) .ne. ' ' ) then
                  lenuni = i
                  goto 21
              endif
   20     continue
   21     continue
      endif
      if ( lenuni .lt. 0 ) then
          write (*,*) ' ',unit
          stop 'ADDUNI Fatal Error 1'
      endif

      if ( lennam + lenuni .gt. 50 ) then
          lennam = 50-lenuni
c          write (*,'('' '',a50,1x,i2)') name,lennam
c          write (*,'('' '',a20,1x,i2)') unit,lenuni
c          stop 'ADDUNI Fatal Error 2'
      endif

      write ( adduni(1          :lennam) , '(a)' ) name(1:lennam)
      do 30 i = 1,50-lennam-lenuni
   30 adduni(lennam+i:lennam+i) = ' '
      if (lenuni.gt.0)
     j write ( adduni(50-lenuni+1:50    ) , '(a)' ) unit(1:lenuni)

      return
      end
