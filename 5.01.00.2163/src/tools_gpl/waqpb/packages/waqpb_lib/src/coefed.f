      subroutine coefed(serial,itmswi)
c
c     Create COEFEDIT.DAT file (Sobek only)
c
c     Include data structures for tables
      include 'data.inc'

      integer serial, lu_out, iitem, niteml
      logical itmswi(nitemm)
      data lu_out /129/

      niteml = 0
      do iitem = 1,nitem
          if ( itmswi(iitem) ) niteml = niteml + 1
      enddo

      open (lu_out,file='coefedit.dat')

      write (lu_out, '(i10)' ) serial
      write (lu_out, '(''NC'')' ) 
      write (lu_out, '(i6)' ) niteml
      write (lu_out, '(''COEFFICIENTS'')' ) 
      do iitem = 1,nitem
          if ( itmswi(iitem) )
c     More significant digits!!!! JvG Jan 2011
c     j    write (lu_out,'(''"'',a10,''",'',e10.3,'',1,1,1'')' ) 
     j    write (lu_out,'(''"'',a10,''",'',g15.7,'',1,1,1'')' ) 
     j    itemid(iitem),itemde(iitem)
      enddo
      write ( lu_out,'(''NG''/''1''/''GROUPS''/
     j        ''1,"Process parameters",0,0'')' )


      close (lu_out)

      return
      end
