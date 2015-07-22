c     Memory to store a PDF file (V3.98)

c     Input items at the segment level

      integer      insmax , ins
      parameter   (insmax=1100)
      character*1  ins_do(insmax)
      character*10 ins_id(insmax)
      character*50 ins_nm(insmax)
      real         ins_va(insmax)
      common /comins/ ins   , ins_id, ins_nm, ins_va, ins_do

c     Input items at the exchange level

      integer      inemax , ine
      parameter   (inemax=1000)
      character*1  ine_do(inemax)
      character*10 ine_id(inemax)
      character*50 ine_nm(inemax)
      real         ine_va(inemax)
      common /comine/ ine   , ine_id, ine_nm, ine_va, ine_do

c     Output items at the segment level

      integer      ousmax , ous
      parameter   (ousmax=2000)
      character*1  ous_do(ousmax)
      character*10 ous_id(ousmax)
      character*50 ous_nm(ousmax)
      common /comous/ ous   , ous_id, ous_nm, ous_do

c     Output items at the exchange level

      integer      ouemax , oue
      parameter   (ouemax=500)
      character*1  oue_do(ouemax)
      character*10 oue_id(ouemax)
      character*50 oue_nm(ouemax)
      common /comoue/ oue   , oue_id, oue_nm, oue_do

c     Fluxes

      integer      flumax , flu
      parameter   (flumax=200)
      character*1  flu_do(flumax)
      character*10 flu_id(flumax)
      character*50 flu_nm(flumax)
      common /comflu/ flu   , flu_id, flu_nm, flu_do

c     Stochi lines

      integer      stomax , sto
      parameter   (stomax=300)
      character*10 sto_fl(stomax), sto_su(stomax)
      real         sto_sc(stomax)
      common /comsto/ sto   , sto_fl, sto_su, sto_sc

c     Disp lines

      integer      dismax , dis
      parameter   (dismax=200)
      character*10 dis_it(dismax), dis_su(dismax)
      real         dis_sc(dismax)
      common /comdis/ dis   , dis_it, dis_su, dis_sc

c     Disp lines

      integer      velmax , vel
      parameter   (velmax=200)
      character*10 vel_it(velmax), vel_su(velmax)
      real         vel_sc(velmax)
      common /comvel/ vel   , vel_it, vel_su, vel_sc

