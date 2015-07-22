function [FI,FileName,Tp,Otherargs]=qp_fmem(cmd,varargin)
%QP_FMEM Routine for opening data files.
%   [FileInfo,FileName,FileType,Otherargs]=QP_FMEM('open',FilterSpec)

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2012 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

lasttp=qp_settings('LastFileType','');

FI=[];
FileName='';
Tp='';
FileFromCall=0;
Otherargs={};
switch cmd
   case {'open','openldb','opennew','openurl'}
      if strcmp(cmd,'opennew') || strcmp(cmd,'openldb')
         targetdir=varargin{1};
         filterspec='';
      elseif nargin>1
         filterspec=varargin{1};
         targetdir=fileparts(filterspec);
      else
         filterspec='';
         targetdir=pwd;
      end
      DoDS=0;
      if strcmp(cmd,'openurl')
         DoDS=1;
      elseif length(filterspec)>7 && isequal(lower(filterspec(1:7)),'http://')
         DoDS=1;
      elseif isempty(targetdir) || exist(targetdir)~=7
         targetdir=pwd;
      end
      filtertbl={};
      if DoDS
         if isempty(filterspec)
            % ask user for link
            FileName=uigeturl;
            if ~ischar(FileName)
               return
            end
         else
            FileName=filterspec;
         end
         FileFromCall=1;
      elseif ~isempty(filterspec) && exist(filterspec)==2
         FileName=filterspec;
         FileFromCall=1;
      else
         if ~isempty(filterspec)
            [dummypath,tmpfn,tmpext]=fileparts(filterspec);
            filterspec=[tmpfn,tmpext];
         else
            if strcmp(cmd,'openldb')
               filtertbl={'*.ldb;*.pol;*.gen;*.bna;*.shp'                'Land Boundary Files'         'tekal'};
            else
               filtertbl = qp_filefilters('selected+');
               [dum,Reorder] = sort(filtertbl(:,2));
               filtertbl=cat(1,filtertbl(Reorder,:),{'*.*','All Files' 'anyfile'});
            end
            if matlabversionnumber<6
               filterspec='*.dat';
            else
               if ~isempty(lasttp)
                   for ifilter = size(filtertbl,1):-1:1
                       if iscell(filtertbl{ifilter,3})
                           if any(strcmp(lasttp,filtertbl{ifilter,3}))
                               break
                           end
                       elseif strcmp(lasttp,filtertbl{ifilter,3})
                           break
                       end
                   end
                  if ifilter>1
                     filtertbl=filtertbl([ifilter 1:end],:);
                  end
               else
               end
               filterspec=filtertbl(:,1:2);
            end
         end
         currentdir=pwd;
         cd(targetdir);
         [fn,pn]=uigetfile(filterspec,'Select data file to open ...');
         cd(currentdir);
         if ~ischar(fn)
            return
         end
         FileName=[pn fn];
      end
      %autodetect intelligence ...
      FI=[];
      try1=1;
      trytp='nefis';
      [pn,fn,en]=fileparts(FileName);
      
      if DoDS
         trytp='NetCDF';
      elseif strmatch('sds-',lower(fn))
         trytp='waquasds';
      elseif strmatch('morf',lower(fn))
         trytp='morf';
      elseif strmatch('bagdpt',lower(fn))
         trytp='bagdpt';
      elseif strcmp('gcmplt',lower(fn)) || strcmp('gcmtsr',lower(fn))
         trytp='ecomsed-binary';
      elseif strcmp('network.ntw',lower([fn en])) || strcmp('deptop.1',lower([fn en]))
         trytp='sobek1d';
      else
         switch lower(en)
            case {'.grd','.rgf'}
               trytp='wlgrid';
            case {'.n','.e','.node','.ele'}
               trytp='nodelemesh';
            case {'.14','.gr3'}
               trytp='adcircmesh';
            case {'.mesh'}
               trytp='mikemesh';
            case {'gem'}
               trytp='geomesh';
            case {'.mat'}
               trytp='matlab';
            case {'.map'}
               trytp='pcraster';
            case {'.his','.plo','.psf','.bal'}
               trytp='delwaqbin';
            case {'.arc','.amc','.amd','.amh','.amp','.amt','.amu','.amv'}
               trytp='arcgrid';
            case {'.spw','.wnd'}
               trytp='asciiwind';
            case {'.inc','.crs','.bin'}
               trytp='fls';
            case {'.grib','.grib1','.grib2'}
               trytp='grib';
            case {'.tek','.ann','.ldb','.pol','.spl','.tka','.tkp','.tkf'}
               trytp='tekal';
            case {'.xyz'}
               trytp='samples';
            case {'.seq'}
               trytp='aukepc';
            case {'.bct'}
               trytp='bct';
            case {'.dmp'}
               trytp='CFX dmp';
            case {'.pst','.stu'}
               trytp='JSPost';
            case {'.jpg','.jpeg','.bmp','.tif','.tiff','.png','.pcx','.xwd'}
               trytp='bitmap';
            case {'.slf','.out','.res'}
               trytp='telemac';
            case '.bna'
               trytp='BNA File';
            case '.gen'
               trytp='ArcInfoUngenerate';
            case '.nc'
               trytp='NetCDF';
            case {'.fun','.daf'}
               trytp='unibest';
            case {'.shx','.shp'}
               trytp='shape';
             case {'.sma'}
                 trytp='shipma';
            case {'.sp1','.sp2'}
               trytp='SWAN spectral';
            case {'.mdm'}
               trytp='morf';
            case {'.tim'}
               trytp='DelwaqTimFile';
            case {'.bil','.hdr'}
               trytp='bil/hdr';
         end
      end
      
      %try opening the file ...
      userasked=0;
      usertrytp='';
      while isempty(FI)
         %ui_message('','Trying %s ...\n',trytp);
         %pause
         lasttp=trytp;
         switch trytp
            case 'nefis'
               try1=0;
               try
                  FI=vs_use(FileName,'quiet');
               end
               if ~isempty(FI)
                  switch lower(FI.SubType)
                     case {'delft3d-waq-map','delft3d-par-map'}
                        delwaq_results = cat(2,'DEL',upper(FI.SubType(9:11)),'_RESULTS');
                        if isstruct(vs_disp(FI,delwaq_results,[]))
                           NfsSeg=vs_disp(FI,delwaq_results,'SUBST_001');
                        else
                           NfsSeg=vs_disp(FI,lower(delwaq_results),lower('SUBST_001'));
                        end
                        NfsSeg=NfsSeg.SizeDim;
                        filterspec='';
                        maybeusegrid=1;
                        if FileFromCall
                           if nargin>2
                              filterspec=varargin{2};
                           else
                              maybeusegrid=0;
                              G=[];
                           end
                        end
                        if maybeusegrid
                           [G,GridFileName]=get_matching_grid(NfsSeg,pn,filterspec);
                        end
                        if ~isstruct(G) % cancel for grid -> use indices
                           Statw=ceil(log10(NfsSeg+1));
                           FI.SegmentName=num2cell(reshape(sprintf(strcat('%-',num2str(Statw),'i'),1:NfsSeg),Statw,NfsSeg)',2);
                           FI.SubType=[FI.SubType(1:end-3) 'his'];
                        else
                           Otherargs{1}=GridFileName;
                           F.Nfs=FI;
                           F.FileType=FI.FileType;
                           F.SubType=FI.SubType;
                           F.Grid=G;
                           FI=F;
                        end
                  end
                  if isfield(FI,'SubType')
                     Tp=FI.SubType;
                  else
                     Tp=vs_type(FI);
                  end
               end
               
               if isfield(FI,'SubType') && (strcmp(FI.SubType,'Delft3D-trim') || strcmp(FI.SubType,'Delft3D-com') || strcmp(FI.SubType,'Delft3D-trih'))
                  FI.Options=1;
               end
               trytp='matlab';
            case 'matlab'
               try
                  if isstandalone
                     FI=load(FileName);
                  else
                     FI=load('-mat',FileName);
                  end
               end
               if ~isempty(FI)
                  f=fieldnames(FI);
                  if length(f)==1 && strcmp(lower(f{1}),'data')
                     FI=getfield(FI,f{1});
                     FI.FileName=FileName;
                     Tp=trytp;
                  else
                     FI=[];
                  end
               end
               trytp='ecomsed-binary';
            case 'ecomsed-binary'
               try
                  FI=ecomsed('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                     if strcmp(FI.SubType,'GCMPLT')
                        NSeg = [FI.IM FI.JM];
                        filterspec='corners*';
                        askforgrid=1;
                        if FileFromCall
                           if nargin>2
                              filterspec=varargin{2};
                           else
                              askforgrid=0;
                              G=[];
                           end
                        end
                        if askforgrid
                           [G,GridFileName]=get_matching_grid(NSeg,pn,filterspec);
                        end
                        if isstruct(G) % cancel for grid -> use indices
                           Otherargs{1}=GridFileName;
                           FI.Grid=G;
                        else
                           error('Grid required for processing ECOMSED-GCMPLT file.')
                        end
                     elseif strcmp(FI.SubType,'MODEL_GRID')
                        Tp = 'wlgrid';
                        ui_message('error', ...
                           {'An attempt was made to reconstruct the grid coordinates', ...
                              'from the distances in the MODEL_GRID file. Significant', ...
                              'errors may arise.'})
                     end
                  end
               end
               trytp='NetCDF';
            case 'NetCDF'
               try
                  FI = nc_info(FileName);
                  FI = nc_interpret(FI);
                  %nc_dump(FileName)
                  FI.FileName = FileName;
                  Tp = trytp;
               catch
                  FI = [];
               end
               if DoDS && isempty(FI)
                  trytp='';
                  try1=0;
               else
                  trytp='sobek1d';
               end
            case 'sobek1d'
               try
                  FI=sobek('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                  end
                  FI.Data={};
                  p=fileparts(FileName);
                  files=dir(p);
                  fl={'flowmap.his','minmax.his','gsedmap.his','kafhmap.his', ...
                        'kafpmap.his','kafrmap.his','kaphmap.his','kappmap.his', ...
                        'saltmap.his','sedtmap.his','morpmap.his','sobekwq.map', ...
                        'calcpnt.his','reachseg.his','reachvol.his'};
                  %'calcdim.his','delwaq.his','delwaq.map','flowanal.his', ...
                  %'nodesvol.his','nodes_cr.his','qlat.his','qwb.his', ...
                  %'reachdim.his','reachflw.his','reachvol.his','reach_cr.his', ...
                  %'struc.his','strucdim.his','wqbou20.his'};
                  for i=1:length(files)
                     if ~isempty(strmatch(lower(files(i).name),fl,'exact'))
                        try
                           FIH=delwaq('open',fullfile(p,files(i).name));
                        catch
                           FIH=[];
                        end
                        if ~isempty(FIH)
                           FI.Data{end+1}=FIH;
                        end
                     end
                  end
               end
               trytp='pcraster';
            case 'pcraster'
               try
                  FI=pcraster('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                  end
               end
               trytp='delwaqbin';
            case 'delwaqbin'
               try
                  FI=delwaq('open',FileName);
               end
               if ~isempty(FI)
                  Tp=FI.FileType;
                  switch lower(Tp)
                     case 'delwaqlga'
                        FI.Options=1;
                     case 'delwaqmap'
                        filterspec='';
                        maybeusegrid=1;
                        if FileFromCall
                           if nargin>2
                              filterspec=varargin{2};
                           else
                              maybeusegrid=0;
                              G=[];
                           end
                        end
                        if maybeusegrid
                           [G,GridFileName]=get_matching_grid(FI.NumSegm,pn,filterspec);
                        end
                        if ~isstruct(G) % cancel for grid -> use indices
                           Statw=ceil(log10(FI.NumSegm+1));
                           FI.SegmentName=num2cell(reshape(sprintf(strcat('%-',num2str(Statw),'i'),1:FI.NumSegm),Statw,FI.NumSegm)',2);
                           FI.FileType='DelwaqHIS';
                        else
                           Otherargs{1}=GridFileName;
                           F.DwqBin=FI;
                           F.FileType=FI.FileType;
                           F.Grid=G;
                           FI=F;
                        end
                  end
                  FI.balancefile=0;
                  if strcmp(FI.FileType,'DelwaqHIS')
                     FI.Options=1;
                     [pn,fn,fne]=fileparts(FI.FileName);
                     if isequal(lower(fne),'.bal')
                        FI.balancefile=1;
                     elseif length(FI.FileName)>7 && ...
                           isequal(lower(FI.FileName(end-6:end)),'bal.his')
                        FI.balancefile=1;
                     end
                  end
               end
               trytp='fls';
            case 'fls'
               try
                  FI=fls('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                  end
               end
               trytp='grib';
            case 'grib'
               try
                  FI=grib('open',FileName);
                  if ~isfield(FI,'OK')
                     FI=[];
                  elseif FI.OK~=1
                     error(FI.Error)
                  end
               end
               if ~isempty(FI)
                  Tp=FI.FileType;
                  for b = 1:length(FI.Block)
                     if FI.Block(b).Edition>1
                        ui_message('warning','Skipping GRIB edition %i block',FI.Block(b).Edition)
                     end
                  end
               end
               trytp='arcgrid';
            case 'arcgrid'
               try
                  FI=arcgrid('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                  end
               end
               trytp='asciiwind';
            case 'asciiwind'
               try
                  FI=asciiwind('open',FileName);
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     if strcmp(FI.Header.filetype,'meteo_on_computational_grid')
                        if FileFromCall
                           if nargin>2
                              gridspec=varargin{2};
                           else
                              gridspec='';
                           end
                        else
                           mpn=fileparts(FI.FileName);
                           [gfn,gpn]=uigetfile([mpn filesep '*.grd'],'Select matching grid file ...');
                           gridspec = [gpn gfn];
                        end
                        if ~ischar(gridspec) || isempty(gridspec)
                           error('ASCIIWIND of type ''meteo_on_flow_grid'' requires grid specification.')
                        end
                        FI.Header.grid_file=wlgrid('open',gridspec);
                        if prod(size(FI.Header.grid_file.X)+1)~=FI.NVal
                           error('Number of data values in grid file does not match number of grid points.')
                        end
                        Otherargs{1}=gridspec;
                     end
                     Tp=FI.FileType;
                  end
                  if FI.Header.n_quantity==1 && ...
                          strcmp(FI.Header.quantity{1}(2:end),'_wind')
                      if FileFromCall
                          if nargin>2
                              veccomp2=varargin{2};
                          else
                              veccomp2='';
                          end
                      else
                          switch lower(FI.Header.quantity{1}(1))
                              case 'x'
                                  Y='y';
                              case 'y'
                                  Y='x';
                              otherwise
                                  Y='';
                          end
                          if ~isempty(Y)
                              ft = qp_filefilters('asciiwind');
                              ft(2,:) = {'*.*' 'All Files' 'anyfile'};
                              targetdir=fileparts(FI.FileName);
                              %
                              currentdir=pwd;
                              cd(targetdir);
                              [gfn,gpn]=uigetfile(ft(:,1:2),sprintf('Select matching %s-component wind file ...',Y));
                              cd(currentdir);
                              %
                              veccomp2 = [gpn gfn];
                          else
                              veccomp2 = '';
                          end
                      end
                      if ~isempty(veccomp2)
                          FI2=asciiwind('open',veccomp2);
                          FI=asciiwind('merge',FI,FI2);
                          if isfield(FI,'Vector')
                              Otherargs{1}=veccomp2;
                          end
                      end
                  end
               catch
                  FI=[];
               end
               trytp='waquasds';
            case 'waquasds'
               try
                  FI=waqua('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                  end
               end
               trytp='mike0';
            case 'mike0'
               try
                  FI=mike('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                  end
                  FI.Options=1;
               end
               trytp='wlgrid';
            case 'wlgrid'
               try
                  FI=wlgrid('open',FileName);
               end
               if ~isempty(FI)
                  if isequal(FI.Type,'RGF')
                     Tmp=repmat(NaN,size(FI.X)+1);
                     Tmp(1:end-1,1:end-1)=FI.X;
                     FI.X=Tmp;
                     Tmp(1:end-1,1:end-1)=FI.Y;
                     FI.Y=Tmp;
                  end
                  FI.FileType='wlgrid';
                  FI.Options=1;
                  Tp=FI.FileType;
               end
               trytp='nodelemesh';
            case 'nodelemesh'
               try
                  FI=nodelemesh('open',FileName);
               end
               if ~isempty(FI)
                  FI.Options=0;
                  Tp=FI.FileType;
               end
               trytp='adcircmesh';
            case 'adcircmesh'
               try
                  FI=adcircmesh('open',FileName);
               end
               if ~isempty(FI)
                  FI.Options=0;
                  Tp=FI.FileType;
               end
               trytp='mikemesh';
            case 'mikemesh'
               try
                  FI=mikemesh('open',FileName);
               end
               if ~isempty(FI)
                  FI.Options=0;
                  Tp=FI.FileType;
               end
               trytp='geomesh';
            case 'geomesh'
               try
                  FI=geomesh('open',FileName);
               end
               if ~isempty(FI)
                  FI.Options=0;
                  FI.DomainName = 'Layer';
                  Tp=FI.FileType;
               end
               trytp='tekal';
            case 'tekal'
               try
                  FI=tekal('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp='Tekal';
                     [pn,fn,ex]=fileparts(FI.FileName);
                     FI.can_be_ldb=1;
                     FI.combinelines=0;
                     for i=1:length(FI.Field)
                         if length(FI.Field(i).Size)~=2
                             FI.can_be_ldb=0;
                         elseif FI.Field(i).Size(2)~=2
                             FI.can_be_ldb=0;
                         elseif ~strcmp(FI.Field(i).DataTp,'numeric')
                             FI.can_be_ldb=0;
                         end
                         if ~FI.can_be_ldb
                             break
                         end
                     end
                     can_be_kub=0;
                     switch lower(ex)
                        case {'.ldb','.pol'}
                            if FI.can_be_ldb
                                FI.combinelines=1;
                            end
                        case '.kub'
                           can_be_kub=1;
                           if length(FI.Field)>1
                              can_be_kub=0;
                           elseif length(FI.Field(1).Size)~=2
                              can_be_kub=0;
                           elseif ~strcmp(FI.Field(1).DataTp,'numeric')
                              can_be_kub=0;
                           end
                     end
                     if can_be_kub
                        ppn = '';
                        if nargin>2
                           pfn=varargin{2};
                        else
                           pfn='';
                        end
                        while 1
                           if ~exist([ppn pfn])
                              cp=pwd;
                              cd(pn);
                              [pfn,ppn]=uigetfile('*.ldb;*.pol','Select matching polygon file ...');
                              cd(cp);
                           end
                           if ~ischar(pfn)
                              break
                           end
                           try
                              pfile = tekal('open',[ppn pfn]);
                              if length(pfile.Field)~=FI.Field(1).Size(1)
                                 ui_message('error','Number of values in KUBINT file (%i) does not\nmatch the number of polygons (%i)',FI.Field(1).Size(1),length(pfile.Field))
                                 ppn='';
                                 pfn='';
                              else
                                 Otherargs{1} = [ppn pfn];
                                 break
                              end
                           end
                        end
                        if ischar(pfn)
                           FI.plotonpoly=pfile;
                        end
                     end
                     FI.Options=FI.can_be_ldb;
                  end
               end
               trytp='shape';
            case 'shape'
               try
                  FI=shape('open',FileName);
               end
               if ~isempty(FI)
                  Tp=FI.FileType;
               end
               trytp='SWAN spectral';
            case 'SWAN spectral'
               try
                  FI=readswan(FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=FI.FileType;
                  end
               end
               trytp='DelwaqTimFile';
            case 'DelwaqTimFile'
               try
                  FI=delwaqtimfile(FileName);
               end
               if ~isempty(FI)
                  Tp=FI.FileType;
               end
               trytp='morf';
            case 'morf'
               try
                  FI=morf('read',FileName);
               end
               if ~isempty(FI)
                  Tp='MorfTree';
               end
               trytp='aukepc';
            case 'aukepc'
               try
                  FI=aukepc('open',FileName);
               end
               if ~isempty(FI)
                  Tp='AukePC';
               end
               trytp='bct';
            case 'bct'
               try
                  FI=bct_io('read',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check') || strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp='Bct';
                  end
               end
               trytp='CFX dmp';
            case 'CFX dmp'
               try
                  FI=cfx('open',FileName);
               end
               if ~isempty(FI)
                  Tp=trytp;
                  Tmp=FI;
                  FI=[];
                  FI.Encaps=Tmp;
                  try
                     B1=cfx1block(Tmp);
                  catch
                     B1=[];
                  end
                  FI.B1=B1;
               end
               trytp='JSPost';
            case 'JSPost'
               try
                  FI=jspost('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=trytp;
                  end
               end
               trytp='bagdpt';
            case 'bagdpt'
               try
                  FI=bagdpt('read',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=trytp;
                  end
               end
               trytp='bitmap';
            case 'bitmap'
               try
                  FI=imfinfo(FileName);
               end
               if ~isempty(FI) && isstruct(FI)
                  FI_temp.FileInfo=FI;
                  FI=FI_temp;
                  FI_temp=[];
                  %
                  % is it a series of bitmaps?
                  %
                  i=find(ismember(FileName,'1234567890'));
                  slash=max(find(FileName==filesep));
                  if ~isempty(i) && slash<max(i)
                     lasti = max(i);
                     firsti = lasti;
                     while ismember(firsti-1,i)
                        firsti = firsti-1;
                     end
                     FN_len = length(FileName);
                     files=dir([FileName(1:firsti-1) '*' FileName(lasti+1:end)]);
                     files={files.name};
                     times=repmat(NaN,1,length(files));
                     for i=1:length(files)
                        time=str2num(files{i}(firsti-slash:end-FN_len+lasti));
                        if ~isempty(time)
                           times(i)=time;
                        end
                     end
                     times=sort(times(~isnan(times)));
                     if length(times)>1
                        format='%i';
                        %
                        FN_len = cellfun('length',files);
                        if length(unique(FN_len))==1
                           i=num2str(lasti-firsti+1);
                           format=['%0' num2str(i) 'i'];
                        end
                        %
                        FI.FileInfo.times=times;
                        FI.FileInfo.format=format;
                        FI.FileInfo.prefix=FileName(1:firsti-1);
                        FI.FileInfo.postfix=FileName(lasti+1:end);
                     end
                  end
                  %
                  FI.FileType=trytp;
                  FI.FileName=FileName;
                  FI.Options=1;
                  FI.Loc=[0 0 FI.FileInfo.Width FI.FileInfo.Height];
                  [ImP,ImF,ImE]=fileparts(FI.FileName);
                  switch lower(ImE)
                     case {'.tif','.jpg','.png','.bmp'}
                        ImE=ImE([1 2 4]);
                        if ImE(3)==lower(ImE(3))
                           ImE = [ImE 'w'];
                        else
                           ImE = [ImE 'W'];
                        end
                  end
                  fid = fopen(fullfile(ImP,[ImF ImE]),'r');
                  if fid>0
                     Coords = fscanf(fid,'%f',6);
                     fclose(fid);
                     if length(Coords)==6
                        FI.Loc = [Coords(5)-Coords(1)/2 Coords(6)-Coords(4)/2+Coords(4)*FI.FileInfo.Height Coords(1)*FI.FileInfo.Width -Coords(4)*FI.FileInfo.Height];
                        if Coords(2)~=0 || Coords(3)~=0
                           ui_message('warning',{'Bitmap distortion not yet supported.','Distortion factors reset to 0.'})
                        end
                     end
                  end
                  Tp=trytp;
               else
                  FI=[];
               end
               trytp='telemac';
            case 'telemac'
               try
                  FI=telemac('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=trytp;
                  end
               end
               trytp='samples';
            case 'samples'
               FI=[];
               try
                  XYZ=samples('read',FileName);
               catch
                  XYZ=[];
               end
               if isempty(XYZ)
                  FI=[];
               elseif isstruct(XYZ)
                  if isfield(XYZ,'FileType')
                     FI=XYZ;
                     Tp=FI.FileType;
                     FI.Options=0;
                  else
                     FI=[];
                  end
               else
                  FI.XYZ=XYZ;
                  for i=1:size(FI.XYZ,2)
                     FI.Params{i}=sprintf('Parameter %i',i);
                  end
                  FI.FileType='samples';
                  FI.FileName=FileName;
                  FI.Options=0;
                  Tp=FI.FileType;
               end
               trytp='BNA File';
            case 'BNA File'
               try
                  FI=bna('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=trytp;
                  end
               end
               trytp='ArcInfoUngenerate';
            case 'ArcInfoUngenerate'
               try
                  FI=ai_ungen('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=trytp;
                  end
               end
               trytp='bil/hdr';
            case 'bil/hdr'
               try
                  FI=bil('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=trytp;
                  end
               end
               trytp='shipma';
            case 'shipma'
               try
                   FI=shipma('open',FileName);
               end
               if ~isempty(FI)
                  Tp=trytp;
                  FI.DomainName = 'Proj\Case';
                  FI.Options=1;
               end
               trytp='unibest';
            case 'unibest'
               try
                  FI=unibest('open',FileName);
               end
               if ~isempty(FI)
                  if ~isfield(FI,'Check')
                     FI=[];
                  elseif strcmp(FI.Check,'NotOK')
                     FI=[];
                  else
                     Tp=trytp;
                  end
               end
               
               trytp='';
            otherwise
               errstr = sprintf('Please check program code:\ntrytp case ''%s'' not found!',trytp);
               ui_message('error',errstr)
               return
         end
         
         if userasked
            % I had already tried everything, so it should fail again.
            % Therefore, the following isempty statement is actually
            % superfluous.
            if isempty(FI)
               ui_message('error','Error while opening\n%s\nas %s:\n%s',FileName,usertrytp,lasterr)
            end
            break
         elseif ~try1 && isempty(trytp)
            if isempty(filtertbl)
               if DoDS
                  Message=sprintf('Error while opening\n%s\nUnable to make connection.',FileName);
               else
                  Message=sprintf('Error while opening\n%s\nFile format not supported.',FileName);
               end
               ui_message('error',Message)
               break
            else
                filtertbl = qp_filefilters('all');
                [trytp,try_i]=ui_type(filtertbl(:,2),'windowtitle','Specify file format');
            end
            if isempty(trytp)
               break
            end
            usertrytp=trytp;
            trytp=filtertbl{try_i,3};
            if trytp(1)=='>'
               trytp=trytp(2:end);
            end
            userasked=1;
            lasterr('');
         end
         
         if try1
            trytp='nefis';
            try1=0;
         end
      end
end
if isempty(FI)
   lasttp=[];
end
qp_settings('LastFileType',lasttp)
