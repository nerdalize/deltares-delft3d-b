function filename=qp_export(ExpType,filenm1,DataState)
%QP_EXPORT Export data set from a QuickPlot support data source.

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

persistent savedir
if ~ischar(savedir)
    savedir='';
elseif ~isempty(savedir)
    if savedir(end)~=filesep
        savedir(end+1)=filesep;
    end
end
T_=1; ST_=2; M_=3; N_=4; K_=5;
filename=filenm1;
scalar=1;

FileInfo=DataState.FI;
Props=DataState.Props;
Domain=DataState.Domain;
Subfield=DataState.SubField;
Selected=DataState.Selected;
Ops=DataState.Ops;
componentstrings = {'x','y','z'};
if Ops.MNK
    componentstrings = {'m','n','z'};
    Props.MNK=1.5;
end

sign=1;
if isequal(ExpType(1),'-'),
    sign=-1;
    ExpType=ExpType(2:end);
end

expType=lower(ExpType);
retreive='griddata';
AllTAtOnce=0;
MATfile=0;
switch expType
    case {'csv file (time series)'}
        % assumptions: currently only time series
        AllTAtOnce=1;
        ext='csv';
    case {'grid file','grid file (old format)'}
        % assumptions: 2D, one timestep
        ext='grd';
    case {'quickin file','morsys field file','delft3d-mor field file','box file','simona box file'}
        % assumptions: 2D, one timestep
        % morsys field file: NVal=1
        ext='dep';
    case 'tekal file'
        ext='tek';
    case 'tekal file (time series)'
        AllTAtOnce=1;
        ext='tek';
    case 'spline'
        ext='spl';
    case 'landboundary file'
        ext='ldb';
    case 'tecplot file'
        ext='plt';
    case 'arcview shape'
        % assumptions: 2D, one timestep
        ext='shp';
        if strcmp(Ops.presentationtype,'')
            if Props.NVal==0
                Ops.presentationtype='grid';
            elseif Props.NVal==0.5
                Ops.presentationtype='thin dams';
            end
        end
        switch Ops.presentationtype
            case {'patches','patches with lines','grid','polylines','polygons',''}
                retreive='gridcelldata';
            case {'markers','values'}
                retreive='griddata';
            otherwise
                retreive='';
        end
    case 'sample file'
        % assumptions: one timestep
        ext='xyz';
    case {'mat file (v6)','mat file (v7)','mat file (v7.3/hdf5)'}
        % assumptions: one timestep
        MATfile=1;
        AllTAtOnce=1;
        ext='mat';
        if ~isfield(Ops,'presentationtype')
            Ops.presentationtype='dummy';
        end
        switch Ops.presentationtype
            case {'patches','patches with lines'}
                retreive='gridcelldata';
        end
        saveops={};
        switch expType
            case 'mat file (v6)'
                if matlabversionnumber>=7
                    saveops={'-v6'};
                end
            case 'mat file (v7)'
            case 'mat file (v7.3/hdf5)'
                saveops={'-v7.3'};
        end
    otherwise
        ui_message('warning','Export type %s not implemented.',ExpType);
        filename='';
        return
end

if isempty(filename)
    BaseName = Props.Name;
    BaseName = str2file(BaseName);
    [f,p] = uiputfile([savedir BaseName '.' ext], 'Save As');
    if ~ischar(f)
        return
    end
    filename=[p f];
end
[p,f,e]=fileparts(filename);
savedir=p;
if isempty(e)
    filename=cat(2,filename,'.',ext);
end

SelTim='*';
HasTime = Props.DimFlag(T_);
if HasTime
    if ~AllTAtOnce
        SelTim=Selected{T_};
        if isequal(SelTim,0)
            [Chk,sz]=qp_getdata(FileInfo,Domain,Props,'size');
            if Chk && sz(T_)>0
                SelTim=1:sz(T_);
            end
        end
    end
else % not HasTime
    SelTim = 1;
end

ntim=length(SelTim);
for f=1:ntim
    lastfield = f==ntim;
    if HasTime && ~AllTAtOnce
        Selected{T_}=SelTim(f);
    end
    LocSelected = Selected;
    LocSelected(~Props.DimFlag)=[];

    switch retreive
        case 'griddata'
            [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'griddata',Subfield{:},LocSelected{:});
        case 'gridcelldata'
            [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'gridcelldata',Subfield{:},LocSelected{:});
        otherwise
            Chk=1;
            data=[];
    end
    if ~Chk
        filename='';
        return
    end
    if isfield(data,'XYZ') && ~MATfile
        data(1).X=data.XYZ(:,:,:,1);
        data(1).Y=data.XYZ(:,:,:,2);
        if size(data(1).XYZ,4)>2
            data(1).Z=data.XYZ(:,:,:,3);
        end
    end
    if ~isempty(data)
        Units=data(1).Units;
        if isfield(Ops,'units') && ~isempty(Ops.units) && ~isempty(Units)
            dataX=qp_unitconversion(Units,Ops.units,data);
            if ~ischar(dataX)
                data=dataX;
                dataX=[];
                Units=data(1).Units;
            end
        end
        ValUnits=Units;
    end
    component='';
    if isfield(data,'XComp')
        [data,scalar,component]=computecomponent(data,Ops);
        component=[component ' of '];
        ValUnits=data(1).Units;
        if scalar
            Props.NVal=1;
        end
    end

    if f==1
        crds={};
        vars={};
        flds={};
        vars{1,end+1}='x coordinate';
        crds{1}='X';
        ypres=isfield(data,'Y');
        if ypres
            crds{1,end+1}='Y';
            vars{1,end+1}='y coordinate';
        end
        zpres=isfield(data,'Z');
        if zpres
            crds{1,end+1}='Z';
            vars{1,end+1}='z coordinate';
        end
        nCrd=length(vars);
        if isfield(data,'XComp') && Props.NVal>1
            flds{1,end+1}='XComp';
            vars{1,end+1}=sprintf('%s component of %s',componentstrings{1},Props.Name);
            if ~isempty(Units)
                vars{1,end}=cat(2,vars{1,end},' (',Units,')');
            end
        end
        if isfield(data,'YComp') && Props.NVal>1
            flds{1,end+1}='YComp';
            vars{1,end+1}=sprintf('%s component of %s',componentstrings{2},Props.Name);
            if ~isempty(Units)
                vars{1,end}=cat(2,vars{1,end},' (',Units,')');
            end
        end
        if isfield(data,'ZComp') && Props.NVal>1
            flds{1,end+1}='ZComp';
            vars{1,end+1}=sprintf('%s component of %s',componentstrings{3},Props.Name);
            if ~isempty(Units)
                vars{1,end}=cat(2,vars{1,end},' (',Units,')');
            end
        end
        if isfield(data,'Val')
            flds{1,end+1}='Val';
            vars{1,end+1}=[component Props.Name];
            if ~isempty(ValUnits)
                vars{1,end}=cat(2,vars{1,end},' (',ValUnits,')');
            end
        end
        nVar=length(vars);
        nVal=nVar-nCrd;
    end

    switch expType
        case {'csv file (time series)','tekal file (time series)'}
            NTim=max(1,length(data.Time));
            switch Props.NVal
                case 1
                    NLoc=prod(size(data.Val))/NTim;
                otherwise
                    NLoc=prod(size(data.XComp))/NTim;
            end
            expdata=zeros(6+nVal*NLoc,NTim);
            sz=[NLoc NTim];
            if isempty(data.Time)
                expdata = zeros(6,1);
            else
                expdata(1:6,:)=transpose(datevec(data.Time*(1+eps)));
                expdata(6,:)=floor(expdata(6,:));
            end
            for fld=1:length(flds)
                FldData=getfield(data,flds{fld});
                expdata(6+fld+(0:NLoc-1)*nVal,:)=reshape(FldData(:,:)',sz);
            end
            %
            for i = 2:NLoc
                len = length(vars);
                vars(end+(1:nVal)) = vars(nCrd+1:nVar);
            end
            %
            [Chk,szvar]=qp_getdata(FileInfo,Domain,Props,'size');
            if isequal(Selected{K_},0)
                Selected{K_} = 1:szvar(K_);
            end
            if isempty(Selected{ST_})
                if ~isempty(Selected{N_})
                    stations = sprintf('m=%i, n=%i',Selected{M_},Selected{N_});
                elseif ~isempty(Selected{M_})
                    stations = sprintf('m=%i',Selected{M_});
                else
                    stations = 'location unknown';
                end
                stations = {stations};
            else
                [Chk,stations]=qp_getdata(FileInfo,Domain,Props,'stations');
                if ~Chk
                    stations = cell(1,szvar(ST_));
                    for i = 1:szvar(ST_)
                        stations{i} = sprintf('station %i',i);
                    end
                end
                if ~isequal(Selected{ST_},0)
                    stations = stations(Selected{ST_});
                end
            end
            %
            i = 0;
            for k = 1:length(Selected{K_})
                for s = 1:length(stations)
                    i = i+1;
                    Loc = sprintf('%s - layer %i',stations{s},Selected{K_}(k));
                    for j = 1:nVal
                        vars{nCrd+(i-1)*nVal+j} = [vars{nCrd+(i-1)*nVal+j} ' - ' Loc];
                    end
                end
            end
            %
            nVar = nCrd+NLoc*nVal;
            switch expType
                case 'csv file (time series)'
                    fid=fopen(filename,'wt');
                    if fid<0
                        error(['Could not create or open: ',filename])
                    end
                    Format=cat(2,'%04d-%02d-%02d %02d:%02d:%02d',repmat(', %14.6g',1,size(expdata,1)-6),'\n');
                    if size(expdata,1)-5>256
                        ui_message('error','Number of columns exceeds 256. Too many data columns for Excel!')
                    end
                    fprintf(fid,cat(2,'date and time',repmat(',%s',1,nVar-nCrd),'\n'),vars{nCrd+1:nVar});
                    Str=sprintf(Format,expdata);
                    Str=strrep(Str,'NaN','');
                    fprintf(fid,'%s',Str);
                    fclose(fid);
                case 'tekal file (time series)'
                    cmnt={};
                    cmnt{1}='column 1 = Date';
                    cmnt{2}='column 2 = Time';
                    for i=nCrd+1:nVar
                        ic=i-nCrd+2;
                        cmnt{ic}=sprintf('column %i = %s',ic,vars{i});
                    end
                    FI.Field(1).Comments=cmnt;
                    FI.Field(1).Name=data.Name;
                    times=expdata(1:6,:)';
                    expdata=expdata([1 4  7:end],:)';
                    expdata(:,1)=times(:,1)*10000+times(:,2)*100+times(:,3);
                    expdata(:,2)=times(:,4)*10000+times(:,5)*100+times(:,6);
                    FI.Field(1).Data=expdata;
                    tekal('write',filename,FI);
            end
        case {'grid file','grid file (old format)'}
            G.X=data.X(1:end-1,1:end-1);
            G.Y=data.Y(1:end-1,1:end-1);
            G.CoordSys='Cartesian';
            if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                G.CoordSys='Spherical';
            end
            switch expType
                case 'grid file'
                    wlgrid('write',filename,G);
                case 'grid file (old format)'
                    wlgrid('writeold',filename,G);
            end
        case {'quickin file','morsys field file','delft3d-mor field file','box file','simona box file'}
            for fld=1:length(flds)
                Temp=getfield(data,flds{fld});
                Temp(isnan(Temp))=-999;
                if sign<0
                    expdata(fld).Data=-Temp;
                else
                    expdata(fld).Data=Temp;
                end
            end
            switch expType
                case 'quickin file'
                    wldep('write',filename,'format',Ops.expformat,expdata);
                case 'delft3d-mor field file'
                    wlfdep('write',filename,expdata.Data);
                case 'simona box file'
                    boxfile('write',filename,expdata.Data);
            end
        case {'tekal file','spline','landboundary file'}
            expdata=zeros([size(data.X) nVar]);
            dims(1:ndims(data.X))={':'};
            cmnt={};
            for i=1:nVar
                cmnt{i}=sprintf('column %i = %s',i,vars{i});
            end
            locflds=cat(2,crds,flds);
            for fld=1:length(locflds)
                expdata(dims{:},fld)=getfield(data,locflds{fld});
            end
            switch expType
                case 'spline'
                    expdata=squeeze(expdata);
                    %
                    % the following line initially made sense when skipping
                    % over small gaps in grid lines, but it doesn't work in
                    % the cases of (a) big gaps in grid lines and (b) lines
                    % from shape files.
                    %
                    %expdata(any(isnan(expdata),2),:)=[];
                case 'landboundary file'
                    expdata=squeeze(expdata);
                    expdata(any(isnan(expdata),2),:)=999.999;
                otherwise
                    expdata(isnan(expdata))=-999;
            end
            if ~isempty(data.Time) && ~isnan(data.Time)
                cmnt={sprintf('time     = %s',datestr(data.Time,0)),cmnt{:}};
            end
            xx.Field(f).Comments=cmnt;
            xx.Field(f).Name=sprintf('F%3.3i',f);
            xx.Field(f).Data=expdata;
            if lastfield
                if strcmp(expType,'spline')
                    landboundary('write',filename,{xx.Field.Data},'dosplit','-1','format','S%3.3i');
                else
                    tekal('write',filename,xx);
                end
            end
        case 'tecplot file'
            if f==1
                vars{1,end+1}='Active';
                xx.Variables=vars;
                nVar=length(vars);
            end
            expdata=zeros([size(data.X) nVar]);
            dims(1:ndims(data.X))={':'};
            %
            Act=1;
            i=0;
            Flds=cat(2,crds,flds);
            for fldi=1:length(Flds)
                if isfield(data,Flds{fldi})
                    tmp=getfield(data,Flds{fldi});
                    NaNs=isnan(tmp);
                    Act=Act & ~NaNs;
                    tmp(NaNs)=min(tmp(:));
                    i=i+1;
                    expdata(dims{:},i)=tmp;
                end
            end
            i=i+1;
            expdata(dims{:},i)=Act;
            expdata(isnan(expdata))=-999;
            %
            xx.Zone(f).Title=datestr(data.Time,0);
            xx.Zone(f).Data=expdata;
            if lastfield
                tecplot('write',filename,xx);
            end
        case 'arcview shape'
           if isfield(data,'XDam')
              Ops.presentationtype = 'thin dams';
           end
            switch Ops.presentationtype
                case {'patches','patches with lines','markers','values','grid','polylines','polygons',''}
                    xy=[];
                    if isfield(Props,'Geom') && (strcmp(Props.Geom,'POLYL') || strcmp(Props.Geom,'POLYG'))
                        vNaN=isnan(data.X);
                        if any(vNaN)
                            bs=findseries(~vNaN);
                        else
                            bs=[1 length(vNaN)];
                        end
                        xyc={};
                        for i=size(bs,1):-1:1
                            xyc{i}=[data.X(bs(i,1):bs(i,2)) data.Y(bs(i,1):bs(i,2))];
                        end
                        vals={};
                        if isfield(data,'Val')
                           vals={data.Val(bs(:,1))};
                        end
                        if strcmp(Ops.facecolour,'none')
                           shp_type = 'polyline';
                        else
                           shp_type = 'polygon';
                        end
                        shapewrite(filename,shp_type,xyc,vals{:})
                    else
                        d=1;
                        if isfield(Props,'Tri') && Props.Tri
                            if strcmp(retreive,'gridcelldata')
                                xv=data(d).XYZ(1,:,1,1:2);
                                xv=reshape(xv,[size(xv,2) 2]);
                                fv=data(d).TRI;
                                rm=[];
                            else
                                xy=[data(d).X(:) data(d).Y(:)];
                                rm=any(isnan(xy),2);
                                xy(rm,:)=[];
                            end
                        else
                            data(d).X=data(d).X(:,:,1); % remove 3rd dimension when appropriate
                            data(d).Y=data(d).Y(:,:,1); % remove 3rd dimension when appropriate
                            if strcmp(retreive,'gridcelldata')
                                faces=reshape(1:prod(size(data(d).X)),size(data(d).X));
                                faces=faces(1:end-1,1:end-1);
                                xv=[data(d).X(:) data(d).Y(:)];
                                fv=[faces(:) faces(:)+1 faces(:)+size(data(d).X,1)+1 faces(:)+size(data(d).X,1)];
                                xx=data(d).X(fv);
                                yy=data(d).Y(fv);
                                rm=any(isnan(xx),2)|any(isnan(yy),2);
                                fv(rm,:)=[];
                            else
                                xy=[data(d).X(:) data(d).Y(:)];
                                rm=any(isnan(xy),2);
                                xy(rm,:)=[];
                            end
                        end
                        cv=[];
                        cLabels={};
                        if isfield(data,'XComp')
                            cv=[data(d).XComp(:)];
                            cLabels{end+1}='X comp.';
                        end
                        if isfield(data,'YComp')
                            cv=[cv data(d).YComp(:)];
                            cLabels{end+1}='Y comp.';
                        end
                        if isfield(data,'ZComp')
                            cv=[cv data(d).ZComp(:)];
                            cLabels{end+1}='Z comp.';
                        end
                        if isfield(data,'Val')
                            cv=[cv data(d).Val(:)];
                            cLabels{end+1}=component;
                        end
                        if isempty(cv)
                            %
                            % grid only ... export M, N coordinate?
                            %
                            cv={};
                        else
                            cLabels=strrep(cLabels,' ','_');
                            %
                            cv(rm,:)=[];
                            rm=any(isnan(cv),2);
                            cv(rm,:)=[];
                            if isempty(xy)
                                fv(rm,:)=[];
                            else
                                xy(rm,:)=[];
                            end
                            cv={cLabels,cv};
                        end
                        if strcmp(retreive,'gridcelldata')
                            %
                            % make sure that polygons are stored clockwise ...
                            %
                            if clockwise(data(d).X(fv(1,:)),data(d).Y(fv(1,:)))<0
                                fv=fliplr(fv);
                            end
                            shapewrite(filename,xv,fv,cv{:})
                        else
                            shapewrite(filename,'point',xy,cv{:})
                        end
                    end
                case {'vector','contour lines','coloured contour lines','contour patches','contour patches with lines','thin dams'}
                    TempFg=figure('visible','off');
                    TempAx=axes('parent',TempFg);
                    if isequal(Ops.presentationtype,'contour lines')
                        Ops.presentationtype='coloured contour lines';
                    end
                    PS=DataState;
                    PS.Ops=Ops;
                    PS.Parent=TempAx;
                    PS.Handles=[];
                    PS.Stations={};
                    [hNew,Error,Info]=qp_plot(PS);
                    %
                    % process handles in reverse order for correct overlay effect in case of patches
                    %
                    if iscell(hNew)
                        hNew=cat(1,hNew{:});
                    end
                    hNew0=hNew(1);
                    hNew=hNew(end:-1:1);
                    switch Ops.presentationtype
                        case 'coloured contour lines'
                            xy=get(hNew,'vertices');
                            cv=get(hNew,'facevertexcdata');
                            UD=get(hNew0,'userdata');
                            Thresholds=UD.XInfo.Thresholds;
                            for i=1:length(xy)
                                xy{i}=xy{i}(1:end-1,:);
                                cv{i}=Thresholds(cv{i}(1));
                            end
                            cv=cat(1,cv{:});
                            cLabels={'Value'};
                            shapewrite(filename,'polyline',xy,cLabels,cv)
                        case 'thin dams'
                            x=get(hNew,'xdata');
                            y=get(hNew,'ydata');
                            switch get(hNew,'type')
                                case 'line'
                                    xy=cell(1,size(x,2)/3);
                                    for i=1:length(xy)
                                        xy{i}=[x((i-1)*3+(1:2));y((i-1)*3+(1:2))]';
                                    end
                                    shapewrite(filename,'polyline',xy)
                                case 'surface'
                                    xy=cell(1,size(x,2)/3);
                                    for i=1:length(xy)
                                        xy{i}=[x(:,(i-1)*3+1) y(:,(i-1)*3+1)];
                                    end
                                    cv=get(hNew,'cdata');
                                    cv=cv(1,1:3:end)';
                                    shapewrite(filename,'polyline',xy,{'Value'},cv)
                            end
                        case {'contour patches','contour patches with lines'}
                            xy=get(hNew,'vertices');
                            cv=get(hNew,'facevertexcdata');
                            UD=get(hNew0,'userdata');
                            Thresholds=UD.XInfo.Thresholds;
                            nThresh=length(Thresholds);
                            cv(cellfun('isempty',cv))={0};
                            cv=cat(1,cv{:});
                            for i=1:size(cv,1)
                                ci=cv(i,1);
                                if ci==0
                                    cv(i,1)=NaN;
                                    cv(i,2)=NaN;
                                elseif ci<nThresh
                                    cv(i,1)=Thresholds(ci);
                                    cv(i,2)=Thresholds(ci+1);
                                else
                                    cv(i,1)=Thresholds(ci);
                                    cv(i,2)=NaN;
                                end
                            end
                            cLabels={'Min','Max'};
                            %
                            for i=length(xy):-1:1
                                if size(xy{i},1)==1
                                    xy(i) = [];
                                    cv(i,:) = [];
                                end
                            end
                            inside = false(length(xy));
                            s = warning('query','MATLAB:inpolygon:ModelingWorld');
                            warning('off','MATLAB:inpolygon:ModelingWorld')
                            for i=1:length(xy)
                               for j=1:length(xy)
                                  if i~=j
                                     inside(i,j) = all(inpolygon(xy{i}(:,1),xy{i}(:,2),xy{j}(:,1),xy{j}(:,2)));
                                  end
                               end
                            end
                            warning(s);
                            %
                            changed = 1;
                            while changed
                               changed = 0;
                               outerpolys = find(~any(inside,2));
                               for i = 1:length(outerpolys)
                                  outerpoly = outerpolys(i);
                                  %
                                  % find polygons that fit inside this
                                  % polygon, but not in any other.
                                  %
                                  inpoly = find(inside(:,outerpoly));
                                  biggestinnerpolys = inpoly(sum(inside(inpoly,:),2)==1);
                                  for j = 1:length(biggestinnerpolys)
                                     biggestinnerpoly = biggestinnerpolys(j);
                                     %
                                     % remove biggestinnerpoly from this
                                     % outerpoly. We should distinguish two
                                     % cases:
                                     %  - simple case in which the inner
                                     %    polygon is strictly inside the
                                     %    outer polygon. The polygon should
                                     %    then just be appended as second
                                     %    part/hole.
                                     %  - the inner polygon may be partly
                                     %    aligned with the outer polygon
                                     %    and this case should be handled
                                     %    by cutting away part of the
                                     %    polygon.
                                     %
                                     xy1 = xy{outerpoly};
                                     xy2 = flipud(xy{biggestinnerpoly});
                                     %
                                     [aligned,index] = ismember(xy2,xy1,'rows');
                                     %
                                     if ~any(aligned)
                                        %
                                        % simple case
                                        %
                                        xy{outerpoly}=[xy1;xy2];
                                     else
                                        %
                                        % partial alignment
                                        %
                                        npnt1 = size(xy1,1);
                                        npnt2 = size(xy2,1);
                                        %
                                        % make sure that the first point is
                                        % the first aligned point
                                        %
                                        if ~aligned(1)
                                           %
                                           % find first point aligned
                                           % (could be implemented in
                                           % recent MATLAB versions using
                                           % find first).
                                           %
                                           i2 = 1;
                                           while ~aligned(i2)
                                              i2 = i2+1;
                                           end
                                           %
                                           % renumber such that first point
                                           % aligned becomes first point.
                                           %
                                           renumber = [i2:npnt2 2:i2];
                                           xy2 = xy2(renumber,:);
                                           aligned = aligned(renumber);
                                           index = index(renumber);
                                        end
                                        %
                                        % determine last point aligned
                                        %
                                        i2 = 1;
                                        i1 = index(i2);
                                        if i1==1
                                           i1 = npnt1;
                                        end
                                        while isequal(xy2(i2+1,:),xy1(i1-1,:))
                                           i2 = i2+1;
                                           i1 = i1-1;
                                           if i1==1 % wrap around if necessary
                                              i1 = npnt1;
                                           end
                                        end
                                        %
                                        % determine first point aligned
                                        %
                                        j2 = npnt2;
                                        j1 = index(j2);
                                        if j1==npnt1
                                           j1 = 1;
                                        end
                                        while isequal(xy2(j2-1,:),xy1(j1+1,:))
                                           j2 = j2-1;
                                           j1 = j1+1;
                                           if j1==npnt1 % wrap around if necessary
                                              j1 = 1;
                                           end
                                        end
                                        %
                                        % now stick the non-aligned parts
                                        % of the two polygons together
                                        %
                                        xy{outerpoly}=[xy2(i2:j2,:);xy1(j1+1:i1,:)];
                                     end
                                     %
                                     % polygons inside biggestinnerpoly don't
                                     % fit inside outerpoly anymore.
                                     %
                                     inside(inside(:,biggestinnerpoly),outerpoly)=0;
                                     %
                                     % biggestinnerpoly itself doesn't fit
                                     % inside outerpoly anymore.
                                     %
                                     inside(biggestinnerpoly,outerpoly)=0;
                                     changed = 1;
                                  end
                               end
                            end
                            %
                            shapewrite(filename,xy,cLabels,cv)
                        case 'vector'
                            x1=get(hNew(2),'xdata');
                            y1=get(hNew(2),'ydata');
                            x2=get(hNew(3),'xdata');
                            y2=get(hNew(3),'ydata');
                            switch get(hNew(2),'type')
                               case 'line'
                                  x1=x1';
                                  x2=x2';
                                  y1=y1';
                                  y2=y2';
                                  values={};
                               case 'patch'
                                  x1=x1(:,1);
                                  x2=x2(:,1);
                                  y1=y1(:,1);
                                  y2=y2(:,1);
                                  v1=get(hNew(2),'cdata');
                                  v2=get(hNew(3),'cdata');
                                  cv=[v1(1:4:end,1);v2(1:3:end,1)];
                                  UD=get(hNew(3),'userdata');
                                  values={{UD.PlotState.Ops.vectorcolour} cv};
                            end
                            xy=[x1 y1; x2 y2];
                            seps=[0;find(isnan(xy(:,1)))];
                            %
                            n = seps(2:end)-seps(1:end-1)-1;
                            xy_cell=cell(sum(n>1),1);
                            for i=1:length(n)
                                if n(i)>1
                                    xy_cell{i}=xy(seps(i)+(1:n(i)),:);
                                end
                            end
                            %
                            shapewrite(filename,'polyline',xy_cell,values{:});
                    end
                    delete(TempFg);
            end
        case 'sample file'
            x=0; y=0; z=0; sz=[];
            if isfield(data,'X')
                x=1;
                sz=size(data.X);
            end
            if isfield(data,'Y')
                y=1;
                sz=size(data.Y);
            end
            if isfield(data,'Z')
                z=1;
                sz=size(data.Z);
            end
            if isempty(sz)
                if Props.NVal==1
                    sz=size(data.Val);
                elseif Props.NVal>1
                    sz=size(data.XComp);
                end
            end
            xyz=x+y+z;
            expdata=zeros([nVar prod(sz)]);
            if x
                expdata(1,:)=data.X(:)';
            end
            if y
                expdata(x+1,:)=data.Y(:)';
            end
            if z
                expdata(x+y+1,:)=data.Z(:)';
            end
            if isfield(data,'XComp') && Props.NVal>1
                xyz=xyz+1;
                expdata(xyz,:)=data.XComp(:)';
            end
            if isfield(data,'YComp') && Props.NVal>1
                xyz=xyz+1;
                expdata(xyz,:)=data.YComp(:)';
            end
            if isfield(data,'ZComp') && Props.NVal>1
                xyz=xyz+1;
                expdata(xyz,:)=data.ZComp(:)';
            end
            if isfield(data,'Val')
                xyz=xyz+1;
                expdata(xyz,:)=data.Val(:)';
            end
            flag=any(isnan(expdata),1);
            expdata=expdata(:,~flag);
            fid=fopen(filename,'wt');
            if fid<0
                error(['Could not create or open: ',filename])
            end
            fprintf(fid,'"%s" ',vars{:});
            fprintf(fid,'\n');
            Format=repmat(' %14.6f',1,size(expdata,1));
            Format=[Format(2:end) '\n'];
            fprintf(fid,Format,expdata);
            fclose(fid);
        case {'mat file','mat file (v6)','mat file (v7)','mat file (v7.3/hdf5)'}
            if scalar && isfield(data,'XComp')
                data.Name = [data.Name ', ' Ops.vectorcomponent];
                if isfield(data,'XComp')
                    data = rmfield(data,'XComp');
                end
                if isfield(data,'YComp')
                    data = rmfield(data,'YComp');
                end
                if isfield(data,'ZComp')
                    data = rmfield(data,'ZComp');
                end
            end
            save(filename,'data',saveops{:});
    end
end
