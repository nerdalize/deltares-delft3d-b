function varargout=d3d_waqfil(FI,domain,field,cmd,varargin)
%D3D_WAQFIL QP support for Delft3D-WAQ and -PART map and history files.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%
%   The DataFld can only be either an element of the DataProps structure.

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

%========================= GENERAL CODE =======================================
T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments');
elseif nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'optionstransfer'
            varargout{1}=optionstransfer(FI,cmd);
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{1})};
        return
    case 'stations'
        varargout={readsts(FI,Props,0)};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    case 'plot'
        if isfield(Props,'BalSubFld')
            % balance plot
            Parent=varargin{1};
            Ops=varargin{2};
            Station=varargin{3};
            [Time,Data]=delwaq('read',FI,Props.BalSubFld{1},Station,0);
            Labels=Props.BalSubFld{2};
            transin=strmatch('Transport In',Labels);
            transout=strmatch('Transport Out',Labels);
            if isfield(FI,'nettransport') && FI.nettransport
                if ~isempty(transin) && ~isempty(transout)
                    Data(transin,:,:) = Data(transin,:,:)+Data(transout,:,:);
                    Labels(transin) = {'Net transport'};
                    Labels(transout) = [];
                    Data(transout,:,:) = [];
                else
                    transin=strmatch('Transp in',Labels);
                    transout=strmatch('Transp out',Labels);
                    if ~isempty(transin) && ~isempty(transout)
                        Data(transin,:,:) = Data(transin,:,:)+Data(transout,:,:);
                        Labels{transin} = 'Net transport';
                        Labels(transout) = [];
                        Data(transout,:,:) = [];
                    end
                end
            end
            hNew=balanceplot(Time,squeeze(Data)','parent',Parent,'color',Ops.colour);
            for i=1:length(Labels)
                Labels{i}=strrep(Labels{i},'_','\_');
            end
            legend(Parent,hNew(end-(0:length(Labels)-1)),Labels,3);
            tick(Parent,'x','autodate')
            setappdata(Parent,'AxesType','Time-<blocking>')
            LocationStr=readsts(FI,Props,Station);
            set(get(Parent,'title'),'string',LocationStr,'interpreter','none')
            set(get(Parent,'xlabel'),'string','time \rightarrow')
            set(get(Parent,'ylabel'),'string',[Props.Name,' (',Props.Units,') \rightarrow'])
        else
            % limiting factors
            Parent=varargin{1};
            Ops=varargin{2};
            hNew=plotlimitingfactors(FI,Parent,varargin(3:end),'color',Ops.colour);
            setappdata(Parent,'AxesType','LimitingFactorsAxes')
            setappdata(getappdata(Parent,'LimitingFactorsAxes'), ...
                'AxesType','LimitingFactorsAxes2')
        end
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);

subf=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    Props.Val1=Props.SubFld{1}(varargin{1});
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

% select appropriate timestep ...
sz=getsize(FI,Props);
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
end

% select appropriate spatial indices ...
%================== NEFIS SPECIFIC CODE =======================================
%if DimFlag(M_)&& DimFlag(N_)
%  sz([M_ N_])=sz([N_ M_]);
%  idx([M_ N_])=idx([N_ M_]);
%end

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
        if (i~=K_) && ~strcmp(subtype,'plot') && ~strcmp(Props.Geom,'POLYG')
            if DataInCell && isequal(idx{i},1)
                idx{i}=[1 2];
                ind{i}=2;
            elseif DataInCell && idx{i}(1)==1
                ind{i}=2:length(idx{i});
            else
                if idx{i}(1)==1
                    idx{i}=idx{i};
                    ind{i}=1:length(idx{i});
                else
                    idx{i}=[idx{i}(1) idx{i}];
                    ind{i}=(1:(length(idx{i})-1))+1;
                end
            end
        else % i==K_ || strcmp(subtype,'plot')
            ind{i}=1:length(idx{i});
        end
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end
if isbinary
    if isequal(idx{T_},1:sz(T_))
        % don't change it yet
    elseif ~isequal(size(idx{T_}),[1 1]) && sz(T_)>0
        % error('Can only read one or all timesteps from Delwaq bin file.')
    end
end

% read grid ...
x=[];
y=[];
z=[];
ZUnits='';
XUnits='';
%if XYRead || strcmp(subtype,'plot')
%
missingvalue = clip2single(999.999);
%
%======================== SPECIFIC CODE =======================================
switch subtype
    case 'history'
        if Props.DimFlag(ST_)
            x=idx{ST_};
        else
            x=idx{M_};
        end
        if DimFlag(K_)
            if DataInCell
                idxK_=[idx{K_} idx{K_}(end)+1];
            else
                idxK_=idx{K_};
            end
            cthk=-(0:sz(K_));
            cthk=cthk(idxK_);
            cthk=reshape(cthk,[1 1 length(cthk)]);
            z=repmat(cthk,[1 length(x) 1]);
        end
    case 'grid'
        x=FI.X(idx{[M_ N_]});
        y=FI.Y(idx{[M_ N_]});
        if DimFlag(K_)
            x=reshape(x,[1 size(x)]);
            y=reshape(y,[1 size(y)]);
            x=repmat(x,[1 1 1 length(idx{K_})+DataInCell]);
            y=repmat(y,[1 1 1 length(idx{K_})+DataInCell]);
        end
    case {'map','plot'}
        switch subtype
            case 'map'
                switch FI.Grid.FileType
                    case 'Serafin'
                        x=FI.Grid.Discr.X(idx{M_});
                        y=FI.Grid.Discr.Y(idx{M_});
                    case 'netCDF'
                        if DataInCell
                           [x, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.BCoordinates{1},FI.Grid.CoordDims);
                           [y, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.BCoordinates{2},FI.Grid.CoordDims);
                        else
                           [x, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.CCoordinates{1},FI.Grid.CoordDims(1));
                           [y, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.CCoordinates{2},FI.Grid.CoordDims(1));
                        end
                        if isfield(FI.Grid,'Aggregation') && ~isempty(FI.Grid.Aggregation)
                            [agg, errmsg] = qp_netcdf_get(FI.Grid,FI.Grid.Aggregation,FI.Grid.AggregationDims);
                            clip = isnan(agg);
                            x(clip,:)=[];
                            y(clip,:)=[];
                            %agg(clip)=[]; % For aggregation, use agg or FI.Grid.Index
                        end
                        x=x(idx{M_},:);
                        y=y(idx{M_},:);
                        XUnits = FI.Grid.Unit;
                    case 'arcgrid'
                        eidx=idx;
                        eidx{M_}=eidx{M_}+1;
                        eidx{N_}=eidx{N_}+1;
                        x=transpose(FI.Grid.XCorner+((1:FI.Grid.NCols+1)-1)*FI.Grid.CellSize(1))*ones(1,FI.Grid.NRows+1);
                        y=ones(FI.Grid.NCols+1,1)*(FI.Grid.YCorner+(FI.Grid.NRows+1)*FI.Grid.CellSize(2)-(1:FI.Grid.NRows+1)*FI.Grid.CellSize(2));
                        x=x(eidx{[M_ N_]});
                        y=y(eidx{[M_ N_]});
                    otherwise
                        if DimFlag(M_) && DimFlag(N_)
                            x=FI.Grid.X(idx{[M_ N_]});
                            y=FI.Grid.Y(idx{[M_ N_]});
                        end
                end
            case 'plot'
                if isbinary
                    X=LocFI.PlotWindow;
                else
                    [X,Chk]=vs_let(LocFI,feval(casemod,[DELWAQ '_PARAMS']),feval(casemod,'PLOT_WINDOW'),'quiet');
                end
                gidx=idx;
                if DataInCell
                    gidx{M_}=[gidx{M_}(1)-1 gidx{M_}];
                    gidx{N_}=[gidx{N_}(1)-1 gidx{N_}];
                else
                    gidx{M_}=gidx{M_}-0.5;
                    gidx{N_}=gidx{N_}-0.5;
                end
                x=repmat(X(1)+(X(2)-X(1))*(gidx{N_})/sz(N_),length(gidx{M_}),1);
                y=repmat(transpose(X(3)+(X(4)-X(3))*(gidx{M_})/sz(M_)),1,length(gidx{N_}));
        end
        if DimFlag(K_)
            if isbinary
                names=LocFI.SubsName;
            else
                [names,Chk]=vs_get(LocFI,feval(casemod,[DELWAQ '_PARAMS']),feval(casemod,'SUBST_NAMES'),'quiet');
            end
            ld=strmatch('localdepth',lower(names));
            if iscell(ld)
                if strcmp(subtype,'plot') && (length(ld)~=sz(K_)) && ~isempty(ld)
                    ui_message('warning', ...
                        {sprintf('Number of LocalDepth layers does not match number of layers of %s.',Props.Name), ...
                        'Z coordinate based on LocalDepth disabled.'});
                    ld=[];
                end
            end
            wlflag=strmatch('zcoordwaterlevel',lower(names));
            if DataInCell
                idxK_=[idx{K_} idx{K_}(end)+1];
            else
                idxK_=idx{K_};
            end
            if isempty(ld)
                cthk=-(0:sz(K_));
                cthk=cthk(idxK_);
                cthk=reshape(cthk,[1 1 1 length(cthk)]);
                z=repmat(cthk,[1 size(x) 1]);
            else
                ZUnits = 'm';
                if isbinary
                    if iscell(ld)
                        for k=length(ld):-1:1
                            [T,Tmp]=delwaq('read',LocFI,ld(k),0,idx{T_});
                            % only one substance, so, size(Tmp,1)=1
                            Tmp=permute(Tmp,[3 2 1]);
                            z(:,:,k)=Tmp;
                        end
                    else
                        [T,z]=delwaq('read',LocFI,ld,0,idx{T_});
                        z=permute(z,[3 2 1]);
                    end
                    if ~isempty(wlflag)
                        [T,wl]=delwaq('read',LocFI,wlflag,0,idx{T_});
                        wl=permute(wl,[3 2 1]);
                    end
                else
                    if iscell(ld)
                        for k=length(ld):-1:1
                            [z(:,:,k),Chk]=vs_let(LocFI,feval(casemod,[DELWAQ '_RESULTS']),idx(T_), ...
                                feval(casemod,sprintf('SUBST_%3.3i',ld(k))),'quiet');
                        end
                    else
                        [z,Chk]=vs_let(LocFI,feval(casemod,[DELWAQ '_RESULTS']),idx(T_), ...
                            feval(casemod,sprintf('SUBST_%3.3i',ld)),'quiet');
                    end
                    if ~isempty(wlflag)
                        [wl,Chk]=vs_let(LocFI,feval(casemod,[DELWAQ '_RESULTS']),idx(T_), ...
                            feval(casemod,sprintf('SUBST_%3.3i',wlflag)),'quiet');
                    end
                end
                if isfield(Props,'BedLayer') && Props.BedLayer == -1
                    NoSegFlow=prod(sz([M_ N_ K_]));
                    if size(z,2)>NoSegFlow
                        z=z(:,1:NoSegFlow);
                        if wlflag
                            wl=wl(:,1:NoSegFlow);
                        end
                    end
                end
                %
                z(z==-999)=NaN;
                z(z==missingvalue)=NaN;
                if wlflag
                    wl(wl==-999)=NaN;
                    wl(wl==missingvalue)=NaN;
                end
                if strcmp(subtype,'plot')
                    z=reshape(z,[size(z,1) sz([M_ N_ K_])]);
                    if wlflag
                        wl=reshape(wl,[size(z,1) sz(M_) sz(N_)]);
                    end
                else
                    z=z(:,max(FI.Grid.Index,1));
                    z=reshape(z,[size(z,1) size(FI.Grid.Index)]);
                    nact=FI.Grid.Index<=0;
                    if wlflag
                        wl=wl(:,max(FI.Grid.Index(:,:,1),1));
                        wl=reshape(wl,[size(z,1) size(FI.Grid.Index(:,:,1))]);
                    end
                end
                if isempty(wlflag)
                    szz=[size(z) 1];
                    wl=repmat(0,szz([1 2 3]));
                    wl(isnan(z(:,:,:,1)))=NaN;
                end
                if DataInCell
                    for i=1:size(z,1)
                        if strcmp(subtype,'map')
                            z(i,nact)=NaN;
                        end
                        kmax=size(z,4);
                        for k=kmax+1:-1:2
                            z(i,:,:,k)=z(i,:,:,k-1)+wl(i,:,:);
                        end
                        z(i,:,:,1)=wl(i,:,:);
                    end
                else
                    for i=1:size(z,1)
                        if strcmp(subtype,'map')
                            z(i,nact)=NaN;
                        end
                        for k=size(z,4):-1:2
                            z(i,:,:,k)=(z(i,:,:,k)+z(i,:,:,k-1))/2+wl(i,:,:);
                        end
                        z(i,:,:,1)=z(i,:,:,1)/2+wl(i,:,:);
                    end
                end
                %
                z0 = z(:,:,:,1);
                for i=2:size(z,4)
                    z1 = z(:,:,:,i);
                    z1(z1<z0) = NaN;
                    z0 = max(z0,z1);
                    z(:,:,:,i) = z1;
                end
                %
                if isequal(idx{N_},0) % unstructured data sets
                    z=-z(:,idx{M_},1,idxK_);
                else
                    z=-z(:,idx{[M_ N_]},idxK_);
                end
            end
            %      cthk=-(0:(FI.Grid.MNK(3)-1));
            %      cthk=cthk(idx{K_});
            x=reshape(x,[1 size(x)]);
            x=repmat(x,[1 1 1 length(idxK_)]);
            %      x=repmat(x,[1 1 1 length(cthk)+DataInCell]);
            y=reshape(y,[1 size(y)]);
            y=repmat(y,[1 1 1 length(idxK_)]);
            %      y=repmat(y,[1 1 1 length(cthk)+DataInCell]);
        end
end
%========================= GENERAL CODE =======================================
x(x==-999) = NaN;
%end

% grid interpolation ...
if ~strcmp(subtype,'history') && ~strcmp(Props.Geom,'TRI') && ~strcmp(Props.Geom,'POLYG')
    [x,y]=gridinterp(DataInCell | strcmp(subtype,'plot'),DimFlag(K_),Props.ReqLoc,x,y);
end

% load data ...
%================== NEFIS SPECIFIC CODE =======================================
elidx=idx(2:end);
allt=isbinary && length(idx{T_})>1;
if allt
    allt_=idx{T_};
    %  idx{T_}=0;
end
elidx(~DimFlag(2:end))=[];
mapgrid=isfield(FI,'Grid');
if mapgrid
    mapgrid=~isempty(FI.Grid);
end
TDam=0;
T=[];
if ~isempty(Props.SubFld)
    elidx(end+1)={Props.SubFld}; % last dimension automatically dropped after reading
end
if strcmp(Props.Name,'grid') & ~strcmp(Props.Geom,'POLYG')
    if isequal(FI.FileType,'DelwaqLGA')
        L=FI.Index(:,:,1);
    else
        L=FI.Grid.Index(:,:,1);
    end
    x=x(:,:,1);
    y=y(:,:,1);
    z=z(:,:,1);
    val1=[diff(L,[],1)~=0 ;zeros(1,size(L,2))];
    val1=reshape(val1,[1 size(val1)]);
    val1=val1(1,elidx{:});
    val2=[diff(L,[],2)~=0, zeros(size(L,1),1)];
    val2=reshape(val2,[1 size(val2)]);
    val2=val2(1,elidx{:});
    TDam=1;
    Props.NVal=2;
elseif (strcmp(subtype,'map') && mapgrid) || strcmp(subtype,'plot') || strcmp(subtype,'grid')
    if isempty(Props.Val1)
        %
        % Requesting segment number
        %
        val1=[];
    elseif strcmp(subtype,'grid')
        %
        % Delwaq Vol, Flux, ... file
        %
        val1=waqfil('read',FI.Attributes.(Props.Val1),idx{T_})';
        if strcmp(Props.Name,'bed level')
            val1 = -val1;
        end
    elseif iscell(Props.Val1) && DimFlag(K_)
        %
        % Plot grid or non-aggregated map grid with multiple layers stored in
        % different substances.
        %
        val1=zeros([length(idx{T_}) sz(N_) sz(M_) sz(K_)]);
        for s=idx{K_}
            [Tmp,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1{s},'quiet'); % load all
            val1(:,:,:,s)=reshape(Tmp,[size(val1,1) sz(N_) sz(M_)]);
        end
    else
        if isbinary
            [T,val1]=delwaq('read',LocFI,Props.Val1,0,idx{T_});
            val1=permute(val1,[3 2 1]);
        else
            [val1,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1,'quiet'); % load all
        end
        %
        if isfield(Props,'BedLayer')
            if Props.BedLayer == -1
                NoSegFlow=prod(sz([M_ N_ K_]));
                if size(val1,2)>NoSegFlow
                    val1=val1(:,1:NoSegFlow);
                end
            elseif Props.BedLayer == 1
                if strcmp(subtype,'plot')
                   NoSegBedLayer=sz(M_)*sz(N_);
                else
                   NoSegBedLayer=FI.Grid.NoSegPerLayer;
                end
                val1=val1(:,end-NoSegBedLayer+1:end);
            end
        end
    end
    if strcmp(subtype,'plot')
        if DimFlag(K_)
            val1=reshape(val1,[size(val1,1) sz([M_ N_ K_])]);
        else
            val1=reshape(val1,[size(val1,1) sz([M_ N_])]);
        end
        %val1=permute(val1,[1 3 2 4]);
    else
        if mapgrid
            index=FI.Grid.Index;
            if Props.BedLayer == 1
                index=index(:,:,1);
            end
        else
            index=FI.Index;
        end
        if ~DimFlag(K_) && isempty(Props.SubFld)
            index=index(:,:,1);
        end
        if isempty(val1)
            val1=reshape(index,[1 size(index)]);
        else
            val1=val1(:,max(index,1));
        end
        val1(:,index<=0)=NaN;
        val1=reshape(val1,[size(val1,1) size(index)]);
    end
    val1=val1(:,elidx{:});
    if isempty(Props.Val2)
        val2=[];
    else
        if isbinary
            [T,val2]=delwaq('read',LocFI,Props.Val2,0,idx{T_});
        else
            [val2,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val2,'quiet'); % load all
        end
        val2=val2(:,max(FI.Grid.Index,1));
        val2=reshape(val2,[size(val2,1) size(FI.Grid.Index)]);
        val2=val2(:,elidx{:});
    end
else
    st_ = ST_;
    if DimFlag(M_) && ~DimFlag(N_)
        st_ = M_;
    end
    if isbinary
        if DimFlag(K_)
            ival = Props.Val1(idx{K_});
        else
            ival = Props.Val1;
        end
        [T,val1]=delwaq('read',LocFI,ival,idx{st_},idx{T_});
    else
        if iscell(Props.Val1) && DimFlag(K_)
            %
            % Multiple layers stored in different substances.
            %
            val1=zeros([length(idx{T_}) length(idx{st_}) length(idx{K_})]);
            i_s = 0;
            for s=idx{K_}
                i_s = i_s+1;
                [val1(:,:,i_s),Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1{s},idx(st_),'quiet'); % load station
            end
        else
            [val1,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val1,idx(st_),'quiet'); % load station
        end
    end
    if isempty(Props.Val2)
        val2=[];
    else
        if isbinary
            [T,val2]=delwaq('read',LocFI,Props.Val2,idx{st_},idx{T_});
        else
            [val2,Chk]=vs_let(LocFI,Props.Group,idx(T_),Props.Val2,idx(st_),'quiet'); % load station
        end
    end
end
if allt
    idx{T_}=allt_;
end
if ~isempty(val1)
    if ~isempty(z)
        val1(isnan(z(:,:,:,2:end)))=NaN;
    end
    val1(val1==-999)=NaN;
    val1(val1==missingvalue)=NaN;
end
if ~isempty(val2)
    if ~isempty(z)
        val1(any(isnan(z),4))=NaN;
    end
    val2(val2==-999)=NaN;
    val2(val2==missingvalue)=NaN;
end

if DataInCell && isequal(Props.ReqLoc,'d')
    Props.ReqLoc='z';
end
% combine vectors components ...
if isequal(Props.VecType,'m')
    [val1,val2]=dir2uv(val1,val2);
end
% data interpolation ...
if isequal(Props.Loc,'d') && isequal(Props.ReqLoc,'z')
    val1=interp2cen(val1,'t');
    if ~isempty(val2)
        val2=interp2cen(val2,'t');
    end
elseif isequal(Props.Loc,'u') && isequal(Props.ReqLoc,'z')
    [val1,val2]=uv2cen(val1,val2);
end
% combine vectors components ...
if isequal(Props.VecType,'u') && Props.MNK<=1
    % rotate n,m components into x,y direction ...
    error('Alfa rotation not available for Delwaq files...');
end

%======================== SPECIFIC CODE =======================================
% select active points ...
if strcmp(subtype,'map') && mapgrid && ~strcmp(Props.Geom,'TRI') && ~strcmp(Props.Geom,'POLYG')
    act=FI.Grid.Index(idx{[M_ N_]},1)~=0;
    gridact=~isnan(x(:,:,:,1));
elseif strcmp(subtype,'plot') && ~isempty(z)
    act=~isnan(z(:,:,:,1));
    gridact=1;
else
    act=1;
    gridact=1;
end
%========================= GENERAL CODE =======================================
if XYRead && ~strcmp(subtype,'history')
    if DimFlag(K_)
        szx=[size(x) 1]; % extent szx for the case that dataset in K dir. is 1
        szx1=szx([1:2 4:end]);
        szx1(2)=szx(2)*szx(3);
        x=reshape(x,szx1);
        x(:,gridact~=1,:)=NaN;
        x=reshape(x,szx);
        y=reshape(y,szx1);
        y(:,gridact~=1,:)=NaN;
        y=reshape(y,szx);
        %---
        szz=[size(z) 1]; % extent szx for the case that dataset in K dir. is 1
        szz1=szz([1:2 4:end]);
        szz1(2)=szz(2)*szz(3);
        z=reshape(z,szz1);
        z(:,act~=1,:)=NaN;
        z=reshape(z,szz);
    else
        x(gridact~=1)=NaN;
        y(gridact~=1)=NaN;
    end
end
if Props.NVal>0 && ~TDam && ~strcmp(subtype,'history')
    %  if strcmp(subtype,'plot')
    %    act=permute(act,[1 3 2]);
    %  end
    szz=[size(val1) 1]; % extent szx for the case that dataset in K dir. is 1
    szz1=szz([1:2 4:end]);
    szz1(2)=szz(2)*szz(3);
    %val1=z;
    val1=reshape(val1,szz1);
    val1(:,act~=1,:)=NaN;
    val1=reshape(val1,szz);
    if ~isempty(val2)
        val2(:,isnan(val1))=NaN;
    end
end

% read time ...
if ~isbinary % for delwaqbin the time is already read when the data is read
    T=readtim(FI,Props,idx{T_});
end

% select subrange if necessary ... M,N,K only
DimMask=[0 0 1 1 1];
if DataInCell
    for i=[M_ N_ K_]
        if DimFlag(i)
            allidx(i)=0;
        end
    end
end
if DimFlag(K_) && isempty(z)
    if DataInCell
        k = idx{K_};
        k = -[k(1)-1;k(:)];
    else
        k = -idx{K_}+0.5;
    end
    z = reshape(k,[1 1 1 length(k)]);
    szx = size(x);
    z = repmat(z,[szx(1:3) 1]);
end
if ~all(allidx(DimMask & DimFlag))
    if XYRead
        if DataInCell
            if DimFlag(M_) && DimFlag(N_) && DimFlag(K_)
                z=z(:,ind{[M_ N_]},:);
            end
        else
            if DimFlag(M_) && DimFlag(N_)
                ii=ind([M_ N_]);
            elseif DimFlag(M_)
                ii={ind{M_} 1};
            else
                ii={':' ':'};
            end
            jj = {':' ii{:} ':'};
            if ~DimFlag(K_)
                jj(1) = [];
                jj(end) = [];
            end
            x=x(jj{:});
            y=y(jj{:});
            if DimFlag(K_)
                z=z(jj{:});
            end
        end
    end
    % DimMask=[0 1 1 1 1];
    ind=ind(DimMask & DimFlag);
    if Props.NVal==1
        val1=val1(:,ind{:});
    elseif Props.NVal==2
        val1=val1(:,ind{:});
        val2=val2(:,ind{:});
    end
end
%========================= GENERAL CODE =======================================

% reshape if a single station is selected ...
if DimFlag(ST_) && length(idx{ST_})==1
    sz=[size(val1) 1]; sz(2)=[];
    if isempty(val2)
        val1=reshape(val1,sz);
    else
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
    if DimFlag(K_)
        sz=[size(z) 1]; sz(2)=[];
        z=reshape(z,sz);
    end
end

% reshape if a single timestep is selected ...
if (~DimFlag(T_) || (DimFlag(T_) && isequal(size(idx{T_}),[1 1]))) && ~strcmp(Props.Geom,'TRI')
    sz=size(x); sz=[sz(2:end) 1];
    if DimFlag(K_)
        x=reshape(x,sz);
        if ~isempty(y)
            y=reshape(y,sz);
        end
        sz=size(z); sz=[sz(2:end) 1];
        z=reshape(z,sz);
    end
    sz=size(val1); sz=[sz(2:end) 1];
    if Props.NVal==1
        val1=reshape(val1,sz);
    elseif Props.NVal==2
        val1=reshape(val1,sz);
        val2=reshape(val2,sz);
    end
end

% generate output ...
if XYRead
    switch Props.Geom
        case 'POLYG'
            if size(x,2)==2 % segments
                x(:,end+1) = NaN;
                y(:,end+1) = NaN;
            else % polygons
                x(:,end+1:end+2) = NaN;
                y(:,end+1:end+2) = NaN;
                % close all polygons
                for j=1:size(x,1)
                    for k=1:size(x,2)
                        if isnan(x(j,k))
                            x(j,k) = x(j,1);
                            y(j,k) = y(j,1);
                            break
                        end
                    end
                end
            end
            val1 = repmat(val1,1,size(x,2));
            val1 = val1';
            val1 = val1(:);
            x = x';
            Ans.X = x(:);
            y = y';
            Ans.Y = y(:);
        case 'TRI'
            Ans.TRI=FI.Grid.Discr.Elem;
            if size(z,1)>1
                x=repmat(x,[size(z,1) 1 1 1]);
                y=repmat(y,[size(z,1) 1 1 1]);
            end
            Ans.XYZ=cat(5,x,y,z);
            szXYZ=size(Ans.XYZ);
            Ans.XYZ=reshape(Ans.XYZ,szXYZ([1 2 4 5]));
        otherwise
            if ~isempty(x)
                Ans.X=x;
            end
            if ~isempty(y)
                Ans.Y=y;
            end
            if DimFlag(K_)
                Ans.Z=z;
                Ans.ZUnits=ZUnits;
            end
    end
    %
    if ~isempty(XUnits)
        Ans.XUnits = XUnits;
        Ans.YUnits = XUnits;
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    if TDam
        Ans.XDam=val1;
        Ans.YDam=val2;
    else
        Ans.XComp=val1;
        Ans.YComp=val2;
    end
end

% read time ...
Ans.Time=T;

% create updated data info
if isfield(FI,'DwqBin')
    FI.DwqBin=LocFI;
elseif isfield(FI,'Nfs')
    FI.Nfs=LocFI;
else
    FI=LocFI;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
PropNames={'Name'                   'Units' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc'  'Loc3D' 'Group'          'Val1'     'Val2'    'SubFld' 'MNK' 'ClosedPoly'};
if isbinary
    Type=lower(LocFI.FileType);
else
    Type=lower(LocFI.SubType);
end
plotfile=0;
includegrid=1;
enablegridview=1;
switch Type
    case {'delwaqlga'}
        DataProps={'grid'          ''     '' 'xy'     [0 0 1 1 0]  0         0     ''       'd'   'd'       ''      ''               ''              ''    []       0 0
            'segment number'       ''     '' 'xy'     [0 0 1 1 0]  1         1     ''       'z'   'z'       'c'     ''               ''              ''    []       0 0
            '-------'              ''     '' ''       [0 0 0 0 0]  0         0     ''       ''    ''        ''      ''               ''              ''    []       0 0};
        Out=cell2struct(DataProps,PropNames,2);
        if FI.MNK(3)>1
            Out(2).DimFlag(K_) = 1;
            Out(2).Coords(end+(1:2))='+z';
        end
        if isfield(FI,'Attributes') && isstruct(FI.Attributes)
            flds = fieldnames(FI.Attributes);
            for i = 1:length(flds)
                Out(end+1) = Out(2);
                Out(end).Val1 = flds{i};
                name = flds{i};
                units = '';
                switch name
                    case 'vol'
                        Out(end).DimFlag(T_) = 5;
                        name = 'flow volumes';
                        units = 'm^3';
                    case 'sal'
                        Out(end).DimFlag(T_) = 5;
                        name = 'salinity';
                        units = 'ppt';
                    case 'tem'
                        Out(end).DimFlag(T_) = 5;
                        name = 'temperature';
                        units = '°C';
                    case 'vdf'
                        Out(end).DimFlag(T_) = 5;
                        name = 'vertical diffusivity';
                        units = 'm^2/s';
                    case 'tau'
                        Out(end).DimFlag(T_) = 5;
                        name = 'bed shear stress';
                        units = 'N/m^2';
                    case 'are'
                        Out(end).DimFlag(T_) = 5;
                        name = 'flow areas';
                        units = 'm^2';
                    case 'flo'
                        Out(end).DimFlag(T_) = 5;
                        name = 'fluxes';
                        units = 'm^3/s';
                    case 'poi'
                        name = 'segment indices';
                    case 'len'
                        name = 'lengths';
                        units = 'm';
                    case 'chz'
                        name = 'chezy';
                        units = 'm^{1/2}/s';
                    case 'srf'
                        name = 'surface areas';
                        units = 'm^2';
                        Out(end).DimFlag(K_) = 0;
                    case 'dps'
                        name = 'bed level';
                        units = 'm';
                        Out(end).DimFlag(K_) = 0;
                end
                Out(end).Name = name;
                Out(end).Units = units;
            end
        end
        [Out.UseGrid]=deal(1);
        return
    case {'delft3d-waq-history','delwaqhis','delft3d-par-his','delft3d-par-history'}
        DataProps={'--constituents'       ''      '' ''    [1 5 0 0 0]  0         1     ''       'z'   'z'       'c'     feval(casemod,'DELWAQ_RESULTS') feval(casemod,'SUBST_001')     ''    []       0 0};
        if isfield(FI,'treatas1d') && FI.treatas1d
            DataProps{5}=[1 0 1 0 0];
        end
    case {'delft3d-par-detailed_plot','delparplot','delft3d-par-plot','delft3d-par-psf'}
        plotfile=1;
        DataProps={'--constituents'       ''      '' 'xy'    [1 0 1 1 0]  1         1     ''       'z'   'z'       'c'     feval(casemod,'DELPAR_RESULTS') feval(casemod,'SUBST_001')     ''    []       0 0};
    otherwise
        DataProps={'--constituents'       ''      '' 'xy'    [1 5 0 0 0]  1         1     ''       'z'   'z'       'c'     feval(casemod,'DELWAQ_RESULTS') feval(casemod,'SUBST_001')     ''    []       0 0};
        if isfield(FI,'Grid') && ~isempty(FI.Grid)
            DataProps{5}=[1 0 1 1 0];
            if FI.Grid.MNK(3)>1
                DataProps{4}(end+(1:2))='+z';
                DataProps{5}(K_)=1;
            end
            if isfield(FI.Grid,'FileType') 
                switch FI.Grid.FileType
                    case {'Serafin'} % triangular
                        includegrid=0;
                        enablegridview=0;
                        DataProps{3}='TRI';
                        DataProps{4}='xy';
                        DataProps{5}(M_)=6;
                        DataProps{5}(N_)=0;
                        DataProps{6}=0;
                    case {'netCDF'} % polygon bounds
                        enablegridview=0;
                        DataProps{3}='POLYG';
                        DataProps{4}='xy';
                        DataProps{5}(M_)=6;
                        DataProps{5}(N_)=0;
                        DataProps{6}=1;
                        DataProps{end}=1; %closedpoly
                end
            end
        end
end
Out=cell2struct(DataProps,PropNames,2);
Out.Group=feval(casemod,[DELWAQ '_RESULTS']);
%======================== SPECIFIC CODE REMOVE ================================
%for i=size(Out,1):-1:1
%  Info=vs_disp(LocFI,Out(i).Group,Out(i).Val1);
%  if ~isstruct(Info)
%    % remove references to non-stored data fields
%    Out(i,:)=[];
%  elseif isequal(Info.SizeDim,1)
%    % remove references to non-stored data fields
%    Out(i,:)=[];
%  elseif strcmp(Out(i).Name,'epsilon'),
%    % turbulent kinetic energy automatically caught by previous case
%    % epsilon only present as second field in RTUR1: RTUR1(:,:,:,2)
%    if ~isequal(Info.SizeDim(4),2)
%      Out(i,:)=[];
%    end
%  end
%end
%======================== SPECIFIC CODE ADD ===================================
i=strmatch('--constituents',{Out.Name});
ii=0;
if ~isfield(FI,'balancefile')
    FI.balancefile=0;
end
if ~isempty(i)
    if isbinary
        names=LocFI.SubsName;
    else
        [names,Chk]=vs_get(LocFI,feval(casemod,[DELWAQ '_PARAMS']),feval(casemod,'SUBST_NAMES'),'quiet');
    end
    if FI.balancefile
        if iscell(names)
            names=strvcat(names{:});
        end
        if size(names,2)>7 && sum(names(:,7)=='_')>size(names,1)/2
            nn1 = names(:,1:6);
            nn2 = cellstr(names(:,8:end));
        elseif size(names,2)>11
            nn1 = names(:,1:10);
            nn2 = cellstr(names(:,11:end));
        else
            nn1 = names;
            nn2 = names(:,[]);
        end
        [names,dummy,index]=unique(nn1,'rows');
        names=cellstr(names);
        Ins=Out(i*ones(length(names),1));
        for j=1:length(names)
            sub = find(index==j);
            Ins(j).Name=names{j};
            Ins(j).Val1=sub(1);
            if length(sub)==1
                % normal history
            else
                % actual balance
                %Ins(j).SubFld={sub nn2(sub)};
                Ins(j).BalSubFld={sub nn2(sub)};
                Ins(j).NVal=-1;
                Ins(j).DimFlag(1)=0;
            end
        end
    else
        names=cellstr(names);
        Ins=Out(i*ones(length(names),1));
        for j=1:length(names)
            Ins(j).Name=names{j};
            Ins(j).Val1=j;
        end
    end
    j=0;
    KnownMultilayer=0;
    if isfield(LocFI,'K')
        KnownMultilayer=LocFI.K>1;
    elseif ~isbinary
        InfoPLOT=vs_disp(LocFI,feval(casemod,'PLO-VERSION'),[]);
        InfoPSF=vs_disp(LocFI,feval(casemod,'PSF-VERSION'),[]);
        if isstruct(InfoPLOT) || isstruct(InfoPSF)
            [PloDimensions,Chk]=vs_get(LocFI,feval(casemod,'DELPAR_PARAMS'),feval(casemod,'SIZES'),'quiet');
            KnownMultilayer=PloDimensions(4)>1;
        end
    end
    while j<length(Ins)
        j=j+1;
        if strcmp(Ins(j).Name,'*already processed*')
            % skip it
        elseif plotfile && KnownMultilayer
            Ins(j).DimFlag=[1 0 1 1 1];
            if isbinary
                Ins(j).Val1=j+ii;
            else
                Ins(j).Val1=feval(casemod,sprintf('SUBST_%3.3i',j+ii));
            end
        else%if plotfile
            if length(Ins(j).Name)>17 && strcmp(Ins(j).Name(15:17),'lay')
                % j will be 1
                nm=Ins(j).Name(1:14);
                nmidx=1:14;
                lay=Ins(j).Name(18:20);
                nsubplo=1;
                while j+nsubplo<=length(Ins) && length(Ins(j+nsubplo).Name)>=20 && strcmp(Ins(j+nsubplo).Name(18:20),lay)
                    nsubplo=nsubplo+1;
                end
                nlayplo=1;
                while j+nsubplo<=length(Ins) && strcmp(Ins(j+nsubplo).Name(nmidx),nm(nmidx))
                    j=j+nsubplo;
                    nlayplo=nlayplo+1;
                end
                j=j-nsubplo*(nlayplo-1);
                % j will again be 1
                for jj=0:nsubplo-1
                    Ins(j+jj).Name=deblank(Ins(j+jj).Name(1:14));
                    %Ins(j+jj).DimFlag(5)=1;
                    Ins(j+jj).DimFlag=[1 0 1 1 1];
                    if isbinary
                        Ins(j+jj).Val1=j+ii+jj+(0:nlayplo-1)*nsubplo;
                    else
                        Ins(j+jj).Val1={};
                        for s=0:nlayplo-1,
                            Ins(j+jj).Val1{s+1}=feval(casemod,sprintf('SUBST_%3.3i',j+ii+jj+s*nsubplo));
                        end
                    end
                end
                rmidx=j-1+nsubplo+(1:(nsubplo*(nlayplo-1)));
                if any(rmidx>length(Ins))
                    nmiss=sum(rmidx>length(Ins)); % nmiss <= nsubplo-1
                    for jj=nsubplo-nmiss:nsubplo-1
                        Ins(j+jj).Val1(end)=[];
                    end
                    rmidx(rmidx>length(Ins))=[];
                end
                Ins(rmidx)=[];
                j=j+nsubplo-1;
                ii=ii+length(rmidx);
            elseif length(Ins(j).Name)>8 && strcmp(Ins(j).Name(6:8),'_of')
                % j will be 1
                if length(Ins(j).Name)>10
                    nm=[Ins(j).Name(1:5) '[...]' Ins(j).Name(11:end)];
                    nmidx=[1:5 11:length(Ins(j).Name)];
                else
                    nm=Ins(j).Name(1:5);
                    nmidx=1:5;
                end
                lay=Ins(j).Name(9:10);
                nsubplo=1;
                while j+nsubplo<=length(Ins) && length(Ins(j+nsubplo).Name)>=10 && strcmp(Ins(j+nsubplo).Name(9:10),lay)
                    nsubplo=nsubplo+1;
                end
                nlayplo=1;
                while j+nsubplo<=length(Ins) && strcmp(Ins(j+nsubplo).Name(nmidx),nm(nmidx))
                    j=j+nsubplo;
                    nlayplo=nlayplo+1;
                end
                j=j-nsubplo*(nlayplo-1);
                % j will again be 1
                for jj=0:nsubplo-1
                    if length(Ins(j+jj).Name)>10
                        nm=[Ins(j).Name(1:5) '[...]' Ins(j).Name(11:end)];
                    else
                        nm=Ins(j).Name(1:5);
                    end
                    Ins(j+jj).Name=deblank(nm);
                    %
                    %DF = Ins(j+jj).DimFlag;
                    %DF(K_) = 1;
                    %Ins(j+jj).DimFlag=DF;
                    Ins(j+jj).DimFlag(K_)=1;
                    %
                    if isbinary
                        Ins(j+jj).Val1=j+ii+jj+(0:nlayplo-1)*nsubplo;
                    else
                        Ins(j+jj).Val1={};
                        for s=0:nlayplo-1,
                            Ins(j+jj).Val1{s+1}=feval(casemod,sprintf('SUBST_%3.3i',j+ii+jj+s*nsubplo));
                        end
                    end
                end
                rmidx=j-1+nsubplo+(1:(nsubplo*(nlayplo-1)));
                if any(rmidx>length(Ins))
                    nmiss=sum(rmidx>length(Ins)); % nmiss <= nsubplo-1
                    for jj=nsubplo-nmiss:nsubplo-1
                        Ins(j+jj).Val1(end)=[];
                    end
                    rmidx(rmidx>length(Ins))=[];
                end
                Ins(rmidx)=[];
                j=j+nsubplo-1;
                ii=ii+length(rmidx);
            elseif plotfile
                Ins(j).DimFlag(K_)=0;
                if isbinary
                    Ins(j).Val1=j+ii;
                else
                    Ins(j).Val1=feval(casemod,sprintf('SUBST_%3.3i',j+ii));
                end
            elseif strcmp(subtype,'history') && length(Ins(j).Name)>3 && strcmp(Ins(j).Name(end-2:end),'_01')
                idx = j;
                k = 2;
                for jj = j+1:length(Ins)
                    CheckName = sprintf('%s_%2.2i',Ins(j).Name(1:end-3),k);
                    if strcmp(Ins(jj).Name,CheckName)
                        idx = [idx jj];
                        k = k+1;
                    end
                end
                k = k-1;
                if isbinary
                    Ins(j).Val1=idx;
                else
                    Ins(j).Val1={};
                    for s=1:length(idx)
                        Ins(j).Val1{s}=feval(casemod,sprintf('SUBST_%3.3i',idx(s)));
                    end
                end
                Ins(j).DimFlag = [1 5 0 0 1];
                for jj=idx(2:end)
                    Ins(jj).Name='*already processed*';
                end
                Ins(j).Name = Ins(j).Name(1:end-3);
            elseif isbinary
                j0 = Ins(j).Val1;
                Ins(j).Val1=j0+ii;
            else
                Ins(j).Val1=feval(casemod,sprintf('SUBST_%3.3i',j+ii));
            end
        end
    end
    Ins(strmatch('*already processed*',{Ins.Name}))=[];
    %
    [Ins(:).BedLayer]=deal(0);
    if strcmp(subtype,'map')
        bedlayer = 0;
        if isfield(FI.Grid,'NoSegPerLayer')
           noseg_ifbedlayer = FI.Grid.NoSegPerLayer*(FI.Grid.MNK(3)+1);
        else % e.g. in case of Telemac
           noseg_ifbedlayer = prod(FI.Grid.MNK+[0 0 1]);
        end
        if isbinary
            bedlayer = FI.DwqBin.NumSegm==noseg_ifbedlayer;
        else
            Info = vs_disp(FI.Nfs,feval(casemod,[DELWAQ '_RESULTS']),feval(casemod,'SUBST_001'));
            bedlayer = Info.SizeDim==noseg_ifbedlayer;
        end
        if bedlayer
            nVal=length(Ins);
            Ins=cat(1,Ins,Ins);
            for j=2*nVal:-1:nVal+1
                if strcmpi(Ins(j).Name,'LocalDepth')
                    Ins(j,:)=[];
                else
                    Ins(j).BedLayer=1;
                    Ins(j).DimFlag(K_)=0;
                end
            end
        end
    elseif strcmp(subtype,'plot')
        iLD=strcmpi('LocalDepth',names);
        if any(iLD) && Ins(iLD).DimFlag(K_)
            iLD = find(iLD);
            %
            if isbinary
                if isfield(FI,'K')
                    KMAX = FI.K;
                else
                    KMAX = length(Props.Val1);
                end
                %
                NoSegPerLayer = prod(FI.GridSize);
                LastLayer = NoSegPerLayer*(KMAX-1)+(1:NoSegPerLayer);
                [T,val1]=delwaq('read',LocFI,iLD,0,1);
            else
                NoSegPerLayer = prod(PloDimensions(5:6));
                LastLayer = NoSegPerLayer*(PloDimensions(4)-1)+(1:NoSegPerLayer);
                [val1,Chk]=vs_let(LocFI,feval(casemod,Ins(iLD).Group),{1},feval(casemod,Ins(iLD).Val1),'quiet'); % load all
            end
            SecLastLayer = LastLayer - NoSegPerLayer;
            %
            missingvalue = double(single(999.999));
            %
            vLast = val1(LastLayer);
            vLast(vLast == missingvalue) = 0;
            vLast2 = val1(SecLastLayer);
            vLast2(vLast2 == missingvalue) = 0;
            if all(vLast==0) && any(vLast2~=0)
                nVal=length(Ins);
                [Ins(:).BedLayer]=deal(-1);
                Ins=cat(1,Ins,Ins);
                for j=2*nVal:-1:nVal+1
                    if strcmpi(Ins(j).Name,'LocalDepth')
                        Ins(j,:)=[];
                    else
                        Ins(j).BedLayer=1;
                        Ins(j).DimFlag(K_)=0;
                    end
                end
            end
        end
    end
    %
    % check whether substance names were expanded by adding 001,002,...
    % the following algorithm requires fractions to be numbered
    % consecutively. For this puerpose we sort the entries first.
    %
    [sorted_names,reorder] = sort({Ins.Name}');
    Ins = Ins(reorder);
    %
    names=substdb;
    onenames = names(wildstrmatch('*01',names),:);
    j=1;
    while j<=length(Ins)
       nm = Ins(j).Name;
       if length(nm)<3 % name to too short for name expansion
          j=j+1;
          continue
       end
       if strcmp(nm(end-1:end),'01') % do the last two characters match '01'? [two digits if there are less than 100 fractions)
          n=2;
          f='%s%2.2d';
          if strcmp(nm(end-2:end),'001') % do the last three characters match '001'? (three digits if there are more than 99 fractions)
             n=3;
             f='%s%3.3d';
          end
       else % the last two characters are not '01'
          j=j+1;
          continue
       end
       if ~isempty(strmatch(lower(nm),onenames)) % there exists a name in the process definition file that matches the full name (no name expansion)
          j=j+1;
          continue
       end
       for k=j+1:length(Ins) % count expansion
          nm2 = sprintf(f,nm(1:end-n),k-j+1);
          if ~strcmp(Ins(k).Name,nm2)
             k=k-1;
             break
          end
       end
       Ins(j).Name = nm(1:end-n);
       nms = {};
       for i=k-j+1:-1:1
          nms{i} = sprintf('fraction %i',i);
       end
       Ins(j).SubFld = {[Ins(j:k).Val1] nms};
       Ins(j+1:k)=[];
       j=j+1;
    end
    for j=1:length(Ins)
       s1 = strfind(Ins(j).Name,'S1');
       s2 = strfind(Ins(j).Name,'S2');
       s = max([s1 s2]);
       if ~isempty(s)
          smatch = strmatch(Ins(j).Name(s+2:end),{'','M2','TOT','-DIS','-PAR','OMP'},'exact');
          if ~isempty(smatch)
             if Ins(j).DimFlag(5)
                Ins(j).DimFlag(5)=0;
                Ins(j).SubFld=FI.Grid.MNK(3);
             end
          end
       end
    end
    for j=1:length(Ins)
        Ins(j).ShortName = Ins(j).Name;
        [Ins(j).Name,Ins(j).Units,Ins(j).SubsGrp]=substdb(Ins(j).Name);
        if Ins(j).BedLayer>0
            Ins(j).SubsGrp=[Ins(j).SubsGrp '-bedlayer'];
            Ins(j).Name=[Ins(j).Name ' (bed layer)'];
            Ins(j).Units='kg/m^2';
        end
        if FI.balancefile
            %
            % balance file
            %
            Ins(j).Units = [Ins(j).Units '/d'];
            if Ins(j).NVal<0
                for k=1:length(Ins(j).BalSubFld{2})
                    Ins(j).BalSubFld{2}{k}=substdb(Ins(j).BalSubFld{2}{k});
                end
            end
        end
        if isequal(Ins(j).Name,'Limit Chlo') || isequal(Ins(j).Name,'total chlorophyll in algae')
            Ins(end+1)=Ins(j);
            Ins(end).Name = 'total chlorophyll in algae (limiting factors)';
            Ins(end).NVal = -1;
            Ins(end).DimFlag(1)=0;
        end
    end
    [dummy,reorder]=sort({Ins.SubsGrp});
    Ins=Ins(reorder);
    [subsgrp,I,J]=unique({Ins.SubsGrp});
    for i=length(subsgrp):-1:1
        j=find(J==i);
        [dummy,reorder]=sort({Ins(j).Name});
        Ins(j+i-1)=Ins(j(reorder));
        if i>1
            j0=j(1)+i-2;
            Ins(j0).Name='-------';
            Ins(j0).Units='';
            Ins(j0).DimFlag=[0 0 0 0 0];
            Ins(j0).Group='';
            Ins(j0).Val1='';
            Ins(j0).ShortName='';
            Ins(j0).SubsGrp='';
        end
    end
    %end
    Out=Ins;
end
if isfield(FI,'Grid') && ~isempty(FI.Grid) && includegrid
    Out=Out([1 1 1 1:length(Out)]);
    Out(1).Name='grid';
    Out(1).Units='';
    Out(1).DimFlag=Out(4).DimFlag;
    Out(1).DimFlag(T_)=0;
    Out(1).DimFlag(K_)=0;
    if strcmp(FI.Grid.FileType,'netCDF')
       Out(1).DataInCell=2;
    else
       Out(1).DataInCell=0;
    end
    Out(1).NVal=0;
    Out(1).SubFld=[];
    Out(1).Loc='d';
    Out(1).ReqLoc='d';
    Out(2).Name='segment number';
    Out(2).Units='';
    %
    % if the first field is a quantity in a sediment layer, then the
    % 3D segment information might get lost. Let's verify whether any of
    % the quantities has a 3rd dimension.
    %
    hask=0;
    for i=3:length(Out)
       hask = hask | Out(i).DimFlag(K_);
    end
    %
    Out(2).DimFlag(K_)=hask; %1
    %Out(2).DimFlag(T_)=0; % switch off time, but keep it 3D
    Out(2).DataInCell=1;
    Out(2).NVal=1;
    Out(2).SubFld=[];
    Out(2).Loc='z';
    Out(2).ReqLoc='z';
    %Out(2).Group='';
    Out(2).Val1='';
    Out(3).Name='-------';
    Out(3).Units='';
    Out(3).DimFlag=[0 0 0 0 0];
    Out(3).Group='';
    Out(3).Val1='';
    Out(3).ShortName='';
    Out(3).SubsGrp='';
end

if isPartFile(FI)
    lasti = 0;
    for i = 3:3:length(Out)
        Substance = Out(i-2).Name;
        if length(Out(i).Name)>5 && strcmp(Out(i).Name(end-4:end),'stick') && ...
                length(Out(i-1).Name)>4 && strcmp(Out(i-1).Name(end-3:end),'disp') && ...
                strcmp(Substance,deblank(Out(i).Name(1:end-5))) && ...
                strcmp(Substance,deblank(Out(i-1).Name(1:end-4)))
            Out(i-2).Name = [Substance ' (floating)'];
            Out(i-2).Units = 'kg/m^2';
            Out(i-1).Name = [Substance ' (dispersed)'];
            Out(i-1).Units = 'kg/m^3';
            Out(i).Name = [Substance ' (sticking)'];
            Out(i).Units = 'kg/m^2';
            lasti = i;
        end
    end
    for i = lasti+1:length(Out)
        if isempty(Out(i).Units)
            switch Out(i).Name
                case {'grid','segment number','-------'}
                otherwise
                    Out(i).Units = 'kg/m^3';
            end
        end
    end
end

% enable GridView
if (strcmp(subtype,'map') || strcmp(subtype,'plot')) && isfield(FI,'Grid') && ~isempty(FI.Grid) && enablegridview
   [Out.UseGrid] = deal(1);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
if ~iscell(Props.SubFld)
    subf={};
else
    subf=Props.SubFld{2};
end
if nargin>2 && f~=0
    subf=subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
if isequal(LocFI.FileType,'DelwaqLGA')
    if Props.DimFlag(T_)
        sz(T_) = FI.Attributes.(Props.Val1).NTimes;
    end
    sz([M_ N_])=FI.MNK(1:2);
    if Props.DimFlag(K_)
        sz(K_) = FI.MNK(3);
    end
    return
end
switch subtype
    case 'map'
        %if Props.DimFlag(M_) && Props.DimFlag(N_)
        sz([M_ N_])=FI.Grid.MNK(1:2);
        %end
        if Props.DimFlag(K_)
            sz(K_)=FI.Grid.MNK(3);
        end
    case 'plot'
        if isbinary
            sz([M_ N_])=FI.GridSize;
        else
            [X,Chk]=vs_get(LocFI,feval(casemod,'DELPAR_PARAMS'),feval(casemod,'SIZES'),'quiet');
            sz([M_ N_])=X([5 6]);
        end
        if Props.DimFlag(K_)
            if isbinary
                if isfield(LocFI,'K')
                    sz(K_)=LocFI.K;
                else
                    sz(K_)=length(Props.Val1);
                end
            else
                sz(K_)=X(4);
            end
        end
    otherwise
        if Props.DimFlag(ST_) || (Props.DimFlag(M_) && ~Props.DimFlag(N_))
            m_ = M_;
            if Props.DimFlag(ST_)
                m_ = ST_;
            end
            if isbinary
                sz(m_)=LocFI.NumSegm;
                if Props.DimFlag(K_)
                    sz(K_)=length(Props.Val1);
                end
            else
                if iscell(Props.Val1)
                    sz(K_)=length(Props.Val1);
                    Info=vs_disp(LocFI,Props.Group,Props.Val1{1});
                else
                    Info=vs_disp(LocFI,Props.Group,Props.Val1);
                end
                sz(m_)=Info.SizeDim;
            end
        end
end
if isfield(Props,'BedLayer') && Props.BedLayer<0
    sz(K_)=sz(K_)-1;
end
if Props.DimFlag(T_)
    if isbinary
        sz(T_)=LocFI.NTimes;
    else
        Info=vs_disp(LocFI,Props.Group,[]);
        sz(T_)=Info.SizeDim;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
if strcmp(subtype,'grid')
    T=FI.Attributes.(Props.Val1).Times;
    if t~=0
        T=T(t);
    end
elseif isbinary
    T=delwaq('read',LocFI,1,1,t);
else
    if strcmp(subtype,'plot')
        [X,Chk]=vs_get(LocFI,feval(casemod,[DELWAQ '_PARAMS']),feval(casemod,'TIME_OFFSET'),'quiet');
        T0=datenum(X(1),X(2),X(3)/3600/24);
        TStep=1/(24*3600);
    else
        [Str,Chk]=vs_get(LocFI,feval(casemod,[DELWAQ '_PARAMS']),feval(casemod,'TITLE'),'quiet');
        [T0,TStep] = delwaqt0(Str(4,:));
    end
    if isequal(Props.Group,feval(casemod,[DELWAQ '_RESULTS']))
        [T,Chk]=vs_let(LocFI,feval(casemod,[DELWAQ '_RESULTS']),{t},feval(casemod,'TIME'),'quiet');
        T=T0+T*TStep;
    else % ONE FIELD
        T=T0;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
[LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI);
if isbinary
    S=LocFI.SegmentName;
else
    if ~isempty(strmatch(LocFI.SubType,{'Delft3D-waq-map','Delft3D-par-map'}))
        S='';
        return
    end
    if isfield(LocFI,'SegmentName')
        S=LocFI.SegmentName;
    else
        [S,Chk]=vs_get(LocFI,feval(casemod,[DELWAQ '_PARAMS']),feval(casemod,'LOCATION_NAMES'),'quiet');
        S=cellstr(S);
    end
end
if ~isequal(t,0)
    S=S(t);
end
% -----------------------------------------------------------------------------


function [Full,Unit,GroupID]=substdb(Abb,cmd)
% substance database
persistent x
if nargin==2
    switch cmd
        case 'filename'
            if isempty(x)
                substdb;
            end
            if isfield(x,'ProcDefFile')
                Full = x.ProcDefFile;
            else
                Full = '[unable to locate]';
            end
        case 'reload'
            x=[];
            substdb;
    end
    return
end
%
% nargin == 0 or 1
%
if isempty(x)
    ErrMsg='';
    x=-1;
    try
        ErrMsg='Cannot find or open ';
        tbl=qp_settings('delwaq_procdef');
        if isequal(tbl,'auto') || ~exist(tbl,'file')
            tbl=cat(2,getenv('D3D_HOME'),filesep,getenv('ARCH'),filesep,'waq',filesep,'default',filesep,'proc_def.dat');
        end
        if ~exist(tbl,'file') && ispc
            for drive = 'cdef'
                subdir = dir([drive ':\S*']);
                for j = 1:length(subdir)
                    if subdir(j).isdir
                        tbl = [drive ':\' subdir(j).name '\programs\delwaq\fixed\proc_def.dat'];
                        if exist(tbl,'file')
                            break
                        end
                    end
                end
                if exist(tbl,'file')
                    break
                end
            end
        end
        if ~exist(tbl,'file')
            tbl='proc_def.dat';
        end
        TBL=vs_use(tbl,'quiet');
        [ID,Chk]=vs_get(TBL,'TABLE_P2','ITEM_ID','quiet');
        [NM,Chk]=vs_get(TBL,'TABLE_P2','ITEM_NM','quiet');
        [GRPID,Chk]=vs_get(TBL,'TABLE_P2','GROUPID','quiet');
        [UNIT,Chk]=vs_get(TBL,'TABLE_P2','UNIT','quiet');
        if Chk
            x=[];
            x.ID=lower(ID);
            x.NM=NM;
            x.GRPID=GRPID;
            x.UNIT=UNIT;
            x.ProcDefFile=tbl;
        end
    catch
        ui_message('error',[ErrMsg tbl]);
        x=-1;
    end
 end
 %
 Unit='';
 GroupID='';
 if nargin==0
    if isstruct(x)
       Full=x.ID;
    else
       Full='';
    end
 else
    if isstruct(x)
       db=strmatch(lower(Abb),x.ID,'exact');
       if isequal(size(db),[1 1])
          Full=deblank(x.NM(db,:));
          if isequal(lower(Full),'undefined')
             Full=Abb;
          end
          Unit=deblank(x.UNIT(db,:));
          Unit=Unit(2:end-1);
          GroupID=deblank(x.GRPID(db,:));
       else
          Full=Abb;
       end
    else
       Full=Abb;
    end
 end
 
% -----------------------------------------------------------------------------
function [LocFI,isbinary,subtype,DELWAQ,casemod]=KeyParamFI(FI)
%==============================================================================
isbinary=isfield(FI,'ByteOrder') | isfield(FI,'DwqBin');
subtype='history';
casemod=@nochange;
if strcmp('DelwaqLGA',FI.FileType)
    LocFI=FI;
    subtype='grid';
    DELWAQ='';
elseif strcmp('DelparPLOT',FI.FileType)
    if isfield(FI,'DwqBin')
        LocFI=FI.DwqBin;
    else
        LocFI=FI;
    end
    isbinary=1;
    subtype='plot';
    DELWAQ='DELPAR';
elseif isbinary
    if isfield(FI,'DwqBin')
        LocFI=FI.DwqBin;
    else
        LocFI=FI;
    end
    if ~isempty(strmatch(lower(FI.FileType),{'delwaqmap','delparmap'}))
        subtype='map';
    end
    DELWAQ='';
else
    %Extract NEFIS data
    if isfield(FI,'Nfs')
        LocFI=FI.Nfs;
    else
        LocFI=FI;
    end
    if strcmp(lower(LocFI.SubType(end-2:end)),'map')
        subtype='map';
    elseif strcmp(lower(LocFI.SubType(end-3:end)),'plot') || strcmp(lower(LocFI.SubType(end-2:end)),'psf')
        subtype='plot';
    end
    DELWAQ=['DEL' upper(LocFI.SubType(9:11))];
    if ~isstruct(vs_disp(LocFI,[DELWAQ '_PARAMS'],[]))
        casemod=@lower;
    end
end


% -----------------------------------------------------------------------------
function bool=isPartFile(FI)
%==============================================================================
[LocFI,isbinary,subtype,DELWAQ]=KeyParamFI(FI);
if strcmp(DELWAQ,'DELPAR')
    bool = 1;
elseif strcmp(DELWAQ,'DELWAQ')
    bool = 0;
elseif strcmp(subtype,'grid')
    bool = 0;
elseif isfield(FI,'balancefile') && FI.balancefile
    bool = 0;
else
    bool = 0; % unknown
    if isfield(FI,'DwqBin')
        Subs = FI.DwqBin.SubsName;
    else
        Subs = FI.SubsName;
    end
    for i = 1:length(Subs)
        sub = Subs{i};
        if length(sub)>5 && strcmp(sub(end-4:end),'stick')
            bool = 1;
        end
    end
end

% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd
    case 'initialize'
        OK=optfig(mfig);
        switch lower(FI.FileType)
            case 'delwaqlga'
                cellflds = {'loadvolflux'};
            otherwise
                cellflds = {'treatas1d','balancefile'};
        end
        for cellfld = cellflds
            Fld = cellfld{1};
            f = findobj(mfig,'tag',Fld);
            if isfield(FI,Fld)
                value = getfield(FI,Fld);
            else
                value = 0;
            end
            set(f,'value',value,'enable','on')
            %
            switch Fld
                case 'balancefile'
                    Fld = 'nettransport';
                    f = findobj(mfig,'tag',Fld);
                    if value
                        if isfield(FI,Fld)
                            value = getfield(FI,Fld);
                        else
                            value = 0;
                        end
                        set(f,'value',value,'enable','on')
                    else
                        set(f,'value',0,'enable','off')
                    end
                case 'loadvolflux'
                    Fld = 'loadvolflux';
                    f = findobj(mfig,'tag',Fld);
                    if isfield(FI,'Attributes') && isstruct(FI.Attributes)
                        set(f,'string','Reload Volumes, Fluxes, ...')
                    end
            end
        end
        options(FI,mfig,'updateprocdef');
        
    case {'updateprocdef'}
        f1 = findobj(mfig,'tag','autoprocdef');
        f2 = findobj(mfig,'tag','procdefname');
        f3 = findobj(mfig,'tag','procdefbrowse');
        if strcmp(qp_settings('delwaq_procdef'),'auto')
            set(f1,'value',1,'enable','on')
            set(f2,'string',substdb('cmd','filename'),'enable','inactive')
            set(f2,'backgroundcolor',[1 1 0],'backgroundcolor',Inactive) % need dummy backgroundcolor to get proper color set (R2012a)
            set(f3,'enable','off')
        else
            set(f1,'value',0,'enable','on')
            set(f2,'string',substdb('cmd','filename'),'enable','on','backgroundcolor',Active)
            set(f3,'enable','on')
        end
        
    case {'autoprocdef'}
        f1 = findobj(mfig,'tag','autoprocdef');
        if get(f1,'value')
            % now auto
            qp_settings('delwaq_procdef','auto')
            substdb('cmd','reload')
        else
            % now manual
            qp_settings('delwaq_procdef','manual but not yet specied')
        end
        options(FI,mfig,'updateprocdef');

    case {'procdefname'}
        f1 = findobj(mfig,'tag','procdefname');
        qp_settings('delwaq_procdef',get(f1,'string'))
        substdb('cmd','reload')
        options(FI,mfig,'updateprocdef');
        
    case {'procdefbrowse'}
        procdef = qp_settings('delwaq_procdef');
        if isequal(procdef,'auto')
            procdef = 'proc_def.dat';
        end
        [fn,pn]=uigetfile({'proc_def.dat','Process definition files'},'Select process definition file',procdef);
        if ischar(fn)
            qp_settings('delwaq_procdef',[pn fn])
            substdb('cmd','reload')
        end
        options(FI,mfig,'updateprocdef');
        
    case {'loadvolflux'}
        NewFI.Attributes = [];
        base = FI.FileBase;
        H = progressbar(0,'title','Scanning for files ...');
        attribs = dir([base '*']);
        nattribs = length(attribs);
        bytesdone = 0;
        bytestotal = sum([attribs.bytes]);
        for i = 1:nattribs
            [p,n,extp] = fileparts(attribs(i).name);
            progressbar(bytesdone/bytestotal,H,'title',['Reading ' attribs(i).name]);
            bytesdone = bytesdone + attribs(i).bytes;
            ext = extp(2:end); % extension without point
            try
                switch lower(extp)
                    case {'.vol','.sal','.tem','.vdf','.tau'}
                        NewFI.Attributes.(ext) = waqfil('open',[base ext],FI.NoSeg);
                    case {'.are','.flo','.poi','.len'}
                        %NewFI.Attributes.(ext) = waqfil('open',[base ext],sum(FI.NoExchMNK));
                    case {'.srf','.dps'}
                        NewFI.Attributes.(ext) = waqfil('open',[base ext]);
                end
            catch
            end
        end
        delete(H)
        f = findobj(mfig,'tag','loadvolflux');
        set(f,'string','Reload Volumes, Fluxes, ...')
        
    case {'treatas1d','balancefile','nettransport'}
        f = findobj(mfig,'tag',cmd);
        if nargin>3
            Log = varargin{1};
            if ~isequal(Log,0) && ~isequal(Log,1)
                error('Invalid argument specified for %s.',cmd)
            end
            value = Log;
        else
            value = get(f,'value');
        end
        set(f,'value',value)
        NewFI = setfield(NewFI,cmd,value);
        cmdargs = {cmd value};
        %
        if isequal(cmd,'balancefile')
            Fld = 'nettransport';
            f = findobj(mfig,'tag',Fld);
            if value
                if isfield(FI,Fld)
                    value = getfield(FI,Fld);
                else
                    value = 0;
                end
                set(f,'value',value,'enable','on')
            else
                set(f,'value',0,'enable','off')
            end
        end
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function OK=optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
%
voffset=FigPos(4)-30;
width=FigPos(3)-20;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions treatas1d', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Treat as 1D', ...
    'Enable','off', ...
    'Tooltip','Assume that history stations are ordered along a line', ...
    'Tag','treatas1d');
%
voffset=voffset-30;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions balancefile', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Balance File', ...
    'Enable','off', ...
    'Tooltip','Treat file as balance file', ...
    'Tag','balancefile');
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions nettransport', ...
    'Horizontalalignment','left', ...
    'Position',[181 voffset width 18], ...
    'String','Net transport', ...
    'Enable','off', ...
    'Tooltip','Combine "transport in" and "transport out" to "net transport" term', ...
    'Tag','nettransport');
%
voffset=voffset-30;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions loadvolflux', ...
    'Enable','off', ...
    'Position',[11 voffset 160 20], ...
    'String','Load Volumes, Fluxes, ...', ...
    'Tag','loadvolflux')
%
voffset=voffset-30;
h2 = uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions autoprocdef', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width 18], ...
    'String','Auto Locate Process Definition File', ...
    'Enable','off', ...
    'Tooltip','Automatically locate process definition file', ...
    'Tag','autoprocdef');
voffset=voffset-25;
h2 = uicontrol('Parent',h0, ...
    'Style','edit', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions procdefname', ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset width-30 20], ...
    'String','procdef.dat', ...
    'Enable','off', ...
    'Tooltip','Process definition file', ...
    'Tag','procdefname');
h2 = uicontrol('Parent',h0, ...
    'Style','pushbutton', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions procdefbrowse', ...
    'Horizontalalignment','left', ...
    'Position',[21+width-30 voffset 20 20], ...
    'String','...', ...
    'Enable','off', ...
    'Tooltip','Browse for process definition file', ...
    'Tag','procdefbrowse');
OK=1;
% -----------------------------------------------------------------------------

function S = nochange(S)
% dummy routine to not change the case
