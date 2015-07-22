function varargout=pharosfil(FI,domain,field,cmd,varargin)
%PHAROSFIL QP support for Pharos files.
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
switch cmd,
    case 'size',
        varargout={getsize(FI,Props)};
        return;
    case 'times',
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'stations',
        varargout={readsts(FI,Props,varargin{:})};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    case 'plot'
        Parent=varargin{1};
        Ops=varargin{2};
        Station=varargin{3};
        Location = vs_get(FI,'SEICH_loc',{Station},'Point_nr','quiet');
        %
        Freqs = vs_let(FI,'SEICH_def','FREQ','quiet');
        Freqs = Freqs(Freqs>0)';
        %
        Name = Props.Data.Name;
        %Get data for Location+1 because the Location is zero-based (writing
        %program is a C-program).
        data = qpread(FI,Props.Data,'data',0,Location+1);
        %
        hNew=line(Freqs,data.Val,'color',Ops.colour);
        setappdata(Parent,'AxesType','<blocking>')
        LocationStr=readsts(FI,Props,Station);
        set(get(Parent,'title'),'string',LocationStr,'interpreter','none')
        set(get(Parent,'xlabel'),'string','frequency (Hz) \rightarrow')
        if isempty(Props.Units)
            set(get(Parent,'ylabel'),'string',[Name,' \rightarrow'])
        else
            set(get(Parent,'ylabel'),'string',[Name,' (',Props.Units,') \rightarrow'])
        end
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

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
    Props.SubFld=varargin{1};
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end


% select appropriate timestep ...
sz=getsize(FI,Props);
NRun = 1;
if ~DimFlag(T_)
    if ~isempty(Props.SubFld)
        idx{T_}=Props.SubFld;
    else
        idx{T_}=1;
    end
else
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
    switch Props.Name
        case 'wave image'
            t = idx{T_};
            if ~isempty(Props.SubFld)
                Info = vs_disp(FI,Props.Group,[]);
                NRun = Info.SizeDim;
                if Props.SubFld>NRun
                    idx{T_}=1:NRun;
                else
                    idx{T_}=Props.SubFld;
                end
            else
                idx{T_}=1;
            end
    end
end

% generate output ...
if XYRead
    X=vs_let(FI,'GRID_coor','X_coor','quiet');
    Y=vs_let(FI,'GRID_coor','Y_coor','quiet');
    XYZ=[X Y];
    %
    PointIndices=vs_let(FI,'MESH_2','KMESHC','quiet');
    INPELM=vs_let(FI,'MESH_1','INPELM','quiet');
    INELEM=vs_let(FI,'MESH_1','INELEM','quiet');
    %
    % INPELM contains the number of points per element
    % INELEM contains the number of elements
    % The last "element set" contains the triangles.
    % The other "element sets" contain the boundary conditions.
    %
    switch Props.Name
        case {'closed boundaries','open boundary','transmission boundaries'}
            LELNR=vs_let(FI,'GRID_adm',{1},'LELNR','quiet');
            LELNR=LELNR(cumsum(INELEM))';
            elm={};
            ind=0;
            for i=1:length(INPELM)-1
                if LELNR(i)==6 & strcmp(Props.Name,'closed boundaries')
                    elm{i}=transpose(reshape(PointIndices(ind+(1:INPELM(i)*INELEM(i))),[INPELM(i) INELEM(i)]));
                elseif LELNR(i)==3 & strcmp(Props.Name,'open boundary')
                    bnd=reshape(PointIndices(ind+(1:INPELM(i))),[INPELM(i) 1]);
                    elm{i}=[bnd(1:end-1) bnd(2:end)];
                elseif LELNR(i)==4 & strcmp(Props.Name,'transmission boundaries')
                    bnd=reshape(PointIndices(ind+(1:INPELM(i))),[INPELM(i) 1]);
                    elm{i}=[bnd(1:end-1) bnd(2:end)];
                else
                    elm{i}=zeros(0,2);
                end
                ind=ind+INPELM(i)*INELEM(i);
            end
            %
            Ans.XY=XYZ;
            Ans.SEG=cat(1,elm{:});
        otherwise
            if idx{M_}~=0
                XYZ = XYZ(idx{M_},:);
            end
            XYZ=reshape(XYZ,[1 size(XYZ,1) 1 2]);
            Ans.XYZ=XYZ;
            %
            ind=sum(INPELM(1:end-1).*INELEM(1:end-1));
            TRI=transpose(reshape(PointIndices(ind+(1:INPELM(end)*INELEM(end))),[INPELM(end) INELEM(end)]));
            %
            if idx{M_}~=0
                Translate=zeros(sz(M_),1);
                Translate(idx{M_})=1:length(idx{M_});
                TRI = Translate(TRI);
                TRI = TRI(all(TRI,2),:);
            end
            Ans.TRI=TRI;
    end
end

switch Props.Name
    case 'wave image'
        % need all points for wave image to determine phase associated with
        % the maximum amplitude in the whole field
        m = idx{M_};
        idx{M_}=0;
end

RPAR=vs_get(FI,'INFO',{1},'RPAR','quiet');
WL = RPAR(4);
switch Props.NVal
    case 0
    case 1
        %H=vs_get(FI,'GRID','H_depth');
        switch Props.Name
            case 'relative breaking intensity'
                Val1=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,{0},'quiet');
            otherwise
                Val1=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,{idx{M_}},'quiet');
        end
        Val2=[];
        if ~isempty(Props.Val2)
            Val2=vs_let(FI,Props.Group,{idx{T_}},Props.Val2,{idx{M_}},'quiet');
        end
        switch Props.Name
            case 'water level'
                Ans.Val = Val1;
                Ans.Val(:) = WL;
            case 'bed level'
                Ans.Val = WL - Val1;
            case 'wave height'
                Amp = sqrt(Val1.^2 + Val2.^2);
                Ans.Val = 2*Amp;
            case 'wave image'
                Amp   = sqrt(Val1.^2 + Val2.^2);
                Phase = atan2(Val2,Val1);
                if NRun>1
                    Info = vs_disp(FI,'SPECTRAL-INFO',[]);
                    if isstruct(Info) & Info.SizeDim>0
                        Freqs = vs_let(FI,'SPECTRAL-INFO','SPECTRAL-RPAR',{1},'quiet');
                        Weights = vs_let(FI,'SPECTRAL-INFO','WEIGHTS','quiet')';
                        Weights = Weights(:);
                        if size(Amp,1)~=NRun
                            Weights = Weights(idx{T_});
                        end
                        for i=1:size(Amp,1)
                            Amp(i,:) = Weights(i)*Amp(i,:);
                        end
                    else
                        error('Combination not implemented.');
                        Freqs = vs_let(FI,'INFO','RPAR',{1},'quiet');
                    end
                    Per = 1./Freqs;
                    Per = repmat(Per(:)',NRun/length(Per),1);
                    Per = Per(:);
                    if size(Amp,1)~=NRun
                        Per = Per(idx{T_});
                    end
                    SeicheSpecial=0;
                else
                    Per=Props.Period;
                    SeicheSpecial=size(Amp,1)>1;
                end
                %
                if SeicheSpecial
                    %
                    [Ampmax,i] = max(Amp,[],2);
                    ii = sub2ind(size(Phase),1:size(Phase,1),i');
                    PhaseRef   = Phase(ii)';
                    % apply selection
                    if m~=0
                        Amp = Amp(:,m);
                        Phase = Phase(:,m);
                    end
                    Ans.Val = zeros(length(ii),size(Amp,2));
                    for i=1:length(ii)
                        Ans.Val(i,:) = Amp(i,:).*cos(Phase(i,:)-PhaseRef(i));
                    end
                else
                    SumAmp=sum(Amp,1);
                    [Ampmax,i] = max(SumAmp);
                    PhaseRef   = Phase(:,i);
                    % apply selection
                    if m~=0
                        Amp = Amp(:,m);
                        Phase = Phase(:,m);
                    end
                    if ~DimFlag(T_)
                        t=0;
                    end
                    Ans.Val = zeros(length(t),size(Amp,2));
                    deltaT = Props.Period/Props.NSamples;
                    for i=1:length(t)
                        for f=1:size(Amp,1)
                            Ans.Val(i,:) = Ans.Val(i,:) + Amp(f,:).*cos(Phase(f,:)-PhaseRef(f)-2*pi*t(i)*deltaT/Per(f));
                        end
                    end
                    idx{T_} = t;
                end
            case 'wave phase'
                Phase   = atan2(Val2,Val1);
                Ans.Val = Phase;
            case 'relative breaking intensity'
                mVal1 = max(Val1(:));
                if ~isequal(idx{M_},0)
                    Val1 = Val1(:,idx{M_});
                end
                Ans.Val = Val1/mVal1;
            otherwise
                Ans.Val = Val1;
        end
    otherwise
        Val1=vs_let(FI,Props.Group,{idx{T_}},Props.Val1,{idx{M_}},'quiet');
        Val2=vs_let(FI,Props.Group,{idx{T_}},Props.Val2,{idx{M_}},'quiet');
        switch Props.Name
            case 'maximum velocity'
                Ans.XComp=Val1.*cos(Val2);
                Ans.YComp=Val1.*sin(Val2);
            otherwise
                Ans.XComp=Val1;
                Ans.YComp=Val2;
        end
end

% read time ...
T=readtim(FI,Props,idx{T_});
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);

%======================== SPECIFIC CODE =======================================
PropNames={'Name'             'Units'  'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Group'      'Val1'           'Val2'      'UseGrid' 'SubFld'};
DataProps={'grid'             ''       'TRI'  'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_coor'   'X_coor'         ''           1         ''
    'open boundary'            ''       'SEG'  'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_adm'   'LELNR'          ''           2         ''
    'transmission boundaries'  ''       'SEG'  'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_adm'   'LELNR'          ''           2         ''
    'closed boundaries'        ''       'SEG'  'xy'     [0 0 6 0 0]  0          0     ''        ''    ''       'GRID_adm'   'LELNR'          ''           2         ''
    '-------'                  ''       ''     ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'water level'              'm'      'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'H_depth'        ''           1         ''
    'bed level'                'm'      'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'H_depth'        ''           1         ''
    'water depth'              'm'      'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'H_depth'        ''           1         ''
    'velocity'                 'm/s'    'TRI'  'xy'     [0 0 6 0 0]  0          2     ''        ''    ''       'CURRENT'    'Ux_veloc'       'Uy_veloc'   1         ''
    %   'relative radiation freqency' '1/s' 'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'CURRENT'    'Omega_rel'      ''           1         ''
    '-------'                  ''       ''     ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'relative breaking intensity' ''     'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'BREAKING'   'Gamma_b'        ''           1         ''
    'weighted mean wave height dir.' 'm' 'TRI'  'xy'    [0 0 6 0 0]  0          1     ''        ''    ''       'HS_dir'     'HS_directional' ''           1         'md'
    'wave height'              'm'      'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          'PHI_i'      1         'nrun'
    'wave height'              'm'      'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         'PHIs_i'     1         'sf'
    'wave phase'               ''       'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          'PHI_i'      1         'nrun'
    'wave phase'               ''       'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         'PHIs_i'     1         'sf'
    'wave image'               'm'      'TRI'  'xy'     [7 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          'PHI_i'      1         'nrun'
    'wave image'               'm'      'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         'PHIs_i'     1         'sf'
    'maximum velocity'         'm/s'    'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'UMAX'           ''           1         'sf'
    'maximum velocity direction' ...
    'radians' 'TRI' 'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'UDIR'           ''           1         'sf'
    %   'maximum velocity'         'm/s'    'TRI'  'xy'     [0 0 6 0 0]  0          2     ''        ''    ''       'SEICH_res'  'UMAX'           'UDIR'       1         'sf'
    'minimum velocity'         'm/s'    'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'UMIN'           ''           1         'sf'
    '-------'                  ''       ''     ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'wave number'              ''       'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'K_wave'         ''           1         'nfreq'
    'phase velocity'           'm/s'    'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'C_wave'         ''           1         'nfreq'
    'group velocity'           'm/s'    'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'GRID'       'Cg_wave'        ''           1         'nfreq'
    'potential (real part)'    ''       'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_r'          ''           1         'nrun'
    'potential (imag part)'    ''       'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'POTENTIALS' 'PHI_i'          ''           1         'nrun'
    'seiches potential (real part)' ''  'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_r'         ''           1         'sf'
    'seiches potential (imag part)' ''  'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SEICH_res'  'PHIs_i'         ''           1         'sf'
    '-------'                  ''       ''     ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''
    'weighted period Tm-1,0'   's'      'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SPECTRAL-PAR' 'TM10'         ''           1         ''
    'wave number based on Tm-1,0' ...
    ''       'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SPECTRAL-PAR' 'KTM10'        ''           1         ''
    'radial frequency based on Tm-1,0' ...
    ''       'TRI'  'xy'     [0 0 6 0 0]  0          1     ''        ''    ''       'SPECTRAL-PAR' 'OMEGATM10'    ''           1         ''
    '-------'                  ''       ''     ''       [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''};
%======================== SPECIFIC CODE DIMENSIONS ============================
SkipGroup={};
SkipElem={'RKN_roughness'};
%
Info = vs_disp(FI,'GRID_coor',[]);
npnt = Info.SizeDim;
%
% Don't want to sort the Grps and therefore I don't use setdiff here ...
%
Grps=vs_disp(FI,[]);
Grps(ismember(Grps,SkipGroup))=[];
%
dpGrp=strmatch('Group',PropNames,'exact');
dpVal1=strmatch('Val1',PropNames,'exact');
dpVal2=strmatch('Val2',PropNames,'exact');
fld=size(DataProps,1);
for i=1:length(Grps)
    Grpi=strmatch(Grps{i},DataProps(:,dpGrp),'exact');
    InfoG=vs_disp(FI,Grps{i},[]);
    %
    % Don't want to sort the Elms and therefore I don't use setdiff here ...
    %
    Elms=vs_disp(FI,Grps{i});
    Elms(ismember(Elms,SkipElem))=[];
    %
    for j=1:length(Elms)
        Elmi=strmatch(Elms{j},DataProps(Grpi,dpVal1),'exact');
        Elmi=[Elmi;strmatch(Elms{j},DataProps(Grpi,dpVal2),'exact')];
        Info=vs_disp(FI,Grps{i},Elms{j});
        if (isempty(Elmi) | isempty(Grpi)) & isstruct(Info) & all(InfoG.SizeDim>0)
            if isequal(Info.SizeDim,npnt)
                if isempty(Info.ElmDescription)
                    edescr=sprintf('%s of %s',Elms{j},Grps{i});
                else
                    edescr=Info.ElmDescription;
                end
                eunit='';
                if ~isempty(Info.ElmUnits)
                    eunit=deblank2(Info.ElmUnits);
                    if isequal(eunit([1 end]),'[]')
                        eunit=eunit(2:end-1);
                    end
                end
                subf='';
                if ~isequal(InfoG.SizeDim,1)
                    subf='agd';
                end
                fld=fld+1;
                DataProps(fld,:)={edescr            eunit  'TRI' 'xy' [0 0 6 0 0]  0          1     ''        ''    ''       Grps{i}      Elms{j}          ''           1         subf};
            end
        end
    end
    fld=fld+1;
    DataProps(fld,:)={'-------'                  ''   ''  ''   [0 0 0 0 0]  0          0     ''        ''    ''       ''           ''               ''           1         ''};
end
%======================== DataProps conversion ================================
Out=cell2struct(DataProps,PropNames,2);
%======================== SPECIFIC CODE REMOVE ================================
for i=size(Out,1):-1:1
    Info=vs_disp(FI,Out(i).Group,[]);
    Info2=vs_disp(FI,Out(i).Group,Out(i).Val1);
    if ~isempty(strmatch('---',Out(i).Name))
    elseif ~isstruct(Info2) | Info.SizeDim==0
        % remove references to non-stored data fields
        Out(i)=[];
    end
end

Info = vs_disp(FI,'SEICH_def',[]);
if isstruct(Info)
    %seiches
    Period=1; % arbitrary finite, non-zero value
    NSamp=1;
    for i=length(Out):-1:1
        if isequal(Out(i).SubFld,'nfreq')
            Out(i)=[];
        end
    end
else
    % not seiches
    Info = vs_disp(FI,'SPECTRAL-INFO',[]);
    if isstruct(Info) & Info.SizeDim>0
        % spectral model
        for i=1:length(Out)
            if strcmp(Out(i).Name,'weighted mean wave height dir.')
                Out(i).Name = 'wave height spectral model';
                Out(i).SubFld = [];
            end
        end
        %
        Freqs = vs_let(FI,'SPECTRAL-INFO','SPECTRAL-RPAR',{1},'quiet');
    else
        Freqs = vs_let(FI,'INFO','RPAR',{1},'quiet');
    end
    iszero = Freqs==0;
    Freqs(iszero) = 1;
    Per = 1./Freqs;
    Per(iszero) = 1000;
    %
    Pmin = min(Per);
    Pmax = max(Per);
    nPmin = Pmin:Pmin:max(100*Pmin,10*Pmax);
    dP = 0;
    for i = 1:length(Per)
        dP = max(dP,abs(max(1,round(nPmin/Per(i)))*Per(i)-nPmin));
    end
    [dPmin,n] = min(dP);
    Period = n*Pmin;
    %
    NSamp = 100*(Period/min(Per));
end
for i=1:length(Out)
    Out(i).Period   = Period;
    Out(i).NSamples = NSamp;
end
Info = vs_disp(FI,'SEICH_loc',[]);
if isstruct(Info) & Info.SizeDim>0
    SeichFreq = strmatch('sf',{Out.SubFld});
    if ~isempty(SeichFreq)
        Out(end+1)=Out(end);
        for i=SeichFreq'
            Out(end+1) = Out(i);
            Out(end).Name = cat(1,[Out(end).Name ' (frequency graph)']);
            Out(end).SubFld = '';
            Out(end).DimFlag = [0 5 0 0 0];
            Out(end).NVal = -1;
            Out(end).Tri = 0;
            Out(end).Data = Out(i);
        end
    end
end

% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
if isempty(Props.SubFld)
    return
end
switch Props.SubFld
    case 'agd'
        InfoG = vs_disp(FI,Props.Group,[]);
        for i=InfoG.SizeDim:-1:1
            subf{i}=sprintf('field %i',i);
        end
    case 'md' % main directions in case of directional spreading
        MainDirs = vs_let(FI,'HS_dir','QH_inc',{1},'quiet')*180/pi;
        for i=length(MainDirs):-1:1
            subf{i}=sprintf('%g deg',MainDirs(i));
        end
    case {'sf','nfreq','nrun'} % frequencies / periods
        Info = vs_disp(FI,'SPECTRAL-INFO',[]);
        spectral = isstruct(Info);
        %
        freq = 0;
        switch Props.SubFld
            case 'sf'
                freq = 1;
                Freqs = vs_let(FI,'SEICH_def','FREQ','quiet');
            case {'nfreq','nrun'}
                Info = vs_disp(FI,'SPECTRAL-INFO',[]);
                if isstruct(Info) & Info.SizeDim>0
                    Freqs = vs_let(FI,'SPECTRAL-INFO','SPECTRAL-RPAR',{1},'quiet');
                else
                    Freqs = vs_let(FI,'INFO','RPAR',{1},'quiet');
                end
        end
        Freqs = Freqs(Freqs>0);
        NFreq = length(Freqs);
        for i=NFreq:-1:1
            if freq
                if Freqs(i)<0.1
                    subf{i}=sprintf('%g mHz',1000*Freqs(i));
                else
                    subf{i}=sprintf('%g Hz',Freqs(i));
                end
            else
                subf{i}=sprintf('%g s',1/Freqs(i));
            end
        end
        if NFreq==0
            subf={};
        end
        if strcmp(Props.SubFld,'nrun')
            freq = subf;
            Info = vs_disp(FI,'POTENTIALS',[]);
            NRun = Info.SizeDim;
            NDir = NRun/NFreq;
            Dirs = vs_let(FI,'GRID_adm','QH_incident',{1},'quiet')*180/pi;
            for direction=NDir:-1:1
                dirstr{direction} = sprintf('%g deg',Dirs(direction));
            end
            direction = NDir;
            frequency = NFreq;
            for run=NRun:-1:1
                subf{run}=sprintf('%s - %s',freq{frequency},dirstr{direction});
                direction = direction-1;
                if direction<1
                    frequency = frequency-1;
                    direction = NDir;
                end
            end
            %
            if NRun>1 & strcmp(Props.Name,'wave image') % & ~spectral
                subf{end+1}='combined';
            end
        end
end
if nargin>2 & f~=0
    subf=subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(M_)
    Info=vs_disp(FI,'GRID_coor',[]);
    sz(M_)=Info.SizeDim;
end
if Props.DimFlag(ST_)
    Info=vs_disp(FI,'SEICH_loc',[]);
    sz(ST_)=Info.SizeDim;
end
if Props.DimFlag(T_)
    switch Props.Name
        case 'wave image'
            sz(T_)=Props.NSamples;
        otherwise
            Info=vs_disp(FI,Props.Group,[]);
            sz(T_)=Info.SizeDim;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
if isequal(t,0)
    t=1:Props.NSamples;
end
switch Props.Name
    case 'wave image'
        T=Props.Period*t/Props.NSamples/24/3600;
    otherwise
        T=zeros(length(t),1);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,s)
%======================== SPECIFIC CODE =======================================
if nargin<3
    s=0;
end
Sx=vs_let(FI,'SEICH_loc',{s},'DESCR','quiet');
S=cell(size(Sx,1),1);
for i=1:size(Sx,1)
    Name = Sx(i,:);
    j = min(find(Name==0));
    if ~isempty(j)
        S{i} = Name(1:j-1);
    else
        S{i} = deblank(Name);
    end
end
% -----------------------------------------------------------------------------
