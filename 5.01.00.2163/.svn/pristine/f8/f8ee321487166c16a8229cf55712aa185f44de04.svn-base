function varargout=mikezerofil(FI,domain,field,cmd,varargin)
%MIKEZEROFIL QP support for Mike 11, 21 and 3 files.
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
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'stations'
        varargout={{}};
        return
    case 'subfields'
        varargout={{}};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
gidx={[] [] 0 0 0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

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

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) | isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            if DataInCell, gidx{i}=1:sz(i)+1; end
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        elseif DataInCell
            gidx{i}=idx{i}(1):(idx{i}(end)+1);
        end
    end
end
if ~DataInCell
    gidx=idx;
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

% read grid ...
x=[];
y=[];
z=[];
triangular = 0;
if XYRead & DimFlag(M_)
    if isequal(FI.NumCoords,'u')
        triangular = 1;
        xid = strmatch('X-coord',{FI.Item.Name},'exact');
        x=mike('read',FI,xid,1);
        yid = strmatch('Y-coord',{FI.Item.Name},'exact');
        y=mike('read',FI,yid,1);
        zid = strmatch('Z-coord',{FI.Item.Name},'exact');
        if ~isempty(zid)
            z=mike('read',FI,zid,1);
        end
        tri = strmatch('Connectivity',{FI.Item.Name},'exact');
        TRI=mike('read',FI,tri,1);
        nnd = strmatch('No of nodes',{FI.Item.Name},'exact');
        NND=mike('read',FI,nnd,1);
        TRI = reshape(TRI,[NND(1) length(TRI)/NND(1)])';
    else
        if isfield(FI,'Grid') % Mike21C grid
            x=mike('read',FI.Grid,1,1);
            y=mike('read',FI.Grid,2,1);
        else
            G=mike('read',FI,Props.Index,-1);
            if isfield(G,'X')
                x=G.X;
            end
            if isfield(G,'Y')
                y=G.Y;
            end
            if isfield(G,'Z')
                z=G.Z;
            end
        end
        if ~DataInCell
            if isempty(y)
                x=transpose(conv(x,[.5 .5]));
            else
                [x,y,z]=corner2center(x,y,z);
            end
        end
    end
end

% read data ...
val1=mike('read',FI,Props.Index,idx{T_});
if triangular
    %nothing
    reshape(val1,[length(idx{T_}) NND(1) length(TRI)])
else
    elidx=idx([M_ N_ K_]);
    elgidx=gidx([M_ N_ K_]);
    for i=1:3, if isequal(elidx{i},0), elidx{i}=1; end, end
    for i=1:3, if isequal(elgidx{i},0), elgidx{i}=1; end, end

    if ~isempty(x)
        x=squeeze(x(elgidx{:}));
    end
    if ~isempty(y)
        y=squeeze(y(elgidx{:}));
    end
    if ~isempty(z)
        z=squeeze(z(elgidx{:}));
    end
    if length(idx{T_})~=1
        val1=squeeze(val1(:,elidx{:}));
    else
        val1=squeeze(val1(elidx{:}));
    end
end

% generate output ...
if XYRead
    if triangular
        Ans.TRI=TRI;
        if ~isempty(z)
            Ans.XYZ=reshape([x y z],[1 length(x) 1 3]);
        else
            Ans.XYZ=reshape([x y],[1 length(x) 1 2]);
        end
    else
        if ~isempty(x)
            Ans.X=x;
            Ans.XUnits = 'm';
        end
        if ~isempty(y)
            Ans.Y=y;
            Ans.YUnits = 'm';
        end
        if ~isempty(z)
            Ans.Z=z;
            Ans.ZUnits = 'm';
        end
    end
end

switch Props.NVal
    case 0
    case 1
        Ans.Val=val1;
end

% read time ...
Ans.Time=readtim(FI,Props,idx{T_});

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                         'DimFlag'    'NVal' 'DataInCell' 'Index' 'UseGrid' 'Tri'};
DataProps={'data field'                    [1 0 0 0 0]  1           1       0          1       0};
Out=cell2struct(DataProps,PropNames,2);
switch FI.NumCoords
    case 'u'
        Out(1).Tri=1;
        fm=strmatch('MIKE_FM',{FI.Attrib.Name},'exact');
        if FI.Attrib(fm).Data(3)==3
            Out(1).DimFlag=[1 0 1 1 1];
        else
            Out(1).DimFlag=[1 0 1 1 0];
        end
    case 1
        Out(1).DimFlag=[1 0 1 0 0];
    case 2
        Out(1).DimFlag=[1 0 1 1 0];
    case 3
        Out(1).DimFlag=[1 0 1 1 1];
end
if ~isempty(FI.Item)
    for i=1:length(FI.Item)
        Out(i)=Out(1);
        Out(i).Name=deblank2(FI.Item(i).Name);
        Out(i).Index=i;
    end
else
    Out(:,1)=[];
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
switch FI.FileType
    case 'MikeCTDT'
        if Props.DimFlag(M_)
            sz(M_)=FI.DataDim(1)+1;
        end
        if Props.DimFlag(N_)
            sz(N_)=FI.DataDim(2)+1;
        end
        if Props.DimFlag(K_)
            sz(K_)=FI.DataDim(3)+1;
        end
        if Props.DimFlag(T_)
            sz(T_)=max(1,FI.NumTimeSteps);
        end
    case 'MikeDFS'
        idx=Props.Index;
        if Props.Tri
            fm=strmatch('MIKE_FM',{FI.Attrib.Name},'exact');
            sz(M_)=FI.Attrib(fm).Data(2);
            sz(N_)=1;
            if Props.DimFlag(K_)
                sz(K_)=FI.Attrib(fm).Data(4)-1;
                sz(M_)=sz(M_)/sz(K_);
            end
        else
            if Props.DimFlag(M_)
                sz(M_)=FI.Item(idx).MatrixSize(1);
            end
            if Props.DimFlag(N_)
                sz(N_)=FI.Item(idx).MatrixSize(2);
            end
            if Props.DimFlag(K_)
                sz(K_)=FI.Item(idx).MatrixSize(3);
            end
        end
        if Props.DimFlag(T_)
            if FI.Item(idx).Static
                sz(T_)=1;
            else
                sz(T_)=FI.NumTimeSteps;
            end
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)

%======================== SPECIFIC CODE =======================================
switch FI.FileType
    case 'MikeCTDT'
        if isequal(t,0)
            t=1:max(1,FI.NumTimeSteps);
        end
        T=FI.RefDate+FI.TimeStep*(t-1+FI.StartTimeStep);
    case 'MikeDFS'
        idx=Props.Index;
        if FI.Item(idx).Static
            T=FI.RefDate;
        else
            if isequal(t,0)
                t=1:FI.NumTimeSteps;
            end
            T=FI.RefDate+(t-1)*FI.TimeStep;
        end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin);
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=[];
cmd=lower(cmd);
cmdargs={};
switch cmd,
    case 'initialize'
        OK=optfig(mfig);
    case 'selgrid'
        Grd=mike('open');
        if ~isempty(Grd) & isfield(Grd,'Check') & isequal(Grd.Check,'OK') & Grd.NumItems==2
            NewFI=FI;
            NewFI.Grid=Grd;
        end
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function OK=optfig(h0);
Inactive=get(0,'defaultuicontrolbackground');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selgrid', ...
    'Position',[11 341 160 20], ...
    'String','select grid ...', ...
    'Tag','Pushbutton1');
OK=1;
% -----------------------------------------------------------------------------
