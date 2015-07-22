function varargout=usrdeffil(FI,domain,field,cmd,varargin)
%USRDEFFIL QP support for user defined variables.
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
    error('Not enough input arguments')
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
        return;
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'stations'
        varargout={readsts(FI,Props,varargin{:})};
        return
    case 'subfields'
        varargout={{}};
        return
    case 'plot'
        Parent = varargin{1};
        Ops = varargin{2};
        SubSelected = varargin(3:end);
        Selected = FI.Selected;
        j=0;
        for i = 1:5
            if FI.DimFlag(i)~=0
                j=j+1;
                Selected{i}=Selected{i}(SubSelected{j});
            end
        end
        Selected(FI.DimFlag==0)=[];
        [Chk,hNew,FI.FileInfo]=qp_getdata(FI.FileInfo,FI.Domain,FI.Props,'plot',Parent,Ops,Selected{:});
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] [] [] []};
idx(DimFlag~=0)={0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

i=Props.Fld;
[Ans,FIi]=getdata(FI(i),cmd,idx);
Ans.Time=readtim(FI,Props,idx{1});
FI(i)=FIi;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                       'DimFlag' 'DataInCell' 'NVal' 'Fld' 'Tri'};
DataProps={''                            [0 0 1 1 0]  0          0     0    0};
DataProps(1,:)=[];

for i=1:length(FI)
    DataProps{i,1}=FI(i).Name;
    DataProps{i,2}=FI(i).DimFlag;
    if isfield(FI(i).Props,'DataInCell')
        DataProps{i,3}=FI(i).Props.DataInCell;
    else
        DataProps{i,3}=0;
    end
    DataProps{i,4}=FI(i).Props.NVal;
    DataProps{i,5}=i;
    if isfield(FI(i),'Tri') & ~isempty(FI(i).Tri)
        DataProps{i,6}=FI(i).Tri;
    else
        DataProps{i,6}=0;
    end
end
Out=cell2struct(DataProps,PropNames,2);


% -----------------------------------------------------------------------------
function [Ans,NProps]=getdata(Props,cmd,sel)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
TDep=0;
kIndex=3;
if isfield(Props,'Tri') & Props.Tri
    TDep=1;
    kIndex=2;
elseif Props.DimFlag(T_)
    if length(sel{T_})>1
        TDep=1;
    elseif sel{T_}==0
        szi=getsize([],Props);
        if szi(T_)>1
            TDep=1;
        end
    end
end
if isequal(Props.FileInfo,'operator')
    P=Props.Props.Data{1};
    Oper=Props.Props.Oper;
    switch Oper
        case {'series: A,B'}
            t=sel{T_};
            first=1;
            off=0;
            for i=1:length(Props.Props.Data)
                P=Props.Props.Data{i};
                szi=getsize([],P);
                szit=szi(T_);
                if t==0
                    [Ansi,FI]=getdata(P,cmd,sel);
                    Props.Props.Data{i}=FI;
                else
                    ti=t(t>off)-off;
                    ti(ti>szit)=[];
                    sel{T_}=ti;
                    if ~isempty(sel{T_})
                        [Ansi,FI]=getdata(P,cmd,sel);
                        Props.Props.Data{i}=FI;
                    else
                        Ansi=[];
                    end
                end
                if isempty(Ansi)
                elseif first
                    Ans=Ansi;
                    first=0;
                else
                    if isfield(Ans,'XComp')
                        Ans.XComp=cat(1,Ans.XComp,Ansi.XComp);
                    end
                    if isfield(Ans,'YComp')
                        Ans.YComp=cat(1,Ans.YComp,Ansi.YComp);
                    end
                    if isfield(Ans,'ZComp')
                        Ans.ZComp=cat(1,Ans.ZComp,Ansi.ZComp);
                    end
                    if isfield(Ans,'Val')
                        Ans.Val=cat(1,Ans.Val,Ansi.Val);
                    end
                end
                off=off+szit;
            end
        case {'A+B','A-B','A*B','A/B','max(A,B)','min(A,B)','vector(A,B)','vec_M&D(A,B)','A under condition B'}
            sz1=getsize([],P);
            P2=Props.Props.Data{2};
            sz2=getsize([],P2);
            cmd2=cmd;
            if any(sz1([M_ N_ K_])<sz2([M_ N_ K_])) % somewhere a 1 to N mapping, so use 2nd grid
                if strcmp(cmd(1:4),'grid')
                    cmd(1:4)=[];
                end
                griduse=2;
            else % default (N to N mappings or N to 1 mapping), use 1st grid
                if strcmp(cmd(1:4),'grid')
                    cmd2(1:4)=[];
                end
                griduse=1;
            end
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            [Ans2,FI]=getdata(P2,cmd2,sel);
            Props.Props.Data{2}=FI;
            if griduse~=1
                if isfield(Ans2,'XYZ')
                    Ans.XYZ=Ans2.XYZ;
                end
                if isfield(Ans2,'TRI')
                    Ans.XYZ=Ans2.TRI;
                end
                if isfield(Ans2,'X')
                    Ans.X=Ans2.X;
                end
                if isfield(Ans2,'Y')
                    Ans.Y=Ans2.Y;
                end
                if isfield(Ans2,'Z')
                    Ans.Z=Ans2.Z;
                end
            end
            if isfield(Ans,'Val')
                sz1=size(Ans.Val);
                vec1=0;
            else
                sz1=size(Ans.XComp);
                vec1=1;
            end
            if isfield(Ans2,'Val')
                sz2=size(Ans2.Val);
                vec2=0;
            else
                sz2=size(Ans2.XComp);
                vec2=1;
            end
            if length(sz1)~=length(sz2)
                jj=max(length(sz1),length(sz2));
                if jj>length(sz1)
                    sz1=[sz1 ones(1,jj-length(sz1))];
                else
                    sz2=[sz2 ones(1,jj-length(sz2))];
                end
            end
            sz=max(sz1,sz2);
            res1=sz./max(sz1,1);
            if ~all(res1==1)
                if vec1
                    Ans.XComp=repmat(Ans.XComp,res1);
                    if isfield(Ans,'YComp');
                        Ans.YComp=repmat(Ans.YComp,res1);
                    end
                    if isfield(Ans,'ZComp');
                        Ans.ZComp=repmat(Ans.ZComp,res1);
                    end
                else
                    Ans.Val=repmat(Ans.Val,res1);
                end
            end
            res2=sz./max(sz2,1);
            if ~all(res2==1)
                if vec2
                    Ans2.XComp=repmat(Ans2.XComp,res2);
                    if isfield(Ans2,'YComp');
                        Ans2.YComp=repmat(Ans2.YComp,res2);
                    end
                    if isfield(Ans2,'ZComp');
                        Ans2.ZComp=repmat(Ans2.ZComp,res2);
                    end
                else
                    Ans2.Val=repmat(Ans2.Val,res2);
                end
            end
            special=0;
            switch Oper
                case 'A+B'
                    OpFun='plus';
                case 'A-B'
                    OpFun='minus';
                case 'A*B'
                    OpFun='times';
                case 'A/B'
                    OpFun='rdivide';
                    if isfield(Ans2,'XComp');
                        Ans2.XComp(Ans2.XComp==0)=NaN;
                    end
                    if isfield(Ans2,'YComp');
                        Ans2.YComp(Ans2.YComp==0)=NaN;
                    end
                    if isfield(Ans2,'ZComp');
                        Ans2.ZComp(Ans2.ZComp==0)=NaN;
                    end
                    if isfield(Ans2,'Val');
                        Ans2.Val(Ans2.Val==0)=NaN;
                    end
                case 'max(A,B)'
                    OpFun='max';
                case 'min(A,B)'
                    OpFun='min';
                case {'vector(A,B)','vec_M&D(A,B)','A under condition B'}
                    special=1;
            end
            if special
                switch Oper
                    case 'vector(A,B)'
                        Ans.XComp=Ans.Val;
                        Ans.YComp=Ans2.Val;
                        Ans=rmfield(Ans,'Val');
                    case 'vec_M&D(A,B)'
                        Ans.XComp=Ans.Val.*cos(Ans2.Val);
                        Ans.YComp=Ans.Val.*sin(Ans2.Val);
                        Ans=rmfield(Ans,'Val');
                    case 'A under condition B'
                        cond=Props.Props.Data{3};
                        val=Ans2.Val;
                        val=realset('keep',cond,val);
                        if vec1
                            Ans.XComp(isnan(val))=NaN;
                            Ans.YComp(isnan(val))=NaN;
                        else
                            Ans.Val(isnan(val))=NaN;
                        end
                end
            elseif vec1 & vec2
                Ans.XComp=feval(OpFun,Ans.XComp,Ans2.XComp);
                if isfield(Ans,'YComp');
                    Ans.YComp=feval(OpFun,Ans.YComp,Ans2.YComp);
                end
                if isfield(Ans,'ZComp');
                    Ans.ZComp=feval(OpFun,Ans.ZComp,Ans2.ZComp);
                end
            elseif vec1
                Ans.XComp=feval(OpFun,Ans.XComp,Ans2.Val);
                if isfield(Ans,'YComp');
                    Ans.YComp=feval(OpFun,Ans.YComp,Ans2.Val);
                end
                if isfield(Ans,'ZComp');
                    Ans.ZComp=feval(OpFun,Ans.ZComp,Ans2.Val);
                end
            elseif vec2
                Ans.XComp=feval(OpFun,Ans.Val,Ans2.XComp);
                if isfield(Ans2,'YComp');
                    Ans.YComp=feval(OpFun,Ans.Val,Ans2.YComp);
                end
                if isfield(Ans2,'ZComp');
                    Ans.ZComp=feval(OpFun,Ans.Val,Ans2.ZComp);
                end
                Ans=rmfield(Ans,'Val');
            else
                Ans.Val=feval(OpFun,Ans.Val,Ans2.Val);
            end
        case {'+ constant','* constant','^ constant','max(A,constant)','min(A,constant)'}
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            c=Props.Props.Data{2};
            vec1=~isfield(Ans,'Val');
            switch Oper
                case 'max(A,constant)'
                    OpFun='max';
                case 'min(A,constant)'
                    OpFun='min';
                case '+ constant'
                    OpFun='plus';
                case '* constant'
                    OpFun='times';
                case '^ constant'
                    OpFun='power';
                    if abs(c)<1
                        if isfield(Ans,'XComp');
                            Ans.XComp(Ans.XComp<0)=NaN;
                        end
                        if isfield(Ans,'YComp');
                            Ans.YComp(Ans.YComp<0)=NaN;
                        end
                        if isfield(Ans,'ZComp');
                            Ans.ZComp(Ans.ZComp<0)=NaN;
                        end
                        if isfield(Ans,'Val');
                            Ans.Val(Ans.Val<0)=NaN;
                        end
                    end
                    if c<0
                        if isfield(Ans,'XComp');
                            Ans.XComp(Ans.XComp==0)=NaN;
                        end
                        if isfield(Ans,'YComp');
                            Ans.YComp(Ans.YComp==0)=NaN;
                        end
                        if isfield(Ans,'ZComp');
                            Ans.ZComp(Ans.ZComp==0)=NaN;
                        end
                        if isfield(Ans,'Val');
                            Ans.Val(Ans.Val==0)=NaN;
                        end
                    end
            end
            if vec1
                Ans.XComp=feval(OpFun,Ans.XComp,c);
                if isfield(Ans,'YComp');
                    Ans.YComp=feval(OpFun,Ans.YComp,c);
                end
                if isfield(Ans,'ZComp');
                    Ans.ZComp=feval(OpFun,Ans.ZComp,c);
                end
            else
                Ans.Val=feval(OpFun,Ans.Val,c);
            end
            switch Oper
                case '^ constant'
                    Ans.Val=real(Ans.Val);
            end
        case {'10log','abs'}
            switch Oper
                case '10log'
                    fun='log10';
                case 'abs'
                    fun='abs';
            end
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            if isfield(Ans,'XComp');
                Ans.XComp(Ans.XComp<=0)=NaN;
                Ans.XComp=feval(fun,Ans.XComp);
            end
            if isfield(Ans,'YComp');
                Ans.YComp(Ans.YComp<=0)=NaN;
                Ans.YComp=feval(fun,Ans.YComp);
            end
            if isfield(Ans,'ZComp');
                Ans.ZComp(Ans.ZComp<=0)=NaN;
                Ans.ZComp=feval(fun,Ans.ZComp);
            end
            if isfield(Ans,'Val');
                Ans.Val(Ans.Val<=0)=NaN;
                Ans.Val=feval(fun,Ans.Val);
            end
        case {'max m','alg.mean m','min m','sum m', ...
                'max n','alg.mean n','min n','sum n', ...
                'max k','alg.mean k','min k','sum k'}
            switch Oper(end)
                case 'm'
                    m_ = M_;
                    dIndex = 1;
                case 'n'
                    m_ = N_;
                    dIndex = 2;
                case 'k'
                    m_ = K_;
                    dIndex = kIndex;
            end
            sel{m_}=0;
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            switch Oper(1:end-2)
                case 'max'
                    Ans.Val=max(Ans.Val,[],TDep+dIndex);
                    Ans=avgcoord(Ans,dIndex);
                case 'alg.mean'
                    Ix=isnan(Ans.Val);
                    Ans.Val(Ix)=0;
                    Ans.Val=sum(Ans.Val,TDep+dIndex)./max(sum(~Ix,TDep+dIndex),1);
                case 'sum'
                    Ix=isnan(Ans.Val);
                    Ans.Val(Ix)=0;
                    Ans.Val=sum(Ans.Val,TDep+dIndex);
                case 'min'
                    Ans.Val=min(Ans.Val,[],TDep+dIndex);
            end
            Ans=avgcoord(Ans,dIndex);
        case {'flip m','flip n','flip k'}
            switch Oper(end)
                case 'm'
                    m_ = M_;
                    dIndex = 1;
                case 'n'
                    m_ = N_;
                    dIndex = 2;
                case 'k'
                    m_ = K_;
                    dIndex = kIndex;
            end
            sel{m_}=0;
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            for fldc={'Val','XComp','YComp','ZComp'}
                fld = fldc{1};
                if isfield(Ans,fld)
                    data = getfield(Ans,fld);
                    dims = repmat({':'},1,ndims(data));
                    dims{dIndex} = size(data,dIndex):-1:1;
                    data = data(dims{:});
                    Ans = setfield(Ans,fld,data);
                    data = [];
                end
            end
        case 'magnitude'
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            Ans.Val=Ans.XComp.^2;
            Ans=rmfield(Ans,'XComp');
            if isfield(Ans,'YComp');
                Ans.Val=Ans.Val+Ans.YComp.^2;
                Ans=rmfield(Ans,'YComp');
            end
            if isfield(Ans,'ZComp');
                Ans.Val=Ans.Val+Ans.ZComp.^2;
                Ans=rmfield(Ans,'ZComp');
            end
            Ans.Val=sqrt(Ans.Val);
        otherwise
            error('Operator ''%s'' not implemented',Oper)
    end
    %
    % As long as Units treatment is not yet valid, remove Units ...
    %
    if isfield(Ans,'Units')
        Ans=rmfield(Ans,'Units');
    end
else
    for m_=1:5
        t=sel{m_};
        if ~isempty(t)
            selt=Props.Selected{m_};
            if ~isempty(selt) & ~isequal(selt,0)
                if length(selt)==1
                    t=selt;
                elseif isequal(t,0)
                    t=selt;
                else
                    t=selt(t);
                end
            end
            sel{m_}=t;
        end
    end
    S=sel(Props.DimFlag~=0);
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,Ans,FI]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,cmd,Props.SubField{:},S{:});
    Props.FileInfo=FI;
end
NProps=Props;
% -----------------------------------------------------------------------------

function Ans=avgcoord(AnsIn,i);
Ans=AnsIn; AnsIn=[];
if isfield(Ans,'X')
    Ix=~isnan(Ans.X);
    Ans.X(~Ix)=0;
    Ans.X=sum(Ans.X,i)./max(1,sum(Ix,i));
    Ix=any(Ix,i);
    Ans.X(~Ix)=NaN;
end
if isfield(Ans,'Y')
    Ix=~isnan(Ans.Y);
    Ans.Y(~Ix)=0;
    Ans.Y=sum(Ans.Y,i)./max(1,sum(Ix,i));
    Ix=any(Ix,i);
    Ans.Y(~Ix)=NaN;
end
if isfield(Ans,'Z')
    Ix=~isnan(Ans.Z);
    Ans.Z(~Ix)=0;
    Ans.Z=sum(Ans.Z,i)./max(1,sum(Ix,i));
    Ix=any(Ix,i);
    Ans.z(~Ix)=NaN;
end
if isfield(Ans,'XYZ')
    if i==1
        Ans.XYZ=mean(Ans.XYZ,2);
        Ans.TRI=[];
    else
        Ans.XYZ=mean(Ans.XYZ,3);
    end
end


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
if isfield(Props,'Fld')
    i=Props.Fld;
    Props=FI(i);
end
if isequal(Props.FileInfo,'operator')
    switch Props.Props.Oper
        case 'series: A,B'
            P=Props.Props.Data{1};
            sz=getsize([],P);
            for i=2:length(Props.Props.Data)
                P=Props.Props.Data{i};
                szi=getsize([],P);
                sz(T_)=sz(T_)+szi(T_);
            end
        case {'max m','alg.mean m','min m','sum m'}
            P=Props.Props.Data{1};
            sz=getsize([],P);
            sz(M_)=1;
        case {'max n','alg.mean n','min n','sum n'}
            P=Props.Props.Data{1};
            sz=getsize([],P);
            sz(N_)=1;
        case {'max k','alg.mean k','min k','sum k'}
            P=Props.Props.Data{1};
            sz=getsize([],P);
            sz(K_)=1;
        otherwise
            sz=zeros(1,5);
            for i=1:length(Props.Props.Data)
                P=Props.Props.Data{i};
                if isstruct(P) & isfield(P,'FileInfo')
                    sz=max(sz,getsize([],P));
                end
            end
    end
else
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,sz]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,'size');
    for i=1:5
        if ~isempty(Props.Selected{i}) & ~isequal(Props.Selected{i},0)
            sz(i)=length(Props.Selected{i});
        end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
if isfield(Props,'Fld')
    i=Props.Fld;
    Props=FI(i);
end
if isequal(Props.FileInfo,'operator')
    switch Props.Props.Oper
        case 'series: A,B'
            T=[];
            off=0;
            for i=1:length(Props.Props.Data)
                P=Props.Props.Data{i};
                szi=getsize([],P);
                szit=szi(T_);
                if t==0
                    Ti=readtim([],P,0);
                else
                    ti=t(t>off)-off;
                    ti(ti>szit)=[];
                    Ti=readtim([],P,ti);
                end
                T=[T;Ti];
                off=off+szit;
            end
        otherwise
            nt=0;
            j=0;
            for i=1:length(Props.Props.Data)
                P=Props.Props.Data{i};
                if isstruct(P) & isfield(P,'FileInfo')
                    szi=getsize([],P);
                    if szi(T_)>nt
                        nt=szi(T_);
                        j=i;
                    end
                end
            end
            if j==0
                T=[];
            else
                P=Props.Props.Data{j};
                T=readtim([],P,t);
            end
    end
else
    selt=Props.Selected{T_};
    if ~isempty(selt) & ~isequal(selt,0)
        if length(selt)==1
            t=selt;
        elseif isequal(t,0)
            t=selt;
        else
            t=selt(t);
        end
    end
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,T]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,'times',t);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,s)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
if nargin==2
    s=0;
end
if isfield(Props,'Fld')
    i=Props.Fld;
    Props=FI(i);
end
if isequal(Props.FileInfo,'operator')
    nstat=0;
    j=0;
    for i=1:length(Props.Props.Data)
        P=Props.Props.Data{i};
        if isstruct(P)
            szi=getsize([],P);
            if szi(ST_)>nstat
                nstat=szi(ST_);
                j=i;
            end
        end
    end
    if j==0
        S={};
    else
        P=Props.Props.Data{j};
        S=readsts([],P,s);
    end
else
    sels=Props.Selected{ST_};
    if ~isempty(sels) & ~isequal(sels,0)
        if length(sels)==1
            s=sels;
        elseif isequal(s,0)
            s=sels;
        else
            s=sels(s);
        end
    end
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,S]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,'stations',s);
    if length(S)>length(s)
        S=S(s);
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin);
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
        Handle_VarList=findobj(mfig,'tag','varlist');
        Str={};
        for i=1:length(FI)
            Str{i}=FI(i).Name;
        end
        if length(Str)>0
            set(Handle_VarList,'userdata',FI,'string',Str,'value',1,'enable','on','backgroundcolor',Active);
            Handle_DelVar=findobj(mfig,'tag','delvar');
            set(Handle_DelVar,'enable','on');
        end
        options(FI,mfig,'selectvar');

    case 'delvar'
        Handle_VarList=findobj(mfig,'tag','varlist');
        Vars=get(Handle_VarList,'userdata');
        Str=get(Handle_VarList,'string');
        NrInList=get(Handle_VarList,'value');
        Vars(NrInList)=[];
        Str(NrInList)=[];
        if isempty(Vars)
            set(Handle_VarList,'enable','off','backgroundcolor',Inactive);
            Handle_DelVar=findobj(mfig,'tag','delvar');
            set(Handle_DelVar,'enable','off');
            Str=' ';
        else
            NrInList=min(NrInList,length(Vars));
        end
        set(Handle_VarList,'userdata',Vars,'string',Str,'value',NrInList);
        options(FI,mfig,'selectvar');
        NewFI=Vars;
        cmdargs={cmd};

    case 'selectvar'
        Handle_OpList=findobj(mfig,'tag','operatorlist');
        Handle_VarList=findobj(mfig,'tag','varlist');
        Vars=get(Handle_VarList,'userdata');
        Str=get(Handle_VarList,'string');
        i=get(Handle_VarList,'value');
        if isequal(Str,' ')
            Ops={};
        else
            if nargin>3
                iArg=varargin{1};
                if ischar(iArg),
                    iArg=strmatch(iArg,Str,'exact');
                    if ~isequal(size(iArg),[1 1])
                        return;
                    end
                end
                i=iArg;
                set(Handle_VarList,'value',i)
            end

            NVal=Vars(i).Props.NVal;
            switch NVal
                case {0,-1}
                    Ops={};
                case {1,2,3}
                    Ops={'A+B','A-B','A*B','A/B','max(A,B)','min(A,B)', ...
                        '+ constant','* constant','^ constant','max(A,constant)','min(A,constant)', ...
                        '10log','abs','series: A,B','A under condition B'};
                    if Vars(i).DimFlag(M_)
                        if NVal==1
                            Ops(end+(1:4))={'max m' 'alg.mean m' 'min m' 'sum m'};
                        end
                        Ops(end+1) = {'flip m'};
                    end
                    if Vars(i).DimFlag(N_)
                        if NVal==1
                            Ops(end+(1:4))={'max n' 'alg.mean n' 'min n' 'sum n'};
                        end
                        Ops(end+1) = {'flip n'};
                    end
                    if Vars(i).DimFlag(K_)
                        if NVal==1
                            Ops(end+(1:4))={'max k' 'alg.mean k' 'min k' 'sum k'};
                        end
                        Ops(end+1) = {'flip k'};
                    end
                    if NVal>1
                        Ops(end+1)={'magnitude'};
                    else
                        Ops(end+1:end+2)={'vector(A,B)','vec_M&D(A,B)'};
                    end
            end
            cmdargs={cmd Str{i}};
        end
        iop=get(Handle_OpList,'Value');
        strop=get(Handle_OpList,'String');
        strop=strop{iop};
        iop=strmatch(strop,Ops,'exact');
        if isempty(iop)
            iop=1;
        end
        if isempty(Ops)
            set(Handle_OpList,'Enable','off','Value',iop,'String',{' '},'backgroundcolor',Inactive);
        else
            set(Handle_OpList,'Enable','on','Value',iop,'String',Ops,'backgroundcolor',Active);
        end
        options(FI,mfig,'selectoperator');

    case 'selectoperator'
        Handle_DefVar=findobj(mfig,'tag','defvariable');
        set(Handle_DefVar,'enable','on')

        Handle_VarList=findobj(mfig,'tag','varlist');
        Vars=get(Handle_VarList,'userdata');
        Str=get(Handle_VarList,'string');
        i=get(Handle_VarList,'value');

        Handle_OpList=findobj(mfig,'tag','operatorlist');
        Ops=get(Handle_OpList,'string');
        k=get(Handle_OpList,'value');
        if nargin>3
            kArg=varargin{1};
            if ischar(kArg)
                kArg=strmatch(kArg,Ops,'exact');
                if ~isequal(size(kArg),[1 1])
                    return;
                end
            end
            k=kArg;
            set(Handle_OpList,'value',k)
        end

        Handle_VarList2=findobj(mfig,'tag','varlist2');
        Str2=get(Handle_VarList2,'string');
        j=get(Handle_VarList2,'value');

        Handle_ConstTxt=findobj(mfig,'tag','constant');
        set(Handle_ConstTxt,'enable','off')
        Handle_Const=findobj(mfig,'tag','constant=?');
        set(Handle_Const,'enable','off','backgroundcolor',Inactive)

        Handle_CondTxt=findobj(mfig,'tag','condition');
        set(Handle_CondTxt,'enable','off')
        Handle_Cond=findobj(mfig,'tag','condition=?');
        set(Handle_Cond,'enable','off','backgroundcolor',Inactive)

        if isempty(Str) | isequal(Ops{k},' ')
            set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
            set(Handle_DefVar,'enable','off')
        else
            Str2Sel=Str2{j};
            NVali=Vars(i).Props.NVal;
            for ii=1:length(Vars)
                SZ{ii}=getsize(FI,FI(ii));
            end
            SZi=SZ{i};
            jj=logical(ones(length(Vars),1));
            switch Ops{k}
                case {'A+B','A-B','A*B','A/B','max(A,B)','min(A,B)','vector(A,B)','vec_M&D(A,B)','A under condition B'}
                    NValMismatchNotAllowed=ismember(Ops{k},{'vector(A,B)','vec_M&D(A,B)'});
                    NValBMustEqual1=ismember(Ops{k},{'A under condition B'});
                    for ii=1:length(Vars)
                        if (NValBMustEqual1 & Vars(ii).Props.NVal~=1)
                            jj(ii)=0;
                        elseif (Vars(ii).Props.NVal~=NVali) & (NValMismatchNotAllowed | ...
                                ~((Vars(ii).Props.NVal==1) | (NVali==1 & Vars(ii).Props.NVal>0)))
                            jj(ii)=0;
                        elseif ~isequal(SZ{ii}(T_),SZi(T_)) & ~isequal(SZ{ii}(T_),1) & FI(ii).DimFlag(T_) & ~isequal(1,SZi(T_)) & FI(i).DimFlag(T_)
                            jj(ii)=0;
                        elseif any(SZ{ii}([M_ N_ K_])~=SZi([M_ N_ K_]) & SZ{ii}([M_ N_ K_])~=1 & SZi([M_ N_ K_])~=1)
                            jj(ii)=0;
                        elseif any(SZ{ii}([M_ N_ K_])~=SZi([M_ N_ K_]) & SZ{ii}([M_ N_ K_])~=1 & SZi([M_ N_ K_])==1) & ...
                                any(SZ{ii}([M_ N_ K_])~=SZi([M_ N_ K_]) & SZ{ii}([M_ N_ K_])==1 & SZi([M_ N_ K_])~=1)
                            jj(ii)=0;
                        end
                    end
                    Str2=Str(jj);
                    if isempty(Str2)
                        set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                        set(Handle_DefVar,'enable','off')
                        if strcmp(Ops{k},'A under condition B')
                            set(Handle_CondTxt,'enable','off');
                            set(Handle_Cond,'enable','off','backgroundcolor',Inactive);
                        end
                    else
                        j=strmatch(Str2Sel,Str2,'exact');
                        if isempty(j)
                            j=1;
                        elseif ~isequal(size(j),[1 1])
                            j=j(1);
                        end
                        jj=find(jj);
                        set(Handle_VarList2,'enable','on','value',j,'string',Str2,'backgroundcolor',Active,'userdata',jj);
                        if strcmp(Ops{k},'A under condition B')
                            set(Handle_CondTxt,'enable','on');
                            set(Handle_Cond,'enable','on','backgroundcolor',Active);
                            if isempty(get(Handle_Cond,'string'))
                                set(Handle_DefVar,'enable','off')
                            end
                        end
                    end
                case {'series: A,B'}
                    for ii=1:length(Vars)
                        if Vars(ii).Props.NVal~=NVali
                            jj(ii)=0;
                        elseif ~isequal(SZ{ii}([M_ N_ K_]),SZi([M_ N_ K_]))
                            jj(ii)=0;
                        end
                    end
                    Str2=Str(jj);
                    if isempty(Str2)
                        set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                        set(Handle_DefVar,'enable','off')
                    else
                        j=strmatch(Str2Sel,Str2,'exact');
                        if isempty(j)
                            j=1;
                        elseif ~isequal(size(j),[1 1])
                            j=j(1);
                        end
                        jj=find(jj);
                        set(Handle_VarList2,'enable','on','value',j,'string',Str2,'backgroundcolor',Active,'userdata',jj);
                    end
                case {'+ constant','* constant','^ constant','max(A,constant)','min(A,constant)'}
                    set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                    set(Handle_ConstTxt,'enable','on');
                    set(Handle_Const,'enable','on','backgroundcolor',Active);
                    set(Handle_DefVar,'enable','on')
                case {'magnitude','abs','10log','max m','alg.mean m','min m','sum m','max n','alg.mean n','min n','sum n','max k','alg.mean k','min k','sum k','flip m','flip n','flip k'}
                    set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                otherwise
                    set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                    set(Handle_DefVar,'enable','off')
            end
            cmdargs={cmd Ops{k}};
        end

    case 'selectvar2'
        Handle_VarList2=findobj(mfig,'tag','varlist2');
        Str2=get(Handle_VarList2,'string');
        j=get(Handle_VarList2,'value');
        if nargin>3
            jStr=varargin{1};
            if ischar(jStr)
                jStr=strmatch(jStr,Str2,'exact');
                if ~isequal(size(jStr),[1 1])
                    return
                end
            end
            j=jStr;
            set(Handle_VarList2,'value',j)
        end
        cmdargs={cmd Str2{j}};

    case 'const'
        Handle_ConstTxt=findobj(mfig,'tag','constant');
        Handle_Const=findobj(mfig,'tag','constant=?');
        c=get(Handle_Const,'userdata');
        cstr=get(Handle_Const,'string');
        cnew=str2num(cstr);
        if nargin>3
            cnewArg=varargin{1};
            if ischar(cnewArg)
                cnewArg=str2num(cnewArg);
            end
            if ~isequal(size(cnewArg),[1 1])
                return
            end
            cnew=cnewArg;
        end
        if isempty(cnew)
            set(Handle_Const,'string',num2str(cnew));
        else
            if ~isequal(size(cnew),[1 1])
                cnew=cnew(1);
            end
            set(Handle_Const,'string',num2str(cnew),'userdata',cnew);
        end
        cmdargs={cmd cnew};

    case 'condition'
        Handle_CondTxt=findobj(mfig,'tag','condition');
        Handle_Cond=findobj(mfig,'tag','condition=?');
        c=get(Handle_Cond,'userdata');
        Str=get(Handle_Cond,'string');
        Str0=Str;
        if nargin>3
            Str=varargin{1};
        end
        %
        lasterr('');
        try
            if ischar(Str)
                [c,Str]=realset(Str);
            else
                c=Str;
                Str=realset(c);
            end
        catch
            ui_message('error',{'Catch in d3d_qp\clippingvals',lasterr})
            c=get(Handle_Cond,'userdata');
            if isstruct(c)
                Str=realset(c);
            else
                Str=vec2str(c,'noones','nobrackets');
            end
        end
        %
        set(Handle_Cond,'string',Str,'userdata',c)
        Handle_DefVar=findobj(mfig,'tag','defvariable');
        if isempty(Str)
            set(Handle_DefVar,'enable','off')
        else
            set(Handle_DefVar,'enable','on')
        end

    case 'defvariable'
        Handle_VarList=findobj(mfig,'tag','varlist');
        Vars=get(Handle_VarList,'userdata');
        Strs=get(Handle_VarList,'string');
        i=get(Handle_VarList,'value');

        Handle_OpList=findobj(mfig,'tag','operatorlist');
        Ops=get(Handle_OpList,'string');
        k=get(Handle_OpList,'value');

        Handle_VarList2=findobj(mfig,'tag','varlist2');
        jj=get(Handle_VarList2,'userdata');
        j=get(Handle_VarList2,'value');

        Handle_ConstTxt=findobj(mfig,'tag','constant');
        Handle_Const=findobj(mfig,'tag','constant=?');
        c=get(Handle_Const,'userdata');

        Handle_CondTxt=findobj(mfig,'tag','condition');
        Handle_Cond=findobj(mfig,'tag','condition=?');
        cond=get(Handle_Cond,'userdata');

        Props.Tri=Vars(i).Tri;
        switch Ops{k}
            case {'series: A,B'}
                ii=jj(j);
                VarName=sprintf('%s,%s',Vars(i).Name,Vars(ii).Name);
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'vector(A,B)'}
                ii=jj(j);
                VarName=sprintf('vector(%s,%s)',Vars(i).Name,Vars(ii).Name);
                Props.NVal=2;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'vec_M&D(A,B)'}
                ii=jj(j);
                VarName=sprintf('vector(mag=%s,dir=%s)',Vars(i).Name,Vars(ii).Name);
                Props.NVal=2;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'A+B','A-B','A*B','A/B'}
                ii=jj(j);
                VarName=sprintf('(%s) %s (%s)',Vars(i).Name,Ops{k}(2),Vars(ii).Name);
                Props.NVal=max(Vars(i).Props.NVal,Vars(ii).Props.NVal);
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'max(A,B)','min(A,B)'}
                ii=jj(j);
                VarName=sprintf('%s(%s,%s)',Ops{k}(1:3),Vars(i).Name,Vars(ii).Name);
                Props.NVal=max(Vars(i).Props.NVal,Vars(ii).Props.NVal);
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'+ constant','* constant','^ constant'}
                VarName=sprintf('(%s) %s %g',Vars(i).Name,Ops{k}(1),c);
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) c};
                Props.DataInCell = Vars(i).DataInCell;
            case {'max(A,constant)','min(A,constant)'}
                VarName=sprintf('%s(%s,%g)',Ops{k}(1:3),Vars(i).Name,c);
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) c};
                Props.DataInCell = Vars(i).DataInCell;
            case {'10log','abs','max m','alg.mean m','min m','max n','alg.mean n','min n','max k','alg.mean k','min k','sum k','sum m','sum n','flip m','flip n','flip k'}
                VarName=sprintf('%s(%s)',Ops{k},Vars(i).Name);
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i)};
                Props.DataInCell = Vars(i).DataInCell;
            case {'magnitude'}
                VarName=sprintf('%s(%s)',Ops{k},Vars(i).Name);
                Props.NVal=1;
                Props.Oper=Ops{k};
                Props.Data={Vars(i)};
                Props.DataInCell = Vars(i).DataInCell;
            case {'A under condition B'}
                ii=jj(j);
                VarName=sprintf('%s if %s in {%s}',Vars(i).Name,Vars(ii).Name,realset(cond));
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii) cond};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            otherwise
                return
        end
        if nargin>3
            VarName=varargin{1};
            accept=isempty(strmatch(VarName,Strs,'exact'));
        else % interactive
            accept=0;
        end
        prompt={'Name of variable:'};
        def={VarName};
        dlgTitle='Specify unique name of variable';
        lineNo=1;
        if ~isempty(strmatch(VarName,Strs,'exact'))
            prompt={'Name of variable (current name not unique):'};
        end
        while ~accept
            answer=stdinputdlg(prompt,dlgTitle,lineNo,def);
            if isempty(answer)
                break
            end
            VarName=answer{1};
            accept=isempty(strmatch(VarName,Strs,'exact'));
        end
        if accept
            ii=length(Vars)+1;
            Vars(ii).Name=VarName;
            Vars(ii).FileInfo='operator';
            Props.Name=VarName;
            Vars(ii).Props=Props;
            Vars(ii).Selected=[];
            Vars(ii).DataInCell=Props.DataInCell;
            Vars(ii).DimFlag=[0 0 0 0 0];
            for d = 1:5
                dflag = 0;
                for k = 1:length(Props.Data)
                    if dflag == 0 & ~isnumeric(Props.Data{k}) & isfield(Props.Data{k},'DimFlag')
                        dflag = Props.Data{k}.DimFlag(d);
                    end
                end
                Vars(ii).DimFlag(d) = dflag;
            end
            Vars(ii).Tri=Props.Tri;
            set(Handle_VarList,'userdata',Vars);
            Str={};
            for i=1:length(Vars)
                Str{i}=Vars(i).Name;
            end
            set(Handle_VarList,'userdata',Vars,'string',Str,'value',length(Vars));
            options(Vars,mfig,'selectvar');
            NewFI=Vars;
            cmdargs={cmd VarName};
        end

    otherwise
        fprintf('Unknown option command: %s\n',cmd)
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function OK=optfig(h0);
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
voffset=FigPos(4)-30;
%
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions delvar', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'String','Delete Variable', ...
    'Tag','delvar');
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selectvar', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',' ', ...
    'Style','popupmenu', ...
    'Tag','varlist', ...
    'Value',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selectoperator', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',{' '}, ...
    'Style','popupmenu', ...
    'Tag','operatorlist', ...
    'Value',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selectvar2', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',{' '}, ...
    'Style','popupmenu', ...
    'Tag','varlist2', ...
    'Value',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 150 20], ...
    'HorizontalAlignment','left', ...
    'String','Constant', ...
    'Style','text', ...
    'Tag','constant');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions const', ...
    'Enable','off', ...
    'Position',[171 voffset 160 20], ...
    'HorizontalAlignment','right', ...
    'String','1', ...
    'Style','edit', ...
    'Tag','constant=?', ...
    'UserData',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 150 20], ...
    'HorizontalAlignment','left', ...
    'String','Condition', ...
    'Style','text', ...
    'Tag','condition');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions condition', ...
    'Enable','off', ...
    'Position',[171 voffset 160 20], ...
    'HorizontalAlignment','left', ...
    'String',' ', ...
    'Style','edit', ...
    'Tag','condition=?', ...
    'UserData',[]);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions defvariable', ...
    'Enable','off', ...
    'Position',[171 voffset 160 20], ...
    'String','Define Variable', ...
    'Tag','defvariable');
OK=1;
% -----------------------------------------------------------------------------

