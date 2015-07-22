function varargout=sobekfil(FI,domain,field,cmd,varargin)
%SOBEKFIL QP support for SOBEK-RE and Rural/Urban/River network and output files.
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
        Parent=varargin{1};
        Ops=varargin{2};
        switch Props.Name
            case 'network*'
                hNew=sobek('plot',FI);
                recolor(hNew,'b',Ops.colour)
        end
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 [] []};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;
sz=getsize(FI,Props);
if isempty(idx{T_})
    idx{T_}=sz(T_);
end
if idx{M_}==0
    idx{M_}=1:sz(M_);
end
if idx{ST_}==0
    idx{ST_}=1:sz(ST_);
end

if Props.DFil==-1
    inodes=strmatch(strtok(Props.Name),FI.Node.Type);
    inodes=inodes(idx{ST_});
    Ans.XY=FI.Node.XY(inodes,:);
    Ans.SEG=zeros(0,2);
elseif Props.DFil==-2
    Ans.XY=(FI.Branch.XYXY(idx{ST_},1:2)+FI.Branch.XYXY(idx{ST_},3:4))/2;
    Ans.SEG=zeros(0,2);
elseif strcmp(FI.FileType,'SOBEK River network')
    if strcmp(Props.Name,'network (nodes only)')
        Ans.XY=FI.Node.XY;
        Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
    else
        Ans.XY=cat(1,FI.Branch.XY{FI.Grid.IBranch});
        BrLen=cellfun('size',FI.Branch.XY,1)-1;
        SEG=zeros(sum(BrLen),2);
        offset=0;
        Noffset=0;
        for i=FI.Grid.IBranch%1:length(BrLen)
            BL=BrLen(i);
            SEG(offset+(1:BL),1)=Noffset+(1:BL)';
            offset=offset+BL;
            Noffset=Noffset+BL+1;
        end
        SEG(:,2)=SEG(:,1)+1;
        Ans.SEG=SEG;
    end
else
    Ans.XY=FI.Node.XY;
    if Props.NVal==0
        Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
        Renum=zeros(sz(M_),1);
        Renum(idx{M_})=1:length(idx{M_});
        Ans.SEG=Renum(Ans.SEG);
        Ans.SEG(any(Ans.SEG==0,2),:)=[];
        Ans.XY=Ans.XY(idx{M_},:);
    else
        if Props.DataInCell
            HisFile=FI.Data{Props.DFil};
            [ID,BranchReorder,KnownBranches]=intersect(HisFile.SegmentName(idx{M_}),FI.Branch.ID);
            MissingBranches=1:FI.nBranches;
            MissingBranches(KnownBranches)=[];
            %
            MB=ismember(FI.Node.Type(FI.Branch.IFrom(MissingBranches)), ...
                {'SBK_PROFILE','SBK_LATERALFLOW','SBK_MEASSTAT','SBK_PUMP','SBK_WEIR'});
            ProfileBranches=MissingBranches(MB);
            OtherPart=zeros(1,length(ProfileBranches));
            %
            %    '3B_OPENWATER','3B_PAVED','3B_UNPAVED','3B_WEIR','SBK_SBK-3B-NODE'
            %
            %    'SBK_BOUNDARY','SBK_CHANNELCONNECTION','SBK_CONNECTIONNODE', ...
            %    'SBK_CHANNEL_STORCONN&LAT','SBK_CONN&LAT','SBK_GRIDPOINT'
            %
            %    'SBK_LATERALFLOW','SBK_MEASSTAT','SBK_PROFILE','SBK_PUMP','SBK_WEIR',
            %
            for i=1:length(ProfileBranches)
                Ref=find(FI.Branch.ITo(KnownBranches)==FI.Branch.IFrom(ProfileBranches(i)));
                if ~isempty(Ref)
                    OtherPart(i)=Ref;
                end
            end
            stimes=0;
            while any(OtherPart==0) & stimes<5
                stimes=stimes+1;
                NewBranches=ProfileBranches(OtherPart>0);
                for i=1:length(ProfileBranches)
                    Ref=find(FI.Branch.ITo(NewBranches)==FI.Branch.IFrom(ProfileBranches(i)));
                    if ~isempty(Ref)
                        OtherPart(i)=OtherPart(ProfileBranches==NewBranches(Ref));
                    end
                end
            end
            ProfileBranches(OtherPart==0)=[];
            OtherPart(OtherPart==0)=[];
            KnownBranches=cat(1,KnownBranches(:),ProfileBranches(:));
            ValueRef=cat(1,BranchReorder(:),BranchReorder(OtherPart));
            SEG=[FI.Branch.IFrom FI.Branch.ITo];
            Ans.SEG=SEG(KnownBranches,:);
        else
            HisFile=FI.Data{Props.DFil};
            [ID,NodeReorder,NodeIndex]=intersect(HisFile.SegmentName(idx{M_}),FI.Node.ID);
            Ans.XY=Ans.XY(NodeIndex,:);
        end
    end
end
%         l=patch(Network.Node.XY(NodeIndex,1)',Network.Node.XY(NodeIndex,2)',Val(NodeReorder), ...

if Props.DFil==-1
    Ans.Val=FI.Node.ID(inodes);
elseif Props.DFil==-2
    Ans.Val=FI.Branch.ID(idx{ST_});
elseif Props.NVal>0
    HisFile=FI.Data{Props.DFil};
    [t,Ans.Val]=delwaq('read',HisFile,Props.Subs,idx{M_},idx{T_});
    if idx{M_}==0
        Ans.Val=permute(Ans.Val,[3 2 1]);
    end
    if strcmp(FI.FileType,'SOBEK network')
        if Props.DataInCell
            Ans.Val=Ans.Val(:,ValueRef);
        else
            Ans.Val=Ans.Val(:,NodeReorder);
        end
    end
    Ans.Time=t;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                           'Geom'  'Coords' 'DimFlag' 'DataInCell' 'NVal' 'DFil' 'Subs' 'UseGrid'};
%          'network*'                             'SEG' 'xy' [0 0 4 0 0]   0          -1      0      0       0
DataProps={'network'                              'SEG' 'xy' [0 0 6 0 0]   0           0      0      0       1};
if strcmp(FI.FileType,'SOBEK River network')
    DataProps(2,:)=DataProps(1,:);
    DataProps{2,1}='network (nodes only)';
else
    nodetypes = unique(FI.Node.Type);
    for i=1:length(nodetypes)
        DataProps(i+1,:)=DataProps(1,:);
        DataProps{i+1,1}=[nodetypes{i} ' nodes'];
        DataProps{i+1,4}=[0 3 0 0 0];
        DataProps{i+1,6}=4;
        DataProps{i+1,7}=-1;
        DataProps{i+1,end}=0;
    end
    DataProps(end+1,:)=DataProps(1,:);
    DataProps{end,1}='reach segments';
    DataProps{end,4}=[0 3 0 0 0];
    DataProps{end,6}=4;
    DataProps{end,7}=-2;
    DataProps{end,end}=0;
end
for i=1:length(FI.Data)
    [pn,fn]=fileparts(FI.Data{i}.FileName);
    fn=lower(fn);
    DataProps(end+1,:)={'-------'                  ''    ''   [0 0 0 0 0]   0           0      0      0       0};
    for j=1:length(FI.Data{i}.SubsName)
        DataProps(end+1,:)={FI.Data{i}.SubsName{j}  'SEG' 'xy' [1 0 6 0 0]   0           1      i      j       0};
        if strcmp(fn,'reachseg')
            DataProps{end,5}=1; % DataInCell
        end
    end
end
Out=cell2struct(DataProps,PropNames,2);
%======================== SPECIFIC CODE REMOVE ================================

%======================== SPECIFIC CODE ADD ===================================

% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
if Props.DimFlag(T_)
    FIH=FI.Data{Props.DFil};
    sz(T_)=FIH.NTimes;
    sz(M_)=FIH.NumSegm;
else
    if strcmp(FI.FileType,'SOBEK River network')
        if strcmp(Props.Name,'network (nodes only)')
            sz(M_)=length(FI.Node.ID);
        else
            sz(M_)=sum(cellfun('size',FI.Branch.XY,1));
        end
    elseif Props.DFil==-1
        sz(ST_)=length(strmatch(strtok(Props.Name),FI.Node.Type));
    elseif Props.DFil==-2
        sz(ST_)=length(FI.Branch.ID);
    else
        sz(M_)=length(FI.Node.ID);
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [T,Tsc]=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
T=delwaq('read',FI.Data{Props.DFil},1,1,t);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props)
%======================== SPECIFIC CODE =======================================
switch Props.DFil
    case -1
        inodes=strmatch(strtok(Props.Name),FI.Node.Type);
        S=FI.Node.ID(inodes);
    case -2
        S=FI.Branch.ID;
        for i=1:length(S)
            ID = S{i};
            Name = FI.Branch.Name{i};
            if isempty(ID)
                S{i} = Name;
            elseif ~isempty(Name)
                S{i} = [ID ':' Name];
            end
        end
end
% -----------------------------------------------------------------------------
