function varargout=difffil(FI,domain,field,cmd,varargin)
%DIFFFIL QP support for file differences.
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
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

[success,Ans,FI(1)] = qp_getdata(FI(1),Props.Q1,cmd,varargin{:});
if ~success
    error(lasterr)
end

if Props.NVal>0
    cmd = strrep(cmd,'grid','');
    [success,Data2,FI(2)] = qp_getdata(FI(2),Props.Q2,cmd,varargin{:});
    if ~success
        error(lasterr)
    end
    %
    fld = {'Val','XComp','YComp','ZComp','XDamVal','YDamVal'};
    for i=1:length(fld)
        if isfield(Data2,fld{i})
            v1 = getfield(Ans,fld{i});
            v2 = getfield(Data2,fld{i});
            v1 = v1-v2;
            Ans = setfield(Ans,fld{i},v1);
        end
    end
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Domains=domains(FI);
[success,D1] = qp_getdata(FI(1),'domains');
[success,D2] = qp_getdata(FI(2),'domains');
if ~isempty(D1) || ~isempty(D2)
    error('Support for domains not yet implemented.')
end
Domains = {};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
[success,Q1] = qp_getdata(FI(1));
if ~success
    error(lasterr)
end
[success,Q2] = qp_getdata(FI(2));
if ~success
    error(lasterr)
end
%
Out = cell2struct(cell(9,length(Q1)),{'Name','Units','DimFlag','DataInCell','NVal','Q1','Q2','Size','SubFld'});
q2 = {Q2.Name};
j = 0;
for i=1:length(Q1)
    if strcmp(Q1(i).Name,'-------')
        j=j+1;
        Out(j).Name = Q1(i).Name;
        Out(j).Units = '';
        Out(j).DimFlag = [0 0 0 0 0];
        Out(j).DataInCell = 0;
        Out(j).NVal = 0;
        Out(j).Q1 = [];
        Out(j).Q2 = [];
        Out(j).Size = [];
        Out(j).SubFld = {};
        continue
    end
    if Q1(i).NVal<0
        % cannot diff dedicated plots
        continue
    end
    i2 = strmatch(Q1(i).Name,q2,'exact');
    if isempty(i2)
        % no match found
        continue
    end
    [success,sz] = qp_getdata(FI(1),Q1(i),'size');
    if ~success
        error(lasterr)
    end
    [success,sf] = qp_getdata(FI(1),Q1(i),'subfields');
    if ~success
        error(lasterr)
    end
    for k=length(i2):-1:1
        if ~isequal(Q1(i).NVal,Q2(i2(k)).NVal)
            % cannot take difference of scalar and vector or similar
            % mismatches.
            i2(k)=[];
        elseif ~isequal(Q1(i).DimFlag,Q2(i2(k)).DimFlag)
            % the dimension should match, although a mismatch in time
            % dependence and stations might be acceptable in the future.
            i2(k)=[];
        else
            [success,sz2] = qp_getdata(FI(2),Q2(i2(k)),'size');
            if ~success
                error(lasterr)
            end
            if ~isequal(sz,sz2)
                % allow for different number of time steps, stations in the
                % future?
                i2(k)=[];
            else
                [success,sf2] = qp_getdata(FI(2),Q2(i2(k)),'subfields');
                if ~success
                    error(lasterr)
                end
                if ~isequal(sf,sf2)
                    % might be acceptable in the future to select matching
                    % subfields, e.g. corresponding sediment fractions.
                    i2(k)=[];
                end
            end
        end
    end
    if length(i2)~=1
        % unable to uniquely identify the corresponding quantity
        continue
    end
    %
    NewFld.Name = Q1(i).Name;
    NewFld.Units = Q1(i).Units;
    if ~isequal(NewFld.Units,Q2(i2).Units)
        % might be able to relax this to compatible units later.
        continue
    end
    NewFld.DimFlag = Q1(i).DimFlag;
    NewFld.DataInCell = Q1(i).DataInCell;
    if ~isequal(NewFld.DataInCell,Q2(i2).DataInCell)
        % should not be too restrictive
        continue
    end
    NewFld.NVal = Q1(i).NVal;
    if NewFld.NVal~=0
        NewFld.Name = [NewFld.Name ' difference'];
    end
    NewFld.Q1 = Q1(i);
    NewFld.Q2 = Q2(i2);
    NewFld.Size = sz;
    NewFld.SubFld = sf;
    %
    j = j+1;
    Out(j) = NewFld;
end
Out(j+1:end)=[];
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf = Props.SubFld;
if nargin>2
    subf = subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
sz = Props.Size;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
TimeNr = {};
if nargin>2
    TimeNr = {t};
end
[success,T] = qp_getdata(FI(1),Props.Q1,'times',TimeNr{:});
if ~success
    error(lasterr)
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
StationNr = {};
if nargin>2
    StationNr = {t};
end
[success,S] = qp_getdata(FI(1),Props.Q1,'stations',StationNr{:});
if ~success
    error(lasterr)
end
% -----------------------------------------------------------------------------
