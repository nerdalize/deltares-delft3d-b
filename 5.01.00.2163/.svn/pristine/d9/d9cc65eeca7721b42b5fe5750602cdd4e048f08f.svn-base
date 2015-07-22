function varargout=qp_unitconversion(unit1,unit2,varargin)
%QP_UNITCONVERSION Convert unit strings.
%   SystemList = QP_UNITCONVERSION('systems') returns the unit systems
%   supported. Currently these are SI, CGS, FPS, IPS, NMM.
%
%   UnitList = QP_UNITCONVERSION('units') returns a list of all supported
%   elementary unit strings. Unit strings may be combined of any
%   combination of units. E.g. 'km/h', 'ft/s', 'N*s/kg'.
%
%   QP_UNITCONVERSION(UStr1,UStr2) displays a conversion table for
%   transforming quantities expressed in UStr1 into UStr2 and vice versa.
%
%   ConversionFactor = QP_UNITCONVERSION(UStr,UStr2) returns the factor
%   needed for the conversion of quantities expressed in UStr1 into UStr2.
%
%   QP_UNITCONVERSION(UStr1,System) displays a conversion table for
%   transforming quantities expressed in UStr1 into the equivalent in the
%   selected unit system and vice versa.
%
%   [ConversionFactor,UStr2] = QP_UNITCONVERSION(UStr1,System) returns the
%   factor needed for the conversion of quantities expressed in unit1 into
%   the equivalent in the selected unit system and returns the unit string
%   UStr2 in that system as well.
%
%   DATA2 = QP_UNITCONVERSION(UStr1,UStr2,DATA1) converts the data provided by
%   DATA1 in UStr1 unit into UStr2 units.
%
%   Note: The current support for temperature concerns relative
%   temperatures only, i.e. 5 degrees celsius will be converted to 5
%   degrees kelvin. This is correct for temperature differences but not for
%   absolute temperatures.

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

persistent unittableread
if isempty(unittableread)
    initialize_unittable;
    unittableread=1;
end

unitsystems={'SI','CGS','FPS','IPS','NMM'};
if nargout>0
    varargout=cell(1,nargout);
end
Out=[];
if nargin==1
    unit2='SI';
end

if isempty(unit1)
    unit1='-';
end
if nargin==1 && strcmpi(unit1,'systems')
    varargout={unitsystems};
elseif nargin==1 && strcmpi(unit1,'units')
    varargout=search_elem('retrievetable');
else
    if isempty(unit2)
        unit2='-';
    end
    if isequal(unit1,unit2)
        convfactor=[0 1];
        if nargin<=2
            varargout{1}=convfactor(2); % maintain temporarily backward compatibility
            varargout{2}=unit2;
        else
            varargout=varargin;
        end
    else
        if isequal(unit1,'-')
            factor1=[0 1];
            SI1=zeros(1,8);
        else
            [factor1,SI1]=factor2si(unit1);
        end
        if ischar(factor1)
            Out=factor1;
            SI2=NaN;
        else
            if isequal(unit2,'-')
                factor2=[0 1];
                SI2=zeros(1,8);
            elseif ~isempty(strmatch(unit2,unitsystems,'exact'))
                SI2=SI1;
                SIUnits={'m','s','A','kg','cd','mol','degK','deg'};
                switch unit2
                    case 'SI'
                        Units=SIUnits;
                    case 'CGS'
                        Units={'cm','s','A','g','cd','mol','degK','deg'};
                    case 'FPS'
                        Units={'ft','s','A','lb','cd','mol','degK','deg'};
                    case 'IPS'
                        Units={'in','s','A','lb','cd','mol','degK','deg'};
                    case 'NMM'
                        Units={'mm','s','A','g','cd','mol','degK','deg'};
                    otherwise
                        error('Unit system %s not yet implemented.',unit2)
                end
                factor2=[0 1];
                if ~strcmp(unit2,'SI')
                    for i=1:length(Units)
                        sifactor = search_elem(Units{i});
                        factor2(2)=factor2(2)*sifactor(2)^SI2(i);
                    end
                end
                unit2=dispunit(SI2,Units);
                if isempty(unit2)
                    unit2='-';
                end
            else
                [factor2,SI2]=factor2si(unit2);
            end
            if ischar(factor2)
                Out=factor2;
            end
        end
        if ~isempty(Out)
            varargout={Out ''};
        elseif ~isequal(SI1,SI2)
            SI1unit=dispunit(SI1);
            if isempty(SI1unit)
                SI1unit='-';
            end
            if ~isequal(unit1,SI1unit)
                unit1=sprintf('%s (in SI base units: %s)',unit1,SI1unit);
            end
            SI2unit=dispunit(SI2);
            if isempty(SI2unit)
                SI2unit='-';
            end
            if ~isequal(unit2,SI2unit)
                unit2=sprintf('%s (in SI base units: %s)',unit2,SI2unit);
            end
            varargout={sprintf('Incompatible units: %s cannot be converted into %s.',unit1,unit2) ''};
        else
            convfactor=factor1(2)/factor2(2);
            offset=0;
            if factor1(1)~=0 || factor2(1)~=0
                offset=factor1(1)-factor2(1)/convfactor;
            end
            if nargin<=2
                if nargout==0
                    cf='';
                    if convfactor~=1
                        cf=sprintf('%g ',convfactor);
                    end
                    xunit1=sprintf('[quantity in %s]',unit1);
                    absval=[];
                    if offset~=0
                        sign='+';
                        dispoffset=offset;
                        if dispoffset<0
                            sign='-';
                            dispoffset=-dispoffset;
                        end
                        xunit1=sprintf('%s %c %g',xunit1,sign,dispoffset);
                        if ~isempty(cf)
                            xunit1=['(' xunit1 ')'];
                        end
                        absval=[0 -offset];
                    end
                    fprintf('[quantity in %s] = %s%s\n',unit2,cf,xunit1);
                    for f=sort([absval 1 2 50 100 [1 2 50 100]/convfactor-offset])
                        fprintf('%10g %s = %10g %s\n',f,unit1,convfactor*(f+offset),unit2)
                    end
                else
                    varargout={convfactor unit2};
                end
            else
                for i=1:length(varargin)
                    if isstruct(varargin{i})
                        data=varargin{i};
                        flds={'Val','XComp','YComp','ZComp'};
                        for fldi=1:length(flds)
                            fld=flds{fldi};
                            if isfield(data,fld)
                                for d=1:length(data)
                                    Temp=convfactor*(getfield(data(d),fld)+offset);
                                    data(d)=setfield(data(d),fld,Temp);
                                end
                            end
                        end
                        [data(:).Units]=deal(unit2);
                        varargout{i}=data;
                    else
                        varargout{i}=convfactor*(varargin{i}+offset);
                    end
                end
            end
        end
    end
end


function Str=dispunit(SI,Units)
if nargin==1
    Units={'m','s','A','kg','cd','mol','degK','deg'};
end
Str='';
if all(SI==0)
    Str='';
    return
end
for i=1:length(SI)
    if SI(i)>0
        if SI(i)==1
            Str=[Str '*' Units{i}];
        else
            Str=[Str '*' Units{i} '^' num2str(SI(i))];
        end
    end
end
if isempty(Str)
    Str=' 1';
end
for i=1:length(SI)
    if SI(i)<0
        if SI(i)==-1
            Str=[Str '/' Units{i}];
        else
            Str=[Str '/' Units{i} '^' num2str(-SI(i))];
        end
    end
end
if ~isempty(Str)
    Str=Str(2:end);
end


function [factor,si]=factor2si(unit)
[factor,si]=factor2si_multiply_divide(unit);


function [factor,si]=factor2si_multiply_divide(unit)
factor=[0 1];
si=zeros(1,8);

nob=0;
prevcmd='*';
ki=1;
if isempty(unit)
    k=0;
else
    k=1;
    while k<=length(unit)
        switch unit(k)
            case '('
                nob=nob+1;
            case ')'
                nob=nob-1;
                if nob<0
                    break
                end
            case {'*','/'}
                if isequal(unit(k),'*') && k<length(unit) && isequal(unit(k+1),'*')
                    % two asteriks form a power operator instead of a multiplication
                    % operator.
                    unit = unit([1:k k+2:end]);
                    unit(k) = '^';
                elseif nob==0
                    [factor1,si1]=factor2si_power(unit(ki:k-1));
                    if ischar(factor1)
                        factor=factor1;
                        return
                    end
                    if factor(1)~=0 || factor1(1)~=0
                        error('multiplying offset')
                    end
                    switch prevcmd
                        case '*'
                            factor(2)=factor(2)*factor1(2);
                            si=si+si1;
                        case '/'
                            factor(2)=factor(2)/factor1(2);
                            si=si-si1;
                    end
                    prevcmd=unit(k);
                    ki=k+1;
                end
            otherwise
        end
        if k==length(unit)
            break
        else
            k=k+1;
        end
    end
end
if nob>0
    factor=['no matching bracket found: ',unit(ki:k)];
    return
elseif nob<0
    factor='closing bracket preceding opening bracket';
    return
else
    [factor1,si1]=factor2si_power(unit(ki:k));
    if ischar(factor1)
        factor=factor1;
        return
    end
    if ki==1
        factor=factor1;
        si=si1;
    else
        if factor(1)~=0 || factor1(1)~=0
            error('multiplying offset')
        end
        switch prevcmd
            case '*'
                factor(2)=factor(2)*factor1(2);
                si=si+si1;
            case '/'
                factor(2)=factor(2)/factor1(2);
                si=si-si1;
        end
    end
end


function [factor,si]=factor2si_power(unit)
factor='unknown error';
si='';

nob=0;
cmd='';
for k=1:length(unit)
    switch unit(k)
        case '('
            nob=nob+1;
        case ')'
            nob=nob-1;
            if nob<0
                break
            end
        case '^'
            if nob==0
                cmd='^';
                shift=1;
                break
            end
        case {'¹','²','³'}
            if nob==0
                cmd='^';
                shift=0;
                break
            end
        otherwise
    end
end
if nob>0
    factor=['no matching bracket found: ',unit(1:k)];
    return
elseif nob<0
    factor='closing bracket preceding opening bracket';
    return
elseif isequal(cmd,'^')
    [factor,si]=factor2si_brackets(unit(1:k-1));
    if ischar(factor)
        return
    end
    switch unit(k+shift)
        case '¹'
            pow=1;
        case '²'
            pow=2;
        case '³'
            pow=3;
        case '¼'
            pow = 0.25;
        case '½'
            pow = 0.5;
        case '¾'
            pow = 0.75;
        otherwise
            pow=str2num(unit(k+shift:end));
            if isempty(pow)
                factor=sprintf('Invalid exponent ''%s''.',unit(k+shift:end));
                return
            end
    end
    if factor(1)~=0
        error('power offset')
    end
    factor(2)=factor(2)^pow;
    si=si*pow;
else
    [factor,si]=factor2si_brackets(unit(1:end));
end


function [factor,si]=factor2si_brackets(unit)
factor='unknown error';
si='';
if isequal(unit(1),'(') && isequal(unit(end),')')
    [factor,si]=factor2si_multiply_divide(unit(2:end-1));
else
    [factor,si]=search_elem(unit);
end


function [factor,si]=search_elem(unit,newtable)
persistent unittable
factor=[0 1];
si='';
if nargin == 2
    unittable = newtable;
    return
elseif isequal(unit,'retrievetable')
    factor = unittable;
    return
end
i=strmatch(unit,unittable{1},'exact');
prefix=1;
if isempty(i)
    [v,n,e]=sscanf(unit,'%f',2);
    if n==1 && isempty(e)
        factor=[0 v];
        si=zeros(1,8);
    else
        prefixfound=0;
        pref='';
        if length(unit)>3
            j=strmatch(unit(1:3),{'exa'},'exact');
            if ~isempty(j)
                scale=1e18;
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:3);
                unit=unit(4:end);
            end
        end
        if ~prefixfound && length(unit)>4
            j=strmatch(unit(1:4),{'peta','tera','giga','mega','kilo','deka','deci','nano','pico','atto'},'exact');
            if ~isempty(j)
                scale=[1e15 1e12 1e9 1e6 1e3 10 1/10 1e-9 1e-12 1e-18];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:4);
                unit=unit(5:end);
            end
        end
        if ~prefixfound && length(unit)>5
            j=strmatch(unit(1:5),{'yotta','zetta','hecto','centi','milli','micro','femto','zepto','yocto'},'exact');
            if ~isempty(j)
                scale=[1e24 1e21 100 1/100 1e-3 1e-6 1e-15 1e-21 1e-24];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1:5);
                unit=unit(6:end);
            end
        end
        if ~prefixfound && length(unit)>1
            j=find(unit(1)=='YZEPTGMkhDdcmµunpfazy');
            if ~isempty(j)
                scale=[1e24 1e21 1e18 1e15 1e12 1e9 1e6 1e3 100 10 1e-1 1e-2 1e-3 1e-6 1e-6 1e-9 1e-12 1e-15 1e-18 1e-21 1e-24];
                prefixfound=1;
                prefix=scale(j);
                pref=unit(1);
                unit=unit(2:end);
            end
        end
        if prefixfound
            i=strmatch(unit,unittable{1},'exact');
        end
        absfound=0;
        if isempty(i) && length(unit)>5 && strcmpi(unit(end-4:end),' abs.')
            absfound=1;
            unit=unit(1:end-5);
            i=strmatch(unit,unittable{1},'exact');
        end
        if isempty(i)
            unit = [pref unit];
            %
            % Check for space in unit string ...
            %
            for k=1:length(unit)
                switch unit(k)
                    case ' '
                        [factor,si]=search_elem(unit(1:k-1));
                        if ischar(factor)
                            return
                        end
                        [factor1,si1]=search_elem(unit(k+1:end));
                        if ischar(factor1)
                            factor=factor1;
                            return
                        end
                        if factor(1)~=0 || factor1(1)~=0
                            error('multiplying offset')
                        end
                        factor(2)=factor(2)*factor1(2);
                        si=si+si1;
                        return
                end
            end
            %
            % Check for number at end of string ...
            %
            number = ismember(unit,'1234567890');
            ki = max(find(~number));
            if ki<length(unit)
                if unit(ki)=='-'
                    pow = str2num(unit(ki:end));
                    [factor,si]=search_elem(unit(1:ki-1));
                else
                    pow = str2num(unit(ki+1:end));
                    [factor,si]=search_elem(unit(1:ki));
                end
                if ischar(factor)
                    return
                elseif factor(1)~=0
                    error('power offset')
                end
                factor(2)=factor(2)^pow;
                si=si*pow;
                return
            end
            factor=['Unit definition not found: ',unit];
        else
            i=unittable{2}(i);
            factor=[0 unittable{3}(i,2)*prefix];
            if absfound
                factor(1)=unittable{3}(i,1);
            end
            si=unittable{4}(i,:);
        end
    end
else
    i=unittable{2}(i);
    factor=[0 unittable{3}(i,2)];
    si=unittable{4}(i,:);
end


function initialize_unittable
table=basisunittable;
search_elem('newtable',table);
%
filename=[qp_basedir('exe') filesep 'units.ini'];
fid=fopen(filename,'r');
if fid<0
    % for ease of debugging ...
    filename='units.ini';
    fid=fopen(filename,'r');
    if fid<0
        ui_message('error','Unit conversion table not found.')
        return
    end
end
fclose(fid);
%
UNIT=inifile('open',filename);
units=inifile('chapters',UNIT);
identified=zeros(1,length(units));
nNames=size(table{1},1);
anychange=1;
while anychange && any(~identified)
    anychange=0;
    i_notidentified=find(~identified);
    for i=i_notidentified
        j=size(table{4},1);
        Def=inifile('get',UNIT,i,'definition','');
        Names=inifile('get',UNIT,i,'name','');
        if isempty(Names)
            identified(i)=1;
            if ~strcmpi(units{i},'general')
                ui_message('', ...
                    'No names in chapter #%i: %s', ...
                    i,units{i})
            end
        elseif isempty(Def)
            identified(i)=1;
            ui_message('', ...
                'No definition for %s in chapter #%i: %s', ...
                list2string(Names),i,units{i})
        else
            if ~iscell(Names)
                Names={Names};
            end
            if ischar(Def)
                [factor,si]=factor2si(Def);
            else
                factor(2)=Def;
                si=zeros(1,8);
            end
            if ~ischar(factor)
                j=j+1;
                nNewNames=length(Names);
                n=1;
                while n<nNewNames
                    ii=strmatch(Names{n},table{1},'exact');
                    if ~isempty(ii)
                        ii=table{2}(ii);
                        ui_message('', ...
                            '%s defined multiple times, using definition: %g %s', ...
                            Names{n},table{3}(ii,2),dispunit(table{4}(ii,:)))
                        Names(n)=[];
                        nNewNames=nNewNames-1;
                    else
                        n=n+1;
                    end
                end
                identified(i)=1;
                anychange=1;
                factor(1)=inifile('get',UNIT,i,'absoffset',0);
                table{3}(j,:)=factor;
                table{4}(j,:)=si;
                table{1}(nNames+(1:nNewNames))=Names;
                table{2}(nNames+(1:nNewNames))=j;
                nNames=nNames+nNewNames;
            end
        end
        search_elem('newtable',table);
    end
end
if any(~identified)
    i_notidentified=find(~identified);
    for i=i_notidentified
        Def=inifile('get',UNIT,i,'definition','');
        Names=inifile('get',UNIT,i,'name','');
        if ~iscell(Names)
            Names={Names};
        end
        ui_message('', ...
            'Cannot understand definition ''%s''\ntherefore the definition of %s has been cancelled.', ...
            Def,list2string(Names))
    end
end


function strNames=list2string(Names)
if ~iscell(Names)
    strNames=Names;
    return
end
if length(Names)>1
    strNames=sprintf('%s, ',Names{1:end-1});
    strNames(end-1:end)=[];
    strNames=sprintf('%s and %s',strNames,Names{end});
else
    strNames=Names{1};
end


function table=basisunittable
table={'m'   [0 1]          [1 0 0 0 0 0 0 0]
    'ft'     [0 0.3048]     [1 0 0 0 0 0 0 0]
    'in'     [0 0.3048/12]  [1 0 0 0 0 0 0 0]
    's'      [0 1]          [0 1 0 0 0 0 0 0]
    'A'      [0 1]          [0 0 1 0 0 0 0 0]
    'g'      [0 0.001]      [0 0 0 1 0 0 0 0]
    'lb'     [0 0.45359237] [0 0 0 1 0 0 0 0]
    'cd'     [0 1]          [0 0 0 0 1 0 0 0]
    'mol'    [0 1]          [0 0 0 0 0 1 0 0]
    'degK'   [0 1]          [0 0 0 0 0 0 1 0]
    'deg'    [0 1]          [0 0 0 0 0 0 0 1]
    'radian' [0 180/pi]     [0 0 0 0 0 0 0 1]};
table={table(:,1) (1:size(table,1))' cat(1,table{:,2}) cat(1,table{:,3})};
