function varargout=inifile(cmd,varargin)
%INIFILE Read/write INI files.
%   Info=INIFILE('open',FileName)
%   Open and read the INI file; return the data to the workspace in a
%   set of nested cell arrays.
%
%   Info=INIFILE('new')
%   Create a new INI file structure.
%
%   Info=INIFILE('write',FileName,Info)
%   Open and write the INI file; the data in the file is overwritten
%   without asking.
%
%   ListOfChapters=INIFILE('chapters',Info)
%   Retrieve list of Chapters (cell array of strings).
%
%   Val=INIFILE('get',Info,Chapter,Keyword,Default)
%   Retrieve Chapter/Keyword from the Info data set. The Default value is
%   optional. If the Chapter ID is '*', the Keyword is searched for in
%   all chapters in the file.
%
%   Info=INIFILE('set',Info,Chapter,Keyword,Value)
%   Set Chapter/Keyword in the data set to the indicated value. The
%   updated data set is returned. Data is not written to file. If the
%   chapter and/or keyword do not exist, they are created. If Value equals
%   [], the keyword is deleted (see below). Use the 'write' option to
%   write the data to file.
%
%   Info=INIFILE('delete',Info,Chapter,Keyword)
%   Info=INIFILE('set',Info,Chapter,Keyword,[])
%   Delete Chapter/Keyword from the data set. The updated data set is
%   returned. Data is not written to file. Use the 'write' option to
%   write the data to file.

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

S=[];
switch lower(cmd)
    case 'open'
        S=readfile(varargin{:});
    case 'chapters'
        S=chapfile(varargin{:});
    case {'get','getstring'}
        S=getfield(lower(cmd),varargin{:});
    case 'set'
        S=setfield(varargin{:});
    case 'delete'
        S=setfield(varargin{:},[]);
    case 'write'
        writefile(varargin{:});
    case 'new'
        S=newfile;
end
if nargout>0
    varargout={S};
end


function FI=newfile
S=cell(0,2);
FI.FileName='new file';
FI.FileType='INI file';
FI.Data=S;


function FI=readfile(filename)
S=cell(0,2);
fid=fopen(filename,'r');
if fid<0
    error('Error opening %s.',filename)
end
Line={};
while ~feof(fid)
    L = fgetl(fid);
    if ischar(L)
        nLine=deblank2(L);
        if length(nLine)>0
            Line{end+1}=nLine;
        end
    end
end
fclose(fid);
for i=1:length(Line)
    ln=Line{i};
    if ln(1)=='['
        S(end+1,1:2)={ln(2:end-1) cell(0,2)};
    else
        if isempty(S)
            S(end+1,1:2)={'' cell(0,2)};
        end
        eq=strfind(ln,'=');
        if length(eq)>0
            SF={ln(1:eq(1)-1) deblank2(ln(eq(1)+1:end))};
        else
            SF={'' ln};
        end
        S{end,2}(end+1,1:2)=SF;
    end
end
FI.FileName=filename;
FI.FileType='INI file';
FI.Data=S;


function writefile(filename,FI)
S=FI.Data;
fid=fopen(filename,'wt');
if fid<0
    error('Error opening %s.',filename)
end
%
% Keywords without a Chapter title should be written first.
%
for i=1:size(S,1)
    if isempty(S{i,1})
        S=cat(1,S(i,:),S(1:i-1,:),S(i+1:end,:));
    end
end
maxkeywordlength=0;
for i=1:size(S,1)
    SF=S{i,2};
    for j=1:size(SF,1)
        maxkeywordlength=max(maxkeywordlength,length(SF{j,1}));
    end
end
format=['%-',num2str(maxkeywordlength),'s= %s\n'];
format_spaces=[repmat(' ',1,maxkeywordlength),'  %s\n'];
for i=1:size(S,1)
    if ~isempty(S{i,1})
        fprintf(fid,'[%s]\n',S{i,1});
    end
    SF=S{i,2};
    for j=1:size(SF,1)
        Str=SF{j,2};
        if ~ischar(Str)
            Str=sprintf('%g ',Str);
            Str(end)=[];
        end
        if isempty(SF{j,1})
            fprintf(fid,format_spaces,Str);
        else
            fprintf(fid,format,SF{j,1},Str);
        end
    end
end
fclose(fid);


function chaps=chapfile(FI)
chaps=FI.Data(:,1);


function val=getfield(cmd,FI,grpS,keyS,def)
S=FI.Data;
if ischar(grpS)
    if isequal(grpS,'*')
        grp=1:size(S,1);
    else
        grp=strmatch(grpS,S(:,1),'exact');
    end
else
    grp=grpS;
    grpS=sprintf('group#%i',grp);
end
if isempty(grp)
    if nargin>=4
        val=def;
        return;
    end
    error('Chapter ''%s'' does not exist',grpS)
end
Keywords=cat(1,S{grp,2});
key=strmatch(keyS,Keywords(:,1),'exact');
if isequal(size(key),[1 1])
    val=Keywords{key,2};
    if ischar(val) && ~strcmp(cmd,'getstring')
        [lni,n,err,SF2i]=sscanf(val,'%f',[1 inf]);
        if isempty(err) & SF2i>length(val)
            val=lni;
        end
    end
elseif isempty(key) & nargin>=4
    val=def;
elseif ~isempty(key)
    val=Keywords(key,2);
    if ~strcmp(cmd,'getstring')
        for i=1:length(val)
            [lni,n,err,SF2i]=sscanf(val{i},'%f',[1 inf]);
            if isempty(err) & SF2i>length(val{i})
                val{i}=lni;
            end
        end
    end
else
    error('Keyword ''%s'' not found in Chapter ''%s''.',keyS,grpS)
end


function FI=setfield(FI,grpS,keyS,val)
S=FI.Data;
if nargin<4
    error('Not enough input arguments.')
end
if ischar(grpS)
    if isequal(grpS,'*')
        grp=1:size(S,1);
    else
        grp=strmatch(grpS,S(:,1),'exact');
    end
else
    grp=grpS;
    grpS=sprintf('group#%i',grp);
end
if isempty(grp)
    if isempty(val) & ~ischar(val)
        return
    end
    S(end+1,1:2)={grpS cell(0,2)};
    grp=size(S,1);
end
ingrp=zeros(size(grp));
for i=1:length(grp)
    Keywords=S{grp(i),2};
    key=strmatch(keyS,Keywords(:,1),'exact');
    if ~isempty(key)
        ingrp(i)=1;
    end
end
if ~any(ingrp)
    if isempty(val) & ~ischar(val)
        return
    end
    if length(grp)==1
        S{grp,2}(end+1,1:2)={keyS val};
    else
        error('Cannot add key to multiple chapters at once.')
    end
else
    if sum(ingrp)>1
        %
        % Key found in multiple chapters: which one to change?
        %
        if isempty(val) & ~ischar(val)
            error('Cannot remove key from multiple chapters at once.')
        end
        error('Cannot set value of key in multiple chapters at once.')
    else
        %
        % Key found in one chapter (may still occur multiple times).
        %
        grp = grp(ingrp~=0);
        Keywords=S{grp,2};
        key=strmatch(keyS,Keywords(:,1),'exact');
        if ~isempty(key)
            if isempty(val) & ~ischar(val)
                S{i,2}(key,:)=[];
            else
                S{i,2}{key(1),2}=val;
                if length(key)>1
                    S{i,2}(key(2:end),:)=[];
                end
            end
        end
    end
end
FI.Data=S;
