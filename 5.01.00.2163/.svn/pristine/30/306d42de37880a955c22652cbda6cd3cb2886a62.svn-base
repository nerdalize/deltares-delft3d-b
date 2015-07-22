function Out=tekal(cmd,varargin)
%TEKAL Read/write for Tekal files.
%   INFO = TEKAL('open',FILENAME) reads the specified Tekal file. If the
%   FILENAME is not specified you will be asked to select a file
%   interactively. The returned structure contains the metadata of the
%   Tekal file.
%
%   DATA = TEKAL('read',INFO,RECORD) reads the selected data record from
%   the specified file. The RECORD may be specified as integer (data block
%   number) or as block name (if that name is unique). In case RECORD is an
%   integer, you may select to read multiple records. In this case DATA
%   will be a cell array grouping all data blocks read.
%
%   INFO = TEKAL('write',FILENAME,DATA) writes the matrix DATA to a simple
%   plain Tekal file containing one data block called 'DATA'. The function
%   returns a structure INFO containing the metadata of the newly created
%   file.
%
%   NEWINFO = TEKAL('write',FILENAME,INFO) writes a more complete new Tekal
%   file based on the information in the INFO structure. INFO should be a
%   structure with at least a field called Field having two subfields Name
%   and Data. An optional subfield Comments will also be processed and
%   written to file.
%
%   Example
%      % Data for block 1
%      INFO.Field(1).Name = 'Magic';
%      INFO.Field(1).Data = magic(5);
%      % Data for block 2
%      INFO.Field(2).Name = 'Random';
%      INFO.Field(2).Data = rand(8,5,3,2);
%      INFO.Field(2).Comments = {'Note we can handle nD arrays too.'};
%      % write file
%      OUT = tekal('write','test.tek',INFO);
%
%      Random = tekal('read',OUT,'Random');
%      size(Random) % returns [8 5 3 2]
%
%   See also LANDBOUNDARY, TEKAL2TBA, QPFOPEN, QPREAD.

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

if nargin==0
    if nargout>0
        Out=[];
    end
    return
end
switch lower(cmd)
    case 'open'
        Out=Local_open_file(varargin{:});
    case 'read'
        if ischar(varargin{1}) % tekal('read','filename', ... : implicit open
            Out=Local_open_file(varargin{:});
        else
            Out=Local_read_file(varargin{:});
        end
    case 'write'
        Out=Local_write_file(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd))
end

function FileInfo=Local_open_file(varargin)
FileInfo.Check='NotOK';
TryToCorrect=0;
LoadData=0;
nSkipDataLines=0;
if nargin==0
    [fn,fp]=uigetfile('*.*');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
else
    filename=varargin{1};
end
INP=varargin(2:end);
i=1;
while i<=length(INP)
    switch lower(INP{i})
        case 'autocorrect'
            TryToCorrect=1;
        case 'loaddata'
            LoadData=1;
        case 'nskipdatalines'
            nSkipDataLines=INP{i+1};
            i=i+1;
    end
    i=i+1;
end

variable=0;
fid=fopen(filename);
if fid<0
    error('Cannot open file ...')
end
%
FileInfo.FileName=filename;
FileInfo.FileType='tekal';
ErrorFound=0;
Cmnt={};

while 1
    line=fgetl(fid);
    while isempty(line)
        line=fgetl(fid);
    end
    % end of file check (or some other error such that no line was read)
    % fprintf(1,'>''%s''<\n',line);
    if ~ischar(line)
        break
    end

    % check if we are dealing with a non-Tekal binary file ...
    % Tabs (char 9) are acceptable!
    if feof(fid) && length(line)==1 && line==26
        break
    elseif any(line<32 & line~=9) && ~TryToCorrect
        fclose(fid);
        error('Invalid line: %s',line)
    end

    % remove empty spaces
    line=deblank(line);
    space = ['%*[ ,' char(9),']'];
    % is comment or empty line
    if isempty(line)
    elseif line(1)=='*' % comment
        Cmnt{end+1,1}=line;
    elseif nSkipDataLines>0
        nSkipDataLines=nSkipDataLines-1;
    else
        % if not, it must be a variable name
        variable=variable+1;
        FileInfo.Field(variable).Name=deblank(line);
        FileInfo.Field(variable).Comments=Cmnt;
        % read data dimensions
        line=fgetl(fid);
        if ~ischar(line)
            dim=[];
        else
            dim=sscanf(line,['%f' space],[1 inf]);
            if ~isequal(dim,round(dim))
                fclose(fid);
                error('Error reading data dimensions from line: %s',line)
            end
        end
        if ~isempty(dim)
            dim(dim==-999)=inf;
            FileInfo.Field(variable).Size=dim;
            if (length(dim)>2) && prod(FileInfo.Field(variable).Size(3:end))~=FileInfo.Field(variable).Size(1)
                % MN   n   M    should be interpreted as    MN   n   M   N (=MN/M)
                if prod(FileInfo.Field(variable).Size(3:end))>0 && rem(FileInfo.Field(variable).Size(1),prod(FileInfo.Field(variable).Size(3:end)))==0
                    FileInfo.Field(variable).Size(end+1)=FileInfo.Field(variable).Size(1)/prod(FileInfo.Field(variable).Size(3:end));
                else
                    Sz = FileInfo.Field(variable).Size;
                    error(['Field %i labelled ''%s''\n' ...
                        'Specified dimension: %s\n' ...
                        'Number of rows (%i) is not integer multiple of reshape dimensions (%s).'], ...
                    variable, ...
                    FileInfo.Field(variable).Name, ...
                    sprintf('%i ',Sz), ...
                    Sz(1), ...
                    deblank(sprintf('%i ',Sz(3:end))))
                end
            end
            if length(dim)>1
                dim=dim([2 1]);
            else
                % auto detect number of values per line
                here = ftell(fid);
                line = fgetl(fid);
                fseek(fid,here,-1);
                values = sscanf(line,'%f');
                nvalues = max(1,length(values));
                FileInfo.Field(variable).Size=[dim nvalues];
                dim=[nvalues dim];
            end
            FileInfo.Field(variable).ColLabels=getcollabels(dim(1),Cmnt);
            FileInfo.Field(variable).Offset=ftell(fid);

            % create Data field but don't use it
            % This field is created to be compatible with the write statement
            FileInfo.Field(variable).Data=[];
            % skip data values
            if prod(FileInfo.Field(variable).Size)==0
                FileInfo.Field(variable).DataTp='numeric';
            else
                line=fgetl(fid);
                fseek(fid,FileInfo.Field(variable).Offset,-1);
                [X,N]=sscanf(line,['%f' space]);
                FileInfo.Field(variable).DataTp='numeric';
                if dim(1)==N+1 % annotation mode
                    FileInfo.Field(variable).DataTp='annotation';
                    if LoadData
                        Data=[];
                        for i=1:dim(2)
                            line=fgetl(fid);
                            Tkn=find(diff([0 ~ismember(line,[' ,' char(9)])])==1);
                            Data{1}(:,i)=sscanf(line,['%f' space],dim(1)-1);
                            Str=deblank(line(Tkn(dim(1)):end));
                            if Str(1)=='''' && Str(end)==''''
                                Str=Str(2:end-1);
                            elseif Str(1)=='"' && Str(end)=='"'
                                Str=Str(2:end-1);
                            end
                            Data{2}{i}=Str;
                        end
                        Data{1}=Data{1}';
                        Data{2}=Data{2}';
                        FileInfo.Field(variable).Data=Data;
                    else
                        for i=1:dim(2)
                            fgetl(fid);
                        end
                    end
                else % all numerics; use %f a bit faster than the scan-string above
                    % check number of values per line,
                    if (length(FileInfo.Field(variable).Size)>1) && (N<dim(1))
                        Msg=sprintf('Actual number of values per line %i does not match indicated number\nof values per line %i.',N,dim(1));
                        if ~TryToCorrect
                            fclose(fid);
                            error(Msg)
                        end
                        fprintf(1,'ERROR: %s\nUsing actual number and trying to continue ...\n',Msg);
                        dim(1)=N;
                        FileInfo.Field(variable).Size(2)=N;
                    end
                    [Data,Nr]=fscanf(fid,['%f%*[ ,' char(9) char(13) char(10) ']'],dim);
                    if Nr<prod(dim) % number read less than number expected
                        %
                        fseek(fid,FileInfo.Field(variable).Offset,-1);
                        [Data,Nr]=fscanf(fid,['%*[^',char(10),']%c'],dim(2));
                        %
                        if Nr<dim(2)-1
                            Msg=sprintf('End of file reached while skipping data field %i.\nData is probably damaged.',variable);
                            if ~TryToCorrect
                                fclose(fid);
                                error(Msg)
                            end
                            fprintf(1,'ERROR: %s\n',Msg);
                            ErrorFound=1;
                            break
                        else
                            EndData = ftell(fid);
                            NBytes = EndData-FileInfo.Field(variable).Offset;
                            fseek(fid,FileInfo.Field(variable).Offset,-1);
                            Chars = fread(fid,[1 NBytes],'char=>char');
                            Chars(Chars==char(13))=[];
                            Chars = [' ' strrep(Chars,char(10),[' ',char(10)])];
                            n = ['%*[^' char(13) char(10) ']'];
                            p = ['%*[ ,' char(9) char(13) char(10) ']'];
                            [Data,Nr,Er,Nxt]=sscanf(Chars,[repmat([p '%f'],1,dim(1)) n],dim);
                            if Nr<prod(dim)
                                irow = floor(Nr/dim(1))+1;
                                if irow==1
                                    rowStr = sscanf(Chars(2:end),n([1 3:end]),1);
                                else
                                    rowFirst=fliplr(sscanf(Chars(Nxt-1:-1:1),n([1 3:end]),1));
                                    rowStr = [rowFirst sscanf(Chars(Nxt:end),n([1 3:end]),1)];
                                end
                                Msg=sprintf(['Field %i labelled ''%s''\n' ...
                                    'Unable to interpret data on row %i: ''%s''\n' ...
                                    'Data field %i is probably damaged.'], ...
                                    variable, ...
                                    FileInfo.Field(variable).Name, ...
                                    floor(Nr/dim(1))+1, ...
                                    sscanf(Chars(Nxt:end),n([1 3:end]),1), ...
                                    variable);
                                if ~TryToCorrect
                                    fclose(fid);
                                    error(Msg)
                                end
                                fprintf(1,'ERROR: %s\nTrying to continue ...\n',Msg);
                                ErrorFound=1;
                                break
                            end
                        end
                    end
                    if ~isfinite(dim(2));
                        dim(2)=floor(Nr/dim(1));
                        FileInfo.Field(variable).Size(1)=dim(2);
                        if length(FileInfo.Field(variable).Size)>2
                            i = find(~isfinite(FileInfo.Field(variable).Size));
                            FileInfo.Field(variable).Size(i) = 1;
                            FileInfo.Field(variable).Size(i) = dim(2)/prod(FileInfo.Field(variable).Size(3:end));
                        end
                    end

                    % replace 999999 by Not-a-Numbers
                    Data(Data(:)==999999)=NaN;

                    % transpose data if it has two dimensions
                    if length(dim)>1
                        Data=Data';
                    end

                    % reshape to match Size
                    if length(FileInfo.Field(variable).Size)>2,
                        Data=reshape(Data,[FileInfo.Field(variable).Size(3:end) FileInfo.Field(variable).Size(2)]);
                    end
                    FileInfo.Field(variable).Data=Data;

                    % read closing end of line
                    fgetl(fid);
                end
            end
        else
            if ~isempty(line) && ischar(line) && ~TryToCorrect
                fclose(fid);
                error('Cannot determine field size from %s',line)
            end
            % remove field
            FileInfo.Field(variable)=[];
            variable=variable-1;
        end
        Cmnt={};
    end
end
fclose(fid);
if ~isfield(FileInfo,'Field')
    error('File is empty: %s',FileInfo.FileName)
end
if ~ErrorFound
    FileInfo.Check='OK';
end

function Data=Local_read_file(FileInfo,var)
% check whether the data has been read
if ischar(var)
    Fields={FileInfo.Field.Name};
    varnr=ustrcmpi(var,Fields);
    if varnr<0
        fprintf('Cannot determine which field to read.\n');
        Data=[];
        return
    else
        var=varnr;
    end
end
if length(var)~=1 || var==0
    if var==0
        var=1:length(FileInfo.Field);
    end
    for v=1:length(var)
        Data{v}=Local_read_file(FileInfo,var(v));
    end
    return
end
if isfield(FileInfo.Field(var),'Data') && ~isempty(FileInfo.Field(var).Data)
    Data=FileInfo.Field(var).Data;
else %if ~isnan(FileInfo.Field(var).Offset)
    fid=fopen(FileInfo.FileName);
    fseek(fid,FileInfo.Field(var).Offset,-1);
    if length(FileInfo.Field(var).Size)>1
        dim=FileInfo.Field(var).Size([2 1]);
    else
        dim=FileInfo.Field(var).Size;
    end
    switch FileInfo.Field(var).DataTp
        case 'annotation'
            for i=1:dim(2)
                line=fgetl(fid);
                Tkn=find(diff([0 ~ismember(line,[' ,' char(9)])])==1);
                Data{1}(:,i)=sscanf(line,'%f%*[ ,]',dim(1)-1);
                Str=deblank(line(Tkn(dim(1)):end));
                if Str(1)=='''' && Str(end)==''''
                    Str=Str(2:end-1);
                elseif Str(1)=='"' && Str(end)=='"'
                    Str=Str(2:end-1);
                end
                Data{2}{i}=Str;
            end
            Data{1}=Data{1}';
            Data{2}=Data{2}';
            fclose(fid);
        otherwise %'numeric' % all numerics; use fscanf
            %Data=fscanf(fid,'%f',dim);
            [Data,Nr]=fscanf(fid,['%f%*[ ,' char(9) char(13) char(10) ']'],dim);
            if Nr~=prod(dim)
                %
                % Maybe there is some text after the last column.
                % Reread the data by means of a different format
                % string. This format string requires a space or
                % line break before the first number, so seek to
                % one character before the start of the line (i.e.
                % include (part of) the EOL character).
                %
                fseek(fid,FileInfo.Field(var).Offset-1,-1);
                %
                n = ['%*[^' char(13) char(10) ']'];
                p = ['%*[ ,' char(9) char(13) char(10) ']'];
                [Data,Nr]=fscanf(fid,[repmat([p '%f'],1,dim(1)) n],dim);
            end
            fclose(fid);

            % replace 999999 by Not-a-Numbers
            Data(Data(:)==999999)=NaN;

            % transpose data if it has two dimensions
            if length(dim)>1
                Data=Data';
            end

            % reshape to match Size
            if length(FileInfo.Field(var).Size)>2
                Data=reshape(Data,[FileInfo.Field(var).Size(3:end) FileInfo.Field(var).Size(2)]);
            end
    end
end

function NewFileInfo=Local_write_file(filename,FileInfo)
fid=fopen(filename,'w');
NewFileInfo.Check='NotOK';
if fid<0
    error('invalid filename')
end

if isstruct(FileInfo)
    %  uiwait(msgbox('Not yet implemented','modal'));
    % sprintf('%s',[FileInfo.Field(var).Name char(32*ones(1,4-length(FileInfo.Field(var).Name)))]) % Write a name of at least four characters
    NewFileInfo.FileName=filename;
    for i=1:length(FileInfo.Field)
        NewFileInfo.Field(i).Size=size(FileInfo.Field(i).Data);
        if ndims(FileInfo.Field(i).Data)>2
            NewFileInfo.Field(i).Size=[prod(NewFileInfo.Field(i).Size(1:end-1)) NewFileInfo.Field(i).Size(end) NewFileInfo.Field(i).Size(1:end-1)];
            FileInfo.Field(i).Data=reshape(FileInfo.Field(i).Data,NewFileInfo.Field(i).Size(1:2));
        end

        ncol=NewFileInfo.Field(i).Size(2);
        NewFileInfo.Field(i).ColLabels=getcollabels(ncol,{});
        if isfield(FileInfo.Field(i),'Comments')
            if isempty(FileInfo.Field(i).Comments)
                NewFileInfo.Field(i).Comments={};
            elseif ischar(FileInfo.Field(i).Comments)
                NewFileInfo.Field(i).Comments{1}=FileInfo.Field(i).Comments;
            else
                NewFileInfo.Field(i).Comments=FileInfo.Field(i).Comments;
            end
            for c=1:length(NewFileInfo.Field(i).Comments)
                if isempty(NewFileInfo.Field(i).Comments{c}) || NewFileInfo.Field(i).Comments{c}(1)~='*'
                    NewFileInfo.Field(i).Comments{c}=['*' NewFileInfo.Field(i).Comments{c}];
                end
                fprintf(fid,'%s\n',NewFileInfo.Field(i).Comments{c});
            end
            NewFileInfo.Field(i).ColLabels=getcollabels(ncol,NewFileInfo.Field(i).Comments);
        end

        if isempty(FileInfo.Field(i).Name)
            FileInfo.Field(i).Name = sprintf('BL%2.2i',i);
        end
        NewFileInfo.Field(i).Name=FileInfo.Field(i).Name;
        fprintf(fid,'%s\n',FileInfo.Field(i).Name);

        fprintf(fid,' %i',NewFileInfo.Field(i).Size); fprintf(fid,'\n');
        NewFileInfo.Field(i).Offset=ftell(fid);

        if length(NewFileInfo.Field(i).ColLabels)>1 && strcmp(NewFileInfo.Field(i).ColLabels(1),'Date') & strcmp(NewFileInfo.Field(i).ColLabels(2),'Time')
            Format=['%08i %06i' repmat(' %.15g',1,size(FileInfo.Field(i).Data,2)-2) '\n'];
        else
            Format=[repmat(' %.15g',1,size(FileInfo.Field(i).Data,2)) '\n'];
        end

        if any(isnan(FileInfo.Field(i).Data(:)))
            FileInfo.Field(i).Data(isnan(FileInfo.Field(i).Data))=-999;
        end
        fprintf(fid,Format,FileInfo.Field(i).Data');

        NewFileInfo.Field(i).Data=[];
        NewFileInfo.Field(i).DataTp='numeric';
    end
    NewFileInfo.Check='OK';
else
    NewFileInfo.FileName=filename;
    fprintf(fid,'* This was created by Matlab at %s.\n',datestr(now));
    fprintf(fid,'DATA\n');
    NewFileInfo.Field.Name='DATA';
    NewFileInfo.Field.Size=size(FileInfo);
    %  if size(FileInfo,2)>1, % if matrix (or row vector)
    %    FileInfo=FileInfo';
    %  end;
    if ndims(FileInfo)>2
        NewFileInfo.Field.Size=[prod(NewFileInfo.Field.Size(1:end-1)) NewFileInfo.Field.Size(end) NewFileInfo.Field.Size(1:end-1)];
        FileInfo=reshape(FileInfo,NewFileInfo.Field.Size(1:2));
    end
    fprintf(fid,' %i',NewFileInfo.Field.Size); fprintf(fid,'\n');
    NewFileInfo.Field.Offset=ftell(fid);
    NewFileInfo.Field.Data=[];
    NewFileInfo.Field.DataTp='numeric';
    fprintf(fid,[repmat(' %.15g',1,size(FileInfo,2)) '\n'],FileInfo');
    NewFileInfo.Check='OK';
end

fclose(fid);

function ColLabels=getcollabels(ncol,Cmnt)
ColLabels={}; % necessary for standalone version
ColLabels(1:ncol,1)={''};
if ~isempty(Cmnt)
    for i=1:length(Cmnt)
        [Tk,Rm]=strtok(Cmnt{i}(2:end));
        if (length(Cmnt{i})>10) && strcmpi(Tk,'column')
            [a,c,err,idx]=sscanf(Rm,'%i%*[ :=]%c',2);
            if (c==2) && a(1)<=ncol && a(1)>0
                ColLabels{a(1)}=deblank(Rm(idx-1:end));
            end
        end
    end
end
