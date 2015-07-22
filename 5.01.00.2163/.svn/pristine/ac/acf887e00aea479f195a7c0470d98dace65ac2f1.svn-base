function varargout=dbase(cmd,varargin)
%DBASE Read data from a dBase file.
%
%   FI=DBASE('open','filename')
%   Open a dBase file.
%
%   Data=DBASE('read',FI,Records,Fields)
%   Read specified records from the opened dBase file.
%   Support 0 for reading all records / fields.

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
        varargout=cell(1,nargout);
    end
    return
end

switch lower(cmd)
    case {'open'}
        Info=Local_open_dbase(varargin{:});
        varargout={Info};
    case {'read'}
        Data=Local_read_dbase(varargin{:});
        varargout={Data};
    otherwise
        error('Unknown command')
end


function S=Local_open_dbase(filename)
S.Check='NotOK';
S.FileType='dBase';

if (nargin==0) | strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.dbf','Select dBase file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end

S.FileName=filename;
fid=fopen(filename,'r','l');
S.SubTypeNr=fread(fid,1,'uint8');
switch S.SubTypeNr
    case 3
        S.SubType='dBase III+';
    case 4
        S.SubType='dBase IV';
    case 5
        S.SubType='dBase V';
    case {6,7,8,9,10}
        S.SubType=sprintf('dBase %i ?',S.SubTypeNr);
    case 67
        S.SubType='with .dbv memo var size';
    case 131
        S.SubType='dBase III+ with memo'; % .dbt
    case 139
        S.SubType='dBase IV with memo'; % .dbt
    case 142
        S.SubType='dBase IV with SQL table';
    case 179
        S.SubType='with .dbv and .dbt memo';
    case 245
        S.SubType='FoxPro with memo'; % .fmp
    otherwise
        S.SubType='unknown';
end
Date=fread(fid,[1 3],'uint8');
if (Date(2)>12) | (Date(2)==0) | (Date(3)==0) | (Date(3)>31)
    fclose(fid);
    error('Invalid date in dBase file.');
end
S.LastUpdate=datenum(Date(1)+1900,Date(2),Date(3));
S.NRec=fread(fid,1,'uint32');
S.HeaderBytes=fread(fid,1,'uint16');
S.NFld=(S.HeaderBytes-33)/32;
if S.NFld~=round(S.NFld)
    fclose(fid);
    error('Invalid header size in dBase file.');
end
S.NBytesRec=fread(fid,1,'uint16'); % includes deleted flag
fread(fid,2,'uint8'); % reserved
fread(fid,1,'uint8'); % dBase IV flag
S.Encrypted=fread(fid,1,'uint8');
fread(fid,12,'uint8'); % dBase IV multi-user environment
S.ProdIndex=fread(fid,1,'uint8'); % Production Index Exists (Fp,dB4,dB5)
S.LangID=fread(fid,1,'uint8'); % 1: USA, 2: MultiLing, 3: Win ANSI, 200: Win EE, 0: ignored
fread(fid,2,'uint8'); % reserved
for i=1:S.NFld
    S.Fld(i).Name=deblank(char(fread(fid,[1 11],'uchar')));
    S.Fld(i).Type=char(fread(fid,1,'uchar'));
    fread(fid,[1 4],'uint8'); % memory address, record offset, ignored in latest versions
    S.Fld(i).Width=fread(fid,1,'uint8');
    S.Fld(i).NDec=fread(fid,1,'uint8'); % Type='C' also Width
    fread(fid,2,'uint8'); % reserved
    fread(fid,1,'uint8'); % dBase IV,V work area ID
    fread(fid,2,'uint8'); % multi-user dBase
    fread(fid,1,'uint8'); % set fields
    fread(fid,7,'uint8'); % reserved
    S.Fld(i).ProdIndex=fread(fid,1,'uint8'); % field is part of production index
end
Flag=fread(fid,1,'uint8'); % end of header
if Flag~=13,
    fclose(fid);
    error('Invalid end of dBase header.');
end
Flag=fread(fid,1,'uint8'); % first record
if (Flag~=' ') & (Flag~='*'), % *=deleted
    fclose(fid);
    error('Invalid first record in dBase file.');
end
fclose(fid);
%
% Should the following depend on ProdIndex?
%
MdxFileName=S.FileName;
MdxFileName(end-2:end)=MdxFileName(end-2:end)-'DBF'+'MDX';
fid = fopen(MdxFileName,'r','l');
if fid>0
    fseek(fid,0,1);
    FileSize = ftell(fid);
    fseek(fid,0,-1);
    %
    MDX.FileName = MdxFileName;
    MDX.SubTypeNr=fread(fid,1,'uint8');
    Date=fread(fid,[1 3],'uint8');
    if (Date(2)>12) | (Date(2)==0) | (Date(3)==0) | (Date(3)>31)
        fclose(fid);
        error('Invalid date in dBase MDX file.');
    end
    MDX.Date_Creation = datenum(Date(1)+1900,Date(2),Date(3));
    MDX.Name = deblank(char(fread(fid,[1 16],'uchar')));
    MDX.BlockSizePages = fread(fid,1,'uint16'); % 2 % Page = 512 Bytes
    MDX.BlockSizeBytes = fread(fid,1,'uint16'); % 1024
    MDX.ProdIndex = fread(fid,1,'uint8'); % Production Index Exists (Fp,dB4,dB5)
    MDX.NEntries = fread(fid,1,'uint8'); % max 48
    MDX.TagLen = fread(fid,1,'uint8'); % max 32
    fread(fid,1,'uint8');
    MDX.NPages = fread(fid,1,'int32');
    MDX.FileSizePages = fread(fid,1,'int32'); % First Free Page
    X = fread(fid,[1 2],'int32'); % 0 0
    Date=fread(fid,[1 3],'uint8');
    if (Date(2)>12) | (Date(2)==0) | (Date(3)==0) | (Date(3)>31)
        fclose(fid);
        error('Invalid date (2) in dBase MDX file.');
    end
    MDX.Date_LastUpdate=datenum(Date(1)+1900,Date(2),Date(3));
    %
    fseek(fid,528,-1);
    fread(fid,[1 4],'int32'); % 256 0 0 0
    for i=1:MDX.NPages
        MDX.Index(i).PageNumber = fread(fid,[1 1],'int32'); % 2*(1+i)
        MDX.Index(i).Offset = 512*MDX.Index(i).PageNumber; % 2*(1+i)
        MDX.Index(i).Name = deblank(char(fread(fid,[1 11],'uchar')));
        MDX.Index(i).KeyFormat = fread(fid,1,'uint8'); % 00h Calculated, 10h Data field
        switch MDX.Index(i).KeyFormat
            case 0
                MDX.Index(i).KeyFormatStr = 'calculated';
            case 16
                MDX.Index(i).KeyFormatStr = 'data field';
            otherwise
                MDX.Index(i).KeyFormatStr = 'unknown';
        end
        MDX.Index(i).ForwardTagThread1 = fread(fid,1,'uint8'); % 0/3 (<)
        MDX.Index(i).ForwardTagThread2 = fread(fid,1,'uint8'); % 0/2/3/4/5 (>)
        MDX.Index(i).BackwardTagThread = fread(fid,1,'uint8'); % 0/1/2/4
        MDX.Index(i).X1a = fread(fid,1,'uint8');
        MDX.Index(i).KeyType = char(fread(fid,1,'uchar'));
        MDX.Index(i).X1b = fread(fid,[1 11],'uint8');
        % 15 6 7 8 9 20
        % 16 0 2 0 2 67 0 0 0 0 0 0 0 0 0 0 0: LOCID, D_LOCID+D_STORMID
        % 16 3 2 0 2 67 0 0 0 0 0 0 0 0 0 0 0: STORMID
        % 16 0 4 1 2 67 0 0 0 0 0 0 0 0 0 0 0: MPEIL+WINDR+WINDS
        % 16 0 0 1 2 67 0 0 0 0 0 0 0 0 0 0 0: MPEIL, X+Y
        % 16 0 5 2 2 67 0 0 0 0 0 0 0 0 0 0 0: WINDR
        % 16 0 0 4 2 67 0 0 0 0 0 0 0 0 0 0 0: WINDS
        % 16 0 3 1 2 67 0 0 0 0 0 0 0 0 0 0 0: D_LOCID
        % 16 0 0 2 2 67 0 0 0 0 0 0 0 0 0 0 0: D_STORMID
    end
    %
    for i=1:MDX.NPages
        fseek(fid,MDX.Index(i).Offset,-1);
        %
        % Offset of last part of table?
        %
        MDX.Index(i).RootPage = fread(fid,[1 1],'int32');
        MDX.Index(i).RootOffset = 512*MDX.Index(i).RootPage;
        %
        MDX.Index(i).X2a = fread(fid,1,'int32'); % 0
        MDX.Index(i).KeyFormat1 = fread(fid,1,'uint8'); % 00h Right left dtoc, 08h Decending order, 10h Fields string, 40h Unique keys
        KT = char(fread(fid,1,'uchar'));
        if ~isequal(KT,MDX.Index(i).KeyType)
            error('Second key type (%s) does not match first key type (%s)',KT,MDX.Index(i).KeyType)
        end
        MDX.Index(i).X2b = fread(fid,1,'int16');
        MDX.Index(i).NBytes = fread(fid,1,'int16'); % Numeric 12, Date 8, Character <= 100
        MDX.Index(i).NBytesExtended = ceil(MDX.Index(i).NBytes/4)*4;
        MDX.Index(i).MaxNKeysPerPage = fread(fid,1,'int16');
        MDX.Index(i).SecKeyType = fread(fid,1,'int16'); % 0 char/num (db4), char (db3); 1 date (db4), num/date (db3)
        MDX.Index(i).NBytesItem = fread(fid,1,'int16'); % 4+MDX.Index(i).NBytesExtended
        MDX.Index(i).X3 = fread(fid,[1 4],'uint8');
        % XX = mod(S.NRec,256)+2
        % XX 0 0 64: STORMID, LOCID
        % XX 0 0 64: MPEIL+WINDR+WINDS, D_LOCID+D_STORMID
        % XX 0 0  0: MPEIL, D_LOCID, D_STORMID
        % XX 0 0  0: WINDR, WINDS
        % XX 0 0 64: X+Y
        %      Key^
        MDX.Index(i).Name2 = deblank(char(fread(fid,[1 1000],'uchar')));
        fseek(fid,MDX.Index(i).RootOffset,-1);
        MDX.Index(i).X4 = fread(fid,[1 2],'int32');
        for j=1:MDX.Index(i).X4(1)
            {MDX.Index(i).Name2 fread(fid,[1 1],'int32') char(fread(fid,[1 MDX.Index(i).NBytesExtended],'uchar'))};
        end
        MDX.Index(i)
    end
    %
    %fread(fid,[1 1],'int32')
    %fread(fid,[1 1],'int32')
    %for i=1:89
    %   fread(fid,[1 1],'int32')
    %   char(fread(fid,[1 4],'uchar'))
    %end

    N = zeros(1,MDX.NPages);
    %
    % Storm
    ii = [1 2 3 4 5 2 2 1 1 3 3 4 4 5 5 2 1 4 5];
    %
    %  1   2   3   4   5   2   2   1   1   3   3   4   4   5   5   2   1   4   5
    % 90  66  93  90  73  84   2  63   2 123   1  63   2  78   2  66  63  63  65
    %                          *       *       *       *       *
    %
    % Location
    %ii = [1 2 2 2];
    %
    %      1     2     2     2
    %     89    33    56     1
    %      *                 *
    %
    % Data
    %    ii = [1 2 3 1 1 2 2 3 3 1 1 2 2 2 2 2 1 1 1 2 1 2 2 2 2 1 2 1 2 1 2 ...
    %       2 1 1 2 1 1 2 2 2 1 2 2 2 2 1 2 2 2 1 2 1 2 1 1 1 1 2 2 2 2 2 2 2 ...
    %       2 1 1 1 1 2 1 1 1 2 1 2 2 2 2 2 1 2 2 2 2 2 2 2 1 1 2 1 2 2 2 2 2 ...
    %       1 2 2 1 1 2 1 1 1 1 1 2 1 1 1 2 1 1 1 1 1 1 1 2 2 1 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 1 2 1 1 1 1 1 1 2 2 1 2 2 1 ...
    %       2 2 1 2 2 2 2 2 2 2 1 2 1 1 2 2 1 2 2 2 2 1 2 1 1 2 1 1 1 1 1 2 1 ...
    %       1 1 2 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 ...
    %       2 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 2 1 1 1 2 1 1 1 2 1 1 1 2 1 2 1 2 ...
    %       1 1 2 2 1 2 2 2 2 2 2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 ...
    %       2 1 1 2 1 1 2 2 2 1 1 2 1 1 1 1 2 2 1 1 2 2 2 2 1 1 2 1 1 1 1 2 1 ...
    %       1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 ...
    %       1 1 2 1 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 ...
    %       2 2 2 2 1 2 2 2 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 2 1 2 2 2 ...
    %       2 2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1 2 1 1 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 ...
    %       2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 ...
    %       1 1 1 1 1 1 2 2 1 1 1 1 2 2 1 1 2 1 1 1 1 1 1 1 2 2 1 1 2 1 1 1 2 ...
    %       1 1 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 ...
    %       2 2 2 1 2 2 2 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 2 1 2 2 2 2 ...
    %       2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 2 2 1 1 2 2 1 1 2 ...
    %       2 2 2 2 2 2 1 1 2 1 1 1 1 2 2 1 1 2 2 2 2 1 1 2 1 1 1 1 2 1 1 1 1 ...
    %       1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 ...
    %       2 1 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 ...
    %       2 2 2 1 1 1 1 2 2 1 1 1 1 2 1 2 1 1 2 1 1 1 2 2 1 2 1 2 1 1 2 2 2 ...
    %       2 1 2 2 2 2 2 2 2 2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 ...
    %       1 1 2 1 1 2 2 2 1 1 2 1 1 1 1 2 2 1 1 2 2 2 2 1 1];
    for b = 1:MDX.FileSizePages/2-2-MDX.NPages
        fseek(fid,(b+1+MDX.NPages)*1024,-1);
        i=ii(1,b);
        A=fread(fid,[1 2],'int32');
        ii(2,b)=A(1);
        for j=1:A(1)-1
            {MDX.Index(i).Name2 fread(fid,[1 1],'int32') char(fread(fid,[1 MDX.Index(i).NBytesExtended],'uchar'))};
        end
        {MDX.Index(i).Name2 fread(fid,[1 1],'int32') char(fread(fid,[1 MDX.Index(i).NBytesExtended],'uchar'))};
        N(i)=N(i)+A(1);
    end
    ii;
    fclose(fid);
    S.MDX = MDX;
end
%
S.Check='OK';


function Dbs=Local_read_dbase(S,Records,Fields)
if ~isequal(Records,0)
    Records=Records(:);
    if any((Records>S.NRec) | (Records<1) | (Records~=round(Records)))
        error('Invalid record number.');
    end
end
if isequal(Fields,0)
    Fields=1:S.NFld;
else
    Fields=Fields(:)';
    if any((Fields>S.NFld) | (Fields<1) | (Fields~=round(Fields)))
        error('Invalid field number.');
    end
end
fid=fopen(S.FileName,'r','l');
Dbs=cell(1,length(Fields));
for j=1:length(Fields)
    i=Fields(j);
    fseek(fid,S.HeaderBytes+1+sum([S.Fld(1:(i-1)).Width]),-1);
    switch S.Fld(i).Type
        case '2' % binary (int16)
            ReadFld=fread(fid,[S.NRec 1],'int16',S.NBytesRec-2);
        case '4' % binary (int32)
            ReadFld=fread(fid,[S.NRec 1],'int32',S.NBytesRec-4);
        case '8' % binary (float64)
            ReadFld=fread(fid,[S.NRec 1],'float64',S.NBytesRec-8);
        case {'B','G','M','P','C','D','F','N','L'}
            type = S.Fld(i).Type;
            StFormat=sprintf('%%%ic',S.NBytesRec);
            switch type
                case {'B','G','M','P','C','L'}
                    ReadFld=repmat(' ',S.NRec,S.Fld(i).Width);
                case 'D' % date: YYYYMMDD, Width=8
                    Format='%%4i%%2i%%2i';
                    ReadFld=zeros(S.NRec,1);
                case {'F','N'} % F floating point, or N numeric
                    Format=sprintf('%%%if',S.Fld(i).Width);
                    ReadFld=zeros(S.NRec,1);
            end
            %
            NPerRead=max(1,floor(10000/S.NBytesRec));
            ix=0;
            while ix<S.NRec
                if S.NRec-ix<NPerRead
                    NPerRead = S.NRec-ix;
                end
                St=fscanf(fid,StFormat,[1 NPerRead]);
                if length(St)<NPerRead*S.NBytesRec
                    St(NPerRead*S.NBytesRec)=' ';
                end
                St=reshape(St,[S.NBytesRec NPerRead]);
                St=St(1:S.Fld(i).Width,:);
                switch type
                    case {'B','G','M','P','C'}
                        % B binary: binary data in .dbt
                        % G general (FoxPro)
                        % M dbt memo index
                        % P picture (FoxPro): binary data in .ftp
                        % C character
                        Tmp=St';
                    case 'D' % date: YYYYMMDD, Width=8
                        T=sscanf(St,Format,[3 NPerRead]);
                        Tmp=datenum(T(1,:),T(2,:),T(3,:))';
                    case {'F','N'} % F floating point, or N numeric
                        %
                        % The following code works in recent MATLAB versions,
                        % i.e. the 2006 releases. However, R13 does not accept
                        % spaces in %6f formatted reading.
                        %
                        %Tmp=sscanf(St,Format,[NPerRead 1]);
                        %
                        % The following is a workaround that works for all MATLAB
                        % versions considered.
                        %
                        St(end+1,:)=' ';
                        Tmp=sscanf(St,'%f',[NPerRead 1]);
                    case 'L' % logical (T:t,F:f,Y:y,N:n,? or space)
                        Tmp=upper(St');
                end
                ReadFld(ix+(1:NPerRead),:)=Tmp;
                ix = ix+NPerRead;
            end
            %
            switch type
                case {'B','G','M','P','C'}
                    ReadFld=cellstr(ReadFld);
            end
        otherwise
            error('Invalid dBase field type.');
    end
    if isequal(Records,0)
        Dbs{j}=ReadFld;
    else
        Dbs{j}=ReadFld(Records);
    end
end
fclose(fid);
