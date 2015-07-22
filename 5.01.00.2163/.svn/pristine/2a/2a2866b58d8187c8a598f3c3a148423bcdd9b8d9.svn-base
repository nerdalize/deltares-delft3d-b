function varargout=bna(cmd,varargin)
%BNA Read/write for ArcInfo (un)generate files.
%   FILEINFO = BNA('open',FILENAME) opens the specified file, reads its
%   contents and returns a structure describing the data.
%
%   [X,Y] = BNA('read',FILEINFO) returns the X and Y data of all lines in
%   the file. If instead of the FILEINFO structure -- as obtained from a
%   BNA('open',...) call -- a file name is provided then the indicated file
%   is first opened. If only one output argument is requested then a Nx2
%   array is returned with X data in the first column and Y data in the
%   second column.
%
%   BNA('write',FILENAME,X,Y) writes the (X,Y) coordinates as line segments
%   to the indicated file. X and Y should either be vectors of equal length
%   (optionally containing NaN values to separate line segments) or cell
%   arrays of equal length where each cell contains one line segment.
%   Instead of two variables X and Y, one may also provide a Nx2 array or a
%   cell array containing N(i)x2 arrays containing X and Y coordinates as
%   columns.
%
%   BNA('write',...,'-1') does not write line segments of length 1.
%
%   See also LANDBOUNDARY.

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

if nargout>0
    varargout=cell(1,nargout);
end
if nargin==0
    return
end
switch cmd
    case 'open'
        Out=Local_open_file(varargin{:});
        varargout{1}=Out;
    case 'read'
        Out=Local_read_file(varargin{:});
        if nargout==1
            varargout{1}=Out;
        elseif nargout>1
            varargout{1}=Out(:,1);
            varargout{2}=Out(:,2);
        end
    case 'write'
        Local_write_file(varargin{:});
    otherwise
        error('Unknown command: %s.',cmd)
end


function T=Local_open_file(filename)
T=[];
if nargin==0,
    [fn,fp]=uigetfile('*.bna');
    if ~ischar(fn),
        return
    end
    filename=[fp fn];
end

fid=fopen(filename,'r');
try
    T.FileName=filename;
    T.FileType='BNA File';
    T.Check='NotOK';
    i=0;
    while ~feof(fid)
        Line=fgetl(fid);
        %
        % Scan segment header line
        %
        DQuotes=strfind(Line,'"');
        i=i+1;
        T.Seg(i).ID1=Line((DQuotes(1)+1):(DQuotes(2)-1));
        T.Seg(i).ID2=Line((DQuotes(3)+1):(DQuotes(4)-1));
        NPnt=sscanf(Line((DQuotes(4)+1):end),'%*[ ,]%i');
        if isempty(NPnt)
            error('Unable to determine number of points in data block %i.',i)
        end
        T.Seg(i).NPnt=NPnt;
        NPnt=abs(NPnt);
        %
        % Store offset to read data later
        %
        T.Seg(i).Offset=ftell(fid);
        %
        % Read data
        %
        T.Seg(i).Coord=fscanf(fid,'%f%*[ ,]%f',[2 NPnt])';
        %
        % Read remainder of last line
        %
        Line=fgetl(fid);
        if ~isempty(deblank(Line))
            error('Unexpected data found at end of BNA record %i: "%s"\n',i,Line(1:min(100,end)))
        end
    end
    fclose(fid);
    T.Check='OK';
catch
    fclose(fid);
    rethrow(lasterror)
end
%
% Compute total number of points
%
nel=0;
for i=1:length(T.Seg)
    nel=nel+size(T.Seg(i).Coord,1)+1;
end
nel=nel-1;
T.TotalNPnt=nel;

function Data=Local_read_file(varargin)
if nargin==1 && isstruct(varargin{1})
    T=varargin{1};
else
    T=Local_open_file(varargin{:});
end

nel=T.TotalNPnt;

Data=repmat(NaN,nel,2);
offset=0;
for i=1:length(T.Seg)
    t1=size(T.Seg(i).Coord,1);
    Data(offset+(1:t1),:)=T.Seg(i).Coord;
    offset=offset+t1+1;
end


function Local_write_file(varargin)
ai_ungen('write',varargin{:},'BNA');
