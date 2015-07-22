function [x,y,z]=samples(cmd,varargin)
%SAMPLES Read/write sample data from file.
%     XYZ = SAMPLES('read',FILENAME) read the specified file and return the
%     samples as one Nx3 array.
%
%     [X,Y,Z] = SAMPLES('read',FILENAME) read the specified file and return
%     the samples in three separate Nx1 arrays.
%
%     SAMPLES('write',FILENAME,XYZ) write samples given in a Nx3 array to
%     file.
%
%     SAMPLES('write',FILENAME,X,Y,Z) write samples given in three Nx1
%     arrays to file.

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

switch lower(cmd)
    case 'read'
        xyz=Local_read_samples(varargin{:});
        if nargout>1
            x=xyz(:,1);
            y=xyz(:,2);
            z=xyz(:,3);
        else
            x=xyz;
        end
    case 'write'
        if ~ischar(varargin{1})
            Local_write_samples('?',varargin{:});
        else
            Local_write_samples(varargin{:});
        end
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end

function xyz=Local_read_samples(filename)
if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.xyz','Select sample file');
    if ~ischar(fname)
        xyz=zeros(0,3);
        return
    end
    filename=[fpath,fname];
end

if exist(filename)~=2
    error(['Cannot open ',filename,'.']);
end
simplexyz=0;
try
    xyz=load(filename);
    simplexyz=1;
catch
    try
        xyz=asciiload(filename);
        simplexyz=1;
    catch
    end
end
if ~simplexyz
    fid=fopen(filename,'r');
    try
        cloc=0;
        str=fgetl(fid);
        while ischar(str) && ~isempty(str) && str(1)=='*'
            cloc=ftell(fid);
            str=fgetl(fid);
        end
        if ~ischar(str)
            fclose(fid);
            error('%s does not contain samples.',filename);
        end
        [X,n,er]=sscanf(str,'%g');
        Params={};
        while ~isempty(er)
            [Param,n,er,ni]=sscanf(str,' "%[^"]%["]',2);
            if isempty(er) && n==2
                Params{end+1}=Param(1:end-1);
            else
                [Param,n,er]=sscanf(str,' %s',1);
                if isempty(er) && n==1
                    error('Reading line: %s\nIf this line contains a parameter name, then it should be enclosed by double quotes.',str);
                    %Params{end+1}=Param; %<--- makes it impossible to distinguish
                    %certain observation files from Tekal file format. For example:
                    %B001
                    %160 4 16 10
                    %1.0 2.0 3.0 4.0
                    % :   :   :   :
                else
                    break
                end
            end
            str=str(ni:end);
            if ~isempty(str)
                er='scan remainder of line';
            else
                cloc=ftell(fid);
                str=fgetl(fid);
                [X,n,er]=sscanf(str,'%g');
            end
        end
        [X,n,er]=sscanf(str,'%g');
        if ~isempty(er)
            error('Unable to data values on line: %s',str);
        elseif n<3
            error('Not enough values for sample data (X,Y,Value1,...)')
        end
        str=fgetl(fid);
        [X,n2]=sscanf(str,'%g');
        if n2~=n && ~feof(fid)
            error('Number of values per line should be constant.')
        end
        if length(Params)<n
            for i=(length(Params)+1):n
                Params{i}=sprintf('Parameter %i',i);
            end
        elseif length(Params)>n
            Params=Params(1:n);
        end
        if n==0
            fclose(fid);
            error('Number of values cannot be zero.')
        end
        fclose(fid);
    catch
        fclose(fid);
        rethrow(lasterror)
    end
    
    xyz.XYZ=asciiload(filename,'seek',cloc);
    xyz.Params=Params;
    xyz.FileType='samples';
    xyz.FileName=filename;
end


function Local_write_samples(filename,x,y,z)
if strcmp(filename,'?')
    [fn,fp]=uiputfile('*.xyz');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'wt');
if fid<0
    error(['Could not create or open: ',filename])
end

if nargin==4
    if size(x,2)==1 % column vectors
        xyz=transpose([x y z]);
    else % row vectors
        xyz=[x;y;z];
    end
    fprintf(fid,'%f %f %f\n',xyz);
else
    if size(x,2)==3 % column vector (3x)
        x=transpose(x);
    end
    fprintf(fid,'%f %f %f\n',x);
end
fclose(fid);
