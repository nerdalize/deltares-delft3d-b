function [Data, errmsg] = qp_netcdf_get(FI,var,RequestDims,RequestedSubset)
%QP_NETCDF_GET Get data from netcdf file and reshape.
%   [DATA,ERR] = QP_NETCDF_GET(FILE,VAR,REQDIMS,REQIND) reads data for the
%   variable VAR from the specified netcdf file FILE. The order of the
%   dimensions of VAR (and optional additional dimensions) is to be specified
%   by the cell string REQDIMS, and the corresponding requested indices are
%   given by REQIND. The data is returned as DATA and ERR will be a non-empty
%   string is an error occurs.

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

errmsg='';
%
if isempty(var)
    errmsg='Variable name is empty.';
    error(errmsg)
elseif ischar(var)
    var = strmatch(var,{FI.Dataset.Name},'exact')-1;
elseif isstruct(var)
    var = var.Varid;
end
Info = FI.Dataset(var+1);
varid = Info.Varid;
if isempty(Info)
    errmsg=sprintf('Variable ''%s'' not found in file.',Info.Name);
    error(errmsg)
end
%
rank=Info.Rank;
N=length(RequestDims);
permuted=zeros(1,N);
for d=1:N
    DName=RequestDims{d};
    if ~isempty(DName)
        d_netcdf=strmatch(DName,Info.Dimension,'exact');
        if isempty(d_netcdf)
            %
            % requested dimension does not occur in NetCDF source
            % use higher dimensions to fill in these dimensions.
            %
            rank=rank+1;
            permuted(d)=rank;
        else
            %
            % requested dimension occurs in NetCDF source
            %
            permuted(d)=d_netcdf;
        end
    end
end
%
for d=1:Info.Rank
    if all(permuted~=d)
        errmsg=sprintf('Dimension ''%s'' of ''%s'' could not be matched to any of the requested dimensions: ',Info.Dimension{d},Info.Name);
        errmsg=cat(2,errmsg,sprintf('''%s'', ',RequestDims{permuted~=0}));
        errmsg(end-1:end)=[];
        error(errmsg)
    end
end
%
fliporder = ~getpref('SNCTOOLS','PRESERVE_FVD',false);
%if fliporder
%   reverse=Info.Rank:-1:1;
%else
   reverse=1:Info.Rank;
%end
%
loadtype = 'double';
if Info.Nctype == nc_char
    loadtype = 'text';
end
if isempty(Info.Dimid)
    Data = nc_varget(FI.Filename,FI.Dataset(varid+1).Name);
elseif nargin==3
    Data = nc_varget(FI.Filename,FI.Dataset(varid+1).Name);
else
    %
    % Convert data subset in QP dimension order to NetCDF dimension order
    %
    RS_netcdf=cell(1,Info.Rank);
    start_coord=zeros(1,Info.Rank);
    count_coord=zeros(1,Info.Rank);
    %
    % The following block selection procedure is inefficient if a relatively
    % limited number of values is requested compared to full range of
    % indices, e.g. [1 2 100]
    %
    for d=1:N
        p=permuted(d);
        if p>0 && p<=Info.Rank
            RS_netcdf(p)=RequestedSubset(d);
            start_coord(p)=min(RS_netcdf{p})-1;
            count_coord(p)=max(RS_netcdf{p})-start_coord(p);
            RS_netcdf{p}=RS_netcdf{p}-start_coord(p);
        end
    end
    %
    %if fliporder
    %    start_coord = fliplr(start_coord);
    %    count_coord = fliplr(count_coord);
    %end
    %
    Data = nc_varget(FI.Filename,FI.Dataset(varid+1).Name,start_coord,count_coord);
    if length(count_coord)>1
        Data = reshape(Data,count_coord);
    end
    %Data=Data(RS_netcdf{reverse});
end
%
if ~isa(Data,'double') && ~isa(Data,'char')
    Data = double(Data);
end
%
reverse=[reverse Info.Rank+1:5];
permuted(permuted==0)=[];
if length(permuted)>1
    Data=permute(Data,reverse(permuted));
end
%
if ~isempty(Info.Attribute)
    Attribs = {Info.Attribute.Name};
    %
    missval = strmatch('missing_value',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        Data(Data==missval)=NaN;
    end
    %
    missval = strmatch('valid_min',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        Data(Data<missval)=NaN;
    end
    %
    missval = strmatch('valid_max',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        Data(Data>missval)=NaN;
    end
    %
    missval = strmatch('valid_range',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        Data(Data<missval(1) | Data>missval(2))=NaN;
    end
    %
    missval = strmatch('_FillValue',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        if missval>0
            Data(Data>=missval)=NaN;
        else
            Data(Data<=missval)=NaN;
        end
    else
        % NCL standard or general standard?
        % The following are the default settings for _FillValue by type: 
        %
        switch Info.Datatype
            case 'short'
                Data(Data<=-32767)=NaN;
            case 'int'
                Data(Data<=-2147483647)=NaN;
            case {'float','double'}
                Data(Data>=9.9692099683868690e+36)=NaN;
        end
    end
end
