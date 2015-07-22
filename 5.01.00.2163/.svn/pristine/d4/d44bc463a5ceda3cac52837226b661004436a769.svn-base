function varargout = mdf(cmd,varargin)
%MDF Manipulate Delft3D-FLOW mdf files.
%   MDF_STRUCT = MDF('read',FILENAME) read mdf file and various attribute
%   files into structure.
%
%   MDF_OUT = MDF('rotate',MDF_IN,ANGLE) rotates the model administration
%   by ANGLE degrees; ANGLE can be 0, 90 (default), 180 or 270 degrees.
%
%   MDF('write',MDF_STRUCT,CASENAME,PATH) stores the data of the MDF_STRUCT
%   in an mdf file named CASENAME.mdf and all files are stored using the
%   same CASENAME combined with their default extension.
%
%   See also WLGRID, WLDEP, D3D_ATTRIB.

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
        varargout{1} = mdfread(varargin{:});
    case 'rotate'
        varargout{1} = mdfrotate(varargin{:});
    case 'clip'
        [varargout{1:max(1,nargout)}] = mdfclip(varargin{:});
    case 'write'
        mdfwrite(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end


function varargout = mdfclip(MDF1,varargin)
% MDFCLIP(MDF,MASK)
% MDFCLIP(MDF,MMIN,MMAX,NMIN,NMAX
% MDFCLIP(MDF,MLIM,NLIM)
%
mnkmax = inifile('get',MDF1.mdf,'','MNKmax');
switch nargin
    case 2
        % MDFCLIP(MDF,MASK)
        mask = varargin{1};
        if ~isnumeric(mask) && ~islogical(mask)
            error('MASK array should be logical or numeric')
        elseif ~isequal(size(mask),mnkmax(1:2))
            error('MASK argument should be size %i x %i.',mnkmax(1:2))
        end
    case 3
        % MDFCLIP(MDF,MLIM,NLIM)
        mask = zeros(mnkmax(1:2));
        mlim = varargin{1};
        if ~isnumeric(mlim) || ~isequal(size(mlim),[1 2])
            error('Invalid MLIM argument.')
        end
        nlim = varargin{2};
        if ~isnumeric(nlim) || ~isequal(size(nlim),[1 2])
            error('Invalid NLIM argument.')
        end
        mask(mlim(1):mlim(2),nlim(1):nlim(2))=1;
    case 5
        % MDFCLIP(MDF,MMIN,MMAX,NMIN,NMAX
        mask = zeros(mnkmax(1:2));
        mmin = varargin{1};
        if ~isnumeric(mmin) || ~isequal(size(mmin),[1 1])
            error('Invalid MMIN argument.')
        end
        mmax = varargin{2};
        if ~isnumeric(mmax) || ~isequal(size(mmax),[1 1])
            error('Invalid MMAX argument.')
        end
        nmin = varargin{3};
        if ~isnumeric(nmin) || ~isequal(size(nmin),[1 1])
            error('Invalid NMIN argument.')
        end
        nmax = varargin{4};
        if ~isnumeric(nmax) || ~isequal(size(nmax),[1 1])
            error('Invalid NMAX argument.')
        end
        mask(mmin:mmax,nmin:nmax)=1;
    otherwise
        error('Invalid input arguments for mdf(''clip'',...)')
end
%
% find mask numbers; replace NaNs by 0; exclude 0 from domain number list
%
mask(isnan(mask)) = 0;
mask([1 end],:) = 0;
mask(:,[1 end]) = 0;
domains = unique(mask(:));
domains(domains==0)=[];
%
if nargout<length(domains)
    warning('More domains indicated in MASK array than output arguments.')
    domains = domains(1:nargout);
end
varargout = cell(1,length(domains));
for i = 1:length(domains)
    mask1 = mask==domains(i);
    MDF2 = MDF1;
    %
    MAct = find(any(mask1,2));
    mmin = MAct(1)-1;
    mmax = MAct(end)+1;
    NAct = find(any(mask1,1));
    nmin = NAct(1)-1;
    nmax = NAct(end)+1;
    %
    cmask = ~mask1(mmin:mmax,nmin:nmax);
    gmask = cmask([2:end end],[2:end end])&cmask(:,[2:end end])&cmask([2:end end],:)&cmask;
    %
    MDF2.mdf = inifile('set',MDF2.mdf,'','MNKmax',[mmax-mmin+1 nmax-nmin+1 mnkmax(3)]);
    MDF2.grd.X = MDF2.grd.X(mmin:mmax-1,nmin:nmax-1);
    MDF2.grd.X(gmask(1:end-1,1:end-1))=MDF2.grd.MissingValue;
    MDF2.grd.Y = MDF2.grd.Y(mmin:mmax-1,nmin:nmax-1);
    MDF2.grd.Y(gmask(1:end-1,1:end-1))=MDF2.grd.MissingValue;
    MDF2.grd.Enclosure = enclosure('extract',MDF2.grd.X,MDF2.grd.Y);
    %
    if isfield(MDF2,'dep')
        MDF2.dep = MDF2.dep(mmin:mmax,nmin:nmax);
        %
        dpsopt = upper(getstring(inifile('get',MDF2.mdf,'','Dpsopt','DEFAULT')));
        if strcmp(dpsopt,'DP')
            MDF2.dep(cmask)=-999;
        else
            MDF2.dep(gmask)=-999;
        end
    end
    %
    varargout{i} = MDF2;
end


function string = getstring(hashedstring)
hashes = findstr(hashedstring,'#');
if isempty(hashes)
    string = deblank(hashedstring);
else
    string = deblank(hashedstring(hashes(1)+1:hashes(2)-1));
end

function [M2,N2] = rotate(M1,N1,MMAX)
if nargin==2
    MMAX = N1;
    N1 = M1(:,2);
    M1 = M1(:,1);
end
M2 = N1;
N2 = MMAX-M1+1;
if nargout<=1
    M2 = [M2 N2];
end


function MDF2 = mdfrotate(MDF1,angle)
if nargin>1
    switch angle
        case 0
            MDF2 = MDF1;
            return
        case 90
            % going to do here ...
        case 180
            % rotate first 90 degrees
            MDF1 = mdfrotate(MDF1);
            % other 90 degrees going to do here ...
        case 270
            % rotate first 90 degrees
            MDF1 = mdfrotate(MDF1);
            % rotate second 90 degrees
            MDF1 = mdfrotate(MDF1);
            % final 90 degrees going to do here ...
        otherwise
            error('Can only rotate 0, 90, 180 and 270 degrees.')
    end
end
MDF2 = MDF1;
%
mnkmax = inifile('get',MDF2.mdf,'','MNKmax');
MMAX = mnkmax(1);
MDF2.mdf = inifile('set',MDF2.mdf,'','MNKmax',mnkmax([2 1 3]));
%
ccofu = inifile('get',MDF2.mdf,'','Ccofu');
ccofv = inifile('get',MDF2.mdf,'','Ccofv');
MDF2.mdf = inifile('set',MDF2.mdf,'','Ccofu',ccofv);
MDF2.mdf = inifile('set',MDF2.mdf,'','Ccofv',ccofu);
%
MDF2.grd.X = fliplr(MDF2.grd.X');
MDF2.grd.Y = fliplr(MDF2.grd.Y');
MDF2.grd.Enclosure = rotate(MDF2.grd.Enclosure,MMAX);
%
if isfield(MDF2,'dep')
    dpsopt = inifile('get',MDF2.mdf,'','Dpsopt');
    if strcmpi(dpsopt,'DP')
        % data in cell centres: dummy row along all sides
        MDF2.dep = fliplr(MDF2.dep');
    else
        % data at grid points: dummy row only at high M and N
        szM = size(MDF2.dep,1);
        szN = size(MDF2.dep,2);
%        MDF2.dep = [fliplr(MDF2.dep(:,1:end-1)');repmat(-999,1,szM)];
        MDF2.dep = [rot90(MDF2.dep(1:end-1,1:end-1),-1) repmat(-999,szN-1,1); repmat(-999,1,szM)];
    end
end
%
if isfield(MDF2,'dry')
    MDF2.dry.MN(:,1:2) = rotate(MDF2.dry.MN(:,1:2),MMAX);
    MDF2.dry.MN(:,3:4) = rotate(MDF2.dry.MN(:,3:4),MMAX);
end
%
if isfield(MDF2,'thd')
    MDF2.thd.MNu(:,1:2) = rotate(MDF2.thd.MNu(:,1:2),MMAX);
    MDF2.thd.MNu(:,3:4) = rotate(MDF2.thd.MNu(:,3:4),MMAX);
    MDF2.thd.MNv(:,1:2) = rotate(MDF2.thd.MNv(:,1:2),MMAX);
    MDF2.thd.MNv(:,3:4) = rotate(MDF2.thd.MNv(:,3:4),MMAX);
    TMP_MN = MDF2.thd.MNu;
    TMP_CHAR = MDF2.thd.CHARu;
    MDF2.thd.MNu = MDF2.thd.MNv;
    MDF2.thd.CHARu = MDF2.thd.CHARv;
    MDF2.thd.MNv = TMP_MN;
    MDF2.thd.CHARv = TMP_CHAR;
end
%
if isfield(MDF2,'bnd')
    MDF2.bnd.MN(:,1:2) = rotate(MDF2.bnd.MN(:,1:2),MMAX);
    MDF2.bnd.MN(:,3:4) = rotate(MDF2.bnd.MN(:,3:4),MMAX);
    %
    for i = 1:length(MDF2.bnd.Name)
        if any('CQTRN'==MDF2.bnd.BndType(i))
            if MDF2.bnd.BndType(i)=='R'
                warning('Riemann data may need adjustment: sign of velocity component changes')
            end
            % if along N axis, then change sign of flux
            if MDF2.bnd.MN(i,1)~=MDF2.bnd.MN(i,3)
                switch MDF2.bnd.Forcing(i)
                    case 'T'
                        for j = 1:length(MDF2.bct.Table)
                            if strcmp(MDF2.bct.Table(j).Location,MDF2.bnd.Name{i})
                                MDF2.bct.Table(j).Data(:,2:end) = -MDF2.bct.Table(j).Data(:,2:end);
                                break
                            end
                        end
                end
            end
        end
    end
end
%
if isfield(MDF2,'sta')
    MDF2.sta.MN = rotate(MDF2.sta.MN,MMAX);
end
%
if isfield(MDF2,'crs')
    MDF2.crs.MNMN(:,1:2) = rotate(MDF2.crs.MNMN(:,1:2),MMAX);
    MDF2.crs.MNMN(:,3:4) = rotate(MDF2.crs.MNMN(:,3:4),MMAX);
end


function mdfwrite(MDF,caseid,path)
if nargin<3
    path = '';
end
if isfield(MDF,'grd')
    filename = [caseid '.grd'];
    wlgrid('write',fullfile(path,filename),MDF.grd);
    MDF.mdf = inifile('set',MDF.mdf,'','Filcco',['#' filename '#']);
    %
    filename = [caseid '.enc'];
    MDF.mdf = inifile('set',MDF.mdf,'','Filgrd',['#' filename '#']);
end
%
if isfield(MDF,'dep')
    filename = [caseid '.dep'];
    wldep('write',fullfile(path,filename),MDF.dep);
    MDF.mdf = inifile('set',MDF.mdf,'','Fildep',['#' filename '#']);
end
%
if isfield(MDF,'dry')
    filename = [caseid '.dry'];
    d3d_attrib('write',fullfile(path,filename),MDF.dry);
    MDF.mdf = inifile('set',MDF.mdf,'','Fildry',['#' filename '#']);
end
%
if isfield(MDF,'thd')
    filename = [caseid '.thd'];
    d3d_attrib('write',fullfile(path,filename),MDF.thd);
    MDF.mdf = inifile('set',MDF.mdf,'','Filtd',['#' filename '#']);
end
%
if isfield(MDF,'bnd')
    filename = [caseid '.bnd'];
    d3d_attrib('write',fullfile(path,filename),MDF.bnd);
    MDF.mdf = inifile('set',MDF.mdf,'','Filbnd',['#' filename '#']);
end
%
if isfield(MDF,'bct')
    filename = [caseid '.bct'];
    bct_io('write',fullfile(path,filename),MDF.bct);
    MDF.mdf = inifile('set',MDF.mdf,'','FilbcT',['#' filename '#']);
end
%
if isfield(MDF,'bcc')
    filename = [caseid '.bcc'];
    bct_io('write',fullfile(path,filename),MDF.bcc);
    MDF.mdf = inifile('set',MDF.mdf,'','FilbcC',['#' filename '#']);
end
%
if isfield(MDF,'sed')
    filename = [caseid '.sed'];
    inifile('write',fullfile(path,filename),MDF.sed);
    MDF.mdf = inifile('set',MDF.mdf,'','Filsed',['#' filename '#']);
end
%
if isfield(MDF,'mor')
    filename = [caseid '.mor'];
    inifile('write',fullfile(path,filename),MDF.mor);
    MDF.mdf = inifile('set',MDF.mdf,'','Filmor',['#' filename '#']);
end
%
if isfield(MDF,'sta')
    filename = [caseid '.obs'];
    d3d_attrib('write',fullfile(path,filename),MDF.sta);
    MDF.mdf = inifile('set',MDF.mdf,'','Filsta',['#' filename '#']);
end
%
if isfield(MDF,'crs')
    filename = [caseid '.crs'];
    d3d_attrib('write',fullfile(path,filename),MDF.crs);
    MDF.mdf = inifile('set',MDF.mdf,'','Filcrs',['#' filename '#']);
end
%
filename = [caseid '.mdf'];
inifile('write',fullfile(path,filename),MDF.mdf);


function MDF = mdfread(filename)
MDF.mdf = inifile('open',filename);
mdfpath = fileparts(filename);
%
grdname = inifile('get',MDF.mdf,'','Filcco','');
grdname = rmhash(grdname);
if ~isempty(grdname)
    grdname = relpath(mdfpath,grdname);
    MDF.grd = wlgrid('read',grdname);
else
    error('Filcco is empty: grid in mdf file not yet supported.');
end
%
depname = inifile('get',MDF.mdf,'','Fildep','');
depname = rmhash(depname);
if ~isempty(depname)
    depname = relpath(mdfpath,depname);
    MDF.dep = wldep('read',depname,MDF.grd);
end
%
rghname = inifile('get',MDF.mdf,'','Filrgh','');
rghname = rmhash(rghname);
if ~isempty(rghname)
    warning('Support for Filrgh not yet implemented.')
end
%
dryname = inifile('get',MDF.mdf,'','Fildry','');
dryname = rmhash(dryname);
if ~isempty(dryname)
    dryname = relpath(mdfpath,dryname);
    MDF.dry = d3d_attrib('read',dryname);
end
%
thdname = inifile('get',MDF.mdf,'','Filtd','');
thdname = rmhash(thdname);
if ~isempty(thdname)
    thdname = relpath(mdfpath,thdname);
    MDF.thd = d3d_attrib('read',thdname);
end
%
wndname = inifile('get',MDF.mdf,'','Filwnd','');
wndname = rmhash(wndname);
if ~isempty(wndname)
    warning('Support for Filwnd not yet implemented.')
end
%
wndname = inifile('get',MDF.mdf,'','Filwp','');
wndname = rmhash(wndname);
if ~isempty(wndname)
    warning('Support for Filwp not yet implemented.')
end
%
wndname = inifile('get',MDF.mdf,'','Filwu','');
wndname = rmhash(wndname);
if ~isempty(wndname)
    warning('Support for Filwu not yet implemented.')
end
%
wndname = inifile('get',MDF.mdf,'','Filwv','');
wndname = rmhash(wndname);
if ~isempty(wndname)
    warning('Support for Filwv not yet implemented.')
end
%
bndname = inifile('get',MDF.mdf,'','Filbnd','');
bndname = rmhash(bndname);
if ~isempty(bndname)
    bndname = relpath(mdfpath,bndname);
    MDF.bnd = d3d_attrib('read',bndname);
    %
    bctname = inifile('get',MDF.mdf,'','FilbcT','');
    bctname = rmhash(bctname);
    if ~isempty(bctname)
        bctname = relpath(mdfpath,bctname);
        MDF.bct = bct_io('read',bctname);
    end
    %
    bcaname = inifile('get',MDF.mdf,'','Filana','');
    bcaname = rmhash(bcaname);
    if ~isempty(bcaname)
        warning('Support for Filana not yet implemented.')
    end
    %
    bccname = inifile('get',MDF.mdf,'','FilbcC','');
    bccname = rmhash(bccname);
    if ~isempty(bccname)
        bccname = relpath(mdfpath,bccname);
        MDF.bcc = bct_io('read',bccname);
    end
end
%
sedname = inifile('get',MDF.mdf,'','Filsed','');
sedname = rmhash(sedname);
if ~isempty(sedname)
    sedname = relpath(mdfpath,sedname);
    MDF.sed = inifile('open',sedname);
end
%
morname = inifile('get',MDF.mdf,'','Filmor','');
morname = rmhash(morname);
if ~isempty(morname)
    morname = relpath(mdfpath,morname);
    MDF.mor = inifile('open',morname);
end
%
staname = inifile('get',MDF.mdf,'','Filsta','');
staname = rmhash(staname);
if ~isempty(staname)
    staname = relpath(mdfpath,staname);
    MDF.sta = d3d_attrib('read',staname);
end
%
crsname = inifile('get',MDF.mdf,'','Filcrs','');
crsname = rmhash(crsname);
if ~isempty(crsname)
    crsname = relpath(mdfpath,crsname);
    MDF.crs = d3d_attrib('read',crsname);
end


function str = rmhash(str)
hashes = strfind(str,'#');
if length(hashes)>1
    str = str(hashes(1)+1:hashes(2)-1);
end


function pf = relpath(path,file)
if length(file)>1 && file(2)==':'
    pf = file;
else
    pf = fullfile(path,file);
end

