function [cout,H,CS] = contourfcorr(varargin)
%CONTOURFCORR Filled contour plot (corrected).
%   CONTOURF(...) is the same as CONTOUR(...) except that the contours
%   are filled.  Areas of the data at or above a given level are filled.
%   Areas below a level are either left blank or are filled by a lower
%   level.  NaN's in the data leave holes in the filled contour plot.
%
%   C = CONTOURF(...) returns contour matrix C as described in CONTOURC
%   and used by CLABEL.
%
%   [C,H,CF] = CONTOURF(...) also returns a column vector H of handles
%   to PATCH objects and the contour matrix CF for the filled areas.
%   The UserData property of each object contains the height value for each
%   contour.
%
%   Example
%      z=peaks; contourf(z), hold on, shading flat
%      [c,h]=contour(z,'k-'); clabel(c,h), colorbar
%
%   See also CONTOUR, CONTOUR3, CLABEL, COLORBAR.

% To correct erroneous ordering of the full area patches
% when the grid is clipped using NaN in the values matrix
% and a constant value for the coordinates. Reason for
% error: the NaNs are replaced by a small value, for
% contour lines close to that value the enclosed area is
% smaller (goes to zero) than the area enclosed by a
% contour line of approximately min(val). This causes
% the contours for smaller values to be plotted after the
% contours for the larger values. The lowest value will
% appear to belong to lie between the smallest thresholds.
% The following code reproduces this phenomenon:
%
% [xx,yy]=meshgrid(1:10,1:10);
% xx([1 end],:)=0;
% xx(:,[1 end])=0;
% yy=xx';
% zz=ones(10,10);
% zz(5,5)=2;
% zz(xx==0)=NaN;
% surf(xx,yy,zz)
% figure;
% contourf(xx,yy,zz,[-2 -1 0 1.5])
% colorbar
%
% This function solves this problem by basing the ordering on
% a dummy, rectangular grid.

%   Author: R. Pawlowicz (IOS)  rich@ios.bc.ca   12/14/94
%   Copyright (c) 1984-98 by The MathWorks, Inc.
%   $Revision$  $Date$
%
%   Correction by H.R.A. Jagers (Deltares)
%                        bert.jagers@deltares.nl, 2001/07/18

error(nargchk(1,5,nargin));

% Check for empty arguments.
for i = 1:nargin
    if isempty(varargin{i})
        error ('Invalid Argument - Input matrix is empty');
    end
end

% Trim off the last arg if it's a string (line_spec).
nin = nargin;
if isstr(varargin{end})
    [lin,col,mark,msg] = colstyle(varargin{end});
    if ~isempty(msg), error(msg); end
    nin = nin - 1;
else
    lin = '';
    col = '';
end

if (nin == 4),
    [x,y,z,nv] = deal(varargin{1:4});
    if (size(y,1)==1), y=y'; end;
    if (size(x,2)==1), x=x'; end;
    [mz,nz] = size(z);
elseif (nin == 3),
    [x,y,z] = deal(varargin{1:3});
    nv = [];
    if (size(y,1)==1), y=y'; end;
    if (size(x,2)==1), x=x'; end;
    [mz,nz] = size(z);
elseif (nin == 2),
    [z,nv] = deal(varargin{1:2});
    [mz,nz] = size(z);
    x = 1:nz;
    y = (1:mz)';
elseif (nin == 1),
    z = varargin{1};
    [mz,nz] = size(z);
    x = 1:nz;
    y = (1:mz)';
    nv = [];
end
[mz0,nz0] = size(z);
x0 = 1:nz0;
y0 = (1:mz0)';

if nin <= 2,
    [mc,nc] = size(varargin{1});
    lims = [1 nc 1 mc];
else
    lims = [min(varargin{1}(:)),max(varargin{1}(:)), ...
        min(varargin{2}(:)),max(varargin{2}(:))];
end

i = find(isfinite(z));
minz = min(z(i));
maxz = max(z(i));

% Generate default contour levels if they aren't specified
if length(nv) <= 1
    if isempty(nv)
        CS=contourc([minz maxz ; minz maxz]);
    else
        CS=contourc([minz maxz ; minz maxz],nv);
    end

    % Find the levels
    ii = 1;
    nv = minz; % Include minz so that the contours are totally filled
    while (ii < size(CS,2)),
        nv=[nv CS(1,ii)];
        ii = ii + CS(2,ii) + 1;
    end
end

% Don't fill contours below the lowest level specified in nv.
% To fill all contours, specify a value of nv lower than the
% minimum of the surface.
draw_min=0;
if any(nv <= minz),
    draw_min=1;
end

% Get the unique levels
nv = sort([minz nv(:)']);
zi = [1, find(diff(nv))+1];
nv = nv(zi);

% Surround the matrix by a very low region to get closed contours, and
% replace any NaN with low numbers as well.

zz=[ repmat(NaN,1,nz+2) ; repmat(NaN,mz,1) z repmat(NaN,mz,1) ; repmat(NaN,1,nz+2)];
kk=isnan(zz(:));
zz(kk)=-realmax;
% using -realmax instead of
% * -inf since this seems to result in contours drawn at value+eps rather
%   than value-eps at least for the example given above in R2009B
% * minz-1e4*(maxz-minz) since this moves contours too far in the direction
%   of NaN missing data points.

xx0 = [2*x0(:,1)-x0(:,2), x0, 2*x0(:,nz0)-x0(:,nz0-1)];
yy0 = [2*y0(1,:)-y0(2,:); y0; 2*y0(mz0,:)-y0(mz0-1,:)];
[CS,msg]=contours(xx0,yy0,zz,nv);
if ~isempty(msg), error(msg); end

% Find the indices of the curves in the c matrix, and get the
% area of closed curves in order to draw patches correctly.
ii = 1;
ncurves = 0;
I = [];
Area=[];
while (ii < size(CS,2)),
    nl=CS(2,ii);
    ncurves = ncurves + 1;
    I(ncurves) = ii;
    if nl==1
        Area(ncurves) = 0;
    else
        xp=CS(1,ii+(1:nl));  % First patch
        yp=CS(2,ii+(1:nl));
        Area(ncurves)=sum( diff(xp).*(yp(1:nl-1)+yp(2:nl))/2 );
    end
    ii = ii + nl + 1;
end

newplot;
if ~ishold,
    view(2);
    set(gca,'box','on');
    set(gca,'xlim',lims(1:2),'ylim',lims(3:4))
end

% Plot patches in order of decreasing size. This makes sure that
% all the levels get drawn, not matter if we are going up a hill or
% down into a hole. When going down we shift levels though, you can
% tell whether we are going up or down by checking the sign of the
% area (since curves are oriented so that the high side is always
% the same side). Lowest curve is largest and encloses higher data
% always.

H=[];
[FA,IA]=sort(-abs(Area));
if ~isstr(get(gca,'color')),
    bg = get(gca,'color');
else
    bg = get(gcf,'color');
end
if isempty(col)
    edgec = get(gcf,'defaultsurfaceedgecolor');
else
    edgec = col;
end
if isempty(lin)
    edgestyle = get(gcf,'defaultpatchlinestyle');
else
    edgestyle = lin;
end

% Tolerance for edge comparison
xtol = 0.1*(lims(2)-lims(1))/size(z,2);
ytol = 0.1*(lims(4)-lims(3))/size(z,1);

if nargout>0
    cout = [];
end
xx=x(:)'; yy=y(:)';
szx=size(x);
if min(szx)==1
    szn=length(yy);
    szm=length(xx);
else
    szn=szx(1);
    szm=szx(2);
end
for jj=IA,
    nl=CS(2,I(jj));
    lev=CS(1,I(jj));
    if (lev ~= minz | draw_min ),
        mp=CS(1,I(jj)+(1:nl));
        np=CS(2,I(jj)+(1:nl));
        ipm=floor(mp);
        ipn=floor(np);
        ipm1=min(szm,ipm+1); % prevent index szm+1
        ipn1=min(szn,ipn+1); % prevent index szn+1
        dmp=mp-ipm;
        dnp=np-ipn;
        ipm=max(1,ipm); % prevent index 0
        ipn=max(1,ipn); % prevent index 0
        if min(szx)==1
            xp=(1-dmp).*xx(ipm)+dmp.*xx(ipm1);
            yp=(1-dnp).*yy(ipn)+dnp.*yy(ipn1);
        else
            ip=sub2ind(szx,ipn,ipm);
            ip_m=sub2ind(szx,ipn,ipm1);
            ip_n=sub2ind(szx,ipn1,ipm);
            ip_mn=sub2ind(szx,ipn1,ipm1);
            xp=(1-dmp).*((1-dnp).*xx(ip)+dnp.*xx(ip_n))+dmp.*((1-dnp).*xx(ip_m)+dnp.*xx(ip_mn));
            yp=(1-dmp).*((1-dnp).*yy(ip)+dnp.*yy(ip_n))+dmp.*((1-dnp).*yy(ip_m)+dnp.*yy(ip_mn));
        end
        CS(1,I(jj)+(1:nl))=xp;
        CS(2,I(jj)+(1:nl))=yp;
        if (sign(Area(jj)) ~=sign(Area(IA(1))) ),
            kk=find(nv==lev);
            if (kk>1+sum(nv<=minz)*(~draw_min)),
                lev=nv(kk-1);
            else
                lev=NaN;         % missing data section
            end
        end

        if (isfinite(lev)),
            H=[H;patch(xp,yp,lev,'facecolor','flat','edgecolor',edgec, ...
                'linestyle',edgestyle,'userdata',lev)];
        else
            H=[H;patch(xp,yp,lev,'facecolor',bg,'edgecolor',edgec, ...
                'linestyle',edgestyle,'userdata',CS(1,I(jj)))];
        end

        if nargout>0
            xp(abs(xp - lims(1)) < xtol | abs(xp - lims(2)) < xtol) = NaN;
            yp(abs(yp - lims(3)) < ytol | abs(yp - lims(4)) < ytol) = NaN;
            cout = [cout,[lev xp;nl yp]];
        end
    end
end

numPatches = length(H);
if numPatches>1
    for i=1:numPatches
        set(H(i), 'faceoffsetfactor', 0, 'faceoffsetbias', (1e-3)+(numPatches-i)/(numPatches-1)/30);
    end
end
