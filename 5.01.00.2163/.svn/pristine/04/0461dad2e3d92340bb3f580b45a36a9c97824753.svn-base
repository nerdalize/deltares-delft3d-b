function hNew=gencontour(hOld,Ops,Parent,X,Y,Z,Thresholds)
%GENCONTOUR Generic plot routine for contour plot.

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

delete(hOld);
compat7={};
warnstate=[];
if matlabversionnumber>=7
    compat7={'v6'};
    %
    % Recent versions produce warning if 'v6' is used.
    % Suppress this warning.
    %
    warnstate = warning('query','MATLAB:contour:DeprecatedV6Argument');
    warning('off','MATLAB:contour:DeprecatedV6Argument')
    warnstate(2) = warning('query','MATLAB:contourf:DeprecatedV6Argument');
    warning('off','MATLAB:contourf:DeprecatedV6Argument')
    warnstate(3) = warning('query','MATLAB:contour3:DeprecatedErrorOutputArgument');
    warning('off','MATLAB:contour3:DeprecatedErrorOutputArgument')
    warnstate(4) = warning('query','MATLAB:contours:DeprecatedErrorOutputArgument');
    warning('off','MATLAB:contours:DeprecatedErrorOutputArgument')
end

%
% Make sure that we don't error out this routine before the 'v6' warning
% has been set back to its original setting.
%
e = '';
try
    Thresholds(Thresholds==-inf)=-realmax;
    Thresholds(Thresholds==inf)=realmax;
    switch Ops.presentationtype
        case 'contour lines'
            [dummy,hNew]=contour(compat7{:},X,Y,Z,Thresholds,'k');
            set(hNew,'color',Ops.colour)
        case 'coloured contour lines'
            [dummy,hNew]=contour(compat7{:},X,Y,Z,Thresholds);
            for i=1:length(hNew)
                hNewC=get(hNew(i),'cdata');
                ii = max(find(Thresholds<=hNewC(1)));
                set(hNew(i),'cdata',repmat(ii,size(hNewC)))
            end
            set(hNew, ...
                'linewidth',Ops.linewidth, ...
                'linestyle',Ops.linestyle, ...
                'marker',Ops.marker, ...
                'markeredgecolor',Ops.markercolour, ...
                'markerfacecolor',Ops.markerfillcolour);
        case 'contour patches'
            if any(~isnan(Z(:)))
                [dummy,hNew]=contourfcorr(X,Y,Z,Thresholds);
                for i=1:length(hNew)
                    hNewC=get(hNew(i),'cdata');
                    ii = max(find(Thresholds<=hNewC(1)));
                    set(hNew(i),'cdata',ii)
                end
            else
                hNew=patch(1,1,1,'xdata',[],'ydata',[],'cdata',[]);
            end
            set(hNew,'edgecolor','none');
            hNew=flipud(hNew);
        case 'contour patches with lines'
            if any(~isnan(Z(:)))
                [dummy,hNew]=contourfcorr(X,Y,Z,Thresholds);
                for i=1:length(hNew)
                    hNewC=get(hNew(i),'cdata');
                    ii = max(find(Thresholds<=hNewC(1)));
                    set(hNew(i),'cdata',ii)
                end
            else
                hNew=patch(1,1,1,'xdata',[],'ydata',[],'cdata',[]);
            end
            set(hNew,'edgecolor',Ops.colour, ...
                'linewidth',Ops.linewidth, ...
                'linestyle',Ops.linestyle, ...
                'marker',Ops.marker, ...
                'markeredgecolor',Ops.markercolour, ...
                'markerfacecolor',Ops.markerfillcolour);
            hNew=flipud(hNew);
    end
    set(hNew,'parent',Parent)
    if ~isnan(min(Thresholds)) && ~strcmp(Ops.presentationtype,'contour lines')
        set(Parent,'clim',[1 length(Thresholds)]);
    end
catch
    e = lasterr;
end

%
% Set all warnings back to original setting.
%
if ~isempty(warnstate)
    warning(warnstate); 
end

if ~isempty(e)
    error(e);
end
