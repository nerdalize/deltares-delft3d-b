function setaxesprops(Parent,FullAxesType, ...
    dimension1,unit1,dimension2,unit2,dimension3,unit3)
%SETAXESPROPS Set appropriate axes properties.

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

if nargin==1
    update_axesprops(Parent)
    return
end

AxesType=full2basic_axestype(FullAxesType);
if ~ishandle(Parent)
else
    if isappdata(Parent,'AxesType')
        if ~isequal(getappdata(Parent,'AxesType'),FullAxesType)
            warning('AxesType mismatch: %s (set) vs. %s (new).',getappdata(Parent,'AxesType'),FullAxesType)
            FullAxesType = getappdata(Parent,'AxesType');
            AxesType = full2basic_axestype(FullAxesType);
        end
    end
    if nargin<3
        dimension1 = 'quantity';
        unit1 = '?';
    end
    if nargin<4
        dimension2 = 'quantity';
        unit2 = '?';
    end
    if nargin<5
        dimension3 = 'quantity';
        unit3 = '?';
    end
    %
    set(Parent,'layer','top')
    %
    % Get label handles. If the label is not empty, let the handle be empty
    % such that the label contents will not be overwritten.
    %
    switch AxesType
        %
        %----------------------------------------------------------------
        %
        case '<blocking>'
        case 'Time-Val'
            setlabel(Parent,'x','time','')
            setlabel(Parent,'y',dimension3,unit3)
            set_2d_axes_behavior(Parent)
        case 'Time-Z'
            setlabel(Parent,'x','time','')
            setlabel(Parent,'y',dimension3,unit3)
            set_2d_axes_behavior(Parent)
        case 'X-Time'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y','time','')
            set_2d_axes_behavior(Parent)
        case 'Time-X'
            setlabel(Parent,'x','time','')
            setlabel(Parent,'y',dimension1,unit1)
            set_2d_axes_behavior(Parent)
        case 'X-Time-Val'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y','time','')
            setlabel(Parent,'z',dimension3,unit3)
        case 'X-Time-Z'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y','time','')
            setlabel(Parent,'z',dimension3,unit3)
            %
            %-------------------------------------------------------------
            %
        case 'Val-Val'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y',dimension3,unit3)
            set_2d_axes_behavior(Parent)
        case 'X-Val'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y',dimension3,unit3)
            set_2d_axes_behavior(Parent)
        case 'Val-Z'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y',dimension3,unit3)
            set_2d_axes_behavior(Parent)
        case 'X-Y-Val'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y',dimension2,unit2)
            setlabel(Parent,'z',dimension3,unit3)
        case 'Lon-Lat-Val'
            setappdata(Parent,'LonLat',1)
            sethscale_lonlat(Parent)
            setlabel(Parent,'x','longitude','deg')
            setlabel(Parent,'y','latitude','deg')
            setlabel(Parent,'z',dimension3,unit3)
            %
            %-------------------------------------------------------------
            %
        case 'X-Y'
            sethscale(Parent,1)
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y',dimension2,unit2)
            set_2d_axes_behavior(Parent)
        case 'X-Z'
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y',dimension3,unit3)
            set_2d_axes_behavior(Parent)
        case 'X-Y-Z'
            sethscale(Parent,1)
            setlabel(Parent,'x',dimension1,unit1)
            setlabel(Parent,'y',dimension2,unit2)
            setlabel(Parent,'z',dimension3,unit3)
        case 'Lon-Lat'
            setappdata(Parent,'LonLat',1)
            sethscale_lonlat(Parent)
            setlabel(Parent,'x','longitude','deg')
            setlabel(Parent,'y','latitude','deg')
            set_2d_axes_behavior(Parent)
        case 'Lon-Lat-Z'
            setappdata(Parent,'LonLat',1)
            sethscale_lonlat(Parent)
            setlabel(Parent,'x','longitude','deg')
            setlabel(Parent,'y','latitude','deg')
            setlabel(Parent,'z',dimension3,unit3)
            %
            %-------------------------------------------------------------
            %
        otherwise
            AxisQuant = multiline(AxesType,'-','cell');
            for i = 1:length(AxisQuant)
                if i == 1
                    dir = 'x';
                    quantity = dimension1;
                    unit = unit1;
                elseif i == 2
                    dir = 'y';
                    quantity = dimension2;
                    unit = unit2;
                else
                    dir = 'z';
                    quantity = dimension3;
                    unit = unit3;
                end
                if isequal(AxisQuant{i},'Val')
                    quantity = dimension3;
                    unit = unit3;
                end
                setlabel(Parent,dir,quantity,unit)
            end
            if length(AxisQuant)<3
                set_2d_axes_behavior(Parent)
            end
    end
    %
    setappdata(Parent,'AxesType',FullAxesType)
    setappdata(Parent,'BasicAxesType',AxesType)
end


function sethscale_lonlat(Parent)
ylimv=get(Parent,'ylim');
if ylimv(1)<-90
    ylimv(1)=-90;
    if ylimv(2)<=ylimv(1)
        ylimv(2)=-89;
    end
end
if ylimv(2)>90
    ylimv(2)=90;
    if ylimv(1)>=ylimv(2)
        ylimv(1)=89;
    end
end
lat=mean(ylimv);
lat=min(max(lat,-89),89);
sethscale(Parent,cos(lat*pi/180))
set(Parent,'ylim',ylimv)


function sethscale(Parent,ratio)
if strcmp(get(Parent,'dataaspectratiomode'),'auto')
    set(Parent,'dataaspectratio',[1 ratio 1/30])
else
    da = get(Parent,'dataaspectratio');
    da(2) = da(1)*ratio;
    set(Parent,'dataaspectratio',da)
end


function axestype = full2basic_axestype(axestype)
unitsloc=strfind(axestype,' [');
for i=length(unitsloc):-1:1
    unitsclose=strfind(axestype(unitsloc(i):end),']');
    if ~isempty(unitsclose)
        axestype(:,unitsloc(i)+(0:max(unitsclose)-1))=[];
    end
end


function update_axesprops(Parent)
for d = 'xyz'
    update_axticks(Parent,d)
end
if isequal(getappdata(Parent,'LonLat'),1)
    sethscale_lonlat(Parent)
end


function setlabel(ax,dir,quantity,unit)
axlabel = get(ax,[dir 'label']);
if ~isempty(get(axlabel,'string')) && isempty(get(axlabel,'userdata'))
    return
end
%
arrow = '\rightarrow';
if ~isempty(unit)
    dimstr = sprintf('%s (%s) %s',quantity,unit,arrow);
else
    dimstr = sprintf('%s %s',quantity,arrow);
end
%
set(axlabel,'string',dimstr)
set(axlabel,'userdata','autolabel')
setappdata(ax,[dir 'quantity'],quantity)
setappdata(ax,[dir 'unit'],unit)
update_axticks(ax,dir)


function update_axticks(Parent,dir)
quantity = getappdata(Parent,[dir 'quantity']);
if ~isempty(quantity)
    switch quantity
        case {'longitude','latitude'}
            set(Parent,[dir 'ticklabelmode'],'auto',[dir 'tickmode'],'auto');
            tick(Parent,dir,quantity);
        case {'time'}
            set(Parent,[dir 'ticklabelmode'],'auto',[dir 'tickmode'],'auto');
            tick(Parent,dir,'autodate');
        case {'distance','x coordinate','y coordinate'}
            if strcmp(getappdata(Parent,[dir 'unit']),'m')
               distanceticks(Parent,dir)
            end
        otherwise
            if strncmp(quantity,'distance along',14) && ...
                    strcmp(getappdata(Parent,[dir 'unit']),'m')
               distanceticks(Parent,dir)
            end
    end
end

function distanceticks(ax,dir)
unitQ=[0.001  1  1000];
unitT=[0      1  10000];
unitS={'mm'  'm' 'km'};
%
dx=max(abs(get(ax,[dir 'lim'])));
scale=sum(dx>unitT);
%
set(ax,[dir 'ticklabelmode'],'auto',[dir 'tickmode'],'auto');
tick(ax,dir,'%g',1/unitQ(scale))
%
quantity = getappdata(ax,[dir 'quantity']);
%setappdata(ax,[dir 'unit'],unitS{scale})
set(get(ax,[dir 'label']),'string',sprintf('%s (%s) \\rightarrow',quantity,unitS{scale}))

function set_2d_axes_behavior(ax)
try
    bh = hggetbehavior(ax,'rotate3d');
    set(bh,'Enable',false);
catch
end
