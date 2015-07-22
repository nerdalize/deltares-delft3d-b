function [ax,axoptions]=qp_createaxes(fig)
%QP_CREATEAXES Create an axes for plotting.

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

ax=[];
axoptions.Editable=1;
axoptions.Type='undefined';
axoptions.Name='[axes]';

% which kind of axes should be created?
labels={'One Plot', ...
    'User Selected Subplot', ...
    'User Positioned Subplot'};
if d3d_qp('iswl')
    labels=cat(2,labels, ...
        {'Deltares Logo', ...
        'Deltares Logo'});
end
[axtype,axname]=ui_typeandname(labels);

if isempty(axtype) % cancel pressed?
    return
end

switch axtype
    case 'One Plot'
        ax=Local_subplot(fig,1,1,1);
        axoptions.Name=axname;
        set(ax,'tag',axoptions.Name,'userdata',axoptions);

    case 'User Selected Subplot'
        labels={'Number of Plots per Column','2'; ...
            'Number of Plots per Row','2'; ...
            'Plot Number(s)','1'};
        inp=stdinputdlg(labels(:,1),'Please Specify',1,labels(:,2));
        if ~isempty(inp)
            lasterr='';
            Correct=1;
            try
                NR=str2vec(inp{1},'%d');
                NC=str2vec(inp{2},'%d');
                NP=str2vec(inp{3},'%d');
            catch
                NR=1;
                NC=1;
                NP=1;
                Correct=0;
            end
            Correct = Correct & isnumeric(NR) & isequal(size(NR),[1 1]) & ~isnan(NR) & ~isinf(NR) & (NR>0) & (NR==round(NR));
            Correct = Correct & isnumeric(NC) & isequal(size(NC),[1 1]) & ~isnan(NC) & ~isinf(NC) & (NC>0) & (NC==round(NC));
            Correct = Correct & isnumeric(NP) & isequal(size(NP,1),1) & ~any(isnan(NP(:))) & ~any(isinf(NP(:))) & all(NP(:)>0) & all(NP(:)==round(NP(:)));
            if Correct
                for i=length(NP(:)):-1:1;
                    ax(i)=Local_subplot(fig,NR,NC,NP(i));
                    axoptions.Name=sprintf([axname ' (%d,%d,%d)'],NR,NC,NP(i));
                    set(ax(i),'tag',axoptions.Name,'userdata',axoptions);
                end
            else
                Str=lasterr;
                if isempty(Str)
                    Str='Invalid numbers specified.';
                end
                ui_message('error',Str);
            end
        end

    case {'User Positioned Subplot','Deltares Logo','Deltares Logo'}
        Pos=getnormpos(fig);
        ax=axes('parent',fig,'units','normalized','position',Pos);
        switch axtype
            case 'User Positioned Subplot'
            case 'Deltares Logo'
                xx_logo('deltares',ax);
                axname=axtype;
                setappdata(ax,'AxesType','<special>')
            case 'Deltares Logo'
                xx_logo('dh',ax,1,'k');
                axname=axtype;
                setappdata(ax,'AxesType','<special>')
        end
        axoptions.Name=axname;
        set(ax,'tag',axname,'userdata',axoptions);

    otherwise
        Str=sprintf('Requested axes type not yet implemented.');
        ui_message('warning',Str);
        return
end
qp_defaultaxessettings(ax)


function ax = Local_subplot(fig,nrows, ncols, thisPlot)
%LOCAL_SUBPLOT Create axes in tiled positions.
%   LOCAL_SUBPLOT(fig,m,n,p), breaks the Figure <fig> window into
%   an m-by-n matrix of small axes, selects the p-th axes for
%   for the current plot, and returns the axis handle.  The axes
%   are counted along the top row of the Figure window, then the
%   second row, etc.

% This is the percent offset from the subplot grid of the plotbox.
PERC_OFFSET_L = 2*0.09;
PERC_OFFSET_R = 2*0.045;
PERC_OFFSET_B = PERC_OFFSET_L;
PERC_OFFSET_T = PERC_OFFSET_R;
if nrows > 2
    PERC_OFFSET_T = 0.9*PERC_OFFSET_T;
    PERC_OFFSET_B = 0.9*PERC_OFFSET_B;
end
if ncols > 2
    PERC_OFFSET_L = 0.9*PERC_OFFSET_L;
    PERC_OFFSET_R = 0.9*PERC_OFFSET_R;
end

row = (nrows-1) -fix((thisPlot-1)/ncols);
col = rem (thisPlot-1, ncols);

% For this to work the default axes position must be in normalized coordinates
def_pos = [.13 .11 .775 .815];

col_offset = def_pos(3)*(PERC_OFFSET_L+PERC_OFFSET_R)/ ...
    (ncols-PERC_OFFSET_L-PERC_OFFSET_R);
row_offset = def_pos(4)*(PERC_OFFSET_B+PERC_OFFSET_T)/ ...
    (nrows-PERC_OFFSET_B-PERC_OFFSET_T);
totalwidth = def_pos(3) + col_offset;
totalheight = def_pos(4) + row_offset;
width = totalwidth/ncols*(max(col)-min(col)+1)-col_offset;
height = totalheight/nrows*(max(row)-min(row)+1)-row_offset;
position = [def_pos(1)+min(col)*totalwidth/ncols ...
    def_pos(2)+min(row)*totalheight/nrows ...
    width height];
if width <= 0.5*totalwidth/ncols
    position(1) = def_pos(1)+min(col)*(def_pos(3)/ncols);
    position(3) = 0.7*(def_pos(3)/ncols)*(max(col)-min(col)+1);
end
if height <= 0.5*totalheight/nrows
    position(2) = def_pos(2)+min(row)*(def_pos(4)/nrows);
    position(4) = 0.7*(def_pos(4)/nrows)*(max(row)-min(row)+1);
end

% create the axis:
if isappdata(fig,'MaximumPlotExtent')
    plotbox = getappdata(fig,'MaximumPlotExtent');
else
    plotbox = [0 0 1 1];
end
position = [plotbox(1:2) 0 0] + position.*plotbox([3 4 3 4]);
ax = axes('parent',fig,'units','normal','Position', position);
set(ax,'units',get(fig,'defaultaxesunits'))
