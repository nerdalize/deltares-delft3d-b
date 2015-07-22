function hBorder=md_paper(cmd,varargin)
%MD_PAPER Add border to plot.
%   MD_PAPER(PAPERTYPE,BORDERTYPE) sets the appropriate page size for the
%   current figure and adds an empty border to it. Right click on the
%   border to fill in the texts via a dialog. PAPERTYPE should equal a
%   papertype supported by MATLAB followed by 'p' (portrait) or 'l'
%   (landscape), e.g. 'a4p'. BORDERTYPE should equal one of the following
%   strings
%      'none'       no border (default) just set paper size
%      '1box'       Framed figure with just one 2cm high text box and 1 cm
%                   margins at all sides.
%      '2box'       Non-framed figure with two textboxes with 3 cm margin
%                   at left side and 1 cm margins at the other sides.
%      '7box'       Framed figure with seven textboxes (classic Delft
%                   Hydraulics layout) with 1 cm margins at all sides.
%   or an explicit border structure BSTRUCT may be specified.
%                                                         |             |
%                   |             |                       |    .7box.   |
%                   |    .1box.   |         .2box.        |_________ _ _|
%                   |_____________|     ___ _________     |         |_|_|
%                   |             |    |   |         |    |_________|___|
%                   |_____________|    |___|_________|    |_________|_|_|
%
%   MD_PAPER(PAPERTYPE,BORDERTYPE,CTEXTS) does the same thing, but also
%   fills in the texts by using entries from the CTEXTS cell array of
%   strings. Right click on the border or texts to add or edit texts.
%
%   MD_PAPER(...,'OptionName1',OptionValue1,'OptionName2',OptionValue2,...)
%   uses non-default values for the specified options.
%
%   Supported options (also valid fields for border structure BSTRUCT):
%      'Margin'     [left bottom right top] margin
%      'Border'     draw border 1=yes/0=no
%      'LineWidth'  line width of borders and boxes
%      'Color'      line and text color
%
%   Additional fields for border structure BSTRUCT:
%      'Box'        matrix containing indices of textboxes
%      'HTabs'      relative widths of boxes
%                   length of the vector should match size(Box,2)
%      'HRange'     (maximum) width of all boxes together
%      'VTabs'      relative heights of boxes
%                   length of the vector should match size(Box,1)
%      'VRange'     (maximum) height of all boxes together
%      'Bold'       flags for printing text in bold font
%                   length of the vector should match the number
%                   of textboxes
%      'PlotText'   cell array containing default texts
%                   length of the vector should match the number
%                   of textboxes
%
%   hBorder = MD_PAPER('no edit',...) right click editing disabled. Use 
%   MD_PAPER('edit',hBorder) to edit the texts via a dialog.
%
%   NOTE: There are some compatibility problems with the LEGEND function.
%   When the border is added, all subplots are made slightly smaller. The
%   LEGEND function detects this and resets the subplots to their original
%   size. As a workaround use the following approach:
%
%         AX1=subplot(2,1,1);
%         x=0:.1:10; plot(x,sin(x));
%         md_paper('a4p','wl');
%         legend(AX1,'sine');
%
%
%   Backward compatibility:
%
%   MD_PAPER(PAPERTYPE) where PAPERTYPE can be either 'portrait' or
%   'landscape' adds "Deltares (date and time)" to a figure and sets the
%   page size to A4 portrait/landscape.
%
%   MD_PAPER(PAPERTYPE,'String') where PAPERTYPE can be either 'portrait'
%   or 'landscape' adds "String (date and time)" to a figure and sets the
%   page size to A4 portrait/landscape.
%
%   MD_PAPER(PAPERTYPE,'String1','String2',...) where PAPERTYPE can be
%   either 'portrait' or 'landscape' adds the 7 box border to the figure
%   and sets the page size to A4 portrait/landscape. Right click on the
%   text to edit the texts.

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
    cmd='portrait';
end

INP=varargin;

NoEdit=0;
if strcmp(cmd,'no edit')
    NoEdit=1;
    cmd=INP{1};
    INP=INP(2:end);
end

switch lower(cmd)
    case {'apply','done'}
        Fig=gcbf;
        gcba=get(Fig,'userdata');
        if ~ishandle(gcba),
            delete(gcbf);
            return;
        end;
        hpts=findobj(gcba,'type','text');
        for i=1:7
            hplottext=findobj(hpts,'flat','tag',sprintf('plottext%i',i));
            hedittext=findobj(Fig,'tag',sprintf('Text%i',i));
            str=get(hedittext,'string');
            if isempty(str)
                str={' '};
            end
            set(hplottext,'string',str);
        end
        leftpage=findobj(Fig,'tag','LeftPage');
        switch get(get(gcba,'parent'),'paperorientation'),
            case 'portrait'
                if get(leftpage,'value'), %=1
                    set(gcba,'xdir','reverse');
                else %=0
                    set(gcba,'xdir','normal');
                end
            case 'landscape'
                if get(leftpage,'value'), %=1
                    set(gcba,'ydir','reverse');
                else %=0
                    set(gcba,'ydir','normal');
                end
        end
        if strcmp(cmd,'done')
            delete(gcbf);
        end
    case 'edit'
        if (nargin==1) && (isempty(gcbf) || ~strcmp(get(gcbf,'selectiontype'),'alt'))
            return;
        elseif nargin==2,
            gcba=INP{1};
        else
            gcba=get(gcbo,'parent');
        end;
        if strcmp(get(gcba,'type'),'figure')
            gcba=findobj(gcba,'type','axes','tag','border');
        end
        %first check whether it already exists ...
        Fig=findobj(allchild(0),'type','figure','tag','Border manager for Matlab (c)');
        if ~isempty(Fig)
            for f=transpose(Fig),
                if isequal(get(f,'userdata'),gcba)
                    set(f,'visible','on');
                    return
                end
            end
        end
        Fig=Local_ui_paper(gcba);

        fig=get(gcba,'parent');
        HandleStr=[' ' num2str(fig)];
        StringStr=get(fig,'name');
        if strcmp(get(fig,'numbertitle'),'on')
            if isempty(StringStr)
                StringStr=['Figure No.' HandleStr];
            else
                StringStr=['Figure No.' HandleStr ':' StringStr];
            end
        end
        set(Fig,'name',[get(Fig,'name') ' for ' StringStr]);

        hpts=findobj(gcba,'type','text');
        for i=1:7
            hplottext=findobj(hpts,'flat','tag',sprintf('plottext%i',i));
            hedittext=findobj(Fig,'tag',sprintf('Text%i',i));
            set(hedittext,'string',get(hplottext,'string'));
        end
        set(Fig,'userdata',gcba);
        leftpage=findobj(Fig,'tag','LeftPage');
        switch get(get(gcba,'parent'),'paperorientation'),
            case 'portrait',
                set(leftpage,'value',strcmp(get(gcba,'xdir'),'reverse'));
            case 'landscape',
                set(leftpage,'value',strcmp(get(gcba,'ydir'),'reverse'));
        end
    otherwise
        Orientation=lower(cmd);
        if (strcmp(Orientation,'portrait') || strcmp(Orientation,'landscape')) && length(INP)<2
            hTempBorder=SimpleBorder(Orientation,INP{:});
        else
            if strcmp(Orientation,'portrait')
                Orientation='a4p';
                BFormat='7box';
                INP={INP};
            elseif strcmp(Orientation,'landscape')
                Orientation='a4l';
                BFormat='7box';
                INP={INP};
            else
                if isempty(INP)
                    BFormat='none';
                else
                    BFormat=INP{1};
                    INP(1)=[];
                end
            end
            hTempBorder=Local_createborder(NoEdit,Orientation,BFormat,INP{:});
        end
        if nargout>0,
            hBorder=hTempBorder;
        end
end


function organization=getorg
try
    organization = qp_settings('organizationname'); 
catch
    organization = 'Deltares';
end

function hBorder=SimpleBorder(Orientation,varargin)
if ~isempty(varargin)
    if isempty(varargin{1}),
        PlotText=locDateStr;
    else
        PlotText=[varargin{1},' (',locDateStr,')'];
    end
else
    PlotText=[getorg ' (',locDateStr,')'];
end
%
[ax,fg,allchld,allax,xmax,ymax,hBorder]=CreateBorderAxes('a4letter',Orientation);
%
switch Orientation
    case 'portrait'
        text(0.98*xmax,0.02*ymax, ...
            PlotText, ...
            'parent',hBorder', ...
            'fontsize',5, ...
            'horizontalalignment','right', ...
            'verticalalignment','bottom');
    case 'landscape'
        text(0.02*xmax,0.02*ymax, ...
            PlotText, ...
            'parent',hBorder', ...
            'fontsize',5, ...
            'rotation',270, ...
            'horizontalalignment','right', ...
            'verticalalignment','bottom');
end
%
AdjustFigPos(ax,fg,Orientation)
set(fg,'children',[allchld;hBorder]);


function hBorder=Local_createborder(NoEdit,Orientation,BFormat,varargin)
if nargin<4
    Strings={};
    INP={};
elseif iscell(varargin{1})
    Strings=varargin{1};
    INP=varargin(2:end);
else
    Strings={};
    INP=varargin;
end
%
% Set/unset buttondownfunction
%
if NoEdit
    BDFunction='';
else
    BDFunction='md_paper edit';
end
%
% Set up border defaults ...
%
Border=1;
Margin=[1 1 1 1];
LineWidth=1;
Color='k';
HTabs=1; HRange=inf;
VTabs=[0.5 0.5]; VRange=2;
Box=[1;1];
Bold=0;
PlotText={' '};
if isstruct(BFormat)
    %
    % User specified border ...
    %
    if isfield(BFormat,'Border'), Border=BFormat.Border; end
    if isfield(BFormat,'Margin'), Margin=BFormat.Margin; end
    if isfield(BFormat,'LineWidth'), LineWidth=BFormat.LineWidth; end
    if isfield(BFormat,'Color'), Color=BFormat.Color; end
    if isfield(BFormat,'HTabs'), HTabs=BFormat.HTabs; end
    if isfield(BFormat,'HRange'), HRange=BFormat.HRange; end
    if isfield(BFormat,'VTabs'), VTabs=BFormat.VTabs; end
    if isfield(BFormat,'VRange'), VRange=BFormat.VRange; end
    if isfield(BFormat,'Box'),
        Box=BFormat.Box;
        Bold=zeros(1,max(Box(:)));
        PlotText(1,1:max(Box(:)))={''};
    end
    if isfield(BFormat,'Bold'), Bold=BFormat.Bold; end
    if isfield(BFormat,'PlotText'), PlotText=BFormat.PlotText; end
    if length(HTabs(:))~=size(Box,2)
        error('Inconsistent frame type')
    end
    if length(VTabs(:))~=size(Box,1)
        error('Inconsistent frame type')
    end
    if length(Bold(:))~=max(Box(:))
        error('Length of bold vector does not match number of texts')
    end
    if ~iscell(PlotText)
        error('PlotText should be a cell array')
    elseif length(PlotText(:))~=max(Box(:))
        error('Length of plot text vector does not match number of texts')
    end
else
    %
    % Predefined borders ...
    %
    switch lower(BFormat)
        case {'1box',''}
        case 'none'
            Border=0;
            Margin=[0 0 0 0];
            LineWidth=1.5;
            Color='k';
            HTabs=[]; HRange=0;
            VTabs=[]; VRange=0;
            Box=[];
            Bold=[];
            PlotText={};
        case {'7box','wl'}
            Border=1;
            Margin=[1 1 1 1];
            LineWidth=1.5;
            Color='k';
            HTabs=[0.68 0.16 0.16]; HRange=19;
            VTabs=[1 1 1]/3;        VRange=2.7;
            Box=[1 2 3
                1 4 4
                7 5 6];
            Bold=[0 0 0 0 0 0 1];
            PlotText={' '  ' '  ' '  ' '  ' '  ' '  getorg};
        case {'2box','spankracht'}
            Border=0;
            Margin=[3 1 1 1];
            LineWidth=0.5;
            Color='k';
            HTabs=[0.32 0.68]; HRange=inf;
            VTabs=[0.5 0.5];   VRange=1.4;
            Box=[1 2
                1 2];
            Bold=[0 0];
            PlotText={' '  ' '};
        otherwise
            error('Unsupported border format: %s',BFormat)
    end
end
%
% Process options
%
i=1;
while i<=length(INP)
    if ~ischar(INP{i})
        error('Invalid option.');
    end
    switch lower(INP{i})
        case 'margin'
            i=i+1;
            if isequal(size(INP{i}),[1 1])
                Margin=[INP{i} INP{i} INP{i} INP{i}];
            else
                Margin=INP{i};
            end
        case 'linewidth'
            i=i+1;
            LineWidth=INP{i};
        case 'color'
            i=i+1;
            Color=INP{i};
        case 'border'
            i=i+1;
            if ischar(INP{i})
                Border=isempty(strmatch(INP{i},{'off','no'}));
            else
                Border=INP{i};
            end
        otherwise
            error('Invalid option: %s',INP{i})
    end
    i=i+1;
end
%
% Store fields in records
%
BFormat=[];
BFormat.Border=Border;
BFormat.Margin=Margin;
BFormat.LineWidth=LineWidth;
BFormat.Color=Color;
BFormat.HTabs=HTabs;
BFormat.HRange=HRange;
BFormat.VTabs=VTabs;
BFormat.VRange=VRange;
BFormat.Box=Box;
BFormat.Bold=Bold;
%
% Interpret Orientation
%
orientation=lower(Orientation);
if orientation(end)=='p'
    Orientation='portrait';
elseif orientation(end)=='l'
    Orientation='landscape';
else
    error('Unrecognized paper type/orientation');
end
PType=orientation(1:end-1);
%
for i=1:length(PlotText)
    if i<=length(Strings)
        PlotText{i}=Strings{i};
    end
    if ischar(PlotText{i}),
        PlotText{i}=PlotText(i);
    end
end
%
[ax,fg,allchld,allax,xmax,ymax,hBorder]=CreateBorderAxes(PType,Orientation);
set(hBorder,'userdata',BFormat)
%
Box=fliplr(Box');
switch Orientation
    case 'portrait'
        %
        % Draw border ...
        %
        L=line([Margin(1) xmax-Margin(3) xmax-Margin(3) Margin(1) Margin(1)], ...
            [Margin(2) Margin(2) ymax-Margin(4) ymax-Margin(4) Margin(2)], ...
            'parent',hBorder,'color',Color,'linewidth',LineWidth, ...
            'tag','border','visible','off');
        if Border, set(L,'visible','on'); end
        %
        % Plot boxes and texts ...
        %
        HRange=min(HRange,xmax-Margin(1)-Margin(3));
        VRange=min(VRange,ymax-Margin(2)-Margin(4));
        if isempty(HRange), HRange=0; end
        if isempty(VRange), VRange=0; end
        HTabs=cumsum([0 HTabs])*HRange+Margin(1);
        VTabs=cumsum([0 VTabs])*VRange+Margin(2);
        Boxs=unique(Box(:))';
        for b=Boxs
            [m,n]=find(Box==b);
            m1=min(m); m2=max(m); n1=min(n); n2=max(n);
            if b~=0
                line([HTabs(m1) HTabs(m2+1) HTabs(m2+1) HTabs(m1)   HTabs(m1)], ...
                    [VTabs(n1) VTabs(n1)   VTabs(n2+1) VTabs(n2+1) VTabs(n1)], ...
                    'parent',hBorder,'color',Color,'linewidth',LineWidth, ...
                    'buttondownfcn',BDFunction);
            end
            if b>0
                T=text((HTabs(m1)+HTabs(m2+1))/2, ...
                    (VTabs(n1)+VTabs(n2+1))/2, ...
                    PlotText{b}, ...
                    'horizontalalignment','center', ...
                    'verticalalignment','middle', ...
                    'fontname','helvetica', ...
                    'tag',sprintf('plottext%i',b), ...
                    'buttondownfcn',BDFunction, ...
                    'color',Color);
                if Bold(b), set(T,'fontweight','bold'); end
            elseif b<0
                pos=[HTabs(m1) VTabs(n1) HTabs(m2+1)-HTabs(m1) VTabs(n2+1)-VTabs(n1)];
                pos(1:2)=pos(1:2)+0.05*pos(3:4);
                pos(3:4)=0.9*pos(3:4);
                switch b
                    case -1
                        xx_logo('wl',hBorder,pos,LineWidth,Color,[])
                    case -2
                        xx_logo('ut',hBorder,pos,LineWidth,'none',Color)
                    case -3
                        xx_logo('deltares',hBorder,pos,LineWidth,'none',Color)
                end
            end
        end
        %
        % Shift axes ...
        %
        plotbox=[Margin(1)+0.1 Margin(2)+VRange+0.1 ...
            xmax-Margin(1)-Margin(3)-0.2 ymax-VRange-Margin(2)-Margin(4)-0.2];
        plotbox=plotbox./[xmax ymax xmax ymax];
        for i=1:length(allax)
            set(allax(i),'units','normalized');
            pos_i=get(allax(i),'position');
            n_pos_i(1)=plotbox(1)+pos_i(1)*plotbox(3);
            n_pos_i(2)=plotbox(2)+pos_i(2)*plotbox(4);
            n_pos_i(3)=pos_i(3)*plotbox(3);
            n_pos_i(4)=pos_i(4)*plotbox(4);
            set(allax(i),'position',n_pos_i);
        end
    case 'landscape'
        %
        % Draw border ...
        %
        L=line([Margin(2) xmax-Margin(4) xmax-Margin(4) Margin(2) Margin(2)], ...
            [Margin(3) Margin(3) ymax-Margin(1) ymax-Margin(1) Margin(3)], ...
            'parent',hBorder,'color',Color,'linewidth',LineWidth, ...
            'tag','border','visible','off');
        if Border, set(L,'visible','on'); end
        %
        % Plot boxes and texts ...
        %
        HRange=min(HRange,ymax-Margin(1)-Margin(3));
        VRange=min(VRange,xmax-Margin(2)-Margin(4));
        if isempty(HRange), HRange=0; end
        if isempty(VRange), VRange=0; end
        HTabs=cumsum([0 HTabs])*HRange+Margin(1);
        VTabs=cumsum([0 VTabs])*VRange+Margin(2);
        Boxs=unique(Box(:))';
        for b=Boxs
            [m,n]=find(Box==b);
            m1=min(m); m2=max(m); n1=min(n); n2=max(n);
            if b~=0
                line([VTabs(n1) VTabs(n1)   VTabs(n2+1) VTabs(n2+1) VTabs(n1)], ...
                    ymax-[HTabs(m1) HTabs(m2+1) HTabs(m2+1) HTabs(m1)   HTabs(m1)], ...
                    'parent',hBorder,'color',Color,'linewidth',LineWidth, ...
                    'buttondownfcn',BDFunction);
            end
            if b>0
                T=text((VTabs(n1)+VTabs(n2+1))/2, ...
                    ymax-(HTabs(m1)+HTabs(m2+1))/2, ...
                    PlotText{b}, ...
                    'horizontalalignment','center', ...
                    'verticalalignment','middle', ...
                    'fontname','helvetica', ...
                    'tag',sprintf('plottext%i',b), ...
                    'buttondownfcn',BDFunction, ...
                    'color',Color, ...
                    'rotation',270);
                if Bold(b), set(T,'fontweight','bold'); end
            elseif b<0
                pos=[VTabs(n1) ymax-HTabs(m2+1) VTabs(n2+1)-VTabs(n1) HTabs(m2+1)-HTabs(m1)];
                pos(1:2)=pos(1:2)+0.05*pos(3:4);
                pos(3:4)=0.9*pos(3:4);
                pos(5)=270*pi/180;
                switch b
                    case -1
                        xx_logo('wl',hBorder,pos,LineWidth,Color,[])
                    case -2
                        xx_logo('ut',hBorder,pos,LineWidth,'none',Color)
                    case -3
                        xx_logo('deltares',hBorder,pos,LineWidth,'none',Color)
                end
            end
        end
        %
        % Shift axes ...
        %
        plotbox=[Margin(2)+VRange+0.1 Margin(3)+0.1 ...
            xmax-VRange-Margin(2)-Margin(4)-0.2 ymax-Margin(1)-Margin(3)-0.2];
        plotbox=plotbox./[xmax ymax xmax ymax];
        for i=1:length(allax)
            set(allax(i),'units','normalized');
            pos_i=get(allax(i),'position');
            n_pos_i(1)=plotbox(1)+pos_i(1)*plotbox(3);
            n_pos_i(2)=plotbox(2)+pos_i(2)*plotbox(4);
            n_pos_i(3)=pos_i(3)*plotbox(3);
            n_pos_i(4)=pos_i(4)*plotbox(4);
            set(allax(i),'position',n_pos_i);
        end
    otherwise
        plotbox=[0 0 1 1];
end
setappdata(fg,'MaximumPlotExtent',plotbox)
AdjustFigPos(ax,fg,Orientation)
set(fg,'children',[allchld;hBorder]);


function [ax,fg,allchld,allax,xmax,ymax,hBorder]=CreateBorderAxes(PType,Orientation)
ax=gca;
fg=gcf;
allchld=allchild(fg);
allax=findobj(allchld,'type','axes');
set(fg,'paperunits','centimeter', ...
    'papertype',PType, ...
    'paperorientation',Orientation);
xmax=get(fg,'papersize');
ymax=xmax(2);
xmax=xmax(1);
set(fg,'paperposition',[0 0 xmax ymax]);
hBorder=axes('units','normalized', ...
    'position',[0 0 1 1], ...
    'tag','border', ...
    'xlimmode','manual', ...
    'ylimmode','manual', ...
    'xlim',[0 xmax], ...
    'ylim',[0 ymax], ...
    'visible','off');
setappdata(hBorder,'NonDataObject',[]);


function AdjustFigPos(ax,fg,Orientation)
axes(ax);
funits0=get(fg,'paperunits');
set(fg,'paperunits','centimeters');
PSize=get(fg,'papersize');
set(fg,'paperunits',funits0);
units0=get(0,'units');
%
set(0,'units','centimeters');
maxdim=get(0,'screensize');
maxdim=maxdim(3:4);
if strcmp(Orientation,'landscape')
    pos1=round(PSize*min(maxdim./PSize));
    pos2=round(PSize*min(fliplr(maxdim)./PSize));
    pos=min(pos1,pos2);
else % 'portrait'
    pos1=round(PSize*min(fliplr(maxdim)./PSize));
    pos2=round(PSize*min(maxdim./PSize));
    pos=min(pos1,pos2);
end
pos=pos*0.85;
pos=[(maxdim(1)-pos(1))/2 (maxdim(2)-pos(2))/2 pos];
set(fg, ...
    'units','centimeters', ...
    'position',pos);
set(fg,'units','pixels');
set(0,'units',units0);


function Str=locDateStr
t=[datestr(now,13) ' on ' datestr(now,8) ' '];
x=clock;
if x(3)>3
    t=[t num2str(x(3)) 'th'];
elseif x(3)==1
    t=[t num2str(x(3)) 'st'];
elseif x(3)==2
    t=[t num2str(x(3)) 'nd'];
elseif x(3)==3
    t=[t num2str(x(3)) 'rd'];
end
Str=[t ' ' datestr(now,3) ' ' datestr(now,10)];


function fig = Local_ui_paper(hBorder)
BFormat=get(hBorder,'userdata');
if isempty(BFormat) % backward compatibility and update
    BFormat.Border= 1;
    BFormat.Margin= [1 1 1 1];
    BFormat.LineWidth= 1.5;
    BFormat.Color= 'k';
    BFormat.HTabs= [0.68 0.16 0.16];
    BFormat.HRange= 19;
    BFormat.VTabs= [1 1 1]/3;
    BFormat.VRange= 2.7;
    BFormat.Box= [1 2 3; 1 4 4; 7 5 6];
    BFormat.Bold= [0 0 0 0 0 0 1];
    set(hBorder,'userdata',BFormat)
end
HRng=460-15-5*length(BFormat.HTabs);
HTabs=cumsum([0 BFormat.HTabs]);
N=size(BFormat.Box,1);

h0 = figure('Units','points', ...
    'Color',get(0,'defaultuicontrolbackgroundcolor'), ...
    'HandleVisibility','off', ...
    'IntegerHandle','off', ...
    'MenuBar','none', ...
    'Name','Border manager', ...
    'NumberTitle','off', ...
    'Position',[80 115 460 40+25*N], ...
    'Resize','off', ...
    'Visible','off', ...
    'Tag','Border manager for Matlab (c)');

Box=fliplr(BFormat.Box');
for b=1:max(Box(:))
    [m,n]=find(Box==b);
    m1=min(m); m2=max(m); n1=min(n); n2=max(n);
    uicontrol('Parent',h0, ...
        'Units','points', ...
        'BackgroundColor',[1 1 1], ...
        'ListboxTop',0, ...
        'FontUnits','pixels', ...
        'FontSize',12, ...
        'Max',1+n2-n1, ...
        'HorizontalAlignment','left', ...
        'Position',[10+5*(m1-1)+HRng*HTabs(m1) 10+25*(n1-1) HRng*(HTabs(m2+1)-HTabs(m1))+5*(m2-m1) 25*(n2-n1+1)-5], ...
        'Style','edit', ...
        'Tag',sprintf('Text%i',b));
end

uicontrol('Parent',h0, ...
    'Units','points', ...
    'ListboxTop',0, ...
    'FontUnits','pixels', ...
    'FontSize',12, ...
    'HorizontalAlignment','left', ...
    'Position',[10 10+25*N 85 20], ...
    'Style','checkbox', ...
    'BackgroundColor',get(h0,'color'), ...
    'String','left page', ...
    'Tag','LeftPage');

uicontrol('Parent',h0, ...
    'Units','points', ...
    'ListboxTop',0, ...
    'FontUnits','pixels', ...
    'FontSize',12, ...
    'Position',[285 10+25*N 80 20], ...
    'String','apply', ...
    'Callback','md_paper apply', ...
    'Tag','Apply');

uicontrol('Parent',h0, ...
    'Units','points', ...
    'ListboxTop',0, ...
    'FontUnits','pixels', ...
    'FontSize',12, ...
    'Position',[370 10+25*N 80 20], ...
    'String','done', ...
    'Callback','md_paper done', ...
    'Tag','Done');

set(h0,'Visible','on')
if nargout > 0, fig = h0; end
