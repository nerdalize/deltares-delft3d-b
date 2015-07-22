function cmap=md_colormap(varargin)
%MD_COLORMAP Colour map editor.
%    MD_COLORMAP(CMAPOBJ) starts an interactive colormap editor to edit the
%    specified colormap. By default the editor starts with the colormap
%    JET.
%
%    CMAPOBJ = MD_COLORMAP(...) returns a structure containing the edited
%    colormap. Use the CLRMAP command to convert the structure to a
%    standard MATLAB colormap array.
%
%    See also CLRMAP.

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

if nargin==0 || ~ischar(varargin{1})
    if nargout==1
        cmap=md_colormap_interface(varargin{:});
    else
        md_colormap_interface(varargin{:});
    end
    return
end

F=gcbf;
cmd=lower(varargin{1});
if isequal(cmd,'close')
    tg=get(F,'tag');
    switch tg
        case 'STANDALONE MODE'
            delete(F)
        case 'FUNCTION MODE'
            set(F,'visible','off')
    end
    return
end

updateuicontrols
UD=get(F,'userdata');
S=UD{1};
uih=UD{2};
currentcolor=UD{3};
a1=findobj(F,'tag','Colorbar');
m = get(a1,'userdata');

switch cmd
    case 'down'
        a1=gcbo;
        if ~isequal(get(a1,'type'),'axes')
            a1=get(a1,'parent');
        end
        cp=get(a1,'currentpoint'); cp=cp(1);
        N=size(S.Colors,1);
        if S.AlternatingColors
            uniform=1;
            index=(0.5+(0:N-1))/N*(1+1/m)-1/(2*m);
        elseif isempty(S.Index)
            uniform=1;
            index=(0:N-1)/(N-1);
        else
            uniform=0;
            index=S.Index;
        end
        button=get(F,'selectiontype');
        switch button
            case 'normal'
                [dummy,currentcolor]=min(abs(index-cp));
                set(F,'windowbuttonmotionfcn','md_colormap motion')
                set(F,'windowbuttonupfcn','md_colormap up')
            case 'alt'
                j=sum(index<cp);
                clr=clrmap(S,cp);
                switch S.Space
                    case 'RGB'
                    case 'CMY'
                        clr=1-clr;
                    case 'HSV'
                        clr=rgb2hsv(clr);
                    case 'HLS'
                        clr=rgb2hls(clr);
                end
                if ~uniform
                    S.Index(j+2:end+1)=S.Index(j+1:end);
                    S.Index(j+1)=cp;
                else
                    S.Index=[];
                end
                S.Colors(j+2:end+1,:)=S.Colors(j+1:end,:);
                S.Colors(j+1,:)=clr;
                currentcolor=j+1;
        end
    case 'motion'
        cp=get(findobj(F,'tag','Colorbar'),'currentpoint');
        S0=S;
        currentcolor0=currentcolor;
        [S,currentcolor]=moveit(S,currentcolor,cp,F);
        set(F,'userdata',{S0 uih currentcolor0 S currentcolor});
        updateinterface(F,S,currentcolor,uih)
        return
    case 'up'
        set(F,'windowbuttonmotionfcn','')
        set(F,'windowbuttonupfcn','')
        if length(UD)>3
            S=UD{4};
            currentcolor=UD{5};
        end
    case 'index'
        Idx=findobj(F,'tag','index');
        cp=str2double(get(Idx,'string'));
        if length(cp)==1
            cp(1,2)=0.5;
            [S,currentcolor]=moveit(S,currentcolor,cp,F);
        else
            set(Idx,'string',sprintf('%.4f',S.Index(currentcolor)))
        end
    case 'altcolors'
        S.AlternatingColors=get(gcbo,'value');
    case 'options'
        a1=findobj(F,'tag','Colorbar');
        m = get(a1,'userdata');
        M=md_dialog('Select Option',{'Length of Colour Map'},{'editint'},{[1 max(m,256)]},{m});
        if ~isempty(M)
            m = M{1};
            set(a1,'userdata',m)
            cbI=findobj(a1,'tag','ColorbarImage2');
            set(cbI,'cdata',1:m);
            axI=get(cbI,'parent');
            set(axI,'xlim',[0 1]+[-1 1]/m/2);
        end
    case 'uniform'
        uniform=get(gcbo,'value');
        if uniform
            S.Index=[];
        else
            N=size(S.Colors,1);
            S.Index=(0:N-1)/(N-1);
        end
    case 'grayscale'
        switch S.Space
            case {'RGB','CMY'}
                S.Colors=repmat(mean(S.Colors,2),1,3);
            case 'HSV'
                RGB=hsv2rgb(S.Colors);
                RGB=repmat(mean(RGB,2),1,3);
                S.Colors=rgb2hsv(RGB);
            case 'HLS'
                RGB=hls2rgb(S.Colors);
                RGB=repmat(mean(RGB,2),1,3);
                S.Colors=rgb2hls(RGB);
        end
    case 'reverse'
        S.Colors=flipud(S.Colors);
        S.Index=1-flipud(S.Index(:));
    case 'invert'
        switch S.Space
            case {'RGB','CMY'}
                S.Colors=1-S.Colors;
            case 'HSV'
                RGB=hsv2rgb(S.Colors);
                RGB=1-RGB;
                S.Colors=rgb2hsv(RGB);
            case 'HLS'
                RGB=hls2rgb(S.Colors);
                RGB=1-RGB;
                S.Colors=rgb2hls(RGB);
        end
    case {'red','green','blue','select','cyan','magenta','yellow'}
        switch cmd
            case 'select'
                switch S.Space
                    case 'RGB'
                        RGB=S.Colors(currentcolor,:);
                    case 'CMY'
                        RGB=1-S.Colors(currentcolor,:);
                    case 'HSV'
                        RGB=hsv2rgb(S.Colors(currentcolor,:));
                    case 'HLS'
                        RGB=hls2rgb(S.Colors(currentcolor,:));
                end
                RGB=uisetcolor(RGB);
                if isequal(RGB,0)
                    return
                end
            case {'red','green','blue'}
                RGB(3)=str2double(get(uih(3,1),'string'));
                RGB(2)=str2double(get(uih(2,1),'string'));
                RGB(1)=str2double(get(uih(1,1),'string'));
                if any(isnan(RGB))
                    RGB=0;
                else
                    RGB=[RGB{:}]/255;
                end
            case {'cyan','magenta','yellow'}
                CMY(3)=str2double(get(uih(3,4),'string'));
                CMY(2)=str2double(get(uih(2,4),'string'));
                CMY(1)=str2double(get(uih(1,4),'string'));
                if any(isnan(CMY))
                    RGB=0;
                else
                    CMY=[CMY{:}]/255;
                    RGB=1-CMY;
                end
        end
        if ~isequal(RGB,0)
            switch S.Space
                case 'RGB'
                    S.Colors(currentcolor,:)=RGB;
                case 'CMY'
                    S.Colors(currentcolor,:)=1-RGB;
                case 'HSV'
                    S.Colors(currentcolor,:)=rgb2hsv(RGB);
                case 'HLS'
                    S.Colors(currentcolor,:)=rgb2hls(RGB);
            end
        end
    case {'hsv-hue','hsv-saturation','value'}
        HSV(3)=str2double(get(uih(3,2),'string'));
        HSV(2)=str2double(get(uih(2,2),'string'));
        HSV(1)=str2double(get(uih(1,2),'string'));
        if any(isnan(HSV))
            HSV=0;
        else
            HSV=[HSV{:}]/255;
        end
        if ~isequal(HSV,0)
            switch S.Space
                case 'RGB'
                    S.Colors(currentcolor,:)=hsv2rgb(HSV);
                case 'HSV'
                    S.Colors(currentcolor,:)=HSV;
                case 'HLS'
                    RGB=hsv2rgb(HSV);
                    S.Colors(currentcolor,:)=rgb2hls(RGB);
                case 'CMY'
                    S.Colors(currentcolor,:)=1-hsv2rgb(HSV);
            end
        end
    case {'hls-hue','lightness','hls-saturation'}
        HLS(3)=str2double(get(uih(3,3),'string'));
        HLS(2)=str2double(get(uih(2,3),'string'));
        HLS(1)=str2double(get(uih(1,3),'string'));
        if any(isnan(HLS))
            HLS=0;
        else
            HLS=[HLS{:}]/255;
        end
        if ~isequal(HLS,0)
            switch S.Space
                case 'RGB'
                    S.Colors(currentcolor,:)=hls2rgb(HLS);
                case 'HSV'
                    RGB=hls2rgb(HLS);
                    S.Colors(currentcolor,:)=rgb2hsv(RGB);
                case 'HLS'
                    S.Colors(currentcolor,:)=HLS;
                case 'CMY'
                    S.Colors(currentcolor,:)=1-hls2rgb(HLS);
            end
        end
    case 'name'
        S.Name=get(gcbo,'string');
        setappdata(gcbo,'NameChanged',1)
    case 'colorspace'
        spaces=get(gcbo,'string');
        value=get(gcbo,'value');

        currentspace=S.Space;
        newspace=spaces{value};
        if isequal(currentspace,newspace)
            return;
        end
        clrs=S.Colors;
        switch [currentspace '->' newspace]
            case 'RGB->HSV'
                clrs=rgb2hsv(clrs);
            case 'RGB->HLS'
                clrs=rgb2hls(clrs);
            case 'RGB->CMY'
                clrs=1-clrs;
            case 'HSV->RGB'
                clrs=hsv2rgb(clrs);
            case 'HSV->HLS'
                clrs=hsv2rgb(clrs);
                clrs=rgb2hls(clrs);
            case 'HSV->CMY'
                clrs=1-hsv2rgb(clrs);
            case 'HLS->RGB'
                clrs=hls2rgb(clrs);
            case 'HLS->HSV'
                clrs=hls2rgb(clrs);
                clrs=rgb2hsv(clrs);
            case 'HLS->CMY'
                clrs=1-hls2rgb(clrs);
            case 'CMY->RGB'
                clrs=1-clrs;
            case 'CMY->HSV'
                clrs=rgb2hsv(1-clrs);
            case 'CMY->HLS'
                clrs=rgb2hls(1-clrs);
        end
        S.Colors=clrs;
        S.Space=newspace;
    case 'save'
        try
            qp_path=qp_basedir;
            clrmap_path=[qp_path filesep 'colormaps'];
        catch
            clrmap_path=pwd;
        end
        %
        [f,p]=uiputfile(fullfile(clrmap_path,'*.clrmap'));
        if ~ischar(f)
            return;
        end
        [pp,ff,ee]=fileparts(f);
        if isempty(ee)
            f=[f '.clrmap'];
        end
        filename=[p f];
        %
        Nm=findobj(F,'tag','name');
        if ~getappdata(Nm,'NameChanged')
            [pp,ff,ee]=fileparts(f);
            S.Name = ff;
        end
        %
        try
            clrmap('write',filename,S);
        catch
            ui_message('error',lasterr);
        end
        set(Nm,'string',S.Name)
        setappdata(Nm,'NameChanged',0)
        return
    case 'export'
        [f,p]=uiputfile('*.hls');
        if ~ischar(f)
            return
        end
        [pp,ff,ee]=fileparts(f);
        if isempty(ee)
            f=[f '.hls'];
        end
        filename=[p f];
        a1=findobj(F,'tag','Colorbar');
        m = get(a1,'userdata');
        map=clrmap(S,m);
        try
            qnhls('write',filename,map,S.Name);
        catch
            ui_message('error',lasterr);
        end
        return
    case 'apply'
        appfig=get(0,'currentfigure');
        if isempty(appfig)
            return
        end
        currentmap = get(appfig,'colormap');
        m = size(currentmap,1);
        map=clrmap(S,m);
        set(appfig,'colormap',map);
        return;
    case {'load','import'}
        switch cmd
            case 'load'
                try
                    qp_path=qp_basedir;
                    clrmap_path=[qp_path filesep 'colormaps'];
                catch
                    clrmap_path=pwd;
                end
                %
                [f,p]=uigetfile(fullfile(clrmap_path,'*.clrmap'));
                if ~ischar(f)
                    return
                end
                filename=[p f];
                try
                    SS=clrmap('read',filename);
                catch
                    ui_message('error',lasterr);
                    return
                end
            case 'import'
                [f,p]=uigetfile('*.hls');
                if ~ischar(f)
                    return
                end
                [pp,ff,ee]=fileparts(f);
                if isempty(ee)
                    f=[f '.hls'];
                end
                filename=[p f];
                try
                    [map,label]=qnhls('read',filename);
                    map=rgb2hls(map);
                    SS.Name=label;
                    SS.Space='HLS';
                    SS.Colors=map;
                    SS.AlternatingColors=0;
                    SS.Index=[];
                catch
                    ui_message('error',lasterr);
                    return
                end
        end
        S=SS;
        if ~isfield(S,'AlternatingColors')
            S.AlternatingColors=0;
        end
        AC=findobj(F,'tag','altcolors');
        set(AC,'value',S.AlternatingColors);
        if ~isfield(S,'Index')
            S.Index=[];
        end
        uniform=isempty(S.Index);
        Un=findobj(F,'tag','uniform');
        set(Un,'value',uniform)
        Nm=findobj(F,'tag','name');
        setappdata(Nm,'NameChanged',0)
        if isfield(S,'Name')
            set(Nm,'string',S.Name);
        else
            set(Nm,'string','<no name>');
        end
        CSp=findobj(F,'tag','colorspace');
        spaces=get(CSp,'string');
        S.Space=upper(S.Space);
        space=strmatch(S.Space,spaces,'exact');
        if isempty(space)
            error('Unknown colour space')
        end
        set(CSp,'value',space)
        currentcolor=1;

    otherwise
        fprintf('Unknown command: %s\n',cmd);
end

set(F,'userdata',{S uih currentcolor});
updateinterface(F,S,currentcolor,uih)


function S1=md_colormap_interface(S,uicontrolfont)
if nargin==0
    S.Name='jet';
    S.Space='RGB';
    S.Index=[0 1/8 3/8 5/8 7/8 1];
    S.Colors=[0 0 0.5; 0 0 1; 0 1 1; 1 1 0; 1 0 0; 0.5 0 0];
end
if ishandle(S)
    GCF=S;
    S.Name='current colormap';
    cmap=get(GCF,'colormap');
    N=size(cmap,1);
    % Alternating colors
    period=1;
    ALTikeep=[];
    while isempty(ALTikeep)
        ALTcmap=repmat(cmap(period,:),ceil(N/length(period)),1);
        ALTcmap=ALTcmap(1:N,:);
        FirstDiff=min(find(~all((cmap-ALTcmap)==0,2)));
        if isempty(FirstDiff)
            ALTikeep=period;
            break;
        end
        period=1:FirstDiff;
    end
    % RGB
    RGBcmap=cmap;
    RGBirem=1+find(all(abs(diff(diff(RGBcmap)))<1e-4,2));
    RGBikeep=setdiff(1:N,RGBirem);
    % HSV
    HSVcmap=rgb2hsv(cmap);
    HSVirem=1+find(all(abs(diff(diff(HSVcmap)))<1e-4,2));
    HSVikeep=setdiff(1:N,HSVirem);
    %
    if length(ALTikeep)<min(length(RGBikeep),length(HSVikeep))
        S.Space='RGB';
        S.AlternatingColors=1;
        ikeep=ALTikeep;
    elseif length(HSVikeep)>length(RGBikeep)
        S.Space='RGB';
        ikeep=RGBikeep;
    else
        S.Space='HSV';
        ikeep=HSVikeep;
        cmap=HSVcmap;
    end
    S.Index=(ikeep-1)/max(ikeep-1);
    S.Colors=cmap(ikeep,:);
elseif ~isstruct(S)
    error('Invalid input argument.')
end
if nargin<2
    uicontrolfont.DefaultUicontrolFontWeight = get(0,'DefaultUicontrolFontWeight');
end
sz=get(0,'screensize');

Inactive=get(0,'defaultuicontrolbackgroundcolor');
Active=[1 1 1];
dims=[390 225];
pos(1:2)=(sz(3:4)-dims)/2;
pos(3:4)=dims;

if ~isfield(S,'Name')
    Name='<no name>';
else
    Name=S.Name;
end
uniform=1;
if isfield(S,'Index') && ~isempty(S.Index)
    uniform=0;
else
    S.Index=[];
end

S.Space=upper(S.Space);
Spaces={'RGB','HSV','HLS','CMY'};
spaceval=strmatch(S.Space,Spaces,'exact');
if isempty(spaceval)
    error('Unknown colour space.')
end

if isfield(S,'AlternatingColors') && isequal(S.AlternatingColors,1)
    AlternatingColors=1;
else
    AlternatingColors=0;
end
S.AlternatingColors=AlternatingColors;

h0 = figure('Visible','on', ...
    'Units','pixels', ...
    'Color',Inactive, ...
    'IntegerHandle','off', ...
    'MenuBar','none', ...
    'Name','Colour Map Editor', ...
    'Doublebuffer','on', ...
    'CloseRequestFcn','', ...
    'NumberTitle','off', ...
    'Resize','off', ...
    'Position',pos, ...
    'Handlevisibility','callback', ...
    'Tag','STANDALONE MODE', ...
    uicontrolfont);
setappdata(h0,'WL_UserInterface',1)

m0=uimenu('parent',h0, ...
    'label','&File');
uimenu('parent',m0, ...
    'label','&Open...', ...
    'accelerator','O', ...
    'callback','md_colormap load');
uimenu('parent',m0, ...
    'label','&Save...', ...
    'accelerator','S', ...
    'callback','md_colormap save');
uimenu('parent',m0, ...
    'label','&Import...', ...
    'accelerator','I', ...
    'separator','on', ...
    'callback','md_colormap import');
uimenu('parent',m0, ...
    'label','&Export...', ...
    'accelerator','E', ...
    'callback','md_colormap export');
uimenu('parent',m0, ...
    'label','&Apply...', ...
    'accelerator','A', ...
    'callback','md_colormap apply');
uimenu('parent',m0, ...
    'label','&Close', ...
    'separator','on', ...
    'callback','md_colormap close');

m0=uimenu('parent',h0, ...
    'label','&Options');
uimenu('parent',m0, ...
    'label','&Settings', ...
    'callback','md_colormap options');
uimenu('parent',m0, ...
    'label','Convert to &Gray Scale', ...
    'callback','md_colormap grayscale');
uimenu('parent',m0, ...
    'label','&Reverse Colour Map', ...
    'callback','md_colormap reverse');
uimenu('parent',m0, ...
    'label','&Invert Colour Map', ...
    'callback','md_colormap invert');

%======

voffset=11;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap select', ...
    'Position',[11 voffset 100 18], ...
    'String','Select Colour...', ...
    'Enable','on', ...
    'Tag','selcolor');
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap close', ...
    'Position',[261 voffset 100 18], ...
    'String','Close', ...
    'Enable','on', ...
    'Tag','close');

%======

voffset=voffset+25;
uih=zeros(3,4);
uih=cv_edit(h0,Inactive, 11,voffset,uih,3,1,'blue'          ,'Blue');
uih=cv_edit(h0,Inactive,141,voffset,uih,3,2,'value'         ,'Value');
uih=cv_edit(h0,Inactive,281,voffset,uih,3,3,'HLS-saturation','Saturation');
uih=cv_edit(h0,Inactive, 11,voffset,uih,3,4,'yellow'        ,'Yellow');

%======

voffset=voffset+25;
uih=cv_edit(h0,Inactive, 11,voffset,uih,2,1,'green'         ,'Green');
uih=cv_edit(h0,Inactive,141,voffset,uih,2,2,'HSV-saturation','Saturation');
uih=cv_edit(h0,Inactive,281,voffset,uih,2,3,'lightness'     ,'Lightness');
uih=cv_edit(h0,Inactive, 11,voffset,uih,2,4,'magenta'       ,'Magenta');

%======

voffset=voffset+25;
uih=cv_edit(h0,Inactive, 11,voffset,uih,1,1,'red'           ,'Red');
uih=cv_edit(h0,Inactive,141,voffset,uih,1,2,'HSV-hue'       ,'Hue');
uih=cv_edit(h0,Inactive,281,voffset,uih,1,3,'HLS-hue'       ,'Hue');
uih=cv_edit(h0,Inactive, 11,voffset,uih,1,4,'cyan'          ,'Cyan');

%======

m = size(get(h0,'colormap'),1);
voffset=voffset+35;
a1 = axes('Parent',h0, ...
    'Color',Active, ...
    'Units','Pixels', ...
    'Buttondownfcn','md_colormap down', ...
    'Position',[11 voffset 370 20], ...
    'tag','Colorbar', ...
    'xtick',[], ...
    'ytick',[], ...
    'xlim',[0 1]+[-1 1]/m/2, ...
    'ylim',[0 1], ...
    'box','on', ...
    'userdata',m, ...
    'layer','top');
image('Parent',a1, ...
    'Buttondownfcn','md_colormap down', ...
    'cdata',1:m, ...
    'xdata',[0 1], ...
    'ydata',[1/3 2/3], ...
    'tag','ColorbarImage');
image('Parent',a1, ...
    'Buttondownfcn','md_colormap down', ...
    'cdata',1:m, ...
    'xdata',[0 1], ...
    'ydata',[1 4/3], ...
    'tag','ColorbarImage2');

%======

voffset=voffset+25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 40 18], ...
    'String','Index', ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on', ...
    'tag','indextext');
uicontrol('Parent',h0, ...
    'BackgroundColor',Active, ...
    'Callback','md_colormap index', ...
    'Position',[51 voffset 60 20], ...
    'String','', ...
    'Horizontalalignment','right', ...
    'Style','edit', ...
    'Enable','on', ...
    'tag','index');
%---
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[131 voffset 120 18], ...
    'String','Colour Order Fixed', ...
    'Horizontalalignment','left', ...
    'Style','checkbox', ...
    'Value',1, ...
    'Enable','on', ...
    'tag','dragorder');
%---
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap altcolors', ...
    'Position',[261 voffset 120 18], ...
    'String','Alternating Colours', ...
    'Horizontalalignment','left', ...
    'Style','checkbox', ...
    'Value',AlternatingColors, ...
    'Enable','on', ...
    'tag','altcolors');

%======

voffset=voffset+25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 40 18], ...
    'String','Space', ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on');
uicontrol('Parent',h0, ...
    'BackgroundColor',Active, ...
    'Callback','md_colormap colorspace', ...
    'Position',[51 voffset 60 20], ...
    'String',Spaces, ...
    'value',spaceval, ...
    'Horizontalalignment','right', ...
    'Style','popupmenu', ...
    'Enable','on', ...
    'tag','colorspace');
%---
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap uniform', ...
    'Position',[131 voffset 250 18], ...
    'String','Reference Colours Uniformly Distributed', ...
    'Horizontalalignment','left', ...
    'Style','checkbox', ...
    'Value',uniform, ...
    'Enable','on', ...
    'tag','uniform');


%======

voffset=voffset+25;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 40 18], ...
    'String','Name', ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on');
Nm = uicontrol('Parent',h0, ...
    'BackgroundColor',Active, ...
    'Callback','md_colormap name', ...
    'Position',[51 voffset 330 20], ...
    'String',Name, ...
    'Horizontalalignment','left', ...
    'Style','edit', ...
    'Enable','on', ...
    'tag','name');
setappdata(Nm,'NameChanged',0)
%=====

if matlabversionnumber > 5.01
    h1vec=allchild(h0)';
    c10=char(10);
    for h1=h1vec,
        t=get(h1,'tag');
        if ~isempty(t)
            switch t
                case {'red','green','blue','cyan','magenta','yellow'}
                    Str=['Specify ' t ' colour component.'];
                case 'HSV-hue'
                    Str='Specify HSV hue: 0=red, 85=green, 170=blue.';
                case {'HSV-saturation','HLS-saturation'}
                    Str='Specify saturation: 0=white, 255=full colour.';
                case 'value'
                    Str='Specify gray component value: 0=black, 255=full colour.';
                case 'HLS-hue'
                    Str='Specify HLS hue: 85=red, 170=green, 0=blue.';
                case 'lightness'
                    Str='Specify lightness: 0=black, 128=full colour, 255=white.';
                case 'index'
                    Str=['Specify relative colour map index: location of current colour in colour map.',c10,'Alternative click and drag colour along colour bar.'];
                case 'name'
                    Str='Specify name of colour map.';
                case 'colorspace'
                    Str='Select colour space.';
                case 'selcolor'
                    Str='Select from standard colour interface.';
                case 'close'
                    Str='Close this interface without further confirmation.';
                case 'dragorder'
                    Str='Maintain colour order while dragging colours along colour bar.';
                case 'uniform'
                    Str='Force colours to be separated by equal intervals.';
                otherwise
                    Str='';
            end
            if ~isempty(Str)
                set(h1,'tooltip',Str);
            end
        end
    end
end

%=====

set(h0,'userdata',{S uih 1})
set(uih(1),'userdata',{S -1})
updateinterface(h0,S,1,uih)

if nargout==1
    set(h0,'tag','FUNCTION MODE')
    waitfor(h0,'visible');
    UD=get(h0,'userdata');
    S1=UD{1};
    delete(h0);
end


function updateinterface(F,S,currentcolor,uih)
a1=findobj(F,'tag','Colorbar');
N=size(S.Colors,1);


m = get(a1,'userdata');
set(F,'colormap',clrmap(S,m));
xdata=[0 1];
if S.AlternatingColors
    corf=(1+1/m);
    cors=-1/(2*m);
    xdata=[0 1]+[-1 1]/(2*m)-[-1 1]/(2*N);
    m=N;
end
cbI=findobj(a1,'tag','ColorbarImage');
set(cbI,'xdata',xdata,'cdata',1:m);

index=(0:N-1)/(N-1);
uniform=1;
if S.AlternatingColors
    index=(0.5+(0:N-1))/N*corf+cors;
elseif ~isempty(S.Index)
    uniform=0;
    index=max(0,min(1,S.Index));
    if index(1)~=0
        index(1)=0;
    end
    if index(end)~=1
        index(end)=1;
    end
end

t=findall(a1,'type','patch');
if length(t)~=N
    delete(t);
    for i=1:N
        t=patch(index(i)+[0 -1 1]/100,[-0.3 -1.3 -1.3]/4,1, ...
            'Buttondownfcn','md_colormap down', ...
            'parent',a1, ...
            'edgecolor','k', ...
            'facecolor','w', ...
            'clipping','off');
        if i==currentcolor
            set(t,'facecolor','k')
        end
    end
else
    for i=1:N
        set(t(i),'xdata',index(i)+[0 -1 1]/100, ...
            'ydata',[-0.3 -1.3 -1.3]/4, ...
            'facecolor','w');
        if i==currentcolor
            set(t(i),'facecolor','k')
        end
    end
end

Inactive=get(0,'defaultuicontrolbackgroundcolor');
Active=[1 1 1];

cpstr=sprintf('%.4f',index(currentcolor));
if uniform
    set(findobj(F,'tag','index'),'enable','inactive','backgroundcolor',Inactive,'string',cpstr)
elseif currentcolor==1 || currentcolor==N
    set(findobj(F,'tag','index'),'enable','inactive','backgroundcolor',Inactive,'string',cpstr)
else
    set(findobj(F,'tag','index'),'enable','on','backgroundcolor',Active,'string',cpstr)
end

Prev=get(uih(1),'userdata');
if currentcolor~=Prev{2} || ~isequal(S.Colors,Prev{1}.Colors)
    set(uih(1),'userdata',{S currentcolor})
    switch S.Space
        case 'RGB'
            set(uih(:,1),'backgroundcolor',Active)
            set(uih(:,[2 3]),'backgroundcolor',Inactive)
            set(uih(:,1:3,:),'visible','on')
            set(uih(:,4,:),'visible','off')
            RGB=S.Colors(currentcolor,:);
            HSV=rgb2hsv(RGB);
            HLS=rgb2hls(RGB);
            CMY=1-RGB;
        case 'HSV'
            set(uih(:,2),'backgroundcolor',Active)
            set(uih(:,[1 3]),'backgroundcolor',Inactive)
            set(uih(:,1:3,:),'visible','on')
            set(uih(:,4,:),'visible','off')
            HSV=S.Colors(currentcolor,:);
            RGB=hsv2rgb(HSV);
            HLS=rgb2hls(RGB);
            CMY=1-RGB;
        case 'HLS'
            set(uih(:,3),'backgroundcolor',Active)
            set(uih(:,[1 2]),'backgroundcolor',Inactive)
            set(uih(:,1:3,:),'visible','on')
            set(uih(:,4,:),'visible','off')
            HLS=S.Colors(currentcolor,:);
            RGB=hls2rgb(HLS);
            HSV=rgb2hsv(RGB);
            CMY=1-RGB;
        case 'CMY'
            set(uih(:,4),'backgroundcolor',Active)
            set(uih(:,[2 3]),'backgroundcolor',Inactive)
            set(uih(:,2:4,:),'visible','on')
            set(uih(:,1,:),'visible','off')
            CMY=S.Colors(currentcolor,:);
            RGB=1-CMY;
            HSV=rgb2hsv(RGB);
            HLS=rgb2hls(RGB);
    end
    RGB=round(RGB*255);
    HSV=round(HSV*255);
    HLS=round(HLS*255);
    CMY=round(CMY*255);

    set(uih(1,1),'string',num2str(RGB(1)))
    set(uih(2,1),'string',num2str(RGB(2)))
    set(uih(3,1),'string',num2str(RGB(3)))

    set(uih(1,2),'string',num2str(HSV(1)))
    set(uih(2,2),'string',num2str(HSV(2)))
    set(uih(3,2),'string',num2str(HSV(3)))

    set(uih(1,3),'string',num2str(HLS(1)))
    set(uih(2,3),'string',num2str(HLS(2)))
    set(uih(3,3),'string',num2str(HLS(3)))

    set(uih(1,4),'string',num2str(CMY(1)))
    set(uih(2,4),'string',num2str(CMY(2)))
    set(uih(3,4),'string',num2str(CMY(3)))
end

function [S,currentcolor]=moveit(S,currentcolor,cp,F)
deletepoint=cp(1,2)<-1 || cp(1,2)>2;
a1=findobj(F,'tag','Colorbar');
N=size(S.Colors,1);
m = get(a1,'userdata');
uniform=1;
index=(0:N-1)/(N-1);
if S.AlternatingColors
    index=(0.5+(0:N-1))/N*(1+1/m)-1/(2*m);
elseif ~isempty(S.Index);
    index=S.Index;
    uniform=0;
end
if deletepoint && N>2
    S.Colors(currentcolor,:)=[];
    if ~isempty(S.Index)
        S.Index(currentcolor)=[];
        if currentcolor==1 || currentcolor==N
            S.Index=(S.Index-S.Index(1))/(S.Index(end)-S.Index(1));
        end
    end
    if currentcolor==N
        currentcolor=N-1;
    end
else
    cp=cp(1);
    if ~get(findobj(F,'tag','dragorder'),'value')
        while currentcolor>1 && index(currentcolor-1)>cp
            index(currentcolor+[-1 0])=index(currentcolor+[0 -1]);
            S.Colors(currentcolor+[-1 0],:)=S.Colors(currentcolor+[0 -1],:);
            currentcolor=currentcolor-1;
        end
        if currentcolor>1 && index(currentcolor-1)==cp
            cp=cp+eps;
        end
        while currentcolor<N && index(currentcolor+1)<cp
            index(currentcolor+[1 0])=index(currentcolor+[0 1]);
            S.Colors(currentcolor+[1 0],:)=S.Colors(currentcolor+[0 1],:);
            currentcolor=currentcolor+1;
        end
        if currentcolor<N && index(currentcolor+1)==cp
            cp=cp-eps;
        end
    elseif ~uniform
        if currentcolor>1
            cp=max(cp,S.Index(currentcolor-1)+eps);
        end
        if currentcolor<N
            cp=min(cp,S.Index(currentcolor+1)-eps);
        end
    end
    if ~uniform
        index(currentcolor)=cp;
        S.Index=index;
        if S.Index(1)~=0 || S.Index(end)~=1
            S.Index=(S.Index-S.Index(1))/(S.Index(end)-S.Index(1));
        end
    end
end


function uih=cv_edit(h0,Inactive,hoffset,voffset,uih,i,j,tag,str)
uih(i,j,2) = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[hoffset voffset 60 18], ...
    'String',str, ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on');
uih(i,j,1) = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback',['md_colormap ' tag], ...
    'Position',[hoffset+60 voffset 40 20], ...
    'String','0', ...
    'Horizontalalignment','right', ...
    'Style','edit', ...
    'Enable','on', ...
    'tag',tag);
