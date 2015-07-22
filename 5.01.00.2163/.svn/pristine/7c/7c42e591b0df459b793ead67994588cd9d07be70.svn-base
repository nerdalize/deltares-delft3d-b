function outdata = qp_plotmanager(cmd,UD,logfile,logtype,cmdargs)
%QP_PLOTMANAGER QuickPlot Plot Manager callback functions.

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

Inactive = UD.Inactive;
Active = UD.Active;
mfig = findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');

T_=1; ST_=2; M_=3; N_=4; K_=5;
DimStr={'subfield ','timestep ','station ','M=','N=','K='};

switch cmd
    case 'resize'
        PM = UD.PlotMngr;
        %
        % Get new and old figure size (note: for this to work the figure size
        % must be initialized before the first call).
        %
        fig = PM.Fig;
        PrevSize = getappdata(fig,'FigureSize');
        MinSize = getappdata(fig,'MinimumFigureSize');
        if isempty(MinSize)
            MinSize = PrevSize;
            setappdata(fig,'MinimumFigureSize',MinSize)
        end
        NewPos = get(fig,'position');
        NewSize=NewPos(3:4);
        if any(NewSize<MinSize)
            NewSize=max(NewSize,MinSize);
            NewPos(2)=NewPos(2)+NewPos(4)-NewSize(2);
            NewPos(3:4)=NewSize;
            set(fig,'position',NewPos)
        end
        %
        % Define some shift operators
        %
        aligntop   = [0 NewSize(2)-PrevSize(2) 0 0];
        alignright = [NewSize(1)-PrevSize(1) 0 0 0];
        stretchhor = [0 0 NewSize(1)-PrevSize(1) 0];
        stretchver = [0 0 0 NewSize(2)-PrevSize(2)];
        stretch5   = [0 0 (NewSize(1)-PrevSize(1))/5 0];
        shift5     = [(NewSize(1)-PrevSize(1))/5 0 0 0];
        %
        % Shift the buttons
        %
        shiftcontrol(PM.FigTxt,aligntop)
        shiftcontrol(PM.FigList,aligntop+stretchhor)
        shiftcontrol(PM.FigAll,aligntop+alignright)
        shiftcontrol(PM.AxTxt,aligntop)
        shiftcontrol(PM.AxList,aligntop+stretchhor)
        shiftcontrol(PM.AxAll,aligntop+alignright)
        shiftcontrol(PM.Show,aligntop)
        shiftcontrol(PM.ShowList,aligntop+stretchhor)
        shiftcontrol(PM.ItTxt,aligntop)
        shiftcontrol(PM.ItList,stretchhor+stretchver)
        shiftcontrol(PM.ItUp,aligntop+alignright)
        shiftcontrol(PM.ItDown,alignright)
        %
        shiftcontrol(PM.FigColorTxt,aligntop)
        shiftcontrol(PM.FigColor,aligntop)
        %
        shiftcontrol(PM.AxColorTxt,aligntop)
        shiftcontrol(PM.HasAxColor,aligntop)
        shiftcontrol(PM.AxColor,aligntop+stretch5)
        shiftcontrol(PM.AxBox,aligntop+shift5+stretch5)
        %
        shiftcontrol(PM.AxPosition,aligntop)
        shiftcontrol(PM.AxXLowerLeft,aligntop+stretch5)
        shiftcontrol(PM.AxYLowerLeft,aligntop+shift5+stretch5)
        shiftcontrol(PM.AxWidth,aligntop+2*shift5+stretch5)
        shiftcontrol(PM.AxHeight,aligntop+3*shift5+stretch5)
        shiftcontrol(PM.AxPosUnit,aligntop+4*shift5+stretch5)
        %
        shiftcontrol(PM.XLimitTxt,aligntop)
        shiftcontrol(PM.XLimitMin,aligntop+stretch5)
        shiftcontrol(PM.XLimitMax,aligntop+shift5+stretch5)
        shiftcontrol(PM.XScale,aligntop+2*shift5+stretch5)
        shiftcontrol(PM.XGrid,aligntop+3*shift5+stretch5)
        shiftcontrol(PM.XLoc,aligntop+4*shift5+stretch5)
        shiftcontrol(PM.XColor,aligntop+5*shift5)
        shiftcontrol(PM.XLabelTxt,aligntop)
        shiftcontrol(PM.XLabel,aligntop+5*stretch5)
        %
        shiftcontrol(PM.YLimitTxt,aligntop)
        shiftcontrol(PM.YLimitMin,aligntop+stretch5)
        shiftcontrol(PM.YLimitMax,aligntop+shift5+stretch5)
        shiftcontrol(PM.YScale,aligntop+2*shift5+stretch5)
        shiftcontrol(PM.YGrid,aligntop+3*shift5+stretch5)
        shiftcontrol(PM.YLoc,aligntop+4*shift5+stretch5)
        shiftcontrol(PM.YColor,aligntop+5*shift5)
        shiftcontrol(PM.YLabelTxt,aligntop)
        shiftcontrol(PM.YLabel,aligntop+5*stretch5)
        %
        shiftcontrol(PM.ItTxt2,aligntop)
        shiftcontrol(PM.ItList2,aligntop+stretchhor)
        %
        % Store the new figure size for usage during next resize command
        %
        setappdata(fig,'FigureSize',NewSize);
        
    case 'newfigure'
        [h,figops,createops]=qp_createfig(cmdargs{:});
        if ~isempty(h)
            UDplot=get(h,'userdata');
            UDplot.ProgID='QuickPlot';
            set(h,'userdata',UDplot);
            if ~isempty(h)
                set(UD.PlotMngr.FigList,'value',1,'string',listnames(h,'showType','no','showHandle','no','showTag','no'),'userdata',h);
                d3d_qp refreshfigs
            end
            if logfile
                writelog(logfile,logtype,cmd,createops{:});
            end
        end
        
    case 'newaxes'
        FigIDs=get(UD.PlotMngr.FigList,'userdata');
        if isempty(FigIDs)
            set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.DelAx,'enable','off');
            UD.PlotMngr.CurrentAxes=[];
            setappdata(mfig,'QPHandles',UD)
            d3d_qp refreshitems
            d3d_qp refreshaxprop
        else
            FigVal=get(UD.PlotMngr.FigList,'value');
            Fig=FigIDs(FigVal);
            if ~ishandle(Fig)
                d3d_qp refreshfigs
            else
                h=qp_createaxes(Fig);
                if ~isempty(h)
                    set(UD.PlotMngr.AxList,'value',1,'string',listnames(h),'userdata',h);
                    d3d_qp refreshaxes
                    d3d_qp refreshfigprop
                end
            end
        end
        
    case 'openfigure'
        figuredir=qp_settings('figuredir');
        if ~isempty(cmdargs)
            [p,f,extension] = fileparts(cmdargs{1});
            f = [f,extension];
        else
            pf = fullfile(figuredir,'*.fig');
            [f,p]=uigetfile(pf,'Open figure ...');
        end
        if ischar(f)
            figuredir=p;
            qp_settings('figuredir',figuredir)
            %
            pf = fullfile(p,f);
            h=hgload(pf);
            set(h,'menubar','none','closerequestfcn','d3d_qp closefigure')
            qp_figurebars(h)
            %set(cbar,'deletefcn','qp_colorbar delete')
            hName = listnames(h,'showtype','no','showhandle','no','showtag','no');
            set(UD.PlotMngr.FigList,'value',1,'string',hName,'userdata',h);
            d3d_qp refreshfigs
            if logfile
                writelog(logfile,logtype,cmd,pf);
            end
        end
        
    case 'refreshfigs'
        if ~isempty(cmdargs)
            Fg=cmdargs{1};
            FgName = listnames(Fg,'showtype','no','showhandle','no','showtag','no');
            set(UD.PlotMngr.FigList,'value',1,'string',FgName,'userdata',Fg);
        end
        
        Figs=get_nondialogs;
        if isempty(Figs)
            set(UD.PlotMngr.FigList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.SavFig,'enable','off');
            set(UD.PlotMngr.ClsFig,'enable','off');
            set(UD.PlotMngr.FigAll,'enable','off');
            set(UD.PlotMngr.NewAx,'enable','off');
        else
            fignames=listnames(Figs,'showtype','no','showhandle','no','showtag','no');
            [fignames,Order]=sort(fignames);
            Figs=Figs(Order);
            FigNms=get(UD.PlotMngr.FigList,'string');
            FigHnd=get(UD.PlotMngr.FigList,'userdata');
            FigVal=get(UD.PlotMngr.FigList,'value');
            if FigVal<=length(FigHnd) && ismember(FigHnd(FigVal),Figs)
                i=find(Figs==FigHnd(FigVal));
            elseif FigVal<=length(FigNms)
                if iscell(FigNms)
                    FigNm=FigNms{FigVal};
                else
                    FigNm=deblank(FigNms(FigVal,:));
                end
                i=ustrcmpi(FigNm,fignames);
                if i<0
                    i=1;
                end
            else
                i=1;
            end
            set(UD.PlotMngr.FigAll,'enable','on');
            enable = 'on';
            backgroundcolor = Active;
            if get(UD.PlotMngr.FigAll,'value')
                enable = 'off';
                backgroundcolor = Inactive;
            end
            set(UD.PlotMngr.FigList,'string',fignames,'userdata',Figs,'value',i, ...
                'enable',enable,'backgroundcolor',backgroundcolor);
            set(UD.PlotMngr.SavFig,'enable','on');
            set(UD.PlotMngr.ClsFig,'enable',enable);
            set(UD.PlotMngr.NewAx,'enable','on');
        end
        d3d_qp refreshaxes
        d3d_qp refreshfigprop
        d3d_qp update_addtoplot
        
    case 'allfigures'
        if ~isempty(cmdargs)
            allFigs = cmdargs{1};
            set(UD.PlotMngr.FigAll,'value',allFigs)
        else
            allFigs = get(UD.PlotMngr.FigAll,'value');
        end
        if allFigs
            figlistenable='off';
            figlistcolour=Inactive;
        else
            figlistenable='on';
            figlistcolour=Active;
        end
        set(UD.PlotMngr.FigList, ...
            'enable',figlistenable,'backgroundcolor',figlistcolour);
        set(UD.PlotMngr.ClsFig,'enable',figlistenable);
        d3d_qp refreshaxes
        d3d_qp refreshfigprop
        d3d_qp update_addtoplot
        
    case 'refreshaxes'
        FigIDs=get(UD.PlotMngr.FigList,'userdata');
        if isempty(FigIDs)
            set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.DelAx,'enable','off');
            set(UD.PlotMngr.AxAll,'enable','off');
            UD.PlotMngr.CurrentAxes=[];
            setappdata(mfig,'QPHandles',UD)
        else
            FigVal=get(UD.PlotMngr.FigList,'value');
            allfigs=get(UD.PlotMngr.FigAll,'value');
            if allfigs
                Fig=FigIDs;
            else
                Fig=FigIDs(FigVal);
            end
            if any(~ishandle(Fig))
                d3d_qp refreshfigs
            else
                Axs=findall(Fig,'type','axes');
                for i=length(Axs):-1:1
                    if strcmp(get(Axs(i),'tag'),'scribeOverlay')
                        Axs(i)=[];
                    elseif isappdata(Axs(i),'NonDataObject')
                        Axs(i)=[];
                    end
                end
                if isempty(Axs)
                    set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                        'enable','off','backgroundcolor',Inactive);
                    set(UD.PlotMngr.DelAx,'enable','off');
                    UD.PlotMngr.CurrentAxes=[];
                    setappdata(mfig,'QPHandles',UD)
                else
                    axnames=listnames(Axs);
                    AxVal=get(UD.PlotMngr.AxList,'value');
                    
                    AxH=get(UD.PlotMngr.AxList,'userdata');
                    if AxVal>length(AxH)
                        i=1;
                    else
                        i=find(AxH(AxVal)==Axs);
                        if isempty(i)
                            i=1;
                        end
                    end
                    axallenabled='on';
                    if allfigs
                        axallenabled='off';
                    end
                    set(UD.PlotMngr.AxAll,'enable',axallenabled);
                    axlistenable='on';
                    axlistcolour=Active;
                    if get(UD.PlotMngr.AxAll,'value') || allfigs
                        axlistenable='off';
                        axlistcolour=Inactive;
                    end
                    set(UD.PlotMngr.AxList,'string',axnames,'userdata',Axs,'value',i, ...
                        'enable',axlistenable,'backgroundcolor',axlistcolour);
                    set(UD.PlotMngr.DelAx,'enable',axlistenable);
                    UD.PlotMngr.CurrentAxes=Axs(i);
                    setappdata(mfig,'QPHandles',UD)
                end
            end
        end
        d3d_qp refreshitems
        d3d_qp refreshaxprop
        
    case 'allaxes'
        if ~isempty(cmdargs)
            allAxes = cmdargs{1};
            set(UD.PlotMngr.AxAll,'value',allAxes)
        else
            allAxes = get(UD.PlotMngr.AxAll,'value');
        end
        if allAxes
            axlistenable='off';
            axlistcolour=Inactive;
        else
            axlistenable='on';
            axlistcolour=Active;
        end
        set(UD.PlotMngr.AxList, ...
            'enable',axlistenable,'backgroundcolor',axlistcolour);
        set(UD.PlotMngr.DelAx,'enable',axlistenable);
        d3d_qp refreshitems
        d3d_qp refreshaxprop
        d3d_qp update_addtoplot
        
    case 'pmshowselect'
        ipane = get(UD.PlotMngr.ShowList,'value');
        h = UD.PlotMngr.Handles;
        idx = UD.PlotMngr.Pane == ipane;
        set(h(idx),'visible','on')
        set(h(~idx),'visible','off')
        
    case 'refreshitems'
        AxIDs=get(UD.PlotMngr.AxList,'userdata');
        if isempty(AxIDs)
            set(UD.PlotMngr.ItList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.DelIt,'enable','off');
            set(UD.PlotMngr.ItInfo,'enable','off');
            set(UD.PlotMngr.ItLink,'enable','off');
            %
            set(UD.PlotMngr.ItList2,'string',' ','value',1, ...
                'enable','off','backgroundcolor',Inactive);
        else
            Ax = getAx(UD);
            if any(~ishandle(Ax))
                d3d_qp refreshaxes
                d3d_qp refreshfigprop
            else
                Items=allchild(Ax);
                if iscell(Items)
                    Items(:,2)={0};
                    Items(end,2)={[]};
                    Items=Items';
                    Items=cat(1,Items{:});
                end
                Tags=get(Items,'tag');
                for t=find(Items==0)'
                    Tags(t)={sprintf('QPPlotTag---%i',t)};
                end
                UserDatas=get(Items,'userdata');
                UserDatas(Items==0)={'---'};
                %---
                TUDvalid=~cellfun('isempty',Tags) & ~cellfun('isempty',UserDatas);
                Items=Items(TUDvalid);
                Tags=Tags(TUDvalid);
                UserDatas=UserDatas(TUDvalid);
                %---
                QPTag=strncmp('QPPlotTag',Tags,9);
                Items=Items(QPTag);
                Tags=Tags(QPTag);
                UserDatas=UserDatas(QPTag);
                %---
                [Tags,I]=unique(Tags);
                [I,Isort]=sort(I);
                Tags=Tags(Isort);
                Items=Items(I);
                UserDatas=UserDatas(I);
                %---
                while ~isempty(Items) && Items(end)==0
                    Items(end)=[];
                    UserDatas(end)=[];
                    Tags(end)=[];
                end
                %---
                while ~isempty(Items) && Items(1)==0
                    Items(1)=[];
                    UserDatas(1)=[];
                    Tags(1)=[];
                end
                %---
                separator='------';
                if isempty(Items)
                    set(UD.PlotMngr.ItList,'string',{''},'userdata',[],'value',1, ...
                        'enable','off','backgroundcolor',Inactive);
                    set(UD.PlotMngr.DelIt,'enable','off');
                    set(UD.PlotMngr.ItInfo,'enable','off');
                    set(UD.PlotMngr.ItLink,'enable','off');
                    %
                    set(UD.PlotMngr.ItList2,'string',' ','value',1, ...
                        'enable','off','backgroundcolor',Inactive);
                else
                    prevseparator=0;
                    it=length(Items);
                    if it>0
                        prevseparator=1;
                        Nms = cell(1,it);
                    end
                    while it>=1
                        %
                        % Backward compatible with cell version of PlotState ...
                        %
                        if isequal(UserDatas{it},'---')
                            Nms{it}=separator;
                            if prevseparator
                                Nms(it)=[];
                                UserDatas(it)=[];
                                Items(it)=[];
                                Tags(it)=[];
                            end
                            prevseparator=1;
                        elseif iscell(UserDatas{it}.PlotState)
                            Nms{it}=UserDatas{it}.PlotState{2}.Name;
                            prevseparator=0;
                        else
                            Nms{it}=UserDatas{it}.PlotState.Props.Name;
                            prevseparator=0;
                        end
                        it=it-1;
                    end
                    if prevseparator
                        Nms(1)=[];
                        UserDatas(1)=[];
                        Items(1)=[];
                        Tags(1)=[];
                    end
                    for it=1:length(Items)
                        it_same_name=find(strcmp(Nms{it},Nms));
                        extend=1;
                        out_of_options=0;
                        while length(it_same_name)>1 && ~strcmp(Nms{it},separator)
                            extrastr = repmat({''},1,length(Items));
                            for itloc=it_same_name
                                switch extend
                                    case 1
                                        extrastr{itloc}=abbrevfn(UserDatas{itloc}.PlotState.FI.Name);
                                    case 2
                                        stat=UserDatas{itloc}.PlotState.Selected{ST_};
                                        if ~isempty(stat)
                                            %stats=qpread(UserDatas{itloc}.PlotState.FI,UserDatas{itloc}.PlotState.Props,'stations');
                                            stats=UserDatas{itloc}.PlotState.Stations;
                                            if length(stat)>1
                                                extrastr{itloc}=['ST=' vec2str(stat,'nobrackets')];
                                            elseif stat==0
                                                extrastr{itloc}='all';
                                            elseif iscell(stats)
                                                extrastr{itloc}=stats{stat};
                                            else
                                                extrastr{itloc}=deblank(stats(stat,:));
                                            end
                                        end
                                    case 3
                                        if ~isempty(UserDatas{itloc}.PlotState.SubField)
                                            subflds=qpread(UserDatas{itloc}.PlotState.FI,UserDatas{itloc}.PlotState.Props,'subfields');
                                            extrastr{itloc}=subflds{UserDatas{itloc}.PlotState.SubField{1}};
                                        else
                                            extrastr{itloc}='';
                                        end
                                    case 4
                                        m=UserDatas{itloc}.PlotState.Selected{M_};
                                        if iscell(m)
                                            extrastr{itloc}=[m{1} ' line'];
                                        elseif isequal(m,0)
                                            extrastr{itloc}='All M';
                                        elseif ~isempty(m)
                                            extrastr{itloc}=['M=' vec2str(m,'nobrackets')];
                                        end
                                    case 5
                                        n=UserDatas{itloc}.PlotState.Selected{N_};
                                        if isequal(n,0)
                                            extrastr{itloc}='All N';
                                        elseif ~isempty(n)
                                            extrastr{itloc}=['N=' vec2str(n,'nobrackets')];
                                        end
                                    case 6
                                        k=UserDatas{itloc}.PlotState.Selected{K_};
                                        if isequal(k,0)
                                            extrastr{itloc}='All K';
                                        elseif ~isempty(k)
                                            extrastr{itloc}=['K=' vec2str(k,'nobrackets')];
                                        end
                                    case 7
                                        extrastr{itloc}=UserDatas{itloc}.PlotState.Ops.presentationtype;
                                    case 8
                                        t=UserDatas{itloc}.PlotState.Selected{T_};
                                        if isequal(t,0)
                                            extrastr{itloc}='All TS';
                                        elseif ~isempty(t)
                                            extrastr{itloc}=['TS=' vec2str(t,'nobrackets')];
                                        end
                                    otherwise
                                        out_of_options=1;
                                        break
                                end
                            end
                            if out_of_options
                                break
                            end
                            it_extra_same=find(strcmp(extrastr{it},extrastr(it_same_name)));
                            if length(it_extra_same)<length(it_same_name)
                                for itloc=it_same_name
                                    Nms{itloc}=cat(2,Nms{itloc},' - ',extrastr{itloc});
                                end
                                it_same_name=it_same_name(it_extra_same);
                            end
                            extend=extend+1;
                        end
                    end
                    %
                    % Select the item with the same tag.
                    %
                    val = [];
                    valOld = get(UD.PlotMngr.ItList,'value');
                    InfoOld = get(UD.PlotMngr.ItList,'userdata');
                    if iscell(InfoOld)
                        TagOld = InfoOld{1};
                        for i = length(valOld):-1:1
                            val(i) = ustrcmpi(TagOld(valOld(i)),Tags);
                        end
                        val(val<0) = [];
                    end
                    %
                    % If no item with same name was found, then select the
                    % first item that's not a separator.
                    %
                    if isempty(val)
                        val=1;
                        while val<length(Nms) && strcmp(Nms{val},separator)
                            val=val+1;
                        end
                    end
                    %
                    % if there are only separators, select none
                    %
                    if strcmp(Nms{val},separator)
                        val=[];
                    end
                    set(UD.PlotMngr.ItList,'string',Nms, ...
                        'userdata',{Tags Items},'value',val, ...
                        'enable','on','backgroundcolor',Active);
                    %
                    set(UD.PlotMngr.ItList2,'string',Nms, ...
                        'value',val(1), ...
                        'enable','on','backgroundcolor',Active);
                    %
                    % buttons should not be enabled if a separator is selected
                    %
                    enable='on';
                    if isempty(val)
                        enable='off';
                    end
                    set(UD.PlotMngr.DelIt,'enable',enable);
                    set(UD.PlotMngr.ItInfo,'enable',enable);
                    set(UD.PlotMngr.ItLink,'enable',enable);
                end
            end
        end
        qp_plotmanager('updatearrows',UD)

    case 'updatearrows'
        Ax = getAx(UD);
        if strcmp(get(UD.PlotMngr.ItList,'enable'),'off') || length(Ax)>1
            set(UD.PlotMngr.ItUp,'enable','inactive', ...
                'cdata',getappdata(UD.PlotMngr.ItUp,'ArrowInactive'));
            set(UD.PlotMngr.ItDown,'enable','inactive', ...
                'cdata',getappdata(UD.PlotMngr.ItDown,'ArrowInactive'));
        else
            it  = get(UD.PlotMngr.ItList,'value');
            its = get(UD.PlotMngr.ItList,'string');
            if length(it)~=1 || it==1
                set(UD.PlotMngr.ItUp,'enable','inactive', ...
                    'cdata',getappdata(UD.PlotMngr.ItUp,'ArrowInactive'));
            else
                set(UD.PlotMngr.ItUp,'enable','on', ...
                    'cdata',getappdata(UD.PlotMngr.ItUp,'ArrowActive'));
            end
            if length(it)~=1 || it==length(its)
                set(UD.PlotMngr.ItDown,'enable','inactive', ...
                    'cdata',getappdata(UD.PlotMngr.ItDown,'ArrowInactive'));
            else
                set(UD.PlotMngr.ItDown,'enable','on', ...
                    'cdata',getappdata(UD.PlotMngr.ItDown,'ArrowActive'));
            end
        end

    case {'moveitemup','moveitemdown'}
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            d3d_qp refreshaxes
            d3d_qp refreshfigprop
        else
            pfig = get(Ax,'parent');
            ItInfo = get(UD.PlotMngr.ItList,'userdata');
            ItVal = get(UD.PlotMngr.ItList,'value');
            ItTags = ItInfo{1};
            ItHand = ItInfo{2};
            %
            ItTag1 = ItTags{ItVal};
            hIt1 = findall(pfig,'tag',ItTag1); % the object itself
            if length(hIt1)>1
                hItem1 = hIt1(~cellfun('isempty',get(hIt1,'userdata')));
            else
                hItem1 = hIt1;
            end
            ZCurrent1 = getappdata(hItem1,'Level');
            %
            switch cmd
                case 'moveitemup'
                    ItVal2 = ItVal-1;
                case 'moveitemdown'
                    ItVal2 = ItVal+1;
            end
            ItTag2 = ItTags{ItVal2};
            hIt2 = findall(pfig,'tag',ItTag2); % the object itself
            if length(hIt2)>1
                hItem2 = hIt2(~cellfun('isempty',get(hIt2,'userdata')));
            else
                hItem2 = hIt2;
            end
            ZCurrent2 = getappdata(hItem2,'Level');
            %
            setzcoord(hIt1,ZCurrent2)
            setappdata(hItem1,'Level',ZCurrent2)
            setzcoord(hIt2,ZCurrent1)
            setappdata(hItem2,'Level',ZCurrent1)
            %
            children = allchild(Ax);
            child1 = find(ismember(children,hIt1));
            child2 = find(ismember(children,hIt2));
            BeforeBoth = children(1:min([min(child1) min(child2)])-1);
            AfterBoth = children(max([max(child1) max(child2)])+1:end);
            switch cmd
                case 'moveitemup'
                    children = [BeforeBoth; children(child1); ...
                        children(child2); AfterBoth];
                case 'moveitemdown'
                    children = [BeforeBoth; children(child2); ...
                        children(child1); AfterBoth];
            end
            set(Ax,'children',children)
            %
            d3d_qp refreshitems
        end

    case 'itemlist'
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            d3d_qp refreshaxes
            d3d_qp refreshfigprop
        else
            ItInfo=get(UD.PlotMngr.ItList,'userdata');
            ItVal=get(UD.PlotMngr.ItList,'value');
            ItTags=ItInfo{1};
            ItIDs=ItInfo{2};
            OK=1;
            ItVal(ItIDs(ItVal)==0)=[];
            for itVal=ItVal
                ItTag=ItTags{itVal};
                hIt=findall(Ax,'tag',ItTag);
                if isempty(hIt)
                    d3d_qp refreshitems
                    d3d_qp refreshaxprop
                    OK=0;
                    break
                end
            end
            if OK
                set(UD.PlotMngr.ItList,'value',ItVal);
                if length(ItVal)==1
                    set(UD.PlotMngr.DelIt,'enable','on');
                    set(UD.PlotMngr.ItLink,'enable','on');
                    set(UD.PlotMngr.ItInfo,'enable','on');
                else
                    if isempty(ItVal)
                        set(UD.PlotMngr.DelIt,'enable','off');
                        set(UD.PlotMngr.ItLink,'enable','off');
                    end
                    set(UD.PlotMngr.ItInfo,'enable','off');
                end
            end
        end
        qp_plotmanager('updatearrows',UD)
        
    case 'iteminfo'
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            d3d_qp refreshaxes
            d3d_qp refreshfigprop
        else
            pfig=get(Ax,'parent');
            ItIDs=get(UD.PlotMngr.ItList,'userdata');
            ItVal=get(UD.PlotMngr.ItList,'value');
            %ItTags=get(UD.PlotMngr.ItList,'string');
            ItTags=ItIDs{1};
            ItTag=ItTags{ItVal};
            
            hIt=findall(pfig,'tag',ItTag);
            UserDatas=get(hIt,'userdata');
            if iscell(UserDatas)
                UserDatas=UserDatas(~cellfun('isempty',UserDatas));
                UserDatas=UserDatas{1};
            end
            if isstruct(UserDatas)
                if isfield(UserDatas,'XInfo') && ~isempty(UserDatas.XInfo)
                    locStruct.DataInfo=UserDatas.XInfo;
                end
                %
                % Backward compatible with cell version of PlotState ...
                %
                if iscell(UserDatas.PlotState)
                    nm=UserDatas.PlotState{2}.Name;
                    locStruct.PlotInfo=UserDatas.PlotState{end};
                else
                    nm=UserDatas.PlotState.Props.Name;
                    locStruct.PlotInfo=UserDatas.PlotState.Ops;
                end
                ui_inspectstruct(locStruct,nm);
            end
            
        end
        
    case 'deleteaxes'
        Ax = getAx(UD);
        if length(Ax)==1 && ishandle(Ax)
            pfig=get(Ax,'parent');
            Items=allchild(Ax);
            Tags=get(Items,'tag');
            UserDatas=get(Items,'userdata');
            TUDvalid=~cellfun('isempty',Tags) & ~cellfun('isempty',UserDatas);
            Tags=Tags(TUDvalid);
            QPTag=strncmp('QPPlotTag',Tags,9);
            Tags=Tags(QPTag);
            Tags=unique(Tags);
            for i=1:length(Tags)
                ItTag=Tags{i};
                hAnIt=findall(pfig,'userdata',ItTag);
                if ~isempty(hAnIt)
                    delete(hAnIt)
                end
            end
            delete(Ax)
        end
        d3d_qp refreshaxes
        d3d_qp refreshfigprop
        
    case 'deleteitems'
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            d3d_qp refreshaxes
            d3d_qp refreshfigprop
        else
            pfig=get(Ax,'parent');
            if iscell(pfig)
                pfig=unique(cat(1,pfig{:}));
            end
            ItIDs=get(UD.PlotMngr.ItList,'userdata');
            ItVal=get(UD.PlotMngr.ItList,'value');
            ItTags=ItIDs{1};
            for itVal=ItVal
                ItTag=ItTags{itVal};
                hIt=findall(pfig,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                delete(hIt)
                if ~isempty(hAnIt)
                    uicm=get(hAnIt(1),'parent');
                    delete(hAnIt)
                    ItFig=get(uicm,'parent');
                    animsld=findobj(ItFig,'tag','animslid');
                    if isempty(get(uicm,'children'))
                        animpush=findobj(ItFig,'tag','animpush');
                        set(animpush,'enable','off')
                    end
                    UD=get(animsld,'userdata');
                    iobj=1;
                    while iobj<=length(UD)
                        if strcmp(UD(iobj).Tag,ItTag)
                            UD(iobj)=[];
                        end
                        iobj=iobj+1;
                    end
                    animsldEnab='on';
                    if isempty(UD)
                        animsldEnab='off';
                    end
                    set(animsld,'enable',animsldEnab,'userdata',UD)
                end
            end
            d3d_qp refreshitems
            d3d_qp refreshaxprop
        end
        
    case 'linkitems'
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            d3d_qp refreshaxes
            d3d_qp refreshfigprop
        else
            pfig=get(Ax,'parent');
            if iscell(pfig)
                pfig=unique(cat(1,pfig{:}));
            end
            uicm=findall(pfig,'type','uicontextmenu');
            checked=findall(uicm,'checked','on');
            ItIDs=get(UD.PlotMngr.ItList,'userdata');
            ItVal=get(UD.PlotMngr.ItList,'value');
            ItTags=ItIDs{1};
            CanAnim={};
            for itVal=ItVal(1)
                ItTag=ItTags{itVal};
                %hIt=findall(Ax,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                hCanAnim=get(hAnIt,'children');
                for h=hCanAnim'
                    AnimInfo=get(h,'userdata');
                    nsteps=length(AnimInfo.Values);
                    CanAnim{end+1,1}=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                end
            end
            for itVal=ItVal
                ItTag=ItTags{itVal};
                %hIt=findall(Ax,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                hCanAnim=get(hAnIt,'children');
                CanAnimL={};
                for h=hCanAnim'
                    AnimInfo=get(h,'userdata');
                    nsteps=length(AnimInfo.Values);
                    CanAnimL{end+1,1}=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                end
                CanAnim=intersect(CanAnim,CanAnimL);
                
            end
            if isempty(CanAnim)
                if length(ItVal)>1
                    ui_message('error','Selected items do not have any animation field in common.')
                else
                    ui_message('error','This item has no animation field.')
                end
                return
            end
            if length(CanAnim)>1
                if ~isempty(cmdargs)
                    i = ustrcmpi(cmdargs{1},CanAnim);
                else
                    i = -1;
                end
                if i>0
                    AnimateThis=CanAnim{i};
                else
                    AnimateThis=ui_type(CanAnim);
                end
            else
                AnimateThis=CanAnim{1};
            end
            if isempty(AnimateThis)
                return
            end
            set(checked,'checked','off')
            AnimObj=[];
            for itVal=ItVal
                ItTag=ItTags{itVal};
                %hIt=findall(Ax,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                hCanAnim=get(hAnIt,'children');
                for h=hCanAnim'
                    AnimInfo=get(h,'userdata');
                    nsteps=length(AnimInfo.Values);
                    AniStr=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                    if strcmp(AniStr,AnimateThis)
                        set(h,'checked','on')
                        AnimObj(end+1).Fld=AnimInfo.Dim;
                        NSteps=length(AnimInfo.Values);
                        AnimObj(end).Tag=ItTag;
                        AnimObj(end).Values=AnimInfo.Values;
                    end
                end
            end
            animsld=findobj(pfig,'tag','animslid');
            hIt=findall(pfig,'tag',ItTags{ItVal(1)}); % get the first object
            hItUD=get(hIt,'userdata');
            if iscell(hItUD) % object consisting of multiple HG objects
                mainIt=find(~cellfun('isempty',hItUD));
                hItUD=hItUD{mainIt(1)};
            end
            %
            % Backward compatible with cell version of PlotState ...
            %
            if iscell(hItUD.PlotState)
                hItUD.PlotState=plotstatestruct(hItUD.PlotState);
            end
            t_=AnimObj(1).Fld;
            if t_==0
                t=hItUD.PlotState.SubField{1};
            else
                t=hItUD.PlotState.Selected{t_};
            end
            AnimMax=NSteps;
            sstep=[min(1/(AnimMax-1),.1) min(10/(AnimMax-1),.9)];
            set(animsld,'userdata',AnimObj,'value',1,'sliderstep',sstep,'Max',AnimMax,'enable','on','value',t)
            Str=sprintf('%i',t);
            if t_==0
                [Chk,sflds] = qp_getdata(hItUD.PlotState.FI, ...
                    hItUD.PlotState.Domain, ...
                    hItUD.PlotState.Props,'subfields',t);
                if Chk
                    Str=sflds{1};
                end
            end
            set(animsld,'tooltip',[DimStr{t_+1},Str]);
            qck_anim('slider',pfig(1))
        end
        
    case 'selectedfigure'
        FigureHandles=get(UD.PlotMngr.FigList,'userdata');
        if get(UD.PlotMngr.FigAll,'value')
            iFg=1:length(FigureHandles);
        else
            iFg=get(UD.PlotMngr.FigList,'value');
        end
        if isempty(FigureHandles)
            outdata = [];
        else
            outdata = FigureHandles(iFg);
        end
        
    case 'selectfigure'
        if ~isempty(cmdargs)
            h = cmdargs{1};
            if any(ishandle(h))
                %
                h(~ishandle(h))=[];
                for i = length(h):-1:1
                    while ~isequal(get(h(i),'type'),'figure') && ~isequal(get(h(i),'type'),'root')
                        h(i) = get(h(i),'parent');
                    end
                    if isequal(get(h(i),'type'),'root')
                        h(i) = [];
                    end
                end
                h = unique(h);
                %
                FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                iFg=find(ismember(FigureHandles,h));
                if length(iFg)<length(h)
                    %
                    % If a handle not found in list of figures, try
                    % once refreshing the list of figures.
                    %
                    d3d_qp refreshfigs
                    FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                    iFg=find(ismember(FigureHandles,h));
                end
                %
                % If still not found, continue without selecting the requested
                % figure.
                %
                if ~isempty(iFg)
                    d3d_qp('allfigures',length(iFg)>1)
                    if length(iFg)==1
                        set(UD.PlotMngr.FigList,'value',iFg)
                    end
                end
                %
            end
        end
        d3d_qp refreshaxes
        d3d_qp update_addtoplot
        d3d_qp refreshfigprop
        
    case 'selectedaxes'
        AxesHandles=get(UD.PlotMngr.AxList,'userdata');
        if get(UD.PlotMngr.AxAll,'value') || get(UD.PlotMngr.FigAll,'value')
            iAx=1:length(AxesHandles);
        else
            iAx=get(UD.PlotMngr.AxList,'value');
        end
        if isempty(AxesHandles)
            outdata = [];
        else
            outdata = AxesHandles(iAx);
        end
        
    case 'selectaxes'
        if ~isempty(cmdargs)
            h = cmdargs{1};
            if ischar(h)
                %
                AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                Tags = get(AxesHandles,'tag');
                iAx = ustrcmpi(h,Tags);
                if iAx<0
                    %
                    % If tag not found in list of axes, try once refreshing the
                    % list of axes.
                    %
                    d3d_qp refreshaxes
                    d3d_qp refreshfigprop
                    AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                    Tags = get(AxesHandles,'tag');
                    iAx = ustrcmpi(h,Tags);
                end
                %
                % If still not found, continue without selecting the requested
                % axes.
                %
                if iAx>0
                    set(UD.PlotMngr.AxList,'value',iAx)
                end
                %
            elseif any(ishandle(h))
                %
                h(~ishandle(h))=[];
                for i = length(h):-1:1
                    while ~isequal(get(h(i),'type'),'axes') && ~isequal(get(h(i),'type'),'figure') && ~isequal(get(h(i),'type'),'root')
                        h(i) = get(h(i),'parent');
                    end
                    if isequal(get(h(i),'type'),'figure') || isequal(get(h(i),'type'),'root')
                        h(i) = [];
                    end
                end
                h = unique(h);
                %
                hFig = get(h,'parent');
                if iscell(hFig)
                    hFig = cat(1,hFig{:});
                end
                d3d_qp('selectfigure',hFig);
                %
                AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                iAx=find(ismember(AxesHandles,h));
                if ~isempty(iAx)
                    d3d_qp('allaxes',length(iAx)>1)
                    if length(iAx)==1
                        set(UD.PlotMngr.AxList,'value',iAx)
                    end
                end
                %
            end
        end
        d3d_qp refreshaxes
        d3d_qp update_addtoplot
        d3d_qp refreshaxprop
        
    case 'selecteditem'
        ItemNames=get(UD.PlotMngr.ItList,'string');
        ItemHandles=get(UD.PlotMngr.ItList,'userdata');
        iItem = get(UD.PlotMngr.ItList,'value');
        nItems = max(1,length(iItem));
        outdata(nItems).Name='dummy';
        outdata(nItems).Tag='dummy';
        if isempty(iItem)
            outdata(1)=[];
        else
            for i = nItems:-1:1
                outdata(i).Name = ItemNames{iItem(i)};
                outdata(i).Tag = ItemHandles{1}{iItem(i)};
            end
        end
        
    case 'selectitem'
        if ~isempty(cmdargs)
            h = cmdargs{1};
            if isstruct(h)
                if ~isfield(h,'Tag')
                    error('Invalid structure provided select item call: missing Tag field!')
                end
                Tags = {h(:).Tag};
            elseif isnumeric(h)
                Tags = get(h,'tag');
                if isempty(Tags)
                    Tags = {};
                elseif ischar(Tags)
                    Tags = {Tags};
                end
            end
            %
            allObjs = findall(0);
            allTags = get(allObjs,'tag');
            %
            allObjs = allObjs(ismember(allTags,Tags));
            allAxs = get(allObjs,'parent');
            if iscell(allAxs)
                allAxs = cat(1,allAxs{:});
            end
            %
            if ~isempty(allAxs)
                d3d_qp('selectaxes',allAxs)
                %
                ItemHandles = get(UD.PlotMngr.ItList,'userdata');
                iIt = find(ismember(ItemHandles{1},Tags));
                set(UD.PlotMngr.ItList,'value',iIt)
            end
        else
            get(gcbo,'tag')
        end
        qp_plotmanager('updatearrows',UD)
        d3d_qp update_addtoplot

    case 'refreshfigprop'
        fig = qpsf;
        PM = UD.PlotMngr;
        if length(fig)==1
            set(PM.FigColor,'backgroundcolor',get(fig,'color'), ...
                'enable','on')
        else
            set(PM.FigColor,'backgroundcolor',Inactive, ...
                'enable','off')
        end

    case 'refreshaxprop'
        ax = qpsa;
        PM = UD.PlotMngr;
        if length(ax)==1
            clr = get(ax,'color');
            if isequal(clr,'none')
                set(PM.HasAxColor,'enable','on','value',0)
                set(PM.AxColor,'string','X','backgroundcolor',Inactive, ...
                    'enable','on')
            else
                set(PM.HasAxColor,'enable','on','value',1)
                set(PM.AxColor,'backgroundcolor',clr,'string','', ...
                    'enable','on')
            end
            xlim = get(ax,'xlim');
            ylim = get(ax,'ylim');
            set([PM.XLimitMin PM.XLimitMax PM.YLimitMin PM.YLimitMax], ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.XLimitMin,'string',num2str(xlim(1)))
            set(PM.XLimitMax,'string',num2str(xlim(2)))
            set(PM.YLimitMin,'string',num2str(ylim(1)))
            set(PM.YLimitMax,'string',num2str(ylim(2)))
            xgr  = valuemap(get(ax,'xgrid'),{'on' 'off'},[1 0]);
            ygr  = valuemap(get(ax,'ygrid'),{'on' 'off'},[1 0]);
            lbx  = valuemap(get(ax,'box'),{'on' 'off'},[1 0]);
            xsc  = valuemap(get(ax,'xscale'),{'linear','log'},[1 2]);
            ysc  = valuemap(get(ax,'yscale'),{'linear','log'},[1 2]);
            xlc  = valuemap(get(ax,'xaxislocation'),{'top','bottom'},[1 2]);
            ylc  = valuemap(get(ax,'yaxislocation'),{'left','right'},[1 2]);
            xcl  = get(ax,'xcolor');
            ycl  = get(ax,'ycolor');
            posu = get(ax,'unit');
            if ismember(posu,{'points','pixels','characters'})
                set(ax,'unit','centimeters')
                posu = 'centimenters';
            end
            pos = get(ax,'position');
            %
            if all(xlim>0)
                set(PM.XScale,'value',xsc, ...
                    'backgroundcolor',Active, ...
                    'enable','on')
            else
                set(PM.XScale,'value',1, ...
                    'backgroundcolor',Inactive, ...
                    'enable','off')
            end
            set(PM.XGrid,'value',xgr, ...
                'enable','on')
            set(PM.XLoc,'value',xlc, ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.XColor,'backgroundcolor',xcl, ...
                'enable','on')
            set(PM.XLabel,'string',get(get(ax,'xlabel'),'string'), ...
                'backgroundcolor',Active, ...
                'enable','inactive')
            if all(ylim>0)
                set(PM.YScale,'value',ysc, ...
                    'backgroundcolor',Active, ...
                    'enable','on')
            else
                set(PM.YScale,'value',1, ...
                    'backgroundcolor',Inactive, ...
                    'enable','off')
            end
            set(PM.YGrid,'value',ygr, ...
                'enable','on')
            set(PM.YLoc,'value',ylc, ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.YColor,'backgroundcolor',ycl, ...
                'enable','on')
            set(PM.YLabel,'string',get(get(ax,'ylabel'),'string'), ...
                'backgroundcolor',Active, ...
                'enable','inactive')
            set(PM.AxBox,'value',lbx, ...
                'enable','on')
            %
            posu  = valuemap(posu, ...
                {'centimeters','inches','normalized'}, ...
                1:3);
            set(PM.AxPosUnit,'value',posu, ...
                'backgroundcolor',Active, ...
                'enable','on')
            if posu==3
                pos = pos*100;
            end
            set(PM.AxXLowerLeft,'string',num2str(pos(1)), ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.AxYLowerLeft,'string',num2str(pos(2)), ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.AxWidth,'string',num2str(pos(3)), ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.AxHeight,'string',num2str(pos(4)), ...
                'backgroundcolor',Active, ...
                'enable','on')
        else
            set([PM.HasAxColor PM.AxColor PM.AxPosUnit PM.XColor PM.YColor], ...
                'backgroundcolor',Inactive, ...
                'enable','off')
            set([PM.XLimitMin PM.XLimitMax PM.YLimitMin PM.YLimitMax ...
                PM.AxXLowerLeft PM.AxYLowerLeft PM.AxWidth PM.AxHeight ...
                PM.XLabel PM.YLabel], ...
                'backgroundcolor',Inactive, ...
                'string','', ...
                'enable','off')
            set([PM.XGrid PM.YGrid PM.AxBox], ...
                'backgroundcolor',Inactive, ...
                'value',0, ...
                'enable','off')
            set([PM.XLoc PM.YLoc PM.XScale PM.YScale], ...
                'backgroundcolor',Inactive, ...
                'value',1, ...
                'enable','off')
        end

end

function Ax = getAx(UD)
AxIDs=get(UD.PlotMngr.AxList,'userdata');
AxVal=get(UD.PlotMngr.AxList,'value');
if get(UD.PlotMngr.FigAll,'value') || ...
        get(UD.PlotMngr.AxAll,'value') || ...
        isempty(AxIDs)
    Ax=AxIDs;
else
    Ax=AxIDs(AxVal);
end