function [DomainNr,Props,subf,selected,stats,Ops]=qp_interface_update_options(mfig,UD)
%QP_INTERFACE_UPDATE_OPTIONS Update QuickPlot user interface options.

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

T_=1; ST_=2; M_=3; N_=4; K_=5;
Inactive=UD.Inactive;
Active=UD.Active;

datafields=findobj(mfig,'tag','selectfield');
%
fld=get(datafields,'value');
Props=get(datafields,'userdata');
Props=Props(fld);
Units='';
if isfield(Props,'Units')
    Units=Props.Units;
end
[nval,nvalstr]=convertnval(Props.NVal);

DimFlag=Props.DimFlag;
if isfield(Props,'Geom') && ~isempty(Props.Geom)
    geometry=Props.Geom;
    coordinates=Props.Coords;
elseif nval<0
    if DimFlag(M_) && DimFlag(N_)
        geometry='sQUAD';
        coordinates='xy';
    else
        geometry='SELFPLOT';
        coordinates='';
    end
else
    if DimFlag(M_) && DimFlag(N_)
        geometry='sQUAD';
        coordinates='xy';
    elseif DimFlag(M_) || DimFlag(N_)
        geometry='sSEG';
        coordinates='x';
    else
        geometry='PNT';
        coordinates='';
    end
    if isfield(Props,'Tri') && isequal(Props.Tri,1)
        geometry='TRI';
        coordinates='xy';
    end
    if DimFlag(K_)
        geometry=cat(2,geometry,'+');
        coordinates=cat(2,coordinates,'+z');
    end
end
triangles=isequal(geometry,'TRI') | isequal(geometry,'TRI+');

selected=cell(1,5);
MW=UD.MainWin;
if strcmp(get(MW.SubFld,'enable'),'on')
    subf={get(MW.SubFld,'value')};
else
    subf={};
end
if DimFlag(T_)
    if get(MW.AllT,'value')
        selected{T_}=0;
    else
        selected{T_}=get(MW.EditT,'userdata');
    end
end
if DimFlag(ST_)
    stats=get(MW.StList,'userdata');
    alls=get(MW.AllS,'value');
    if alls
        selected{ST_}=0;
    else
        selected{ST_}=get(MW.EditS,'userdata');
    end
else
    stats={};
end

allm=0;
alln=0;
vslice=0;
hslice=0;
if all(~DimFlag([M_ N_ K_]))
else
    switch getvalstr(MW.HSelType)
        case 'M range and N range'
            if DimFlag(M_)
                allm=get(MW.AllM,'value');
                if allm
                    selected{M_}=0;
                else
                    selected{M_}=get(MW.EditM,'userdata');
                end
            else
                allm=0;
            end
            %maxm=get(MW.MaxM,'userdata');
            if DimFlag(N_)
                alln=get(MW.AllN,'value');
                if alln
                    selected{N_}=0;
                else
                    selected{N_}=get(MW.EditN,'userdata');
                end
            else
                alln=0;
            end
            %maxn=get(MW.MaxN,'userdata');
        case '(M,N) point/path'
            vslice=1;
            selected{M_}={'MN' get(MW.EditMN,'userdata')};
            if DimFlag(N_)
                selected{N_}=0;
            end
        case '(X,Y) point/path'
            vslice=1;
            selected{M_}={'XY' get(MW.EditXY,'userdata')};
            if DimFlag(N_)
                selected{N_}=0;
            end
    end
end

allt=get(findobj(mfig,'tag','allt'),'value');
maxt=get(findobj(mfig,'tag','max_t'),'userdata');

switch getvalstr(MW.VSelType)
    case {'K range'}
        if DimFlag(K_)
            if get(MW.AllK,'value')
                selected{K_}=0;
            else
                selected{K_}=get(MW.EditK,'userdata');
            end
        end
    case {'Z slice','dZ below surface','dZ above bed'}
end
set(setdiff(UD.Options.Handles,gcbo),'enable','off','backgroundcolor',Inactive)
Ops.thinningmode='none';
Ops.thinningfactors=[1 1 1];
Ops.thinningdistance=50;
Ops.vectorscalingmode='';
Ops.vectorscale=1;
Ops.thresholds='none';
Ops.vectorcolour='';
Ops.presentationtype='';
Ops.vectorcomponent='';
Ops.angleconvention='';
Ops.MNK=0;

Handle_Domain=findobj(mfig,'tag','selectdomain');
DomainNr=get(Handle_Domain,'value');

for i=5:-1:1
    multiple(i) = (length(selected{i})>1) | isequal(selected{i},0);
end
Ops.animate = multiple(T_);

MNK=0;
if isfield(Props,'MNK')
    MNK=Props.MNK;
end

%
%-------- PLOT OPTIONS -------------
%
OH = UD.Options.Handles;
%
vectors=0;
plotstexts=0;
usesmarker=0;
forcemarker=0;
forcemarkercolor=0;
markerflatfill=0;
edgeflatcolour=0;
lineproperties=0;
data2d=0;

thindams = nval>0 & nval<1;
MultipleColors = nval>=1 & nval<4;
%--------------------------------------------------------------------------
%
%---- axes type
%
axestype={'noplot'};
switch geometry
    case 'SELFPLOT'
        axestype={''};
    case 'PNT'
        if multiple(ST_) || multiple(M_)
            axestype={'X-Y'};
        elseif multiple(T_)
            if isequal(coordinates,'d')
                axestype={'Time-Val','Distance-Val'};
            elseif nval==0
                axestype={'X-Y'};
            else
                axestype={'Time-Val'};
            end
        else
            if isequal(coordinates,'d')
                axestype={'Time-Val','Distance-Val','Text'};
            elseif nval==0
                axestype={'X-Y'};
            else
                axestype={'Time-Val','Text'};
            end
        end
    case 'PNT+'
        if multiple(K_)
            if ~multiple(M_) && ~multiple(ST_)
                axestype={'Val-Z'};
            end
        else
            if multiple(M_) || multiple(ST_)
                axestype={'X-Y'};
            elseif multiple(T_)
                axestype={'Time-Val','X-Y'};
            else
                axestype={'Time-Val','Text'};
            end
        end
    case 'SEG'
        axestype={'X-Y'};
        lineproperties=1;
    case 'sSEG'
        if multiple(M_)
            switch nval
                case {0,2,4}
                    axestype={'X-Y'};
                case 1
                    axestype={'X-Val'};
            end
        else
            switch nval
                case {0,2,4}
                    axestype={'X-Y'};
                case 1
                    axestype={'Time-Val','X-Val'};
            end
        end
    case 'sSEG+'
        if multiple(M_) && multiple(K_)
            axestype={'X-Z'};
        elseif multiple(M_)
            axestype={'X-Val'};
        elseif multiple(K_)
            axestype={'Val-Z'};
        else
            axestype={'X-Z'};
        end
    case {'POLYL','POLYG'}
        axestype={'X-Y'};
        lineproperties=1;
    case {'sQUAD','sQUAD+'}
        if multiple(K_)
            if multiple(M_) && multiple(N_) && ~vslice
                axestype={'X-Y-Z'};
            elseif multiple(M_) || multiple(N_) || vslice
                axestype={'X-Z'};
            elseif multiple(T_)
                axestype={'Val-Z','Time-Z'};
            else
                axestype={'Val-Z'};
            end
        else
            if nval==0
                axestype={'X-Y'};
            elseif nval==4
                if ~multiple(T_) && ~multiple(M_) && ~multiple(N_)
                    axestype={'X-Y','Text'};
                else
                    axestype={'X-Y'};
                end
            else
                if multiple(M_) && multiple(N_) && ~vslice
                    axestype={'X-Y','X-Y-Val'};
                elseif multiple(M_) || multiple(N_) || vslice
                    if multiple(T_)
                        axestype={'X-Val','X-Y','X-Time','Time-X'};
                    else
                        axestype={'X-Val','X-Y'};
                    end
                elseif multiple(T_)
                    axestype={'Time-Val','X-Y','X-Val'};
                else
                    axestype={'X-Y','X-Val','Text'};
                end
            end
        end
    case {'TRI','TRI+'}
        if multiple(M_) && multiple(K_)
            if vslice
                axestype={'X-Z'};
            else
                axestype={'X-Y-Z'};
            end
        elseif multiple(M_)
            if ~vslice || nval==0
                axestype={'X-Y'};
            else
                axestype={'X-Val','X-Y'};
            end
        elseif multiple(K_)
            axestype={'Val-Z'};
        else
            if multiple(T_)
                axestype={'Time-Val'};
            else
                axestype={'X-Y'};
            end
        end
    case 'QUAD'
    case 'GEN2D'
    case 'GEN-2D'
    case 'SEG+'
    case 'QUAD+'
    case 'POLY+'
    case 'GEN2D+'
    case 'sHEX'
    case 'TET'
    case 'WED' % WEDGE aka PRISM
    case 'HEX'
    case 'PYR'
    case 'GEN3D'
    case 'GEN-3D'
end

h_axtype=findobj(OH,'tag','axestype=?');
if length(axestype)>1
    set(findobj(OH,'tag','axestype'),'enable','on');
    paxestype = get(h_axtype,'string');
    if isequal(paxestype',axestype)
        %
        % if the supported axes types are the same then use the previously
        % selected axes type
        %
        i = get(h_axtype,'value');
    else
        %
        % if the supported axes types are different then use the axes type
        % of the current plot if an axes object is select and if that axes
        % type is supported, otherwise use the default first axes type.
        %
        ax=qpsa;
        if isempty(ax)
            i=1;
        else
            paxestype = getappdata(ax,'BasicAxesType');
            if isempty(paxestype)
                paxestype = getappdata(ax,'AxesType');
            end
            i = [];
            if ~isempty(paxestype)
                i = strmatch(paxestype,axestype,'exact');
            end
            if isempty(i)
                i=1;
            end
        end
    end
    set(h_axtype,'string',axestype,'value',i,'enable','on','backgroundcolor',Active)
else
    set(h_axtype,'string',{'no choice'},'value',1)
    i=1;
end
axestype=axestype{i};

if strcmp(axestype,'X-Y-Val')
    data2d=1;
elseif strfind(axestype,'Val')
    MultipleColors=0;
    lineproperties=1;
    if  strcmp(axestype,'Time-Val') && ~multiple(T_)
        plotstexts=1;
        Ops.numformat='%g';
    end
elseif strcmp(axestype,'X-Y') || strcmp(axestype,'X-Z')
    data2d=1;
elseif strcmp(axestype,'X-Time') || strcmp(axestype,'Time-X') || strcmp(axestype,'Time-Z')
    data2d=1;
elseif strcmp(axestype,'Text') || (strcmp(axestype,'Time-Val') && ~multiple(T_))
    MultipleColors=0;
    plotstexts=1;
    Ops.numformat='%g';
end
if nval<1
    lineproperties=1;
end
if ~isempty(strfind(axestype,'Time'))
    Ops.animate = 0;
elseif ~multiple(M_) && ~multiple (N_) && ~multiple(K_) && nval==0
    Ops.animate = 0;
elseif strcmp(axestype,'Distance-Val')
    Ops.animate = 0;
end

coords={'path distance','reverse path distance','x coordinate','y coordinate'};

if strfind(axestype,'X')
    Spatial=1;
else
    Spatial=0;
end
if strfind(axestype,'Y')
    Spatial=Spatial+1;
end
SpatialH=Spatial;
if strfind(axestype,'Z')
    Spatial=Spatial+1;
    SpatialV=1;
else
    SpatialV=0;
end
Ops.spatial=Spatial;
Ops.spatialh=SpatialH;
Ops.spatialv=SpatialV;

if strcmp(axestype,'X-Y-Z') || (multiple(K_) && DimFlag(N_) && nval==2) % cannot plot 3D volumes and vector datasets containing no vertical component
    %won't plot
    axestype='noplot';
end

if strfind(axestype,'Y')
    if isfield(Props,'MName') && ~isempty(Props.MName)
        axestype = strrep(axestype,'X',Props.MName);
    end
    if isfield(Props,'NName') && ~isempty(Props.NName)
        axestype = strrep(axestype,'Y',Props.NName);
    end
else
    if isfield(Props,'MName') && ~isempty(Props.MName) && multiple(M_)
        axestype = strrep(axestype,'X',Props.MName);
        coords={'x coordinate'};
    elseif isfield(Props,'NName') && ~isempty(Props.NName)
        axestype = strrep(axestype,'X',Props.NName);
        coords={'y coordinate'};
    end
end

if ismember(axestype,{'X-Val','X-Z','X-Time','Time-X'})
    pd=findobj(OH,'tag','plotcoordinate=?');
    prev_coords=get(pd,'string');
    i=get(pd,'value');
    plotcoord=prev_coords{i};
    %
    j=strmatch(plotcoord,coords,'exact');
    if ~isempty(j)
        i=j;
    else
        i=1;
    end
    %
    if length(coords)>1
        set(findobj(OH,'tag','plotcoordinate'),'enable','on');
        set(pd,'string',coords,'value',i,'enable','on','backgroundcolor',Active)
    end
    Ops.plotcoordinate=coords{i};
end

if ~isempty(axestype)
    SingleColor=~MultipleColors;
elseif nval==-1
    MultipleColors=0;
    SingleColor=1;
else
    MultipleColors=0;
    SingleColor=0;
end

%--------------------------------------------------------------------------
%
%---- vector component
%
if nval==2 || nval==3
    vectors=1;
    set(findobj(OH,'tag','component'),'enable','on');
    compon=findobj(OH,'tag','component=?');
    if DimFlag(M_) && (DimFlag(N_) || triangles)
        if MNK<2
            if MNK>0
                if DimFlag(K_) && DimFlag(M_) && DimFlag(N_)
                    compList={'vector','vector (split x,y)','vector (split m,n)','patch centred vector','magnitude','magnitude in plane','angle','x component','y component','z component','m component','n component'};
                    if Ops.spatialh ~=2
                        ii=strmatch('magnitude in plane',compList,'exact');
                        compList(ii)=[];
                    end
                    if Ops.spatial==2 && Ops.spatialh==1
                        compList{end+1}='normal component';
                    end
                else
                    compList={'vector','vector (split x,y)','vector (split m,n)','patch centred vector','magnitude','angle','x component','y component','m component','n component'};
                end
            else
                compList={'vector','vector (split x,y)','patch centred vector','magnitude','angle','x component','y component'};
            end
        else
            compList={'vector','patch centred vector','magnitude','m component','n component'};
        end
    elseif DimFlag(M_) && DimFlag(K_)
        compList={'vector','patch centred vector','magnitude','x component','z component'};
    else
        switch nvalstr
            case 'xy'
                compList={'vector','magnitude','angle'};
                if MNK<2
                    compList(end+1:end+2)={'x component','y component'};
                end
                if MNK>0
                    compList(end+1:end+2)={'m component','n component'};
                end
            case 'xyz'
                compList={'vector','magnitude','angle','x component','y component','z component'};
            case 'xz'
                compList={'vector','magnitude','x component','z component'};
        end
    end

    if Ops.spatialv
        ii=strmatch('vector (split',compList);
        compList(ii)=[];
    elseif Ops.spatial==1
        ii=strmatch('vector',compList);
        compList(ii)=[];
    end
    ii=strmatch('patch centred vector',compList,'exact');
    compList(ii)=[];

    set(compon,'enable','on','backgroundcolor',Active)
    comp=get(compon,'value');
    prevCompList=get(compon,'string');
    if ~isequal(compList,prevCompList)
        % try to find an exact match when switching presentation type strings
        if iscellstr(prevCompList)
            comp=prevCompList{comp};
        else
            comp=prevCompList(comp,:);
        end
        comp=strmatch(comp,compList,'exact');
        if isempty(comp)
            comp=1;
        end
        set(compon,'value',1,'string',compList,'value',comp)
    end
    Ops.vectorcomponent=lower(compList{comp});
    switch Ops.vectorcomponent
        case {'vector','patch centred vector','vector (split x,y)','vector (split m,n)'}
            Ops.presentationtype=Ops.vectorcomponent;
            if (multiple(M_) + multiple(N_) == 1) && (multiple(K_) == 1) && MNK
                Ops.MNK=1;
            end
        case {'magnitude','x component','y component','z component'}
            vectors=0;
        case 'angle'
            vectors=0;
            Units = 'radians';
            Ops.angleconvention='?';
        case {'magnitude in plane','m component','n component','normal component'}
            vectors=0;
            Ops.MNK=1;
        case 'edge'
            Ops.presentationtype=Ops.vectorcomponent;
            vectors=0;
            nval=0.9;
        otherwise
            ui_message('error','Unexpected plot type encountered: %s\nin main module.',Ops.vectorcomponent)
            Ops.presentationtype='failed';
            return
    end
end
if (nval==2 || nval==3) && ~vectors
    nval=1;
end
if isequal(Ops.vectorcomponent,'vector')
    geometry='sSEG';
    Props.Geom='sSEG';
    Props.ClosedPoly=0;
end
%--------------------------------------------------------------------------
%
%---- presentation type
%
extend2edge = 0;
if (nval==1 && data2d && ~strcmp(geometry,'SEG')) || nval==1.9 || strcmp(nvalstr,'strings') || strcmp(nvalstr,'boolean') || strcmp(geometry,'POLYG') % || (nval==0 & ~DimFlag(ST_))
    switch nvalstr
        case 1.9
            PrsTps={'vector','edge'};
        case 'strings'
            if multiple(T_)
                PrsTps={'tracks'}; % {'labels';'tracks'};
            else
                PrsTps={'labels';'markers'};
            end
        case 'boolean'
            PrsTps={'patches'};
       case 'none'
          switch geometry
             case {'POLYG'}
                PrsTps={'polygons'};
             otherwise
                PrsTps={'grid','grid with numbers'};
          end
        otherwise
            if isfield(Props,'DataInCell')
                dic=Props.DataInCell;
            else
                dic=0;
            end
            switch dic
                case 0
                    if triangles
                        if Ops.spatialv
                            PrsTps={'continuous shades';'markers';'values'};
                        else
                            PrsTps={'patches';'patches with lines';'continuous shades';'markers';'values';'contour lines';'coloured contour lines';'contour patches';'contour patches with lines'};
                        end
                    elseif isequal(geometry,'PNT')
                        PrsTps={'markers';'values'};
                    elseif isequal(geometry,'POLYL')
                        PrsTps={'polylines'};
                    else
                        PrsTps={'continuous shades';'markers';'values';'contour lines';'coloured contour lines';'contour patches';'contour patches with lines'};
                    end
                case 1
                    switch geometry
                        case {'POLYG'}
                            if DimFlag(M_) && DimFlag(N_)
                                PrsTps={'polygons';'markers';'values';'continuous shades';'contour lines';'coloured contour lines';'contour patches';'contour patches with lines'};
                            else
                                PrsTps={'polygons';'markers';'values'};
                            end
                        otherwise
                            PrsTps={'patches';'patches with lines';'continuous shades';'markers';'values';'contour lines';'coloured contour lines';'contour patches';'contour patches with lines'};
                    end
                case 2
                    switch geometry
                        case {'POLYG'}
                           PrsTps={'polygons'};
                       otherwise
                          PrsTps={'patches';'patches with lines'};
                    end
            end
    end
    if length(PrsTps)==1
       p=1;
    else
       set(findobj(OH,'tag','presenttype'),'enable','on')
       pt=findobj(OH,'tag','presenttype=?');
       pPrsTps=get(pt,'string');
       if isequal(pPrsTps,PrsTps)
          set(pt,'enable','on','backgroundcolor',Active)
          p=get(pt,'value');
       else
          % try to find an exact match when switching presentation type strings
          p=get(pt,'value');
          if iscellstr(pPrsTps),
             p=pPrsTps{p};
          else
             p=pPrsTps(p,:);
          end
          p=strmatch(p,PrsTps,'exact');
          if isempty(p),
             p=1;
          end
          set(pt,'enable','on','value',1,'string',PrsTps,'value',p,'backgroundcolor',Active)
       end
    end
    Ops.presentationtype=lower(PrsTps{p});
    switch Ops.presentationtype
        case 'patches with lines'
            SingleColor=1;
        case 'continuous shades'
            extend2edge = 1;
        case 'values'
            MultipleColors=0;
            SingleColor=1;
            %
            plotstexts=1;
            %
            Ops.numformat='%g';
            Ops.thinningmode='?';
            if strcmp(geometry,'POLYG')
                geometry='PNT';
            end
        case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
            Ops.thresholds='get_from_user';
            switch Ops.presentationtype
                case 'contour lines'
                    MultipleColors=0;
                    SingleColor=1;
                    lineproperties=1;
                case 'coloured contour lines'
                    lineproperties=1;
                case 'contour patches with lines'
                    SingleColor=1;
                    lineproperties=1;
            end
            extend2edge = 1;
        case 'markers'
            usesmarker=1;
            forcemarker=1;
            switch nvalstr
                case {'strings'}
                    SingleColor=0;
                    forcemarkercolor=1;
                otherwise
                    markerflatfill=1;
                    %
                    Ops.thinningmode='?';
            end
            if strcmp(geometry,'POLYG')
                geometry='PNT';
            end
        case 'patches'
            if strcmp(nvalstr,'boolean')
                SingleColor=1;
                MultipleColors=0;
            end
        case 'labels'
            plotstexts=1;
            SingleColor=1;
        case 'polylines'
            markerflatfill=nval>0;
            edgeflatcolour=nval>0;
        case 'grid with numbers'
            plotstexts=1;
        case 'edge'
            thindams=1;
            lineproperties=1;
            nval=0.9;
        case 'vector'
            vectors=1';
            Ops.vectorcomponent='edge';
    end
end

%--------------------------------------------------------------------------

if vectors %&& ~isempty(strmatch(axestype,{'X-Y','X-Y-Z','X-Y-Val','X-Z'},'exact'))
    colvect=findobj(OH,'tag','colourvectors');
    set(colvect,'enable','on')
    if get(colvect,'value')
        switch Ops.vectorcomponent
            case {'vector (split x,y)','vector (split m,n)','edge'}
                Ops.vectorcolour='component';
            otherwise
                colvecm=findobj(OH,'tag','vectorcolour=?');
                pvecCLR=get(colvecm,'string');
                colveci=get(colvecm,'value');
                if Ops.MNK
                    vecCLR={'magnitude in plane','m component','n component','normal component','edge'};
                    vecCLRi=ismember(vecCLR,compList);
                    vecCLR(~vecCLRi)=[];
                else
                    vecCLR={'magnitude','angle','x component','y component','z component','edge'};
                    vecCLRi=ismember(vecCLR,compList);
                    vecCLR(~vecCLRi)=[];
                end
                if ~isequal(vecCLR,pvecCLR)
                    % try to find an exact match when switching vector colouring strings
                    if isempty(pvecCLR) || length(pvecCLR)<colveci
                        colveci=1;
                    else
                       colveci=strmatch(pvecCLR{colveci},vecCLR,'exact');
                       if isempty(colveci)
                           colveci=1;
                       end
                    end
                    set(colvecm,'value',1,'string',vecCLR,'value',colveci)
                end
                if length(vecCLR)>1
                    set(colvecm,'enable','on','backgroundcolor',Active)
                end
                Ops.vectorcolour=vecCLR{colveci};
        end
        if strcmp(Ops.vectorcolour,'angle')
            Units = 'radians';
            Ops.angleconvention='?';
        end
    else
        Ops.vectorcolour='';
        MultipleColors=0;
        SingleColor=1;
    end
    %
    if ~strcmp(Ops.vectorcomponent,'edge')
        Ops.thinningmode='?';
    end
end

%--------------------------------------------------------------------------
%
%---- data units
%
if ~isempty(Units)
    set(findobj(OH,'tag','dataunits'),'enable','on')
    dunit=findobj(OH,'tag','dataunits=?');
    set(dunit,'enable','on', ...
        'backgroundcolor',Active)
    system=get(dunit,'value');
    systems=get(dunit,'string');
    if system==1
        % As in file
        qp_settings('UnitSystem',systems{system})
        user_units=Units;
        dunit=findobj(OH,'tag','dataunits=!');
        set(dunit,'backgroundcolor','r') % needed to get next line working properly R2011b
        set(dunit,'backgroundcolor',Inactive)
        set(dunit,'enable','inactive')
        set(dunit,'string',user_units)
        actualunits=user_units;
    elseif system==length(systems)
        % Hide
        user_units='**Hide**';
        Units='**Hide**';
        actualunits='**Hide**';
    elseif system==length(systems)-1
        % Other (user specified)
        dunit=findobj(OH,'tag','dataunits=!');
        set(dunit,'enable','on', ...
            'backgroundcolor',Active)
        user_units=get(dunit,'string');
        conversion=qp_unitconversion(Units,user_units);
        if ischar(conversion)
            user_units=Units;
        else
            Units=user_units;
        end
        set(dunit,'string',user_units)
        actualunits=user_units;
    else
        % Specified format: SI, CGS, etc.
        qp_settings('UnitSystem',systems{system})
        user_units=systems{system};
        dunit=findobj(OH,'tag','dataunits=!');
        set(dunit,'enable','inactive', ...
            'backgroundcolor',Inactive)
        [conversion,SIunit]=qp_unitconversion(Units,user_units);
        set(dunit,'string',SIunit)
        actualunits=SIunit;
    end
    Ops.units=user_units;
else
    actualunits='';
    Ops.units='';
end

if strcmp(Ops.angleconvention,'?')
    pd=findobj(OH,'tag','angleconvention=?');
    conventions=get(pd,'string');
    i=get(pd,'value');
    Ops.angleconvention=conventions{i};
    %
    set(findobj(OH,'tag','angleconvention'),'enable','on');
    set(pd,'enable','on','backgroundcolor',Active)
end

Ops.colourdams=0;
if thindams
    if nval==0.9
        Ops.colourdams=1;
    else
        coldams=findobj(OH,'tag','colourdams');
        set(coldams,'enable','on')
        Ops.colourdams=get(coldams,'value');
    end
    MultipleColors=Ops.colourdams;
    SingleColor=~MultipleColors;
    edgeflatcolour=MultipleColors;
end

if vectors && ~isempty(strmatch(axestype,{'X-Y','X-Y-Z','X-Y-Val','X-Z'},'exact'))
    set(findobj(OH,'tag','vectorstyle'),'enable','on')
    vstyle=findobj(OH,'tag','vectorstyle=?');
    set(vstyle,'enable','on','backgroundcolor',Active)
    vstyles=get(vstyle,'string');
    Ops.vectorstyle=vstyles{get(vstyle,'value')};
    %
    set(findobj(OH,'tag','vecscalem'),'enable','on')
    vsmode=findobj(OH,'tag','vecscalem=?');
    set(vsmode,'enable','on','backgroundcolor',Active)
    vsmodes=get(vsmode,'string');
    Ops.vectorscalingmode=vsmodes{get(vsmode,'value')};
    switch Ops.vectorscalingmode
        case {'automatic','automatic normalised'}
            Ops.vectorscale=1;
        case {'manual','manual normalised'}
            oneunitis=findobj(OH,'tag','1vecunit=?');
            set(findobj(OH,'tag','1vecunit'),'enable','on')
            set(oneunitis,'enable','on','backgroundcolor',Active)
            Ops.vectorscale=get(oneunitis,'userdata');
    end
end

Ops.verticalscalingmode='unrestricted';
if vectors && ~isempty(strmatch(axestype,{'X-Z' 'X-Y-Z'},'exact'))
    set(findobj(OH,'tag','vertscalem'),'enable','on')
    vsm=findobj(OH,'tag','vertscalem=?');
    set(vsm,'enable','on','backgroundcolor',Active)
    VsMeths=get(vsm,'string');
    vsm=get(vsm,'value');
    Ops.verticalscalingmode=lower(VsMeths{vsm});
    switch Ops.verticalscalingmode
        case 'manual'
            set(findobj(OH,'tag','vscale'),'enable','on')
            enl=findobj(OH,'tag','vscale=?');
            set(enl,'enable','on','backgroundcolor',Active)
            Ops.verticalscalefactor=get(enl,'userdata');
    end
end

if extend2edge
    h = findobj(OH,'tag','extend2edge');
    if multiple(M_)+multiple(N_)+multiple(K_)==2
        set(h,'enable','on')
        Ops.extend2edge = get(h,'value');
    else
        %set(h,'enable','inactive','value',1)
        switch axestype
            case {'X-Time','Time-X','Time-Z'}
                Ops.extend2edge = 0;
            otherwise
                Ops.extend2edge = 1;
        end
    end
end

if isfield(Ops,'numformat')
    set(findobj(OH,'tag','numformat'),'enable','on');
    numform=findobj(OH,'tag','numformat=?');
    Ops.numformat=get(numform,'string');
    set(numform,'enable','on','backgroundcolor',Active);
end

Ops.horizontalalignment='centre';
Ops.verticalalignment='middle';
Ops.fontsize=6;
if plotstexts
    set(findobj(OH,'tag','fontsize'),'enable','on');
    hFontsize=findobj(OH,'tag','fontsize=?');
    Ops.fontsize=get(hFontsize,'userdata');
    set(hFontsize,'enable','on','backgroundcolor',Active);

    set(findobj(OH,'tag','alignment'),'enable','on');
    set(findobj(OH,'tag','horizontalalignment'),'enable','on');
    set(findobj(OH,'tag','verticalalignment'),'enable','on');
    hHorAlign=findobj(OH,'tag','horizontalalignment=?');
    iHorAlign=get(hHorAlign,'value');
    strHorAlign=get(hHorAlign,'string');
    Ops.horizontalalignment=strHorAlign{iHorAlign};
    set(hHorAlign,'enable','on','backgroundcolor',Active);
    hVerAlign=findobj(OH,'tag','verticalalignment=?');
    iVerAlign=get(hVerAlign,'value');
    strVerAlign=get(hVerAlign,'string');
    Ops.verticalalignment=strVerAlign{iVerAlign};
    set(hVerAlign,'enable','on','backgroundcolor',Active);
end

if strcmp(Ops.thinningmode,'?')
    set(findobj(OH,'tag','thinfld'),'enable','on');
    thinfld=findobj(OH,'tag','thinfld=?');
    set(thinfld,'enable','on','backgroundcolor',Active)
    thinmodes = {'none','uniform','distance'}';
    if triangles
        thinmodes(2)=[];
    end
    prevthinmodes = get(thinfld,'string');
    thinmode = prevthinmodes{get(thinfld,'value')};
    if ~isequal(prevthinmodes,thinmodes)
        thinmode=thinmodes{1};
        set(thinfld,'string',thinmodes,'value',1)
    end
    Ops.thinningmode=thinmode;
    switch lower(Ops.thinningmode)
        case 'none'
        case 'uniform'
            set(findobj(OH,'tag','thinfact'),'enable','on');
            thinfact=findobj(OH,'tag','thinfact=?');
            set(thinfact,'enable','on','backgroundcolor',Active);
            Ops.thinningfactors=get(thinfact,'userdata')*[1 1 1];
        case 'distance'
            set(findobj(OH,'tag','thindist'),'enable','on');
            thindist=findobj(OH,'tag','thindist=?');
            set(thindist,'enable','on','backgroundcolor',Active);
            Ops.thinningdistance=get(thindist,'userdata');
    end
end

Ops.colour=[1 0 0];
if SingleColor
    set(findobj(OH,'tag','colour'),'enable','on')
    clrh=findobj(OH,'tag','colour=?');
    clr=get(clrh,'userdata');
    set(clrh,'enable','on','backgroundcolor',clr)
    Ops.colour=clr;
end

Ops.facecolour='none';
if isfield(Props,'ClosedPoly')
    if isequal(Props.ClosedPoly,1) && ~strcmp(geometry,'PNT')
        fpoly=findobj(OH,'tag','fillpolygons');
        set(fpoly,'enable','on')
        fillpoly=get(fpoly,'value');
    elseif isequal(Props.ClosedPoly,2)
        fillpoly=1;
    else
        fillpoly=0;
    end
    if fillpoly
        if MultipleColors
            Ops.facecolour='yes';
        else
            clrh=findobj(OH,'tag','facecolour=?');
            clr=get(clrh,'userdata');
            set(clrh,'enable','on','backgroundcolor',clr)
            Ops.facecolour=clr;
        end
    end
end

Ops.textboxfacecolour='none';
if plotstexts
    if matlabversionnumber>=6.05
        hTextbox=findobj(OH,'tag','textbox=?');
        set(hTextbox,'enable','on');
        if get(hTextbox,'value')
            hTextboxcolour=findobj(OH,'tag','textboxfacecolour=?');
            Ops.textboxfacecolour=get(hTextboxcolour,'userdata');
            set(hTextboxcolour,'enable','on','backgroundcolor',Ops.textboxfacecolour);
        end
    end
end

if ismember(geometry,{'PNT'}) && ~multiple(T_) && nval>=0 && nval<4
    Ops.linestyle='none';
    Ops.linewidth=0.5;
    if ~strcmp(Ops.presentationtype,'values')
        usesmarker = 1;
        forcemarker = 1;
    end
elseif lineproperties || nval==0 %|| nval==4
    set(findobj(OH,'tag','linestyle'),'enable','on')
    lns=findobj(OH,'tag','linestyle=?');
    set(lns,'enable','on','backgroundcolor',Active)
    lnstls=get(lns,'string');
    Ops.linestyle=lnstls{get(lns,'value')};

    set(findobj(OH,'tag','linewidth'),'enable','on')
    lnw=findobj(OH,'tag','linewidth=?');
    set(lnw,'enable','on','backgroundcolor',Active)
    Ops.linewidth=get(lnw,'userdata');
    usesmarker=1;
end
if usesmarker
    set(findobj(OH,'tag','marker'),'enable','on')
    mrk=findobj(OH,'tag','marker=?');
    set(mrk,'enable','on','backgroundcolor',Active)
    mrkrs=get(mrk,'string');
    imrk=get(mrk,'value');
    if forcemarker && ismember('none',mrkrs)
        inone=strmatch('none',mrkrs);
        if imrk==inone
            set(mrk,'value',1)
        end
        mrkrs(inone)=[];
        set(mrk,'string',mrkrs)
    elseif ~forcemarker && ~ismember('none',mrkrs)
        mrkrs{end+1}='none';
        imrk=length(mrkrs); % select no by marker by default
        set(mrk,'string',mrkrs,'value',imrk);
    end
    if imrk>length(mrkrs)
        imrk=1;
        set(mrk,'value',imrk);
    end
    Ops.marker=mrkrs{get(mrk,'value')};
    Ops.markercolour='auto';
    if markerflatfill
        Ops.markerfillcolour='flat';
    else
        Ops.markerfillcolour='none';
    end
    if strcmp(Ops.marker,'none')
        %MultipleColors=0 | edgeflatcolour;
    else
        mc=findobj(OH,'tag','usemarkercolour');
        set(mc,'enable','on')
        if  forcemarkercolor
            set(mc,'style','text')
        else
            set(mc,'style','checkbox')
        end
        if get(mc,'value') || forcemarkercolor
            mc=findobj(OH,'tag','markercolour=?');
            clr=get(mc,'userdata');
            set(mc,'enable','on','backgroundcolor',clr)
            Ops.markercolour=clr;
        end
        if isempty(strmatch(Ops.marker,{'.','*','+','x'},'exact'))
            mc=findobj(OH,'tag','usemarkerfillcolour');
            set(mc,'enable','on')
            if get(mc,'value')
                mc=findobj(OH,'tag','markerfillcolour=?');
                clr=get(mc,'userdata');
                set(mc,'enable','on','backgroundcolor',clr)
                Ops.markerfillcolour=clr;
            end
            if ~strcmp(Ops.markerfillcolour,'flat') && ~strcmp(Ops.markercolour,'auto')
                MultipleColors=0 | edgeflatcolour;
            end
        else
            Ops.markerfillcolour=[0 0 0];
            if ~strcmp(Ops.markercolour,'auto')
                MultipleColors=0 | edgeflatcolour;
            end
        end
    end
end

switch Ops.presentationtype
    case {'vector','patches','patches with lines','markers'};
        if MultipleColors
            cclass=findobj(OH,'tag','colclassify');
            set(cclass,'enable','on')
            if get(cclass,'value')
                Ops.thresholds='get_from_user';
            end
        end
end

Ops.thresholddistribution='linear';
if ~strcmp(Ops.thresholds,'none')
    set(findobj(OH,'tag','thresholds'),'enable','on')
    set(findobj(OH,'tag','thresholds=?'),'enable','on','backgroundcolor',Active)
    Ops.thresholds=get(findobj(OH,'tag','thresholds=?'),'userdata');
    if isempty(Ops.thresholds) || ...
            (isequal(size(Ops.thresholds),[1 1]) && isnumeric(Ops.thresholds) && isequal(Ops.thresholds,round(Ops.thresholds)) && Ops.thresholds>0)
        thrd=findobj(OH,'tag','threshdistr=?');
        set(thrd,'enable','on','backgroundcolor',Active)
        thrdStr=get(thrd,'string'); % linear, logarithmic, anti-logarithmic
        Ops.thresholddistribution=thrdStr{get(thrd,'value')};
    end
end

Ops.colourlimits=[];
Ops.symmetriccolourlimits=0;
Ops.colourbar='none';
Ops.colourmap=[];
if MultipleColors
    set(findobj(OH,'tag','climmode'),'enable','on')
    climmode=findobj(OH,'tag','climmode=?');
    set(climmode,'enable','on','backgroundcolor',Active)
    clmodes=get(climmode,'string');
    CLimMode=clmodes{get(climmode,'value')};
    switch CLimMode,
        case 'automatic'
            Ops.colourlimits=[];
            Ops.symmetriccolourlimits=0;
            if strcmp(Ops.thresholddistribution,'linear')
                climsymm=findobj(OH,'tag','climsymm');
                set(climsymm,'enable','on')
                Ops.symmetriccolourlimits=get(climsymm,'value');
            end
        case 'manual'
            set(findobj(OH,'tag','climmax'),'enable','on')
            set(findobj(OH,'tag','climmax=?'),'enable','on','backgroundcolor',Active)
            set(findobj(OH,'tag','climmin'),'enable','on')
            set(findobj(OH,'tag','climmin=?'),'enable','on','backgroundcolor',Active)
            Min=get(findobj(OH,'tag','climmin=?'),'userdata');
            Max=get(findobj(OH,'tag','climmax=?'),'userdata');
            if Min>Max
                set(findobj(OH,'tag','climmin=?'),'userdata',Max,'string',sprintf('%g',Max));
                set(findobj(OH,'tag','climmax=?'),'userdata',Min,'string',sprintf('%g',Min));
                Ops.colourlimits=[Max Min];
            elseif Min==Max
                if Min==0
                    Max=1;
                elseif Min<0
                    Max=Min*(1-1e-6);
                else
                    Max=Max*(1+1e-6);
                end
                set(findobj(OH,'tag','climmax=?'),'userdata',Max,'string',sprintf('%g',Max));
                Ops.colourlimits=[Min Max];
            else
                Ops.colourlimits=[Min Max];
            end
    end
    set(findobj(OH,'tag','colourmap'),'enable','on')
    set(findobj(OH,'tag','colourmapbutton'),'enable','on')
    cmap=findobj(OH,'tag','colourmap=?');
    set(cmap,'enable','on','backgroundcolor',Active)
    cmaps=get(cmap,'string');
    Ops.colourmap=cmaps{get(cmap,'value')};
    cbar=findobj(OH,'tag','colourbar');
    set(cbar,'enable','on')
    if get(cbar,'value')
        cbarh=findobj(OH,'tag','colbarhorz');
        set(cbarh,'enable','on')
        if get(cbarh,'value')
            Ops.colourbar='horiz';
        else
            Ops.colourbar='vert';
        end
    else
        Ops.colourbar='none';
    end
end

%
%---- axes type
%

levelvar=0;
if ~isempty(strfind(axestype,'Val'))
    if ~isempty(strfind(Props.Name,'level'))
        levelvar=1;
        % only convert to elevation if unit is equivalent to m or
        % dimensionless
        if ~ischar(qp_unitconversion(Units,'m')) || ...
                ~ischar(qp_unitconversion(Units,''))
            axestype=strrep(axestype,'Val','Z');
        end
    end
end
Ops.basicaxestype=axestype;

if levelvar
    axestype=strrep(axestype,'Z',['Z [',actualunits,']']);
elseif ~isempty(strfind(axestype,'Val'))
    axestype=strrep(axestype,'Val',['Val [',actualunits,']']);
end
Ops.axestype=axestype;

%
%---- clipping values
%

Ops.clippingvalues=[];
if (nval==1 || ~isempty(Ops.vectorcolour) || Ops.colourdams) && (lineproperties || data2d)
    set(findobj(OH,'tag','clippingvals'),'enable','on')
    set(findobj(OH,'tag','clippingvals=?'),'enable','on','backgroundcolor',Active)
    Ops.clippingvalues=get(findobj(OH,'tag','clippingvals=?'),'userdata');
end

if (Ops.spatialh==2)
    set(findobj(OH,'tag','clippingvals'),'enable','on')
    set(findobj(OH,'tag','xclipping'),'enable','on')
    set(findobj(OH,'tag','xclipping=?'),'enable','on','backgroundcolor',Active)
    Ops.xclipping=get(findobj(OH,'tag','xclipping=?'),'userdata');
    set(findobj(OH,'tag','yclipping'),'enable','on')
    set(findobj(OH,'tag','yclipping=?'),'enable','on','backgroundcolor',Active)
    Ops.yclipping=get(findobj(OH,'tag','yclipping=?'),'userdata');
end

%---- Export option

ExpTypes={};
if (Ops.spatial<=1) && nval>0
    ExpTypes{end+1}='csv file (time series)';
    ExpTypes{end+1}='Tekal file (time series)';
end
if (allm && alln) && ~multiple(K_) && ~multiple(T_)
    ExpTypes{end+1}='grid file';
    ExpTypes{end+1}='grid file (old format)';
end
if (((allm || ~multiple(M_)) && ~multiple(N_)) || ...
        ((alln || ~multiple(N_)) && ~multiple(M_))) && ...
        ~multiple(K_) && ~multiple(T_) && nval==0
    ExpTypes{end+1}='spline';
end
if (allm && alln) && ~multiple(K_) && ~multiple(T_)
    if nval>0
        ExpTypes{end+1}='QuickIn file';
    end
    if nval==1
        ExpTypes{end+1}='-QuickIn file';
    end
    if nval==1
        ExpTypes{end+1}='Delft3D-MOR field file';
        ExpTypes{end+1}='-Delft3D-MOR field file';
    end
    if nval==1
        ExpTypes{end+1}='SIMONA box file';
        ExpTypes{end+1}='-SIMONA box file';
    end
end
if (multiple(M_) && (multiple(N_) || triangles)) && ~multiple(K_) && ~multiple(T_)
    if ~isequal(Ops.presentationtype,'continuous shades')
        ExpTypes{end+1}='ARCview shape';
    end
elseif strcmp(geometry,'POLYL') || strcmp(geometry,'POLYG')
    ExpTypes{end+1}='ARCview shape';
    ExpTypes{end+1}='landboundary file';
end
if ((length(selected{T_})<11 && ~allt) || (maxt<11 && allt)) && nval>0 && (multiple(M_) || multiple(N_) || multiple(K_))
    ExpTypes{end+1}='Tekal file';
    ExpTypes{end+1}='Tecplot file';
end
if (multiple(M_) || multiple(N_) || multiple(K_)) && ~multiple(T_) && nval>0
    ExpTypes{end+1}='sample file';
end
if nval>=0
    Mver = matlabversionnumber;
    ExpTypes{end+1}='mat file (v6)';
    if Mver>=7
        ExpTypes{end+1}='mat file (v7)';
        if Mver>=7.03
            ExpTypes{end+1}='mat file (v7.3/hdf5)';
        end
    end
end
if ~isempty(ExpTypes)
    set(findobj(OH,'tag','exporttype'),'enable','on');
    expt=findobj(OH,'tag','exporttype=?');
    pExpTypes=get(expt,'string');
    pet=get(expt,'value');
    petStr=pExpTypes{pet};
    et=strmatch(petStr,ExpTypes,'exact');
    if isempty(et)
        et=1;
    end
    set(expt,'enable','on','value',1,'string',ExpTypes,'value',et,'backgroundcolor',Active);
    %
    switch ExpTypes{et}
        case {'QuickIn file','-QuickIn file'}
            set(findobj(OH,'tag','expformat'),'enable','on');
            expf=findobj(OH,'tag','expformat=?');
            set(expf,'enable','on');
            Ops.expformat=get(expf,'string');
    end
    %
    set(findobj(OH,'tag','exportdata'),'enable','on');
    Ops.exporttype=ExpTypes{et};
end
%
OH=-1;
%
%-------- END OF PLOT OPTIONS -------------
%
%---- Set quick view or quick animate
%

if Ops.animate
    viewstr='Quick Animate';
else
    viewstr='Quick View';
end
qv=findobj(mfig,'tag','quickview');
set(qv,'string',viewstr);
if nval==-1
    set(qv,'enable','on')
    set(findobj(mfig,'tag','loaddata'),'enable','off')
elseif strcmp(axestype,'noplot')
    set(qv,'enable','off')
else
    set(qv,'enable','on')
end

%---- Ops Version

Ops.version=1.2;
UD.State=Ops;

%---- Show/hide options

update_option_positions(UD)
