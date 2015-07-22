function [hNew,Thresholds,Param]=qp_plot_default(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_DEFAULT Plot function of QuickPlot for structured data sets.

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

FirstFrame=Param.FirstFrame;
PName=Param.PName;
TStr=Param.TStr;
Selected=Param.Selected;
multiple=Param.multiple;
NVal=Param.NVal;
quivopt=Param.quivopt;
stats=Param.stats;
stn=Param.stn;
s=Param.s;
compat7=Param.compat7;

DimFlag=Props.DimFlag;
Thresholds=Ops.Thresholds;
spatial=Ops.spatial;
spatialh=Ops.spatialh;

%data = qp_dimsqueeze(data,Ops.axestype,multiple,DimFlag,Props);
switch NVal

    case {0,0.5}

        if spatial==2 && spatialh==2

            if isfield(data,'TRI')
                if FirstFrame
                    hNew=patch('vertices',squeeze(data.XYZ(1,:,1,:)),'faces',data.TRI, ...
                        'facecolor','none','edgecolor',Ops.colour, ...
                        'linewidth',Ops.linewidth, ...
                        'linestyle',Ops.linestyle, ...
                        'marker',Ops.marker, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour, ...
                        'parent',Parent);
                else
                    set(hNew,'vertices',data.XYZ,'faces',data.TRI);
                end
            elseif isfield(data,'XDam')
                if ~FirstFrame
                    delete(hNew)
                end
                if isfield(data,'XDamVal') && Ops.colourdams
                    hNew=thindam(data.X,data.Y,data.XDam,data.YDam,'color',data.XDamVal,data.YDamVal,'parent',Parent);
                    set(hNew,'linewidth',Ops.linewidth, ...
                        'linestyle',Ops.linestyle, ...
                        'marker',Ops.marker, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour);
                else
                    hNew=thindam(data.X,data.Y,data.XDam,data.YDam,'parent',Parent);
                    set(hNew,Ops.LineParams{:});
                end
            elseif sum(size(data.X)>1)==2
                if FirstFrame
                    hNew=surface(data.X,data.Y,zeros(size(data.X)), ...
                        'cdata',[], ...
                        'parent',Parent, ...
                        'edgecolor',Ops.colour, ...
                        'linewidth',Ops.linewidth, ...
                        'linestyle',Ops.linestyle, ...
                        'marker',Ops.marker, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour, ...
                        'facecolor','none');
                else
                    set(hNew,'xdata',data.X,'ydata',data.Y);
                end
            else
                if FirstFrame
                    hNew=line(data.X,data.Y, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                else
                    set(hNew,'xdata',data.X,'ydata',data.Y);
                end
            end
            set(get(Parent,'title'),'string',{PName,TStr})
        elseif spatialh==1 && spatial==1
            if FirstFrame
                hNew=line(data.X,zeros(size(data.X)), ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
            else
                set(hNew,'xdata',data.X);
            end
            set(get(Parent,'title'),'string',{PName,TStr})
        elseif spatial==2 && spatialh==1
            switch Ops.plotcoordinate
                case {'path distance','reverse path distance'}
                    xx=data.X(:,:,1);
                    if isfield(data,'Y')
                        yy=data.Y(:,:,1);
                    else
                        yy=0*xx;
                    end
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        xx=flipud(fliplr(xx));
                        yy=flipud(fliplr(yy));
                    end
                    if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                        x=pathdistance(xx,yy,'geographic');
                    else
                        x=pathdistance(xx,yy);
                    end
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        x=flipud(fliplr(x));
                    end
                    x = squeeze(reshape(repmat(x,[1 1 size(data.X,3)]),size(data.X)));
                    y = squeeze(data.Z);
                    z = zeros(size(y));
                case 'x coordinate'
                    x = squeeze(data.X);
                    y = squeeze(data.Z);
                    z = zeros(size(y));
                case 'y coordinate'
                    x = squeeze(data.Y);
                    y = squeeze(data.Z);
                    z = zeros(size(y));
                case '(x,y)'
                    x = squeeze(data.X);
                    y = squeeze(data.Y);
                    z = squeeze(data.Z);
            end
            if FirstFrame
                hNew=surface(x,y,z, ...
                    'cdata',[], ...
                    'parent',Parent, ...
                    'edgecolor',Ops.colour, ...
                    'linewidth',Ops.linewidth, ...
                    'linestyle',Ops.linestyle, ...
                    'marker',Ops.marker, ...
                    'markeredgecolor',Ops.markercolour, ...
                    'markerfacecolor',Ops.markerfillcolour, ...
                    'facecolor','none');
            else
                set(hNew,'xdata',x,'ydata',y,'zdata',z)
            end
            set(get(Parent,'title'),'string',{PName,TStr})
        elseif spatial==1 && spatialh==0
            z=squeeze(data.Z);
            if FirstFrame
                hNew=line(zeros(size(z)),z, ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
            else
                set(hNew,'ydata',z);
            end
            set(get(Parent,'title'),'string',{PName,TStr})
        elseif spatial==0
            if FirstFrame
                hNew=line(data.X,data.Y, ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
                if isfield(data,'Z')
                    if length(hNew)==1
                        set(hNew,'zdata',data.Z);
                    else
                        for i=1:size(data.Z,2)
                            set(hNew(i),'zdata',data.Z(:,i));
                        end
                    end
                    set(get(Parent,'zlabel'),'string','elevation (m) \rightarrow')
                end
            else
                if isfield(data,'Z')
                    set(hNew,'xdata',data.X,'ydata',data.Y,'zdata',data.Z);
                else
                    set(hNew,'xdata',data.X,'ydata',data.Y);
                end
            end
            if ~isempty(stn)
                Str={PName,stn};
            else
                Str={PName};
            end
            set(get(Parent,'title'),'string',Str)
        else
            hNew=gentext(hNew,Ops,Parent,'Plot not defined');
        end

    case {1,5}

        if spatialh==2 && spatial==2
            if isfield(data,'TRI')
                set(Parent,'NextPlot','add');
                switch Ops.presentationtype
                    case {'values','markers'}
                        if isfield(data,'Z') && 0
                            hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,data.Z,data.Val,Ops);
                        else
                            hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,[],data.Val,Ops);
                        end
                    otherwise
                        hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'TRI',data.TRI,data.XYZ,data.Val,Ops);
                end                        
                set(get(Parent,'title'),'string',{PName,TStr})
            else
                data = qp_dimsqueeze(data,Ops.axestype,multiple,DimFlag,Props);
                if isfield(data,'Z') && 0
                    hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,data.Z,data.Val,Ops);
                else
                    hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,[],data.Val,Ops);
                end
                if isempty(Selected{K_})
                    str=PName;
                else
                    str=sprintf('%s in layer %i',PName,Selected{K_});
                end
                set(get(Parent,'title'),'string',{str,TStr})
                tit = {str};
                if ~isempty(stn)
                    tit{end+1}=stn;
                end
                if ~isempty(TStr)
                    tit{end+1}=TStr;
                end
                if length(tit)>2
                    tit{1}=[tit{1} ' at ' tit{2}];
                    tit(2)=[];
                end
                set(get(Parent,'title'),'string',tit)
            end

        elseif spatialh==1 && spatial==1
            %Ops.plotcoordinate='(x,y)';
            switch Ops.plotcoordinate
                case {'path distance','reverse path distance'}
                    %data.X(isnan(data.Val))=NaN;
                    xx=data.X;
                    if isfield(data,'Y')
                        %data.Y(isnan(data.Val))=NaN;
                        yy=data.Y;
                    else
                        yy=0*xx;
                    end
                    if isfield(data,'Z')
                        %data.Z(isnan(data.Val))=NaN;
                        zz=data.Z;
                    else
                        zz=0*xx;
                    end
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        xx=flipud(fliplr(xx));
                        yy=flipud(fliplr(yy));
                        zz=flipud(fliplr(zz));
                    end
                    if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                        x=pathdistance(xx,yy,'geographic');
                    else
                        x=pathdistance(xx,yy);
                    end
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        x=flipud(fliplr(x));
                    end
                    y=data.Val;
                    z=zeros(size(x));
                case 'x coordinate'
                    data.X(isnan(data.Val))=NaN;
                    x=data.X;
                    y=data.Val;
                    z=zeros(size(x));
                case 'y coordinate'
                    data.Y(isnan(data.Val))=NaN;
                    x=data.Y;
                    y=data.Val;
                    z=zeros(size(x));
                case '(x,y)'
                    data.X(isnan(data.Val))=NaN;
                    data.Y(isnan(data.Val))=NaN;
                    x=data.X;
                    y=data.Y;
                    z=data.Val;
            end
            if length(data.Time)>1
                if strcmp(Ops.axestype,'X-Time')
                    c1 = x;
                    c2 = data.Time;
                    v = squeeze(data.Val);
                else
                    c1 = data.Time;
                    c2 = x;
                    v = squeeze(data.Val)';
                end
                if FirstFrame
                    hNew=surface(c1,c2,v,v, ...
                        'parent',Parent, ...
                        'facecolor','interp', ...
                        'edgecolor','none');
                    set(Parent,'layer','top')
                else
                    set(hNew,'xdata',c1, ...
                        'ydata',c2, ...
                        'cdata',v);
                end
            elseif strcmp(Ops.facecolour,'none')
                if FirstFrame
                    hNew=line(x,y,z, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                    set(Parent,'layer','top')
                elseif ishandle(hNew)
                    set(hNew,'xdata',x, ...
                        'ydata',y, ...
                        'zdata',z);
                else
                    return
                end
            else
                if ~FirstFrame
                    delete(hNew)
                end
                vNaN=isnan(y);
                if any(vNaN)
                    bs=findseries(~vNaN);
                else
                    bs=[1 length(vNaN)];
                end
                for i=1:size(bs,1)
                    if x(bs(i,1))==x(bs(i,2)) & ...
                            y(bs(i,1))==y(bs(i,2))
                        hNew(i)=patch(x(bs(i,1):bs(i,2)), ...
                            y(bs(i,1):bs(i,2)), ...
                            1, ...
                            'edgecolor',Ops.colour, ...
                            'facecolor',Ops.facecolour, ...
                            'linestyle',Ops.linestyle, ...
                            'linewidth',Ops.linewidth, ...
                            'marker',Ops.marker, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'parent',Parent);
                    else
                        hNew(i)=line(x(bs(i,1):bs(i,2)), ...
                            y(bs(i,1):bs(i,2)), ...
                            'parent',Parent, ...
                            Ops.LineParams{:});
                    end
                end
                set(Parent,'layer','top')
            end
            tit = {};
            if ~isempty(stn)
                tit{end+1}=stn;
            end
            if ~isempty(TStr)
                tit{end+1}=TStr;
            end
            set(get(Parent,'title'),'string',tit)

        elseif spatialh==1 && spatial==2

            data = qp_dimsqueeze(data,Ops.axestype,multiple,DimFlag,Props);
            Mask=repmat(min(data.Z,[],3)==max(data.Z,[],3),[1 1 size(data.Z,3)]);
            if isequal(size(Mask),size(data.X))
                data.X(Mask)=NaN;
            end
            switch Ops.plotcoordinate
                case {'path distance','reverse path distance'}
                    x=data.X(:,:,1);
                    if isfield(data,'Y')
                        y=data.Y(:,:,1);
                    else
                        y=0*x;
                    end
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        x=flipud(fliplr(x));
                        y=flipud(fliplr(y));
                    end
                    if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                        s=pathdistance(x,y,'geographic');
                    else
                        s=pathdistance(x,y);
                    end
                    if ~isequal(size(data.X),size(data.Val))
                        ds=s(min(find(s>0)))/2;
                        if ~isempty(ds)
                            s=s-ds;
                        end
                    end
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        s=flipud(fliplr(s));
                    end
                    s=reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
                case 'x coordinate'
                    s=data.X;
                case 'y coordinate'
                    s=data.Y;
            end
            s=squeeze(s);
            data.X=squeeze(data.X);
            if isfield(data,'Y')
                data.Y=squeeze(data.Y);
            end
            data.Z=squeeze(data.Z);
            data.Val=squeeze(data.Val);
            %
            set(Parent,'NextPlot','add');
            switch Ops.presentationtype
                case {'patches','patches with lines'}
                    if isfield(Props,'ThreeD')
                        hNew=genfaces(hNew,Ops,Parent,data.Val,data.X,data.Y,data.Z);
                    else
                        hNew=genfaces(hNew,Ops,Parent,data.Val,s,data.Z);
                    end

                case 'values'
                    I=~isnan(data.Val);
                    hNew=gentextfld(hNew,Ops,Parent,data.Val(I),s(I),data.Z(I));

                case 'continuous shades'
                    hNew=gensurface(hNew,Ops,Parent,data.Val,s,data.Z,data.Val);

                case 'markers'
                    hNew=genmarkers(hNew,Ops,Parent,data.Val,s,data.Z);
                    
                case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
                    if isequal(size(s),size(data.Val)+1)
                        [s,data.Z,data.Val]=face2surf(s,data.Z,data.Val);
                    end
                    data.Val(isnan(s) | isnan(data.Z))=NaN;
                    ms=max(s(:));
                    mz=max(data.Z(:));
                    s(isnan(s))=ms;
                    data.Z(isnan(data.Z))=mz;
                    hNew=gencontour(hNew,Ops,Parent,s,data.Z,data.Val,Thresholds,Param);

            end
            if FirstFrame
                set(Parent,'view',[0 90],'layer','top');
                set(get(Parent,'ylabel'),'string','elevation (m) \rightarrow')
            end
            set(get(Parent,'title'),'string',{PName,TStr})

        elseif spatialh==0 && spatial==1

            if length(data.Time)>1 % Time-Z
                c2 = squeeze(data.Z);
                c1 = repmat(data.Time,1,size(c2,2));
                v = squeeze(data.Val);
                if FirstFrame
                    hNew=surface(c1,c2,v,v, ...
                        'parent',Parent, ...
                        'facecolor','interp', ...
                        'edgecolor','none');
                    set(Parent,'layer','top')
                else
                    set(hNew,'xdata',c1, ...
                        'ydata',c2, ...
                        'cdata',v);
                end
            elseif FirstFrame
                hNew=line(squeeze(data.Val),squeeze(data.Z), ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
            elseif ishandle(hNew)
                set(hNew,'xdata',squeeze(data.Val), ...
                    'ydata',squeeze(data.Z));
            else
                return
            end
            if ~isempty(stn)
                Str={stn,TStr};
            else
                Str={TStr};
            end
            set(get(Parent,'title'),'string',Str);

        elseif multiple(T_) && spatial==0
            if FirstFrame
                hNew=line(data.Time,data.Val, ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
                if Props.DimFlag(T_)~=5
                    tick(Parent,'x','autodate')
                end
            else
                set(hNew,'xdata',data.Time,'ydata',data.Val);
            end
            if ~isempty(stn)
                Str=stn;
            else
                Str='';
            end
            set(get(Parent,'title'),'string',Str)

        else
            strval=sprintf(Ops.numformat,data.Val);
            if isfield(Ops,'axestype') && ...
                    (isequal(strtok(Ops.axestype),'Time-Val') || ...
                    isequal(strtok(Ops.axestype),'Time-Z'))
                ylim = get(Parent,'ylim');
                yval = min(ylim(2),max(ylim(1),data.Val));
                if isempty(hNew)
                    hNew(2)=line(data.Time*[1 1],ylim,'parent',Parent,'color',Ops.colour);
                    hNew(1)=text('position',[data.Time yval 0],'string',strval,'parent',Parent,Ops.FontParams{:});
                else
                    i1 = strmatch('text',get(hNew,'type')); % 1 or 2
                    i2 = 3-i1; % consequently, 2 or 1
                    set(hNew(i2),'xdata',data.Time*[1 1],'ydata',ylim);
                    set(hNew(i1),'position',[data.Time yval 0],'string',strval);
                end
            else
                unit = '';
                if ~isempty(Ops.units)
                    unit = [' ' Ops.units];
                end
                hNew=gentext(hNew,Ops,Parent,['Val = ',strval,unit]);
            end
        end

    case {2,3}

        if multiple(T_) && (spatial==1 || spatial==0)

            if ~isempty(hNew)
                delete(hNew)
            end

            ax=subplot(1,3,1);
            qp_defaultaxessettings(ax)
            hold on
            if isfield(data,'YComp')
                Y=data.YComp;
            else
                Y=data.ZComp;
            end
            hNew(1)=plot(data.XComp,Y);
            xlabel([PName ' comp.1 \rightarrow'])
            ylabel([PName ' comp.2 \rightarrow'])
            set(ax,'da',[1 1 1],'layer','top')

            ax=subplot(2,3,2:3);
            qp_defaultaxessettings(ax)
            hold on
            hNew(2)=plot(data.Time,data.XComp);
            setaxesprops(ax,Ops.axestype,PName)
            ylabel([PName ' comp.1 \rightarrow'])
            if ~isempty(stn)
                set(get(ax,'title'),'string',stn)
            end

            ax=subplot(2,3,5:6);
            qp_defaultaxessettings(ax)
            hold on
            hNew(3)=plot(data.Time,Y);
            setaxesprops(ax,Ops.axestype,PName)
            ylabel([PName ' comp.2 \rightarrow'])
            set(hNew,Ops.LineParams{:});

        elseif spatialh==1 && spatial==2
            x=min(data.X,[],3);
            if isfield(data,'Y')
                y=min(data.Y,[],3);
            else
                y=0*x;
            end
            %
            xsign=0;
            switch Ops.plotcoordinate
                case {'path distance','reverse path distance'}
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        x=flipud(fliplr(x));
                        y=flipud(fliplr(y));
                        xsign=-1;
                    else
                        xsign=1;
                    end
                    if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                        s=pathdistance(x,y,'geographic');
                    else
                        s=pathdistance(x,y);
                    end
                    if strcmp(Ops.plotcoordinate,'reverse path distance')
                        s=flipud(fliplr(s));
                    end
                    s=reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
                case 'x coordinate'
                    s=data.X;
                case 'y coordinate'
                    s=data.Y;
            end
            %
            if isequal(size(s),size(data.XComp))
                %
                % data provided at (X,Y,Z) locations
                %
                % (1) determine sign and size correction for horizontal
                % component in case of x/y coordinate projection
                %
                if xsign==0
                    if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                        t=pathdistance(x,y,'geographic');
                    else
                        t=pathdistance(x,y);
                    end
                    dt1 = t-t([1 1:end-1]);
                    dt1(dt1==0)=NaN;
                    dx1 = (x-x([1 1:end-1]))./dt1;
                    dy1 = (y-y([1 1:end-1]))./dt1;
                    dt2 = t([2:end end])-t;
                    dt2(dt2==0)=NaN;
                    dx2 = (x([2:end end])-x)./dt2;
                    dy2 = (y([2:end end])-y)./dt2;
                    nan1 = isnan(dx1) | isnan(dy1);
                    nan2 = isnan(dx2) | isnan(dy2);
                    dx1(nan1) = 0;
                    dy1(nan1) = 0;
                    dx2(nan2) = 0;
                    dy2(nan2) = 0;
                    dx = (dx1+dx2)./max(~nan1+~nan2,1);
                    dy = (dy1+dy2)./max(~nan1+~nan2,1);
                    dx(nan1+nan2==2)=NaN;
                    dy(nan1+nan2==2)=NaN;
                    switch Ops.plotcoordinate
                        case 'x coordinate'
                            xsign = dx./sqrt(dx.^2+dy.^2);
                        case 'y coordinate'
                            xsign = dy./sqrt(dx.^2+dy.^2);
                    end
                    xsign = reshape(repmat(xsign,[1 1 size(data.XComp,3)]),size(data.XComp));
                end
                %
                % (2) set horizontal coordinate of vector points
                %
                % s has already been set correctly
                %
                % (3) set vertical coordinate of vector points
                %
                Zvector=data.Z;
                %
            else
                %
                % data provided in cell centres
                %
                % (1) determine sign and size correction for horizontal
                % component in case of x/y coordinate projection
                %
                if xsign==0
                    xsign = diff(s(:,:,1))./sqrt(diff(x).^2+diff(y).^2);
                    xsign = reshape(repmat(xsign,[1 1 size(data.XComp,3)]),size(data.XComp));
                end
                %
                % (2) determine horizontal coordinate of cell centres
                %
                s=squeeze(s);
                s=(s(1:end-1,1:end-1)+s(2:end,1:end-1)+s(1:end-1,2:end)+s(2:end,2:end))/4;
                s=reshape(s,size(data.XComp));
                %
                % (3) determine vertical coordinate of cell centres
                %
                Zvector=squeeze(data.Z);
                data.Z=[];
                Zvector=(Zvector(:,1:end-1)+Zvector(:,2:end))/2;
                Zvector=reshape(Zvector,size(data.XComp));
            end

            %
            % get right component to plot: select the component in the plane
            % to be plotted.
            %
            if multiple(M_)
                planecomp=data.XComp;
            else % multiple(N_)
                planecomp=data.YComp;
            end
            planecomp=xsign.*planecomp;
            planecomp((planecomp==0) & (data.ZComp==0))=NaN;

            hold on
            delete(hNew);
            if any(~isnan(data.XComp(:)))

                switch Ops.verticalscalingmode
                    case 'manual'
                        ScaleFacZ=Ops.verticalscalefactor;
                        set(gca,'dataaspectratio',[1 1/ScaleFacZ 1]);
                    case 'automatic'
                        if FirstFrame
                            c1=max(max(Zvector(:))-min(Zvector(:)),1e-6);
                            c2=max(s(:))-min(s(:));
                            ScaleFacZ=c2/c1/10;
                            set(gca,'dataaspectratio',[1 1/ScaleFacZ 1]);
                        else
                            da=get(gca,'dataaspectratio');
                            ScaleFacZ=da(1)/da(2);
                        end
                    otherwise % unrestricted, same as automatic per timestep without actually setting dataaspectratio

                        c1=max(max(Zvector(:))-min(Zvector(:)),1e-6);
                        c2=max(s(:))-min(s(:));
                        ScaleFacZ=c2/c1/10;
                        if ScaleFacZ==0
                            ScaleFacZ = 1;
                        end
                end
                if ScaleFacZ==1
                    hNew=qp_vector(Ops.vectorstyle,s,Zvector,[],planecomp,data.ZComp,[],quivopt{:});
                else

                    %       ----------
                    %        When the following lines are used, the lengths and the directions of
                    %        the vectors can be compared. The standard implementation allows for
                    %        the comparison of the individual components and the direction. The
                    %        standard implementation is furthermore consistent with the inter-
                    %        pretation of the vector as a particle displacement in a given period.
                    %
                    %        mag1=sqrt(planecomp.^2+data.ZComp.^2);
                    %        mag2=sqrt(planecomp.^2+(ScaleFacZ*data.ZComp).^2); mag2(mag2==0)=1;
                    %        mfac=mag1./mag2;
                    %        hNew=qp_vector(Ops.vectorstyle,s,ScaleFacZ*Zvector,[],mfac.*planecomp,ScaleFacZ*mfac.*data.ZComp,[],quivopt{:});
                    %       ----------
                    hNew=qp_vector(Ops.vectorstyle,s,ScaleFacZ*Zvector,[],planecomp,ScaleFacZ*data.ZComp,[],quivopt{:});
                    for i=1:length(hNew)
                        set(hNew(i),'ydata',get(hNew(i),'ydata')/ScaleFacZ)
                    end

                end
                if ~isempty(Ops.vectorcolour)
                    hNew=colquiver(hNew,data.Val);
                else

                    set(hNew,'color',Ops.colour)

                end

            else
                hNew=line(1,1,'xdata',[],'ydata',[]);
            end
            set(gca,'layer','top')
            ylabel('elevation (m) \rightarrow')
            str=PName;
            set(get(gca,'title'),'string',{str,TStr})

        elseif spatial>=1 && spatialh>=1 && isfield(data,'YComp')
            data.XComp((data.XComp==0) & (data.YComp==0))=NaN;
            I=~isnan(data.XComp(:));
            %
            minx=min(data.X(:));
            maxx=max(data.X(:));
            miny=min(data.Y(:));
            maxy=max(data.Y(:));
            %
            hold on
            delete(hNew);
            if any(I)
                %
                data.X=data.X(I);
                data.Y=data.Y(I);
                data.XComp=data.XComp(I);
                data.YComp=data.YComp(I);
                if isfield(data,'Z')
                    data.Z=data.Z(I);
                end
                if isfield(data,'ZComp')
                    data.ZComp=data.ZComp(I);
                end
                if isfield(data,'Val')
                    data.Val=data.Val(I);
                end
                %
                if isfield(data,'ZComp')
                    hNew=qp_vector(Ops.vectorstyle,data.X,data.Y,data.Z,data.XComp,data.YComp,data.ZComp,quivopt{:});
                else
                    hNew=qp_vector(Ops.vectorstyle,data.X,data.Y,[],data.XComp,data.YComp,[],quivopt{:});
                end

                if ~isempty(Ops.vectorcolour)
                    if ~strcmp(Ops.thresholds,'none')
                        vc = zeros(size(data.Val));
                        for i=1:length(Thresholds)
                            vc(data.Val>=Thresholds(i))=i;
                        end
                        data.Val=vc;
                        set(Parent,'clim',[1 length(Thresholds)]);
                    end
                    hNew=colquiver(hNew,data.Val);
                else
                    set(hNew,'color',Ops.colour)
                end

                hNew(end+1)=line([minx maxx],[miny maxy],'linestyle','none','marker','none');
            else
                hNew=line(1,1,'xdata',[],'ydata',[]);
            end
            if isempty(Selected{K_})
                str=PName;
            else
                str=sprintf('%s in layer %i',PName,Selected{K_});
            end
            title({str,TStr})

        elseif spatialh==0 && spatial==1

            if ~isempty(hNew)
                delete(hNew)
            end

            ax=subplot(1,2,1);
            qp_defaultaxessettings(ax)
            hold on
            hNew(1)=plot(squeeze(data.XComp),squeeze(data.Z));
            xlabel([PName ' comp.1 \rightarrow'])
            ylabel('elevation (m) \rightarrow')
            set(ax,'layer','top')
            if ~isempty(stn)
                set(get(ax,'title'),'string',stn)
            end

            ax=subplot(1,2,2);
            qp_defaultaxessettings(ax)
            hold on
            if isfield(data,'YComp')
                hNew(2)=plot(squeeze(data.YComp),squeeze(data.Z));
            else
                hNew(2)=plot(squeeze(data.ZComp),squeeze(data.Z));
            end
            xlabel([PName ' comp.2 \rightarrow'])
            ylabel('elevation (m) \rightarrow')
            set(get(ax,'title'),'string',TStr)
            set(ax,'layer','top')
            set(hNew,Ops.LineParams{:});

        else

            if isfield(data,'XComp')
                strxcomp = sprintf(Ops.numformat,data.XComp);
            end
            if isfield(data,'YComp')
                strycomp = sprintf(Ops.numformat,data.YComp);
            end
            if isfield(data,'ZComp')
                strzcomp = sprintf(Ops.numformat,data.ZComp);
            end
            if NVal==2
                if isfield(data,'YComp')
                    strval=['[' strxcomp ' ' strycomp ']'];
                else
                    strval=['[' strxcomp ' ' strzcomp ']'];
                end
            else
                strval=['[' strxcomp ' ' strycomp ' ' strzcomp ']'];
            end
            if isfield(Ops,'axestype') && isequal(strtok(Ops.axestype),'Time-Val')
                ylim = get(Parent,'ylim');
                yval = min(ylim(2),max(ylim(1),inf)); % XComp, YComp, ZComp, Magnitude?
                if isempty(hNew)
                    hNew=line(data.Time*[1 1],ylim,'parent',Parent,'color',Ops.colour);
                    hNew(2)=text('position',[data.Time yval 0],'string',strval,'parent',Parent,Ops.FontParams{:});
                else
                    set(hNew(1),'xdata',data.Time*[1 1],'ydata',ylim);
                    set(hNew(2),'position',[data.Time yval 0],'string',strval);
                end
            else
                hNew=gentext(hNew,Ops,Parent,['Val=',strval]);
            end

        end
    case {4}
        switch Ops.presentationtype
            case {'markers'}
                hNew=genmarkers(hNew,Ops,Parent,[],data.X,data.Y);
            case {'labels'}
                hNew=gentextfld(hNew,Ops,Parent,data.Val,data.X,data.Y);
        end
end
