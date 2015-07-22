function [out,out2]=qp_gridview(cmd,varargin)
%QP_GRIDVIEW Helper routine for Grid View interface.
%   FIG = QP_GRIDVIEW(GRID,RANGE) initialise interface with grid and
%   selected range. The RANGE argument is optional. The GRID may be dropped
%   if the RANGE is not specified. The GRID variable should be structure
%   containing fields
%     * X and Y for a 2D structured grid.
%     * XYZ, TRI for a 2D unstructured grid (XYZ array needs to contain XY
%       data only).
%     * XY, SEG for a network.
%   The function returns a figure handle. The RANGE structure should
%   contain fields Type and Range. Valid range types are 'none', 'point',
%   'range', 'line', 'lineseg', 'pwline', 'genline'. See the 'getrange'
%   call output for the contents of the Range field.
%
%   QP_GRIDVIEW('setgrid',FIG,GRID) update the grid used by the grid
%   in the specified Grid View interface FIG.
%
%   QP_GRIDVIEW('setrange',FIG,RANGE) update the selected range of the
%   specified Grid View interface FIG to the specified RANGE.
%
%   RANGE = QP_GRIDVIEW('getrange',FIG) get the currently selected range of
%   the specified Grid View interface FIG. The RANGE structure
%   contains both selection type and selection indices.
%
%   [RANGETYPE,RANGEINDEX] = ... get the currently selected range as a
%   range type and range index instead of one range structure.
%
%   QP_GRIDVIEW('callback',FIG,F,arg1,arg2,...) set the callback function
%   to F with the specified arguments; this function will be called each
%   time selected range changes. The new RANGE should be enquired by means
%   of the 'getrange' call.

%   Obsolete structured grid syntax:
%
%   F = QP_GRIDVIEW(X,Y) initialise interface for a structured grid with
%   given X and Y coordinates.
%
%   F = QP_GRIDVIEW(X,Y,RANGE) initialise interface with grid and range.
%
%   QP_GRIDVIEW('setgrid',F,X,Y) update grid used.

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
    cmd='initialize';
    UsrRange=[];
    GRID.X=[];
    GRID.Y=[];
elseif nargin==1 && ischar(cmd)
elseif ischar(cmd) && strcmpi(cmd,'getrange')
    if nargin==1
        F=gcbf;
    else
        F=varargin{1};
    end
    G=findobj(F,'tag','GRID');
    GRID=get(G,'userdata');
    xx=GRID.Selected;
    if ~isfield(xx,'Type')
        xx.Type='none';
        xx.Range=[];
    end
    if nargout<=1
        out=xx;
    else
        out=xx.Type;
        out2=xx.Range;
    end
    return
elseif ischar(cmd)
else
    if isstruct(cmd)
        GRID=cmd;
        n=1;
    else
        GRID.X=cmd;
        GRID.Y=varargin{1};
        n=2;
    end
    cmd='initialize';
    if nargin>n
        UsrRange=varargin{n};
    else
        UsrRange=[];
    end
end

switch cmd
    case {'gridrangeup','gridrangemotion'}
        G=findobj(gcbf,'tag','GRID');
        GRID=get(G,'userdata');
        SelectedGrid=findobj(gcbf,'tag','SELSURF');
        SelectedPatch=findobj(gcbf,'tag','SELPATCH');
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        ij0=get(SelectedLine,'userdata');
        [i,j]=trackpnt(gcbf);
        i0=ij0(1);
        j0=ij0(2);
        i1=min(i0,i);
        i2=max(i0,i);
        j1=min(j0,j);
        j2=max(j0,j);
        if (i1~=i2) && (j1~=j2)
            set(SelectedGrid, ...
                'xdata',GRID.X(i1:i2,j1:j2), ...
                'ydata',GRID.Y(i1:i2,j1:j2), ...
                'zdata',zeros(i2-i1+1,j2-j1+1))
            set(SelectedLine,'xdata',[],'ydata',[])
            set(SelectedPatch,'vertices',[],'faces',[])
        elseif (i1==i2) && (j1==j2)
            set(SelectedLine, ...
                'xdata',GRID.X(i0,j0), ...
                'ydata',GRID.Y(i0,j0), ...
                'marker','.')
            set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            set(SelectedPatch,'vertices',[],'faces',[])
        elseif i1==i2
            set(SelectedLine, ...
                'xdata',GRID.X(i0,j1:j2), ...
                'ydata',GRID.Y(i0,j1:j2), ...
                'marker','none','linestyle','-')
            set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            set(SelectedPatch,'vertices',[],'faces',[])
        else
            set(SelectedLine, ...
                'xdata',GRID.X(i1:i2,j0), ...
                'ydata',GRID.Y(i1:i2,j0), ...
                'marker','none','linestyle','-')
            set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            set(SelectedPatch,'vertices',[],'faces',[])
        end
        switch cmd
            case {'gridrangeup'}
                GRID.Selected.Type='range';
                GRID.Selected.Range=[i1 i2 j1 j2];
                set(G,'userdata',GRID)
                normalstate(gcbf)
        end

    case {'draglineup','draglinemotion','dragwholelineup','dragwholelinemotion'}
        G=findobj(gcbf,'tag','GRID');
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        ij0=get(SelectedLine,'userdata');
        GRID=get(G,'userdata');
        pnt=get(get(G,'parent'),'currentpoint');
        pnt=pnt(1,1:2);
        dist=(pnt(1)-GRID.X).^2+(pnt(2)-GRID.Y).^2;
        i0=ij0(1);
        j0=ij0(2);
        midist=min(dist(i0,:));
        mjdist=min(dist(:,j0));
        switch cmd
            case {'draglineup','draglinemotion'}
                if midist<mjdist
                    j1=find(dist(i0,:)==midist);
                    j1=j1(1);
                    i1=i0;
                    if j1==j0
                        set(SelectedLine, ...
                            'xdata',GRID.X(i0,j0), ...
                            'ydata',GRID.Y(i0,j0), ...
                            'marker','.')
                    elseif j1<j0
                        set(SelectedLine, ...
                            'xdata',GRID.X(i0,j1:j0), ...
                            'ydata',GRID.Y(i0,j1:j0), ...
                            'marker','none','linestyle','-')
                    else
                        set(SelectedLine, ...
                            'xdata',GRID.X(i0,j0:j1), ...
                            'ydata',GRID.Y(i0,j0:j1), ...
                            'marker','none','linestyle','-')
                    end
                else
                    i1=find(dist(:,j0)==mjdist);
                    i1=i1(1);
                    j1=j0;
                    if i1==i0
                        set(SelectedLine, ...
                            'xdata',GRID.X(i0,j0), ...
                            'ydata',GRID.Y(i0,j0), ...
                            'marker','.')
                    elseif i1<i0
                        set(SelectedLine, ...
                            'xdata',GRID.X(i1:i0,j0), ...
                            'ydata',GRID.Y(i1:i0,j0), ...
                            'marker','none','linestyle','-')
                    else
                        set(SelectedLine, ...
                            'xdata',GRID.X(i0:i1,j0), ...
                            'ydata',GRID.Y(i0:i1,j0), ...
                            'marker','none','linestyle','-')
                    end
                end
                %---trackcoord start
                trackxy(gcbf,pnt)
                MN=findobj(gcbf,'tag','MNcoord');
                set(MN,'string',sprintf('m,n: %i,%i',i1,j1))
                %---trackcoord stop
            case {'dragwholelineup','dragwholelinemotion'}
                if midist<mjdist
                    set(SelectedLine, ...
                        'xdata',GRID.X(i0,:), ...
                        'ydata',GRID.Y(i0,:), ...
                        'marker','none','linestyle','-')
                else
                    set(SelectedLine, ...
                        'xdata',GRID.X(:,j0), ...
                        'ydata',GRID.Y(:,j0), ...
                        'marker','none','linestyle','-')
                end
                %---trackcoord start
                trackxy(gcbf,pnt)
                MN=findobj(gcbf,'tag','MNcoord');
                set(MN,'string','m,n:')
                %---trackcoord stop
        end
        switch cmd
            case 'draglineup'
                GRID.Selected.Type='lineseg';
                GRID.Selected.Range=[i0 i1 j0 j1];
                set(G,'userdata',GRID)
                normalstate(gcbf)
            case 'dragwholelineup'
                GRID.Selected.Type='line';
                if midist<mjdist
                    GRID.Selected.Range=[i0 inf];
                else
                    GRID.Selected.Range=[inf j0];
                end
                set(G,'userdata',GRID)
                normalstate(gcbf)
        end

    case {'pwlinedown','pwlinemotion'}
        G=findobj(gcbf,'tag','GRID');
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        Ln=get(SelectedLine,'userdata');
        GRID=get(G,'userdata');
        pnt=get(get(G,'parent'),'currentpoint');
        pnt=pnt(1,1:2);
        dist=(pnt(1)-GRID.X).^2+(pnt(2)-GRID.Y).^2;
        if isempty(Ln)
            mdist=min(dist(:));
            [i,j]=find(dist==mdist);
            i=i(1);
            j=j(1);
            set(SelectedLine, ...
                'xdata',GRID.X(i,j), ...
                'ydata',GRID.Y(i,j), ...
                'marker','.')
            switch cmd
                case 'pwlinedown'
                    Ln.ij0=[i j];
                    Ln.RefPnt=Ln.ij0;
                    Ln.XY=zeros(0,2);
                    set(SelectedLine,'userdata',Ln)
            end
            %---trackcoord start
            trackxy(gcbf,pnt)
            MN=findobj(gcbf,'tag','MNcoord');
            set(MN,'string',sprintf('m,n: %i,%i',i,j))
            %---trackcoord stop
        else
            i0=Ln.ij0(1);
            j0=Ln.ij0(2);
            midist=min(dist(i0,:));
            mjdist=min(dist(:,j0));
            [mddist,indrng,i1,j1]=NearDiag(dist,i0,j0);
            if (mddist<midist) && (mddist<mjdist)
                XY=[GRID.X(indrng)' GRID.Y(indrng)'];
            elseif midist<mjdist
                i1=i0;
                j1=find(dist(i0,:)==midist);
                j1=j1(1);
                if j1==j0
                    XY=zeros(0,2);
                elseif j1<j0
                    XY=[GRID.X(i0,j0:-1:j1)' GRID.Y(i0,j0:-1:j1)'];
                else
                    XY=[GRID.X(i0,j0:j1)' GRID.Y(i0,j0:j1)'];
                end
            else
                j1=j0;
                i1=find(dist(:,j0)==mjdist);
                i1=i1(1);
                if i1==i0
                    XY=zeros(0,2);
                elseif i1<i0
                    XY=[GRID.X(i0:-1:i1,j0) GRID.Y(i0:-1:i1,j0)];
                else
                    XY=[GRID.X(i0:i1,j0) GRID.Y(i0:i1,j0)];
                end
            end
            set(SelectedLine, ...
                'xdata',[Ln.XY(:,1);XY(:,1)], ...
                'ydata',[Ln.XY(:,2);XY(:,2)], ...
                'marker','none','linestyle','-')
            switch cmd
                case {'pwlinedown'}
                    Ln.ij0=[i1 j1];
                    Ln.RefPnt(end+1,:)=Ln.ij0;
                    Ln.XY=[Ln.XY;XY];
                    set(SelectedLine,'userdata',Ln);
                    if ~strcmp(get(gcbf,'selectiontype'),'normal')
                        GRID.Selected.Type='pwline';
                        GRID.Selected.Range=Ln.RefPnt;
                        set(G,'userdata',GRID)
                        normalstate(gcbf)
                    end
            end
            %---trackcoord start
            trackxy(gcbf,pnt)
            MN=findobj(gcbf,'tag','MNcoord');
            set(MN,'string',sprintf('m,n: %i,%i',i1,j1))
            %---trackcoord stop
        end

    case {'genlinedown','genrectdown','genlinemotion','genrectmotion', ...
            'genareadown','genareamotion'}
        G=findobj(gcbf,'tag','GRID');
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        SelectedPatch=findobj(gcbf,'tag','SELPATCH');
        Ln=get(SelectedLine,'userdata');
        pnt=get(get(G,'parent'),'currentpoint');
        pnt=pnt(1,1:2);
        %---trackcoord start
        trackxy(gcbf,pnt)
        MN=findobj(gcbf,'tag','MNcoord');
        set(MN,'string','')
        %---trackcoord stop
        if isempty(Ln)
            set(SelectedLine,'xdata',pnt(1),'ydata',pnt(2),'marker','.')
            switch cmd
                case {'genlinedown','genareadown'}
                    Ln.XY=pnt;
                    set(SelectedLine,'userdata',Ln)
                case 'genrectdown'
                    Ln.XY=pnt;
                    set(SelectedLine,'userdata',Ln)
                    set(SelectedPatch,'vertices',pnt,'faces',[1 1 1 1])
            end
        else
            switch cmd
                case {'genlinedown','genlinemotion'}
                    set(SelectedLine,'xdata',[Ln.XY(:,1);pnt(1)], ...
                        'ydata',[Ln.XY(:,2);pnt(2)], ...
                        'marker','none','linestyle','-')
                case {'genareadown','genareamotion'}
                    if length(Ln.XY(:,1))==1
                        set(SelectedLine,'xdata',[Ln.XY(:,1);pnt(1)], ...
                            'ydata',[Ln.XY(:,2);pnt(2)], ...
                            'marker','none','linestyle','-')
                        set(SelectedPatch, ...
                            'vertices',[Ln.XY;pnt], ...
                            'faces',1:size(Ln.XY,1)+1)
                    else
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPatch, ...
                            'vertices',[Ln.XY;pnt], ...
                            'faces',1:size(Ln.XY,1)+1)
                    end
                case {'genrectmotion'}
                    xy=[Ln.XY(1,:)
                        Ln.XY(1,1) pnt(2)
                        pnt(1) Ln.XY(1,2)
                        pnt];
                    set(SelectedLine,'xdata',[],'ydata',[])
                    set(SelectedPatch,'vertices',xy, ...
                        'faces',[1 2 4 3])
            end
            Ln.XY=[Ln.XY;pnt];
            GRID=get(G,'userdata');
            switch cmd
                case {'genlinedown'}
                    set(SelectedLine,'userdata',Ln);
                    if ~strcmp(get(gcbf,'selectiontype'),'normal')
                        GRID.Selected.Type='genline';
                        GRID.Selected.Range=Ln.XY;
                        set(G,'userdata',GRID)
                        normalstate(gcbf)
                    end
                case {'genareadown'}
                    set(SelectedLine,'userdata',Ln);
                    if ~strcmp(get(gcbf,'selectiontype'),'normal')
                        GRID.Selected.Type='range';
                        switch GRID.Type
                            case 'network'
                                idx = inpolygon(GRID.XY(:,1),GRID.XY(:,2),Ln.XY(:,1),Ln.XY(:,2));
                            case 'triangular'
                                idx = inpolygon(GRID.XYZ(:,1),GRID.XYZ(:,2),Ln.XY(:,1),Ln.XY(:,2));
                        end
                        GRID.Selected.Range={find(idx)'};
                        set(G,'userdata',GRID)
                        normalstate(gcbf)
                        qp_gridview('setrange',gcbf,GRID.Selected)
                    end
                case {'genrectdown'}
                    GRID.Selected.Type='range';
                    Ln.XY=sort(Ln.XY,1);
                    switch GRID.Type
                        case 'network'
                            idx = GRID.XY(:,1)>=Ln.XY(1,1) & ...
                                GRID.XY(:,1)<=Ln.XY(2,1) & ...
                                GRID.XY(:,2)>=Ln.XY(1,2) & ...
                                GRID.XY(:,2)<=Ln.XY(2,2);
                        case 'triangular'
                            idx = GRID.XYZ(:,1)>=Ln.XY(1,1) & ...
                                GRID.XYZ(:,1)<=Ln.XY(2,1) & ...
                                GRID.XYZ(:,2)>=Ln.XY(1,2) & ...
                                GRID.XYZ(:,2)<=Ln.XY(2,2);
                    end
                    GRID.Selected.Range={find(idx)'};
                    set(G,'userdata',GRID)
                    normalstate(gcbf)
                    qp_gridview('setrange',gcbf,GRID.Selected)
            end
        end

    case 'trackcoord'
        G=findobj(gcbf,'tag','GRID');
        pnt=get(get(G,'parent'),'currentpoint');
        if ~isempty(pnt)
            pnt=pnt(1,1:2);
            trackxy(gcbf,pnt)
        end
        MN=findobj(gcbf,'tag','MNcoord');
        set(MN,'string','')

    case {'selpointup','selpointmotion','draglinedown','dragwholelinedown','gridrangedown'}
        G=findobj(gcbf,'tag','GRID');
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        GRID=get(G,'userdata');
        [i,j]=trackpnt(gcbf);
        switch GRID.Type
            case 'structured'
                set(SelectedLine, ...
                    'xdata',GRID.X(i,j), ...
                    'ydata',GRID.Y(i,j), ...
                    'marker','.')
            case 'network'
                set(SelectedLine, ...
                    'xdata',GRID.XY(i,1), ...
                    'ydata',GRID.XY(i,2), ...
                    'marker','.')
            case 'triangular'
                set(SelectedLine, ...
                    'xdata',GRID.XYZ(i,1), ...
                    'ydata',GRID.XYZ(i,2), ...
                    'marker','.')
        end
        switch cmd
            case 'selpointup'
                X.Type='point';
                switch GRID.Type
                    case 'structured'
                        X.Range=[i j];
                    case {'network','triangular'}
                        X.Range=i;
                end
                GRID.Selected=X;
                set(G,'userdata',GRID)
                normalstate(gcbf)
                %            qp_gridview('setrange',gcbf,GRID.Selected)
            case 'draglinedown'
                set(gcbf,'WindowButtonDownFcn','qp_gridview draglineup')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview draglinemotion')
                set(gcbf,'WindowButtonUpFcn','')
                set(SelectedLine,'userdata',[i j])
            case 'dragwholelinedown'
                set(gcbf,'WindowButtonDownFcn','qp_gridview dragwholelineup')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview dragwholelinemotion')
                set(gcbf,'WindowButtonupFcn','')
                set(SelectedLine,'userdata',[i j])
            case 'gridrangedown'
                set(gcbf,'WindowButtonDownFcn','qp_gridview gridrangeup')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview gridrangemotion')
                set(gcbf,'WindowButtonupFcn','')
                set(SelectedLine,'userdata',[i j])
        end

    case {'pathdown','pathmotion'}
        G=findobj(gcbf,'tag','GRID');
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        GRID=get(G,'userdata');
        i=trackpnt(gcbf);
        UD = get(SelectedLine,'userdata');
        %
        if isempty(UD)
            points = [];
        else
            points = UD{1};
            DistanceState = UD{2};
            if DistanceState.distfromlast(i) ==0
                DistanceState = determine_frompoint(DistanceState,i);
                set(SelectedLine,'userdata',{points DistanceState})
            end
            frompoint = DistanceState.frompoint;
            ilast = points(end);
            ilist = repmat(NaN,10000,1);
            j = 1;
            while i~=ilast
                ilist(j) = i;
                j = j+1;
                i = frompoint(i);
            end
            i = ilist(j-1:-1:1);
        end
        %
        points = cat(1,points,i);
        mrkr='none';
        if length(points)==1
            mrkr='.';
        end
        switch GRID.Type
            case 'network'
                set(SelectedLine, ...
                    'xdata',GRID.XY(points,1), ...
                    'ydata',GRID.XY(points,2), ...
                    'linestyle','-','marker',mrkr)
            case 'structured'
                set(SelectedLine, ...
                    'xdata',GRID.X(points), ...
                    'ydata',GRID.Y(points), ...
                    'linestyle','-','marker',mrkr)
            case 'triangular'
                set(SelectedLine, ...
                    'xdata',GRID.XYZ(points,1), ...
                    'ydata',GRID.XYZ(points,2), ...
                    'linestyle','-','marker',mrkr)
        end
        switch cmd
            case 'pathdown'
                if strcmp(get(gcbf,'selectiontype'),'normal')
                    set(gcbf,'WindowButtonMotionFcn','qp_gridview pathmotion')
                    ilast = points(end);
                    switch GRID.Type
                        case 'network'
                            SEG = GRID.SEG;
                            Nseg = size(SEG,1);
                            Npnt = size(GRID.XY,1);
                            XY = reshape(GRID.XY(GRID.SEG,:),[Nseg 2 2]);
                            xx=GRID.XY(ilast,1);
                            yy=GRID.XY(ilast,2);
                            %
                            [dd,I]=sort((sum(XY(:,1,:),3)/2-xx).^2+(sum(XY(:,2,:),3)/2-yy).^2);
                            SEG=SEG(I,:);
                            XY=XY(I,:);
                        case 'triangular'
                            Ntri = size(GRID.TRI,1);
                            SEG = reshape(GRID.TRI(:,[1 1 2 2 3 3]),[3*Ntri 2]);
                            SEG = sort(SEG,2);
                            SEG = unique(SEG,'rows');
                            Nseg = size(SEG,1);
                            Npnt = size(GRID.XYZ,1);
                            XY = reshape(GRID.XYZ(SEG,[1 2]),[Nseg 2 2]);
                            xx=GRID.XYZ(ilast,1);
                            yy=GRID.XYZ(ilast,2);
                            %
                            [dd,I]=sort((sum(XY(:,1,:),3)/2-xx).^2+(sum(XY(:,2,:),3)/2-yy).^2);
                            SEG=SEG(I,:);
                            XY=XY(I,:,:);
                        case 'structured'
                            I = reshape(1:numel(GRID.X),size(GRID.X));
                            SEGx = [I(1:end-1,:) I(2:end,:)];
                            SEGx = reshape(SEGx,[numel(SEGx)/2 2]);
                            SEGy = [I(:,1:end-1) I(:,2:end)];
                            SEGy = reshape(SEGy,[numel(SEGy)/2 2]);
                            Missing = isnan(GRID.X(1:end-1,1:end-1)) | ...
                                isnan(GRID.X(2:end,1:end-1)) | ...
                                isnan(GRID.X(1:end-1,2:end)) | ...
                                isnan(GRID.X(2:end,2:end));
                            SEGd1= [I(1:end-1,1:end-1) I(2:end,2:end)];
                            SEGd1= reshape(SEGd1,[numel(SEGd1)/2 2]);
                            SEGd1(Missing,:)=[];
                            SEGd2= [I(2:end,1:end-1) I(1:end-1,2:end)];
                            SEGd2= reshape(SEGd2,[numel(SEGd2)/2 2]);
                            SEGd2(Missing,:)=[];
                            SEG = cat(1,SEGx,SEGy,SEGd1,SEGd2);
                            Nseg = size(SEG,1);
                            XY = [GRID.X(:) GRID.Y(:)];
                            Npnt = size(XY,1);
                            XY = reshape(XY(SEG,:),[Nseg 2 2]);
                            rm = any(isnan(XY(:,:,1)),2);
                            SEG(rm,:)=[];
                            XY(rm,:,:)=[];
                            [M,N]=ind2sub(size(GRID.X),SEG);
                            [m,n]=ind2sub(size(GRID.X),ilast);
                            [dd,I]=sort((sum(M,2)/2-m).^2+(sum(N,2)/2-n).^2);
                            SEG=SEG(I,:);
                            XY=XY(I,:,:);
                    end
                    dist = sqrt(sum(diff(XY,1,2).^2,3));
                    %
                    DistanceState=[];
                    DistanceState.Npnt=Npnt;
                    DistanceState.ilast=ilast;
                    DistanceState.SEG=SEG;
                    DistanceState.dist=dist;
                    %
                    DistanceState = determine_frompoint(DistanceState,ilast);
                    set(SelectedLine,'userdata',{points DistanceState})
                else
                    GRID.Selected.Type='pwline';
                    switch GRID.Type
                        case {'network','triangular'}
                            GRID.Selected.Range=points;
                        case 'structured'
                            [i,j]=ind2sub(size(GRID.X),points);
                            GRID.Selected.Range=[i j];
                    end
                    set(G,'userdata',GRID)
                    normalstate(gcbf)
                    qp_gridview('setrange',gcbf,GRID.Selected)
                end
        end

    case {'gridviewlineseg','gridviewline','gridviewrange', ...
            'gridviewpiecewise','gridviewarbline','gridviewarbrect', ...
            'gridviewpoint','gridviewpath','gridviewarbarea'}
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','')
        set(gcbf,'WindowButtonUpFcn','')
        switch cmd
            case 'gridviewlineseg'
                set(gcbf,'WindowButtonDownFcn','qp_gridview draglinedown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
            case 'gridviewline'
                set(gcbf,'WindowButtonDownFcn','qp_gridview dragwholelinedown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
            case 'gridviewrange'
                set(gcbf,'WindowButtonDownFcn','qp_gridview gridrangedown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
            case 'gridviewpiecewise'
                set(gcbf,'WindowButtonDownFcn','qp_gridview pwlinedown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview pwlinemotion')
            case 'gridviewarbline'
                set(gcbf,'WindowButtonDownFcn','qp_gridview genlinedown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview genlinemotion')
            case 'gridviewarbrect'
                set(gcbf,'WindowButtonDownFcn','qp_gridview genrectdown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview genrectmotion')
            case 'gridviewarbarea'
                set(gcbf,'WindowButtonDownFcn','qp_gridview genareadown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview genareamotion')
            case 'gridviewpoint'
                set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
                set(gcbf,'WindowButtonUpFcn','qp_gridview selpointup')
            case 'gridviewpath'
                set(gcbf,'WindowButtonDownFcn','qp_gridview pathdown')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
        end
        SelectedGrid=findobj(gcbf,'tag','SELSURF');
        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[],'userdata',[])
        SelectedPatch=findobj(gcbf,'tag','SELPATCH');
        set(SelectedPatch,'vertices',[],'faces',[])
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        set(SelectedLine,'userdata',[])
        switch cmd
            case {'gridviewlineseg','gridviewline','gridviewrange'}
                qp_gridview selpointmotion
            case 'gridviewpiecewise'
                qp_gridview pwlinemotion
            case 'gridviewarbline'
                qp_gridview genlinemotion
            case 'gridviewarbrect'
                qp_gridview genrectmotion
            case 'gridviewarbrect'
                qp_gridview genareamotion
            case 'gridviewpoint'
                qp_gridview selpointmotion
            case 'gridviewpath'
                qp_gridview pathmotion
        end

    case 'gridviewall'
        G=findobj(gcbf,'tag','GRID');
        SelectedLine=findobj(gcbf,'tag','SELLINE');
        X.Type='wholegrid';
        X.Range=[];
        set(SelectedLine,'xdata',[],'ydata',[],'userdata',X)
        GRID=get(G,'userdata');
        SelectedPatch=findobj(gcbf,'tag','SELPATCH');
        set(SelectedPatch,'vertices',[],'faces',[])
        SelectedGrid=findobj(gcbf,'tag','SELSURF');
        switch GRID.Type
            case 'structured'
                set(SelectedGrid,'xdata',GRID.X,'ydata',GRID.Y,'zdata',zeros(size(GRID.X)))
            case 'network'
                NSeg = size(GRID.SEG,1);
                XY = reshape(GRID.XY(GRID.SEG,:),[NSeg 2 2]);
                XY(:,3,:) = NaN;
                X=XY(:,:,1)';
                Y=XY(:,:,2)';
                set(SelectedLine,'xdata',X(:),'ydata',Y(:),'marker','none')
            case 'triangular'
                set(SelectedPatch,'vertices',GRID.XYZ,'faces',GRID.TRI)
        end
        qp_gridview execcallback

    case 'setgrid'
        F=varargin{1};
        if isstruct(varargin{2})
            GRID = varargin{2};
        else
            GRID.X=varargin{2};
            if nargin<4
                GRID.Y=[];
            else
                GRID.Y=varargin{3};
            end
        end
        if isfield(GRID,'XYZ')
            GRID.XYZ=squeeze(GRID.XYZ(1,:,1,:));
        end
        %
        SelectedGrid=findobj(F,'tag','SELSURF');
        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
        SelectedPatch=findobj(F,'tag','SELPATCH');
        set(SelectedPatch,'vertices',[],'faces',[])
        SelectedLine=findobj(F,'tag','SELLINE');
        set(SelectedLine,'xdata',[],'ydata',[])
        %
        localdrawgrid(F,GRID);
        G=findall(F,'tag','GRID');
        GRID=get(G,'userdata');
        GRID.Selected.Type='none';
        GRID.Selected.Range=[];
        set(G,'userdata',GRID)

    case 'initialize'
        F = figure('integerhandle','off', ...
            'color',qp_settings('gridviewbackgroundcolor')/255, ...
            'renderer','zbuffer', ...
            'doublebuffer','on', ...
            'name','Grid View', ...
            'numbertitle','off', ...
            'handlevisibility','off', ...
            'keypressfcn','qp_gridview keypress', ...
            'visible','off');
        set(F,'menubar','none')
        TB = uitoolbar('Parent',F);
        if matlabversionnumber > 5.02
            set(F,'ToolBar','none');
        end
        A=axes('parent',F,'unit','normalized','position',[0 0 1 1], ...
            'defaultlineclipping','off');
        uicontrol('parent',F,'style','edit','hittest','off', ...
            'enable','inactive','units','pixels','string','x,y:', ...
            'horizontalalignment','left','position',[1 1 150 20], ...
            'tag','XYcoord');
        uicontrol('parent',F,'style','edit','hittest','off', ...
            'enable','inactive','units','pixels','string','', ...
            'horizontalalignment','left','position',[151 1 70 20], ...
            'tag','MNcoord');
        G=surface([],[],'parent',A,'tag','GRID','userdata',GRID);
        selcolor = qp_settings('gridviewselectioncolor')/255;
        %SelectedGrid
        surface([],[],[],'parent',A, ...
            'facecolor',selcolor,'edgecolor','none', ...
            'tag','SELSURF','erasemode','xor');
        %SelectedPatch
        patch('vertices',[],'faces',[],'parent',A, ...
            'facecolor',selcolor,'edgecolor','none', ...
            'tag','SELPATCH','erasemode','xor');
        %SelectedLine
        line('xdata',[],'ydata',[],'parent',A, ...
            'color',selcolor,'markersize',18,'linewidth',4, ...
            'tag','SELLINE','erasemode','xor');
        set(A,'color','none','xtick',[],'ytick',[], ...
            'da',[1 1 1],'view',[0 90],'xcolor',get(F,'color'), ...
            'ycolor',get(F,'color'))
        xl=limits(A,'xlim'); xl=xl+[-1 1]*diff(xl)/20;
        yl=limits(A,'ylim'); yl=yl+[-1 1]*diff(yl)/20;
        if ~isfinite(xl), xl=[0 1]; yl=[0 1]; end
        set(A,'xlim',xl,'ylim',yl)
        set(allchild(A),'clipping','off','hittest','off')
        uim=uimenu('label','&Select','parent',F);
        %---
        qp_menutool(uim,TB,'gridviewpoint', ...
            'Grid &Point','Select point',0)
        %---
        %line,wholeline
        qp_menutool(uim,TB,'gridviewline', ...
            'Grid &Line','Select grid line',1)
        %lineseg,line
        qp_menutool(uim,TB,'gridviewlineseg', ...
            'Grid Line &Segment','Select grid line segment',0)
        %pwline,pwline
        qp_menutool(uim,TB,'gridviewpiecewise', ...
            'Piecewise Grid &Line','Select piecewise grid line',0)
        %path
        qp_menutool(uim,TB,'gridviewpath', ...
            'Shortest &Path','Select shortest path',0)
        %---
        %genline,genline
        qp_menutool(uim,TB,'gridviewarbline', ...
            'A&rbitrary Line','Select arbitrary line',1)
        %---
        %range,gridrange
        qp_menutool(uim,TB,'gridviewrange', ...
            'Grid &Range','Select grid range',1)
        %wholegrid,wholegrid
        qp_menutool(uim,TB,'gridviewall' ...
            ,'Whole &Grid','Select whole grid',0)
        %----
        %genrect,genrect
        qp_menutool(uim,TB,'gridviewarbrect', ...
            'Arbitrary Re&ctangle','Select arbitrary rectangle',1)
        %genarea,genarea
        qp_menutool(uim,TB,'gridviewarbarea', ...
            'Arbitrary &Area','Select arbitrary area',0)
        %---
        menusoff(F)
        GRID.Selected.Type='none';
        GRID.Selected.Range=[];
        set(G,'userdata',GRID)
        qp_gridview('setgrid',F,GRID)
        if ~isempty(UsrRange)
            if isstruct(UsrRange)
                GRID.Selected=UsrRange;
            else
                GRID.Selected.Type='range';
                GRID.Selected.Range=UsrRange;
            end
            qp_gridview('setrange',F,GRID.Selected)
        end
        if nargout>0
            out=F;
        end
        set(F,'visible','on')
        set(F,'windowbuttonmotionfcn','qp_gridview trackcoord')

    case 'execcallback'
        F=gcbf;
        G=findobj(F,'tag','GRID');
        A=get(G,'parent');
        UD=get(A,'userdata');
        if ~isempty(UD)
            %    d3d_qp gridviewupdate
            feval(UD{1},UD{2:end})
        end

    case 'keypress'
        F = gcbf;
        ch = get(gcbf,'currentcharacter');
        if ~isempty(ch)
            switch ch
                case 27
                    normalstate(F)
                    G = findobj(F,'tag','GRID');
                    GRID = get(G,'userdata');
                    qp_gridview('setrange',F,GRID.Selected)
                otherwise % all other keys
            end
        end

    case 'callback'
        F=varargin{1};
        G=findobj(F,'tag','GRID');
        A=get(G,'parent');
        set(A,'userdata',varargin(2:end))

    case 'setrange'
        F=varargin{1};
        UsrRange=varargin{2};
        %
%         zoom(F,'off');
%         v72 = matlabversionnumber>=7.02;
%         if v72
%             %Disable listeners
%             mmgr = uigetmodemanager(F);
%             set(mmgr.WindowListenerHandles,'Enable','off');
%         end
%         set(F,'WindowButtonDownFcn','')
%         set(F,'WindowButtonUpFcn','')
        set(F,'WindowButtonMotionFcn','qp_gridview trackcoord')
%         zoom(F,'on');
%         if v72
%             set(mmgr.WindowListenerHandles,'Enable','on');
%         end
        %
        SelectedGrid=findobj(F,'tag','SELSURF');
        SelectedPatch=findobj(F,'tag','SELPATCH');
        SelectedLine=findobj(F,'tag','SELLINE');
        G=findobj(F,'tag','GRID');
        GRID=get(G,'userdata');
        GRID.Selected.Type='none';
        GRID.Selected.Range=[];
        if ~isempty(UsrRange)
            if isstruct(UsrRange)
                GRID.Selected=UsrRange;
            else
                GRID.Selected.Type='range';
                GRID.Selected.Range=UsrRange;
            end
        end
        %
        if isempty(GRID.Selected.Range) && ~strcmp(GRID.Selected.Type,'wholegrid')
            GRID.Selected.Type='none';
        end
        switch GRID.Selected.Type
            case 'none'
                set(SelectedLine,'xdata',[],'ydata',[])
                set(SelectedPatch,'vertices',[],'faces',[])
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            case 'point'
                switch GRID.Type
                    case 'structured'
                        set(SelectedPatch,'vertices',[],'faces',[])
                        set(SelectedLine, ...
                            'xdata',GRID.X(GRID.Selected.Range(1),GRID.Selected.Range(2)), ...
                            'ydata',GRID.Y(GRID.Selected.Range(1),GRID.Selected.Range(2)), ...
                            'marker','.')
                    case 'triangular'
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPatch, ...
                            'vertices',GRID.XYZ, ...
                            'faces',GRID.TRI(GRID.Selected.Range(1),:))
                end
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            case 'range'
                Range = GRID.Selected.Range;
                if ~iscell(Range)
                    Range={min(Range(1:2)):max(Range(1:2)) min(Range(3:4)):max(Range(3:4))};
                end
                switch GRID.Type
                    case 'structured'
                        im = Range{1};
                        if length(Range)>=2
                            in = Range{2};
                        else
                            in = 1;
                        end
                        if length(im)==1 || length(in)==1
                            mrkr='none';
                            if length(im)==1 && length(in)==1
                                mrkr='.';
                            end
                            set(SelectedPatch,'vertices',[],'faces',[])
                            set(SelectedLine, ...
                                'xdata',GRID.X(im,in), ...
                                'ydata',GRID.Y(im,in), ...
                                'marker',mrkr,'linestyle','-')
                            set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                        else
                            set(SelectedLine,'xdata',[],'ydata',[])
                            set(SelectedPatch,'vertices',[],'faces',[])
                            set(SelectedGrid, ...
                                'xdata',GRID.X(im,in), ...
                                'ydata',GRID.Y(im,in), ...
                                'zdata',zeros(length(im),length(in)))
                        end
                    case 'triangular'
                        ip = Range{1};
                        it = GRID.TRI(all(ismember(GRID.TRI,ip),2),:);
                        ip = ip(~ismember(ip,it));
                        set(SelectedLine, ...
                            'xdata',GRID.XYZ(ip,1), ...
                            'ydata',GRID.XYZ(ip,2), ...
                            'marker','.','linestyle','none')
                        set(SelectedPatch,'vertices',GRID.XYZ,'faces',it)
                        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                    case 'network'
                        ip = Range{1};
                        SEG = GRID.SEG(all(ismember(GRID.SEG,ip),2),:);
                        if isempty(SEG)
                            set(SelectedLine, ...
                                'xdata',GRID.XY(ip,1), ...
                                'ydata',GRID.XY(ip,2), ...
                                'marker','.','linestyle','none')
                        else
                            NSeg = size(SEG,1);
                            XY = reshape(GRID.XY(SEG,:),[NSeg 2 2]);
                            XY(:,3,:) = NaN;
                            X=XY(:,:,1)';
                            Y=XY(:,:,2)';
                            set(SelectedLine, ...
                                'xdata',X(:), ...
                                'ydata',Y(:), ...
                                'marker','none','linestyle','-')
                        end
                        set(SelectedPatch,'vertices',[],'faces',[])
                        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                end
            case 'wholegrid'
                set(SelectedLine,'xdata',[],'ydata',[])
                set(SelectedPatch,'vertices',[],'faces',[])
                set(SelectedGrid, ...
                    'xdata',GRID.X, ...
                    'ydata',GRID.Y, ...
                    'zdata',zeros(size(GRID.X)))
            case 'genline'
                mrkr='none';
                if size(GRID.Selected.Range,1)==1
                    mrkr='.';
                end
                set(SelectedLine, ...
                    'xdata',GRID.Selected.Range(:,1), ...
                    'ydata',GRID.Selected.Range(:,2), ...
                    'marker',mrkr,'linestyle','-')
                set(SelectedPatch,'vertices',[],'faces',[])
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            case 'line'
                if isfinite(GRID.Selected.Range(1))
                    set(SelectedLine, ...
                        'xdata',GRID.X(GRID.Selected.Range(1),:), ...
                        'ydata',GRID.Y(GRID.Selected.Range(1),:), ...
                        'marker','none','linestyle','-')
                else
                    set(SelectedLine, ...
                        'xdata',GRID.X(:,GRID.Selected.Range(2)), ...
                        'ydata',GRID.Y(:,GRID.Selected.Range(2)), ...
                        'marker','none','linestyle','-')
                end
                set(SelectedPatch,'vertices',[],'faces',[])
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            case 'lineseg'
                i1=min(GRID.Selected.Range([1 2]));
                i2=max(GRID.Selected.Range([1 2]));
                j1=min(GRID.Selected.Range([3 4]));
                j2=max(GRID.Selected.Range([3 4]));
                mrkr='none';
                if (i1==i2) && (j1==j2)
                    mrkr='.';
                end
                set(SelectedLine, ...
                    'xdata',GRID.X(i1:i2,:), ...
                    'ydata',GRID.Y(j1:j2,:), ...
                    'marker',mrkr,'linestyle','-')
                set(SelectedPatch,'vertices',[],'faces',[])
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            case 'pwline'
                switch GRID.Type
                    case 'structured'
                        RNG=piecewise(GRID.Selected.Range,size(GRID.X));
                        i0 = RNG(:,1);
                        j0 = RNG(:,2);
                        ind=sub2ind(size(GRID.X),i0,j0);
                        mrkr='none';
                        if length(ind)==1
                            mrkr='.';
                        end
                        set(SelectedLine, ...
                            'xdata',GRID.X(ind), ...
                            'ydata',GRID.Y(ind), ...
                            'marker',mrkr,'linestyle','-')
                    case 'triangular'
                        ind = GRID.Selected.Range;
                        mrkr='none';
                        if length(ind)==1
                            mrkr='.';
                        end
                        set(SelectedLine, ...
                            'xdata',GRID.XYZ(ind,1), ...
                            'ydata',GRID.XYZ(ind,2), ...
                            'marker',mrkr,'linestyle','-')
                    case 'network'
                        ind = GRID.Selected.Range;
                        mrkr='none';
                        if length(ind)==1
                            mrkr='.';
                        end
                        set(SelectedLine, ...
                            'xdata',GRID.XY(ind,1), ...
                            'ydata',GRID.XY(ind,2), ...
                            'marker',mrkr,'linestyle','-')
                end
                set(SelectedPatch,'vertices',[],'faces',[])
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
        end
        set(G,'userdata',GRID)
        return

    otherwise
        fprintf('Unkwown command: %s\n',cmd)
end

function [mdist,indrng,i1,j1]=NearDiag(dist,i0,j0)
sz=size(dist);
szi=sz(1);
szj=sz(2);
ind0=sub2ind(sz,i0,j0);

di=min(i0,j0)-1;
ir=i0-di;
jr=j0-di;
di=min(szi-ir,szj-jr);
ir=ir+(0:di);
jr=jr+(0:di);

ind=sub2ind(sz,ir,jr);
[m1dist,ind1]=min(dist(ind));
ir1=ir(ind1);
jr1=jr(ind1);
ind1=ind(ind1);
if ind1<ind0
    indrng1=ind0:-(szi+1):ind1;
else
    indrng1=ind0:(szi+1):ind1;
end

di=min(i0,szj-j0+1)-1;
ir=i0-di;
jr=j0+di;
di=min(szi-ir,jr-1);
ir=ir+(0:di);
jr=jr-(0:di);

ind=sub2ind(sz,ir,jr);
[m2dist,ind2]=min(dist(ind));
ir2=ir(ind2);
jr2=jr(ind2);
ind2=ind(ind2);
if ind2<ind0
    indrng2=ind0:-(szi-1):ind2;
else
    indrng2=ind0:(szi-1):ind2;
end

if m1dist<m2dist
    indrng=indrng1;
    mdist=m1dist;
    i1=ir1;
    j1=jr1;
else
    indrng=indrng2;
    mdist=m2dist;
    i1=ir2;
    j1=jr2;
end

function qp_menutool(uim,TB,tag,label,tooltip,separator)
prog = 'qp_gridview';
newmenu = uimenu('tag',tag,'label',label, ...
    'parent',uim, ...
    'callback',sprintf('%s %s',prog,tag));
if separator
    set(newmenu,'separator','on')
end
qp_toolbarpush(TB,tag,separator,tooltip,prog);

function localdrawgrid(F,GRID)
zoom(F,'off');
G=findobj(F,'tag','GRID');
Go=findobj(F,'tag','GRIDother');
A=get(G,'parent');
delete(G)
delete(Go)
gridcol = qp_settings('gridviewgridcolor')/255;
off = 'off';
if isfield(GRID,'X') && isfield(GRID,'Y')
    if ~isempty(GRID.X)
        GRID.Type = 'structured';
        GRID.X = GRID.X(:,:,1);
        GRID.Y = GRID.Y(:,:,1);
        if qp_settings('gridviewshowindices')
            gridstep = max(10,round(size(GRID.X)/10));
        else
            gridstep = [];
        end
        G=drawgrid(GRID.X,GRID.Y, ...
            'color',gridcol,'fontsize',8,'parent',A,'gridstep',gridstep);
        off='on';
    else
        GRID.Type = 'none';
        G=surface([],[],'parent',A);
        off='off';
    end
elseif isfield(GRID,'SEG')
    GRID.Type = 'network';
    NSeg = size(GRID.SEG,1);
    XY = reshape(GRID.XY(GRID.SEG,:),[NSeg 2 2]);
    XY(:,3,:) = NaN;
    X=XY(:,:,1)';
    Y=XY(:,:,2)';
    G=line('parent',A);
    set(G,'xdata',X(:),'ydata',Y(:),'color',gridcol);
    if ~isempty(X)
        off = 'on';
    end
else
    GRID.Type = 'triangular';
    G = patch('faces',GRID.TRI,'vertices',GRID.XYZ,...
        'facecolor','none','edgecolor',gridcol,'parent',A);
    off='on';
end
set(G(1),'tag','GRID','userdata',GRID)
set(G(2:end),'tag','GRIDother')
set(G,'clipping','off','hittest','off')
%
xl=limits(A,'xlim'); xl=xl+[-1 1]*max(0.00001,abs(diff(xl)*0.01))/20;
yl=limits(A,'ylim'); yl=yl+[-1 1]*max(0.00001,abs(diff(yl)*0.01))/20;
if ~isfinite(xl)
    xl=[0 1];
    yl=[0 1];
end
set(A,'xlim',xl,'ylim',yl)
delete(get(A,'zlabel')) % delete the old ZOOMAxesData applicationdata
drawnow
zoom(F,'reset');
zoom(F,off);
%
selectMenu = findall(F,'label','&Select');
set(selectMenu,'enable',off)
set(findall(selectMenu),'enable',off)
set(findall(F,'type','uipushtool'),'enable',off)
switch GRID.Type
    case 'triangular'
        set(findall(F,'tag','gridviewrange'),'enable','off')
        set(findall(F,'tag','gridviewpiecewise'),'enable','off')
        set(findall(F,'tag','gridviewlineseg'),'enable','off')
        set(findall(F,'tag','gridviewline'),'enable','off')
    case 'network'
        set(findall(F,'tag','gridviewrange'),'enable','off')
        set(findall(F,'tag','gridviewpiecewise'),'enable','off')
        set(findall(F,'tag','gridviewlineseg'),'enable','off')
        set(findall(F,'tag','gridviewline'),'enable','off')
        set(findall(F,'tag','gridviewarbline'),'enable','off')
        %set(findall(F,'tag','gridviewarbrect'),'enable','off')
    case 'structured'
        set(findall(F,'tag','gridviewarbrect'),'enable','off')
        set(findall(F,'tag','gridviewarbarea'),'enable','off')
end

function menusoff(F)
obj = cat(1,findall(F,'type','uimenu'),findall(F,'type','uipushtool'));
for i=1:length(obj)
    set(obj(i),'enable','off','userdata',get(obj(i),'enable'))
end

function menusreset(F)
obj = cat(1,findall(F,'type','uimenu'),findall(F,'type','uipushtool'));
for i=1:length(obj)
    set(obj(i),'enable',get(obj(i),'userdata'))
end

function trackxy(F,pnt)
XY=findobj(F,'tag','XYcoord');
xf = sprintf('%%.%if',min(3,6-floor(log10(abs(pnt(1))))));
yf = sprintf('%%.%if',min(3,6-floor(log10(abs(pnt(2))))));
set(XY,'string',sprintf(['x,y: ',xf,',',yf],pnt))

function [i,j]=trackpnt(F)
G=findobj(F,'tag','GRID');
GRID=get(G,'userdata');
pnt=get(get(G,'parent'),'currentpoint');
pnt=pnt(1,1:2);
switch GRID.Type
    case 'structured'
        dist=(pnt(1)-GRID.X).^2+(pnt(2)-GRID.Y).^2;
    case 'network'
        dist=(pnt(1)-GRID.XY(:,1)).^2+(pnt(2)-GRID.XY(:,2)).^2;
    case 'triangular'
        dist=(pnt(1)-GRID.XYZ(:,1)).^2+(pnt(2)-GRID.XYZ(:,2)).^2;
end
mdist=min(dist(:));
[i,j]=find(dist==mdist);
i=i(1);
j=j(1);
%
trackxy(gcbf,pnt)
MN=findobj(gcbf,'tag','MNcoord');
switch GRID.Type
    case 'structured'
        set(MN,'string',sprintf('m,n: %i,%i',i,j))
        %
        if nargout==1
            i=sub2ind(size(GRID.X),i,j);
        end
    case {'network','triangular'}
        set(MN,'string',sprintf('pnt: %i',i))
end

function normalstate(F)
set(F,'WindowButtonDownFcn','')
set(F,'WindowButtonMotionFcn','')
set(F,'WindowButtonUpFcn','')
menusreset(F)
zoom(F,'inmode');
% v72 = matlabversionnumber>=7.02;
% if v72
%     %Disable listeners
%     mmgr = uigetmodemanager(gcbf);
%     set(mmgr.WindowListenerHandles,'Enable','off');
% end
set(F,'WindowButtonMotionFcn','qp_gridview trackcoord')
% if v72
%     set(mmgr.WindowListenerHandles,'Enable','on');
% end
qp_gridview execcallback
