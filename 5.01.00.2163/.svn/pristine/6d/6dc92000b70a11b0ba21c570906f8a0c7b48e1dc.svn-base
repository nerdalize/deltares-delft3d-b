function qp_updatefieldprop(UD)
%QP_UPDATEFIELDPROP Update subfield, time, M, N, K and grid view in QP dialog.

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

MW=UD.MainWin;
Inactive=UD.Inactive;
Active=UD.Active;
T_=1; ST_=2; M_=3; N_=4; K_=5;
dims={'T','S','M','N','K'};

%
% Get file data
%
File=get(MW.File,'userdata');
NrInList=get(MW.File,'value');
Success=~isempty(File) & NrInList<=length(File);

%
% Get information on active data field
%
Props=get(MW.Field,'userdata');
fld=get(MW.Field,'value');

if isempty(Props) || ~isfield(Props,'DimFlag') || ~isempty(strmatch('---',Props(fld).Name)) || ~Success
    %
    % No quantity selected, so disable all controls and set them to default
    % appearance.
    %
    % Subfield controls ...
    %
    set(MW.SubFldTxt,'enable','off')
    set(MW.SubFld,'enable','off','string',' ','value',1,'backgroundcolor',Inactive)
    %
    % Other controls ...
    %
    disablecontrols(UD)
    d3d_qp hidegridview
    %
    return
end

%
% Select information for active file
%
Info=File(NrInList);

%
% Select active domain
%
DomainNr=get(MW.DList,'value');

%
% Update subfield information for active data field
%
[Chk,SubF]=qp_getdata(Info,DomainNr,Props(fld),'subfields');
if Chk && ~isempty(SubF)
    %
    % Subfields obtained: enable subfield controls
    %
    if isnumeric(SubF)
        SubF=cellstr(multiline(sprintf('field %i\n',1:Info.SizeDim(3))));
        SubF=SubF(1:end-1);
    end
    sf=1;
    if strcmp(get(MW.SubFld,'enable'),'on')
        pnames=get(MW.SubFld,'string');
        sf=get(MW.SubFld,'value');
        pname=pnames{sf};
        sf=ustrcmpi(pname,SubF); % first check equality and longer names
        if sf<0 % no matches
            sf=ustrcmpi(SubF,pname); % check (equality and) shorter names
            if sf<0 % still no matches
                sf=1;
            end
        end
    end
    set(MW.SubFldTxt,'enable','on')
    set(MW.SubFld,'string',SubF,'value',sf,'backgroundcolor',Active,'enable','on')
else
    %
    % No subfields: disable subfield controls
    %
    set(MW.SubFldTxt,'enable','off')
    set(MW.SubFld,'enable','off','string',' ','value',1,'backgroundcolor',Inactive)
end

%
% Update grid view
%
if isfield(Props(fld),'UseGrid') && ~isempty(Props(fld).UseGrid) && Props(fld).UseGrid>0
    %
    % Gridview information: update grid view when shown
    %
    UseGrid=get(UD.GridView.Fig,'userdata');
    i_grd=Props(fld).UseGrid;
    UseGridNew={Info.Name,DomainNr,i_grd};
    if strcmp(get(UD.GridView.Fig,'visible'),'on') ...
            && ~isequal(UseGrid,UseGridNew) ...
            && UseGridNew{2}>0
        set(UD.GridView.Fig,'name','Grid View: updating grid ...')
        drawnow
        [Chk,GRID]=qp_getdata(Info,DomainNr,Props(i_grd),'grid');
        qp_gridview('setgrid',UD.GridView.Fig,GRID)
        set(UD.GridView.Fig,'name','Grid View')
        set(UD.GridView.Fig,'userdata',UseGridNew)
    end
else
    %
    % No gridview information: switch off grid view
    %
    qp_gridview('setgrid',UD.GridView.Fig,[])
    set(UD.GridView.Fig,'userdata',[])
    d3d_qp hidegridview
end

%
% Get size information and return in case of error
%
[Chk,sz]=qp_getdata(Info,DomainNr,Props(fld),'size');
if ~Chk
    %
    % Disable remaining controls ...
    %
    disablecontrols(UD)
    return
end
DimFlag=Props(fld).DimFlag;
if any(DimFlag(2:end)==7)
    [Chk,dimlabels]=qp_getdata(Info,DomainNr,Props(fld),'dimlabels');
    if ~Chk
        dimlabels=cell(size(DimFlag));
        for i=2:length(DimFlag)
            if DimFlag(i)==7
                dimlabels{i} = 1:sz(i);
            end
        end
    end
else
    dimlabels=cell(size(DimFlag));
end

%
% If multiple time steps, enable time selection controls
%
if sz(T_)>1
    set(MW.T,'enable','on')
    pUDM=get(MW.AllT,'userdata');
    if ~isempty(pUDM)
        if length(pUDM)>=3 && isequal(sz(T_),pUDM{3})
            allt=pUDM{1};
            selt=pUDM{2};
        else
            allt=~any(DimFlag([M_ N_ K_]));
            selt=sz(T_);
        end
    else
        allt=~any(DimFlag([M_ N_ K_]));
        selt=sz(T_);
    end
    set(MW.AllT,'enable','on','value',allt,'userdata',{allt selt sz(T_)})
    set(MW.EditT,'string',vec2str(selt,'nobrackets','noones'),'userdata',selt)
    if allt
        set(MW.EditT,'enable','off','backgroundcolor',Inactive)
    else
        set(MW.EditT,'enable','on','backgroundcolor',Active)
    end
    set(MW.MaxT,'enable','on','string',sprintf('%i',sz(T_)),'userdata',sz(T_))
    if sz(T_)>30000 ... % disable timelist if there are more than 30000 times
            || allt        % or if all times have been selected
        set(MW.ShowT,'enable','off')
    else
        set(MW.ShowT,'enable','on')
    end
    set(MW.TList,'enable','off','max',2,'value',[],'string','','backgroundcolor',Inactive,'userdata',0)
    d3d_qp showtimes
else
    %
    % No time steps or only one time step, disable time selection controls
    %
    set(MW.T,'enable','off')
    set(MW.AllT,'enable','off','value',0)
    set(MW.EditT,'enable','off','string','1','backgroundcolor',Inactive,'userdata',1)
    set(MW.MaxT,'enable','on','string','1','userdata',1)
    set(MW.ShowT,'enable','off')
    set(MW.TList,'enable','off','max',2,'value',[],'string','','backgroundcolor',Inactive,'userdata',0)
end

%
%
%
set(MW.QuickV,'string','Quick View');
c10=char(10);

%
% Show station selection or MNK/XYZ selection controls
%
stlist=1;
if DimFlag(ST_)
    %
    % Stations, so show station selection controls and hide MNK/XYZ
    % selection controls
    %
    set(MW.HSelType,'visible','off')
    set(MW.VSelType,'visible','off')
    % switch to editbox if there are more than 30000 stations
    % or if it is possible to select multiple stations ...
    if sz(ST_)>30000 || ~ismember(DimFlag(ST_),[0 3 5 13 15])
        set(MW.Stat,'visible','on')
        set(MW.StList,'visible','off')
        stlist=0;
    else
        set(MW.Stat,'visible','off')
        set([MW.S MW.AllS MW.StList],'visible','on')
    end
else
    %
    % No stations, so hide station selection controls and show MNK/XYZ
    % selection controls
    %
    set(MW.Stat,'visible','off')
    set(MW.HSelType,'enable','on','visible','on','backgroundcolor',Active)
    set(MW.VSelType,'enable','on','visible','on','backgroundcolor',Active)
end

%
% Disable/enable switching between spatial selection mechanisms
%
TRI = 0;
if isfield(Props,'Tri') && ~isempty(Props(fld).Tri) && Props(fld).Tri
   TRI = 1;
elseif isfield(Props,'Geom') && isequal(Props(fld).Geom,'TRI')
   TRI = 1;
end
if DimFlag(M_) && DimFlag(N_)
    % structured grid
    set(MW.HSelType,'string',{'M range and N range','(M,N) point/path','(X,Y) point/path'})
elseif DimFlag(M_) && TRI
    % triangular
    set(MW.HSelType,'string',{'M range and N range','(M,N) point/path','(X,Y) point/path'})
elseif DimFlag(M_)
    % network
    v=get(MW.HSelType,'value');
    if v==3
        v=1;
    end
    set(MW.HSelType,'string',{'M range and N range','(M,N) point/path'},'value',1)
else
    % no m,n
    set(MW.HSelType,'enable','off','backgroundcolor',Inactive)
end
if 1%~DimFlag(K_)
    set(MW.VSelType,'enable','off','backgroundcolor',Inactive)
end

%
% Update controls for station, m, n and k
%
for m_=[ST_ M_ N_ K_]
    %
    % Get handles of relevant controls ...
    %
    m=dims{m_};
    UDM=MW.(m);
    %
    mstr=m;
    if m_ == ST_
        mstr='Station';
    end
    mName = [m 'Name'];
    if isfield(Props,mName)
        mstr1 = Props(fld).(mName);
        if ~isempty(mstr1)
            mstr = mstr1;
        end
    end
    %
    UDAllM  = MW.(['All'  m]);
    UDEditM = MW.(['Edit' m]);
    UDMaxM  = MW.(['Max'  m]);
    %
    % If dimension is active, set controls to appropriate status
    %
    if DimFlag(m_)
        if sz(m_)==1 && m_~=ST_
            %
            % If dimension is degenerate (size one) then there is no choice.
            % Indicate selection and disable controls.
            %
            set(UDM,'string',mstr,'enable','off')
            set(UDAllM,'enable','off','value',0)
            val = 1;
            if DimFlag(m_)==7
                val = dimlabels{m_};
                setappdata(UDEditM,'dimlabels',dimlabels{m_})
            end
            set(UDEditM,'enable','off','string',vec2str(val,'nobrackets'),'backgroundcolor',Inactive,'userdata',val)
            set(UDMaxM,'enable','on','string','1','userdata',1)
            selm=1;
        elseif DimFlag(m_)==4 || DimFlag(m_)==14 || DimFlag(m_)==inf
            %
            % If DimFlag=4 or 14 or inf (variable number of elements) then the
            % only option is to select all values. Indicate selection and
            % disable controls.
            %
            set(UDM,'string',mstr,'enable','on')
            set(UDAllM,'enable','off','value',1)
            set(UDEditM,'enable','off','string','1','backgroundcolor',Inactive,'userdata',1)
            if isfinite(DimFlag(m_))
                set(UDMaxM,'enable','on','string',sprintf('%i',sz(m_)),'userdata',sz(m_))
            else
                set(UDMaxM,'enable','on','string','?','userdata',inf)
            end
        else
            %
            % There is some choice left for the user, so enable the controls
            % and update the tooltips.
            %
            pUDM=get(UDAllM,'userdata');
            if ~isempty(pUDM)
                if length(pUDM)>=3 && isequal(sz(m_),pUDM{3})
                    allm=pUDM{1};
                    selm=pUDM{2};
                elseif m_==ST_
                    allm=0;
                    selm=1;
                else
                    allm=1;
                    selm=1;
                end
            elseif m_==ST_
                allm=0;
                selm=1;
            else
                allm=1;
                selm=1;
            end
            %
            allmon='on';
            if isempty(selm)
                selm = 1;
            end
            switch DimFlag(m_)
                case {1,11} % All, Range, Element
                    if ~isequal(selm,selm(1):selm(end))
                        selm=selm(1);
                    end
                case {2,12} % All, Range
                    if isequal(size(selm),[1 1])
                        selm=1:sz(m_);
                    end
                case {3,13} % All, Element
                    selm=selm(1);
                case {4,14,inf} % All -> handled above
                    % nothing to do anymore
                case {5,15} % Element
                    allm=0;
                    allmon='off';
                    selm=selm(1);
                case {6,16} % Any list of elements
                    % nothing to do
                case {7} % Element from list
                    allm=0;
                    allmon='off';
                    selm=selm(1);
                    if ~ismember(selm,dimlabels{m_})
                        selm=dimlabels{m_}(1);
                    end
            end
            %
            setappdata(UDEditM,'dimlabels',dimlabels{m_})
            set(UDM,'string',mstr,'enable','on')
            set(UDAllM,'enable',allmon,'value',allm,'userdata',{allm selm sz(m_)})
            set(UDEditM,'string',vec2str(selm,'nobrackets','noones'),'userdata',selm)
            if allm,
                set(UDEditM,'enable','off','backgroundcolor',Inactive);
            else
                set(UDEditM,'enable','on','backgroundcolor',Active);
            end
            set(UDMaxM,'enable','on','string',sprintf('%i',sz(m_)),'userdata',sz(m_))
            %
            set(UDAllM,'tooltip',cat(2,'Select all ',lower(mstr),' (equal to : range)'))
            switch DimFlag(m_)
                case {1,11} % All, Range, Element
                    Str=cat(2,'specify ',lower(mstr),' value(s):',c10,'single value, e.g. 4, or',c10,'range, e.g. 20:100');
                case {2,12} % All, Range
                    Str=cat(2,'specify ',lower(mstr),' values:',c10,'range, e.g. 20:100');
                case {3,13} % All, Element
                    Str=cat(2,'specify ',lower(mstr),' value:',c10,'single value, e.g. 4');
                case {4,14,inf} % All -> edit box not active, so, no need to change tooltip
                    % no tooltip
                case {5,15} % Element
                    Str=cat(2,'specify ',lower(mstr),' value:',c10,'single value, e.g. 4');
                case {6,16} % Any list of elements
                    Str=cat(2,'specify ',lower(mstr),' value(s):',c10,'single value, e.g. 4,',c10,'range, e.g. 20:100',c10,'range with step, e.g. 1:4:101,',c10,'or any combination, e.g. 1:5 7 10 14');
                case {7} % Element from limited list
                    Str=cat(2,'specify ',lower(mstr),' value:',c10,'single value from following list',c10,vec2str(dimlabels{m_},'nobrackets'));
            end
            if ~isempty(Str), set(UDEditM,'tooltip',Str); end
            %
        end
        %
        % In case of station dimension, fill the station list when it has not
        % been disabled.
        %
        if m_==ST_
            [Chk,Stats]=qp_getdata(Info,DomainNr,Props(fld),'stations');
            if stlist
                if isempty(Stats) && sz(ST_)==0
                    set(MW.S,'enable','off')
                    set(MW.StList,'visible','on','enable','off','value',1,'string',' ','backgroundcolor',Inactive)
                else
                    if isempty(Stats)
                        x=sz(ST_);
                        Statw=ceil(log10(x+1));
                        Stats=sprintf(strcat('%-',num2str(Statw),'i'),1:x);
                        Stats=cat(2,repmat('station ',x,1),reshape(Stats,[Statw,x])');
                        %for j=sz(ST_):-1:1, Stats{j}=sprintf('station %i',j); end
                    elseif iscellstr(Stats)
                        Stats=char(Stats); % use char instead of strvcat to keep empty names
                    end
                    if allm
                        set(MW.StList,'visible','on','enable','off','value',1,'string',Stats,'backgroundcolor',Inactive,'userdata',Stats)
                    else
                        set(MW.StList,'visible','on','enable','on','value',selm,'string',Stats,'backgroundcolor',Active,'userdata',Stats)
                    end
                end
            else
                set(MW.StList,'visible','off','enable','off','userdata',Stats)
            end
        end
    else
        %
        % Dimension not active, disable controls
        %
        set(UDM,'string',mstr,'enable','off')
        set(UDAllM,'enable','off','value',0)
        set(UDEditM,'enable','off','string','','backgroundcolor',Inactive)
        set(UDMaxM,'enable','off','string','-')
        if m_==ST_
            set(MW.StList,'enable','off','value',1,'string',' ','backgroundcolor',Inactive)
        end
    end
end

%
% If horizontal 2D spatial data, activate other MN/XY selection mechanisms
%
if strcmp(get(MW.HSelType,'enable'),'on')
    set(MW.MN,'enable','on')
    set(MW.EditMN,'enable','on','backgroundcolor',Active)
    mn = get(MW.EditMN,'userdata');
    if DimFlag(N_) && size(mn,2)<2
        mn=[];
    elseif ~DimFlag(N_) && size(mn,2)>1
        mn=[];
    end

    if DimFlag(M_) && DimFlag(N_)
        % structured 2D domain
        [mnexp,mn1]=piecewise(mn,sz([M_ N_]));
    else
        %unstructured domain
        mmax = get(MW.MaxM,'userdata');
        %
        firstproblem = min(find(any(mn<1 | mn>mmax)));
        if ~isempty(firstproblem)
            mn1 = mn(1:firstproblem-1);
        else
            mn1 = mn;
        end
    end

    if isempty(mn)
        set(MW.EditMN,'string','','userdata',[])
    elseif ~isequal(mn,mn1)
        if size(mn,2)==2
            mnstr=sprintf('%i, %i; ',mn1');
        else
            mnstr=sprintf('%i; ',mn1');
        end
        if ~isempty(mnstr)
            mnstr(end-1:end)=[];
        else
            mnstr='';
            mnl=[];
        end
        set(MW.EditMN,'string',mnstr,'userdata',mn1)
    end
else
    set(MW.MN,'enable','off')
    set(MW.EditMN,'enable','off','backgroundcolor',Inactive)
end
if ~isempty(strmatch('(X,Y) ',get(MW.HSelType,'string'))) && strcmp(get(MW.HSelType,'enable'),'on')
    set(MW.XY,'enable','on')
    set(MW.EditXY,'enable','on','backgroundcolor',Active)
else
    set(MW.XY,'enable','off')
    set(MW.EditXY,'enable','off','backgroundcolor',Inactive)
end

%
% If 3D other K/Z selection mechanisms
%
if ~isempty(strmatch('Z ',get(MW.VSelType,'string'))) && strcmp(get(MW.VSelType,'enable'),'on')
    set(MW.Z,'enable','on')
    set(MW.EditZ,'enable','on','backgroundcolor',Active)
else
    set(MW.Z,'enable','off')
    set(MW.EditZ,'enable','off','backgroundcolor',Inactive)
end

%
% Other buttons and options
%
set(MW.LoadData,'enable','on')
set(MW.DefVar,'enable','on')
d3d_qp updateoptions


function disablecontrols(UD)
Inactive=get(0,'defaultuicontrolbackgroundcolor');
MW=UD.MainWin;
%
% Time controls ...
%
set(MW.T,'enable','off')
set(MW.AllT,'enable','off')
set(MW.EditT,'enable','off','string','','backgroundcolor',Inactive)
set(MW.MaxT,'enable','off','string','-')
set(MW.ShowT,'enable','off')
set(MW.TList,'enable','off','max',2,'value',[],'string','','backgroundcolor',Inactive,'userdata',0)
%
% Station controls ...
%
set(MW.S,'enable','off')
set(MW.AllS,'enable','off','visible','off')
set(MW.EditS,'enable','off','string','','backgroundcolor',Inactive,'visible','off')
set(MW.MaxS,'enable','off','string','-','visible','off')
set(MW.StList,'enable','off','value',1,'string',' ','backgroundcolor',Inactive)
%
% MNK/XYZ selection controls ...
%
set(MW.HSelType,'enable','off','backgroundcolor',Inactive)
set(MW.VSelType,'enable','off','backgroundcolor',Inactive)
%
% MN/XY controls ...
%
set(MW.MN,'enable','off')
set(MW.EditMN,'enable','off','backgroundcolor',Inactive)
set(MW.XY,'enable','off')
set(MW.EditXY,'enable','off','backgroundcolor',Inactive)
%
% M controls ...
%
set(MW.M,'enable','off')
set(MW.AllM,'enable','off')
set(MW.EditM,'enable','off','string','','backgroundcolor',Inactive)
set(MW.MaxM,'enable','off','string','-')
%
% N controls ...
%
set(MW.N,'enable','off')
set(MW.AllN,'enable','off')
set(MW.EditN,'enable','off','string','','backgroundcolor',Inactive)
set(MW.MaxN,'enable','off','string','-')
%
% K controls ...
%
set(MW.K,'enable','off')
set(MW.AllK,'enable','off')
set(MW.EditK,'enable','off','string','','backgroundcolor',Inactive)
set(MW.MaxK,'enable','off','string','-')
%
% Plot buttons ...
%
set(MW.LoadData,'enable','off')
set(MW.DefVar,'enable','off')
set(MW.QuickV,'enable','off')
set(MW.Add2Plot,'enable','off')
%
% Options and gridview ...
%
set(UD.Options.Handles,'enable','off','visible','off','backgroundcolor',Inactive)
UD.Options.Act(:)=false;
update_option_positions(UD)

