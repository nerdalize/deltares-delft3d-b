function update_option_positions(UD,newTop)
%UPDATE_OPTION_POSITIONS Update vertical position of plot option controls.

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

if nargin>1
    %
    % called in case of a Plot Options Figure Resize or Dock Command
    %
    P = UD.Options.ActPos;
    P(:,2) = P(:,2)-P(:,4)+20+newTop-UD.Options.Top;
    if isempty(P)
        minP = 999;
    else
        minP = min(P(:,2));
    end
    UD.Options.Top = newTop;
else
    %
    % called from qp_interface_update_options
    %
    UD.Options.Act = ~strcmp(get(UD.Options.Handles,'enable'),'off') | qp_settings('showinactiveopt');  % <- for debugging useful: show all options
    %set(UD.Options.Handles,'enable','on')
    set(UD.Options.Handles(~UD.Options.Act),'visible','off');
    P = UD.Options.Pos(UD.Options.Act,:);
    if isempty(P)
        minP=999;
        %
        % Fix needed for compiled code ...
        %
        P=zeros(0,4);
    else
        [f,i,j]=unique(P(:,2));
        P(:,2)=UD.Options.Top-25*(j(1)-j);
        minP=min(P(:,2));
    end
end
%
if minP<10
    offset=get(UD.Options.Slider,'value');
    offset=min(max(offset,minP-10),0);
    set(UD.Options.Slider,'min',minP-10,'max',0,'value',offset,'enable','on');
else
    offset=0;
    set(UD.Options.Slider,'enable','off','value',0)
end
%
P(:,2)=P(:,2)+P(:,4)-20;
UD.Options.ActPos=P;
P(:,2)=P(:,2)-offset;
P=num2cell(P,2);
set(UD.Options.Handles(UD.Options.Act)',{'position'},P,'visible','on');
%
setappdata(UD.MainWin.Fig,'QPHandles',UD)
