function H=qp_uifigure(Name,closecom,tag,pos,callbackfcn)
%QP_UIFIGURE Create a new empty dialog figure.

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

Inactive=qp_settings('UIInActiveColor');%[0.5 0.5 1];%
%
if nargin<5
    callbackfcn='d3d_qp';
end
%
uicontrolfont = qp_fontsettings('DefaultUicontrolFont');
%
%Force onscreen:
%MATLAB 6: movegui(H,'onscreen')
screensize=get(0,'screensize');
pos(1:2)=max(pos(1:2),0);
pos(1:2)=min(pos(1:2),screensize(3:4)-pos(3:4)-[0 70]);
% -[0 70] added such that toolbar, menu and window title will be on screen
% also in R13 compiled mode
%
if ~isempty(closecom)
    closecom=sprintf('%s %s',callbackfcn,closecom);
end
%
H = figure('Visible','off', ...
    'DefaultUicontrolBackgroundColor',Inactive, ...
    'DefaultUicontrolForegroundColor',qp_settings('UIForeGroundColor'), ...
    uicontrolfont, ...
    'Units','pixels', ...
    'Color',Inactive, ...
    'IntegerHandle','off', ...
    'MenuBar','none', ...
    'Name',Name, ...
    'CloseRequestFcn',closecom, ...
    'NumberTitle','off', ...
    'Resize','off', ...
    'Position',pos, ...
    'Handlevisibility','off', ...
    'Tag',tag);
setappdata(H,'WL_UserInterface',1)
if matlabversionnumber >= 7
    set(H,'WindowStyle','normal','DockControls','off')
end
