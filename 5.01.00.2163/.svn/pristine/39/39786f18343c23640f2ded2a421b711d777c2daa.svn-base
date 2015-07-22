function l=qp_vector(vectorstyle,x,y,z,xc,yc,zc,varargin)
%QP_VECTOR Wrapper for QUIVER and QUIVER3.

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

compat7={};
warnstate=[];
if matlabversionnumber>=7
    compat7={'v6'};
    %
    % Recent versions produce warning if 'v6' is used.
    % Try to suppress this warning.
    %
    try
        warnstate = warning('query','MATLAB:quiver:DeprecatedV6Argument');
        warnstate(2) = warning('query','MATLAB:quiver3:DeprecatedV6Argument');
        warning('off','MATLAB:quiver:DeprecatedV6Argument')
        warning('off','MATLAB:quiver3:DeprecatedV6Argument')
    catch
    end
end

%
% Make sure that we don't error out this routine before the 'v6' warning
% has been set back to its original setting.
%
e = '';
try
    switch vectorstyle
        case 'rooted arrow'
            if isempty(z)
                l=quiver(compat7{:},x,y,xc,yc,varargin{:});
            else
                l=quiver3(compat7{:},x,y,z,xc,yc,zc,varargin{:});
            end
        case 'centered arrow'
            if isempty(z)
                l=quiver(compat7{:},x-xc/2,y-yc/2,xc,yc,varargin{:});
            else
                l=quiver3(compat7{:},x-xc/2,y-yc/2,z-zc/2,xc,yc,zc,varargin{:});
            end
        case 'rooted line'
            if isempty(z)
                l=quiver(compat7{:},x,y,xc,yc,varargin{:},'o');
            else
                l=quiver3(compat7{:},x,y,z,xc,yc,zc,varargin{:},'o');
            end
            set(l(end),'marker','.')
    end
catch
    e = lasterr;
end

%
% Set 'v6' warning back to original setting.
%
if ~isempty(warnstate)
    warning(warnstate); %#ok<WNTAG>
end

if ~isempty(e)
    error(e);
end
