function d0=pathdistance(varargin)
%PATHDISTANCE Computes the distance along a path.
%   Computes the distance along the path from the first
%   point for every point on the path.
%
%   Distance=PATHDISTANCE(XCoord,YCoord,ZCoord)
%   Distance=PATHDISTANCE(XCoord,YCoord)
%   Distance=PATHDISTANCE(Coord)
%
%   NaNs are skipped in the computation of the path length.

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

igeo=0;
iopt=0;
x0=[];
y0=[];
z0=[];
for i=1:length(varargin)
    if ischar(varargin{i})
        switch varargin{i}
            case {'geographic','spherical','deg'}
                igeo=1;
        end
    elseif isempty(x0)
        x0 = varargin{i};
        iopt=1;
    elseif isempty(y0)
        y0 = varargin{i};
        iopt=2;
    elseif isempty(z0)
        z0 = varargin{i};
        iopt=3;
    else
        error('Too many numerical arguments.')
    end
end

d0=NaN(size(x0));

% iopt=1 : 1D
% iopt=2 : 2D
% iopt=3 : 3D

if iopt==1
    iprev=find(~isnan(x0), 1 );
    d0(iprev)=0;
    for i=(iprev+1):length(x0)
        if isnan(x0(i))
            d0(i)=NaN;
        else
            d0(i)=d0(iprev)+abs(x0(i)-x0(iprev));
            iprev=i;
        end
    end
elseif iopt==2
    iprev=find(~isnan(x0) & ~isnan(y0), 1 );
    d0(iprev)=0;
    for i=(iprev+1):length(x0)
        if isnan(x0(i)) || isnan(y0(i))
            d0(i)=NaN;
        else
            if igeo
                d0(i)=d0(iprev)+geodist(x0(iprev), y0(iprev), x0(i), y0(i));
            else
                d0(i)=d0(iprev)+sqrt((x0(i)-x0(iprev))^2+(y0(i)-y0(iprev))^2);
            end
            iprev=i;
        end
    end
elseif iopt==3
    iprev=find(~isnan(x0) & ~isnan(y0) & ~isnan(z0), 1 );
    d0(iprev)=0;
    for i=(iprev+1):length(x0)
        if isnan(x0(i)) || isnan(y0(i)) || isnan(z0(i))
            d0(i)=NaN;
        else
            if igeo
                d0(i)=d0(iprev)+sqrt(geodist(x0(iprev), y0(iprev), x0(i), y0(i))^2+(z0(i)-z0(iprev))^2);
            else
                d0(i)=d0(iprev)+sqrt((x0(i)-x0(iprev))^2+(y0(i)-y0(iprev))^2+(z0(i)-z0(iprev))^2);
            end
            iprev=i;
        end
    end
end
