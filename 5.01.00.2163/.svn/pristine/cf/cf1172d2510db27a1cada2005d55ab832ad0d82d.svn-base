function setzcoord(h,z)
%SETZ Sets the z coordinate.
%   SETZ(H,Z) sets the z coordinate of the objects referred to be array of
%   handles H to the specified value Z.
%
%   See also SET.

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

for i = 1:length(h)
    if ~ishandle(h)
        continue
    end
    switch get(h(i),'type')
        case {'line','surface'}
            zdata = get(h(i),'zdata'); % in case of Z surface on X vector, Y vector
            if isempty(zdata)
                zdata = get(h(i),'xdata'); % in case of lines in X,Y plane
            end
            set(h(i),'zdata',repmat(z,size(zdata)))
        case {'text','light'}
            coord = get(h(i),'position');
            coord(3) = z;
            set(h(i),'position',coord)
        case 'patch'
            coord = get(h(i),'vertices');
            coord(:,3) = z;
            set(h(i),'vertices',coord)
        otherwise
            % don't do anything for root, figure, axes, image, uicontrol, etc.
    end
end
