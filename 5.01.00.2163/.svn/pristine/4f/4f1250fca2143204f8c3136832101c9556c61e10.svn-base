function s1 = deblank2(s)
%DEBLANK2 Remove leading and trailing blanks.
%   DEBLANK2(S) removes trailing blanks from string S.

%   Based on DEBLANK by L. Shure, 6-17-92.
%                       Copyright (c) 1984-98 by The MathWorks, Inc.

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

if ~isempty(s) & ~ischar(s) & ~iscellstr(s)
    warning('Input must be a string.')
end

if isempty(s)
    s1 = s([]);
else
    noncell=0;
    if ~iscellstr(s)
        s={s};
        noncell=1;
    end
    s1=s;
    for i=1:length(s1)
        sloc=s{i};
        % remove trailing blanks
        [r,c] = find(sloc ~= ' ' & sloc ~= 0);
        if isempty(c)
            s1{i} = sloc([]);
        else
            s1{i} = sloc(:,min(c):max(c));
        end
    end
    if noncell
        s1=s1{1};
    end
end
