function lim=limits(ax,limtype);
%LIMITS Determine real x,y,z,c limits.
%   Lim=LIMITS(Axes,LimType)
%   returns the real limits of the objects
%   contained in the axes object, where LimType
%   can be 'clim','xlim','ylim' or 'zlim'.
%
%   Lim=LIMITS(Handles,LimType)
%   returns the real limits based on only the
%   specified objects.

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

if length(ax)==1 & isequal(get(ax,'type'),'axes')
    ch=get(ax,'children');
    xlab=get(ax,'xlabel');
    ylab=get(ax,'ylabel');
    zlab=get(ax,'zlabel');
    titl=get(ax,'title');
    ch=setdiff(ch,[xlab ylab zlab titl]);
else
    ch=ax;
end
ch=ch(:);
%
if isempty(ch)
    lim=[0 1];
else
    lim=[inf -inf];
end
%
i=1;
while i<=length(ch)
    if strcmp(get(ch(i),'type'),'hggroup')
        chd=get(ch(i),'children');
        ch(i,:)=[];
        ch=cat(1,ch,chd);
    else
        i=i+1;
    end
end
%
switch lower(limtype)
    case {'c','cl','cli','clim'}
        for i=1:length(ch)
            switch get(ch(i),'type')
                case {'surface','patch','image'}
                    c=get(ch(i),'cdata');
                    cmap=get(ch(i),'cdatamapping');
                    if (size(c,3)==1) & strcmp(cmap,'scaled') & ~isempty(c)
                        lim(1)=min(lim(1),min(c(:)));
                        lim(2)=max(lim(2),max(c(:)));
                    end
            end
        end
    case {'x','xl','xli','xlim'}
        for i=1:length(ch)
            switch get(ch(i),'type')
                case {'surface','patch','line','image'}
                    x=get(ch(i),'xdata');
                    if ~isempty(x)
                        lim(1)=min(lim(1),min(x(:)));
                        lim(2)=max(lim(2),max(x(:)));
                    end
                case 'text'
                    p=get(ch(i),'position');
                    lim(1)=min(lim(1),p(1));
                    lim(2)=max(lim(2),p(1));
            end
        end
    case {'y','yl','yli','ylim'}
        for i=1:length(ch)
            switch get(ch(i),'type')
                case {'surface','patch','line','image'}
                    y=get(ch(i),'ydata');
                    if ~isempty(y)
                        lim(1)=min(lim(1),min(y(:)));
                        lim(2)=max(lim(2),max(y(:)));
                    end
                case 'text'
                    p=get(ch(i),'position');
                    lim(1)=min(lim(1),p(2));
                    lim(2)=max(lim(2),p(2));
            end
        end
    case {'z','zl','zli','zlim'}
        for i=1:length(ch)
            switch get(ch(i),'type')
                case {'surface','patch','line'}
                    z=get(ch(i),'zdata');
                    if ~isempty(z),
                        lim(1)=min(lim(1),min(z(:)));
                        lim(2)=max(lim(2),max(z(:)));
                    end
                case 'text'
                    p=get(ch(i),'position');
                    lim(1)=min(lim(1),p(3));
                    lim(2)=max(lim(2),p(3));
            end
        end
end
