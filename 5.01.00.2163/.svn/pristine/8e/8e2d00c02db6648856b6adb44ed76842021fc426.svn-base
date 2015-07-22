function hNew=genfaces(hOld,Ops,Parent,Val,X,Y,Z)
%GENFACES Generic plot routine for patches plot.

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

if nargin==4
    if isempty(hOld) || ~ishandle(hOld)
        error('Invalid handle')
    else
        hNew=hOld;
        set(hNew,'facevertexcdata',Val(:))
    end
else
    %
    if nargin>6
        %
        % ---> X, Y and Z
        %
        faces=reshape(1:numel(Z),size(Z));
        if isequal(size(Z),size(Val)+1)
            faces=faces(1:end-1,1:end-1);
            faces=faces(:);
            xv=[X(:) Y(:) Z(:)];
            fv=[faces faces+1 faces+size(X,1)+1 faces+size(X,1)];
        else
            faces=faces(:,1:end-1);
            faces=faces(:);
            X0=X(1:end-1,:);
            Y0=Y(1:end-1,:);
            X1=X(2:end,:);
            Y1=Y(2:end,:);
            xv=[X0(:) Y0(:) Z(:); X1(:) Y1(:) Z(:)];
            fv=[faces faces+numel(X0) faces+size(X0,1)+numel(X0) faces+size(X0,1)];
        end
    elseif ndims(X)==4
        %
        % ---> X=XYZ and Y=TRI
        %
        xv = squeeze(X(1,:,1,:));
        fv = Y;
    else
        %
        % ---> X and Y
        %
        faces=reshape(1:numel(Y),size(Y));
        if isequal(size(Y),size(Val)+1)
            faces=faces(1:end-1,1:end-1);
            faces=faces(:);
            xv=[X(:) Y(:)];
            fv=[faces faces+1 faces+size(X,1)+1 faces+size(X,1)];
        else
            faces=faces(:,1:end-1);
            faces=faces(:);
            X0=X(1:end-1,:);
            X1=X(2:end,:);
            xv=[X0(:) Y(:); X1(:) Y(:)];
            fv=[faces faces+numel(X0) faces+size(X0,1)+numel(X0) faces+size(X0,1)];
        end
    end
    cv=Val(:);
    %
    % set face values of patches with undefined corners to NaN
    %
    for d = 1:size(xv,2)
        coord = xv(:,d);
        cv(any(isnan(coord(fv)),2))=NaN;
    end
    %
    if isfield(Ops,'Thresholds') && ~strcmp(Ops.Thresholds,'none')
        Thresholds = Ops.Thresholds;
    else
        Thresholds = [];
    end
    %
    if any(~ishandle(hOld)) || ~isempty(Thresholds)
        delete(hOld(ishandle(hOld)))
        hOld = [];
    end
    %
    if isempty(hOld)
        if isempty(Ops.colourmap)
            fv=fv(~isnan(cv) & cv~=0,:);
            hNew=patch('vertices',xv, ...
                'faces',fv, ...
                'parent',Parent, ...
                'edgecolor','none', ...
                'facecolor',Ops.colour);
        elseif isempty(Thresholds)
            hNew=patch('vertices',xv, ...
                'faces',fv, ...
                'facevertexcdata',cv, ...
                'parent',Parent, ...
                'edgecolor','none', ...
                'facecolor','flat');
        else
            nThresholds = length(Thresholds);
            Thresholds(end+1) = inf;
            hNew=zeros(1,nThresholds);
            for i = 1:nThresholds
                iclass = cv>=Thresholds(i) & cv<Thresholds(i+1);
                if any(iclass)
                    facecolor = 'flat';
                else
                    facecolor = 'none';
                end
                hNew(i)=patch('vertices',xv, ...
                    'faces',fv(iclass,:), ...
                    'facevertexcdata',0*cv(iclass)+i, ...
                    'parent',Parent, ...
                    'edgecolor','none', ...
                    'facecolor',facecolor);
            end
        end
        if strcmp(Ops.presentationtype,'patches with lines')
            set(hNew,'edgecolor',Ops.colour)
        end
    else
        hNew=hOld;
        if isempty(Ops.colourmap)
            fv=fv(~isnan(cv) & cv~=0,:);
            set(hNew,'vertices',xv, ...
                'faces',fv)
        else
            set(hNew,'vertices',xv, ...
                'faces',fv, ...
                'facevertexcdata',cv)
        end
    end
end
