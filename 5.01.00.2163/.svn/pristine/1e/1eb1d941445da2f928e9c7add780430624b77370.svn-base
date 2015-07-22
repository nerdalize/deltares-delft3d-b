function Str=var2str(X)
%VAR2STR Generic "display" function with string output.
%   STRING = VAR2STR(VARIABLE) works similar to DISP(VARIABLE) but returns
%   the resulting text as string.

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

switch class(X)
    case 'struct'
        flds=fieldnames(X);
        if isempty(flds)
            Str={[LocalSize(X) ' struct array without any fields']};
        elseif numel(X)==1
            cflds=char(flds);
            nflds=length(flds);
            Str=cell(nflds+1,1);
            Str{1}=[LocalSize(X) ' struct array with fields:'];
            for i=1:nflds
                Y=X.(flds{i});
                YStr = [LocalSize(Y) ' ' class(Y) ' array'];
                switch class(Y)
                    case 'char'
                        if ndims(Y)==2 && size(Y,1)==1
                            YStr=['''' Y ''''];
                        end
                    case 'double'
                        if isempty(Y)
                            YStr='[]';
                        elseif numel(Y)==1
                            YStr=num2str(Y);
                        end
                    case 'cell'
                        if isempty(Y)
                            YStr='{}';
                        end
                end
                Str{i+1}=sprintf('%s = %s',cflds(i,:),YStr);
            end
        else
            Str=cat(1,{[LocalSize(X) ' struct array with fields:']},fieldnames(X));
        end
    case {'org.apache.xerces.dom.DeferredDocumentImpl'
            'org.apache.xerces.dom.DeferredElementImpl'}
        atts={};
        XA=X.getAttributes;
        if ~isempty(XA)
            for i=XA.getLength:-1:1
                atts{i,1}=char(XA.item(i-1));
            end
        end
        tags={};
        for i=X.getLength:-1:1
            tags{i,1}=char(X.item(i-1).getNodeName);
        end
        if ~isempty(atts)
            Str=cat(1,{'XML object with attributes:'},atts);
        else
            Str={};
        end
        if isempty(tags)
            if ~isempty(Str)
                Str=cat(1,Str,{'and no subtags'});
            else
                Str={'XML object with neither attributes nor subtags'};
            end
        elseif isequal(tags,{'#text'})
            Str1={char(X.item(0).getData)};
            if ~isempty(Str)
                Str=cat(1,Str,Str1);
            else
                Str={'XML object'
                    Str1{:}};
            end
        else
            if ~isempty(Str)
                Str=cat(1,Str,{'and subtags'},tags);
            else
                Str=cat(1,{'XML object with subtags:'},tags);
            end
        end
    case 'char'
        if ndims(X)<=2
            Str=X;
        else
            Str=[LocalSize(X) ' char array'];
        end
    case {'single','double','logical', ...
          'int8','int16','int32','int64', ...
          'uint8','uint16','uint32','uint64'}
        if ndims(X)<=2 && ~isempty(X)
            if numel(X)<100
                Str=num2str(X);
            else
                Str=[LocalSize(X) ' ' class(X) ' array'];
            end
        else
            Str=[LocalSize(X) ' ' class(X) ' array'];
        end
    case 'cell'
        Str=[LocalSize(X) ' cell array'];
        if iscellstr(X) && sum(size(X)>1)<2 && ~isempty(X)
            Str={[Str ' containing']};
            Str=cat(1,Str,X(:));
        end
    case 'org.apache.xerces.dom.DeferredTextImpl'
        Str={[LocalSize(X) ' ' class(X) ' array']
            char(X.getData)};
    otherwise
        Str=[LocalSize(X) ' ' class(X) ' array'];
end


function Str=LocalSize(X)
Sz=size(X);
Str=[sprintf('%i',Sz(1)) sprintf('x%i',Sz(2:end))];
