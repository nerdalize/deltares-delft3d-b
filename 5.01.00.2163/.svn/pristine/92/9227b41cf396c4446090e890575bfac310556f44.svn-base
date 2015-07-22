function [Sout,Ssep]=multiline(Sin,varargin)
%MULTILINE Converts a string containing LineFeeds to a char matrix.
%   STRMAT=MULTILINE(S,C) splits the input string S at the characters
%   listed in the char vector C. Each section forms a line of the char
%   matrix STRMAT (left aligned). Enter \n for char(10):linefeed and \t for
%   char(9):tab. If C is not specified, the string S is split at linefeeds
%   (i.e. the default value of C is char(10).
%
%   CELSTR=MULTILINE(...,'cell') converts the input string into a cell
%   string instead of a char matrix.
%
%   Example:
%      Str=multiline(sprintf('%i\n',1:10:31))
%      % gives the 4 x 2 char matrix
%
%      Str = 1
%            11
%            21
%            31
%
%   See also CELLSTR, STR2MAT.

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

cArray=char(10);
cellOutput=0;
if nargin>1
    Inp=varargin;
    if isequal('cell',lower(Inp{end}))
        cellOutput=1;
        Inp(end)=[];
    end
    if length(Inp)>1
        error('Too many input arguments.')
    elseif length(Inp)==1
        cArray=Inp{1};
        cArray=strrep(cArray,'\t',char(9));
        cArray=strrep(cArray,'\n',char(10));
    end
end
if nargin<1 | ~ischar(Sin)
    error('Invalid input argument.')
elseif isempty(Sin)
    if cellOutput
        Sout={};
    else
        Sout='';
    end
    if nargout>1
        Ssep='';
    end
elseif ndims(Sin)~=2 | min(size(Sin))~=1
    error('Invalid input argument.')
else
    LineFeed=find(ismember(Sin,cArray));
    if nargout>1
        Ssep=Sin(LineFeed);
    end
    Start=[1 LineFeed+1];
    End=[LineFeed-1 length(Sin)];
    if cellOutput
        Sout=cell(length(Start),1);
        for k=1:length(Start)
            Sout{k}=deblank(Sin(Start(k):End(k)));
        end
    else
        maxStrL=max(End-Start)+1;
        Sout=repmat(' ',length(Start),maxStrL);
        for k=1:length(Start)
            Sout(k,1:(End(k)-Start(k)+1))=Sin(Start(k):End(k));
        end
    end
end
