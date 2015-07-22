function out=vardiff(var1,var2,fid,formatflag,var1name,var2name)
%VARDIFF Determines the differences between two variables.
%   VARDIFF(var1,var2) lists the differences between the two
%   specified variables-files.
%
%   different=VARDIFF(var1,var2) returns the lowest appropriate
%   number in the following list
%     0   if the variables are identical (no NaNs found),
%     1   if the variables are identical (NaNs found),
%     2   if the variables are of different size, class or they are
%         structures with different fields.
%     2+N if the data is different in the Nth level, for matrices
%         this will be at most 3, for cell arrays and structures this
%         can become higher than 3. This basically indicates that you
%         need N subscripts to see the difference.
%   The function does not show the differences as text.
%
%   different=VARDIFF(var1,var2,fid) returns the number as described
%   above and writes the difference log to the file indicated by the
%   fid argument.
%
%   See also ISEQUAL.

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

if nargin<2
    error('Not enough input arguments.')
end
if nargin<3
    fid = double(nargout==0);
end
if nargin>3
    switch lower(formatflag)
        case 'html'
            br='<br>\n';
        otherwise
            br='\n';
    end
else
    br='\n';
end
if fid
    if nargin<6
        var2name = inputname(2);
    end
    if nargin<5
        var1name = inputname(1);
    end
    printdiff(fid,br,'init',var1name,var2name)
end

DiffFound=0;
try
    if isequal(var1,var2)
        myfprintf(fid,['The variables are identical and they don''t contain NaNs.' br]);
    else
        DiffFound=1+detailedcheck(var1,var2,fid,br,'');
        switch DiffFound
            case 1
                myfprintf(fid,['The variables are identical, but they do contain NaNs.' br])
        end
    end
catch
    error('An unexpected error occurred while comparing the files:\n%s',lasterr)
end
if nargout>0
    out=DiffFound;
end


% -----------------------------------------------------------------------
%  Function used for printing ...
% -----------------------------------------------------------------------
function printdiff(fid,br,pflag,varargin)
persistent var1 var2
switch pflag
    case 'init'  % varname1, varname2
        myfprintf(fid,['Comparing variables ...' br]);
        s1=varargin{1};
        s2=varargin{2};
        myfprintf(fid,'Variable 1: ');
        if isempty(s1)
            var1='VAR1';
            myfprintf(fid,['<expression> indicated as VAR1' br]);
        else
            var1=s1;
            myfprintf(fid,['%s' br],var1);
        end
        myfprintf(fid,'Variable 2: ');
        if isempty(s2)
            var2='VAR2';
            myfprintf(fid,['<expression> indicated as VAR2' br br]);
        else
            var2=s2;
            myfprintf(fid,['%s' br br],var2);
        end
    case 'class'  % classvar1, classvar2, subscript string
        cls1=varargin{1};
        cls2=varargin{2};
        substr=varargin{3};
        myfprintf(fid,['Class (%s) of %s%s differs from class (%s) of %s%s.' br],cls1,var1,substr,cls2,var2,substr);
    case 'size'   % classvar1, classvar2, subscript string
        sz1=varargin{1};
        sz2=varargin{2};
        substr=varargin{3};
        myfprintf(fid,'Size of %s%s is [%i',var1,substr,sz1(1));
        myfprintf(fid,' %i',sz1(2:end));
        myfprintf(fid,'], but size of %s%s is [%i',var2,substr,sz2(1));
        myfprintf(fid,' %i',sz2(2:end));
        myfprintf(fid,['].' br]);
    case 'data'  % subscript string
        substr=varargin{1};
        myfprintf(fid,['Data of %s%s differs from data contained in %s%s.' br],var1,substr,var2,substr);
    case 'fieldnames'  % fieldnames1,fieldnames2,subscript string
        fn1=varargin{1};
        fn2=varargin{2};
        substr=varargin{3};
        sfn1=setdiff(fn1,fn2);
        if ~isempty(sfn1)
            myfprintf(fid,['%s%s contains the following fields not part of %s%s:' br],var1,substr,var2,substr);
            myfprintf(fid,['  %s' br],sfn1{:});
        end
        sfn2=setdiff(fn2,fn1);
        if ~isempty(sfn2)
            myfprintf(fid,['%s%s contains the following fields not part of %s%s:' br],var2,substr,var1,substr);
            myfprintf(fid,['  %s' br],sfn2{:});
        end
        if isempty(sfn1) && isempty(sfn2)
            myfprintf(fid,['The order of fields in %s%s differs from those in %s%s:' br],var2,substr,var1,substr);
            sfn1 = char(fn1);
            sfn2 = char(fn2);
            [f1,i1] = sort(fn1);
            [f2,i2] = sort(fn2);
            [ii,r1] = sort(i1);
            for i = 1:length(fn1)
               if ~strcmp(fn1{i},fn2{i})
                  myfprintf(fid,['  %2i %s - %2i %s' br],i1(r1(i)),sfn1(i,:),i2(r1(i)),sfn2(i,:));
               end
            end
        end
end


% -----------------------------------------------------------------------
%  Function used for recursive checking ...
% -----------------------------------------------------------------------
function DiffFound=detailedcheck(s1,s2,fid,br,substr)
DiffFound=0;
if ~isequal(class(s1),class(s2))  % different classes?
    DiffFound=1;
    printdiff(fid,br,'class',class(s1),class(s2),substr);
elseif ~isequal(size(s1),size(s2))  % different size?
    DiffFound=1;
    printdiff(fid,br,'size',size(s1),size(s2),substr);
elseif iscell(s1)  % & s2 is also cell! if cell -> check per element
    for i=1:numel(s1)  % s2 has same size!
        Diff=detailedcheck(s1{i},s2{i},fid,br,sprintf('%s{%i}',substr,i));
        if Diff
            if ~DiffFound
                DiffFound=Diff;
            else
                DiffFound=min(Diff,DiffFound);
            end
        end
    end
    if DiffFound
        DiffFound=DiffFound+1;
    end
elseif isstruct(s1) || isobject(s1)
    if isobject(s1)  % in case of objects convert into structures for detailed check
        s1=struct(s1); s2=struct(s2);
    end
    fn1=fieldnames(s1);
    fn2=fieldnames(s2);
    nf=length(fn1);
    if ~isequal(fn1,fn2)  % fieldnames the same?
        DiffFound=1;
        printdiff(fid,br,'fieldnames',fn1,fn2,substr);
        return
    end
    s1=struct2cell(s1);
    s2=struct2cell(s2);
    j=0;
    for i=1:numel(s1)  % s2 has same size! (array size is the same and fields are the same)
        j=j+1;
        if j>nf
            j=1;
        end
        if numel(s1)~=nf
            Nsubstr=sprintf('%s(%i).%s',substr,(i-j)/nf+1,fn1{j});
        else
            Nsubstr=sprintf('%s.%s',substr,fn1{j});
        end
        Diff=detailedcheck(s1{i},s2{i},fid,br,Nsubstr);
        if Diff
            if ~DiffFound
                DiffFound=Diff;
            else
                DiffFound=min(Diff,DiffFound);
            end
        end
    end
    if DiffFound
        DiffFound=DiffFound+1;
    end
else  % some numeric type of equal size
    if isempty(s1)  % same size, numeric, empty -> no difference
        return
    elseif isa(s1,'double')
        NaNorEqual=(isnan(s1) & isnan(s2)) | (s1==s2);
        DiffFound=~all(NaNorEqual(:));
    else
        DiffFound=~isequal(s1,s2);
    end
    if DiffFound
        printdiff(fid,br,'data',substr);
    end
end

function myfprintf(fid,varargin)
if fid
    fprintf(fid,varargin{:});
end
