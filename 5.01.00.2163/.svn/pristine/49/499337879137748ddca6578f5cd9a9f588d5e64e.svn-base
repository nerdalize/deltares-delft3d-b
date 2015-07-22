function [xo,yo,po,mo,to,lo]=int_lntri(xi,yi,TRI,X,Y)
%INT_LNTRI Intersection of line and triangular mesh.
%   [XCROSS,YCROSS] = INT_LNTRI(XLINE,YLINE,TRI,X,Y)
%   Computes the points where the line (XLINE,YLINE)
%   crosses edges of a triangular mesh (TRI,X,Y).
%
%   [XCROSS,YCROSS,IND,WGHT] = ...
%   Returns also indices IND and weights WGHT to compute values at the
%   points XCROSS and YCROSS using linear interpolation of values V at X,Y
%   using VCROSS = SUM(V(IND).*WGHT,2).
%
%   [XCROSS,YCROSS,IND,WGHT,INDTRI] = ...
%   Returns also the numbers INDTRI of the triangles in which each line
%   segment between crossings is located. INDTRI will be NaN for all line
%   segments that lie outside all triangles.

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

TRI  = double(TRI); % fix in case TRI is provided as int32 (not supported by tsearch)
ntri = size(TRI,1);
edge = TRI(:,[1 1 2 2 3 3]);
edge = reshape(edge,[ntri*3 2]);
tri = repmat((1:ntri)',3,1);

Xvec = X(:);
Yvec = Y(:);

Temp=Xvec(edge);
dX1=diff(Temp,1,2);
X1=Temp(:,1);

Temp=Yvec(edge);
dY1=diff(Temp,1,2);
Y1=Temp(:,1);

N=length(xi);

cN = cell(1,N);
mo=cN;
po=cN;
to=cN;
xo=cN;
yo=cN;
lo=cN;

T=NaN;
First=1;
for i=1:N
    if isnan(T)
        PO = [1 1 1];
        MO = [NaN NaN NaN];
    else
        PO = TRI(T,:);
        MO = trival(X(PO),Y(PO),xi(i),yi(i));
    end
    if i==N
        break
    end

    dxi=xi(i)-xi(i+1);
    dyi=yi(i)-yi(i+1);

    %
    % Determine the mu and lambda coefficients for which the vector equality
    % [xi1]+lambda*[dxi] == [X1]+mu*[dX2]
    % [yi1]        [dyi]    [Y1]    [dY2]
    % is satisfied.
    %
    Det1 = dX1.*dyi-dY1.*dxi;
    Det1(Det1==0) = NaN;
    mu = (dyi*(xi(i)-X1)-dxi*(yi(i)-Y1)) ./Det1;
    lambda = (-dY1.*(xi(i)-X1)+dX1.*(yi(i)-Y1))./Det1;
    %
    % Determine crossing line segments. This can be done using a symmetric
    % one-liner
    %
    % ln1=(lambda>=0) & (lambda<1) & (mu>=0) & (mu<=1);
    %
    % or the following two line approach which seems to be almost a factor 2
    % faster if the xi,yi line segment are much longer than the edges of the
    % triangular mesh.
    %
    ln1 = mu>=0;
    ln1(ln1) = mu(ln1)<=1;
    ln1(ln1) = (lambda(ln1)>=0) & (lambda(ln1)<1);
    ncross=sum(ln1);
    if ncross==0
        l=0;
        mo{i}=MO;
        po{i}=PO;
        triangles=T;
        xo{i}=xi(i);
        yo{i}=yi(i);
    else
        l=cat(1,0,lambda(ln1));
        mo{i}=cat(1,MO,[1-mu(ln1) mu(ln1) zeros(ncross,1)]);
        po{i}=cat(1,PO,edge(ln1,[1 2 2]));
        triangles=cat(1,T,tri(ln1));
        xo{i}=cat(1,xi(i),X1(ln1)+mu(ln1).*dX1(ln1));
        yo{i}=cat(1,yi(i),Y1(ln1)+mu(ln1).*dY1(ln1));
    end

    [l,ind,rev]=unique(l); % includes sort !
    ncross=length(ind);
    to{i}=zeros(ncross,1);
    to{i}(1)=T;
    mo{i}=mo{i}(ind,:);
    po{i}=po{i}(ind,:);
    xo{i}=xo{i}(ind);
    yo{i}=yo{i}(ind);
    lo{i}=i+l;

    for j=2:ncross
        edges = rev==j;
        neighbors = triangles(edges);
        if First
            %
            % Determine whether the starting point is in a triangle or not.
            %
            xx = (xo{i}(j)+xo{i}(j-1))/2;
            yy = (yo{i}(j)+yo{i}(j-1))/2;
            if matlabversionnumber>=7.14
                T = tsearchn([Xvec Yvec],TRI(neighbors,:),[xx yy]);
            else
                T = tsearch(Xvec,Yvec,TRI(neighbors,:),xx,yy);
            end
            if ~isnan(T)
                T = neighbors(T);
                PO = TRI(T,:);
                for ii=i:-1:1
                    to{ii}(1) = T;
                    po{ii}(1,:) = PO;
                    mo{ii}(1,:) = trival(X(PO),Y(PO),xi(ii),yi(ii));
                end
            end
            First=0;
        end
        T2 = neighbors(neighbors~=T);
        if isempty(T2)
            T2 = NaN;
        end
        if length(T2)>1
            %
            % rare exception: point of crossing is on the edge of multiple
            % triangles (not a simple boundary going from one triangle to the
            % next one). Select appropriate one based on direction of next
            % line segment. Note that TSEARCH will choose one of the triangles
            % if the line segment follows the edge of two triangles.
            %
            if j<ncross
                xx = (xo{i}(j)+xo{i}(j+1))/2;
                yy = (yo{i}(j)+yo{i}(j+1))/2;
            else
                xx = (xo{i}(j)+xi(i+1))/2;
                yy = (yo{i}(j)+yi(i+1))/2;
            end
            %
            warnstate = warning('query','MATLAB:tsearch:DeprecatedFunction');
            warning('off','MATLAB:tsearch:DeprecatedFunction')
            %
            if matlabversionnumber>=7.14
                Ti = tsearchn([Xvec Yvec],TRI(T2,:),[xx yy]);
            else
                Ti = tsearch(Xvec,Yvec,TRI(T2,:),xx,yy);
            end
            %
            warning(warnstate);
            %
            if isnan(Ti)
                T2 = NaN;
            else
                T2 = T2(Ti);
            end
        end
        if 0
            fprintf('at %g,%g ',xo{i}(j),yo{i}(j))
            if isnan(T)
                fprintf('going into triangle %i\n',T2)
            elseif isnan(T2)
                fprintf('going out of triangle %i\n',T)
            else
                fprintf('going from triangle %i to %i\n',T,T2)
            end
        end
        T = T2;
        to{i}(j) = T;
    end
end

po{N} = PO;
mo{N} = MO;
to{N}=[];
xo{N}=xi(N);
yo{N}=yi(N);
lo{N}=N;
if First
    %
    % Rare exception: no crossing at all. So, all points are inside one
    % triangle or outside all of them. Use the average coordinates to allow
    % for the even rarer cases that the last point (or any other point) is
    % on the edge of a triangle.
    %
    xx = mean(xi);
    yy = mean(yi);
    %
    % Note that this is the only case in which we have to search among ALL
    % triangles. Since we have to look for just one point for a rare case,
    % it is not worth to investigate other methods of searching.
    %
    if matlabversionnumber>=7.14
        Ti = tsearchn([Xvec Yvec],TRI,[xx yy]);
    else
        Ti = tsearch(Xvec,Yvec,TRI,xx,yy);
    end
    if ~isnan(T)
        PO = TRI(T,:);
        for ii=N:-1:1
            if ii<N
                to{ii}(1) = T;
            end
            po{ii}(1,:) = PO;
            mo{ii}(1,:) = trival(X(PO),Y(PO),xi(ii),yi(ii));
        end
    end
end

mo=cat(1,mo{:});
po=cat(1,po{:});
to=cat(1,to{:});
xo=cat(1,xo{:});
yo=cat(1,yo{:});
lo=cat(1,lo{:});
%
% Coordinates could also have been determined at the end of this routine
% using the two statements given below. However, then we would loose corner
% points that are not part of any triangle!
%
% xo=sum(mo.*X(po),2);
% yo=sum(mo.*Y(po),2);


function abc=trival(xtri,ytri,xi,yi)
%
% Determine linear interpolation coefficients for a point in a triangle.
%
X1 = xtri(1);
X2 = xtri(2);
X3 = xtri(3);
Y1 = ytri(1);
Y2 = ytri(2);
Y3 = ytri(3);
dX12 = X1-X2;
dY12 = Y1-Y2;
dX23 = X2-X3;
dY23 = Y2-Y3;
dX31 = X3-X1;
dY31 = Y3-Y1;
Det=dY12.*dX31-dX12.*dY31;
a = Det + dY23.*(xi-X1) - dX23.*(yi-Y1);
b = Det + dY31.*(xi-X2) - dX31.*(yi-Y2);
c = Det + dY12.*(xi-X3) - dX12.*(yi-Y3); % Det-a-b
abc = [a b c]./Det;
