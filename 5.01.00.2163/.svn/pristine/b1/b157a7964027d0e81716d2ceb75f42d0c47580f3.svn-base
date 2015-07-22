function [hNew,Thresholds,Param]=qp_plot_polyl(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_POLYL Plot function of QuickPlot for polyline data sets.

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

T_=1; ST_=2; M_=3; N_=4; K_=5;

FirstFrame=Param.FirstFrame;
PName=Param.PName;
TStr=Param.TStr;
Selected=Param.Selected;
multiple=Param.multiple;
NVal=Param.NVal;

DimFlag=Props.DimFlag;
Thresholds=[];

switch NVal
    case 0
        if strcmp(Ops.facecolour,'none')
            if ishandle(hNew)
                set(hNew,'xdata',data.X, ...
                    'ydata',data.Y);
            else
                hNew=line(data.X,data.Y, ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
                set(Parent,'layer','top')
            end
        else
            if ~FirstFrame
                delete(hNew)
            end
            vNaN=isnan(data.X);
            if any(vNaN)
                bs=findseries(~vNaN);
            elseif isempty(vNaN)
                bs=zeros(0,2);
            else
                bs=[1 length(vNaN)];
            end
            for i=1:size(bs,1)
                if data.X(bs(i,1))==data.X(bs(i,2)) && ...
                        data.Y(bs(i,1))==data.Y(bs(i,2))
                    hNew(i)=patch(data.X(bs(i,1):bs(i,2)), ...
                        data.Y(bs(i,1):bs(i,2)), ...
                        1, ...
                        'edgecolor',Ops.colour, ...
                        'facecolor',Ops.facecolour, ...
                        'linestyle',Ops.linestyle, ...
                        'linewidth',Ops.linewidth, ...
                        'marker',Ops.marker, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour, ...
                        'parent',Parent);
                else
                    hNew(i)=line(data.X(bs(i,1):bs(i,2)), ...
                        data.Y(bs(i,1):bs(i,2)), ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                end
            end
            set(Parent,'layer','top')
        end
        set(get(Parent,'title'),'string',TStr)
    case 1
        if ~FirstFrame
            delete(hNew)
        end
        vNaN=isnan(data.Val);
        if any(vNaN)
            bs=findseries(~vNaN);
        else
            bs=[1 length(vNaN)];
        end
        %
        fill = ~strcmp(Ops.facecolour,'none');
        if fill
            i = data.X(bs(:,1))==data.X(bs(:,2)) & ...
                data.Y(bs(:,1))==data.Y(bs(:,2));
            bf = bs(i,:);
            bs = bs(~i,:);
        else
            bf = zeros(0,2);
        end
        %
        % Determine patch sizes
        %
        len_bf = bf(:,2)-bf(:,1);
        LEN_BF = unique(len_bf);
        lines_to_do = ~isempty(bs);
        %
        % Now create objects
        %
        %Ops.markerfillcolour = 'none';
        hNew = zeros(length(LEN_BF)+1,1);
        for i=1:length(LEN_BF)
            LBF = LEN_BF(i);
            nbf = sum(len_bf==LBF);
            xyd = repmat(NaN,LBF*nbf,2);
            cl = repmat(NaN,LBF*nbf,1);
            %
            k = 1;
            for j=1:length(len_bf)
                if len_bf(j)==LBF
                    range = bf(j,1):bf(j,2)-1;
                    trange = (k-1)*LBF+(1:LBF);
                    xyd(trange,1)=data.X(range);
                    if lines_to_do
                        data.X(bf(j,1):bf(j,2)) = NaN;
                    end
                    xyd(trange,2)=data.Y(range);
                    cl(trange,1)=data.Val(range(1));
                    k=k+1;
                end
            end
            %
            hNew(i)=patch('vertices',xyd, ...
               'faces',reshape(1:LBF*nbf,[LBF nbf])', ...
               'facevertexcdata',cl, ...
                'edgecolor','flat', ...
                'facecolor','flat', ...
                'linestyle',Ops.linestyle, ...
                'linewidth',Ops.linewidth, ...
                'marker',Ops.marker, ...
                'markeredgecolor',Ops.markercolour, ...
                'markerfacecolor',Ops.markerfillcolour, ...
                'parent',Parent);
        end
        %
        if lines_to_do
            gap2 = isnan(data.X) & [1;isnan(data.X(1:end-1))];
            data.X(gap2,:)=[];
            data.X(end+1)=NaN;
            data.Y(gap2,:)=[];
            data.Y(end+1)=NaN;
            data.Val(gap2,:)=[];
            data.Val(end+1)=NaN;
            %
            hNew(length(LEN_BF)+1)=patch(data.X,data.Y,data.Val, ...
                'edgecolor','flat', ...
                'facecolor','none', ...
                'linestyle',Ops.linestyle, ...
                'linewidth',Ops.linewidth, ...
                'marker',Ops.marker, ...
                'markeredgecolor',Ops.markercolour, ...
                'markerfacecolor',Ops.markerfillcolour, ...
                'parent',Parent);
        else
            hNew = hNew(1:end-1);
        end
        %
        set(Parent,'layer','top')
        set(get(Parent,'title'),'string',{PName,TStr})
    case {2,3}
        if multiple(M_) % network
        else % point
        end
    case 4
        switch Ops.presentationtype
            case {'markers'}
                if isfield(data,'XY')
                    hNew=genmarkers(hNew,Ops,Parent,[],data.XY(:,1),data.XY(:,2));
                else
                    hNew=genmarkers(hNew,Ops,Parent,[],data.X,data.Y);
                end
            case {'labels'}
                if isfield(data,'XY')
                    hNew=genmarkers(hNew,Ops,Parent,[],data.XY(:,1),data.XY(:,2));
                else
                    hNew=gentextfld(hNew,Ops,Parent,data.Val,data.X,data.Y);
                end
        end
end
