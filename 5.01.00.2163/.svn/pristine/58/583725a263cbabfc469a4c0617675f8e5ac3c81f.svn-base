function [hNew,Thresholds,Param]=qp_plot_seg(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_SEG Plot function of QuickPlot for 1D line segment data sets.

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

switch NVal,
    case 0
        if multiple(M_) % network
            if ishandle(hNew)
                set(hNew,'vertices',data.XY,'faces',data.SEG(:,[1 2 2]))
            else
                hNew=patch('vertices',data.XY,'faces',data.SEG(:,[1 2 2]), ...
                    'parent',Parent, ...
                    'edgecolor',Ops.colour, ...
                    'linewidth',Ops.linewidth, ...
                    'linestyle',Ops.linestyle, ...
                    'marker',Ops.marker, ...
                    'markeredgecolor',Ops.markercolour, ...
                    'markerfacecolor',Ops.markerfillcolour);
            end
        else % point
            if ishandle(hNew)
                set(hNew,'xdata',data.XY(:,1),'ydata',data.XY(:,2))
            else
                hNew=line(data.XY(:,1),data.XY(:,2), ...
                    'parent',Parent, ...
                    Ops.LineParams{:});
            end
        end
    case 1
        if multiple(M_) % network
            if Props.DataInCell % segment data
                XY=data.XY(data.SEG(:),:);
                SEG=data.SEG; SEG(:)=1:2*size(SEG,1);
                Val=cat(1,data.Val(:),data.Val(:));
                if ishandle(hNew)
                    set(hNew,'vertices',XY,'faces',SEG, ...
                        'facevertexcdata',Val)
                else
                    hNew=patch('vertices',XY, ...
                        'faces',SEG, ...
                        'facevertexcdata',Val, ...
                        'parent',Parent, ...
                        'edgecolor','flat', ...
                        'linewidth',Ops.linewidth, ...
                        'linestyle',Ops.linestyle, ...
                        'marker',Ops.marker, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour);
                end
            else % point data
                if ishandle(hNew)
                    if isfield(data,'SEG')
                        set(hNew,'vertices',data.XY,'faces',data.SEG, ...
                            'facevertexcdata',data.Val(:))
                    else
                        set(hNew,'vertices',data.XY,'faces',(1:size(data.XY,1))', ...
                            'facevertexcdata',data.Val(:))
                    end
                else
                    if isfield(data,'SEG')
                        hNew=patch('vertices',data.XY,'faces',data.SEG, ...
                            'facevertexcdata',data.Val(:), ...
                            'parent',Parent, ...
                            'edgecolor','interp', ...
                            'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'marker',Ops.marker, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour);
                    else
                        hNew=patch('vertices',data.XY,'faces',(1:size(data.XY,1))', ...
                            'facevertexcdata',data.Val(:), ...
                            'parent',Parent, ...
                            'marker',Ops.marker, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'markersize',6, ...
                            'linestyle','none', ...
                            'edgecolor','flat', ...
                            'facecolor','none');
                    end
                end
            end
        else % point
            if multiple(T_)
                if ishandle(hNew)
                    set(hNew,'xdata',data.Time(:),'ydata',data.Val(:));
                else
                    hNew=line(data.Time(:),data.Val(:,1), ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                end
            end
        end
    case {2,3}
        if multiple(M_) % network
        else % point
        end
    case 4
        hNew=gentextfld(hNew,Ops,Parent,data.Val,data.XY(:,1),data.XY(:,2));
end
set(get(Parent,'title'),'string',{PName,TStr})

