function hNew = qp_scalarfield(Parent,hNew,presentationtype,datatype,varargin)
switch datatype
    case 'TRI'
        hNew = qp_scalarfield_tri(Parent,hNew,presentationtype,varargin{:});
    case 'QUAD'
        hNew = qp_scalarfield_quad(Parent,hNew,presentationtype,varargin{:});
end

function hNew = qp_scalarfield_quad(Parent,hNew,presentationtype,X,Y,Z,Val,Ops)
extended=~isequal(size(X),size(Val));
if ~extended
    Val(isnan(X) | isnan(Y))=NaN;
    mx=max(X(:));
    my=max(Y(:));
    X(isnan(X))=mx;
    Y(isnan(Y))=my;
end
%
set(Parent,'NextPlot','add');
switch presentationtype
    case {'patches','patches with lines'}
        if isequal(size(X),size(Z))
            hNew=genfaces(hNew,Ops,Parent,Val,X,Y,Z);
        else
            hNew=genfaces(hNew,Ops,Parent,Val,X,Y);
        end
        
    case 'values'
        I=~isnan(Val);
        hNew=gentextfld(hNew,Ops,Parent,Val(I),X(I),Y(I));
        
    case 'continuous shades'
        if isequal(size(X),size(Z))
            z=Z;
        else
            z=Val;
        end
        hNew=gensurface(hNew,Ops,Parent,Val,X,Y,z);
        
    case 'markers'
        hNew=genmarkers(hNew,Ops,Parent,Val,X,Y);
        
    case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
        if isequal(size(X),size(Val)+1)
            [X,Y,Val]=face2surf(X,Y,Val);
            X(isnan(X))=mean(X(~isnan(X)));
            Y(isnan(Y))=mean(Y(~isnan(Y)));
        end
        hNew=gencontour(hNew,Ops,Parent,X,Y,Val,Ops.Thresholds);
        if strcmp(Ops.presentationtype,'contour lines')
            set(hNew,Ops.LineParams{:});
        end
end

function hNew = qp_scalarfield_tri(Parent,hNew,presentationtype,TRI,XYZ,Val,Ops)
set(Parent,'NextPlot','add');
switch presentationtype
    case {'patches','patches with lines'}
        hNew=genfaces(hNew,Ops,Parent,Val,XYZ,TRI);
        
    case 'values'
        I=~isnan(Val);
        hNew=gentextfld(hNew,Ops,Parent,Val(I),X(I),Y(I));
        
    case 'markers'
        hNew=genmarkers(hNew,Ops,Parent,Val,X,Y);
        
    case 'continuous shades'
        
        if size(XYZ,4)==2
            sz = size(XYZ);
            sz(4) = 1;
            XYZ = cat(4,XYZ,reshape(Val,sz));
        end
        XYZ=squeeze(XYZ);
        if isempty(hNew)
            hNew=patch('vertices',XYZ,'faces',TRI,'facevertexcdata',Val(:), ...
                'facecolor','interp','edgecolor','none', ...
                'parent',Parent);
            
        elseif ishandle(hNew)
            set(hNew,'vertices',XYZ,'facevertexcdata',Val(:));
        else
            return
        end
        
    case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
        XYZ=squeeze(XYZ);
        delete(hNew);
        switch Ops.presentationtype
            case 'contour lines'
                hNew=tricontour(TRI,XYZ(:,1),XYZ(:,2),Val(:),Ops.Thresholds,'k');
                set(hNew,'color',Ops.colour,'linestyle',Ops.linestyle,'marker',Ops.marker,'markeredgecolor',Ops.markercolour,'markerfacecolor',Ops.markerfillcolour)
            case 'coloured contour lines'
                hNew=tricontour(TRI,XYZ(:,1),XYZ(:,2),Val(:),Ops.Thresholds);
                for i=1:length(hNew)
                    c=get(hNew(i),'FaceVertexCData');
                    set(hNew(i),'FaceVertexCData',0*c+i)
                end
            case 'contour patches'
                hNew=tricontourf(TRI,XYZ(:,1),XYZ(:,2),Val(:),Ops.Thresholds,'clevel','index','zplane',0);
                for i=1:length(hNew)
                    c=get(hNew(i),'FaceVertexCData');
                    set(hNew(i),'FaceVertexCData',0*c+i)
                end
            case 'contour patches with lines'
                hNew1=tricontourf(TRI,XYZ(:,1),XYZ(:,2),Val(:),Ops.Thresholds,'clevel','index','zplane',0);
                for i=1:length(hNew)
                    c=get(hNew(i),'FaceVertexCData');
                    set(hNew(i),'FaceVertexCData',0*c+i)
                end
                hNew2=tricontour(TRI,XYZ(:,1),XYZ(:,2),Val(:),Ops.Thresholds,'k');
                set(hNew2,'color',Ops.colour,'linestyle',Ops.linestyle,'marker',Ops.marker,'markeredgecolor',Ops.markercolour,'markerfacecolor',Ops.markerfillcolour)
                hNew = [hNew1 hNew2];
        end
end
