%MAKE_COLORMAPS
%   Script to create a plot of all colour maps in the colormaps directory.

%   $Id$

cd progsrc/private
d=dir('../colormaps/*.clrmap');
Nd=length(d);
set(gcf,'color','w')
for i=1:Nd
    S=clrmap('read',['../colormaps/',d(i).name]);
    axes('position',[0.01+0.98*(i-1)*1.2/(Nd+(Nd-1)*0.2) 0.01 0.98/(Nd+(Nd-1)*0.2) 0.98])
    if isfield(S,'AlternatingColors') & S.AlternatingColors
        NCol=size(S.Colors,1);
        image(repmat(reshape(clrmap(S,NCol),[NCol 1 3]),[1 3]))
    else
        image(repmat(reshape(clrmap(S),[64 1 3]),[1 3]))
    end
    box on
    set(gca,'xtick',[],'ytick',[])
end
cd ../..
