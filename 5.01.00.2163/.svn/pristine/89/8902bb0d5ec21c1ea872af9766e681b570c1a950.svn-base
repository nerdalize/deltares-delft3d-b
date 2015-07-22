function handle=qp_colorbar(varargin)
%QP_COLORBAR Display color bar (color scale).
%   QP_COLORBAR mixes functionality of the COLORBAR functions in
%   MATLAB 5.3 and MATLAB 6.5. It does not change the settings of
%   the current figure and current axes. It does not make the
%   figure visible in compiled mode. It allows only one colorbar
%   per axes (either vertical or horizontal) and switches upon
%   request between them. Can also remove the colorbar.
%
%   QP_COLORBAR(LOC) where LOC='horiz','vert','none' or axes handle.
%   ...,'peer',AX) links to axes object AX.
%
%   Derived from COLORBAR by Clay M. Thompson 10-9-92, The MathWorks, Inc.

%   If called with COLORBAR(H) or for an existing colorbar, don't change
%   the NextPlot property.

%   $Id$

changeNextPlot = 1;

rloc=[];
loc = 'vert';
h = [];
switch nargin
   case 0
   case 1
      loc = varargin{1};
      if ischar(loc), rloc=loc; end
   case 2
      if ~isequal(lower(varargin{1}),'peer')
         error('Unknown combination of input parameters.')
      else
         h = varargin{2};
      end
   case 3
      if ~isequal(lower(varargin{2}),'peer')
         error('Unknown combination of input parameters.')
      else
         loc = varargin{1};
         if ischar(loc), rloc=loc; end
         h = varargin{3};
      end
   otherwise
      error('Too many input arguments')
end

if ischar(loc), loc=lower(loc); end

% Catch colorbar('delete') special case -- must be called by the deleteFcn.
if nargin==1 & strcmp(loc,'delete'),
   ax = gcbo;
   if isequal(get(ax,'type'),'text')
      delete(get(gcbo,'userdata'))
      return
   end
   if strcmp(get(ax,'tag'),'TMW_COLORBAR'),
      ax=get(ax,'parent');
   end
   ud = get(ax,'userdata');
   if isfield(ud,'PlotHandle') & ishandle(ud.PlotHandle) & ...
         isfield(ud,'origPos') & ~isempty(ud.origPos)
      units = get(ud.PlotHandle,'units');
      set(ud.PlotHandle,'units','normalized');
      set(ud.PlotHandle,'position',ud.origPos);
      set(ud.PlotHandle,'units',units);
   end
   if isfield(ud,'DeleteProxy') & ishandle(ud.DeleteProxy)
      delete(ud.DeleteProxy)
   end
   if ~isempty(legend)
      legend % Update legend
   end
   return
end

ax = [];
if ishandle(loc)
   ax = loc;
   if ~strcmp(get(ax,'type'),'axes'),
      error('Requires axes handle.');
   end
   units = get(ax,'units'); set(ax,'units','pixels');
   rect = get(ax,'position'); set(ax,'units',units)
   if rect(3) > rect(4), loc = 'horiz'; else loc = 'vert'; end
   changeNextPlot = 0;
end

% Determine color limits by context.  If any axes child is an image
% use scale based on size of colormap, otherwise use current CAXIS.

if isempty(h)
   GCF=get(0,'currentfigure');
   if isempty(GCF), return; end
   h = gcda(GCF);
end

if isempty(h) | ~ishandle(h) | ~isequal(get(h,'type'),'axes')
   return;
else
   GCF=get(h,'Parent');
end

ch = get(h,'children');
hasimage = 0; t = [];
cdatamapping = 'direct';

for i=1:length(ch),
   typ = get(ch(i),'type');
   if strcmp(typ,'image'),
      cdata = get(ch(i),'cdata');
      if size(cdata,3)~=3
         hasimage = 1;
         cdatamapping = get(ch(i), 'CDataMapping');
      end
   elseif strcmp(typ,'surface') & ...
         strcmp(get(ch(i),'FaceColor'),'texturemap') % Texturemapped surf
      cdata = get(ch(i),'cdata');
      if size(cdata,3)~=3
         hasimage = 2;
         cdatamapping = get(ch(i), 'CDataMapping');
      end
   elseif strcmp(typ,'patch') | strcmp(typ,'surface')
      cdatamapping = get(ch(i), 'CDataMapping');
   end
end

lengthcmap=size(get(GCF,'colormap'),1);
if ( strcmp(cdatamapping, 'scaled') )
   % Treat images and surfaces alike if cdatamapping == 'scaled'
   t = get(h,'clim');
   d = (t(2) - t(1))/lengthcmap;
   t = [t(1)+d/2  t(2)-d/2];
else
   if hasimage,
      t = [1, lengthcmap];
   else
      t = [1.5  lengthcmap+.5];
   end
end

%
% Use allways existing colorbar
%
% Search for existing colorbar
ch = get(findobj(GCF,'type','image','tag','TMW_COLORBAR'),{'parent'});
for i=1:length(ch),
   ud = get(ch{i},'userdata');
   d = ud.PlotHandle;
   if prod(size(d))==1 & isequal(d,h),
      eax = ch{i};
      units = get(eax,'units'); set(eax,'units','pixels');
      rect = get(eax,'position'); set(eax,'units',units)
      if rect(3)<rect(4), eloc = 'vert'; else eloc = 'horiz'; end
      if ischar(rloc) & ~isequal(eloc,rloc),
         delete(eax);
      else
         loc=eloc;
         ax=eax;
         changeNextPlot = 0;
         % Make sure image deletefcn doesn't trigger a colorbar('delete')
         % for colorbar update
         set(get(ax,'children'),'deletefcn','')
      end
      break;
   end
end

origCurAxes = get(GCF,'CurrentAxes');
origNextPlot = get(GCF,'NextPlot');
if strcmp(origNextPlot,'replacechildren') | strcmp(origNextPlot,'replace'),
   set(GCF,'NextPlot','add')
end

if loc(1)=='v', % Append vertical scale to right of current plot

   if isempty(ax),
      units = get(h,'units'); set(h,'units','normalized')
      pos = get(h,'Position');
      azel=get(h,'view'); az=azel(1); el=azel(2);
      stripe = 0.075; edge = 0.02;
      if all([az,el]==[0 90]), space = 0.05; else space = .1; end
      set(h,'Position',[pos(1) pos(2) pos(3)*(1-stripe-edge-space) pos(4)])
      rect = [pos(1)+(1-stripe-edge)*pos(3) pos(2) stripe*pos(3) pos(4)];
      ud.origPos = pos;

      % Create axes for stripe and
      % create DeleteProxy object (an invisible text object in
      % the target axes) so that the colorbar will be deleted
      % properly.
      ud.DeleteProxy = text('parent',h,'visible','off',...
         'tag','ColorbarDeleteProxy',...
         'handlevisibility','off',...
         'deletefcn','qp_colorbar delete');
      ax = axes('Position', rect);
      setappdata(ax,'NonDataObject',[]); % For DATACHILDREN.M
      set(ud.DeleteProxy,'userdata',ax)
      set(h,'units',units)
   else
      %axes(ax);
      ud = get(ax,'userdata');
   end

   % Create color stripe
   n = size(get(GCF,'colormap'),1);
   image([0 1],t,(1:n)','Parent',ax,'Tag','TMW_COLORBAR','deletefcn','qp_colorbar delete');
   set(ax,'Ydir','normal')
   set(ax,'YAxisLocation','right')
   set(ax,'xtick',[])

   % set up axes deletefcn
   set(ax,'tag','Colorbar','deletefcn','qp_colorbar delete')

elseif loc(1)=='h', % Append horizontal scale to top of current plot

   if isempty(ax),
      units = get(h,'units'); set(h,'units','normalized')
      pos = get(h,'Position');
      stripe = 0.075; space = 0.1;
      set(h,'Position',...
         [pos(1) pos(2)+(stripe+space)*pos(4) pos(3) (1-stripe-space)*pos(4)])
      rect = [pos(1) pos(2) pos(3) stripe*pos(4)];
      ud.origPos = pos;

      % Create axes for stripe and
      % create DeleteProxy object (an invisible text object in
      % the target axes) so that the colorbar will be deleted
      % properly.
      ud.DeleteProxy = text('parent',h,'visible','off',...
         'tag','ColorbarDeleteProxy',...
         'handlevisibility','off',...
         'deletefcn','qp_colorbar delete');
      ax = axes('Position', rect);
      setappdata(ax,'NonDataObject',[]); % For DATACHILDREN.M
      set(ud.DeleteProxy,'userdata',ax)
      set(h,'units',units)
   else
      %axes(ax);
      ud = get(ax,'userdata');
   end

   % Create color stripe
   n = size(get(GCF,'colormap'),1);
   image(t,[0 1],(1:n),'Parent',ax,'Tag','TMW_COLORBAR','deletefcn','qp_colorbar delete');
   set(ax,'Ydir','normal')
   set(ax,'ytick',[])

   % set up axes deletefcn
   set(ax,'tag','Colorbar','deletefcn','qp_colorbar delete')

elseif isequal(loc,'none')
   if nargout>0, handle = ax; end
   return
else
   error('COLORBAR expects a handle, ''vert'', or ''horiz'' as input.')
end

if ~isfield(ud,'DeleteProxy'), ud.DeleteProxy = []; end
if ~isfield(ud,'origPos'), ud.origPos = []; end
ud.PlotHandle = h;
set(ax,'userdata',ud)
set(GCF,'currentaxes',origCurAxes)
set(GCF,'NextPlot',origNextPlot)
if ~isempty(legend)
   legend % Update legend
end
if nargout>0, handle = ax; end

%--------------------------------
function h = gcda(Fig)
%GCDA Get current data axes

h = datachildren(Fig);
GCA=get(Fig,'currentaxes');
if isempty(h) | any(h == GCA)
   h = GCA;
else
   h = h(1);
end