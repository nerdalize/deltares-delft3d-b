function data = qp_dimsqueeze(data,axestype,multiple,DimFlag,Props)

T_=1; ST_=2; M_=3; N_=4; K_=5;

keepext = zeros(1,5);
switch axestype
    case 'X-Y'
        keepext([M_ N_]) = 1;
    case 'X-Z'
        %keepext([M_||N_ K_]) = 1;
end

di=1+~multiple(T_);
for i=[K_ N_ M_]
    if ~multiple(i) && DimFlag(i) && ~keepext(i)
       for d=1:length(data)
          geom='';
          if isfield(data,'Geom') && ~isempty(data(d).Geom)
             geom=data(d).Geom;
          elseif isfield(Props,'Geom')
             geom=Props.Geom;
          end
          if ~strcmp(geom,'POLYL') && ~strcmp(geom,'POLYG')
             if isfield(data,'X') && size(data(d).X,i-di)>1
                data(d).X=mean(data(d).X,i-di);
             end
             if isfield(data,'Y') && size(data(d).Y,i-di)>1
                data(d).Y=mean(data(d).Y,i-di);
             end
             if isfield(data,'Z') && size(data(d).Z,i-di)>1
                data(d).Z=mean(data(d).Z,i-di);
             end
          end
       end
    end
end
