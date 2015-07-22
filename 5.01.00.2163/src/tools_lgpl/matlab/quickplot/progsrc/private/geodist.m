function d = geodist(dlon1, dlat1, dlon2, dlat2, radius)


if nargin<5
    radius = 6378137; %earth
end

ddegrad = pi/180;

rlon1 = dlon1*ddegrad;
rlon2 = dlon2*ddegrad;
rlat1 = dlat1*ddegrad;
rlat2 = dlat2*ddegrad;

x1 = cos(rlat1).*sin(rlon1);
y1 = cos(rlat1).*cos(rlon1);
z1 = sin(rlat1);

x2 = cos(rlat2).*sin(rlon2);
y2 = cos(rlat2).*cos(rlon2);
z2 = sin(rlat2);

dslin = sqrt((x2-x1).^2 + (y2-y1).^2 + (z2-z1).^2);
alpha = asin(dslin/2);
d     = 2*radius*alpha;