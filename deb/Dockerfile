FROM ubuntu:14.04
MAINTAINER Ad van der Veer <a.vanderveer@nerdalize.com>
RUN apt-get update
RUN apt-get install -y tclsh libtool autoconf build-essential flex bison gfortran pkg-config libexpat1 libexpat1-dev mpich2 libnetcdf-dev
ADD delft3d-hydro-morpho-waq_4.01.00-9_amd64.deb / 

RUN dpkg -i /delft3d-hydro-morpho-waq_4.01.00-9_amd64.deb
RUN ln -s /opt/delft3d_4.01.00 /opt/delft3d
ENV D3D_HOME=/opt/delft3d
ENV ARCH=lnx64

RUN mkdir /job
VOLUME /job
WORKDIR /job
ENTRYPOINT ["/job/run.sh"]
