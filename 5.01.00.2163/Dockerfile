FROM ubuntu:14.04
MAINTAINER Ad van der Veer <a.vanderveer@nerdalize.com>
RUN apt-get update
RUN apt-get install -y tclsh libtool autoconf build-essential flex bison gfortran pkg-config libexpat1 libexpat1-dev mpich2 libnetcdf-dev
ADD . /5.01.00.2163
WORKDIR /5.01.00.2163/src
RUN ./autogen.sh
RUN CFLAGS='-O2 -fPIC -m64' CXXFLAGS='-O2 -fPIC -m64' FFLAGS='-O2 -fPIC -m64' FCFLAGS='-O2 -fPIC -m64' ./configure --prefix=`pwd`
RUN make ds-install

WORKDIR /5.01.00.2163/examples/01_standard
RUN cp ../../src/bin/d_hydro.exe ../../src
RUN ls /5.01.00.2163/src/lib
ENV LD_LIBRARY_PATH=/5.01.00.2163/src/lib:$LD_LIBRARY_PATH

RUN mkdir /job
VOLUME /job
WORKDIR /job
CMD /5.01.00.2163/src/bin/deltares_hydro.tcl config_flow2d3d.ini
