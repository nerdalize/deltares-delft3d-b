FROM ubuntu:14.04
MAINTAINER Ad van der Veer <a.vanderveer@nerdalize.com>
RUN apt-get update
RUN apt-get install -y libtool autoconf build-essential flex bison gfortran pkg-config libexpat1 libexpat1-dev mpich2 libnetcdf-dev
ADD ./5.01.00.2163 /5.01.00.2163
WORKDIR /5.01.00.2163/src
RUN ./autogen.sh
RUN CFLAGS='-O2 -fPIC -m64' CXXFLAGS='-O2 -fPIC -m64' FFLAGS='-O2 -fPIC -m64' FCFLAGS='-O2 -fPIC -m64' ./configure --prefix=`pwd`
RUN make

