# deltares-delft3d
![Delft3D](http://walrus.wr.usgs.gov/coastal_processes/sfbaycoastalsys/sfbight/images/NestedGrid.jpg "Delft3D")

Docker container for running Delft3D: a world leading open-source 2D/3D modeling suite to investigate hydrodynamics, sediment transport, morphology and water quality for fluvial, estuarine and coastal environments. 

This repository packages Delft3D into a [Docker](https://www.docker.com/) container, this has several benefits:

- Run modeling jobs on any machine that accepts Docker containers (localhost, Nerdalize, Amazon etc)
- The steps required to start a job are the same regardless of the underlying infrastructure
- Getting started with Delft3D no longer requires manual compilation and configuration 

## Requirements
The Deltares Delft3D container requires a Docker running on a linux host (Boot2Docker, Ubuntu, CentOS etc) and a Docker client that is able to connect to this host. Instructions for installing Docker can be found [here](https://docs.docker.com/installation/)

## Getting Started
