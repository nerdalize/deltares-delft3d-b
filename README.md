# deltares-delft3d
![Delft3D](http://walrus.wr.usgs.gov/coastal_processes/sfbaycoastalsys/sfbight/images/NestedGrid.jpg "Delft3D")

Docker container for running Delft3D: a world leading open-source 2D/3D modeling suite to investigate hydrodynamics, sediment transport, morphology and water quality for fluvial, estuarine and coastal environments. 

This repository packages Delft3D into a [Docker](https://www.docker.com/) container, this has several benefits:

- It is possible to run modeling jobs on any machine thas has Docker (localhost, Nerdalize, Amazon etc)
- The steps required to start a job are the same regardless of the underlying infrastructure
- Getting started with Delft3D no longer requires manual compilation and configuration 

## Requirements
The Deltares Delft3D container requires a Docker running on a linux host (Boot2Docker, Ubuntu, CentOS etc) and a Docker client that is able to connect to this host. Instructions for installing Docker can be found [here](https://docs.docker.com/installation/)

## Getting Started
Delft3D comes with several examples that we can use to demonstrate the usefullness of this container:

1. We'll first need something to model, the Deltares Open Source Software (OSS) communites provides a basic example but requires you to create an [account first](http://oss.deltares.nl/home). 
2. Once you've received your credentials, navigate to your favorite working directory and download an example from the Deltares repository: 

   ```
   cd ~/my-projects
   wget --user=[YOUR_OSS_USERNAME] --password=[YOUR_OSS_PASSWORD] -r --no-parent https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/examples/01_standard/
   ```
3. Then navigate to the example directory and run the Delft3D Docker container. The current working directory (i.e the example) is mounted as a volume into the container using the `pwd` command and the Docker -v option:
 
   ```
   cd svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/examples/01_standard/
   docker run -v $(pwd):/job quay.io/nerdalize/deltares-delft3d:5.0.1
   ```
4. The output should finish with `0 errors and 0 warnings` and the current working directory will contain the newly created output files: 

   ```
   tri-diag.f34
trih-f34.dat
trih-f34.def
trim-f34.dat
trim-f34.def
   ```
## Debugging the Delft3D Container
In order to make debugging easier it is possible to step inside the container, this gives full access to the filesystem and Delft3D binaries allowing you to read log files or fiddle with parameters. To do this run the following:

```
docker run -it -v $(pwd):/job deltares-delft3d bash
```

