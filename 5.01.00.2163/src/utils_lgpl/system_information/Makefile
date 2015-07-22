CC=g++
CFLAGS=-c -Wall
LDFLAGS=
LIBS=-lrt
SOURCES=src/main.cpp src/cpuinfo.cpp src/meminfo.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=sysinfo

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) $(LIBS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@
