

Environment:
============
Operating System:  Mac OSX BigSur 11.2.2
Package Installer: Homebrew
Compiler:          clang++
RPC Package:       Apache Thrift


Installation instructions:
==========================
1. Install commandLineTools for Mac OSX using Homebrew.
   - This is to get the native compilers and development utilities like make/gcc/g++/clang/clang++ etc.

2. brew install thrift
   - Installs Apache "thrift" RPC package and needed dependencies.


Building the binaries:
======================
1. After installing the above dependencies update the following things in the Makefile
   - update "INCLUDE" and "LD_LIBRARY_PATH" based on the installation of Apache thrift in your environment.

2. After updating the Makefile, build the client and server binaries as follows
   cd 20173071
   "make clean"   --> cleans the repository
   "make"         --> builds all the targets: RPC services, client and server.
   "make service" --> generates Apache thrift RPC service files.
                      Should be the first target to build before 'client' or 'server' targets.
   "make client"  --> builds the client
   "make server"  --> builds the server

3. Build the whole project as : "make clean; make"


Compilation Runs:
=================
1. make clean; make
assignment_3$ make clean; make
Cleaning all the autogen files, object files and binaries.
rm -f core ./src/service/* ./bin/* ./obj/*
Generating interface files.
mkdir -p ./src/service
thrift -out ./src/service --gen cpp ./src/interface.thrift
clang++ -Wall -std=c++11 -I./ -I/usr/local/Cellar/thrift/0.14.0/include -c src/GraphClient.cpp -o obj/GraphClient.o
Compiled src/GraphClient.cpp successfully.
clang++ -Wall -std=c++11 -I./ -I/usr/local/Cellar/thrift/0.14.0/include -c src/service/GraphService.cpp -o obj/GraphService.o
Compiled src/service/GraphService.cpp successfully.
clang++ -lthrift ./obj/GraphClient.o ./obj/GraphService.o -o ./bin/client
Linking client objects complete.
To start the "Client" run --> "./bin/client"
clang++ -Wall -std=c++11 -I./ -I/usr/local/Cellar/thrift/0.14.0/include -c src/GraphServer.cpp -o obj/GraphServer.o
Compiled src/GraphServer.cpp successfully.
clang++ -lthrift ./obj/GraphServer.o ./obj/GraphService.o -o ./bin/server
Linking server objects complete.
To start the "Server" run --> "./bin/server"
assignment_3$

2. make service
assignment_3$ make service
Generating interface files.
mkdir -p ./src/service
thrift -out ./src/service --gen cpp ./src/interface.thrift
assignment_3$

3. make client
assignment_3$ make client
clang++ -Wall -std=c++11 -I./ -I/usr/local/Cellar/thrift/0.14.0/include -c src/GraphClient.cpp -o obj/GraphClient.o
Compiled src/GraphClient.cpp successfully.
clang++ -Wall -std=c++11 -I./ -I/usr/local/Cellar/thrift/0.14.0/include -c src/service/GraphService.cpp -o obj/GraphService.o
Compiled src/service/GraphService.cpp successfully.
clang++ -lthrift ./obj/GraphClient.o ./obj/GraphService.o -o ./bin/client
Linking client objects complete.
To start the "Client" run --> "./bin/client"
assignment_3$

4. make server
assignment_3$ make server
clang++ -Wall -std=c++11 -I./ -I/usr/local/Cellar/thrift/0.14.0/include -c src/GraphServer.cpp -o obj/GraphServer.o
Compiled src/GraphServer.cpp successfully.
clang++ -lthrift ./obj/GraphServer.o ./obj/GraphService.o -o ./bin/server
Linking server objects complete.
To start the "Server" run --> "./bin/server"
assignment_3$


Execution Runs:
===============
[INFO]  -> Informational traces
[RPCQ]  -> RPC Request traces
[RPCR]  -> RPC Response traces
[ERROR] -> Error traces
