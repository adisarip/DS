

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
3. After installing the above dependencies, run the Makefile provided in the directory 20173071/
   cd 20173071
   "make clean"   --> cleans the repository
   "make"         --> builds all the targets interface services, client and server.
   "make service" -->
   "make client"  -->
