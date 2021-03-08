###############################################################################
# File        : Makefile                                                      #
# Author      : Aditya Saripalli                                              #
# Description : Makefile for building any binaries for DS course Assignment-3 #
#               following directory structure.                                #
#               ./src                                                         #
#               ./obj                                                         #
#               ./bin                                                         #
###############################################################################

.DEFAULT_GOAL := all

CXX     = clang++
THRIFT  = thrift
CCFLAGS = -Wall -std=c++11
LDFLAGS = -lthrift
SRCDIR  = ./src
OBJDIR  = ./obj
BINDIR  = ./bin
SVCDIR  = ./src/service

INCLUDE = -I./ -I/usr/local/Cellar/thrift/0.14.0/include
LD_LIBRARY_PATH = -L/usr/local/Cellar/thrift/0.14.0/lib

IDL_FILES     = $(SRCDIR)/interface.thrift
ClIENT_FILES  = $(SRCDIR)/GraphClient.cpp
SERVER_FILES  = $(SRCDIR)/GraphServer.cpp $(SRCDIR)/Graph.cpp
SVC_FILES     = $(SVCDIR)/GraphService.cpp

CLIENT_OBJECTS := $(ClIENT_FILES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)
SERVER_OBJECTS := $(SERVER_FILES:$(SRCDIR)/%.cpp=$(OBJDIR)/%.o)
SVC_OBJECTS    := $(SVC_FILES:$(SVCDIR)/%.cpp=$(OBJDIR)/%.o)

CLIENT_TARGET = $(BINDIR)/client
SERVER_TARGET = $(BINDIR)/server

$(CLIENT_OBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.cpp
	$(CXX) $(CCFLAGS) $(INCLUDE) -c $< -o $@
	@echo "Compiled "$<" successfully."

$(SERVER_OBJECTS): $(OBJDIR)/%.o : $(SRCDIR)/%.cpp
	$(CXX) $(CCFLAGS) $(INCLUDE) -c $< -o $@
	@echo "Compiled "$<" successfully."

$(SVC_OBJECTS): $(OBJDIR)/%.o : $(SVCDIR)/%.cpp
	$(CXX) $(CCFLAGS) $(INCLUDE) -c $< -o $@
	@echo "Compiled "$<" successfully."

# build this target before building the client or server targets
service:
	@echo "Generating interface files."
	mkdir -p $(SVCDIR)
	$(THRIFT) -out $(SVCDIR) --gen cpp $(IDL_FILES)

client: $(CLIENT_OBJECTS) $(SVC_OBJECTS)
	$(CXX) $(LDFLAGS) $(CLIENT_OBJECTS) $(SVC_OBJECTS) -o $(CLIENT_TARGET)
	@echo "Linking client objects complete."
	@echo "To start the \"Client\" run --> \"$(CLIENT_TARGET)\""

server: $(SERVER_OBJECTS) $(SVC_OBJECTS)
	$(CXX) $(LDFLAGS) $(SERVER_OBJECTS) $(SVC_OBJECTS) -o $(SERVER_TARGET)
	@echo "Linking server objects complete."
	@echo "To start the \"Server\" run --> \"$(SERVER_TARGET)\""

all: service client server

clean:
	@echo "Cleaning all the autogen files, object files and binaries."
	rm -f core $(SVCDIR)/* $(BINDIR)/* $(OBJDIR)/*
