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
SVCDIR  = ./src/svc

THRIFT_DIR = /usr/local/Cellar/thrift/0.14.0/include
BOOST_DIR  = /usr/local/opt/boost/include
INCLUDE = -I./ -I$(THRIFT_DIR)/ -I$(BOOST_DIR)/
LD_LIBRARY_PATH = -L/usr/local/Cellar/thrift/0.14.0/lib

IDL_FILES     = $(SRCDIR)/hello.thrift
ClIENT_FILES  = $(SRCDIR)/hello_client.cpp
SERVER_FILES  = $(SRCDIR)/hello_server.cpp
SVC_FILES     = $(SVCDIR)/helloSvc.cpp

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

interface:
	@echo "Generating interface files."
	$(THRIFT) -out $(SVCDIR) --gen cpp $(IDL_FILES)

client: $(CLIENT_OBJECTS) $(SVC_OBJECTS)
	$(CXX) $(LDFLAGS) $(CLIENT_OBJECTS) $(SVC_OBJECTS) -o $(CLIENT_TARGET)
	@echo "Linking client objects complete."
	@echo "To start the \"Client\" run --> \"$(CLIENT_TARGET)\""

server: $(SERVER_OBJECTS) $(SVC_OBJECTS)
	$(CXX) $(LDFLAGS) $(SERVER_OBJECTS) $(SVC_OBJECTS) -o $(SERVER_TARGET)
	@echo "Linking server objects complete."
	@echo "To start the \"Server\" run --> \"$(SERVER_TARGET)\""

all: interface client server

clean:
	@echo "Cleaning all the object files and binaries."
	rm -f core $(BINDIR)/* $(OBJDIR)/*
