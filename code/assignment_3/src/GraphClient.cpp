
#include "service/GraphService.h"
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <memory>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

void processUserCommand(GraphServiceClient* client, string inputCmd)
{
    istringstream iss(inputCmd);
    vector<string> tokens(istream_iterator<string>{iss}, istream_iterator<string>());
    string sGraphId;
    string sCmd = (tokens.size() > 0) ? tokens[0] : "";

    if (sCmd == "add_graph" && tokens.size() == 3)
    {
        sGraphId = tokens[1];
        int sNodeCount = stoi(tokens[2]);
        client->addGraph(sGraphId, sNodeCount);
        cout << "\n[RPCR] Graph Added" << endl;
    }
    else if (sCmd == "add_edge" && tokens.size() == 5)
    {
        sGraphId = tokens[1];
        int u = stoi(tokens[2]);
        int v = stoi(tokens[3]);
        int w = stoi(tokens[4]);
        int sRc = client->addEdge(sGraphId, u, v, w);
        if (sRc < 0)
        {
            cout << "\n[RPCR] Command Skipped" << endl;
        }
        else
        {
            cout << "\n[RPCR] Edge Added" << endl;
        }
    }
    else if (sCmd == "get_mst" && tokens.size() == 2)
    {
        sGraphId = tokens[1];
        int sMSTWeight = client->getMSTWeight(sGraphId);
        cout << "\n[RPCR] MST Weight of graph \"" << sGraphId << "\" = " << sMSTWeight << endl;
    }
    else
    {
        // Invalid Input
        cout << "\n[ERROR] Invalid Input Command \'" << inputCmd << "\'" << endl;
    }
}

void displayMenu()
{
    cout << " ___________________________________________________________________ " << endl;
    cout << "| Add a Graph    ( Command: add_graph <graph_id_string> N )         |" << endl;
    cout << "| Add an Edge    ( Command: add_edge <graph_id_string> <u> <v> <w>) |" << endl;
    cout << "| Get MST Weight ( Command: get_mst <graph_id_string> )             |" << endl;
    cout << "|___________________________________________________________________|" << endl;
    cout << "|                           \"-1\" to EXIT                            |" << endl;
    cout << "|___________________________________________________________________|" << endl;
}

int main()
{
    shared_ptr<TTransport> transaction;
    transaction = make_shared<TSocket> ("localhost", 9090);
    transaction = make_shared<TBufferedTransport> (transaction);
    auto proto = make_shared<TBinaryProtocol> (transaction);
    GraphServiceClient client(proto);

    try
    {
        transaction->open();
        cout << "[INFO] Performing Graph Operations using RPC" << endl;
        cout << "[INFO] Operations Supported are ..." << endl;
        while (1)
        {
            string sUserInput;
            displayMenu();
            cout << "Command: ";
            getline(cin, sUserInput);
            if (sUserInput == "-1")
            {
                break;
            }
            processUserCommand(&client, sUserInput);
        }
    }
    catch(...)
    {
        cout << "[ERROR] Client Caught an Exception" << endl;
    }
    transaction->close();
}
