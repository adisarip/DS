
#include "ClientUtils.h"
#include "../service/GraphService.h"
#include <sstream>
#include <iostream>
#include <string>
using namespace std;


// Client utility functions

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
        cout << "[RPCR] Graph Added" << endl;
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
            cout << "[RPCR] Command Skipped" << endl;
        }
        else
        {
            cout << "[RPCR] Edge Added "
                 << "[" << u  << "--" << "(" << w << ")" << "--" << v << "]" << endl;
        }
    }
    else if (sCmd == "get_mst" && tokens.size() == 2)
    {
        sGraphId = tokens[1];
        int sMSTWeight = client->getMSTWeight(sGraphId);
        cout << "[RPCR] MST Weight of graph \"" << sGraphId << "\" = " << sMSTWeight << endl;
    }
    else
    {
        // Invalid Input
        cout << "[ERROR] Invalid Input Command \'" << inputCmd << "\'" << endl;
    }
}
