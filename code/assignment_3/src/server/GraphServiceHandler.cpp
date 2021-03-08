
#include "GraphServiceHandler.h"
#include <iostream>
using namespace std;

// Definitions

void GraphServiceHandler::addGraph(const string& graphId, const int nodeCount)
{
    cout << "[RPCQ] Creating the graph \"" << graphId << "\" with " << nodeCount << " nodes." << endl;
    Graph* pGraph = new Graph(nodeCount);
    mGraphContainer[graphId] = pGraph;
}

int GraphServiceHandler::addEdge(const string& graphId, const int u, const int v, const int w)
{
    int sRc = GraphServiceHandler::Success;
    Graph* pGraph = mGraphContainer[graphId];
    if (NULL != pGraph)
    {
        cout << "[RPCQ] Adding un-directed edge "
             << "[" << u  << "--" << "(" << w << ")" << "--" << v << "]"
             << " to the graph \"" << graphId << "\"" << endl;
        pGraph->addEdge(u, v, w);
    }
    else
    {
        sRc = GraphServiceHandler::Failure;
        cout << "[ERROR] Graph \"" << graphId << "\" donot exists." << endl;
    }
    return sRc;
}

int GraphServiceHandler::getMSTWeight(const string& graphId)
{
    cout << "[RPCQ] Requesting MST weight for graph \"" << graphId << "\"" << endl;
    Graph* pGraph = mGraphContainer[graphId];
    int sMSTWeight = -1;
    if (NULL != pGraph)
    {
        sMSTWeight = pGraph->getMSTWeight();
    }
    return sMSTWeight;
}
