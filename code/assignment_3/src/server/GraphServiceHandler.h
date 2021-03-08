
#ifndef GRAPHSERVICEHANDLER_H
#define GRAPHSERVICEHANDLER_H

#include "../service/GraphService.h"
#include "Graph.h"
#include <map>
using namespace std;

class GraphServiceHandler : public GraphServiceIf
{
public:
    enum
    {
        Success = 0,
        Failure = -1
    };
    virtual void addGraph(const string& graphId, const int nodeCount) override;
    virtual int addEdge(const string& graphId, const int u, const int v, const int w) override;
    virtual int getMSTWeight(const string& graphId) override;
private:
    map<string, Graph*> mGraphContainer;
};

#endif
