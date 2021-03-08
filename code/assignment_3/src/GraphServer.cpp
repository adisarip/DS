
// A Graph Server implementation serving requests on graph manipulations from clients.

#include "service/GraphService.h"
#include "Graph.h"
#include <thrift/server/TThreadedServer.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <memory>
#include <iostream>

using namespace std;
using namespace apache::thrift::server;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace apache::thrift::concurrency;

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

int main ()
{
    auto handler = make_shared<GraphServiceHandler> ();
    auto processor = make_shared<GraphServiceProcessor> (handler);

    auto trans_server = make_shared<TServerSocket> (9090);
    auto trans_factory = make_shared<TBufferedTransportFactory> ();
    auto proto_factory = make_shared<TBinaryProtocolFactory> ();

    TThreadedServer server(processor, trans_server, trans_factory, proto_factory);
    server.serve();
    return 0;
}
