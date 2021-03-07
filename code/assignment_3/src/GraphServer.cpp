
// A Graph Server implementation serving requests on graph manipulations from clients.

#include "service/GraphService.h"
#include <thrift/server/TSimpleServer.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <memory>
#include <iostream>

using namespace std;
using namespace apache::thrift::server;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;


class GraphServiceHandler : public GraphServiceIf
{
public:
    virtual void getMessage(string& return_str, const string& name) override
    {
        cout << "Server received: \"" << name << "\", from client" << endl;
        return_str = "Hello " + name;
    }
};

int main ()
{
    auto handler = make_shared<GraphServiceHandler> ();
    auto processor = make_shared<GraphServiceProcessor> (handler);

    auto trans_server = make_shared<TServerSocket> (9090);
    auto trans_factory = make_shared<TBufferedTransportFactory> ();
    auto proto_factory = make_shared<TBinaryProtocolFactory> ();

    TSimpleServer server(processor, trans_server, trans_factory, proto_factory);
    server.serve();
    return 0;
}
