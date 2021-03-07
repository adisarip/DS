#include "svc/helloSvc.h"
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


class helloSvcHandler : public helloSvcIf
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
    auto handler = make_shared<helloSvcHandler> ();
    auto processor = make_shared<helloSvcProcessor> (handler);

    auto trans_server = make_shared<TServerSocket> (9090);
    auto trans_factory = make_shared<TBufferedTransportFactory> ();
    auto proto_factory = make_shared<TBinaryProtocolFactory> ();

    TSimpleServer server(processor, trans_server, trans_factory, proto_factory);
    server.serve();
    return 0;
}
