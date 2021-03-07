
// clang++ -Wall -std=c++11 -lthrift -o client
// -I/usr/local/Cellar/thrift/0.14.0/include
// -I/usr/local/opt/boost/include
// -L/usr/local/Cellar/thrift/0.14.0/lib hello_client.cpp gen-cpp/helloSvc.cpp


#include "svc/helloSvc.h"
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <memory>
#include <iostream>

using namespace std;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

int main()
{
    shared_ptr<TTransport> trans;
    trans = make_shared<TSocket> ("localhost", 9090);
    trans = make_shared<TBufferedTransport> (trans);
    auto proto = make_shared<TBinaryProtocol> (trans);
    helloSvcClient client(proto);

    try
    {
        trans->open();
        string msg;
        client.getMessage(msg, "World!");
        cout << msg << endl;
    }
    catch(...)
    {
        cout << "Client Caught an Exception" << endl;
    }
    trans->close();
}
