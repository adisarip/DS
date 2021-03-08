
#include "ClientUtils.h"
#include "../service/GraphService.h"
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
    GraphServiceClient client(proto);

    try
    {
        trans->open();
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
    trans->close();
}
