
#include "ClientUtils.h"
#include "../service/GraphService.h"
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <memory>
#include <iostream>
#include <fstream>

using namespace std;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

int main(int argc, char* argv[])
{
    shared_ptr<TTransport> trans;
    trans = make_shared<TSocket> ("localhost", 9090);
    trans = make_shared<TBufferedTransport> (trans);
    auto proto = make_shared<TBinaryProtocol> (trans);
    GraphServiceClient client(proto);

    fstream iFile;
    fstream oFile;
    streambuf* sb_cin = cin.rdbuf();
    streambuf* sb_cout = cout.rdbuf();
    bool isInputFileSupplied = false;
    bool isOutputFileSupplied = false;
    bool isCmdMode = true;
    if (argc == 1)
    {
        // no input files suplied
        // nothing to do here
    }
    else if (argc == 2)
    {
        // input file is supplied
        iFile.open(string(argv[1]), ios::in);
        cin.rdbuf(iFile.rdbuf());
        isInputFileSupplied = true;
        isCmdMode = false;
    }
    else if (argc == 3)
    {
        // input and output files both supplied
        iFile.open(string(argv[1]), ios::in);
        oFile.open(string(argv[2]), ios::out|ios::trunc);
        cin.rdbuf(iFile.rdbuf());
        cout.rdbuf(oFile.rdbuf());
        isInputFileSupplied = true;
        isOutputFileSupplied = true;
        isCmdMode = false;
    }
    else
    {
        // incorrect number of arguments - exit
        return -1;
    }

    try
    {
        trans->open();

        if (isInputFileSupplied)
        {
            while (1)
            {
                string sUserInput;
                if (isCmdMode)
                {
                    cout << "Command: ";
                    getline(cin, sUserInput);
                }
                else
                {
                    getline(cin, sUserInput);
                    if (cin.eof())
                    {
                        cin.rdbuf(sb_cin);
                        cout.rdbuf(sb_cout);
                        cout << "[INFO] Input file processed" << endl;
                        cout << "[INFO] Switching to Command Mode for any additional operations" << endl;
                        displayMenu();
                        isCmdMode = true;
                        continue;
                    }
                }
                if (sUserInput == "-1")
                {
                    break;
                }
                else if (sUserInput == "")
                {
                    // skip empty lines
                    continue;
                }
                processUserCommand(&client, sUserInput);
            }
        }
        else
        {
            cout << "[INFO] Performing Graph Operations using RPC" << endl;
            cout << "[INFO] Operations Supported are ..." << endl;
            displayMenu();
            while (1)
            {
                string sUserInput;
                cout << "Command: ";
                getline(cin, sUserInput);
                if (sUserInput == "-1")
                {
                    break;
                }
                else if (sUserInput == "")
                {
                    // skip empty lines
                    continue;
                }
                processUserCommand(&client, sUserInput);
            }
        }

    }
    catch(...)
    {
        cout << "[ERROR] Client Caught an Exception" << endl;
    }
    trans->close();
}
