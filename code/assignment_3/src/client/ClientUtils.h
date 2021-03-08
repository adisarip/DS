
#ifndef CLIENTUTILS_H
#define CLIENTUTILS_H

#include <string>
using namespace std;

// forward declarations
class GraphServiceClient;

void displayMenu();
void processUserCommand(GraphServiceClient* client, string inputCmd);

#endif
