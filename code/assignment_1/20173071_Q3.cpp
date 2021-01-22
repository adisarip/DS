
#include <mpi.h>
#include <iostream>
#include <cstring>
#include <fstream>
#include <sstream>
#include <vector>
#include <set>
#include <queue>
using namespace std;

const int MASTER = 0;
const int V_MAX = 100;
const int E_MAX = 500;

int V; // number of Vertices
int E; // number of Edges
vector<vector<pair<int,int>>> G; // graph data structure
vector<int> colors;
bool visited[V_MAX] = {false};

void print_graph()
{
    cout << "G(" << V << "," << E << "):" << endl;
    for (int i=1; i<=V; i++)
    {
        cout << i << " : {"<< flush;
        for (auto x : G[i])
        {
            cout << "(" << x.first << "," << x.second << "),";
        }
        cout << "}" << endl;
    }
}

void print_colors()
{
    for (int i=1; i<=E; i++)
    {
        cout << "Edge " << i << " is colored " << colors[i] << endl;
    }
}

int create_graph(string input_file)
{
    int start_index = -1;
    ifstream fin;
    fin.open(input_file, ios::in);

    bool isFirstEntry = true;
    string line;
    int edge_count = 0;

    while (getline(fin, line))
    {
        string token;
        istringstream token_stream(line);
        if (isFirstEntry)
        {
            // read the graph paramters
            token_stream >> token;
            V = stoi(token);
            token_stream >> token;
            E = stoi(token);
            isFirstEntry = false;

            // initialize the graph data structures
            G.resize(V+1);
            colors.resize(E+1,-1);
        }
        else
        {
            // create the undirected graph
            edge_count++;
            string su, sv;
            token_stream >> su;
            token_stream >> sv;
            int u = stoi(su);
            int v = stoi(sv);
            G[u].push_back(make_pair(v,edge_count));
            G[v].push_back(make_pair(u,edge_count));
            if (start_index < 0) start_index = u;
        }
    }

    // input data verification
    if (edge_count != E)
    {
        cout << "[ERROR] Mismatch between input size E=" << E << " and no of entries=" << edge_count << endl;
        start_index = -1;
    }
    return start_index;
}

void color_graph(int node)
{
    queue<int> Q;
    int c = 1;
    set<int> colored;

    // check if node already visited
    if (visited[node])
    {
        return;
    }

    // mark the current node visited
    visited[node] = true;

    // traverse all edges of current node
    for (int i=0; i<G[node].size(); i++)
    {
        // if already colored insert into the set
        if (colors[G[node][i].second] != -1)
        {
            colored.insert(colors[G[node][i].second]);
        }
    }

    for (int i=0; i<G[node].size(); i++)
    {
        // If node not visited insert into the queue
        if (visited[G[node][i].first] == false)
        {
            Q.push(G[node][i].first);
        }
        if (colors[G[node][i].second] == -1)
        {
            while (colored.find(c) != colored.end())
            {
                c++;
            }
            // save it in the colors vector
            colors[G[node][i].second] = c;
            // add it to the set
            colored.insert(c);
            c++;
        }
    }

    while (!Q.empty())
    {
        int next_node = Q.front();
        Q.pop();
        color_graph(next_node);
    }
    return;
}

int main(int argc, char* argv[])
{
    if (argc != 3)
    {
        cout << "[ERR]  Invalid input data." << endl;
        cout << "[INFO] Usage: ./run <input_file> <output_file>" << endl;
        return 0;
    }

    string input_file = string(argv[1]);  // save the input file
    string output_file = string(argv[2]); // save the output file
    int start_index = create_graph(input_file);
    if (start_index < 0)
    {
        exit(1);
    }

    color_graph(start_index);
    print_graph();
    print_colors();

    /*MPI_Init(&argc, &argv);

    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);



    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();*/
}
