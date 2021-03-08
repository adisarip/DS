
// Defining a Graph class storing an undirected graph and implementing Kruskals Algorithm
// to compute the cost of Minimum Spanning Tree using union-find data structure

#include <vector>
#include <unordered_set>
using namespace std;

// Union-Find Data Structure - Declarations
struct Subset
{
    int parent;
    int rank;
};
int Find(Subset subsets[], int idx);
void Union(Subset subsets[], int x, int y);

// Graph Class - Declarations
struct Edge
{
    Edge (){};
    Edge (int u, int v, int w):src(u),dest(v),weight(w){};
    int src; // Source Vertex
    int dest; // Destination Vertex
    int weight; // Edge Weight
};

class Graph
{
public:
    Graph(int nodeCount);
    ~Graph();
    void addEdge(const int u, const int v, const int w);
    int getMSTWeight();
private:
    int mNodesCount;
    int mEdgesCount;
    vector<Edge*> mEdgeList;
    unordered_set<int> mNodes;
};
