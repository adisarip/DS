
#include "Graph.h"
#include "GraphUtils.h"
#include <algorithm>

// Graph Class - Definitions
// Constructor
Graph::Graph(int nodeCount)
:mNodesCount(nodeCount)
,mEdgesCount(0)
{
}

Graph::~Graph()
{
    // Destructor
    mNodesCount = 0;
    mEdgesCount = 0;
    for(Edge* pEdge : mEdgeList)
    {
        delete pEdge;
    }
}

void Graph::addEdge(const int u, const int v, const int w)
{
    // Add edge in the graph
    mEdgeList.push_back(new Edge(u,v,w));
    mNodes.insert(u);
    mNodes.insert(v);
    mNodesCount = (mNodesCount < mNodes.size()) ? mNodes.size() : mNodesCount;
    mEdgesCount++;
}

bool compare(Edge* e1, Edge* e2)
{
    return (e1->weight < e2->weight);
}


int Graph::getMSTWeight()
{
    // return weight of the Minimum Spanning Tree
    Edge* sMST[mNodesCount];

    sort(mEdgeList.begin(), mEdgeList.end(), compare);

    Subset* subsets = new Subset[(mNodesCount+1) * sizeof(Subset)];

    // create subsets with single elements
    for (int i = 1; i <= mNodesCount; i++)
    {
        subsets[i].parent = i;
        subsets[i].rank = 0;
    }

    // iterate over all edges
    int e_mst = 0; // for iterating over full list of edges
    int j = 0; // for iterating over sorted edges
    while (e_mst < mNodesCount-1 && j < mEdgesCount)
    {
        Edge* pNextEdge = mEdgeList[j++];
        int x = Find(subsets, pNextEdge->src);
        int y = Find(subsets, pNextEdge->dest);
        if (x != y)
        {
            sMST[e_mst++] = pNextEdge;
            Union(subsets, x, y);
        }
    }

    // compute the MST weight
    int sMinCost = 0;

    // verify for the edges in the MST Union-Find returns the same subset
    // if not then the graph is not connected and MST donot exists
    int sParent = Find(subsets, sMST[0]->src);
    for (int i = 1; i < e_mst; i++)
    {
        if (sParent != Find(subsets, sMST[i]->src))
        {
            sMinCost = -1;
            break;
        }
    }
    if (0 == sMinCost)
    {
        for (int i=0; i < e_mst; i++)
        {
            sMinCost = sMinCost + sMST[i]->weight;
        }
    }

    return sMinCost;
}

// Helper functions
