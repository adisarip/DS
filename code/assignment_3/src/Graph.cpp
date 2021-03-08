
#include "Graph.h"
#include <algorithm>
#include <iostream>

// Implementing Union-Find data structure

// Find the subset of the element with index 'idx'
// using path-compression technique
int Find(Subset subsets[], int idx)
{
    // find the root and make root as parent of 'idx'
    if (subsets[idx].parent != idx)
    {
        subsets[idx].parent = Find(subsets, subsets[idx].parent);
    }
    return subsets[idx].parent;
}

// A function that does union of two sets of x and y
// by using union by rank
void Union(Subset subsets[], int x, int y)
{
    int x_root = Find(subsets, x);
    int y_root = Find(subsets, y);

    if (subsets[x_root].rank < subsets[y_root].rank)
    {
        subsets[x_root].parent = y_root;
    }
    else if (subsets[x_root].rank > subsets[y_root].rank)
    {
        subsets[y_root].parent = x_root;
    }
    else
    {
        // If ranks are same, then make one as root and
        // increment its rank by one
        subsets[y_root].parent = x_root;
        subsets[x_root].rank++;
    }
}

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

    Subset* subsets = new Subset[mNodesCount * sizeof(Subset)];

    // create subsets with single elements
    for (int i = 0; i < mNodesCount; i++)
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
    for (int i=0; i < e_mst; i++)
    {
        cout << sMST[i]->src << " -- " << sMST[i]->dest << " == " << sMST[i]->weight << endl;
        sMinCost = sMinCost + sMST[i]->weight;
    }

    return sMinCost;
}

// Helper functions
