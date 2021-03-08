service GraphService
{
    oneway void addGraph(1:string graphId,
                         2:i32 nodeCount)
    i32 addEdge(1:string graphId,
                2:i32 srcNode,
                3:i32 dstNode,
                4:i32 edgeWeight)
    i32 getMSTWeight(1:string graphId)
}
