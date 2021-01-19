#include <mpi.h>
#include <iostream>
using namespace std;

const int PING_PONG_LIMIT = 10;

int main(int argc, char** argv)
{
    // Initialize the MPI environment
    MPI_Init(NULL, NULL);

    // Get the number of processes
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    // Get the rank of the process
    int current_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &current_rank);

    if (world_size != 2)
    {
        cout << "World Size not 2 invalid number of processors." << endl;
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    int ping_pong_count = 0;
    int partner_rank = (current_rank + 1) % 2;
    while (ping_pong_count < PING_PONG_LIMIT)
    {
        if (current_rank == ping_pong_count % 2)
        {
            ping_pong_count++;
            MPI_Send(&ping_pong_count, 1, MPI_INT, partner_rank, 0, MPI_COMM_WORLD);
            cout << "proc:" << current_rank << " sent ping_pong=" << ping_pong_count << " to proc:" << partner_rank << endl;
        }
        else
        {
            MPI_Recv(&ping_pong_count, 1, MPI_INT, partner_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            cout << "proc:" << current_rank << " received ping_pong=" << ping_pong_count << " from proc:" << partner_rank << endl;
        }
    }

    // Finalize the MPI environment.
    MPI_Finalize();
}
