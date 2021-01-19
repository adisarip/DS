#include <mpi.h>
#include <iostream>
using namespace std;

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

    if (world_size < 2)
    {
        cout << "World Size is "<< world_size << " should atleast be 2." << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    int pass_token;
    if (current_rank != 0)
    {
        // the process receive the token from a lesser ranked process
        MPI_Recv(&pass_token, 1, MPI_INT, current_rank-1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        cout << "proc:" << current_rank << " received " << pass_token << " from proc:" << current_rank-1 << endl;
    }
    else
    {
        // set the token value in case of process '0'
        pass_token = -100;
    }
    MPI_Send(&pass_token, 1, MPI_INT, (current_rank+1) % world_size, 0, MPI_COMM_WORLD);

    if (current_rank == 0)
    {
        MPI_Recv(&pass_token, 1, MPI_INT, world_size-1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        cout << "proc:" << current_rank << " received " << pass_token << " from proc:" << world_size-1 << endl;
    }

    // Finalize the MPI environment.
    MPI_Finalize();
}
