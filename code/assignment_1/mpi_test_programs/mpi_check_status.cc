#include <mpi.h>
#include <iostream>
#include <stdlib.h>
#include <time.h>
using namespace std;

// An example to check the MPI_Status object received in the MPI_Recv() call.
const int MAX_NUMBERS = 100;

int main(int argc, char** argv)
{
    // Initialize the MPI environment
    MPI_Init(NULL, NULL);

    // Get the number of processes
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);
    if (world_size != 2)
    {
        cout << "World Size is "<< world_size << " should be 2." << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    // Get the rank of the process
    int current_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &current_rank);
    int partner_rank = (current_rank + 1) % 2;

    cout << "MPI Master is: "  << MASTER << endl;

    int numbers[MAX_NUMBERS];
    int msg_size;
    if (current_rank == 0)
    {
        // send a random number of integers to another process
        srand(time(NULL));
        msg_size = (rand() / (float)RAND_MAX) * MAX_NUMBERS;
        MPI_Send(&numbers[10], 10, MPI_INT, partner_rank, 0, MPI_COMM_WORLD);
        cout << "proc:" << current_rank << " sent " << 10 << " numbers to proc:" << partner_rank << endl;
    }
    else
    {
        MPI_Status status;
        MPI_Recv(numbers, MAX_NUMBERS, MPI_INT, partner_rank, 0, MPI_COMM_WORLD, &status);
        MPI_Get_count(&status, MPI_INT, &msg_size);
        cout << "proc:" << current_rank << " received " << msg_size << "/" << MAX_NUMBERS << " from proc:" << partner_rank << endl;
    }

    // Finalize the MPI environment.
    MPI_Finalize();
}
