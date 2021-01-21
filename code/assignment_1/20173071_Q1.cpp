
#include <mpi.h>
#include <iostream>
#include <cstdlib>
#include <iomanip>
using namespace std;

#define _USE_MATH_DEFINES
#include <cmath>
const double REF_VALUE = (M_PI * M_PI) / 6;

// Compute sum of the partial series for each parallel process
double compute_partial_sum(long int start, long int count)
{
    double psum = 0;
    long int i = start;
    while (i <= count)
    {
        psum += 1 / (double)(i*i);
        i++;
    }
    return psum;
}

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        cout << "Usage: a.out <number_of_terms_in_series>" << endl;
        return 0;
    }

    long int N = atol(argv[1]);

    MPI_Init(NULL, NULL);

    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    long int n_per_proc = N / world_size;
    if (rank == world_size-1)
    {
        // for the last process also add the remaining chunk left
        n_per_proc = n_per_proc + N % world_size;
    }

    // start with 1 while computing sum
    long int start_num = rank * n_per_proc + 1;

    // Compute partial sum
    double partial_sum = compute_partial_sum(start_num, n_per_proc);

    // Compute final sum by reduction technique
    double final_sum;
    MPI_Reduce(&partial_sum, &final_sum, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

    if (rank == 0)
    {
        cout << fixed << setprecision(6) << "Sum of the Series = " << final_sum
                                         << " | Reference = " << REF_VALUE << endl;
    }

    // Synchronizing all the processes before termination
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();
}
