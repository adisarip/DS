
#include <mpi.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>
using namespace std;

const int MASTER = 0;

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

int parse_input_data(string input_file)
{
    ifstream fin;
    fin.open(input_file, ios::in);

    string s;
    getline(fin, s); // get the number of elements is series
    fin.close();
    return stol(s);
}

int main(int argc, char* argv[])
{
    if (argc != 3)
    {
        cout << "[ERR]  Invalid input data." << endl;
        cout << "[INFO] Usage: ./a.out <input_file> <output_file>" << endl;
        return 0;
    }

    long int N;
    string input_file;
    string output_file;

    MPI_Init(&argc, &argv);

    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    if (rank == MASTER)
    {
        input_file = string(argv[1]);  // save the input file
        output_file = string(argv[2]); // save the output file
        N = parse_input_data(input_file);
    }

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

    if (rank == MASTER)
    {
        cout << fixed << setprecision(6) << "Sum of the Series = " << final_sum
                                         << " | Reference = " << REF_VALUE << endl;

        ofstream fout;
        fout.open(output_file, ios::trunc | ios::out);
        fout << "Number of terms in Series: " << N << endl;
        fout << fixed << setprecision(6) << "Sum of the Series = " << final_sum << " | Reference = " << REF_VALUE << endl;
        fout.close();
    }

    // Synchronizing all the processes before termination
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();
}
