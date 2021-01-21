
// Assignment-1:
// Q2: Implement a parallel quick sort algorithm on an array of numbers.
// 2 <= N <= 1000000

/*
Sample Input:
7
9 3 -11 100 2 4 1
Sample Output:
-11 1 2 3 4 9 100
*/


//#include <mpi.h>
#include <iostream>
#include <cstring>
#include <fstream>
#include <sstream>
using namespace std;

const int MASTER = 0;
const int MAX_LIMIT = 1000000;

void swap(int* a, int* b)
{
    int t = *a; *a = *b; *b = t;
}

int create_partition(int list[],
                     int start_index,
                     int end_index)
{
    int pivot = list[end_index];   // pivot element
    int i = (start_index - 1); // index of smaller element
    for (int j = start_index; j < end_index; j++)
    {
        if (list[j] < pivot)
        {
            i++;
            swap(&list[i], &list[j]);
        }
    }
    swap(&list[i+1], &list[end_index]);
    return (i+1);
}

void quicksort(int list[],
               int start_index,
               int end_index)
{
    if (start_index < end_index)
    {
        int partion_index = create_partition(list, start_index, end_index);
        quicksort(list, start_index, partion_index-1);
        quicksort(list, partion_index+1, end_index);
    }
}

int parse_input_data(string input_file, int numbers[])
{
    ifstream fin;
    fin.open(input_file, ios::in);

    string s;
    getline(fin, s); // get the input size
    int size = stoi(s);

    getline(fin, s); // get the element list
    istringstream token_stream(s);
    int index = 0;
    string token;
    while (getline(token_stream, token, ' '))
    {
        numbers[index] = stol(token);
        index++;
    }

    // input data verification
    if (index == size)
    {
        cout << "[INFO]  Number of elements to be sorted: " << size << endl;
    }
    else
    {
        cout << "[WARN] Mismatch between input size " << size << " and no of elements " << index << endl;
        cout << "[INFO] Considering the input size as: " << index << endl;
        size = index;
    }
    return size;
}

int main(int argc, char* argv[])
{
    int numbers[MAX_LIMIT];
    if (argc != 3)
    {
        cout << "[ERROR] Invalid input data." << endl;
        cout << "[INFO]  Usage: ./run <input_file> <output_file>" << endl;
        return 0;
    }

    string input_file = string(argv[1]);  // save the input file
    string output_file = string(argv[2]); // save the output file
    int N = parse_input_data(input_file, numbers);

    if (N > MAX_LIMIT)
    {
        cout << "[ERROR] Number of elements out-of-range: " << N << endl;
        cout << "[INFO]  Accecpted input limits : 2 <= N <= 1000000" << endl;
        return 0;
    }

    MPI_Init(NULL, NULL);

    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    if (rank != MASTER)
    {
        MPI_Recv();
        int p_index = create_partition();
        if (2*rank+2 <= world_size)
        {
            MPI_Send(); // send to left  2*r+1
            MPI_send(); // send to right 2*r+2

            MPI_Recv(); // receive from left  2*r+1
            MPI_Recv(); // receive from left  2*r+2

            MPI_Send(); // send back to parent (rank-1)/2
        }
        else if (2*rank+1 <= world_size)
        {
            MPI_Send(); // send to left  2*r+1
            quicksort(); // perform quicksort on right
            MPI_Recv(); // receive from left  2*r+1
            MPI_Send(); // concatenate send back to parent (rank-1)/2
        }
        else
        {
            quicksort();
        }
    }

    // fix the starting index for each partial list
    // & get the partial list from the main list
    int start_index = rank * n_per_proc;
    int partial_list[n_per_proc];
    memcpy(partial_list, &numbers[start_index], sizeof(int)*n_per_proc);*/

    //quicksort(partial_list, n_per_proc, rank);
    quicksort(numbers, 0, N-1);

    cout << endl;
    for (int i=0; i<N; i++)
    {
        cout << numbers[i] << " ";
    }
    cout << endl;

    /*if (rank == 0)
    {
        // print/write the sorted list into a file
    }

    // Synchronizing all the processes before termination
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();*/
}
