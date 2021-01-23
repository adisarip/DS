
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


#include <mpi.h>
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
    fin.close();

    // input data verification
    if (index == size)
    {
        cout << "[INFO] Number of elements to be sorted: " << size << endl;
    }
    else
    {
        cout << "[WARN] Mismatch between input size " << size << " and no of elements " << index << endl;
        cout << "[INFO] Considering the input size as: " << index << endl;
        size = index;
    }
    return size;
}

void print_list(int numbers[], int size)
{
    cout << endl;
    for (int i=0; i<size; i++)
    {
        cout << numbers[i] << " ";
    }
    cout << endl;
}

int main(int argc, char* argv[])
{
    if (argc != 3)
    {
        cout << "[ERR]  Invalid input data." << endl;
        cout << "[INFO] Usage: ./run <input_file> <output_file>" << endl;
        return 0;
    }
    string input_file;
    string output_file;

    MPI_Init(&argc, &argv);

    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    int numbers[MAX_LIMIT];
    int size;

    if (rank == MASTER)
    {
        input_file = string(argv[1]);  // save the input file
        output_file = string(argv[2]); // save the output file
        size = parse_input_data(input_file, numbers);

        if (size > MAX_LIMIT)
        {
            cout << "[ERR]  Number of elements out-of-range: " << size << endl;
            cout << "[INFO] Accecpted input limits : 2 <= N <= 1000000" << endl;
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        else
        {
            // print the un-sorted list
            cout << endl;
            cout << "[INFO] Unsorted List: ";
            print_list(numbers, size);
        }
    }
    else
    {
        MPI_Status status;
        MPI_Recv(numbers,
                 MAX_LIMIT,
                 MPI_INT,
                 (rank-1)/2,
                 0,
                 MPI_COMM_WORLD,
                 &status);
        MPI_Get_count(&status, MPI_INT, &size);
    }

    int p_index = create_partition(numbers, 0, size-1);
    if (2*rank+2 < world_size)
    {
        // send to left  2*r+1
        MPI_Send(numbers,
                 p_index,
                 MPI_INT,
                 (2*rank)+1,
                 0,
                 MPI_COMM_WORLD);

        // send to right  2*r+2
        MPI_Send(&numbers[p_index+1],
                 (size - p_index - 1),
                 MPI_INT,
                 (2*rank)+2,
                 0,
                 MPI_COMM_WORLD);

        // receive from left  2*r+1
        MPI_Recv(numbers,
                 p_index,
                 MPI_INT,
                 (2*rank)+1,
                 0,
                 MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);

        // receive from right  2*r+2
        MPI_Recv(&numbers[p_index+1],
                 (size - p_index - 1),
                 MPI_INT,
                 (2*rank)+2,
                 0,
                 MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
    }
    else if (2*rank+1 < world_size)
    {
        // send to left  2*r+1
        MPI_Send(numbers,
                 p_index,
                 MPI_INT,
                 (2*rank)+1,
                 0,
                 MPI_COMM_WORLD);

        // perform quicksort on right half
        quicksort(numbers, p_index+1, size-1);

        // receive from left  2*r+1
        MPI_Recv(numbers,
                 p_index,
                 MPI_INT,
                 (2*rank)+1,
                 0,
                 MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);
    }
    else
    {
        quicksort(numbers, 0, size-1);
    }

    if (rank != MASTER)
    {
        // send back to parent (rank-1)/2 if not master process
        MPI_Send(numbers,
                 size,
                 MPI_INT,
                 (rank-1)/2,
                 0,
                 MPI_COMM_WORLD);
    }
    else
    {
        // print the sorted list
        cout << endl;
        cout << "[INFO] Sorted List: ";
        print_list(numbers, size);

        // write the sorted list into file
        ofstream fout;
        fout.open(output_file, ios::trunc | ios::out);
        for (int i=0; i<size; i++)
        {
            fout << numbers[i] << " ";
        }
        fout << endl;
        fout.close();
    }

    // Synchronizing all the processes before termination
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();
}
