
#define _USE_MATH_DEFINES
#include <cmath>
#include <iostream>
#include <iomanip>
using namespace std;

int main(int argc, char* argv[])
{
    int N = 0;
    cout << "Enter N: ";
    cin >> N;
    cout << std::fixed << setprecision(6);

    double sum = 0;
    long int i = 0;
    double ref = (M_PI * M_PI) / 6;
    while(1)
    {
        sum += 1 / (double)((i+1)*(i+1));
        if (trunc(sum * 1000000) == trunc(ref * 1000000) || i > N)
        {
            break;
        }
        i++;
    }
    cout << "Computing series for " << i << " terms: " << sum << endl;
    cout << "Reference Value: " << ref << endl;
    return 0;


}
