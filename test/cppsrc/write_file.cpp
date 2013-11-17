#include <iostream>
#include <fstream>
#include <cstdlib>
using namespace std;

int main() {
	ofstream file;
	file.open ("numbers.txt");
	int N = 25000;
	for(int i=0; i<N; i++)
	{
		int Num = rand()%1000;
		file << Num << ",";
	}
	file.close();
	return 0;
}
