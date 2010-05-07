#include <iostream>
#include <fstream>

using namespace std;

int main(){
	
	ifstream file;
	file.open("test.ns");
	char buffer[100];
	if(file.is_open()) {
		while(!file.eof()) {
			file >> buffer;
			cout << buffer;
		}
	}
	file.close();

	return 0;
}