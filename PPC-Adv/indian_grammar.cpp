#include <iostream>
#include <string>

using namespace std;

class Grammar {
 public:
	Grammar(string file_path);
};

Grammar::Grammar(string file_path) {
	cout << file_path << "\n";
	ifstream file(file_path);
	string line;
	while (get_line(file, line)) {
		cout << line << "\n";
	}
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		cout << "Va rugam dati ca argument calea fisierului ce descrie gramatica.";
	}
	auto grammar = new Grammar(string(argv[1]));
	return 0;
}
