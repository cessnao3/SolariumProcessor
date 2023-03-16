#include <iostream>
#include <fstream>
#include <string>
#include <vector>


std::vector<std::string> parse_line(const std::string& l) {
    return {};
}


int main(const int argc, const char* argv[]) {
    if (argc < 2) {
        std::cerr << "must provide a file as an argument to compile" << std::endl;
        return 1;
    }

    std::ifstream input_file(argv[1]);
    if (!input_file.is_open()) {
        std::cerr << "unable to open '" << argv[1] << "' file" << std::endl;
        return 2;
    }

    std::vector<std::string> tokens;

    std::string l;
    while (std::getline(input_file, l)) {
        const auto tks = parse_line(l);
        tokens.insert(tokens.end(), tks.begin(), tks.end());
    }

    return 0;
}
