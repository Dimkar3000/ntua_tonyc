
#include <iostream>
#include <string>
#include <algorithm>
#include <limits>
#include <ios>

extern "C" void puti(int16_t a) {
    std::cout << a;
}

extern "C" void putb(bool a) {
    std::cout << (a ? "true" : "false");
}
//struct t{};
extern "C" void puti8(char c) {
    std::cout << c;
}
extern "C" void putstring(char* s) {
    std::cout << s;
}

extern "C" void gets(int16_t n, char* s) {
    std::cin.clear();
//    if(std::cin.peek() == -1) {
//        std::cin.clear();
//    }
    if(s == 0) {
        return;
    }
    std::cin.get(s,n,'\n');
    if(std::cin.peek() == '\n') {
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
//    std::cout << "left: " << std::cin.peek() << std::endl;
    }
//    s[n] = '\0';
}

extern "C" int16_t geti() {
    std::cin.clear();
    int16_t a;
    std::cin >> a;
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    return a;
}

extern "C" char getcchar() {
    std::cin.clear();
    return std::cin.get();
}


extern "C" bool getb() {
    std::cin.clear();
    std::string data;
    std::getline(std::cin,data);
//    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    std::transform(data.begin(), data.end(), data.begin(),
        [](unsigned char c){ return std::tolower(c); });
    if (data == "true") {
        return true;
    }
    return false;
}
