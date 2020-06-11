#include <iostream>
#include <string>
#include <algorithm>
#include <limits>
#include <ios>

// This forces the std abs to clear the upperbits
extern "C" int16_t _abs(int16_t a)
{
    return abs(a);
}

extern "C" int16_t ord(char c)
{
    return c;
}

extern "C" char chr(int16_t a)
{
    return a;
}

extern "C" void
puti(int16_t a)
{
    std::cout << a;
}

extern "C" void putb(bool a)
{
    std::cout << (a ? "true" : "false");
}

extern "C" void puti8(char c)
{
    std::cout << c;
}
extern "C" void putstring(char *s)
{
    std::cout << s;
}

extern "C" void gets(int16_t n, char *s)
{
    std::cin.clear();
    if (s == 0)
    {
        return;
    }
    std::cin.get(s, n, '\n');
    if (std::cin.peek() == '\n')
    {
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }
}

extern "C" int16_t geti()
{
    std::cin.clear();
    int16_t a;
    std::cin >> a;
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    return a;
}

extern "C" char getcchar()
{
    std::cin.clear();
    return std::cin.get();
}

extern "C" bool getb()
{
    std::cin.clear();
    std::string data;
    std::getline(std::cin, data);
    if (data[0] == 't' || data[0] == 'T')
    {
        return true;
    }
    return false;
}
