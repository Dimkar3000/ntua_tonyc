#ifdef _WIN32
#pragma comment(lib, "user32")
#endif

#include <iostream>
#include <string>
#include <algorithm>
#include <limits>
#include <ios>
#include <cstring>

extern "C" void _strcat(char *dest, char *source)
{
    strcat(dest, source);
}

extern "C" int16_t _strcmp(char *dest, char *source)
{
    return strcmp(dest, source);
}

extern "C" int16_t _strlen(char *str)
{
    return strlen(str);
}

extern "C" void _strcpy(char *dest, char *source)
{
    strcpy(dest, source);
}

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

extern "C" void _gets(int16_t n, char *s)
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
    char c = std::cin.get();
    std::cin.clear();
    return c;
}

extern "C" bool getb()
{
    std::string data;
    std::cin >> data;
    if (data[0] == 't' || data[0] == 'T')
    {
        return true;
    }
    return false;
}
