#include <stdio.h>

void fastscan(int &number)
{
    bool negative = false;
    register int c;
    number = 0;
    c = getchar_unlocked();
    while(c!='-' && (c<48 || c>57)) c = getchar_unlocked();
    if (c=='-')
    {
        negative = true;
        c = getchar_unlocked();
    }
    for (; (c>47 && c<58); c=getchar_unlocked())
        number = number *10 + c - 48;
    if (negative)
        number *= -1;
}
 

int main()
{
    int number;
    fastscan(number);
    cout << number << "\n";
    return 0;
}
