int x = 0;

int incr()
{
    x = x + 1;
    return 1;
}

int main()
{
    int sum = 0;
    sum = sum + (incr() || incr());
    sum = sum + (incr() && incr());
    return x;
}