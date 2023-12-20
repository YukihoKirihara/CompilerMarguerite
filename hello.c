int x = 1;
const int y = 2;
void f()
{
    x = x + 1;
}

int main()
{
    f();
    f();
    f();
    f();
    f();
    f();
    return x;
}