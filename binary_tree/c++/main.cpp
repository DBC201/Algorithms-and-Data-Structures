#include <iostream>
#include "binary_tree.h"
#include <time.h>

using namespace std;

int main()
{
    binary_tree tree;
    srand(time(0));
    for (int i=0;i<40;i++){
        tree.append(rand());
    }
    tree.print();
    return 0;
}
