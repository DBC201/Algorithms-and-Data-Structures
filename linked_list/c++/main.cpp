#include <iostream>
#include "node.h"
#include "linked_list.h"
//data can be configured to be any other abstract data type like a struct containing values etc. from class codes

using namespace std;

int main()
{
    linked_list any_list;
    for(int c=0; c<=100; c++){
        any_list.append(c);
    }
    cout << any_list.return_item_index(31) << endl;
    any_list.replace_item(31, 69);
    cout << any_list.return_item_index(31) << endl;
    return 0;
}
