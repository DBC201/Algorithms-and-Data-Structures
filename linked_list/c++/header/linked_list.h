#ifndef LINKED_LIST_H
#define LINKED_LIST_H
#include "header/node.h"


class linked_list
{
    public:
        linked_list();
        void append(int data);
        int return_item_index(int input_index);
        void replace_item(int input_index, int data);
        ~linked_list();

    protected:

    private:
        node *return_index_node(int input_index);
        node *head_node;
        node *tail_node;
};

#endif // LINKED_LIST_H
