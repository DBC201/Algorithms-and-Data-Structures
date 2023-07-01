#ifndef BINARY_TREE_H
#define BINARY_TREE_H
#include "node.h"


class binary_tree
{
    public:
        binary_tree();
        void append(int data);
        int return_data(int level, int branch);
        void print();
        ~binary_tree();

    protected:

    private:
        int next_binary(int depth, int row_id, int binary_id, int current_depth);
        const int space_level;
        void print_tree(node *root, int space);
        node *root;
};

#endif // BINARY_TREE_H
