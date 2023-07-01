#ifndef NODE_H
#define NODE_H


class node
{
    public:
        node(node *root_node, int data, node *left_node, node *right_node);
        void set_root_node(node *input_node);
        void set_left_node(node *input_node);
        void set_right_node(node *input_node);
        node *return_root_node();
        node *return_left_node();
        node *return_right_node();
        int return_data();
        ~node();

    protected:

    private:
        node *root_node;
        node *left_node;
        node *right_node;
        int data;
};

#endif // NODE_H
