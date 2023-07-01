#ifndef NODE_H
#define NODE_H


class node
{
    public:
        node(node *previous_node, node *next_node, int data);
        node *return_next_node();
        node *return_previous_node();
        void set_next_node(node *next_node);
        void set_previous_node(node *previous_node);
        void set_data(int data);
        int return_data();
        ~node();

    protected:

    private:
        node *previous_node;
        node *next_node;
        int data;
};

#endif // NODE_H
