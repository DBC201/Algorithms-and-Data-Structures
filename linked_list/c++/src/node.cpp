#include "node.h"

node::node(node *previous_node, node *next_node, int data)
{
    this -> previous_node = previous_node;
    if (previous_node != 0){
        previous_node -> set_next_node(this);
    }
    this -> next_node = next_node;
    if (next_node != 0){
        next_node -> set_previous_node(this);
    }
    this -> data = data;
}

node *node::return_next_node(){
    return this -> next_node;
}

node *node::return_previous_node(){
    return this -> previous_node;
}

void node::set_next_node(node *next_node){//setting nodes by hand are not reccomended because there is no auto linking like there is in constructor
    this -> next_node = next_node;
}

void node::set_previous_node(node *previous_node){
    this -> previous_node = previous_node;
}

int node::return_data(){
    return this -> data;
}

void node::set_data(int data){
    this -> data = data;
}

node::~node()
{
    delete this -> previous_node;
    delete this -> next_node;
}
