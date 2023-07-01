#include "node.h"
#include <iostream>

node::node(node *root_node, int data, node *left_node, node *right_node)
{
    if (root_node != 0){
        if (data <= root_node -> return_data()){
            root_node -> set_left_node(this);
        }else{
            root_node -> set_right_node(this);
        }
    }
    this -> root_node = root_node;

    if (left_node != 0){
        left_node -> set_root_node(this);
    }
    this -> left_node = left_node;

    if (right_node != 0){
        right_node -> set_root_node(this);
    }
    this -> right_node = right_node;

    this -> data = data;
}

void node::set_root_node(node *input_node){//use the following set functions with caution, there is no auto linking
   this -> root_node = input_node;
}

void node::set_left_node(node *input_node){
    this -> left_node = input_node;
}

void node::set_right_node(node *input_node){
    this -> right_node = input_node;
}

node *node::return_root_node(){
    return this -> root_node;
}

node *node::return_left_node(){
    return this -> left_node;
}

node *node::return_right_node(){
    return this -> right_node;
}

int node::return_data(){
    return this -> data;
}

node::~node()
{
    delete this -> root_node;
    delete this -> left_node;
    delete this -> right_node;
}
