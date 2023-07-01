#include "binary_tree.h"
#include <iostream>
using namespace std;

binary_tree::binary_tree(): space_level(4)
{
    this -> root = 0;
}

void binary_tree::append(int data){
    if (this -> root == 0){
        this -> root = new node(0, data, 0, 0);
        return;
    }else{
        node *current_node = this -> root;
        node *previous_node;
        while (1){
            if (current_node == 0){
                current_node = new node(previous_node, data, 0, 0);
                return;
            }else if (data <= current_node -> return_data()){
                previous_node = current_node;
                current_node = current_node -> return_left_node();
            }else{
                previous_node =current_node;
                current_node = current_node -> return_right_node();
            }
        }
    }
}

int binary_tree::return_data(int level, int branch){
    node *current_node = this -> root;
    for (int i=1;i<=level;i++){
        int next_pos = next_binary(level, branch, branch%2, i);
        if (next_pos == 0){
            current_node = current_node -> return_left_node();
        }else if(next_pos == 1){
            current_node = current_node -> return_right_node();
        }else{
            //error check here
        }
    }
    return current_node -> return_data();
}

int binary_tree::next_binary(int depth, int row_id, int binary_id, int current_depth){
    if (depth == current_depth){
        return binary_id;
    }
    return next_binary(depth-1, row_id/2, (row_id/2)%2, current_depth);
}

void binary_tree::print(){
    print_tree(this -> root, 0);
}

void binary_tree::print_tree(node *root, int space){
    if (root == 0){
        return;
    }
    space += space_level;
    print_tree(root -> return_right_node(), space);
    cout << endl;
    for (int i=space_level; i<space; i++){
        cout << " ";
    }
    cout << root-> return_data() << endl;
    print_tree(root -> return_left_node(), space);
}

binary_tree::~binary_tree()
{
    //node class handles deletion
}
