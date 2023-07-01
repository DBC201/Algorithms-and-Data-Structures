#include "linked_list.h"

linked_list::linked_list()
{
    this -> head_node = 0;
    this -> tail_node = 0;
}

void linked_list::append(int data){
    if (head_node == 0){
        this -> head_node = new node(0, 0, data);
        this -> tail_node = this -> head_node;
    }else{
        this -> tail_node = new node(tail_node, 0, data);
    }
}

node *linked_list::return_index_node(int input_index){
    int index = 0;
    node *current_node = this -> head_node;
    while (current_node != 0){
        if (index == input_index){
            return current_node;
        }
        current_node = current_node -> return_next_node();
        index++;
    }
    return 0;
}

int linked_list::return_item_index(int input_index){
    node *current_node = return_index_node(input_index);
    if (current_node == 0){
        return -1;//if no such index exists
    }else{
        return return_index_node(input_index) -> return_data();
    }
}

void linked_list::replace_item(int input_index, int data){
    node *node_to_replace = return_index_node(input_index);
    if (node_to_replace == 0){
        return; //do nothing because no node with that index exists
    }else{
        node_to_replace = new node(node_to_replace -> return_previous_node(), node_to_replace -> return_next_node(), data);
    }
}

linked_list::~linked_list()
{
    //node class handles pointer memory destruction itself, can't delete an already deleted memory value
}
