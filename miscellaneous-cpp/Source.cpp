#include <iostream>
#include <array>
#include <vector>
#include "BinaryTree.h"
#include "CircularLinkedList.h"
#include "DoublyLinkedList.h"
#include "Stack.h"
#include "Queue.h"
#include "QueueUV.h"
#include "StackUV.h"
#include "Algorithms_6006.h"

int main()
{
	//////////// TREES //////////
	//tree_node* root = NULL;
	//tree::insert(root, 5);
	//tree::insert(root, 3);
	//tree::insert(root, 1);
	//tree::insert(root, 2);
	//tree::insert(root, 4);
	//tree::insert(root, 7);
	//tree::insert(root, 9);
	//tree::insert(root, 11);
	//tree::insert(root, 6);
	//tree::insert(root, 8);
	//tree::insert(root, 10);

	//unsigned int size = 0;
	//size = tree::getSize(root);

	//// By default 20 capacity of queue
	//queue* q = new queue();  
	//tree::bfs(root, q);

	//// By default 20 capcity of stack
	//stack* s = new stack();
	//tree::dfs(root, s);
	//std::cout << "\n";

	//// IN_ORDER_TRAVERSAL
	//queue* v = new queue();
	//tree::in_order_traversal(root, v);
	//while (v->getSize()) std::cout << v->pop()->data << " ";
	//std::cout << "\n";

	//// PRE_ORDER_TRAVERSAL
	//queue* v1 = new queue();
	//tree::pre_order_traversal(root, v1);
	//while (v1->getSize()) std::cout << v1->pop()->data << " ";
	//std::cout << "\n";

	//// POST_ORDER_TRAVERSAL
	//queue* v2 = new queue();
	//tree::post_order_traversal(root, v2);
	//while (v2->getSize()) std::cout << v2->pop()->data << " ";
	//std::cout << "\n";


	//delete q, s, v, v1, v2;	


	//// By default 20 capacity of queue
	//QueueUV* q1 = new QueueUV();
	//tree::bfs(root, q1);

	//// By default 20 capcity of stack
	//StackUV* s1 = new StackUV();
	//tree::dfs(root, s1);
	//std::cout << "\n";

	//// IN_ORDER_TRAVERSAL
	//QueueUV* v12 = new QueueUV();
	//tree::in_order_traversal(root, v12);
	//while (v12->size()) std::cout << v12->pop()->data << " ";
	//std::cout << "\n";

	//// PRE_ORDER_TRAVERSAL
	//QueueUV* v13 = new QueueUV();
	//tree::pre_order_traversal(root, v13);
	//while (v13->getSize()) std::cout << v13->pop()->data << " ";
	//std::cout << "\n";

	//// POST_ORDER_TRAVERSAL
	//QueueUV* v2 = new QueueUV();
	//tree::post_order_traversal(root, v2);
	//while (v2->getSize()) std::cout << v2->pop()->data << " ";
	//std::cout << "\n";


	Array* ar = new Array(std::vector<int>({9,8,7,6,5,4,3,2,1}));

	dsa_mit::find_peak_1D(ar);
	dsa_mit::merge_sort(ar, 0, ar->getsize());

	ar->printArray();

	heap* hp = new heap(ar);

	std::cin.get();

}

