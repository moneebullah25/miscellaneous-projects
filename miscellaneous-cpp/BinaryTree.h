#pragma once

#include <iostream>
#include <string>
#include <vector>

struct tree_node
{
public:
	int data;
	tree_node* l_child;
	tree_node* r_child;
public:
	tree_node(int data = -1);
};

class queue
{
private:
	std::vector <tree_node*> pointers;
public:
	queue(int size=20);
	void push(tree_node& add);
	tree_node* pop();
	unsigned int getSize();
	~queue();
};

class stack
{
private:
	std::vector <tree_node*> pointers;
public:
	stack(int size = 20);
	void push(tree_node& add);
	tree_node* pop();
	unsigned int getSize();
	~stack();
};

namespace tree
{
	int findMin(tree_node* root);
	void insert(tree_node*& root, int data);
	void remove(tree_node*& root, tree_node*& prev, int data);
	bool search(tree_node* root, int data);
	void bfs(tree_node* root, queue*& q);
	void dfs(tree_node* root, stack*& s);
	void deleteTree(tree_node*& root);
	unsigned int getSize(tree_node* root);
	void in_order_traversal(tree_node* root, queue*& q);
	void pre_order_traversal(tree_node* root, queue*& q);
	void post_order_traversal(tree_node* root, queue*& q);
}