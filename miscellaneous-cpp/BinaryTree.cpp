#include "BinaryTree.h"


tree_node::tree_node(int data)
{
	this->data = data;
	this->l_child = NULL;
	this->r_child = NULL;
}


// QUEUE //
queue::queue(int size)
{
	pointers.reserve(20);
}

void queue::push(tree_node& add)
{
	pointers.emplace_back(&add);
}

tree_node* queue::pop()
{
	if (pointers.size())
	{
		tree_node* val = pointers[0];
		pointers.erase(pointers.begin());
		return val;
	}
	else {
		return NULL;
	}
}

unsigned int queue::getSize()
{
	return pointers.size();
}

queue::~queue()
{
	pointers.clear();
}


// STACK //
stack::stack(int size)
{
	pointers.reserve(20);
}

void stack::push(tree_node& add)
{
	pointers.push_back(&add);
}

tree_node* stack::pop()
{
	if (pointers.size())
	{
		tree_node* val = pointers[pointers.size() - 1];
		pointers.erase(pointers.begin() + (pointers.size() - 1));
		return val;
	}
	else {
		return NULL;
	}
}

unsigned int stack::getSize()
{
	return pointers.size();
}

stack::~stack()
{
	pointers.clear();
}


// NAMESPACE TREE
int tree::findMin(tree_node* root)
{
	if (root == NULL)
		return -1;
	else if (root->l_child == NULL)
		return root->data;
	else
		return tree::findMin(root->l_child);
}

void tree::insert(tree_node*& root, int data)
{
	if (root == NULL)
	{
		root = new tree_node(data);
		
	}
	else if (root != NULL)
	{
		if (data < root->data)
			tree::insert(root->l_child, data);
		else
			tree::insert(root->r_child, data);
	}
}

void tree::remove(tree_node*& root, tree_node*& prev, int data)
{
	if (root == NULL)
	{
		return;
	}
	else if (root->data == data)
	{
		// Node has no CHILD
		if (root->r_child == NULL && root->l_child == NULL)
		{
			prev->l_child = (prev->l_child == root) ? NULL : prev->l_child;
			prev->r_child = (prev->r_child == root) ? NULL : prev->r_child;
			free(root);
		}
		// Node has one left CHILD
		else if (root->r_child == NULL && root->l_child != NULL)
		{
			prev->l_child = (prev->l_child == root) ? root->l_child : prev->l_child;
			prev->r_child = (prev->r_child == root) ? root->l_child : prev->r_child;
			
		}
		// Node has one right CHILD
		else if (root->r_child != NULL && root->l_child == NULL)
		{
			prev->l_child = (prev->l_child == root) ? root->r_child : prev->l_child;
			prev->r_child = (prev->r_child == root) ? root->r_child : prev->r_child;
		
		}
		// Node has more than one right and left child
		else if (root->r_child != NULL && root->l_child != NULL)
		{
			root->data = findMin(root->r_child);
			tree::remove(root->r_child, root, root->data);
		}
	}
	else
	{
		if (data < root->data)
			tree::remove(root->l_child, root, data);
		else
			tree::remove(root->r_child, root, data);
	}
}

bool tree::search(tree_node* root, int data)
{
	if (root == NULL)
	{
		return false;
	}
	else if (root->data == data)
	{
		return true;
	}
		
	if (data < root->data)
		return (false || tree::search(root->l_child, data));
	else
		return (false || tree::search(root->r_child, data));
}

void tree::bfs(tree_node* root, queue*& q)
{
	if (root == NULL)
	{
		std::cout << "\n";
		return;
	}
	else
	{
		if (root->l_child != NULL)
		{
			q->push(*(root->l_child));
		}
		if (root->r_child != NULL)
		{
			q->push(*(root->r_child));
		}
		std::cout << root->data << " ";
		bfs(q->pop(), q);
		
	}
}


void tree::dfs(tree_node* root, stack*& s)
{
	if (root == NULL)
	{
		return;
	}
	else
	{
		std::cout << root->data << " ";
		if (root->l_child != NULL)
		{
			s->push(*(root->l_child));
		}
		dfs(s->pop(), s);
		if (root->r_child != NULL)
		{
			s->push(*(root->r_child));
			dfs(s->pop(), s);
		}
	}
}

unsigned int tree::getSize(tree_node* root)
{
	if (root == NULL)
		return 0;
	else 
	{	
		return 1 + tree::getSize(root->l_child) + tree::getSize(root->r_child);
	}
}

// Left Subtree then root and then right subtree
void tree::in_order_traversal(tree_node* root, queue*& q)
{
	if (root == NULL)
	{
		return;
	}

	if (root->l_child)
	{
		in_order_traversal(root->l_child, q);
	}
	q->push(*(root));
	if (root->r_child)
	{
		in_order_traversal(root->r_child, q);
	}
	
}

void tree::pre_order_traversal(tree_node* root, queue*& q)
{
	if (root == NULL)
	{
		return;
	}

	q->push(*(root));
	if (root->l_child)
	{
		pre_order_traversal(root->l_child, q);
	}
	if (root->r_child)
	{
		pre_order_traversal(root->r_child, q);
	}

}

void tree::post_order_traversal(tree_node* root, queue*& q)
{
	if (root == NULL)
	{
		return;
	}

	if (root->l_child)
	{
		post_order_traversal(root->l_child, q);
	}
	if (root->r_child)
	{
		post_order_traversal(root->r_child, q);
	}

	q->push(*(root));
}
