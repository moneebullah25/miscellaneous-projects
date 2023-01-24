#pragma once
#include <iostream>

#define NULL 0

struct _Node
{
	int data;
	_Node* next;

	_Node(int data)
	{
		this->data = data;
		this->next = NULL;
	}
};

class CircularLinkedList
{
private:
	_Node* head;
	_Node* tail;
	unsigned int size;
public:
	CircularLinkedList(); // Initialize class variables
	void Insert(const int& data); // Insert _Node at the end of list (before head)
	void InsertBeforehead(const int& data); // Insert _Node before head
	void InsertAfterHead(const int& data); // Insert _Node after head
	void InsertBefore(const int& value, const int& data); // Insert before the given _Node
	void InsertAfter(const int& value, const int& data); // Insert after the given _Node
	void RemoveHead(); // Remove _Node at head
	bool RemoveNode(const int& data); // Remove certain _Node with given data
	bool Find(const int& data); // Return true if _Node found 
	bool Replace(const int& val, const int& val2); // Replace first only _Node of val with val2
	bool ReplaceAll(const int& val, const int& val2); // Replace all _Nodes of val data with val2
	int FindMax(); // Find max and return data of the _Node
	int FindMin(); // Find minimun and return data of the _Node
	void ShowList(); // Display entire list on terminal
	~CircularLinkedList(); // Deletes all _Nodes including head and set head and tail to NULL 
};

