#pragma once
#include <iostream>

struct Node
{
	int data;
	Node* next;
	Node* back;
	Node();
	Node(int data);
};

class DoublyLinkedList
{

private:
	Node* m_head;
	Node* m_tail;
	unsigned int m_size;

public:
	DoublyLinkedList(); // Initialize class variables
	Node* AddAtTail(const int& data); // Add after the tail
	Node* AddAtHead(const int& data); // Add before the head
	Node* AddAtIndex(const int& index, const int& data); // Add node after the index node
	void Display(bool asc = true); // if asc = true then print in ascending order
	int Find(const unsigned int& data);  // Return index if data found
	int& At(const unsigned int& index); // Return data at index passed by arg
	void Replace(const int& data, const int& value); // Replace single occurance of data with the value 
	void RemoveAt(const unsigned int& index); // Remove data at the index passed as a argument 
	unsigned int Length(); // Return the length of the list
	void Pop(); // Removes the last element of the list 
	~DoublyLinkedList(); // Delete full list and set head and tail to NULL

};

