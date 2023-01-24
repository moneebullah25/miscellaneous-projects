//#pragma once
//#include <iostream>
//
//#define NULL 0
//
//struct _Node
//{
//	int coef;
//	int exp;
//
//	_Node* next;
//
//	_Node(const int& coef, const int& exp)
//	{
//		this->coef = coef;
//		this->exp = exp;
//		this->next = NULL;
//	}
//};
//
//class CircularLinkedList
//{
//private:
//	_Node* head;
//	_Node* tail;
//	unsigned int size;
//public:
//	CircularLinkedList(); // Initialize class variables
//	void Insert(const int& coef, const int& exp) noexcept; // Insert _Node at the end of list (before head)
//	void InsertBeforehead(const int& coef, const int& exp); // Insert _Node before head
//	void InsertAfterHead(const int& coef, const int& exp); // Insert _Node after head
//	void InsertBefore(const int& coef1, const int& exp1, const int& coef, const int& exp); // Insert before the given _Node
//	void InsertAfter(const int& coef1, const int& exp1, const int& coef, const int& exp); // Insert after the given _Node
//	unsigned int getSize() noexcept;
//	void RemoveHead(); // Remove _Node at head
//	bool RemoveNode(const int& coef, const int& exp); // Remove certain _Node with given data
//	_Node* FindonExp(const int& exp); // Return true if _Node found
//	_Node* FindonCoef(const int& coef); // Return true if _Node found
//	_Node* At(const int& index) const;
//	_Node* At(const int& index);
//	_Node* operator[](const int& index) const;
//	_Node* operator[](const int& index);
//	bool Replace(const int& coef1, const int& exp1,  const int& coef, const int& exp); // Replace first only _Node of val with coef, const int& exp, 
//	bool ReplaceAll(const int& coef1, const int& exp1,  const int& coef, const int& exp); // Replace all _Nodes of val data with coef, const int& exp, 
//	void ShowList(); // Display entire list on terminal
//	~CircularLinkedList(); // Deletes all _Nodes including head and set head and tail to NULL 
//};
//
