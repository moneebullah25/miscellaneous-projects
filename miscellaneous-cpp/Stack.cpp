#include "Stack.h"

NodeS::NodeS()
{
	this->data = NULL;
	this->next = NULL;
}

NodeS::NodeS(int data)
{
	this->data = data;
	this->next = NULL;
}

Stack::Stack()
: head(NULL), tail(NULL) {}

void Stack::push(const int& a)
{
	Stack::insert(a);
}

int Stack::pop(int index)
{
	if (head == NULL)
		return -1;
	if (index == -1)
	{
		return Stack::deleteTail();
	}
	else if (index == 0)
	{
		return Stack::deleteHead();
	}
	
}

int Stack::top()
{
	if (head == NULL)
		return -1;
	return tail->data;
}

int Stack::front()
{
	if (head == NULL)
		return -1;
	return head->data;
}

void Stack::insert(int data)
{
	if (tail == NULL)
		head = tail = new NodeS(data);
	else {
		tail->next = new NodeS(data);
		tail = tail->next;
	}
}

int Stack::deleteTail()
{
	if (tail == NULL) return -1;
	if (head == tail)
	{
		int val = head->data;
		delete head;
		head = tail = NULL;
		return val;
	}
	NodeS* temp = head;
	while (temp->next != tail)
	{
		temp = temp->next;
	}
	int val = tail->data;
	delete tail;
	tail = temp;
	tail->next = NULL;
	temp = NULL;
	return val;
}

int Stack::deleteHead()
{
	if (head == NULL) return -1;
	if (head == tail)
	{
		int val = head->data;
		delete head;
		head = tail = NULL;
		return val;
	}
	NodeS* temp = head->next;
	int val = head->data;
	delete head;
	head = temp;
	temp = NULL;
	return val;
}

int Stack::getsize()
{
	int size = 0;
	if (head == NULL)
		return -1;
	else {
		NodeS* temp = head;
		while (temp != NULL)
		{
			size++;
			temp = temp->next;
		}
	}
	return size;
}

void Stack::display(int ch)
{
	if (ch){
		NodeS* temp = head;
		while (temp != NULL)
		{
			std::cout << char(temp->data) << " ";
			temp = temp->next;
		}
		std::cout << "\n";
	}
	else {
		NodeS* temp = head;
		while (temp != NULL)
		{
			std::cout << temp->data << " ";
			temp = temp->next;
		}
		std::cout << "\n";
	}
}

Stack::~Stack()
{
	while (tail != NULL)
		deleteTail();
}
