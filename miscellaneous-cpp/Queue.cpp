#include "Queue.h"

NodeP::NodeP()
{
	data = NULL;
	next = NULL;
}

NodeP::NodeP(int data)
{
	this->data = data;
	next = NULL;
}

Queue::Queue() :
	head(NULL), tail(NULL) {}

void Queue::push(const int& a)
{
	Queue::insert(a);
}

int Queue::pop(int index)
{
	if (head == NULL)
		return -1;
	if (index == -1)
	{
		return Queue::deleteTail();
	}
	else if (index == 0)
	{
		return Queue::deleteHead();
	}

}

int Queue::top()
{
	if (head == NULL)
		return -1;
	return tail->data;
}

int Queue::front()
{
	if (head == NULL)
		return -1;
	return head->data;
}

void Queue::insert(int data)
{
	if (tail == NULL)
		head = tail = new NodeP(data);
	else {
		tail->next = new NodeP(data);
		tail = tail->next;
	}
}

int Queue::deleteTail()
{
	if (tail == NULL) return -1;
	if (head == tail)
	{
		int val = head->data;
		delete head;
		head = tail = NULL;
		return val;
	}
	NodeP* temp = head;
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

int Queue::deleteHead()
{
	if (head == NULL) return -1;
	if (head == tail)
	{
		int val = head->data;
		delete head;
		head = tail = NULL;
		return val;
	}
	NodeP* temp = head->next;
	int val = head->data;
	delete head;
	head = temp;
	temp = NULL;
	return val;
}

int Queue::getsize()
{
	int size = 0;
	if (head == NULL)
		return -1;
	else {
		NodeP* temp = head;
		while (temp != NULL)
		{
			size++;
			temp = temp->next;
		}
	}
	return size;
}

void Queue::display(int ch)
{
	if (ch) {
		NodeP* temp = head;
		while (temp != NULL)
		{
			std::cout << char(temp->data) << " ";
			temp = temp->next;
		}
		std::cout << "\n";
	}
	else {
		NodeP* temp = head;
		while (temp != NULL)
		{
			std::cout << temp->data << " ";
			temp = temp->next;
		}
		std::cout << "\n";
	}
}

Queue::~Queue()
{
	while (tail != NULL)
		deleteTail();
}