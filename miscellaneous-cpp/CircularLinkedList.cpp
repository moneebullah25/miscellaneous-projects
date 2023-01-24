#include "CircularLinkedList.h"


CircularLinkedList::CircularLinkedList()
{
	head = NULL; tail = NULL; size = 0;
}

void CircularLinkedList::Insert(const int& data)
{
	if (head == NULL)
	{
		head = tail = new _Node(data);
		tail->next = head;
	}
	else
	{
		tail->next = new _Node(data);
		tail = tail->next;
		tail->next = head;
	}
	size++;
}

void CircularLinkedList::InsertBeforehead(const int& data)
{
	if (head == NULL)
		std::invalid_argument;

	else {
		tail->next = new _Node(data);
		tail = tail->next;
		tail->next = head;
	}
	size++;
}

void CircularLinkedList::InsertAfterHead(const int& data)
{
	if (head == NULL)
		std::invalid_argument;
	else 
	{
		_Node* temp = new _Node(data);
		temp->next = head->next;
		head->next = temp;

	}
	size++;
}

void CircularLinkedList::InsertBefore(const int& value, const int& data)
{
	if (head == NULL)
	{
		std::invalid_argument;
		return;
	}

	if (head->data == value)
	{
		CircularLinkedList::InsertBeforehead(data);
		return;
	}
	else
	{
		_Node* temp = head;
		_Node* prev = head;
		while (temp->next != head)
		{
			prev = temp;
			temp = temp->next;
			if (temp->data == value)
			{
				prev->next = new _Node(data);
				prev->next->next = temp;
				temp = nullptr; prev = nullptr;
				break;
			}
		}
	}
	size++;
}

void CircularLinkedList::InsertAfter(const int& value, const int& data)
{
	if (head == NULL)
		std::invalid_argument;

	if (head->data == value)
	{
		CircularLinkedList::InsertAfterHead(data);
		return;
	}
	else if (tail->data == value)
	{
		CircularLinkedList::Insert(data);
	}
	else
	{
		_Node* prev = head;
		_Node* temp = head;
		while (prev->next != head)
		{
			prev = temp;
			temp = temp->next;
			if (prev->data == value)
			{
				prev->next = new _Node(data);
				prev->next->next = temp;
				temp = nullptr; prev = nullptr;
				break;
			}
		}
	}
	size++;
}

void CircularLinkedList::RemoveHead()
{
	if (head == NULL)
		return;

	if (head->next != head)
	{
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
		}
		temp->next = head->next;
		delete head;
		head = temp->next;
	}
	else if (head->next == head)
	{
		free(head);
		head = NULL;
		tail = NULL;
	}
	else {
		delete head;
		head = NULL;
	}
}

bool CircularLinkedList::RemoveNode(const int& data)
{
	if (head == NULL)
		return false;
	
	if (head->data == data)
	{
		CircularLinkedList::RemoveHead();
		return true;
	}
	else if (head->next != NULL)
	{
		_Node* prev = head;
		_Node* temp = head;
		while (temp->next != head)
		{
			prev = temp;
			temp = temp->next;
			if (temp->data == data)
			{
				if (temp->next != NULL)
				{
					prev->next = temp->next;
					delete temp;
					temp = NULL; prev = NULL;
					return true;
				}
				else {
					delete temp;
					prev->next = NULL;
					return true;
				}

			}
		}
		return false;
	}
	else {
		return false;
	}
}

bool CircularLinkedList::Find(const int& data)
{
	if (head == NULL)
		return false;
	if (head->data == data)
		return true;
	else if (head->next != NULL)
	{
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
			if (temp->data == data)
				return true;
		}
		return false;
	}
	else {
		return false;
	}
}

bool CircularLinkedList::Replace(const int& val, const int& val2)
{
	if (head == NULL)
		return false;
	if (head->data == val)
	{
		head->data = val2;
		return true;
	}
	else if (head->next != NULL)
	{
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
			if (temp->data == val)
			{
				temp->data = val2;
				return true;
			}
		}
		return false;
	}
}

bool CircularLinkedList::ReplaceAll(const int& val, const int& val2)
{
	if (head == NULL)
		return false;
	if (head->data == val)
	{
		head->data = val2;
	}
	if (head->next != NULL)
	{
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
			if (temp->data == val)
			{
				temp->data = val2;
			}
		}
		return true;
	}
	return false;
}

int CircularLinkedList::FindMax()
{
	int max = head->data;
	
	if (head == NULL)
		return -1;
	
	_Node* temp = head;
	while (temp->next != head)
	{
		temp = temp->next;
		if (temp->data > max)
		{
			max = temp->data;
		}
	}
	return max;
}

int CircularLinkedList::FindMin()
{
	int min = head->data;

	if (head == NULL)
		return -1;

	_Node* temp = head;
	while (temp->next != head)
	{
		temp = temp->next;
		if (temp->data < min)
		{
			min = temp->data;
		}
	}
	return min;
}

void CircularLinkedList::ShowList()
{	
	if (head == NULL)
		return;
	else {
		std::cout << "Circular List : ";
		std::cout << head->data << " ";
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
			std::cout << temp->data << " ";
		}
		std::cout << "\n";
	}
}

CircularLinkedList::~CircularLinkedList()
{
	while (head != NULL)
		RemoveHead();
}

