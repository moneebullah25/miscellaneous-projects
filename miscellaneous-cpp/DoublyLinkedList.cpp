#include "DoublyLinkedList.h"

Node::Node()
{
	this->data = NULL;
	this->next = NULL;
	this->back = NULL;
}

Node::Node(int data)
{
	this->data = data;
	this->next = NULL;
	this->back = NULL;
}

DoublyLinkedList::DoublyLinkedList()
{
	m_head = m_tail = NULL;
	m_size = 0;
}

Node* DoublyLinkedList::AddAtTail(const int& data)
{
	Node* temp = new Node(data);
	// When no Node in LIST
	if (m_tail == NULL)
	{
		m_head = m_tail = temp;
	}
	// When have Node in LIST
	else
	{
		m_tail->next = temp;
		temp->back = m_tail;
		m_tail = temp;

	}
	m_size++;
	return temp;
}

Node* DoublyLinkedList::AddAtHead(const int& data)
{
	Node* temp = new Node(data);
	if (m_head == NULL)
		m_head = m_tail = temp;
	else
	{
		temp->next = m_head;
		m_head->back = temp;
		m_head = temp;
	}
	m_size++;
	return temp;
}

Node* DoublyLinkedList::AddAtIndex(const int& index, const int& data)
{
	if (index > m_size - 1 || index < 0)
	{
		throw std::out_of_range("");
		std::exit(EXIT_FAILURE);
		return NULL;
	}
	// Insert at head if list is empty and index is equal to 0
	if (index == 0 && m_head == NULL)
	{
		m_head = m_tail = new Node(data);
		m_size++;
		return m_head;
	}
	// Insert at head if list only contains 1 node
	if (index == m_size - 1 && m_size == 1)
	{
		return DoublyLinkedList::AddAtHead(data);
	}
	// Add node at the last element if the list having more than 1 nodes
	else if (index + 1 == m_size)
	{
		return DoublyLinkedList::AddAtTail(data);
	}
	// Add node of index is somewhere between head and tail
	else if (index + 1 < m_size)
	{
		Node* temp = m_head;
		int count = 0;
		while (temp != NULL)
		{
			if (count == index)
			{
				Node* node = new Node(data);
				node->next = temp->next; node->back = temp;
				temp->next->back = node;
				temp->next = node;
				m_size++;
				return node;
			}
			if (temp->next != NULL)
				temp = temp->next;
			else break;
			count += 1;
		}
	}
}

void DoublyLinkedList::Display(bool asc)
{
	if (m_size > 0){
		if (asc)
		{
			Node* temp = m_head;
			std::cout << "DOUBLY LIST : ";
			while (temp != NULL)
			{
				std::cout << temp->data << " ";
				temp = temp->next;
			}
			std::cout << std::endl;
			free(temp);
		}
		else
		{
			Node* temp = m_tail;
			std::cout << "DOUBLY LIST : ";
			while (temp != NULL)
			{
				std::cout << temp->data << " ";
				temp = temp->back;
			}
			std::cout << std::endl;
			free(temp);
		}
	}
	else
	{
		std::cout << "EMPTY LIST\n";
	}
}

int DoublyLinkedList::Find(const unsigned int& data)
{
	// All Options
	int index = 0;
	Node* temp = m_head;
	while (temp != NULL)
	{
		if (temp->data == data)
			return index;
		temp = temp->next;
		index += 1;
	}
	free(temp);
	return -1;
}

int& DoublyLinkedList::At(const unsigned int& index)
{
	if (index > m_size - 1)
	{
		throw std::out_of_range("");
		std::exit(EXIT_FAILURE);
	}
	Node* temp = m_head;
	int count = 0;
	while (temp != NULL)
	{
		if (count == index)
			return (temp->data);
		temp = temp->next;
		count += 1;
	}
	free(temp);
}

void DoublyLinkedList::Replace(const int& data, const int& value)
{
	int val = DoublyLinkedList::Find(data);
	if (val == -1) 
		return;
	this->At(val) = value;
}

void DoublyLinkedList::RemoveAt(const unsigned int& index)
{
	if (index > m_size || index < 0) {
		throw std::out_of_range("");
		return;
	}
	else
	{
		// Delete head if list only contains 1 node
		if (m_head == m_tail)
		{
			delete m_head;
			m_head = NULL;
			m_tail = NULL;
		}
		// Deletes head if the list is having more than 1 nodes
		else if (index == 0 && m_size > 1)
		{
			Node* temp = m_head->next;
			temp->back = NULL;
			delete m_head;
			m_head = temp;
			temp = NULL;
		}
		// Deletes the last element if the list having more than 1 nodes
		else if (index + 1 == m_size)
		{
			Node* temp = m_tail->back;
			delete m_tail;
			m_tail = temp;
			temp->next = NULL;
		}
		// Else delete node if node is somewhere between head and tail
		else if (index + 1 < m_size)
		{
			Node* temp = m_head;
			Node* prev;
			int count = 0;
			while (temp != NULL)
			{
				prev = temp;
				temp = temp->next;
				if (count == index)
				{
					prev->next = temp->next;
					temp->next->back = prev;
					delete temp;
					temp = prev = NULL;
					break;
				}
				count += 1;
			}
		}
		
		m_size -= 1;
	}
}

unsigned int DoublyLinkedList::Length()
{
	return m_size;
}

void DoublyLinkedList::Pop()
{
	RemoveAt(m_size - 1);
}

DoublyLinkedList::~DoublyLinkedList()
{
	while (m_head != NULL)
		RemoveAt(0);
}