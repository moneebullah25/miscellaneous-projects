#include <iostream>
#include <string>

#define NULL 0 

template <typename T>
class QueueUV
{
private:
	T* queue_array;
	int item_size;
	int front, rear, max_size;

public:
	QueueUV() : item_size(0), front(0), rear(0), max_size(1)
	{
		queue_array = new T[max_size];
	}
	QueueUV(int size) : item_size(0), front(0), rear(0), max_size(size)
	{
		queue_array = new T[max_size];
	}
	void push(T data){
		if (rear == max_size && front == 0)
		{
			T* arr2 = new T[max_size * 2];
			for (int i = 0; i < item_size; i++)
				arr2[i] = queue_array[i];
			delete[] queue_array;
			queue_array = arr2;
			arr2 = NULL;
			queue_array[rear++] = data;
			max_size = max_size * 2;
			item_size++;
		}
		else if (rear == max_size && front != 0 && front != rear)
		{
			rear = 0;
			queue_array[rear++] = data;
			item_size++;
		}
		else if (rear == front && item_size != 0)
		{
			T* arr2 = new T[max_size * 2];
			for (int i = front; i < max_size; i++)
				arr2[i - front] = queue_array[i];
			for (int i = 0; i < rear; i++)
				arr2[i + max_size - front] = queue_array[i];
			rear = item_size;
			front = 0;
			arr2[rear++] = data;
			item_size++;
			max_size = max_size * 2;
			delete[] queue_array;
			queue_array = arr2;
		}
		else if (rear == front && item_size == 0)
		{
			front = rear = 0;
			queue_array[rear++] = data;
			item_size++;
		}
		else if (rear < max_size)
		{
			queue_array[rear++] = data;
			item_size++;
		}
	}
	T pop(){
		if (item_size > 0)
		{
			if (front < max_size)
			{
				T value = queue_array[front];
				front++;
				item_size--;
				return value;
			}
			if (front == max_size)
			{
				front = 0;
				T value = queue_array[front];
				front++;
				item_size--;
				return value;
			}
			throw std::out_of_range("List Index is out of Bound");
		}
		else 
		{
			throw std::out_of_range("List Index is out of Bound");
		}
	}
	int capacity(){
		return max_size;
	}
	int size(){
		return item_size;
	}
	void display(){
		if (rear <= front) {
			for (int i = front; i < max_size; i++)
			{
				std::cout << queue_array[i] << " ";
			}
			for (int i = 0; i < rear; i++)
			{
				std::cout << queue_array[i] << " ";
			}
		}
		else {
			for (int i = front; i < rear; i++)
			{
				std::cout << queue_array[i] << " ";
			}
		}
		std::cout << "\n";
	}
	~QueueUV(){
		delete[] queue_array;
	}
};

template <typename T>
class StackUV
{
private:
	T* stack_array;
	int item_size;
	int front, rear, max_size;

public:
	StackUV() : item_size(0), front(0), rear(0), max_size(1)
	{
		stack_array = new int[max_size];
	}
	StackUV(int size) : item_size(0), front(0), rear(0), max_size(size)
	{
		stack_array = new T[max_size];
	}
	void push(T data){
		if (item_size < max_size)
		{
			stack_array[rear] = data;
			item_size++; rear++;
		}
		else if (item_size == max_size)
		{
			T* stack_array_2 = new T[max_size * 2];
			for (int i = front; i < rear; i++)
			{
				stack_array_2[i] = stack_array[i];
			}
			stack_array_2[rear] = data;
			item_size++; rear++;
			delete[] stack_array;
			stack_array = stack_array_2;
			max_size = max_size * 2;
			stack_array_2 = NULL;
		}
	}
	T pop(){
		if (rear != 0) {
			rear = rear - 1;
			item_size = item_size - 1;
			T val = stack_array[rear];
			return val;
		}
		else {
			throw std::out_of_range("List Index is out of Bound");
		}
	}
	int capacity(){
		return max_size;
	}
	int size(){
		return item_size;
	}
	void display(){
		for (int i = front; i < rear; i++)
		{
			std::cout << stack_array[i] << " ";
		}
		std::cout << "\n";
	}
	~StackUV(){
		delete[] stack_array;
	}
};

class MyString
{
private:
	char* m_Buffer;
	unsigned int size;
public:
	MyString(){
		size = 0;
		m_Buffer = new char;
		m_Buffer = '\0';
	}
	MyString(const char* string){
		size = strlen(string);
		m_Buffer = new char[size + 1];
		memcpy(m_Buffer, string, size);
		m_Buffer[size] = '\0';
	}
	// Copy Constuctor
	MyString(const MyString& str){
		size = str.size;
		m_Buffer = new char[size + 1];
		memcpy(m_Buffer, str.m_Buffer, size + 1);
	}
	char& operator[](size_t index)
	{
		if (index >= size)
			std::out_of_range;
		return m_Buffer[index];
	}
	char operator[](size_t index) const
	{
		if (index >= size)
			std::out_of_range;
		return m_Buffer[index];
	}
	// Copy Assignment 
	MyString& operator=(const MyString& str)
	{
		if (this == &str)
			return *this;

		this->MyString::MyString(str);
		return *this;
	}
	// Copy Assignment Const
	MyString operator=(const MyString& str) const 
	{
		if (this == &str)
			return *this;

		this->MyString::MyString(str);
		return *this;
	}
	// Equality Operator
	bool operator==(const MyString& str)
	{
		if (this == &str)
			return true;

		
		if (this->getSize() != str.getSize())
			return false;

		for (int i = 0; i < str.getSize(); i++)
		{
			if (this->operator[](i) != str.operator[](i))
			{
				return false;
			}
		}
		return true;
	}
	// Equality Operator Const
	bool operator==(const MyString& str) const
	{
		if (this == &str)
			return true;


		if (this->getSize() != str.getSize())
			return false;

		for (int i = 0; i < str.getSize(); i++)
		{
			if (this->operator[](i) != str.operator[](i))
			{
				return false;
			}
		}
		return true;
	}
	unsigned int getSize()
	{
		return size;
	}
	unsigned int getSize() const
	{
		return size;
	}
	friend std::ostream& operator<<(std::ostream& stream, const MyString& string){
		stream << string.m_Buffer;
		return stream;
	}
	~MyString(){
		delete[] m_Buffer;
	}
};

template <typename _DATA>
struct NodeDLL
{
	_DATA data;
	NodeDLL* next;
	NodeDLL* back;
	NodeDLL(){
		this->data = NULL;
		this->next = NULL;
		this->back = NULL;
	}

	NodeDLL(_DATA data){
		this->data = data;
		this->next = NULL;
		this->back = NULL;
	}
};

template <typename T>
class DoublyLinkedList
{

private:
	NodeDLL<T>* m_head;
	NodeDLL<T>* m_tail;
	unsigned int m_size;

public:
	DoublyLinkedList() // Initialize class variables
	{
		m_head = m_tail = NULL;
		m_size = 0;
	}
	NodeDLL<T>* AddAtTail(const T& data) // Add after the tail
	{
		NodeDLL<T>* temp = new NodeDLL<T>(data);
		// When no NodeDLL in LIST
		if (m_tail == NULL)
		{
			m_head = m_tail = temp;
		}
		// When have NodeDLL in LIST
		else
		{
			m_tail->next = temp;
			temp->back = m_tail;
			m_tail = temp;

		}
		m_size++;
		return temp;
	}
	NodeDLL<T>* AddAtHead(const T& data) // Add before the head
	{
		NodeDLL<T>* temp = new NodeDLL<T>(data);
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
	NodeDLL<T>* AddAtIndex(const unsigned int& index, const T& data) // Add NodeDLL after the index NodeDLL
	{
		if (index > m_size || index < 0)
		{
			throw std::out_of_range("");
			std::exit(EXIT_FAILURE);
			return NULL;
		}
		// Insert at head if list is empty and index is equal to 0
		if (index == 0 && m_size == 0)
		{
			m_head = m_tail = new NodeDLL<T>(data);
			m_size++;
			return m_head;
		}
		// Insert at head if list only contains 1 node
		if (index == 0)
		{
			return DoublyLinkedList::AddAtHead(data);
		}
		// Add node at the last element if the list having more than 1 nodes
		else if (index == m_size)
		{
			return DoublyLinkedList::AddAtTail(data);
		}
		// Add node of index is somewhere between head and tail
		else if (index < m_size)
		{
			NodeDLL<T>* temp = m_head;
			int count = 1;
			while (temp != NULL)
			{
				if (count == index)
				{
					NodeDLL<T>* node = new NodeDLL<T>(data);
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
	void Display(bool asc = true) // if asc = true then print in ascending order
	{
		if (m_size > 0){
			if (asc)
			{
				NodeDLL<T>* temp = m_head;
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
				NodeDLL<T>* temp = m_tail;
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
	int Find(const T& data) // Return index if data found
	{
		// All Options
		int index = 0;
		NodeDLL<T>* temp = m_head;
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
	T& At(const unsigned int& index) // Return data at index passed by arg
	{
		if (index > m_size - 1)
		{
			throw std::out_of_range("");
			std::exit(EXIT_FAILURE);
		}
		NodeDLL<T>* temp = m_head;
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
	void Replace(const T& data, const T& value) // Replace single occurance of data with the value 
	{
		int val = DoublyLinkedList::Find(data);
		if (val == -1)
			return;
		this->At(val) = value;
	}
	void ReplaceAll(const T& data, const T& value) // Replace all occurance of data with the value 
	{
		int val = DoublyLinkedList::Find(data);
		while (val != -1){
			this->At(val) = value;
			val = DoublyLinkedList::Find(data);
		}
	}
	void RemoveAt(const unsigned int& index) // Remove data at the index passed as a argument 
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
				NodeDLL<T>* temp = m_head->next;
				temp->back = NULL;
				delete m_head;
				m_head = temp;
				temp = NULL;
			}
			// Deletes the last element if the list having more than 1 nodes
			else if (index + 1 == m_size)
			{
				NodeDLL<T>* temp = m_tail->back;
				delete m_tail;
				m_tail = temp;
				temp->next = NULL;
			}
			// Else delete node if node is somewhere between head and tail
			else if (index + 1 < m_size)
			{
				NodeDLL<T>* temp = m_head;
				NodeDLL<T>* prev;
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
	unsigned int Length() // Return the length of the list
	{
		return m_size;
	}
	void Pop() // Removes the last element of the list 
	{
		RemoveAt(m_size - 1);
	}
	~DoublyLinkedList() // Delete full list and set head and tail to NULL
	{
		while (m_head != NULL)
			RemoveAt(0);
	}
};


int main()
{
	//QueueUV<MyString>* q1 = new QueueUV<MyString>(4);
	//q1->push("1");
	//q1->push("2");
	//q1->push("3");
	//q1->push("4");
	//q1->pop();
	//q1->push("5");
	//q1->display();
	//q1->pop();
	//q1->push("6");
	//q1->push("7");
	//q1->display();

	//StackUV<MyString>* st = new StackUV<MyString>(4);
	//st->push("1");
	//st->push("2");
	//st->push("3");
	//st->push("4");
	//st->display();
	//st->pop();
	//st->pop();
	//st->pop();
	//st->pop();
	//st->display();
	//st->push("5");
	//st->display();
	//st->pop();
	//st->push("6");
	//st->push("7");
	//st->display();

	//DoublyLinkedList<MyString>* dll = new DoublyLinkedList<MyString>();
	//dll->AddAtHead("0");
	//dll->AddAtTail("1");
	//dll->AddAtTail("2");
	//dll->AddAtTail("3");
	//dll->AddAtTail("4");
	//dll->AddAtTail("4");
	//dll->AddAtTail("5");
	//dll->AddAtTail("5");
	//dll->AddAtTail("5");
	//dll->AddAtIndex(8, "9");
	//dll->Display();
	//dll->Length();
	//dll->Display();
	//dll->Replace("4", "9");
	//dll->Display();
	//dll->ReplaceAll("4", "9");
	//dll->Display();
	//dll->ReplaceAll("5", "9");
	//dll->Display();
	//dll->RemoveAt(0);
	//dll->Display();
	//dll->Pop();
	//dll->Display();
	//dll->At(dll->Find("1")) = "100";
	//dll->Display();


	std::cin.get();
}