#include <iostream>

#pragma once
#include <iostream>

#define NULL 0

struct _Node
{
	int coef;
	int exp;
	_Node* next;

	_Node(int coef, int exp)
	{
		this->coef = coef;
		this->exp = exp;
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
	void Insert(const int& coef, const int& exp); // Insert _Node at the end of list (before head)
	void InsertBeforehead(const int& coef, const int& exp); // Insert _Node before head
	void InsertAfterHead(const int& coef, const int& exp); // Insert _Node after head
	void InsertBeforeTail(const int& coef, const int& exp); // Insert before the given _Node
	void InsertAfterTail(const int& coef, const int& exp); // Insert after the given _Node
	void InsertBefore(const int& coef, const int& exp, const int& coef1, const int& exp1); // Insert before the given _Node
	void InsertAfter(const int& coef, const int& exp, const int& coef1, const int& exp1); // Insert after the given _Node
	_Node* At(const int& index); // Return pointer to node at index index from head
	_Node* RemoveHead(); // Remove _Node at head
	_Node* RemoveNode(const int& coef, const int& exp); // Remove certain _Node with given coef
	int Find(const int& coef, const int& exp); // Return index if _Node found 
	int FindCoef(const int& coef); // Return index if coefficient found 
	int FindExp(const int& exp); // Return index if exponent found 
	bool Replace(const int& coef, const int& exp, const int& coef1, const int& exp1); // Replace first only _Node of val with val2
	bool ReplaceAll(const int& coef, const int& exp, const int& coef1, const int& exp1); // Replace all _Nodes of val coef with val2
	int FindMaxCoef(); // Find max and return coef of the _Node
	int FindMinCoef(); // Find minimun and return coef of the _Node
	void ShowList(); // Display entire list on terminal
	~CircularLinkedList(); // Deletes all _Nodes including head and set head and tail to NULL 
};

class Polynomial : public CircularLinkedList
{
private:
	CircularLinkedList* equation = NULL;
	unsigned int size;

public:
	Polynomial(); // Create the zero polynomial, that is P (x) = 0
	Polynomial(const Polynomial& p);
	friend std::istream& operator>>(std::istream&, Polynomial&); //Read in a polynomial from cin
	friend std::ostream& operator<<(std::ostream&, const Polynomial&); //Output the polynomial to cout
	friend Polynomial& operator+(const Polynomial& p1, const Polynomial& p2);//Add the two polynomials p1 and p2
	friend Polynomial& operator-(const Polynomial& p1, const Polynomial& p2);//Subtract the first polynomial p1 from the second polynomial p2
	friend Polynomial& operator*(const Polynomial& p1, const Polynomial& p2);//Multiply the two polynomials p1 and p2
	~Polynomial();
};


int main()
{
	Polynomial* a = NULL;
	Polynomial* b = NULL;
	std::cout << "Enter equation for a : ";
	std::cin >> *a;
	std::cout << "You Entered a : " << *a << "\n";
	std::cout << "Enter equation for b : ";
	std::cin >> *b;
	std::cout << "You Entered b : " << *b << "\n";

	std::cout << "a + b" << *a + *b << "\n";
}


Polynomial::Polynomial()
: equation(new CircularLinkedList())
{
	equation->Insert(0, -1);
}
Polynomial::Polynomial(const Polynomial& p) 
: equation(new CircularLinkedList())
{
	if (this == &(p))
		return;

	for (unsigned int i = 0; i < p.size; i++)
	{
		this->equation->Insert(p.equation->At(i)->coef, p.equation->At(i)->exp);
	}
}
std::istream& operator>>(std::istream& is, Polynomial& p)
{
	int coef, exp;
	while (is >> coef && is >> exp && exp != 0){
		p.equation->Insert(coef, exp);
		p.size++;
	}
	p.equation->Insert(0, -1);
	p.size++;
	return is;
}
std::ostream& operator<<(std::ostream& os, const Polynomial& p)
{
	for (unsigned int i = 0; i < p.size; i++)
		os << "(" << p.equation->At(i)->coef << ", " << p.equation->At(i)->exp << ")";
	return os;
}
Polynomial& operator+(const Polynomial& p1, const Polynomial& p2)
{
	Polynomial* temp = new Polynomial();
	
	if (p1.size > p2.size)
	{
		for (unsigned int i = 0; i < p1.size; i++)
			temp->equation->Insert(p1.equation->At(i)->coef, p1.equation->At(i)->exp);
		for (unsigned int i = 0; i < p2.size; i++)
		{
			int same_exp_index = temp->equation->FindExp(p2.equation->At(i)->exp);
			if (same_exp_index != -1)
			{
				temp->equation->At(same_exp_index)->coef = temp->equation->At(same_exp_index)->coef + p2.equation->At(i)->coef;
			}
			else{
				if (temp->At(0)->exp > p2.equation->At(i)->exp){
					// Find nearest greater power and insert after that node
					int exp = p2.equation->At(i)->exp;
					while (temp->equation->FindExp(exp) == -1)
						exp++;
					int nearest_greater_index = temp->equation->FindExp(exp);
					temp->equation->InsertAfter(p2.equation->At(i)->coef, p2.equation->At(i)->exp,
						temp->equation->At(nearest_greater_index)->coef, temp->equation->At(nearest_greater_index)->exp);
				}
				else{
					// Else Find nearest smaller power and insert before that node
					int index = p2.equation->At(i)->exp;
					while (temp->equation->FindExp(index) == -1)
						index--;
					int nearest_small_index = temp->equation->FindExp(index);
					temp->equation->InsertBefore(p2.equation->At(i)->coef, p2.equation->At(i)->exp,
						temp->equation->At(nearest_small_index)->coef, temp->equation->At(nearest_small_index)->exp);
				}
			}
		}
	}
	else {
		for (unsigned int i = 0; i < p2.size; i++)
			temp->equation->Insert(p2.equation->At(i)->coef, p2.equation->At(i)->exp);
		for (unsigned int i = 0; i < p1.size; i++)
		{
			int same_exp_index = temp->equation->FindExp(p1.equation->At(i)->exp);
			if (same_exp_index != -1)
			{
				temp->equation->At(same_exp_index)->coef = temp->equation->At(same_exp_index)->coef + p1.equation->At(i)->coef;
			}
			else{
				if (temp->At(0)->exp > p1.equation->At(i)->exp){
					// Find nearest greater power and insert after that node
					int exp = p1.equation->At(i)->exp;
					while (temp->equation->FindExp(exp) == -1)
						exp++;
					int nearest_greater_index = temp->equation->FindExp(exp);
					temp->equation->InsertAfter(p1.equation->At(i)->coef, p1.equation->At(i)->exp,
						temp->equation->At(nearest_greater_index)->coef, temp->equation->At(nearest_greater_index)->exp);
				}
				else{
					// Else Find nearest smaller power and insert before that node
					int index = p1.equation->At(i)->exp;
					while (temp->equation->FindExp(index) == -1)
						index--;
					int nearest_small_index = temp->equation->FindExp(index);
					temp->equation->InsertBefore(p1.equation->At(i)->coef, p1.equation->At(i)->exp,
						temp->equation->At(nearest_small_index)->coef, temp->equation->At(nearest_small_index)->exp);
				}
			}
		}
	
	}

	return *temp;
	
}
Polynomial& operator-(const Polynomial& p1, const Polynomial& p2)
{
	Polynomial* temp = new Polynomial();

	// No need to check for second case since we always subtract p1 from p2
	for (unsigned int i = 0; i < p2.size; i++)
		temp->equation->Insert(p2.equation->At(i)->coef, p2.equation->At(i)->exp);
	for (unsigned int i = 0; i < p1.size; i++)
	{
		int same_exp_index = temp->equation->FindExp(p1.equation->At(i)->exp);
		if (same_exp_index != -1)
		{
			temp->equation->At(same_exp_index)->coef = temp->equation->At(same_exp_index)->coef - p1.equation->At(i)->coef;
		}
		else{
			if (temp->At(0)->exp > p1.equation->At(i)->exp){
				// Find nearest greater power and insert after that node
				int exp = p1.equation->At(i)->exp;
				while (temp->equation->FindExp(exp) == -1)
					exp++;
				int nearest_greater_index = temp->equation->FindExp(exp);
				// If exponent doesn't match then change the sign and then insert
				temp->equation->InsertAfter(-1 * p1.equation->At(i)->coef, p1.equation->At(i)->exp,
					temp->equation->At(nearest_greater_index)->coef, temp->equation->At(nearest_greater_index)->exp);
			}
			else{
				// Else Find nearest smaller power and insert before that node
				int index = p1.equation->At(i)->exp;
				while (temp->equation->FindExp(index) == -1)
					index--;
				int nearest_small_index = temp->equation->FindExp(index);
				// If exponent doesn't match then change the sign and then insert
				temp->equation->InsertBefore(-1 * p1.equation->At(i)->coef, p1.equation->At(i)->exp,
					temp->equation->At(nearest_small_index)->coef, temp->equation->At(nearest_small_index)->exp);
			}
		}
	}


	return *temp;

}
Polynomial& operator*(const Polynomial& p1, const Polynomial& p2)
{
	
	Polynomial* result = new Polynomial();

	// No need to check for second case since each term will counter other term at once exactly
	for (unsigned int i = 0; i < p1.size; i++)
	{
		Polynomial* temp = new Polynomial();
		for (unsigned int j = 0; j < p2.size; j++)
		{
			temp->equation->Insert(p1.equation->At(i)->coef * p2.equation->At(j)->coef,
				p1.equation->At(i)->exp + p2.equation->At(j)->exp);
		}
		*result = operator+(*result, *temp);

	}
	return *result;
}
Polynomial::~Polynomial()
{
	delete equation;
}


CircularLinkedList::CircularLinkedList()
{
	head = NULL; tail = NULL; size = 0;
}
void CircularLinkedList::Insert(const int& coef, const int& exp)
{
	if (head == NULL)
	{
		head = tail = new _Node(coef, exp);
		tail->next = head;
	}
	else
	{
		tail->next = new _Node(coef, exp);
		tail = tail->next;
		tail->next = head;
	}
	size++;
}
void CircularLinkedList::InsertBeforehead(const int& coef, const int& exp)
{
	if (head == NULL)
		std::invalid_argument;

	else {
		tail->next = new _Node(coef, exp);
		tail = tail->next;
		tail->next = head; 
		size++;
	}
}
void CircularLinkedList::InsertAfterHead(const int& coef, const int& exp)
{
	if (head == NULL)
		std::invalid_argument;
	else
	{
		_Node* temp = new _Node(coef, exp);
		temp->next = head->next;
		head->next = temp;
		size++;
	}
}
void CircularLinkedList::InsertBeforeTail(const int& coef, const int& exp)
{
	if (tail == NULL)
		throw std::invalid_argument("List is Empty");
	else if (head->next == head)
		InsertAfterHead(coef, exp);
	else
	{
		_Node* temp = head;
		while (temp->next != tail)
			temp = temp->next;
		temp->next = new _Node(coef, exp);
		temp->next->next = tail;
		size++;
	}
}
void CircularLinkedList::InsertAfterTail(const int& coef, const int& exp)
{
	if (tail == NULL)
		throw std::invalid_argument("List is Empty");
	else
	{
		tail->next = new _Node(coef, exp);
		tail = tail->next;
		size++;
	}
}
void CircularLinkedList::InsertBefore(const int& coef, const int& exp, const int& coef1, const int& exp1)
{
	if (head == NULL)
	{
		std::invalid_argument;
		return;
	}

	if (head->coef == coef1 && head->exp == exp1)
	{
		CircularLinkedList::InsertBeforehead(coef, exp);
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
			if (temp->coef == coef1 && temp->exp == exp1)
			{
				prev->next = new _Node(coef, exp);
				prev->next->next = temp;
				temp = nullptr; prev = nullptr;
				break;
			}
		}
	}
	size++;
}
void CircularLinkedList::InsertAfter(const int& coef, const int& exp, const int& coef1, const int& exp1)
{
	if (head == NULL)
		std::invalid_argument;

	if (head->coef == coef1 && head->exp == exp1)
	{
		CircularLinkedList::InsertAfterHead(coef, exp);
		return;
	}
	else if (tail->coef == coef1 && tail->exp == exp1)
	{
		CircularLinkedList::Insert(coef, exp);
	}
	else
	{
		_Node* prev = head;
		_Node* temp = head;
		while (prev->next != head)
		{
			prev = temp;
			temp = temp->next;
			if (prev->coef == coef1 && prev->exp == exp1)
			{
				prev->next = new _Node(coef, exp);
				prev->next->next = temp;
				temp = nullptr; prev = nullptr;
				break;
			}
		}
	}
	size++;
}
_Node* CircularLinkedList::At(const int& index)
{
	if (index < 0 || index > (int) size - 1)
		return NULL;
	int count = 0;
	_Node* temp = head;
	while (count != index)
	{
		count++;
	}
	return temp;
}
_Node* CircularLinkedList::RemoveHead()
{
	if (head == NULL)
		return NULL;

	if (head->next != head)
	{
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
		}
		temp->next = head->next;
		_Node* node = new _Node(head->coef, head->exp);
		delete head;
		head = temp->next;
		return node;
	}
	else if (head->next == head)
	{
		_Node* node = new _Node(head->coef, head->exp);
		delete head;
		head = NULL;
		tail = NULL;
		return node;
	}
	else {
		_Node* node = new _Node(head->coef, head->exp);
		delete head;
		head = NULL;
		return node;
	}
}
_Node* CircularLinkedList::RemoveNode(const int& coef, const int& exp)
{
	if (head == NULL)
		return NULL;

	if (head->coef == coef && head->exp == exp)
	{
		return CircularLinkedList::RemoveHead();
	}
	else if (head->next != NULL)
	{
		_Node* prev = head;
		_Node* temp = head;
		while (temp->next != head)
		{
			prev = temp;
			temp = temp->next;
			if (temp->coef == coef && temp->exp == exp)
			{
				if (temp->next != NULL)
				{
					prev->next = temp->next;
					_Node* node = new _Node(temp->coef, temp->exp);
					delete temp;
					temp = NULL; prev = NULL;
					return node;
				}
				else {
					_Node* node = new _Node(temp->coef, temp->exp);
					delete temp;
					prev->next = NULL;
					return node;
				}

			}
		}
		return NULL;
	}
	else {
		return NULL;
	}
}
int CircularLinkedList::Find(const int& coef, const int& exp)
{
	if (head == NULL)
		return -1;
	if (head->coef == coef && head->exp == exp)
		return 0;
	else if (head->next != NULL)
	{
		_Node* temp = head;
		int count = 0;
		while (temp->next != head)
		{
			temp = temp->next;
			count++;
			if (temp->coef == coef && temp->exp == exp)
				return count;
		}
		return -1;
	}
	else {
		return -1;
	}
}
int CircularLinkedList::FindCoef(const int& coef)
{
	if (head == NULL)
		return -1;
	if (head->coef == coef)
		return 0;
	else if (head->next != NULL)
	{
		_Node* temp = head;
		int count = 0;
		while (temp->next != head)
		{
			temp = temp->next;
			count++;
			if (temp->coef == coef)
				return count;
		}
		return -1;
	}
	else {
		return -1;
	}
}
int CircularLinkedList::FindExp(const int& exp)
{
	if (head == NULL)
		return -1;
	if (head->exp == exp)
		return 0;
	else if (head->next != NULL)
	{
		_Node* temp = head;
		int count = 0;
		while (temp->next != head)
		{
			temp = temp->next;
			count++;
			if (temp->exp == exp)
				return count;
		}
		return -1;
	}
	else {
		return -1;
	}
}
bool CircularLinkedList::Replace(const int& coef, const int& exp, const int& coef1, const int& exp1)
{
	if (head == NULL)
		return false;
	if (head->coef == coef && head->exp == exp)
	{
		head->coef = coef1;
		head->exp = exp1;
		return true;
	}
	else if (head->next != NULL)
	{
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
			if (temp->coef == coef && temp->exp == exp)
			{
				temp->coef = coef1;
				temp->exp = exp1;
				return true;
			}
		}
		return false;
	}
}
bool CircularLinkedList::ReplaceAll(const int& coef, const int& exp, const int& coef1, const int& exp1)
{
	if (head == NULL)
		return false;
	if (head->coef == coef && head->exp == exp)
	{
		head->coef = coef1;
		head->exp = exp1;
	}
	if (head->next != NULL)
	{
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
			if (temp->coef == coef && temp->exp == exp)
			{
				temp->coef = coef1;
				temp->exp = exp1;
			}
		}
		return true;
	}
	return false;
}
int CircularLinkedList::FindMaxCoef()
{
	int max = head->coef;

	if (head == NULL)
		return -1;

	_Node* temp = head;
	while (temp->next != head)
	{
		temp = temp->next;
		if (temp->coef > max)
		{
			max = temp->coef;
		}
	}
	return max;
}
int CircularLinkedList::FindMinCoef()
{
	int min = head->coef;

	if (head == NULL)
		return -1;

	_Node* temp = head;
	while (temp->next != head)
	{
		temp = temp->next;
		if (temp->coef < min)
		{
			min = temp->coef;
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
		std::cout << head->coef << " " << head->exp << " ";
		_Node* temp = head;
		while (temp->next != head)
		{
			temp = temp->next;
			std::cout << temp->coef << " " << temp->exp << " ";
		}
		std::cout << "\n";
	}
}
CircularLinkedList::~CircularLinkedList()
{
	while (head != NULL)
		RemoveHead();
}



