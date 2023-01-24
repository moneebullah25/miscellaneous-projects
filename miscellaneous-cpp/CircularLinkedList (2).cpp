//#include "CircularLinkedList.h"
//
//
//CircularLinkedList::CircularLinkedList()
//{
//	head = NULL; tail = NULL; size = 0;
//}
//
//void CircularLinkedList::Insert(const int& coef, const int& exp) noexcept
//{
//	if (head == NULL)
//	{
//		head = tail = new _Node(coef, exp);
//		tail->next = head;
//	}
//	else
//	{
//		tail->next = new _Node(coef, exp);
//		tail = tail->next;
//		tail->next = head;
//	}
//	size++;
//}
//
//void CircularLinkedList::InsertBeforehead(const int& coef, const int& exp)
//{
//	if (head == NULL)
//		throw std::invalid_argument("Empty List");
//
//	else {
//		tail->next = new _Node(coef, exp);
//		tail = tail->next;
//		tail->next = head;
//	}
//	size++;
//}
//
//void CircularLinkedList::InsertAfterHead(const int& coef, const int& exp)
//{
//	if (head == NULL)
//		throw std::invalid_argument("Empty List");
//	else 
//	{
//		_Node* temp = new _Node(coef, exp);
//		temp->next = head->next;
//		head->next = temp;
//
//	}
//	size++;
//}
//
//void CircularLinkedList::InsertBefore(const int& coef1, const int& exp1, const int& coef, const int& exp)
//{
//	if (head == NULL)
//	{
//		throw std::invalid_argument("Empty List");
//	}
//
//	else if (head->coef == coef1 && head->exp == exp1)
//	{
//		CircularLinkedList::InsertBeforehead(coef, exp);
//		return;
//	}
//	else
//	{
//		_Node* temp = head;
//		_Node* prev = head;
//		while (temp->next != head)
//		{
//			prev = temp;
//			temp = temp->next;
//			if (temp->coef == coef1 && temp->exp == exp1)
//			{
//				prev->next = new _Node(coef, exp);
//				prev->next->next = temp;
//				temp = nullptr; prev = nullptr;
//				break;
//			}
//		}
//	}
//	size++;
//}
//
//void CircularLinkedList::InsertAfter(const int& coef1, const int& exp1, const int& coef, const int& exp)
//{
//	if (head == NULL)
//		throw std::invalid_argument("Empty List");
//
//	else if (head->coef == coef1 && head->exp == exp1)
//	{
//		CircularLinkedList::InsertAfterHead(coef, exp);
//		return;
//	}
//	else if (tail->coef == coef1 && tail->exp == exp1)
//	{
//		CircularLinkedList::Insert(coef, exp);
//	}
//	else
//	{
//		_Node* prev = head;
//		_Node* temp = head;
//		while (prev->next != head)
//		{
//			prev = temp;
//			temp = temp->next;
//			if (prev->coef == coef1 && prev->exp == coef1)
//			{
//				prev->next = new _Node(coef, exp);
//				prev->next->next = temp;
//				temp = nullptr; prev = nullptr;
//				break;
//			}
//		}
//	}
//	size++;
//}
//
//unsigned int CircularLinkedList::getSize() noexcept
//{
//	return size;
//}
//
//void CircularLinkedList::RemoveHead()
//{
//	if (head == NULL)
//		return;
//
//	if (head->next != head)
//	{
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			temp = temp->next;
//		}
//		temp->next = head->next;
//		delete head;
//		head = temp->next;
//	}
//	else if (head->next == head)
//	{
//		free(head);
//		head = NULL;
//		tail = NULL;
//	}
//	else {
//		delete head;
//		head = NULL;
//	}
//	size--;
//}
//
//bool CircularLinkedList::RemoveNode(const int& coef, const int& exp)
//{
//	if (head == NULL)
//		return false;
//	
//	if (head->coef == coef && head->exp == exp)
//	{
//		CircularLinkedList::RemoveHead();
//		return true;
//	}
//	else if (head->next != NULL)
//	{
//		_Node* prev = head;
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			prev = temp;
//			temp = temp->next;
//			if (temp->coef == coef && temp->exp == exp)
//			{
//				if (temp->next != NULL)
//				{
//					prev->next = temp->next;
//					delete temp;
//					temp = NULL; prev = NULL;
//					size--;
//					return true;
//				}
//				else {
//					delete temp;
//					prev->next = NULL;
//					size--;
//					return true;
//				}
//
//			}
//		}
//		return false;
//	}
//	else {
//		return false;
//	}
//}
//
//_Node* CircularLinkedList::FindonExp(const int& exp)
//{
//	if (head == NULL)
//		return NULL;
//	else if (head->exp == exp)
//		return head;
//	else
//	{
//		bool is_found = false;
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			temp = temp->next;
//			if (temp->exp == exp)
//			{
//				return temp;
//			}
//		}
//		return NULL;
//	}
//}
//
//_Node* CircularLinkedList::FindonCoef(const int& coef)
//{
//	if (head == NULL)
//		return NULL;
//	else if (head->coef == coef)
//		return head;
//	else
//	{
//		bool is_found = false;
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			temp = temp->next;
//			if (temp->coef == coef)
//			{
//				return temp;
//			}
//		}
//		return NULL;
//	}
//}
//
//_Node* CircularLinkedList::At(const int& index)
//{
//	if (head == NULL)
//		return NULL;
//	else if (head != NULL && index == 0)
//		return head;
//	else if (head->next != NULL)
//	{
//		int count = 0;
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			count++;
//			temp = temp->next;
//			if (count == index)
//				return temp;
//		}
//		return NULL;
//	}
//	else {
//		return NULL;
//	}
//}
//
//_Node* CircularLinkedList::At(const int& index) const
//{
//	if (head == NULL)
//		return NULL;
//	else if (head != NULL && index == 0)
//		return head;
//	else if (head->next != NULL)
//	{
//		int count = 0;
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			count++;
//			temp = temp->next;
//			if (count == index)
//				return temp;
//		}
//		return NULL;
//	}
//	else {
//		return NULL;
//	}
//}
//
//_Node* CircularLinkedList::operator[](const int& index) const
//{
//	return CircularLinkedList::At(index);
//}
//
//_Node* CircularLinkedList::operator[](const int& index)
//{
//	return CircularLinkedList::At(index);
//}
//
//
//bool CircularLinkedList::Replace(const int& coef1, const int& exp1, const int& coef, const int& exp)
//{
//	if (head == NULL)
//		return false;
//	else 
//	if (head->coef == coef1 && head->exp == exp1)
//	{
//		head->coef = coef;
//		head->exp = exp;
//		return true;
//	}
//	else if (head->next != NULL)
//	{
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			temp = temp->next;
//			if (temp->coef == coef1 && temp->exp == exp1)
//			{
//				temp->coef = coef;
//				temp->exp = exp;
//				return true;
//			}
//		}
//		return false;
//	}
//}
//
//bool CircularLinkedList::ReplaceAll(const int& coef1, const int& exp1, const int& coef, const int& exp)
//{
//	if (head == NULL)
//		return false;
//
//	bool is_replace = false;
//
//	_Node* temp = head;
//	while (temp->next != head)
//	{
//		temp = temp->next;
//		if (temp->coef == coef1 && temp->exp == exp1)
//		{
//			temp->coef = coef;
//			temp->exp = exp;
//			is_replace = true;
//		}
//	}
//
//	return is_replace;
//}
//
//
//void CircularLinkedList::ShowList()
//{	
//	if (head == NULL)
//		return;
//	else {
//		std::cout << "Circular List : ";
//		std::cout << head->coef << " (" << head->exp << ") ";
//		_Node* temp = head;
//		while (temp->next != head)
//		{
//			temp = temp->next;
//			std::cout << temp->coef << " (" << temp->exp << ") ";
//		}
//		std::cout << "\n";
//	}
//}
//
//CircularLinkedList::~CircularLinkedList()
//{
//	while (head != NULL)
//		RemoveHead();
//}
//
