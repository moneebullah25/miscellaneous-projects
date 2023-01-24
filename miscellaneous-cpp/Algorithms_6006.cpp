#include "Algorithms_6006.h"

heap::heap(int size)
{
	arr = new Array(size);
}

heap::heap(std::vector<int> vec)
{
	arr = new Array(vec);
}

heap::heap(const Array* const ar)
{
	arr = new Array(ar->getsize() * 2);
	for (int i = 0; i < ar->getsize(); i++)
	{
		arr->append((*ar)[i]);
	}
}

void heap::append(const int& data)
{
	arr->append(data);
}

void heap::maxify_heap(int start, int end)
{
	// swap greatest of childrens to parent
	if (start == end - 1)
		return;

	if ((*arr)[start] < utilities::max((*arr)[2 * start + 1], (*arr)[2 * start + 2]))
		utilities::swap((*arr)[start], utilities::max((*arr)[2 * start + 1], (*arr)[2 * start + 2]));
	
	// repeat step 1 for each children
	maxify_heap(2 * start + 1, end);
	maxify_heap(2 * start + 2, end);
}

void heap::maxify()
{
	maxify_heap(0, arr->getsize());
}

void heap::minify_heap(int start, int end)
{
	// swap smallest of childrens to parent
	if (start == end - 1 || 2 * start + 2 >= end)
		return;

	if ((*arr)[start] > utilities::max((*arr)[2 * start + 1], (*arr)[2 * start + 2]))
		utilities::swap((*arr)[start], utilities::min((*arr)[2 * start + 1], (*arr)[2 * start + 2]));

	// repeat step 1 for each children
	minify_heap(2 * start + 1, end);
	minify_heap(2 * start + 2, end);
}

void heap::minify()
{
	minify_heap(0, arr->getsize());
}

void heap::build_max_heap()
{
	for (int i = arr->getsize() / 2; i > 1; i--)
	{
		maxify_heap(0, i);
	}
}

void heap::printBT(const std::string& prefix, int index, bool isLeft)
{
	if (index >= 0 || index < arr->getsize())
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		// print the value of the node
		std::cout << (*arr)[index] << std::endl;

		// enter the next tree level - left and right branch
		printBT(prefix + (isLeft ? "│   " : "    "), (2 * index) + 1, true);
		printBT(prefix + (isLeft ? "│   " : "    "), (2 * index) + 2, false);
	}
}

void heap::display()
{
	printBT("", 0, false);
}


Array::Array(const int& reserve)
{
	capacity = reserve;
	arr = new int[capacity];
	size = 0;
}

Array::Array(std::vector<int> vec)
{
	int vec_size = vec.size();
	capacity = vec_size;
	arr = new int[capacity];

	for (int i = 0; i < vec_size; i++)
		append(vec[i]);
	
}

void Array::append(int data)
{
	if (size < capacity)
	{
		arr[size] = data;
		size++;
	}
	else if (size == capacity)
	{
		capacity *= 2;
		int* arr2 = new int[capacity];
		for (int i = 0; i < size; i++)
			utilities::copy(arr[i], arr2[i]);
		delete[] arr;
		arr = arr2;
		arr2 = NULL;
		arr[size] = data;
		size++;
	}
}

int& Array::operator[](int index)
{
	if (index < 0 || index >= size)
		throw std::out_of_range("index is not right");
	else
		return arr[index];
	//if (index == capacity)
	//{
	//	capacity *= 2;
	//	int* arr2 = new int[capacity];
	//	for (int i = 0; i < size; i++)
	//		utilities::copy(arr[i], arr2[i]);
	//	delete[] arr;
	//	arr = arr2;
	//	arr2 = NULL;
	//	if (index > 0)
	//	{
	//		if (arr[size - 1] != previous_state)
	//		{
	//			previous_state = arr[size];
	//			size++;
	//		}
	//	}
	//	return *(arr + index);
	//}
	//if (index < 0)
	//	std::out_of_range;
	//else
	//{
	//	if (index > 0)
	//	{
	//		if (arr[size - 1] != previous_state)
	//		{
	//			previous_state = arr[size];
	//			size++;
	//		}
	//	}
	//	return *(arr + index);
	//}
}

int Array::operator[](int index) const 
{
	if (index < 0 || index >= size)
		throw std::out_of_range("index is not right");
	else
		return arr[index];
}

int Array::getsize() const 
{
	return size;
}

void Array::printArray() const
{
	for (int i = 0; i < size; i++)
	{
		std::cout << arr[i] << " ";
	}
	std::cout << "\n";
}

Array::~Array()
{
	delete[] arr;
}

void utilities::swap(int& a, int& b)
{
	int temp = b; b = a; a = temp;
}

void utilities::copy(int& source, int& dest)
{
	dest = source;
}

int& utilities::max(int n_args, ...)
{
	va_list ap;
	va_start(ap, n_args);
	int max = va_arg(ap, int);
	for (int i = 2; i <= n_args; i++) {
		int a = va_arg(ap, int);
		if (a > max) max = a;
	}
	va_end(ap);
	return max;
}

int& utilities::min(int n_args, ...)
{
	va_list ap;
	va_start(ap, n_args);
	int min = va_arg(ap, int);
	for (int i = 2; i <= n_args; i++) {
		int a = va_arg(ap, int);
		if (a < min) min = a;
	}
	va_end(ap);
	return min;
}

int postfixCalculation::getIndex(char op)
{
	char ch[5] = { '-', '+', '*', '/', '0' };
	int index = 0;
	for (int i = 0; i < 4; i++)
	{
		if (ch[i] == op)
			return index;
		index++;
	}
	return -1;
}

bool postfixCalculation::isInt(char op)
{
	return (int(op) > 47 && int(op) < 58) ? true : false;
}

void postfixCalculation::infix_to_postfix(std::string str, Stack* output)
{
	Stack* oper = new Stack();

	for (unsigned int i = 0; i < str.size(); i++)
	{
		if (str[i] >= char('0') && str[i] <= char('9'))
		{
			output->push(int(str[i]));
		}
		else {
			if (oper->top() != -1) {
				if (getIndex(char(oper->top())) < getIndex(str[i]))
				{
					oper->push(int(str[i]));
				}
				else {
					if (str[i] == '(') {
						oper->push(int(str[i]));
						continue;
					}
					if (str[i] == ')') {
						while (char(oper->top()) != '(' && oper->top() != -1)
							output->push(oper->pop());
						if (char(oper->top()) == '(')
							oper->pop();
						else if (oper->top() == -1)
							return;
					}

					while (getIndex(char(oper->top())) >= getIndex(str[i]) &&
						oper->top() != -1)
					{
						output->push(oper->pop());
					}
				}
			}
			else if (oper->top() == -1)
			{
				oper->push(int(str[i]));
			}
		}
	}

	while (oper->top() != -1)
		output->push(oper->pop());

	return;
}

int postfixCalculation::calculate_postfix(Stack* output)
{
	if (output->top() == -1) std::invalid_argument;
	Stack* ans = new Stack();
	while (output->getsize() != -1)
	{
		if (isInt(output->front()))
			ans->push(int(output->pop(0)) - 48);
		else
		{
			int val1 = int(ans->pop());
			int val2 = int(ans->pop());
			if (char(output->front()) == char('-'))
				ans->push(val2 - val1);
			else if (char(output->front()) == char('+'))
				ans->push(val2 + val1);
			else if (char(output->front()) == char('/'))
				ans->push(val2 / val1);
			else
				ans->push(val2 * val1);
			output->pop(0);
		}
	}
	if (ans->getsize() != 1)
		std::invalid_argument;
	return ans->pop();
}

std::string postfixCalculation::postfix_to_infix(Stack* input)
{
	//Stack* ans = new Stack();

	//for (int i = 0; i < input->getsize(); i++)
	//{
	//	if (isInt(input->top()))
	//	{
	//		ans->push(input->pop());
	//	}
	//	else if (getIndex(input->top()) != -1)
	//	{
	//		int val = input->pop();
	//		int val1 = input->pop();

	//	}

	//	ans->push(input->pop());
	//}
	return " ";
}

int dsa_mit::find_peak_1D(Array* arr)
{
	int size = arr->getsize();
	if (size == 0) return -1;
	if (size == 1) return (*arr)[size - 1];
	int mid = size / 2;
	// Go Left
	if ((*arr)[mid] < (*arr)[mid - 1])
	{
		while (mid != 0)
		{
			if ((*arr)[mid] > (*arr)[mid - 1])
				return (*arr)[mid];
			mid--;
		}
		return (*arr)[mid];
	}
	else if ((*arr)[mid] < (*arr)[mid + 1])
	{
		while (mid != size - 1)
		{
			if ((*arr)[mid] > (*arr)[mid + 1])
				return (*arr)[mid];
			mid++;
		}
		return (*arr)[mid];
	} 
	else {
		return (*arr)[mid];
	}
}

void dsa_mit::dot_product(std::map<std::string, int> map1, std::map<std::string, int> map2, std::map<std::string, int>& output)
{
	for (auto& it = map1.begin(); it != map1.end(); ++it)
	{
		if (map2[it->first] != NULL)
		{
			output[it->first] = it->second * map2[it->first];
		}
		else {
			output[it->first] = 0;
		}
	}

	for (auto& it = map2.begin(); it != map2.end(); ++it)
	{
		if (output[it->first] == NULL)
		{
			output[it->first] = it->second * map1[it->first];
		}
	}
}

void dsa_mit::split_document(const char* doc, std::map<std::string, int>& output)
{
	std::string str = "";
	int doc_size = strlen(doc);
	for (int i = 0; i < doc_size; i++)
	{
		if (doc[i] != char(' '))
			str = str + doc[i];
		
		else if (doc[i] == char(' '))
		{
			if (output[str] == NULL)
				output[str] = 1;
			else
				output[str] = output[str] + 1;

			str = "";
		}
	}
}

int dsa_mit::binary_search(int val, Array arr, int start, int end)
{
	int mid = (start + end) / 2;
	if (val == arr[mid])
		return mid;
	else if (val > arr[mid])
		binary_search(val, arr, mid + 1, end);
	else if (val < arr[mid])
		binary_search(val, arr, start, mid - 1);
	else return -1;
}

int dsa_mit::insertion_search(int val, Array arr, int start, int end)
{
	for (int i = start; i < end; i++) 
		if (arr[i] == val)
			return i;
	return -1;
}

void dsa_mit::insertion_sort(Array* arr, int size, bool asc)
{
	for (int i = -1; i < size - 2; i++){
		bool flag = false;
		for (int j = i + 1; j < size - 1; j++){
			if ((*arr)[j] >(*arr)[j + 1]){
				flag = true;
				utilities::swap((*arr)[j], (*arr)[j + 1]);
			}
		}
		if (!flag) return;
	}

}

void dsa_mit::merge_array(Array* arr, int s1, int e1, int s2, int e2, int gap)
{
	// Failed
	//int size2 = e2 - s2;
	//int size1 = e1 - s1;
	//if (size2 == size1)
	//{
	//	int index = 0;
	//	while (index < size2)
	//	{
	//		if ((*arr)[s1 + index] >(*arr)[s2 + index])
	//			utilities::swap((*arr)[s1 + index], (*arr)[s2 + index]);
	//		index++;
	//	}
	//}
	//else if (size2 > size1)
	//{
	//	int index = 0;
	//	while (index < size2)
	//	{
	//		if (index < size1){
	//			if ((*arr)[s1 + index] >(*arr)[s2 + index])
	//				utilities::swap((*arr)[s1 + index], (*arr)[s2 + index]);
	//		}
	//		else break;
	//		index++;
	//	}
	//	utilities::swap((*arr)[s2], (*arr)[e2 - 1]);
	//}
	//else
	//{
	//	int index = 0;
	//	while (index < size1)
	//	{
	//		if (index < size2){
	//			if ((*arr)[s1 + index] >(*arr)[s2 + index])
	//			{
	//				utilities::swap((*arr)[s1 + index], (*arr)[s2 + index]);
	//				dsa_mit::merge_sort(arr, s2 + index, e2);
	//			}
	//		}
	//		else break;
	//		index++;
	//	}
	//}

	if (gap == 0)
		return;

	int end = gap + s1;
	int start = s1;
	while (end < e2)
	{
		if ((*arr)[start] > (*arr)[end])
			utilities::swap((*arr)[start], (*arr)[end]);
		start++; end++;
	}

	merge_array(arr, s1, e1, s2, e2, gap / 2);
}

void dsa_mit::merge_sort(Array* arr, int start, int end)
{
	if (end - start == 1)
		return;
	int mid = (start + end) / 2;

	merge_sort(arr, start, mid);
	merge_sort(arr, mid, end);

	int gap = (end - start) / 2;

	merge_array(arr, start, mid, mid, end, gap);
}