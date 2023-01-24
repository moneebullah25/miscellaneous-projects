#include "StackUV.h"


StackUV::StackUV()
	: item_size(0), front(0), rear(0), max_size(1)
{
	stack_array = new int[max_size];
}

StackUV::StackUV(int size)
	: item_size(0), front(0), rear(0), max_size(size)
{
	stack_array = new int[max_size];
}

void StackUV::push(int data)
{
	if (item_size < max_size)
	{
		stack_array[rear] = data;
		item_size++; rear++;
	}
	else if (item_size == max_size)
	{
		int* stack_array_2 = new int[max_size * 2];
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

int StackUV::pop()
{
	if (rear != 0) {
		rear = rear - 1;
		item_size = item_size - 1;
		int val = stack_array[rear];
		return val;
	}
	else {
		return -1;
	}
}

int StackUV::capacity()
{
	return max_size;
}

int StackUV::size()
{
	return item_size;
}

void StackUV::display()
{
	for (int i = front; i < rear; i++)
	{
		std::cout << stack_array[i] << " ";
	}
}

StackUV::~StackUV()
{
	delete[] stack_array;
}