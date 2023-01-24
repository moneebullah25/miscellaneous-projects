#include "QueueUV.h"


QueueUV::QueueUV()
	: item_size(0), front(0), rear(0), max_size(1)
{
	queue_array = new int[max_size];
}

QueueUV::QueueUV(int size)
	: item_size(0), front(0), rear(0), max_size(size)
{
	queue_array = new int[max_size];
}

void QueueUV::push(int data)
{
	if (item_size < max_size)
	{
		queue_array[rear] = data;
		item_size++; rear++;
	}
	else if (item_size == max_size)
	{
		if (front == 0) {
			int* queue_array_2 = new int[max_size * 2];
			for (int i = front; i < rear; i++)
			{
				queue_array_2[i] = queue_array[i];
			}
			queue_array_2[rear] = data;
			item_size++; rear++;
			delete[] queue_array;
			queue_array = queue_array_2;
			max_size = max_size * 2;
			queue_array_2 = NULL;
		}
		else if (front == rear && item_size == 0)
		{
			front = 0;
			rear = 0;
			queue_array[rear] = data;
			item_size++; rear++;
		}
		else if (front == rear && max_size == item_size)
		{
			int* queue_array_2 = new int[max_size * 2];
			for (int i = front; i < max_size; i++)
			{
				queue_array_2[i] = queue_array[i];
			}
			for (int i = 0; i < rear; i++)
			{
				queue_array_2[i] = queue_array[i];
			}
			front = 0;
			rear = max_size;
			max_size = max_size * 2;
			queue_array_2[rear] = data;
			item_size++; rear++;
		}
		else {
			rear = 0;
			queue_array[rear] = data;
			rear++; item_size++;
		}
	}
}

int QueueUV::pop()
{
	if (rear != 0 && front != rear) {
		int val = queue_array[front];
		front++; 
		item_size--;
		return val;
	}
	else if (front == rear && item_size == max_size)
	{
		int val = queue_array[front];
		front++;
		item_size--;
		return val;
	}
	else if (front == rear && item_size == 0)
	{
		return -1;
	}
}

int QueueUV::capacity()
{
	return max_size;
}

int QueueUV::size()
{
	return item_size;
}

void QueueUV::display()
{
	if (front != 0) {
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
		for (int i = front; i < max_size; i++)
		{
			std::cout << queue_array[i] << " ";
		}
	}
}

QueueUV::~QueueUV()
{
	delete[] queue_array;
}