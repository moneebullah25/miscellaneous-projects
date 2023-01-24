#pragma once

#include <iostream>

#define NULL 0 

class QueueUV
{
private:
	int* queue_array;
	int item_size;
	int front, rear, max_size;

public:
	QueueUV();
	QueueUV(int size);
	void push(int data);
	int pop();
	int capacity();
	int size();
	void display();
	~QueueUV();
};

