#pragma once

#include <iostream>

#define NULL 0 

class StackUV
{
private:
	int* stack_array;
	int item_size;
	int front, rear, max_size;

public:
	StackUV();
	StackUV(int size);
	void push(int data);
	int pop();
	int capacity();
	int size();
	void display();
	~StackUV();
};

