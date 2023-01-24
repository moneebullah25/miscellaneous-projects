#pragma once

#define NULL 0

#include <iostream>
#include <string>

struct NodeS {
	int data;
	NodeS* next;
	NodeS();
	NodeS(int data);
};

class Stack
{
private:
	NodeS* head;
	NodeS* tail;
	void insert(int data);
	int deleteTail();
	int deleteHead();
public:
	Stack();
	void push(const int& a);
	int pop(int index = -1);
	int top();
	int front();
	void display(int ch = 1);
	int getsize();
	~Stack();
	
};
