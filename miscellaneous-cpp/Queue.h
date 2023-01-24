#pragma once

#define NULL 0

#include <iostream>

struct NodeP {
	int data;
	NodeP* next;
	NodeP();
	NodeP(int data);
};

class Queue
{
private:
	NodeP* head;
	NodeP* tail;
	void insert(int data);
	int deleteTail();
	int deleteHead();

public:
	Queue();
	void push(const int& a);
	int pop(int index = 0);
	int top();
	int front();
	void display(int ch = 1);
	int getsize();
	~Queue();
};

