#pragma once

#include <map>
#include "Stack.h"
#include <vector>
#include <cstdarg>

class heap: public Array
{
private:
	Array* arr;
	void maxify_heap(int start, int end);
	void minify_heap(int start, int end);
	void printBT(const std::string& prefix, int index, bool isLeft);
public:
	heap(int size = 4);
	heap(std::vector<int> vec);
	heap(const Array* const ar);
	void append(const int& data);
	void maxify();
	void minify();
	void build_max_heap();
	void display();
};

class Array
{
private:
	int* arr;
	int size;
	int capacity;
public:
	Array(const int& reserve = 4);
	Array(std::vector<int> vec);
	void append(int data);
	int& operator[](int index);
	int Array::operator[](int index) const;
	void operator=(int data);
	int getsize() const;
	void printArray() const;
	~Array();
};

namespace postfixCalculation{
	int getIndex(char op);
	bool isInt(char op);
	void infix_to_postfix(std::string str, Stack* output);
	int calculate_postfix(Stack* output);
	std::string postfix_to_infix(Stack* input);
}

namespace utilities{
	void copy(int& source, int& dest);
	void swap(int& a, int& b);
	int& max(int n_args, ...);
	int& min(int n_args, ...);
}

namespace dsa_mit{
	int find_peak_1D(Array* arr);
	void dot_product(std::map<std::string, int> map1, std::map<std::string, int> map2, std::map<std::string, int>& output);
	void split_document(const char* doc, std::map<std::string, int>& output);
	int binary_search(int val, Array arr, int start, int end);
	int insertion_search(int val, Array arr, int start, int end);
	void insertion_sort(Array* arr, int size, bool asc = true);
	void merge_array(Array* arr, int s1, int e1, int s2, int e2, int gap);
	void merge_sort(Array* arr, int start, int end);

}