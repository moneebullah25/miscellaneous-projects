#include <iostream>

using namespace std;


class hashing
{
private:
	int* hash_table;
	int item_size;
	int table_size;
	void _insert(int* hash_table, int key)
	{
		if (item_size != table_size)
		{
			if (hash_table[hash_function(key)] != -99)
			{
				int index = hash_function(key);
				while (hash_table[index] != -99)
				{
					index++;
					if (index == table_size)
					{
						index = 0;
					}
				}
				hash_table[index] = key;
				item_size++;
				cout << "Collision Occur\n";
				return;
			}
			hash_table[hash_function(key)] = key;
			item_size++;
		}
		else
		{
			resize();
			_insert(this->hash_table, key);
		}
	}
public:
	hashing(int size = 11)
	{
		table_size = size;
		hash_table = new int[table_size];
		for (int i = 0; i < table_size; i++)
			hash_table[i] = -99;
	}

	bool search(int key)
	{
		if (hash_table[hash_function(key)] == key)
			return hash_table[hash_function(key)];
		int index = hash_function(key);
		int count = 0;
		while (hash_table[index] != key)
		{
			index++;
			if (index == table_size)
			{
				index = 0;
				count += 1;
				if (count == 2)
					return false;
			}
			
		}
		return true;
	}

	void insert(int key)
	{
		_insert(this->hash_table, key);
	}

	void display()
	{
		for (int i = 0; i < table_size; i++)
			cout << hash_table[i] << " ";
		cout << "\n";
	}

	int hash_function(int key)
	{
		return key % table_size;
	}

	void delete_val(int key)
	{
		if (hash_table[hash_function(key)] == key)
		{
			hash_table[hash_function(key)] = -99;
			item_size--;
		}
		// Collision happened before
		else
		{
			int index = hash_function(key);
			while (hash_table[index] != key)
			{
				index++;
				if (index == table_size)
				{
					index = 0;
				}

			}
			hash_table[index] = -99;
			item_size--;
		}
	}

	void resize()
	{
		table_size = table_size * 2;
		item_size = 0;
		int* hash_table2 = new int[table_size];
		for (int i = 0; i < table_size; i++)
			hash_table2[i] = -99;
		for (int i = 0; i < table_size / 2; i++)
		{
			_insert(hash_table2, hash_table[i]);
		}
		delete[] hash_table;
		hash_table = hash_table2;
	}
};



int main()
{
	hashing* h = new hashing();
	h->insert(0);
	h->insert(1);
	h->insert(2);
	h->insert(3);
	h->insert(4);
	h->insert(5);
	h->insert(6);
	h->insert(7);
	h->insert(9);
	h->insert(10);
	h->display();
	h->insert(11);
	h->insert(12); // must resize
	h->search(8);// must return false
	h->display();
	h->delete_val(12); // delete 12
	h->search(12);
	h->display();

}
