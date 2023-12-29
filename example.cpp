#include <iostream>
#include <random>

#include "disk_map.h"

using namespace std;
int main()
{
    // initialize a map that uses the disk for memory
    DiskMap<int, int> map;
    map.init_allocator();

    uniform_int_distribution key_distribution{-100, 100};
    minstd_rand rng;

    // add 10 random keys with values equal to the order in which
    // they were inserted to the map
    for (int i = 1; i <= 10; ++i)
    {
        int key = key_distribution(rng);
        cout << "Key: " << key << "; Value: " << i << '\n';
        map[key] = i;
    }

    // print the map in-order, sorted
    cout << "\nIn-order:\n";
    for (auto [key, value] : map)
    {
        cout << "Key: " << key << "; Value: " << value << '\n';
    }
    return 0;
}
