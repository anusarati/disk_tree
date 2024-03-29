# Disk Tree

Implements an AVL tree that allocates its nodes on a file, which can be stored
on a disk. Also implements some parts of std::set and std::map.

Written mostly in 2022. A light touchup was given to improve the code quality, but there are no plans to maintain the code.

For other implementations of AVL trees see Ben Pfaff's
[website](https://adtinfo.org/).

This doesn't support reading/writing heap allocated types like std::string because it uses direct binary deserialization from a file.

## Example
```c++
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
```

Output

![Example output](example.jpg)

## LICENSE
[0BSD](LICENSE)