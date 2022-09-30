# Disk Tree

Implements an AVL tree that allocates its nodes on a file, which can be stored
on a disk. Also implements some parts of std::set and std::map.

For other implementations of AVL trees see Ben Pfaff's
[website](https://adtinfo.org/).

It might not work well with std dynamically allocated types (e.g. std::string) because I think it could attempt to free the memory in RAM multiple times after getting references to the same disk memory, reading in the object, and letting the reference go out of scope, which calls its destructor and the destructor of the object data.

## LICENSE
0BSD

Thanks to the Buddha, Wikipedia, en.cppreference.com, the C++ standards committee or
working group, and anyone else who helped me.
