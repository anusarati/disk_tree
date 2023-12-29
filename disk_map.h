#ifndef DISK_MAP
#define DISK_MAP
#include "disk_tree.h"
template <typename Key, typename Value, typename KeyCompare = std::less<Key>>
class DiskMap
	: private DiskTree<pair<Key, Value>, KeyCompare>
{
public:
	using Pair = pair<Key, Value>;
	struct ValueCompare
	{
		static constexpr KeyCompare c{};
		bool operator()(const Pair &a, const Pair &b) { return c(a.first, b.first); }
	};
	using tree_t = DiskTree<Pair, ValueCompare>;
	// thanks to en.cppreference.com and GCC and Metamath
	typedef tree_t::iterator iterator;
	struct mapped_t // use auto instead of Value when replacing std::map
	{
		tree_t::reference r;
		mapped_t(const Value &other) = delete;
		// https://stackoverflow.com/a/19819249
		mapped_t &operator=(const mapped_t &other)
		{
			const typename tree_t::Node &node = other.r.d;
			return operator=(node.key); // r.d is Node, d.key is Value
		}
		mapped_t(const iterator &i) { r = typename tree_t::reference(i); }
		mapped_t &operator=(const Value &right)
		{
			// fortunately the iterator isn't const qualified ( if you want const qualification for a disk_set you can just use a const parameter for disk_tree )
			r->key.second = right;
			r.write();
			return *this;
		}
		// https://huixie90.github.io/Almost-always-const-auto-ref
		mapped_t &operator=(const auto &something) { return operator=((Value)something); }
		operator Value() { return r->key.second; }
	};
	inline static Pair incomplete_pair(auto &&first)
	{
		Pair p;
		p.first = first;
		return p;
	}
	static void init_allocator()
	{
		tree_t::init_allocator();
	}
	static void init_allocator(auto &&dstream, auto &&sstream)
	{
		tree_t::init_allocator(dstream, sstream);
	}
	inline auto inner_insert(const Key &d) { return tree.insert(incomplete_pair(d)); }
	inline auto inner_find(const Key &d) { return tree.find(incomplete_pair(d)); }
	tree_t tree;
	size_t size() { return tree.size(); }
	bool empty() { return tree.empty(); }
	mapped_t at(const Key &arg)
	{
		if (tree.root)
			return inner_find(arg);
		return tree.end();
	}
	mapped_t operator[](const Key &arg)
	{
		auto f = inner_find(arg);
		iterator i;
		if (f == tree.end())
			i = inner_insert(arg).first;
		else
			i = move(f);
		return i;
	}
	pair<iterator, bool> insert(const Key &d)
	{
		auto i = inner_insert(d);
		return make_pair(iterator{i.first}, i.second);
	}
	inline bool erase(const Key &d) { return tree.erase(incomplete_pair(d)); }

	iterator begin() { return tree.begin(); }
	iterator end() { return tree.end(); }
	iterator find(const Key &arg)
	{
		return inner_find(arg);
	}
	bool contains(const Key &arg) { return tree.contains(incomplete_pair(arg)); }
	// https://en.cppreference.com/w/cpp/container/map/lower_bound
	iterator lower_bound(const Key &arg) { return tree.lower_bound(incomplete_pair(arg)); }
	// https://en.cppreference.com/w/cpp/container/map/upper_bound
	iterator upper_bound(const Key &arg) { return tree.upper_bound(incomplete_pair(arg)); }
	iterator erase(const iterator &i)
	{
		return tree.erase(i);
	}
	// https://en.cppreference.com/w/cpp/language/derived_class
	struct node_type : tree_t::node_type
	{
		// https://en.cppreference.com/w/cpp/language/this
		node_type(const tree_t::node_type &p) { this->r = p.r; }
		// https://en.cppreference.com/w/cpp/container/node_handle
		Key &key() { return this->r->d.first; }
		Value &mapped() { return this->r->d.second; }
	};
	node_type extract(const iterator &i)
	{
		return tree.extract(i);
	}
	typedef tree_t::reverse_iterator reverse_iterator;

	reverse_iterator rbegin()
	{
		return tree.rbegin();
	}
	reverse_iterator rend()
	{
		return tree.rend();
	}
	void clear() { tree.clear(); }

private:
};

#endif /* DISK_MAP */
