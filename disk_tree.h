#ifndef DISK_TREE
#define DISK_TREE
#include <bitset>
#include <initializer_list>
#include <utility>
#include <optional>
#include <set>
#include "disk_allocator.h"
using namespace std;

// https://en.wikipedia.org/wiki/AVL_tree
//  Implements the AVL tree
//  https://en.cppreference.com/w/cpp/container/set
//  also mimics part of std::set
//  it doesn't attempt to insert data that is equal under Compare::()
template <typename Key, typename Compare = std::less<Key>>
class DiskTree // https://en.cppreference.com/w/cpp/named_req/Compare
{

public:
	// the allocations are for nodes
	struct Node;

private:
	// https://en.cppreference.com/w/cpp/container/set
	typedef disk_allocator<Node> allocator_type;
	static inline allocator_type al{}; // the allocator is for allocating nodes
public:
	static inline Compare compare{};

	static void init_allocator()
	{
		allocator_type::init();
	}
	static void init_allocator(auto &&dstream, auto &&sstream)
	{
		allocator_type::init(dstream, sstream);
	}
	using pointer = allocator_type::pointer;
	using reference = allocator_type::reference;
	void clear(pointer &p)
	{
		if (p)
		{
			auto r = *p;
			r->key.~Key();
			al.deallocate(p); // deallocation doesn't replace memory with default constructed Node
			clear(r->left);
			clear(r->right);
		}
	}
	class Node
	{
		friend DiskTree;

	public:
		Key key;
		pointer left, right; // should be usable like Node*, except for new and delete
		// not overloading new or delete to work with the disk allocator
		// the disk allocator doesn't work with std::set
		// what if there was one pointer to the branches and space for two nodes was allocated for each Node's branch pointer?

		Node &operator=(const Key &data)
		{
			key = data;
			return *this;
		}
		// https://en.cppreference.com/w/cpp/memory/new/operator_new#Class-specific_overloads
		signed char balance = 0;
		inline operator Key() { return key; }
		inline Key value() { return key; }
		// root Node is at depth 1
		unsigned char exhaustiveDepth() const // I don't think a larger ret type is necessary because a tree with depth 256 can have more than 1e76 elements
		{
			// inefficient, check all branches, linear in number of nodes
			// with a valid AVL tree, one could look at the balance and go through a single branch
			// choosing the side the balance falls on, or either if balance==0
			if (!(left || right))
				return 1;
			if (!right)
				return left->exhaustiveDepth() + 1;
			if (!left)
				return right->exhaustiveDepth() + 1;
			return max(left->exhaustiveDepth(), right->exhaustiveDepth()) + 1;
		}
		bool correctBalance() const // this uses exhaustiveDepth because depth relies on balance
		{
			unsigned char a = 0, b = 0;
			if (left)
				a = left->exhaustiveDepth();
			if (right)
				b = right->exhaustiveDepth();
			// cout<<"a "<<(int)a<<" b "<<(int)b<<" balance "<<(int)balance<<endl;
			if (balance != (b - a))
				return false;
			if (left && !left->correctBalance())
				return false;
			if (right && !right->correctBalance())
				return false;
			return true;
		}
		unsigned char depth() // logarithmic complexity
		{
			if (balance == -1)
				return left->depth() + 1;
			else if (balance == 1)
				return right->depth() + 1;
			else
			{
				if (!(left || right))
					return 1;
				if (!left)
					return right->depth() + 1;
				return left->depth() + 1;
			}
		}

		inline optional<reference> min_branch() { return extreme_branch<true>(); }
		inline optional<reference> max_branch() { return extreme_branch<false>(); }

		inline Key min_section_value() { return extreme_section_value<true>(); }
		inline Key max_section_value() { return extreme_section_value<false>(); }

		inline optional<reference> min_ancestor() { return extreme_ancestor<true>(); }
		inline optional<reference> max_ancestor() { return extreme_ancestor<false>(); }
		// thanks to Norman Megill and Metamath contributors
		bool well_ordered_unique()
		{
			if (left && !left->well_ordered_less_than(key))
				return false;
			if (right && !right->well_ordered_greater_than(key))
				return false;
			return true;
		}
		bool well_ordered_greater_than(const Key &lesser) // assuming lesser is from ancestor, then everything in its right branch should be greater
		{
			if (!compare(lesser, key))
				return false;
			if (left && !left->well_ordered_greater_than(lesser))
				return false;
			if (right && !right->well_ordered_greater_than(key))
				return false;
			return true;
		}
		bool well_ordered_less_than(const Key &greater)
		{
			if (!compare(key, greater))
				return false;
			if (left && !left->well_ordered_less_than(key))
				return false;
			if (right && !right->well_ordered_less_than(greater))
				return false;
			return true;
		}

	private:
		// thanks to the calculus textbook by James Stewart
		// and the ones by Edward Herman and Gilbert Strang https://openstax.org/details/books/calculus-volume-1
		// for extreme name
		// https://en.cppreference.com/w/cpp/utility/optional
		template <bool min>
		optional<reference> extreme_branch()
		{
			auto &branch_ptr = min ? left : right;
			if (branch_ptr)
			{
				reference branch_ref;
				// thanks to
				// https://en.wikipedia.org/wiki/Optimizing_compiler#Loop_optimizations
				do
				{
					branch_ref = *branch_ptr;
					branch_ptr = min ? branch_ref->left : branch_ref->right;
				} while (branch_ptr);
				return branch_ref;
			}
			return nullopt;
		}
		// returns the greatest or least value among this Node and its branches
		// thanks to Wikipedia article (https://en.wikipedia.org/wiki/AVL_tree ?) for the word subtree
		template <bool min>
		Key extreme_section_value()
		{
			auto maybe_branch = extreme_branch<min>();
			if (maybe_branch)
				return maybe_branch.value()->key;
			return key;
		}
		// unnecessary?
		template <bool min>
		optional<reference> extreme_ancestor()
		{
			auto &p = min ? left : right;
			if (p)
			{
				reference a, r = *p;
				if (p = min ? r->left : r->right)
				{
					do
					{
						a = r;
						r = *p;
						p = min ? r->left : r->right;
					} while (p);
					return a;
				}
			}
			return nullopt;
		}
		// thanks to Wikipedia articles on graphs
		// used to implement contains
		// thanks to GCC and gdb for showing me that contains calls find or something like that
		// search for key in tree with binary search, if it can't be found it's not there
		bool reachable(const Key &r)
		{
			if (compare(r, key))
				return left && left->reachable(r);
			if (compare(key, r))
				return right && right->reachable(r);
			return true;
		}
	};
	void clear(Node &n)
	{
		n.key.~dt();
		clear(n.left);
		clear(n.right);
	}

	// https://en.cppreference.com/w/cpp/container/node_handle
	// https://eel.is/c++draft/container.Node
	struct PreservedNode
	{
		reference r;
		constexpr PreservedNode() noexcept : r() {} // r's location should be where a disk nullptr would point to
		PreservedNode(PreservedNode &&n) noexcept
		{
			r = move(n.r);
			n.r.l = SIZE_MAX;
		}
		PreservedNode &operator=(PreservedNode &&n)
		{
			if (&r)
				al.deallocate(&r);
			r = move(n.r);
			n.r.l = SIZE_MAX;
		}
		~PreservedNode()
		{
			if (&r)
				al.deallocate(&r);
			r.l = SIZE_MAX;
		}
		[[nodiscard]] bool empty() const noexcept { return !&r; }
		explicit operator bool() const noexcept { return bool(&r); }
		Key &value() const { return r->key; }
		// thanks to those who taught/reminded me about using a temporary variable for swap
		void swap(PreservedNode &n) noexcept
		{
			auto intermediate = move(n);
			n = move(*this);
			*this = move(intermediate);
		}
		PreservedNode(reference &&rr) : r(rr) {}
	};
	typedef PreservedNode node_type; // this isn't used except for extract
	PreservedNode extract(const Key &f)
	{
		if (root)
		{
			auto r = *root;
			auto &n = r.key;
			bool depthDecreased = false;
			if (compare(f, n.key))
				return extract<false>(r, n, f, depthDecreased);
			else if (compare(n.key, f))
				return extract<true>(r, n, f, depthDecreased);
			else
			{
				if (n.balance >= 0)
				{
					auto &right = n.right;
					if (right)
						root = &resume<false>(right, n.left, depthDecreased); // rotate left
					else
						root = pointer(nullptr); // no left because balance==0 because if balance>0 and the tree is working properly there would be a right branch
				}
				else
					root = &resume<true>(n.left, n.right, depthDecreased);
				// thanks to my CS teacher Shankar Kumar
				--this->n; // this->n is for number of elements
				reset_memo();
				return r;
			}
		}
		return {};
	}
	DiskTree() {}
	DiskTree(initializer_list<Key> l)
	{
		for (auto &key : l)
			insert(key);
	}
	// convert from std::set
	DiskTree(const set<Key, Compare> &counterpart)
	{
		for (auto &&key : counterpart)
			insert(key);
	}
	template <typename iterator_t>
	DiskTree(iterator_t i, const iterator_t &e)
	{
		while (i != e)
		{
			insert(*i);
			++i;
		}
	}
	DiskTree(DiskTree<Key, Compare> &&t) { root = move(t.root); }
	DiskTree(const DiskTree<Key, Compare> &t) { copy(root, t.root); }
	DiskTree<Key, Compare> &operator=(const DiskTree<Key, Compare> &t)
	{
		clear();
		copy(root, t.root);
		return *this;
	}
	DiskTree<Key, Compare> &operator=(DiskTree<Key, Compare> &&t)
	{
		clear();
		root = move(t.root);
		// thanks to https://en.cppreference.com/w/cpp/container/node_handle
		t.root = nullptr;
		return *this;
	}
	void clear() { clear(root); }
	~DiskTree() { clear(); }

	size_t size() { return n; }
	bool empty() { return !n; }

	bool erase(const Key &key)
	{
		bool b = false;
		return erase(root, key, b);
	}
	struct iterator // consumes a lot of space, not good for parallel iteration on small trees
	{
		iterator() = default;
		iterator(const pointer &p) { nodes[0] = p; }
		pointer nodes[8 * sizeof(size_t)]; // ancestors then current child up to level
		unsigned char level = 0;
		// if there are 8*sizeof(size_t) levels, a full AVL tree would have SIZE_MAX elements
		std::bitset<8 * sizeof(size_t)> right_levels; // used like a stack, with the 1 bits representing which nodes had their right child Node reached
													  // right_levels should take same space as a size_t
		template <bool leftwards>
		pointer traverse_extreme()
		{
			// define the child pointer to traverse based on `leftwards`
			pointer Node::*child = leftwards ? &Node::left : &Node::right;
			// the right branches of ancestors have been visited
			// if the extreme is towards the right
			const bool visited_right = !leftwards;
			pointer current = nodes[level];
			while (current = (*current).d.*child)
			{
				right_levels[level] = visited_right;
				nodes[++level] = current;
			}
			return current;
		}
		pointer traverse_min() { return traverse_extreme<true>(); }
		pointer traverse_max() { return traverse_extreme<false>(); }

		iterator &operator++()
		{
			if (level < 8 * sizeof(size_t))
			{
				auto &p = nodes[level];
				if (p) // allows ++--begin() to be iterator at first element
				{
					pointer c = p->right;
					if (c)
					{
						auto i = level++;
						nodes[level] = c;
						traverse_min();
						right_levels[i] = true;
					}
					else
					{
						auto bl = level;
						while (level > 0 && right_levels[level - 1])
							--level;
						if (level > 0) // right ancestor
							level--;
						else
						{
							level = bl + 1;
							right_levels[bl] = true;
							if (level < 64)
								nodes[level] = nullptr;
							return *this;
						}
						auto reset = (bitset<8 * sizeof(size_t)>)SIZE_MAX >> (8 * sizeof(size_t) - 1 - level); // it could fail depending on endianness without the conversion to bitset
						right_levels &= reset;
					}
				}
				else if (level > 0 && !right_levels[level - 1])
				{
					--level;
				}
			}
			return *this;
		}
		Key peek() const // get next element without advancing iterator (like std::basic_istream::peek() https://en.cppreference.com/w/cpp/io/basic_istream/peek)
		{
			auto &p = nodes[level];
			if (p)
			{
				pointer c = p->right;
				auto l = level;
				if (c)
				{
					auto r = *c;
					return r->min_section_value();
				}
				else
				{
					while (l > 0 && right_levels[l - 1])
						--l;
					if (l > 0) // right ancestor
						l--;
					else
						return Key{};
					return nodes[l]->key;
				}
			}
			return Key{};
		}
		// thanks to GCC for showing me that postfix ++ has int parameter
		// https://en.cppreference.com/w/cpp/language/operators
		iterator operator++(int)
		{
			auto before = *this;
			operator++();
			return before;
		}
		iterator &operator--()
		{
			if (level < 8 * sizeof(size_t))
			{
				auto &p = nodes[level];
				if (p) // allows --end() to be iterator at last element
				{
					pointer c = p->left;
					if (c)
					{
						auto i = level++;
						nodes[level] = c;
						traverse_max();
						right_levels[i] = false;
					}
					else
					{
						auto bl = level;
						while (level > 0 && !right_levels[level - 1])
							--level;
						if (level > 0)
							level--;
						else
						{
							level = bl + 1;
							if (level < 64)
								nodes[level] = nullptr;
							return *this;
						} // mimic standard (as implemented by GCC)
						right_levels[level] = false;
						auto reset = (bitset<8 * sizeof(size_t)>)SIZE_MAX >> (8 * sizeof(size_t) - 1 - level); // it could fail depending on endianness without the conversion to bitset
						right_levels &= reset;
					}
				}
				else if (level > 0 && right_levels[level - 1])
					right_levels[--level] = false;
			}
			return *this;
		}
		iterator operator--(int)
		{
			auto before = *this;
			operator--();
			return before;
		}

		bool operator==(const iterator &i) // logarithmic complexity
		{
			if (level != i.level)
				return false;
			for (char l = level; l >= 0; l--)
				if (nodes[l] != i.nodes[l])
					return false; // checking before nodes[level] and i.nodes[level] isn't necessary if it's known the iterators are from the same tree, and then the complexity can be reduced
			return true;
		}
		Key operator*() // mimic std container iterators, (use auto&& or dt for range for iteration)
		{
			return nodes[level]->key;
		}
		template <bool rightwards>
		bool branch(const reference &r)
		{
			auto &side = rightwards ? r.d.right : r.d.left;
			if (side)
			{
				right_levels[level] = rightwards;
				nodes[++level] = side;
				return true;
			}
			return false;
		}
		inline bool left(const reference &r) { return branch<false>(r); }
		inline bool right(const reference &r) { return branch<true>(r); }
		inline bool left() { return branch<false>(operator reference()); }
		inline bool right() { return branch<true>(operator reference()); }
		operator pointer() { return nodes[level]; }
		// https://en.cppreference.com/w/cpp/language/explicit
		explicit operator pointer &() { return nodes[level]; }
		// https://en.cppreference.com/w/cpp/language/operators
		//  using -> multiple times on same iterator can result in unnecessary disk accesses
		//  let ->() access member of dt key
		Key *operator->() { return &nodes[level]->key; }
		operator reference() const { return *nodes[level]; }
	};
	// mimic std::set::insert https://en.cppreference.com/w/cpp/container/set/insert
	pair<iterator, bool> insert(const Key &key)
	{
		bool b = false, inserted;
		iterator i;
		// cout<<"insert "<<key;
		if (root)
		{
			// cout<<' '<<root.l<<' '<<root->key<<' '<<root->left<<' '<<root->right<<'\n';
			if (inserted = insert(root, key, b))
				n++;
			i = find(key); // easy solution
		}
		else
		{
			root = al.allocate();
			*root = Node{key};
			n++;
			reset_memo();
			inserted = true;
			i = {root};
		}
		// thanks to Errichto for using make_pair and William Lin
		return make_pair(i, inserted);
	}
	iterator begin()
	{
		if (begin_updated_memo)
			return begin_memo;
		iterator i{root};
		if (root != nullptr) // makes this work with comparison with end iterator for empty tree
		{
			i.traverse_min();
		}
		begin_memo = move(i);
		begin_updated_memo = true; // I had forgotten to update it for an empty tree earlier
		return i;
	}
	iterator end()
	{
		if (end_updated_memo)
			return end_memo;
		iterator i{root};
		if (root)
		{
			i.traverse_max();
			i.right_levels[i.level++] = true;
		}
		end_memo = move(i);
		end_updated_memo = true;
		return end_memo;
	}

	iterator find(const Key &key)
	{
		if (root)
		{
			iterator i{root};
			switch (approach(key, i))
			{
			case less:
				[[fallthrough]];
			case greater:
				return end();
			case equal:
				return i;
			}
		}
		return end(); // I realized the case when find is called on an empty tree for some reason
	}
	// https://en.cppreference.com/w/cpp/container/set/lower_bound
	iterator lower_bound(const Key &key)
	{
		if (root)
		{
			iterator i{root};
			switch (approach(key, i))
			{
			case less:
				[[fallthrough]];
			case equal:
				return i;
			case greater:
				return end();
			}
		}
		return end();
	}
	// https://en.cppreference.com/w/cpp/container/set/upper_bound
	iterator upper_bound(const Key &key)
	{
		if (root)
		{
			iterator i{root};
			switch (approach(key, i))
			{
			case less:
				return i;
			case greater:
				return end();
			case equal:
				return ++i;
			}
		}
		return end();
	}
	// https://en.cppreference.com/w/cpp/container/set/contains
	// thanks to GCC and gdb for showing me that contains calls find or something like that
	// search for key in tree with binary search, if it can't be found it's not there
	bool contains(const Key &key)
	{
		return root && root->reachable(key);
	}

	iterator erase(const iterator &i) // the iterator's nodes data should contain the same pointers even if the pointed data changes
	{
		bool n, depthDecreased;
		Key f;
		auto e = end();
		if (i == e)
			return e;
		if (i == --e)
			n = true;
		else
			f = i.peek();
		auto &np = (pointer &)(i);
		auto r = *np;
		erase(np, r, depthDecreased);
		if (i.level > 0)
			popBalance(i, depthDecreased);
		if (n)
			return end();
		return find(f); // logarithmic complexity
	}
	PreservedNode extract(const iterator &i)
	{
		auto &np = (pointer &)(i);
		auto r = *np;
		Node &n = r.key;
		bool depthDecreased;
		if (i.level > 0)
		{
			auto u = i.level - 1;
			extract(i.nodes[u], i.right_levels[u], np, r, depthDecreased);
			popBalance(i, depthDecreased);
		}
		else
			extract(nullptr, false, np, r, depthDecreased);
		return PreservedNode{move(r)};
	}
	struct reverse_iterator
	{
		iterator forward;
		// https://en.cppreference.com/w/cpp/iterator/make_reverse_iterator
		reverse_iterator(iterator &&i)
		{
			forward = i;
			--forward;
		}
		reverse_iterator(iterator &i)
		{
			forward = move(i);
			--forward;
		}
		Key operator*() { return *forward; }
		reverse_iterator &operator++()
		{
			--forward;
			return *this;
		}
		reverse_iterator &operator--()
		{
			++forward;
			return *this;
		}
		reverse_iterator operator++(int)
		{
			auto before = *this;
			--forward;
			return before;
		}
		reverse_iterator operator--(int)
		{
			auto before = *this;
			++forward;
			return before;
		}
		operator pointer() { return pointer(forward); }
		// https://en.cppreference.com/w/cpp/language/explicit
		explicit operator pointer &() { return (pointer &)(forward); }
		// https://en.cppreference.com/w/cpp/language/operators
		pointer operator->() { return forward; }
		operator reference() const { return reference(forward); }
		bool operator==(const reverse_iterator &i) { return forward == i.forward; }
	};
	reverse_iterator rbegin()
	{
		// https://en.cppreference.com/w/cpp/iterator/make_reverse_iterator
		return end(); // forward iterator is decremented in constructor
	}
	reverse_iterator rend()
	{
		return begin();
	}
	static reverse_iterator make_reverse_iterator(iterator i) { return reverse_iterator(i); }
	Key front()
	{
		return root->extreme_section_value(); // this isn't properly defined without root, i.e. without any elements
	}
	unsigned char depth()
	{
		return root->depth();
	}
	unsigned char exhaustiveDepth()
	{
		return root->exhaustiveDepth();
	}
	bool depthMinimal()
	{
		// 1<<x = 2 to the power of x
		return ((size_t)1) << (exhaustiveDepth() - 1) < n; // the previous depth would not be able to contain so many elements if <n
	}
	bool correctDepth()
	{
		return root->depth() == root->exhaustiveDepth();
	}
	// thanks to Norman Megill and Metamath contributors
	// slow iterator implementation of well_ordered
	// all nodes in the left branch should be <=, all nodes in the right branch should be >= for to satisfy BST properties
	bool well_ordered()
	{
		auto i = begin(), e = end();
		auto p = *i;
		while (++i != e)
		{
			if (p > *i)
				return false;
			p = *i;
		}
		return true;
	}
	bool well_ordered_unique() { return root->well_ordered_unique(); }

	// logarithmic time begin and end instead of constant https://en.cppreference.com/w/cpp/container/set/begin
	// I thought it would be better than updating begin and end with each insertion or erasure
	// because I think the checks would happen a logarithmic number of times anyways
	// optional caching to have sometimes constant time
	// actually copying the iterators could take logarithmic time anyways in this implementation
	// https://en.wikipedia.org/wiki/Memoization
	// https://en.wikipedia.org/wiki/Cache_(computing)
protected:
	iterator begin_memo, end_memo;
	bool begin_updated_memo : 1 = false, end_updated_memo : 1 = false;
	// I had mistakenly used a single flag for whether both caches were updated, but only updated one of them when turning the flag on

private:
	pointer root;
	size_t n = 0;

	template <bool leftwards>
	void rotate(reference &nr)
	{
		auto &n = nr.d;
		// thanks to https://en.wikipedia.org/wiki/Lambda_calculus
		pointer &other_side = leftwards ? n.right : n.left;
		reference r = *other_side;
		rotate<leftwards>(nr, other_side, r);
	}
	template <bool leftwards>
	void rotate(reference &nr, pointer &other_side, reference &osr)
	{
		auto &n = nr.d;
		if (osr->balance == (leftwards ? -1 : 1))
			rotate<!leftwards>(osr);
		auto new_side = al.allocate();
		// thanks Wikipedia for explaining balance as difference between max depths
		signed char branchBalance;
		if (osr->balance == (leftwards ? 1 : -1))
			branchBalance = n.balance + (leftwards ? -2 : 2);
		else
			branchBalance = n.balance + (leftwards ? -1 : 1);
		auto graft = leftwards ? osr->left : osr->right;
		pointer &side = leftwards ? n.left : n.right;
		if (leftwards)
			*new_side = Node{move(n.key), n.left, graft, branchBalance};
		else
			*new_side = Node{move(n.key), graft, n.right, branchBalance};
		side = new_side;
		n.key = move(osr->key);
		auto p = other_side;
		other_side = leftwards ? osr->right : osr->left;
		al.deallocate(p);
		if (leftwards)
		{
			if (branchBalance == 1)
				n.balance = -1; // the graft increased the left branch's depth
			else
				n.balance -= 2;
		}
		else
		{
			if (branchBalance == -1)
				n.balance = 1; // the graft increased the right branch's depth
			else
				n.balance += 2;
		}
	}
	template <bool leftwards, bool insertion>
	inline void depthBalance(const pointer &np, bool &depthChanged)
	{
		auto nr = *np;
		return depthBalance<leftwards, insertion>(nr, depthChanged);
	} // should be obsolete
	// leftwards=true depthBalance means a Node was inserted in the left branch or taken from the right branch, so the balance will decrease without rotation or increase with rotation
	template <bool leftwards, bool insertion>
	void depthBalance(reference &nr, bool &depthChanged)
	{
		auto &n = nr.d;
		if (depthChanged)
		{
			leftwards ? n.balance-- : n.balance++;
			// nr.write();
			if (n.balance == (leftwards ? -2 : 2))
			{
				pointer &side = leftwards ? n.left : n.right;
				reference sr = *side;
				if (sr->balance == (leftwards ? 1 : -1))
					rotate<leftwards>(sr);
				rotate<!leftwards>(nr, side, sr);
				// thanks Wikipedia
				if (insertion || n.balance != 0)
					depthChanged = false; // nonzero balance for erase means graft preserved max depth
			}
			else if (insertion)
			{
				if (n.balance == 0)
					depthChanged = false; // thanks Wikipedia
			}
			// thanks Wikipedia
			else if (n.balance != 0)
				depthChanged = false; // this means the max depth is the same as before erase, so now the maximum depth is still the same
		}
	}

	template <bool leftwards>
	bool insert(reference &nr, Node &n, const Key &key, bool &depthIncreased)
	{
		// thanks to MDN and Creele and Python for ? :
		pointer &side = leftwards ? n.left : n.right;
		if (side)
		{
			if (insert(side, key, depthIncreased))
			{
				depthBalance<leftwards, true>(nr, depthIncreased);
				nr.write();
				return true;
			}
			return false;
		}
		else
		{
			side = al.allocate();
			*side = Node{key};
			leftwards ? n.balance-- : n.balance++;
			reset_memo();
			if (leftwards ? !n.right : !n.left)
				depthIncreased = true;
			nr.write();
			return true;
		}
	}
	bool insert(const pointer &np, const Key &key, bool &depthIncreased)
	{
		auto nr = *np;
		auto &n = nr.d;
		// cout<<"insert "<<n.key<<' '<<n.left.l<<' '<<n.right.l;
		//  thanks to those who taught or reminded me about binary search
		//  https://en.wikipedia.org/tree/Binary_search_tree
		if (compare(key, n.key))
			return insert<true>(nr, n, key, depthIncreased);
		else if (compare(n.key, key))
			return insert<false>(nr, n, key, depthIncreased);
		return false;
		// cout<<' '<<n.key<<' '<<n.left.l<<' '<<n.right.l<<endl;
	}
	template <bool leftwards>
	reference take_extreme_node(const pointer &np, bool &depthDecrease, bool &g)
	{
		auto nr = *np;
		auto &n = nr.d;
		auto &side = leftwards ? n.left : n.right;
		if (side)
		{
			auto m = take_extreme_node<leftwards>(side, depthDecrease, g);
			/* executes for immediate descendant only, if it's the min or max */
			if (g) // g for got it
			{
				/* the descendants of a min Node are all to its right, and of a max Node are all to the left */
				side = leftwards ? m->right : m->left;
				g = false;
			}
			depthBalance<!leftwards, false>(nr, depthDecrease); // taking it from the left means balance has increased to the right
			nr.write();
			return m;
		}
		else
		{
			depthDecrease = true;
			g = true;
			return nr;
		}
	}
	inline reference take_min(const pointer &np, bool &depthDecrease, bool &g) { return take_extreme_node<true>(np, depthDecrease, g); }
	inline reference take_max(const pointer &np, bool &depthDecrease, bool &g) { return take_extreme_node<false>(np, depthDecrease, g); }
	// replaces a Node with a Node of an adjacent value
	template <bool leftwards>
	void resume(pointer &side, bool &depthDecreased, Node &n, reference &nr)
	{
		bool g = false;
		auto m = take_extreme_node<!leftwards>(side, depthDecreased, g);
		if (g)
			side = leftwards ? side->left : side->right; // if the first right branch is the minimum it can only have a right branch
		n.key = m->key;
		al.deallocate(&m);
		eraseDepthBalance<leftwards>(nr, depthDecreased); // depthDecreased is also for ancestor if there is one
	}
	// erase Node at reference
	// pass pointer to nullify it if it has no branches
	void erase(pointer &np, reference &nr, bool &depthDecreased)
	{
		auto &n = nr.d;
		if (n.balance >= 0)
		{
			auto &right = n.right;
			if (right)
				resume<false>(right, depthDecreased, n, nr); // rotate left
			else
			{
				al.deallocate(np);
				np = pointer(nullptr);
				depthDecreased = true;
			} // no left because balance==0
		}
		else
			resume<true>(n.left, depthDecreased, n, nr); // if balance <0 and the tree is working properly there must be a left branch
		--this->n;
	}

	template <bool leftwards>
	void eraseDepthBalance(reference &nr, bool &depthDecreased)
	{
		depthBalance<!leftwards, false>(nr, depthDecreased);
		nr.write();
	}
	template <bool leftwards>
	bool erase(reference &nr, Node &n, const Key &f, bool &depthDecreased)
	{
		pointer &side = leftwards ? n.left : n.right;
		if (side)
		{
			bool erased = erase(side, f, depthDecreased);
			if (erased)
			{
				eraseDepthBalance<leftwards>(nr, depthDecreased);
				return true;
			}
		}
		// thanks to The Coding Train or William Lin or whoever taught me that else is not necessary because return returns from the function
		return false;
	}
	bool erase(pointer &np, const Key &f, bool &depthDecreased)
	{
		auto nr = *np;
		auto &n = nr.d;
		if (compare(f, n.key))
			return erase<true>(nr, n, f, depthDecreased); // check left branch to erase ( leftwards=true )
		else if (compare(n.key, f))
			return erase<false>(nr, n, f, depthDecreased); // check right branch to erase ( leftwards=false )
		else
		{
			erase(np, nr, depthDecreased);
			reset_memo();
			return true;
		}
	}
	template <bool leftwards>
	inline reference resume(const pointer &side, const pointer &other_side, bool &depthDecreased)
	{
		bool g = false;
		auto m = take_extreme_node<!leftwards>(side, depthDecreased, g);
		if (leftwards)
		{
			if (g)
				m->left = side;
			m->right = other_side;
		}
		else
		{
			if (g)
				m - right = side;
			m->left = other_side;
		}
		// thanks to Norman Megill and Metamath contributors
		if (g)
			eraseDepthBalance<leftwards>(m, depthDecreased); // m takes the place of n in the tree and might need to be balanced
		return m;
	}
	// left_ancestor means whether the Node is the ancestor's right branch
	template <bool leftwards, bool left_ancestor>
	inline void resume(const pointer &side, const pointer &other_side, bool &depthDecreased, reference &ancestor)
	{
		auto m = resume<leftwards>(side, other_side, depthDecreased);
		if (left_ancestor)
			ancestor->right = &m; // thanks to en.cppreference.com
		else
			ancestor->left = &m;
	}
	// why would the branch pointers matter if the branches aren't extracted from the tree with the Node? why extract the Node and not its value()?
	// it would extract the Node anyways with the reference so that it's written upon destruction
	// https://en.cppreference.com/w/cpp/container/set/extract
	// this doesn't return a PreservedNode because the argument nr can be used right afterwards to construct it
	template <bool left_ancestor>
	void extract(reference &ancestor, pointer &np, const reference &nr, bool &depthDecreased)
	{
		auto &n = nr.key;
		if (n.balance >= 0)
		{
			auto &right = n.right;
			if (right)
				resume<false, left_ancestor>(right, n.left, depthDecreased, ancestor); // rotate left
			else
			{
				np = pointer(nullptr);
				depthDecreased = true;
			} // no left because balance==0
		}
		else
			resume<true, left_ancestor>(n.left, n.right, depthDecreased, ancestor)
				// thanks to my CS teacher Shankar Kumar
				--(this->n); // this->n is for number of elements
	}
	template <bool leftwards>
	PreservedNode extract(reference &nr, Node &n, const Key &f, bool &depthDecreased)
	{
		pointer &side = leftwards ? n.left : n.right;
		if (side)
		{
			auto x = extract(nr, side, f, depthDecreased);
			if (bool(x))
				eraseDepthBalance<leftwards>(nr, depthDecreased);
			return x; // don't construct default PreservedNode several times if there wasn't an extraction
		}
		// thanks to https://en.cppreference.com/w/cpp/utility/optional for {} without writing PreservedNode next to it
		return {};
	}
	template <bool left_ancestor>
	PreservedNode extract(reference &ancestor, pointer &np, const Key &f, bool &depthDecreased)
	{
		auto nr = *np;
		auto &n = nr.key;
		if (compare(f, n.key))
			return extract<true>(nr, n, f, depthDecreased); // check left branch to extract ( leftwards=true )
		else if (compare(n.key, f))
			return extract<false>(nr, n, f, depthDecreased); // check right branch to extract ( leftwards=false )
		else
		{
			extract<left_ancestor>(ancestor, np, nr, depthDecreased);
			reset_memo();
			return nr;
		}
	}
	auto copy()
	{
		DiskTree<Key, Compare> c;
		copy(c.root, root);
		return c;
	}
	// assumes c hasn't been allocated
	void copy(pointer &c, const pointer &f) // I didn't want to overload = for Node
	{
		if (f)
		{
			c = al.allocate();
			auto cr = *c, fr = *f;
			cr.d = Node{fr->key, nullptr, nullptr, fr->balance};
			copy(cr->left, fr->left);
			copy(cr->right, fr->right);
			// assert(cr->well_ordered_unique()); // ease the debug log
			cr.write();
		}
	}
	inline void reset_memo()
	{
		begin_updated_memo = false;
		end_updated_memo = false;
	}
	// thanks to cppreference for showing enum definition in one line
	// https://en.cppreference.com/w/cpp/language/enum
	enum approach_r
	{
		less,
		greater,
		equal
	};
	approach_r approach(const Key &key, iterator &i)
	{
		while (true)
		{
			auto r = (reference)i;
			if (compare(key, r->key))
			{
				// left returns whether the Node had a left branch to move to, and moves the iterator there if it does
				if (!i.left(r))
					return less; // less than closest
			}
			else if (compare(r->key, key))
			{
				if (!i.right(r))
					return greater;
			}
			else
			{
				return equal;
			}
		}
	}
	// balance after taking away top of iterator's Node stack (nodes[level])
	// thanks to Creel and GCC and en.cppreference.com and whoever taught me about stack
	void popBalance(const iterator &i, bool depthDecreased)
	{
		for (char l = i.level - 1; depthDecreased && l >= 0; --l) // stop as early as possible to avoid disk reads
		{
			auto nr = reference(i.nodes[l]);
			if (i.right_levels[l])
				eraseDepthBalance<false>(nr, depthDecreased);
			else
				eraseDepthBalance<true>(nr, depthDecreased);
		}
		reset_memo();
	}
};

#endif /* DISK_TREE */
