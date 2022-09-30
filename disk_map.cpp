#include "disk_tree.cpp"
// attempt to mimic some of std::map
template<typename dt, typename rt, typename ArgCompare=std::less<dt>> struct disk_map
{
	// thanks to GCC and en.cppreference.com for reminding me that a normal std::map is not a std::multimap and doesn't need multiple rt for one dt and that I don't need to implement multimap by wrapping the second member with another disk_tree
	using pt=pair<dt,rt>; 
	struct Compare
	{
		static constexpr ArgCompare c{};
		bool operator ()(const pt& a, const pt& b) { return c(a.first,b.first); }
	};
	using tree_t=disk_tree<pt,Compare>;
	struct iterator;
	struct mapped_t // use auto instead of rt when replacing std::map
	{
		tree_t::reference r;
		mapped_t(const rt& m)=delete;
		//https://stackoverflow.com/a/19819249
		mapped_t& operator=(const mapped_t& m)
		{
			return operator=(m.r.d);
		}
		mapped_t(const tree_t::iterator& i) { r=typename tree_t::reference(i); }
		mapped_t(const iterator& i) { r=typename tree_t::reference(i); } // the compiler didn't understand
		mapped_t& operator =(const rt& right)
		{
			r->d.second=right; // fortunately the iterator isn't const qualified ( if you want const qualification for a disk_set you can just use a const parameter for disk_tree )
			r.write();
			return *this;
		}
		//https://huixie90.github.io/Almost-always-const-auto-ref
		mapped_t& operator =(const auto& something) { return operator=((rt)something); }
		operator rt() { return r->d.second; }
	};
	inline static pt incomplete_pair(auto&& first)
	{
		pt p; p.first=first; return p;
	}
	static void init_allocator()
	{
		tree_t::init_allocator();
	}
	static void init_allocator(auto&& dstream, auto&& sstream)
	{
		tree_t::init_allocator(dstream,sstream);
	}
	inline void minit_allocator() { init_allocator(); } // as member function
	inline void minit_allocator(auto&& dstream, auto&& sstream) { init_allocator(dstream,sstream); } // as member function
	inline auto inner_insert(const dt& d) { return tree.insert(incomplete_pair(d)); }
	inline auto inner_find(const dt& d) { return tree.find(incomplete_pair(d)); }
	tree_t tree;
	size_t size() { return tree.size(); }
	mapped_t at(const dt& arg)
	{
		if (tree.root) return inner_find(arg);
		return tree.end();
	}
	mapped_t operator [](const dt& arg)
	{
		if (tree.root)
		{
			auto f=inner_find(arg);
			iterator i;
			if (f==tree.end()) i=inner_insert(arg).first;
			else i=move(f);
			return i;
		} else return inner_insert(arg).first;
	}
	pair<auto,bool> insert(const dt& d)
	{
		auto i=inner_insert(d);
		return make_pair(iterator{i.first},i.second);
	}
	inline bool erase(const dt& d) { return tree.erase(d); }
	using cpt=pair<const dt,rt>;
	struct iterator
	{
		tree_t::iterator inner;
		iterator()=default;
		iterator(tree_t::iterator&& i) { inner=i; }
		iterator(tree_t::iterator& i) { inner=move(i); }
		cpt operator *()
		{
			return *inner;
		}
		auto operator ->() { return inner; } // for ->first or ->second
		iterator& operator ++() { ++inner; return *this; }
		iterator& operator --() { --inner; return *this; }
		iterator operator ++(int) { auto before=*this; ++inner; return before; } // I haven't checked to see if it actually copies the iterator's nodes data
		iterator operator --(int) { auto before=*this; --inner; return before; }
		bool operator ==(const iterator& o) { return inner==o.inner; }
		operator tree_t::iterator() { return inner; }
		operator tree_t::pointer() { return typename tree_t::pointer(inner); }
		explicit operator tree_t::pointer&() { return (typename tree_t::pointer&)(inner); }
		operator tree_t::reference() const { return typename tree_t::reference(inner); }
	};
	iterator begin() { return tree.begin(); }
	iterator end() { return tree.end(); }
	iterator find(const dt& arg)
	{
		return inner_find(arg);
	}
	iterator erase(const iterator& i)
	{
		return tree.erase(incomplete_pair(i.inner));
	}
	void clear() { tree.clear(); }
};
