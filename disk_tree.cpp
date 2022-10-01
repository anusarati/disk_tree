#include <bitset>
#include <initializer_list>
#include <utility>
#include "disk_allocator.cpp"
using namespace std;

//https://en.wikipedia.org/wiki/AVL_tree
// supposed to implement AVL tree
// https://en.cppreference.com/w/cpp/container/set
// also supposed to mimic part of std::set, implemented as red-black tree by GCC
// it doesn't attempt to insert data that isn't ordered before or after with compare
// licensed under 0 BSD
template<typename dt,typename Compare=std::less<dt>> struct disk_tree // https://en.cppreference.com/w/cpp/named_req/Compare
{
	// the allocations are for nodes
	struct node;
	// https://en.cppreference.com/w/cpp/container/set
	typedef disk_allocator<node> allocator_type;
	static inline allocator_type al{}; // the allocator is for allocating nodes
	static inline Compare compare{};

	static void init_allocator()
	{
		allocator_type::init();
	}
	static void init_allocator(auto&& dstream, auto&& sstream)
	{
		allocator_type::init(dstream,sstream);
	}
	using pointer=allocator_type::pointer;
	using reference=allocator_type::reference;
	void clear(pointer& p)
	{
#ifdef debug
		cout<<"clear "<<p.l<<endl;
#endif
		if (p)
		{
			auto r=*p;
			r->d.~dt();
			al.deallocate(p); // deallocation doesn't replace memory with default constructed node
			clear(r->left); clear(r->right);
		}
	}
	struct node
	{
		dt d;
		pointer left, right; // should be usable like node*, except for new and delete
		// not overloading new or delete to work with the disk allocator
		// the disk allocator doesn't work with std::set
		// what if there was one pointer to the branches and space for two nodes was allocated for each node's branch pointer?

		node& operator =(const dt& data)
		{
			d=data; return *this;
		}
		//https://en.cppreference.com/w/cpp/memory/new/operator_new#Class-specific_overloads
		signed char balance=0;
		inline operator dt() { return d; }
		inline dt value() { return d; }
		// root node is at depth 1
#ifndef NDEBUG
		unsigned char exhaustiveDepth() const // I don't think a larger ret type is necessary because a tree with depth 256 can have more than 1e76 elements
		{
			// inefficient, check all branches, linear in number of nodes
			// with a valid AVL tree, one could look at the balance and go through a single branch
			// choosing the side the balance falls on, or either if balance==0
			if (!(left||right)) return 1;
			if (!right) return left->exhaustiveDepth()+1;
			if (!left) return right->exhaustiveDepth()+1;
			return max(left->exhaustiveDepth(),right->exhaustiveDepth())+1;
		}
		bool correctBalance() const // this uses exhaustiveDepth because depth relies on balance
		{
			unsigned char a=0,b=0;
			if (left) a=left->exhaustiveDepth();
			if (right) b=right->exhaustiveDepth();
			//cout<<"a "<<(int)a<<" b "<<(int)b<<" balance "<<(int)balance<<endl;
			if (balance != (b-a)) return false;
			if (left && !left->correctBalance()) return false;
			if (right && !right->correctBalance()) return false;
			return true;
		}
#endif
		unsigned char depth() // logarithmic complexity
		{
			if (balance==-1) return left->depth()+1;
			else if (balance==1) return right->depth()+1;
			else
			{
				if (!(left||right)) return 1;
				if (!left) return right->depth()+1;
				return left->depth()+1;
			}
		}
#define mNode(name,side)\
		pair<node*,node*> name()\
		{\
			node* p,m=this;\
			while (m->side)\
			{\
				p=m; m=m->side;\
			}\
			return make_pair(m,p);\
		}
		mNode(minNode,left); mNode(maxNode,right);
#undef mNode
		/*
		pair<node*,node*> minNode()
		{
			node* p,m=this;
			while (m->left)
			{
				p=m; m=m->left;
			}
			return make_pair(m,p); // min node and its upper node
		}
		*/
#ifndef NDEBUG
	// thanks to Norman Megill and Metamath contributors
		bool wellOrderedUnique()
		{
			if (left && !left->wellOrderedLessThan(d)) return false;
			if (right && !right->wellOrderedGreaterThan(d)) return false;
			return true;
		}
		bool wellOrderedGreaterThan(const dt& lesser) // assuming lesser is from parent, then everything in its right branch should be greater
		{
			if (!compare(lesser,d)) return false;
			if (left && !left->wellOrderedGreaterThan(lesser)) return false;
			if (right && !right->wellOrderedGreaterThan(d)) return false;
			return true;
		}
		bool wellOrderedLessThan(const dt& greater)
		{
			if (!compare(d,greater)) return false;
			if (left && !left->wellOrderedLessThan(d)) return false;
			if (right && !right->wellOrderedLessThan(greater)) return false;
			return true;
		}
#endif
		// thanks to Wikipedia articles on graphs
		// used to implement contains
		// thanks to GCC and gdb for showing me that contains calls find or something like that
		// search for d in tree with binary search, if it can't be found it's not there
		bool reachable(const dt& r)
		{
			if (compare(r,d)) return left && left->reachable(r);
			if (compare(d,r)) return right && right->reachable(r);
			return true;
		}
	};
	void clear(node& n)
	{
		n.d.~dt(); clear(n.left); clear(n.right);
	}
#ifndef NDEBUG
	auto& dump(auto& n)
	{
		return cout<<' '<<n.d<<' '<<(int)n.balance<<' '<<' '<<n.left.l<<' '<<n.left->d<<' '<<n.right.l<<' '<<n.right->d;
	}
#endif
	//template<bool leftwards> rotate(const pointer& np) { auto nr=*np; rotate<leftwards>(nr); nr.write(); } // should be obsolete
	template<bool leftwards> void rotate(reference& nr)
	{
		auto& n=nr.d;
		// thanks to https://en.wikipedia.org/wiki/Lambda_calculus
		pointer& other_side=leftwards ? n.right : n.left;
		reference r=*other_side;
		rotate<leftwards>(nr,other_side,r);
	}
	template<bool leftwards> void rotate(reference& nr,pointer& other_side, reference& osr)
	{
		auto& n=nr.d;
#ifdef debug
		cout<<"rotate "<<leftwards<<' '<<n.d<<' '<<n.left.l<<' '<<n.right.l<<endl;
#endif
		if (osr->balance== (leftwards ? -1 : 1)) rotate<!leftwards>(osr);
		auto new_side=al.allocate();
		// thanks Wikipedia for explaining balance as difference between max depths
		signed char branchBalance;
		if (osr->balance== (leftwards ? 1 : -1)) branchBalance=n.balance + (leftwards ? -2 : 2);
		else branchBalance=n.balance + (leftwards ? -1 : 1);
		auto graft=leftwards ? osr->left : osr->right;
		pointer& side=leftwards ? n.left : n.right;
		if (leftwards) *new_side=node{move(n.d),n.left,graft,branchBalance};
		else *new_side=node{move(n.d),graft,n.right,branchBalance};
		side=new_side;
		n.d=move(osr->d); auto p=other_side; other_side = leftwards ? osr->right : osr->left;
		al.deallocate(p);
		if (leftwards)
		{
			if (branchBalance==1) n.balance=-1;// the graft increased the left branch's depth
			else n.balance-=2;
		}
		else
		{
			if (branchBalance==-1) n.balance=1;// the graft increased the right branch's depth
			else n.balance+=2;
		}
#ifdef debug
		cout<<"result "<<n.d<<' '<<n.left.l<<' '<<n.right.l<<endl;
#endif
	}
	/*
	// assumes right branch exists
	inline void rotateLeft(const pointer& np) { auto nr=*np; rotateLeft(nr); nr.write(); }
	void rotateLeft(reference& nr)
	{
		auto& n=nr.d;
		auto rr=*n.right;
		if (rr->balance==-1) rotateRight(rr);
	//	cout<<"rotateLeft ";dump(n);
		auto newleft=al.allocate();
		// thanks Wikipedia for explaining balance as difference between max depth
		signed char branchBalance;
		// if balance==2 graft is as deep as left branch because the right branch of the right branch is the deeper one
		if (rr->balance==1) branchBalance=n.balance-2;  // if balance==1 the graft is one less deep than the left branch
		else branchBalance=n.balance-1;
		auto graft=rr->left;
		*newleft=node{move(n.d),n.left,graft,branchBalance};
		n.left=newleft;
		//dump(n);
		n.d=move(rr->d); auto p=n.right; n.right=rr->right;
		al.deallocate(p);
		if (branchBalance==1) n.balance=-1;// the graft increased the left branch's depth
		else n.balance-=2;
		//dump(n)<<endl;
		//nr.write();
	}
	*/
	template<bool leftwards, bool insertion> inline void depthBalance(const pointer& np,bool& depthChanged) { auto nr=*np; return depthBalance<leftwards,insertion>(nr,depthChanged); } // should be obsolete
	// leftwards=true depthBalance means a node was inserted in the left branch or taken from the right branch, so the balance will decrease without rotation or increase with rotation
	template<bool leftwards, bool insertion> void depthBalance(reference& nr,bool& depthChanged)
	{
		auto& n=nr.d;
		if (depthChanged)
		{
			leftwards ? n.balance-- : n.balance++;
			//nr.write();
			if (n.balance== (leftwards ? -2 : 2))
			{
				pointer& side=leftwards ? n.left : n.right;
				reference sr=*side;
				if (sr->balance==(leftwards ? 1 : -1)) rotate<leftwards>(sr);
				rotate<!leftwards>(nr,side,sr);
				// thanks Wikipedia
				if (insertion || n.balance!=0) depthChanged=false; // nonzero balance for erase means graft preserved max depth
			}
			else if (insertion)
			{
				if (n.balance==0) depthChanged=false; // thanks Wikipedia
			}
			// thanks Wikipedia
			else if (n.balance!=0) depthChanged=false; // this means the max depth is the same as before erase, so now the maximum depth is still the same
		}
	}

	/*
	template<bool insertion> inline void rightDepthBalance(const pointer& np,bool& depthChanged) { auto nr=*np; return rightDepthBalance<insertion>(nr,depthChanged); } // should be obsolete

	template<bool insertion> void rightDepthBalance(reference& nr,bool& depthChanged)
	{
		auto& n=nr.d;
		//cout<<"rightdb ";dump(n)<<" depthChanged "<<depthChanged<<endl;
		if (depthChanged)
		{
			n.balance++;
			//nr.write();
			if (n.balance==2)
			{
				if (n.right->balance==-1) rotate<false>(n.right);
				rotate<true>(nr);
				// thanks Wikipedia
				if (insertion || n.balance!=0) depthChanged=false;
			}
			else if (insertion)
			{
				if (n.balance==0) depthChanged=false; // thanks Wikipedia
			}
			// thanks Wikipedia
			else if (n.balance==1) depthChanged=false; // this means the branches had the same depth before, so now the maximum depth is still the same
		}
	}
	*/

	template<bool leftwards> bool insert(reference& nr, node& n, const dt& d, bool& depthIncreased)
	{
		// thanks to MDN and Creele and Python for ? :
		pointer& side = leftwards ? n.left : n.right;
		if (side)
		{
			if (insert(side,d,depthIncreased))
			{
				depthBalance<leftwards,true>(nr,depthIncreased);
				nr.write();
				return true;
			}
			return false;
		}
		else
		{
			side=al.allocate(); *side=node{d};
			leftwards ? n.balance-- : n.balance++;
			updated_memo=false;
			if (leftwards ? !n.right : !n.left) depthIncreased=true;
			nr.write();
			return true;
		}
	}
	bool insert(const pointer& np, const dt& d, bool& depthIncreased)
	{
		auto nr=*np; auto& n=nr.d;
		//cout<<"insert "<<n.d<<' '<<n.left.l<<' '<<n.right.l;
		// thanks to those who taught or reminded me about binary search
		// https://en.wikipedia.org/tree/Binary_search_tree
		if (compare(d,n.d)) return insert<true>(nr,n,d,depthIncreased);
		else if (compare(n.d,d)) return insert<false>(nr,n,d,depthIncreased);
		return false;
		//cout<<' '<<n.d<<' '<<n.left.l<<' '<<n.right.l<<endl;
	}
	// g for got it
#define takeM(M, s1, s2, dbl)\
	reference M(const pointer& np,bool& depthDecrease, bool& g)\
	{\
		auto nr=*np; auto& n=nr.d;\
		if (n.s1)\
		{\
			auto m=M(n.s1,depthDecrease,g);\
			/* executes for immediate descendant only, if it's the min or max */\
			if (g)\
			{\
				/* the descendants of a min node are all to its right, and of a max node are all to the left */\
				n.s1=m->s2;\
				g=false;\
			}\
			depthBalance<dbl,false>(nr,depthDecrease);\
			nr.write();\
			return m;\
		}\
		else\
		{\
			depthDecrease=true;\
			g=true;\
			return nr;\
		}\
	}
	takeM(takeMin,left,right,false);
	takeM(takeMax,right,left,true);
#undef takeM
	// erase node at reference
	// pass pointer to nullify it if it has no branches
	void erase(pointer& np, reference& nr, bool& depthDecreased)
	{
		auto& n=nr.d;
		if (n.balance>=0)
		{
			bool g=false;
			auto& right=n.right;
			if (right) // rotate left
			{
				auto m=takeMin(right,depthDecreased,g);
				if (g) right=right->right; // if the first right branch is the minimum it can only have a right branch
				n.d=m->d;
				al.deallocate(&m);
				eraseDepthBalance<false>(nr,depthDecreased);// depthDecreased is for parent
			}
			else { al.deallocate(np); np=pointer(nullptr); depthDecreased=true; } // no left because balance==0
		}
		else
		{
			bool g=false;
			auto& left=n.left;
			auto m=takeMax(left,depthDecreased,g);
			if (g) left=left->left;
			n.d=m->d;
			al.deallocate(&m);
			eraseDepthBalance<true>(nr,depthDecreased);
		}
	}
	// https://en.cppreference.com/w/cpp/container/node_handle
	// https://eel.is/c++draft/container.node
	struct preserved_node
	{
		reference r;
		constexpr preserved_node() noexcept;
		preserved_node(preserved_node&& n) noexcept { r=move(n.r); n.r.l=SIZE_MAX; }
		preserved_node& operator =(preserved_node&& n) { if (&r) al.deallocate(&r); r=move(n.r); n.r.l=SIZE_MAX; }
		~preserved_node() { if (&r) al.deallocate(&r); r.l=SIZE_MAX; }
		[[nodiscard]] bool empty() const noexcept { return !&r; }
		explicit operator bool() const noexcept { return bool(&r); }
		dt& value() const { return r->d; }
		// thanks to those who taught/reminded me about using a temporary variable for swap
		void swap(preserved_node& n) noexcept { auto intermediate=move(n); n=move(*this); *this=move(intermediate); }
		preserved_node(reference&& rr) : r(rr) {}
	};
	typedef preserved_node node_type; // this isn't used except for extract
	// why would the branch pointers matter if the branches aren't extracted from the tree with the node? why extract the node and not its value()?
	// https://en.cppreference.com/w/cpp/container/set/extract
	// this uses right instead of left because iterator stores rightLevels
	// this doesn't return a preserved_node because the argument nr can be used right afterwards to construct it
	void extract(const pointer& parent, const bool& right, pointer& np, const reference& nr, bool& depthDecreased) 
	{
		auto& n=nr.d;
		if (n.balance>=0)
		{
			bool g=false;
			auto& right=n.right;
			if (right) // rotate left
			{
				auto m=takeMin(right,depthDecreased,g);
				m->left=n.left;
				if (!g) m->maxNode().first->right=right;
				if (parent)
				{
					auto pr=*parent;
					if (right) pr->right=&m;// thanks to en.cppreference.com
					else pr->left=&m;
					eraseDepthBalance<false>(nr,depthDecreased);// depthDecreased is for parent
				}
				else root=&m;
			}
			else { np=pointer(nullptr); depthDecreased=true; } // no left because balance==0
		}
		else
		{
			bool g=false;
			auto& left=n.left;
			auto m=takeMax(left,depthDecreased,g);
			m->right=n.right;
			if (!g) m->minNode().first->left=n.left;
			if (parent)
			{
				auto pr=*parent;
				if (right) pr->right=&m;
				else pr->left=&m; // thanks to en.cppreference.com
				eraseDepthBalance<true>(nr,depthDecreased);// depthDecreased is for parent
			}
			else root=&m;
		}
	}
	template<bool leftwards> void eraseDepthBalance(reference& nr, bool& depthDecreased)
	{
		depthBalance<!leftwards,false>(nr,depthDecreased);
		nr.write();
	}
	template<bool leftwards> bool erase(reference& nr, node& n, const dt& f, bool& depthDecreased)
	{
		pointer& side = leftwards ? n.left : n.right;
		if (side)
		{
			bool erased=erase(side,f,depthDecreased);
			if (erased)
			{
				eraseDepthBalance<leftwards>(nr,depthDecreased);
				return true;
			}
		}
		// thanks to The Coding Train or William Lin or whoever taught me that else is not necessary because return returns from the function
		return false;
	}
	
	/*
	void eraseRight(reference& nr, dt& f, bool& depthDecreased, bool& erased)
	{
		auto& n=nr.d;
		auto& right=n.right;
		if (right)
		{
			erase(right,f,depthDecreased,erased);
			if (erased)
			{
				//if (!right) nr.write();
				leftDepthBalance(nr,depthDecreased,false);
				nr.write();
			}
		}
	}
	*/

	bool erase(pointer& np, const dt& f, bool& depthDecreased)
	{
		auto nr=*np; auto& n=nr.d;
		if (compare(f,n.d)) return erase<true>(nr,n,f,depthDecreased); // check left branch to erase ( leftwards=true )
		else if (compare(n.d,f)) return erase<false>(nr,n,f,depthDecreased); // check right branch to erase ( leftwards=false )
		else
		{
			erase(np,nr,depthDecreased); updated_memo=false;
			return true;
		}
	}

	pointer root;
	size_t n=0;
	disk_tree() {}
	disk_tree(initializer_list<dt> l)
	{
		for (auto& d:l) insert(d);
	}
	template<typename iterator_t> disk_tree(iterator_t i, const iterator_t& e)
	{
		while (i!=e)
		{
			insert(*i);
			++i;
		}
	}
	disk_tree(disk_tree<dt,Compare>&& t) { root=move(t.root); }
	disk_tree(const disk_tree<dt,Compare>& t) { copy(root,t.root); }
	disk_tree<dt,Compare>& operator=(const disk_tree<dt,Compare>& t)
	{
#ifdef debug
		cout<<root.l<<" = "<<t.root.l<<endl;
#endif
		clear(); copy(root,t.root);
		return *this;
	}
	disk_tree<dt,Compare>& operator=(disk_tree<dt,Compare>&& t)
	{
#ifdef debug
		cout<<root.l<<" = "<<t.root.l<<endl;
#endif
		clear();
		root=move(t.root);
		// thanks to https://en.cppreference.com/w/cpp/container/node_handle
		t.root=nullptr;
		return *this;
	}
	void clear() { clear(root); }
	~disk_tree() { clear(); }
	auto copy()
	{
#ifdef debug
		cout<<"copy "<<root.l<<endl;
#endif
		disk_tree<dt,Compare> c;
		copy(c.root,root);
		return c;
	}
	// assumes c hasn't been allocated
	void copy(pointer& c, const pointer& f) // I didn't want to overload = for node
	{
		if (f)
		{
			c=al.allocate();
			auto cr=*c, fr=*f;
			cr.d=node{fr->d,nullptr,nullptr,fr->balance};
			copy(cr->left,fr->left); copy(cr->right,fr->right);
			//assert(cr->wellOrderedUnique()); // ease the debug log
			cr.write();
		}
	}
	size_t size() { return n; }
	// mimic std::set::insert https://en.cppreference.com/w/cpp/container/set/insert
	pair<auto,bool> insert(const dt& d)
	{
#ifdef debug
		cout<<"insert "<<d<<endl;
#endif
		bool b=false,inserted;
		iterator i;
		//cout<<"insert "<<d;
		if (root)
		{
			//cout<<' '<<root.l<<' '<<root->d<<' '<<root->left<<' '<<root->right<<'\n';
			if (inserted=insert(root,d,b)) n++;
			i=find(d); // easy solution
		}
		else
		{
			root=al.allocate();*root=node{d};
			n++; updated_memo=false; inserted=true;
			i={root};
		}
		// thanks to Errichto for using make_pair and William Lin
		return make_pair(i,inserted);
	}
	bool erase(const dt& d)
	{
#ifdef debug
		cout<<"erase "<<d<<endl;
#endif
		bool b=false;
		bool erased=erase(root,d,b);
		if (erased) n--;
		return erased;
	}
	struct iterator // consumes a lot of space, not good for parallel iteration on small trees
	{
		iterator()=default;
		iterator(const pointer& p) { nodes[0]=p; }
		pointer nodes[8*sizeof(size_t)];// parents then current child up to level
		unsigned char level=0;
		// if there are 8*sizeof(size_t) levels, a full AVL tree would have SIZE_MAX elements
		std::bitset<8*sizeof(size_t)> rightLevels; // used like a stack, with the 1 bits representing which nodes had their right child node reached
		// rightLevels should take same space as a size_t
#define traverseM(M,s,r)\
		pointer M()\
		{\
			pointer p=nodes[level];\
			while (p->s)\
			{\
				p=p->s;\
				auto i=level++;\
				nodes[level]=p;\
				rightLevels[i]=r;\
			}\
			return p;\
		}
		traverseM(traverseMin,left,false); traverseM(traverseMax,right,true);
#undef traverseM
#define step(sym,c1,c2,traverse,side)\
		iterator operator sym()\
		{\
			auto bl=level;\
			if (middleLevels[level]==c2)\
			{\
				auto p=nodes[level];\
				auto c=p->side;\
				if (c)\
				{\
					auto i=level++;\
					nodes[level]=c;\
					traverse();\
					rightLevels[i]=c2;\
				}\
				else\
				{\
					bool belroot=level>0;\
					if (belroot && rightLevels[level-1]==c1) level--;\
					while (rightLevels[level]==c2)\
					{\
						if (level>0)\
						{\
							level--;\
						}\
						else { level=bl; return *this; }\
					}\
					auto reset=(bitset<8*sizeof(size_t)>)SIZE_MAX >> (8*sizeof(size_t)-1-level);\
					rightLevels&=reset;\
				}\
			}\
			else middleLevels[level]=c2;\
			return *this;\
		}\
		iterator operator sym(int) { auto before=*this; operator sym(); return before; }
		//https://en.cppreference.com/w/cpp/language/operators
		//step(++,false,true,traverseMin,right);
		//step(--,true,false,traverseMax,left);
#undef step
		// template<bool increment> iterator& step()
		iterator& operator ++()
		{
			if (level<8*sizeof(size_t))
			{
				auto& p=nodes[level];
				if (p) // allows ++--begin() to be iterator at first element
				{
					pointer c=p->right;
					if (c)
					{
						auto i=level++;
						nodes[level]=c;
						traverseMin();
						rightLevels[i]=true;
					}
					else
					{
						auto bl=level;
						while (level>0 && rightLevels[level-1]) --level;
						if (level>0)// right parent
							level--;
						else { level=bl+1; rightLevels[bl]=true; if (level<64) nodes[level]=nullptr; return *this; }
						auto reset=(bitset<8*sizeof(size_t)>)SIZE_MAX >> (8*sizeof(size_t)-1-level); // it could fail depending on endianness without the conversion to bitset
						rightLevels&=reset;
					}
				}
				else if (level>0 && !rightLevels[level-1])
				{
#ifdef debug
					cout<<"level "<<(unsigned)level<<endl;
					cout<<"rightLevels "<<rightLevels<<endl;
					cout<<rightLevels[level-1]<<endl;
#endif
					--level;
				}
			}
			return *this;
		}
		dt peek() // get next element without advancing iterator (like std::basic_istream::peek() https://en.cppreference.com/w/cpp/io/basic_istream/peek)
		{
			auto& p=nodes[level];
			if (p)
			{
				pointer c=p->right;
				auto l=level;
				if (c)
				{
					auto mu=nodes[l+1]->minNode();
					return mu.first->d;
				}
				else
				{
					while (l>0 && rightLevels[l-1]) --l;
					if (l>0)// right parent
						l--;
					else return dt{};
					return nodes[l]->d;
				}
			}
			return dt{};
		}
		// thanks to GCC for showing me that postfix ++ has int parameter
		// https://en.cppreference.com/w/cpp/language/operators
		iterator operator ++(int) { auto before=*this; operator ++(); return before; }
		iterator& operator --()
		{
			if (level<8*sizeof(size_t))
			{
				auto& p=nodes[level];
				if (p) // allows --end() to be iterator at last element
				{
					pointer c=p->left;
					if (c)
					{
						auto i=level++;
						nodes[level]=c;
						traverseMax();
						rightLevels[i]=false;
					}
					else
					{
						auto bl=level;
						while ( level>0 && !rightLevels[level-1] ) --level;
						if (level>0)
							level--;
						else { level=bl+1; if (level<64) nodes[level]=nullptr; return *this; } // mimic standard (as implemented by GCC)
						rightLevels[level]=false;
						auto reset=(bitset<8*sizeof(size_t)>)SIZE_MAX >> (8*sizeof(size_t)-1-level); // it could fail depending on endianness without the conversion to bitset
						rightLevels&=reset;
					}
				}
				else if (level>0 && rightLevels[level-1]) rightLevels[--level]=false;
			}
			return *this;
		}
		iterator operator --(int) { auto before=*this; operator --(); return before; }

		bool operator ==(const iterator& i) // logarithmic complexity
		{
			if (level!=i.level) return false;
			for (char l=level;l>=0;l--) if (nodes[l]!=i.nodes[l]) return false; // checking before nodes[level] and i.nodes[level] isn't necessary if it's known the iterators are from the same tree, and then the complexity can be reduced
			return true;
		}
		dt operator *() // mimic std container iterators, (use auto&& or dt for range for iteration)
		{
			return nodes[level]->d;
		}
		template<bool rightwards> bool branch(const reference& r)
		{
			auto& side=rightwards ? r.d.right : r.d.left;
			if (side)
			{
				rightLevels[level]=rightwards;
				nodes[++level]=side;
				return true;
			}
			return false;
		}
		inline bool left(const reference& r) { return branch<false>(r); }
		inline bool right(const reference& r) { return branch<true>(r); }
		inline bool left() { return branch<false>(operator reference()); }
		inline bool right() { return branch<true>(operator reference()); }
		operator pointer() { return nodes[level]; }
		// https://en.cppreference.com/w/cpp/language/explicit
		explicit operator pointer&() { return nodes[level]; }
		//https://en.cppreference.com/w/cpp/language/operators
		pointer operator ->() { return nodes[level]; }
		operator reference() const { return *nodes[level]; }
	};
	// logarithmic time begin and end instead of constant https://en.cppreference.com/w/cpp/container/set/begin
	// I thought it would be better than updating begin and end with each insertion or erasure
	// because I think the checks would happen a logarithmic number of times anyways
	// optional caching to have sometimes constant time
	// actually copying the iterators could take logarithmic time anyways in this implementation
	protected: iterator begin_memo, end_memo;
	bool updated_memo:1=false;
	public:
	iterator begin()
	{
		if (updated_memo) return begin_memo;
		iterator i{root};
		i.traverseMin();
		begin_memo=move(i); updated_memo=true;
		return i;
	}
	iterator end()
	{
		if (updated_memo) return end_memo;
		iterator i{root};
		i.traverseMax();
		i.rightLevels[i.level++]=true;
		end_memo=move(i); updated_memo=true;
		return end_memo;
	}
	// thanks to cppreference for showing enum definition in one line
	// https://en.cppreference.com/w/cpp/language/enum
	enum approach_r { less, greater, equal };
	approach_r approach(const dt& d,iterator& i)
	{
		while (true)
		{
			auto r=(reference)i;
			if (compare(d,r->d))
			{
				// left returns whether the node had a left branch to move to, and moves the iterator there if it does
				if (!i.left(r)) return less; // less than closest
			}
			else if (compare(r->d,d))
			{
				if (!i.right(r)) return greater;
			}
			else
			{
				return equal;
			}
		}
	}
	iterator find(const dt& d)
	{
		if (root)
		{
			iterator i{root};
			switch (approach(d,i))
			{
				case less: [[fallthrough]];
				case greater:
					return end();
				case equal:
					return i;
			}
		} return end(); // I realized the case when find is called on an empty tree for some reason
	}
	// https://en.cppreference.com/w/cpp/container/set/lower_bound
	iterator lower_bound(const dt& d)
	{
		if (root)
		{
			iterator i{root};
			switch (approach(d,i))
			{
				case less: [[fallthrough]];
				case equal:
					return i;
				case greater:
					return end();
			}
		} return end();
	}
	// https://en.cppreference.com/w/cpp/container/set/upper_bound
	iterator upper_bound(const dt& d)
	{
		if (root)
		{
			iterator i{root};
			switch (approach(d,i))
			{
				case less:
					return i;
				case greater:
					return end();
				case equal:
					return ++i;
			}
		} return end();
	}
	// https://en.cppreference.com/w/cpp/container/set/contains
	// thanks to GCC and gdb for showing me that contains calls find or something like that
	// search for d in tree with binary search, if it can't be found it's not there
	bool contains(const dt& d)
	{
		return root && root->reachable(d);
	}
	void popBalance(const iterator& i, bool depthDecreased)
	{
		for (char l=i.level-1;depthDecreased && i>=0;i++) // stop as early as possible to avoid disk reads
		{
			auto nr=reference(i.nodes[l]);
			if (i.rightLevels[l]) eraseDepthBalance<false>(nr,depthDecreased);
			else eraseDepthBalance<true>(nr,depthDecreased);
		}
		updated_memo=false;
	}
	iterator erase(const iterator& i) // the iterator's nodes data should contain the same pointers even if the pointed data changes
	{
		bool n, depthDecreased;
		dt f;
		auto e=end();
		if (i==e) return e;
		if (i==--e) n=true;
		else f=i.peek();
		auto& np=(pointer&)(i); auto r=*np;
		erase(np,r,depthDecreased);
		if (i.level>0) popBalance(i,depthDecreased);
		if (n) return end();
		return find(f); // logarithmic complexity
	}
	preserved_node extract(const iterator& i)
	{
		auto& np=(pointer&)(i); auto r=*np; node& n=r.d;
		bool depthDecreased;
		if (i.level>0)
		{
			auto u=i.level-1;
			extract(i.nodes[u],i.rightLevels[u],np,r,depthDecreased);
			popBalance(depthDecreased);
		}
		else extract(nullptr,false,np,r,depthDecreased);
		return preserved_node{move(r)};
	}
	struct reverse_iterator
	{
		iterator forward;
		// https://en.cppreference.com/w/cpp/iterator/make_reverse_iterator
		reverse_iterator(iterator&& i) { forward=i; --forward; }
		reverse_iterator(iterator& i) { forward=move(i); --forward; }
		reverse_iterator& operator ++() { --forward; return *this; }
		reverse_iterator& operator --() { ++forward; return *this; }
		reverse_iterator& operator ++(int) { auto before=*this; --forward; return before; }
		reverse_iterator& operator --(int) { auto before=*this; ++forward; return before; }
		operator pointer() { return pointer(forward); }
		// https://en.cppreference.com/w/cpp/language/explicit
		explicit operator pointer&() { return (pointer&)(forward); }
		//https://en.cppreference.com/w/cpp/language/operators
		pointer operator ->() { return forward; }
		operator reference() const { return reference(forward); }
		bool operator ==(const reverse_iterator& i) { return forward==i.forward; }
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
	dt front()
	{
		return *root->minNode()->first;
	}
	void pop_front()
	{
		if (root)
		{
			auto p=root->minNode()->second;
			if (p) clear(p.left);
		}
	}
	dt front_and_pop() // like JS Array.prototype.pop()
	{
		auto ns=root->minNode();
		auto d=ns->first->d,p=ns->second;
		if (p) clear(p.left);
		return d;
	}
	unsigned char depth()
	{
		return root->depth();
	}
#ifndef NDEBUG
	unsigned char exhaustiveDepth()
	{
		return root->exhaustiveDepth();
	}
	bool depthMinimal()
	{
		// 1<<x = 2 to the power of x
		return ((size_t)1)<<(exhaustiveDepth()-1)<n; // the previous depth would not be able to contain so many elements if <n
	}
	bool correctDepth()
	{
		return root->depth()==root->exhaustiveDepth();
	}
	// thanks to Norman Megill and Metamath contributors
	// slow iterator implementation of wellOrdered
	bool wellOrdered() // all nodes in the left branch should be <=, all nodes in the right branch should be >= for binary search algorithm to consistently work ( duplicate nodes make it difficult though, and if there is a duplicate that's the branch of the other duplicate it can only have one branch which would complicate balancing, a simple way to implement multiset is to store a count with an element, which I thought of when considering the subset sum problem https://en.wikipedia.org/wiki/Subset_sum_problem )
	{
		auto i=begin(), e=end();
		auto p=*i;
		while (++i!=e)
		{
			if (p>*i) return false;
			p=*i;
		}
		return true;
	}
	bool wellOrderedUnique() { return root->wellOrderedUnique(); }
#endif
};
