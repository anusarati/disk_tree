#include <cstdio>
#include <iostream>
#include <cinttypes>
#include <fstream>
using namespace std;
// allocates one t at a time
template<typename t> struct disk_allocator
{
	// https://en.cppreference.com/w/cpp/io/c/FILE
	// thanks to The Cherno or whoever else taught me C++
	inline static FILE *dstream=nullptr, *sstream=nullptr; // for data and memory spaces
	inline static void seek(FILE* s,const size_t& l) { fseek(s,l,SEEK_SET); }
	inline static void seek(const size_t& l) { fseek(dstream,l,SEEK_SET); }
	//https://en.cppreference.com/w/cpp/io/basic_ostream
	//https://huixie90.github.io/Almost-always-const-auto-ref
	//https://en.cppreference.com/w/cpp/io/basic_fstream
	//https://en.cppreference.com/w/cpp/io/c/fread
	inline static size_t read(auto& x,FILE* s)
	{
		return fread((void*)&x,sizeof(x),1,s);
	}
	// https://en.cppreference.com/w/cpp/io/c/fwrite
	inline static size_t write(auto& x,FILE* s)
	{
		return fwrite((void*)&x,sizeof(x),1,s);
	}
	struct pointer;
	struct reference
	{
		t d; size_t l;
		inline void read() { seek(l); disk_allocator<t>::read(d,dstream); }
		//https://stackoverflow.com/a/31211386
		reference() { l=SIZE_MAX; }
		reference(const size_t& p) : l(p) { read(); }
		reference(const reference& r) : d(r.d), l(r.l) {}
		void write() const
		{
			//cout<<"write "<<d.d<<' '<<l<<' '<<ftell(dstream);
			seek(l);
			disk_allocator<t>::write(d,dstream);
		}
		// = for another reference should go through constructor instead of this
		reference& operator =(const auto& o)
		{
			d=o; write();
			return *this;
		}
		//reference& operator=(initializer_list<auto> l) { d=move(l); write(); return *this; }
		operator t&() const
		{
			return d;
		}
		// let ->() call &d->()
		t* operator->() { return &d; }
		pointer operator &() const
		{
			return pointer{l};
		}
		//auto operator <=>(const reference& o) const { return d<=>o.d; }
		//~reference()=default;
		//~reference() { cout<<"destroy "<<d.d<<endl<<l<<endl; write(); }
	};
	//https://stackoverflow.com/a/16998837
	struct pointer
	{
		// https://en.cppreference.com/w/cpp/memory/pointer_traits
		// https://en.cppreference.com/w/cpp/language/typedef
		size_t l;
		pointer() : l(SIZE_MAX) {}
		pointer(const size_t& location) : l(location) {}
		pointer(nullptr_t) : l(SIZE_MAX) {}
		reference operator*() const
		{
			reference d{l};
			return d;
		}
		reference operator[](size_t i) const
		{
			auto p=l+i;
			reference d{p};
			return d;
		}
		bool operator ==(const pointer& p) const { return l==p.l; }
		// SIZE_MAX for nullptr equivalent
		bool operator!() const { return l==SIZE_MAX; }
		operator bool() const { return l!=SIZE_MAX; }
		bool operator ==(nullptr_t) const { return operator!(); }
		bool operator ==(const t* p) const
		{
			if (p) return false; // for pointer to RAM location and not disk
			return operator!(); // they can be equal if they are nullptrs
		}
		auto operator <=>(const pointer& p) const { return l<=>p.l; }
		reference operator->() const
		{
			reference d{l};
			return d;
		}
		ptrdiff_t operator -(const pointer& p) { return l-p.l; }
		auto operator ++() { ++l; return *this; }
		auto operator --() { --l; return *this; }
		auto operator +=(integral auto i) { l+=i; return *this; }
	};
	//https://en.cppreference.com/w/cpp/io/basic_fstream/basic_fstream
	static void init(FILE* ds, FILE* ss)
	{
		dstream=ds; sstream=ss;
	}
	static void init()
	{
		dstream=tmpfile(); sstream=tmpfile();
		// let it start at 0
		push_space(0);
	}
	// reads spacestream and moves it backwards
	static size_t take_space()
	{
		//https://en.cppreference.com/w/cpp/io/basic_istream/seekg
		constexpr auto s=sizeof(size_t);
		fseek(sstream,-s,SEEK_CUR);
		size_t l; read(l,sstream);
		fseek(sstream,-s,SEEK_CUR); // assuming seek back at 0 would still be at 0
		return l;
	}
	static void push_space(const size_t& l)
	{
		//cout<<"push space "<<spacestream.tellp()<<' '<<l<<' ';
		write(l,sstream);
	}
	// thanks to en.cppreference.com
	// does not default construct the memory
	pointer allocate()
	{
		size_t l=take_space();
		//cout<<"allocate "<<l<<endl;
		if (!ftell(sstream)) push_space(l+sizeof(t));
		return pointer{l};
	}
	void deallocate(pointer&& p)
	{
		//cout<<"deallocate "<<p.l<<endl;
		push_space(move(p.l));
	}
	// the allocator can stop working correctly after deallocating a deallocated pointer
	void deallocate(pointer& p)
	{
		deallocate(move(p));
		p.l=SIZE_MAX;
	}
	void deallocate(reference& r)
	{
		push_space(move(r.l));
		r.l=SIZE_MAX;
	}
	//https://en.cppreference.com/w/cpp/named_req/Allocator
	constexpr disk_allocator() noexcept {}
	//template<class w> constexpr disk_allocator(const disk_allocator<w>& da) { fs=move(da.fs); }
};
/*
template<typename t> inline void update(const disk_allocator<t>::pointer::reference& r)
{
	r.write();
}
*/
