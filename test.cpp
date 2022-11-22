#include <iostream>
#include <concepts>
#include <set>
using namespace std;

#include "disk_map.cpp"
#include <random>
#include <cassert>
int main()
{
	disk_map<int,int> m;
	m.init_allocator();
	cout<<"basic map test\n";
	m[0]=1;
	m[-1]=2;
	cout<<"Map at 0\n";
	cout<<m[0]<<endl;
	cout<<"Map has key 1?\n";
	cout<<m.contains(1)<<endl;
	cout<<"Map at 1\n";
	cout<<m[1]<<endl;
	cout<<"Map has key 1?\n";
	cout<<m.contains(1)<<endl;
	cout<<"Map at -1\n";
	cout<<m[-1]<<endl;
	disk_tree<int> t;
	t.init_allocator();
	uniform_int_distribution u{-99,99};
	minstd_rand mle;
	set<int> v;
	auto p=t.copy();
	for (char i=0;i<100;i++) 
	{
		auto j=u(mle);
		//cout<<j<<endl;
		t.insert(j);
		if (!t.wellOrderedUnique())
		{
			p.insert(j);
			assert(0);
		}
		v.insert(j);
		p=t.copy();
	}
	assert(t.root->correctBalance());
	cout<<"std::set and disk_tree comparison\n";
	assert(t.size()==v.size());
	auto vi=v.begin();
	// thanks to The Cherno and the people who taught for (int i= ... 
	for (auto&& l:t)
	{
		assert(l==*vi);
		cout<<l<<' '<<*vi<<'\n';
		++vi;
	}
	assert(vi==v.end());
	/*
	 //to expose to GDB
	--t.end();
	--v.end();
	*/
	cout<<"erase random\n";
	while (auto n=v.size())
	{
		u=uniform_int_distribution<int>{0,n-1};
		auto b=v.begin();
		auto j=u(mle);
		for (auto i=0;i<j;i++) b++;
		cout<<"erasing "<<*b<<endl;
		auto s=*b;
		t.erase(s); v.erase(s);
		auto a=t.begin(); b=v.begin();
		while (b!=v.end())
		{
			if (*b!=*a)
			{
				p.erase(s);
				assert(0);
			}
			++b; ++a;
		}
		assert(b==v.end());
		assert(a==t.end());
		p=t.copy();
	}
	cout<<v.size()<<' '<<t.size()<<endl;
	
	return 0;
}
