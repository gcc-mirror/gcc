Copyright (C) 1993, 2000 Free Software Foundation
	: fstreambase(name, mode | ios::in, prot) { }
	{ fstreambase::open(name, mode | ios::in, prot); }
	: fstreambase(name, mode | ios::out, prot) { }
	{ fstreambase::open(name, mode | ios::out, prot); }
