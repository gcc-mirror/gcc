// inner class regression test.

package bar.foo;

class other {
  class foo {}
}

class inner_1 {
    static void bar () {
        inner_1 xxxx;			// Refers to inner_1
	bar.foo.another xyz;		// Refers to non inner another 
	bar.foo.other.foo X;		// OK to declare, inner class foo
	bar.foo.inner_1.t bar;		// Inner class t
	inner_1.t foo;			// Inner class t
	t foobar;			// Inner class t
	other.foo zag;			// Valid for declaration.
	t.ungah x;
    }
    void foo () {
	// z.t.u foo;
	t t1 = new t();
        t1.print (); 
	new t().print();
    }

    class t {
	void print () {
	    System.out.println ("This is `inner_1.t'");
	}
	class ungah {}
    }
}

class another {
} 
