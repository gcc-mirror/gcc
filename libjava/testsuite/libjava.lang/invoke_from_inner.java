// This test case once showed that `f[0].execute(x)' woudln't be
// expanded properly, attempting to retrieve this$0 to be used in
// place of `f[0]'.

abstract class A {
    abstract public void execute(C x);
}

class C {}

class Z extends A {
    public void execute (C x) {
	System.out.println ("Z.execute");
    }
}

public class invoke_from_inner extends A {

    Z f[] = new Z[1];
    class D extends C {
	D (C x) {
	    f[0].execute (x);
	    execute (x);
	}
    }
    public void execute (C x) {
      System.out.println ("invoke_from_inner.execute");
    }

    public static void main (String a[]) {
	new invoke_from_inner().foo();
    }
    void foo () {
	f[0] = new Z();
	new D(new C());
    }
}
