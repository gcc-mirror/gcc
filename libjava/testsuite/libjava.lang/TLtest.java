class TLtest extends Thread {

    public static void main (String [] args) {
	Data d = new Data ();
	new ThreadTest (d, "A").start ();
	new ThreadTest (d, "B").start ();
    }
}

class Data {

    private static ThreadLocal owner = new ThreadLocal () {
	    public Object initialValue () { return ("0"); }
	};
    /* A thread will call `set' to set a value it wants an instance
       of Data to associate with it and only it. */
    synchronized public void set (String v){owner.set (v);}
    /* A thread will call `get' to get a value it wants an instance
       of Data to associate with it and only it. */
    synchronized public String get (){return (String)owner.get();}
}

class ThreadTest extends Thread {

    public Data d;

    ThreadTest (Data d, String name) {
	super (name);
	this.d = d;
    }

    public void run () {

	int value = 0;
	int ref = 0;

	for (int i = 0; i < 20; i++) {

	    int rand = (int)(Math.random ()*20);

	    /* Read `value', ref is kept for comparison */
	    value = Integer.parseInt (d.get());
	    
	    /* change `value' and ref by a random number, store `value'. */
	    value += rand; ref += rand;
	    d.set (Integer.toString (value));

	    try {
		sleep((int)((Math.random() * 20)));
	    } catch (InterruptedException e) {}
	}

	/* If a thread didn't have private value to attach to the
	   instance of Data, results wouldn't be the same */
	if (ref == value)
	    System.out.println ("test OK.");
	else
	    System.out.println ("test failed.");
    }
}
