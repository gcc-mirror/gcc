// Class inner4
// Generated on Tue Dec  7 11:43:48 PST 1999
//

class inner4 {
    static private int xyz () { return 3; }
    private String f;
  
    String p () {
	return "public String p()";
    }

    private String pp (int x, byte y, char c) {
	return "private String pp("+x+", "+y+", "+c+")";
    }

    void foo () {
	t xxx = this.new t();
	xxx.bar ();
        pp (3, (byte)34, 'C');
    }
    public static void main (String[] arg)
    {
	System.out.println ("Testing class `inner4'...");
	new inner4().foo();
    }
    class t {
	void bar () {
            System.out.println (p ());
	    System.out.println (pp (3, (byte)34, 'C'));
            System.out.println (xyz ());
	}
    }
}
