// Class anfi
// Generated on Sat Jan 29 16:06:33 PST 2000
// Anonymous with access to outer context locals

class anfi {

    itf foo (final String s, final int i) {
        return new itf () {
            String buff = s+" "+i;
            public void setString (String s) { buff = s+" "+i; }
            public String getString () { return buff; }
        };
    }

    void test () {
        itf x = foo ("Hello", 123);
        System.out.println (x.getString ());
        x.setString ("Frinkahedron");
        System.out.println (x.getString ());
    }

    public static void main (String[] arg)
    {
        System.out.println ("Testing class `anfi'...");
        new anfi().test();
    }
}

interface itf {
    void setString (String s);
    String getString();
    String buff = null;
}
