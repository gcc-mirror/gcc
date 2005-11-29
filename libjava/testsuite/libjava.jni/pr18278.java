public class pr18278 {
    public pr18278() {}

    public static void main(String[] args) {
        System.loadLibrary("pr18278");
	String bob = "Bob";
	Object o = weakRef("Bob");
	System.out.println(o);
	System.out.println(bob == o);
    }

    static native Object weakRef(Object o);
}
