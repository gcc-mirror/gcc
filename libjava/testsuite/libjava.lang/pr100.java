// PR 100

// Second call to hashcode causes SEGV when dynamically linking.

public class pr100
{
    public static void main(String[] args) {
	pr100 ht = new pr100();
	
	System.err.println(ht.hashCode());
	System.err.println(ht.getClass().hashCode());
    }
}
