public class PR12350
{
    static public void main (String[] ignored) throws Throwable
    {
	StringBuffer b = new StringBuffer ("Good string.  More than 16 chars.");

	// Should cause sharing.
	String s = b.toString();

	// Take a char by char unshared copy of s.
	String t = new String (s.toCharArray());

	b.substring (0, 4);	// BUG: Clears shared flag.
	b.replace (0, 4, "Bad "); // Modifies shared data.

	System.out.println (s);
	assert s.equals (t);
    }

}
