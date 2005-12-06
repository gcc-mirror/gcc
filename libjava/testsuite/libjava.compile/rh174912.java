// Derived from Red Hat bugzilla 174912
// https://bugzilla.redhat.com/bugzilla/show_bug.cgi?id=174912
// The bug is that the anonymous class constructor here will end up
// with a bogus '[3C' in its signature.

public class rh174912 {
  public rh174912(char[][] args) { }

  public Object m() {
    return new rh174912(new char[][] { "hi".toCharArray(),
				       "bob".toCharArray(),
				       "and joe".toCharArray() }) {
      };
  }

  public static void main(String[] args) { }
}
