// Test to see if "blank final" variables work.
// From Mo DeJong <mdejong@cygnus.com>

public class BlankFinal {
    static final boolean cond;

    static {
        try
        {
            cond = true;
        }
        catch(Exception e) {
            // do nothing
        }
    }
}
