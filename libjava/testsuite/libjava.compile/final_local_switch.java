// This bug was discovered while working on java/1413 (compiling freetds.)
// http://gcc.gnu.org/ml/java-prs/2000-q4/msg00156.html
// The following code should build.

class final_local_switch {
    void foo (int type) {
	final byte CHARSET_CHANGE = (byte)3;
	final byte CHARSET_CHANGES = (byte)4;
	switch (type) {
	case CHARSET_CHANGE:
	    break;
	case CHARSET_CHANGES:
	    break;
	}
    }
}
