/* Verify that libffi closures aren't deallocated too early.

   Copyright (C) 2007 Free Software Foundation, Inc
   Contributed by Alexandre Oliva <aoliva@redhat.com>

   If libffi closures are released too early, we lose.
 */

import java.util.HashSet;

public class TestClosureGC {
    public static String objId (Object obj) {
	return obj + "/"
	    + Integer.toHexString(obj.getClass().getClassLoader().hashCode());
    }
    public static class cld extends java.net.URLClassLoader {
	static final Object obj = new cl0();
	public cld () throws Exception {
	    super(new java.net.URL[] { });
	    /* System.out.println (objId (this) + " created"); */
	}
	public void finalize () {
	    /* System.out.println (objId (this) + " finalized"); */
	}
	public String toString () {
	    return this.getClass().getName() + "@"
		+ Integer.toHexString (hashCode ());
	}
	public Class loadClass (String name) throws ClassNotFoundException {
	    try {
		java.io.InputStream IS = getSystemResourceAsStream
		    (name + ".class");
		int maxsz = 1024, readsz = 0;
		byte buf[] = new byte[maxsz];
		for(;;) {
		    int readnow = IS.read (buf, readsz, maxsz - readsz);
		    if (readnow <= 0)
			break;
		    readsz += readnow;
		    if (readsz == maxsz) {
			byte newbuf[] = new byte[maxsz *= 2];
			System.arraycopy (buf, 0, newbuf, 0, readsz);
			buf = newbuf;
		    }
		}
		return defineClass (name, buf, 0, readsz);
	    } catch (Exception e) {
		return super.loadClass (name);
	    }
	}
    }
    public static class cl0 {
	public cl0 () {
	    /* System.out.println (objId (this) + " created"); */
	}
	public void finalize () {
	    /* System.out.println (objId (this) + " finalized"); */
	}
    }
    public static class cl1 {
	final HashSet hs;
	static final Object obj = new cl0();
	public cl1 (final HashSet hs) {
	    this.hs = hs;
	    /* System.out.println (objId (this) + " created"); */
	}
	public void finalize () {
	    /* System.out.println (objId (this) + " finalized"); */
	}
    }
    public static class cl2 {
	final HashSet hs;
	static final Object obj = new cl0();
	public cl2 (final HashSet hs) {
	    this.hs = hs;
	    /* System.out.println (objId (this) + " created"); */
	}
	public void finalize () {
	    /* System.out.println (objId (this) + " finalized"); */
	    hs.add(this);
	    hs.add(new cl0());
	}
    }
    static final HashSet hs = new HashSet();
    static final Object obj = new cl0();
    public static void main(String[] argv) throws Exception {
	{
	    Class[] hscs = { HashSet.class };
	    Object[] hsos = { hs };
	    new cld().loadClass ("TestClosureGC$cl1").
		getConstructor (hscs).newInstance (hsos);
	    new cld().loadClass ("TestClosureGC$cl2").
		getConstructor (hscs).newInstance (hsos);
	    new cld().loadClass ("TestClosureGC$cl1").
		getConstructor (hscs).newInstance (hsos);
	    new cld().loadClass ("TestClosureGC$cl1").
		getConstructor (hscs).newInstance (hsos);
	}
	for (int i = 1; i <= 5; i++) {
	    /* System.out.println ("Will run GC and finalization " + i); */
	    System.gc ();
	    Thread.sleep (100);
	    System.runFinalization ();
	    Thread.sleep (100);
	    if (hs.isEmpty ())
		continue;
	    java.util.Iterator it = hs.iterator ();
	    while (it.hasNext ()) {
		Object obj = it.next();
		/* System.out.println (objId (obj) + " in ht, removing"); */
		it.remove ();
	    }
	}
	System.out.println ("ok");
    }
}
