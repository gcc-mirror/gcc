// CollationKey.java - Sort key for locale-sensitive String.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 25, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status: Believed complete and correct.
 */

public final class CollationKey
{
  public int compareTo (CollationKey target)
  {
    int max = Math.min(key.length, target.key.length);

    for (int i = 0; i < max; ++i)
      {
	if (key[i] != target.key[i])
	  return key[i] - target.key[i];
      }

    return key.length - target.key.length;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof CollationKey))
      return false;

    CollationKey ck = (CollationKey) obj;

    if (key.length != ck.key.length)
      return false;

    for (int i = 0; i < key.length; ++i)
      if (key[i] != ck.key[i])
	return false;

    return true;
  }

  public String getSourceString ()
  {
    return originalText;
  }

  public int hashCode ()
  {
    // We just follow BitSet instead of thinking up something new.
    long h = originalText.hashCode();
    for (int i = key.length - 1; i >= 0; --i)
      h ^= key[i] * (i + 1);
    return (int) ((h >> 32) ^ h);
  }

  public byte[] toByteArray ()
  {
    byte[] r = new byte[4 * key.length];
    int off = 0;
    for (int i = 0; i < key.length; ++i)
      {
	r[off++] = (byte) ((key[i] >>> 24) & 255);
	r[off++] = (byte) ((key[i] >>> 16) & 255);
	r[off++] = (byte) ((key[i] >>>  8) & 255);
	r[off++] = (byte) ((key[i]       ) & 255);
      }
    return r;
  }

  CollationKey (CollationElementIterator iter, String originalText,
		int strength)
  {
    this.originalText = originalText;

    // Compute size of required array.
    int size = 0;
    while (RuleBasedCollator.next(iter, strength)
	   != CollationElementIterator.NULLORDER)
      ++size;

    iter.reset();
    key = new int[size];
    for (int i = 0; i < size; i++)
      key[i] = RuleBasedCollator.next(iter, strength);
  }

  // Original string.
  private String originalText;

  // Collation key.
  private int[] key;
}
