// CollationElementIterator.java - Iterate over decomposed characters.

/* Copyright (C) 1999, 2001  Free Software Foundation

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
 * Status: Believed complete and correct to JDK 1.1.
 */

public final class CollationElementIterator
{
  public static final int NULLORDER = 0xffffffff;

  public int next ()
  {
    if (index == text.length())
      return NULLORDER;
    return collator.ceiNext(this);
  }

  // This one returns int while the others return short.
  public static final int primaryOrder (int order)
  {
    // From the JDK 1.2 spec.
    return order >>> 16;
  }

  public void reset ()
  {
    index = 0;
  }

  public static final short secondaryOrder (int order)
  {
    // From the JDK 1.2 spec.
    return (short) ((order >>> 8) & 255);
  }

  public static final short tertiaryOrder (int order)
  {
    // From the JDK 1.2 spec.
    return (short) (order & 255);
  }

  // Non-public constructor.
  CollationElementIterator (String text, RuleBasedCollator collator)
  {
    this.text = text;
    this.index = 0;
    this.lookahead_set = false;
    this.lookahead = 0;
    this.collator = collator;
  }

  // Text over which we iterate.
  String text;

  // Index of next character to examine in TEXT.
  int index;

  // A piece of lookahead.
  boolean lookahead_set;
  int lookahead;

  // The RuleBasedCollator which created this object.
  RuleBasedCollator collator;
}
