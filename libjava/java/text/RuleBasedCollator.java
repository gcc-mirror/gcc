// RuleBasedCollator.java - Concrete class for locale-based string compare.

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date March 25, 1999
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status: Believed complete and correct
 */

class RBCElement
{
  String key;
  char relation;

  RBCElement (String key, char relation)
  {
    this.key = key;
    this.relation = relation;
  }
}

public class RuleBasedCollator extends Collator
{
  public Object clone ()
  {
    return new RuleBasedCollator (this);
  }

  // A helper for CollationElementIterator.next().
  int ceiNext (CollationElementIterator cei)
  {
    if (cei.lookahead_set)
      {
	cei.lookahead_set = false;
	return cei.lookahead;
      }

    int save = cei.index;
    int max = cei.text.length();
    String s = null;

    // It is possible to have a case where `abc' has a mapping, but
    // neither `ab' nor `abd' do.  In this case we must treat `abd' as
    // nothing special.
    boolean found = false;

    int i;
    for (i = save + 1; i <= max; ++i)
      {
	s = cei.text.substring(save, i);
	if (prefixes.get(s) == null)
	  break;
	found = true;
      }
    // Assume s != null.

    Object obj = map.get(s);
    // The special case.
    while (found && obj == null && s.length() > 1)
      {
	--i;
	s = cei.text.substring(save, i);
	obj = map.get(s);
      }

    // Update state.
    cei.index = i;

    if (obj == null)
      {
	// This idea, and the values, come from JDK.
	// assert (s.length() == 1)
	cei.lookahead_set = true;
	cei.lookahead = s.charAt(0) << 8;
	return 0x7fff << 16;
      }

    return ((Integer) obj).intValue();
  }

  // A helper for compareTo() that returns the next character that has
  // a nonzero ordering at the indicated strength.  This is also used
  // in CollationKey.
  static final int next (CollationElementIterator iter, int strength)
  {
    while (true)
      {
	int os = iter.next();
	if (os == CollationElementIterator.NULLORDER)
	  return os;
	int c = 0;
	switch (strength)
	  {
	  case PRIMARY:
	    c = os & ~0xffff;
	    break;
	  case SECONDARY:
	    c = os & ~0x00ff;
	    break;
	  case TERTIARY:
	  case IDENTICAL:
	    c = os;
	    break;
	  }
	if (c != 0)
	  return c;
      }
  }

  public int compare (String source, String target)
  {
    CollationElementIterator cs, ct;

    cs = new CollationElementIterator (source, this);
    ct = new CollationElementIterator (target, this);

    while (true)
      {
	int os = next (cs, strength);
	int ot = next (ct, strength);

	if (os == CollationElementIterator.NULLORDER
	    && ot == CollationElementIterator.NULLORDER)
	  break;
	else if (os == CollationElementIterator.NULLORDER)
	  {
	    // Source string is shorter, so return "less than".
	    return -1;
	  }
	else if (ot == CollationElementIterator.NULLORDER)
	  {
	    // Target string is shorter, so return "greater than".
	    return 1;
	  }

	if (os != ot)
	  return os - ot;
      }

    return 0;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof RuleBasedCollator) || ! super.equals(obj))
      return false;
    RuleBasedCollator rbc = (RuleBasedCollator) obj;
    // FIXME: this is probably wrong.  Instead we should compare maps
    // directly.
    return (frenchAccents == rbc.frenchAccents
	    && rules.equals(rbc.rules));
  }

  public CollationElementIterator getCollationElementIterator (String source)
  {
    StringBuffer expand = new StringBuffer (source.length());
    int max = source.length();
    for (int i = 0; i < max; ++i)
      decomposeCharacter (source.charAt(i), expand);
    return new CollationElementIterator (expand.toString(), this);
  }

  public CollationKey getCollationKey (String source)
  {
    return new CollationKey (getCollationElementIterator (source), source,
			     strength);
  }

  public String getRules ()
  {
    return rules;
  }

  public int hashCode ()
  {
    return (frenchAccents ? 1231 : 1237
	    ^ rules.hashCode()
	    ^ map.hashCode()
	    ^ prefixes.hashCode());
  }

  private final boolean is_special (char c)
  {
    // Rules from JCL book.
    return ((c >= 0x0009 && c <= 0x000d)
	    || (c >= 0x0020 && c <= 0x002f)
	    || (c >= 0x003a && c <= 0x0040)
	    || (c >= 0x005b && c <= 0x0060)
	    || (c >= 0x007b && c <= 0x007e));
  }

  private final int text_argument (String rules, int index,
				   StringBuffer result)
  {
    result.setLength(0);
    int len = rules.length();
    while (index < len)
      {
	char c = rules.charAt(index);
	if (c == '\'' && index + 2 < len
	    && rules.charAt(index + 2) == '\''
	    && is_special (rules.charAt(index + 1)))
	  index += 2;
	else if (is_special (c) || Character.isWhitespace(c))
	  return index;
	result.append(c);
	++index;
      }
    return index;
  }

  public RuleBasedCollator (String rules) throws ParseException
  {
    this.rules = rules;
    this.frenchAccents = false;

    // We keep each rule in order in a vector.  At the end we traverse
    // the vector and compute collation values from it.
    int insertion_index = 0;
    Vector vec = new Vector ();

    StringBuffer argument = new StringBuffer ();

    int len = rules.length();
    for (int index = 0; index < len; ++index)
      {
	char c = rules.charAt(index);

	// Just skip whitespace.
	if (Character.isWhitespace(c))
	  continue;

	// Modifier.
	if (c == '@')
	  {
	    frenchAccents = true;
	    continue;
	  }

	// Check for relation or reset operator.
	if (! (c == '<' || c == ';' || c == ',' || c == '=' || c == '&'))
	  throw new ParseException ("invalid character", index);

	++index;
	while (index < len)
	  {
	    if (! Character.isWhitespace(rules.charAt(index)))
	      break;
	    ++index;
	  }
	if (index == len)
	  throw new ParseException ("missing argument", index);

	int save = index;
	index = text_argument (rules, index, argument);
	if (argument.length() == 0)
	  throw new ParseException ("invalid character", save);
	String arg = argument.toString();
	int item_index = vec.indexOf(arg);
	if (c != '&')
	  {
	    // If the argument already appears in the vector, then we
	    // must remove it in order to re-order.
	    if (item_index != -1)
	      {
		vec.removeElementAt(item_index);
		if (insertion_index >= item_index)
		  --insertion_index;
	      }
	    RBCElement r = new RBCElement (arg, c);
	    vec.insertElementAt(r, insertion_index);
	    ++insertion_index;
	  }
	else
	  {
	    // Reset.
	    if (item_index == -1)
	      throw
		new ParseException ("argument to reset not previously seen",
				    save);
	    insertion_index = item_index + 1;
	  }

	// Ugly: in this case the resulting INDEX comes from
	// text_argument, which returns the index of the next
	// character we should examine.
	--index;
      }

    // Now construct a hash table that maps strings onto their
    // collation values.
    int primary = 0;
    int secondary = 0;
    int tertiary = 0;
    this.map = new Hashtable ();
    this.prefixes = new Hashtable ();
    Enumeration e = vec.elements();
    while (e.hasMoreElements())
      {
	RBCElement r = (RBCElement) e.nextElement();
	switch (r.relation)
	  {
	  case '<':
	    ++primary;
	    secondary = 0;
	    tertiary = 0;
	    break;
	  case ';':
	    ++secondary;
	    tertiary = 0;
	    break;
	  case ',':
	    ++tertiary;
	    break;
	  case '=':
	    break;
	  }
	// This must match CollationElementIterator.
	map.put(r.key, new Integer (primary << 16
				    | secondary << 8 | tertiary));

	// Make a map of all lookaheads we might need.
	for (int i = r.key.length() - 1; i >= 1; --i)
	  prefixes.put(r.key.substring(0, i), Boolean.TRUE);
      }
  }

  // This is a helper for clone.
  private RuleBasedCollator (RuleBasedCollator other)
  {
    frenchAccents = other.frenchAccents;
    rules = other.rules;
    decmp = other.decmp;
    strength = other.strength;
    map = other.map;
    prefixes = other.prefixes;
  }

  // True if we are using French-style accent ordering.
  private boolean frenchAccents;

  // It's easier to just save the rules than to try to recreate them.
  private String rules;

  // This maps strings onto collation values.
  private Hashtable map;
  // An entry in this hash means that more lookahead is required for
  // the prefix string.
  private Hashtable prefixes;
}
