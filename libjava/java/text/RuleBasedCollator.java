/* RuleBasedCollator.java -- Concrete Collator Class
   Copyright (C) 1998, 1999, 2000, 2001, 2003  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.text;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status: Believed complete and correct
 */

/**
 * This class is a concrete subclass of <code>Collator</code> suitable
 * for string collation in a wide variety of languages.  An instance of
 * this class is normally returned by the <code>getInstance</code> method
 * of <code>Collator</code> with rules predefined for the requested
 * locale.  However, an instance of this class can be created manually
 * with any desired rules.
 * <p>
 * Rules take the form of a <code>String</code> with the following syntax
 * <ul>
 * <li> Modifier: '@' 
 * <li> Relation: '&lt;' | ';' | ',' | '=' : <text>
 * <li> Reset: '&amp;' : <text>
 * </ul>
 * The modifier character indicates that accents sort backward as is the
 * case with French.  The modifier applies to all rules <b>after</b>
 * the modifier but before the next primary sequence. If placed at the end
 * of the sequence if applies to all unknown accented character.
 * The relational operators specify how the text 
 * argument relates to the previous term.  The relation characters have
 * the following meanings:
 * <ul>
 * <li>'&lt;' - The text argument is greater than the prior term at the primary
 * difference level.
 * <li>';' - The text argument is greater than the prior term at the secondary
 * difference level.
 * <li>',' - The text argument is greater than the prior term at the tertiary
 * difference level.
 * <li>'=' - The text argument is equal to the prior term
 * </ul>
 * <p>
 * As for the text argument itself, this is any sequence of Unicode
 * characters not in the following ranges: 0x0009-0x000D, 0x0020-0x002F,
 * 0x003A-0x0040, 0x005B-0x0060, and 0x007B-0x007E. If these characters are 
 * desired, they must be enclosed in single quotes.  If any whitespace is 
 * encountered, it is ignored.  (For example, "a b" is equal to "ab").  
 * <p>
 * The reset operation inserts the following rule at the point where the
 * text argument to it exists in the previously declared rule string.  This
 * makes it easy to add new rules to an existing string by simply including
 * them in a reset sequence at the end.  Note that the text argument, or
 * at least the first character of it, must be present somewhere in the
 * previously declared rules in order to be inserted properly.  If this
 * is not satisfied, a <code>ParseException</code> will be thrown. 
 * <p>
 * This system of configuring <code>RuleBasedCollator</code> is needlessly
 * complex and the people at Taligent who developed it (along with the folks
 * at Sun who accepted it into the Java standard library) deserve a slow
 * and agonizing death.
 * <p>
 * Here are a couple of example of rule strings:
 * <p>
 * "&lt; a &lt; b &lt; c" - This string says that a is greater than b which is 
 * greater than c, with all differences being primary differences.
 * <p>
 * "&lt; a,A &lt; b,B &lt; c,C" - This string says that 'A' is greater than 'a' with
 * a tertiary strength comparison.  Both 'b' and 'B' are greater than 'a' and
 * 'A' during a primary strength comparison.  But 'B' is greater than 'b'
 * under a tertiary strength comparison.
 * <p>
 * "&lt; a &lt; c &amp; a &lt; b " - This sequence is identical in function to the 
 * "&lt; a &lt; b &lt; c" rule string above.  The '&amp;' reset symbol indicates that
 * the rule "&lt; b" is to be inserted after the text argument "a" in the
 * previous rule string segment.
 * <p>
 * "&lt; a &lt; b &amp; y &lt; z" - This is an error.  The character 'y' does not appear
 * anywhere in the previous rule string segment so the rule following the
 * reset rule cannot be inserted.
 * <p>
 * "&lt; a &amp; A @ &lt; e &amp; E &lt; f&amp; F" - This sequence is equivalent to the following
 * "&lt; a &amp; A &lt; E &amp; e &lt; f &amp; F".
 * <p>
 * For a description of the various comparison strength types, see the
 * documentation for the <code>Collator</code> class.
 * <p>
 * As an additional complication to this already overly complex rule scheme,
 * if any characters precede the first rule, these characters are considered
 * ignorable.  They will be treated as if they did not exist during 
 * comparisons.  For example, "- &lt; a &lt; b ..." would make '-' an ignorable
 * character such that the strings "high-tech" and "hightech" would
 * be considered identical.
 * <p>
 * A <code>ParseException</code> will be thrown for any of the following
 * conditions:
 * <ul>
 * <li>Unquoted punctuation characters in a text argument.
 * <li>A relational or reset operator not followed by a text argument
 * <li>A reset operator where the text argument is not present in
 * the previous rule string section.
 * </ul>
 *
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Tom Tromey <tromey@cygnus.com>
 * @author Guilhem Lavaux <guilhem@kaffe.org>
 */
public class RuleBasedCollator extends Collator
{
  /**
   * This class describes what rank has a character (or a sequence of characters) 
   * in the lexicographic order. Each element in a rule has a collation element.
   */
  final class CollationElement
  {
    String key;
    char relation;

    CollationElement(String key, char relation)
    {
      this.key = key;
      this.relation = relation;
    }
  }

  // True if we are using French-style accent ordering.
  private boolean frenchAccents;

  /**
   * This the the original rule string.
   */
  private String rules;

  // This maps strings onto collation values.
  private Hashtable map;
  
  // An entry in this hash means that more lookahead is required for
  // the prefix string.
  private Hashtable prefixes;
  
  /**
   * This method initializes a new instance of <code>RuleBasedCollator</code>
   * with the specified collation rules.  Note that an application normally
   * obtains an instance of <code>RuleBasedCollator</code> by calling the
   * <code>getInstance</code> method of <code>Collator</code>.  That method
   * automatically loads the proper set of rules for the desired locale.
   *
   * @param rules The collation rule string.
   *
   * @exception ParseException If the rule string contains syntax errors.
   */
  public RuleBasedCollator(String rules) throws ParseException
  {
    if (rules.equals(""))
      throw new ParseException("empty rule set", 0);
    
    this.rules = rules;

    // We keep each rule in order in a vector.  At the end we traverse
    // the vector and compute collation values from it.
    int insertion_index = 0;
    Vector vec = new Vector ();

    int index;
    StringBuffer argument = new StringBuffer();
    int len = rules.length();

    for (index = 0; index < len; ++index)
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
	int item_index = -1;
        
        for (int j = 0; j < vec.size(); ++j)
          {
            CollationElement e = (CollationElement) vec.elementAt (j);

            if (arg.equals (e.key))
              {
                item_index = j;
                break;
              }
          }
	
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
	    CollationElement r = new CollationElement (arg, c);
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
	CollationElement r = (CollationElement) e.nextElement();
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

  /**
   * This method creates a copy of this object.
   *
   * @return A copy of this object.
   */
  public Object clone()
  {
    RuleBasedCollator c = (RuleBasedCollator) super.clone ();
    c.map = (Hashtable) map.clone ();
    c.prefixes = (Hashtable) map.clone ();
    return c;
  }

  // A helper for CollationElementIterator.next().
  int ceiNext (CollationElementIterator cei)
  {
    if (cei.lookahead_set)
      {
	cei.lookahead_set = false;
	return cei.lookahead;
      }

    int save = cei.textIndex;
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
    cei.textIndex = i;

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

  /**
   * This method returns an integer which indicates whether the first
   * specified <code>String</code> is less than, greater than, or equal to
   * the second.  The value depends not only on the collation rules in
   * effect, but also the strength and decomposition settings of this object.
   *
   * @param source The first <code>String</code> to compare.
   * @param target A second <code>String</code> to compare to the first.
   *
   * @return A negative integer if source &lt; target, a positive integer
   * if source &gt; target, or 0 if source == target.
   */
  public int compare(String source, String target)
  {
    CollationElementIterator cs, ct;

    cs = new CollationElementIterator(this, source);
    ct = new CollationElementIterator(this, target);

    for(;;)
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

  /**
   * This method tests this object for equality against the specified 
   * object.  This will be true if and only if the specified object is
   * another reference to this object.
   *
   * @param obj The <code>Object</code> to compare against this object.
   *
   * @return <code>true</code> if the specified object is equal to this object,
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof RuleBasedCollator) || ! super.equals(obj))
      return false;
    RuleBasedCollator rbc = (RuleBasedCollator) obj;
    // FIXME: this is probably wrong.  Instead we should compare maps
    // directly.
    return (frenchAccents == rbc.frenchAccents
	    && rules.equals(rbc.rules));
  }

  /**
   * This method returns an instance for <code>CollationElementIterator</code>
   * for the specified <code>String</code> under the collation rules for this
   * object.
   *
   * @param source The <code>String</code> to return the
   * <code>CollationElementIterator</code> instance for.
   *
   * @return A <code>CollationElementIterator</code> for the specified
   * <code>String</code>.
   */
  public CollationElementIterator getCollationElementIterator(String source)
  {
    int len = source.length();
    StringBuffer expand = new StringBuffer(len);
    
    for (int index = 0; index < len; ++index)
      decomposeCharacter(source.charAt(index), expand);
    
    return new CollationElementIterator(this, expand.toString());
  }

  /**
   * This method returns an instance of <code>CollationElementIterator</code>
   * for the <code>String</code> represented by the specified
   * <code>CharacterIterator</code>.
   *
   * @param source The <code>CharacterIterator</code> with the desired <code>String</code>.
   *
   * @return A <code>CollationElementIterator</code> for the specified <code>String</code>.
   */
  public CollationElementIterator getCollationElementIterator(CharacterIterator source)
  {
    StringBuffer expand = new StringBuffer("");
    
    // Right now we assume that we will read from the beginning of the string.
    for (char c = source.first();
	 c != CharacterIterator.DONE;
	 c = source.next())
      decomposeCharacter(c, expand);

    return new CollationElementIterator(this, expand.toString());
  }

  /**
   * This method returns an instance of <code>CollationKey</code> for the
   * specified <code>String</code>.  The object returned will have a
   * more efficient mechanism for its comparison function that could
   * provide speed benefits if multiple comparisons are performed, such
   * as during a sort.
   *
   * @param source The <code>String</code> to create a <code>CollationKey</code> for.
   *
   * @return A <code>CollationKey</code> for the specified <code>String</code>.
   */
  public CollationKey getCollationKey(String source)
  {
    return new CollationKey(this, getCollationElementIterator(source), source,
			    strength);
  }

  /**
   * This method returns a <code>String</code> containing the collation rules
   * for this object.
   *
   * @return The collation rules for this object.
   */
  public String getRules()
  {
    return rules;
  }

  /**
   * This method returns a hash value for this object.
   *
   * @return A hash value for this object.
   */
  public int hashCode()
  {
    return (frenchAccents ? 1231 : 1237
	    ^ rules.hashCode()
	    ^ map.hashCode()
	    ^ prefixes.hashCode());
  }

  private final boolean is_special (char c)
  {
    // Rules from JCL book.
    return ((c >= 0x0021 && c <= 0x002f)
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
	char c = rules.charAt (index);
	if (c == '\''
            && index + 2 < len
	    && rules.charAt (index + 2) == '\'')
          {
            result.append (rules.charAt (index + 1));
            index += 2;
          }
	else if (is_special (c))
	  return index;
        else if (!Character.isWhitespace (c))
          result.append (c);
        
        ++index;
      }
    return index;
  }

}
