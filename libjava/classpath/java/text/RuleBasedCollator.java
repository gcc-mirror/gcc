/* RuleBasedCollator.java -- Concrete Collator Class
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

import java.util.ArrayList;
import java.util.HashMap;

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
 * <li> Modifier: '@'</li>
 * <li> Relation: '&lt;' | ';' | ',' | '=' : &lt;text&gt;</li>
 * <li> Reset: '&amp;' : &lt;text&gt;</li>
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
 * difference level.</li>
 * <li>';' - The text argument is greater than the prior term at the secondary
 * difference level.</li>
 * <li>',' - The text argument is greater than the prior term at the tertiary
 * difference level.</li>
 * <li>'=' - The text argument is equal to the prior term</li>
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
 * <li>Unquoted punctuation characters in a text argument.</li>
 * <li>A relational or reset operator not followed by a text argument</li>
 * <li>A reset operator where the text argument is not present in
 * the previous rule string section.</li>
 * </ul>
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Guilhem Lavaux (guilhem@kaffe.org)
 */
public class RuleBasedCollator extends Collator
{
  /**
   * This class describes what rank has a character (or a sequence of characters) 
   * in the lexicographic order. Each element in a rule has a collation element.
   */
  static final class CollationElement
  {
    String key;
    int primary;
    short secondary;
    short tertiary;
    short equality;
    boolean ignore;
    String expansion;

    CollationElement(String key, int primary, short secondary, short tertiary,
		     short equality, String expansion, boolean ignore)
    {
      this.key = key;
      this.primary = primary;
      this.secondary = secondary;
      this.tertiary = tertiary;
      this.equality = equality;
      this.ignore = ignore;
      this.expansion = expansion;
    }

    int getValue()
    {
      return (primary << 16) + (secondary << 8) + tertiary;
    }
  }

  /**
   * Basic collation instruction (internal format) to build the series of
   * collation elements. It contains an instruction which specifies the new
   * state of the generator. The sequence of instruction should not contain
   * RESET (it is used by
   * {@link #mergeRules(int,java.lang.String,java.util.ArrayList,java.util.ArrayList)})
   * as a temporary state while merging two sets of instructions.
   */
  static final class CollationSorter
  {
    static final int GREATERP = 0;
    static final int GREATERS = 1;
    static final int GREATERT = 2;
    static final int EQUAL = 3;
    static final int RESET = 4;
    static final int INVERSE_SECONDARY = 5;
    
    int comparisonType;
    String textElement;
    int hashText;
    int offset;
    boolean ignore;

    String expansionOrdering;
  }

  /**
   * This the the original rule string.
   */
  private String rules;

  /**
   * This is the table of collation element values
   */
  private Object[] ce_table;

  /**
   * Quick-prefix finder.
   */
  HashMap prefix_tree;

  /**
   * This is the value of the last sequence entered into
   * <code>ce_table</code>. It is used to compute the
   * ordering value of unspecified character.
   */
  private int last_primary_value;

  /**
   * This is the value of the last secondary sequence of the
   * primary 0, entered into
   * <code>ce_table</code>. It is used to compute the
   * ordering value of an unspecified accented character.
   */
  private int last_tertiary_value;

  /**
   * This variable is true if accents need to be sorted
   * in the other direction.
   */
  private boolean inverseAccentComparison;

  /**
   * This collation element is special to unknown sequence.
   * The JDK uses it to mark and sort the characters which has
   * no collation rules.
   */
  static final CollationElement SPECIAL_UNKNOWN_SEQ = 
    new CollationElement("", (short) 32767, (short) 0, (short) 0,
			 (short) 0, null, false);
  
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

    buildCollationVector(parseString(rules));
    buildPrefixAccess();
  }

  /**
   * This method returns the number of common characters at the beginning
   * of the string of the two parameters.
   *
   * @param prefix A string considered as a prefix to test against
   * the other string.
   * @param s A string to test the prefix against.
   * @return The number of common characters.
   */
  static int findPrefixLength(String prefix, String s)
  {
    int index;
    int len = prefix.length();

    for (index = 0; index < len && index < s.length(); ++index)
      {
	if (prefix.charAt(index) != s.charAt(index))
	  return index;
      }


    return index;
  }

  /**
   * Here we are merging two sets of sorting instructions: 'patch' into 'main'. This methods
   * checks whether it is possible to find an anchor point for the rules to be merged and
   * then insert them at that precise point.
   *
   * @param offset Offset in the string containing rules of the beginning of the rules
   * being merged in.
   * @param starter Text of the rules being merged.
   * @param main Repository of all already parsed rules.
   * @param patch Rules to be merged into the repository.
   * @throws ParseException if it is impossible to find an anchor point for the new rules.
   */
  private void mergeRules(int offset, String starter, ArrayList main, ArrayList patch)
    throws ParseException 
  {
    int insertion_point = -1;
    int max_length = 0;
    
    /* We must check that no rules conflict with another already present. If it
     * is the case delete the old rule. 
     */
    
    /* For the moment good old O(N^2) algorithm.
     */
    for (int i = 0; i < patch.size(); i++)
      {
	int j = 0;
	
	while (j < main.size())
	  {
	    CollationSorter rule1 = (CollationSorter) patch.get(i);
	    CollationSorter rule2 = (CollationSorter) main.get(j);
	    
	    if (rule1.textElement.equals(rule2.textElement))
	      main.remove(j);
	    else
	      j++;
	  }
      }

    // Find the insertion point... O(N)
    for (int i = 0; i < main.size(); i++)
      {
	CollationSorter sorter = (CollationSorter) main.get(i);
	int length = findPrefixLength(starter, sorter.textElement);
		
	if (length > max_length)
	  {
	    max_length = length;
	    insertion_point = i+1;
	  }
      }

    if (insertion_point < 0)
      throw new ParseException("no insertion point found for " + starter, offset);

    if (max_length < starter.length())
      {
	/*
	 * We need to expand the first entry. It must be sorted
	 * like if it was the reference key itself (like the spec
	 * said. So the first entry is special: the element is
	 * replaced by the specified text element for the sorting.
	 * This text replace the old one for comparisons. However
	 * to preserve the behaviour we replace the first key (corresponding
	 * to the found prefix) by a new code rightly ordered in the
	 * sequence. The rest of the subsequence must be appended
	 * to the end of the sequence.
	 */
	CollationSorter sorter = (CollationSorter) patch.get(0);
	CollationSorter expansionPrefix =
	  (CollationSorter) main.get(insertion_point-1);
	
	sorter.expansionOrdering = starter.substring(max_length); // Skip the first good prefix element
		
	main.add(insertion_point, sorter);
	
	/*
	 * This is a new set of rules. Append to the list.
	 */
	patch.remove(0);
	insertion_point++;
      }

    // Now insert all elements of patch at the insertion point.
    for (int i = 0; i < patch.size(); i++)
      main.add(i+insertion_point, patch.get(i));
  }

  /**
   * This method parses a string and build a set of sorting instructions. The parsing
   * may only be partial on the case the rules are to be merged sometime later.
   * 
   * @param stop_on_reset If this parameter is true then the parser stops when it
   * encounters a reset instruction. In the other case, it tries to parse the subrules
   * and merged it in the same repository.
   * @param v Output vector for the set of instructions.
   * @param base_offset Offset in the string to begin parsing.
   * @param rules Rules to be parsed.
   * @return -1 if the parser reached the end of the string, an integer representing the
   * offset in the string at which it stopped parsing. 
   * @throws ParseException if something turned wrong during the parsing. To get details
   * decode the message.
   */
  private int subParseString(boolean stop_on_reset, ArrayList v,
			     int base_offset, String rules)
    throws ParseException
  {
    boolean ignoreChars = (base_offset == 0);
    int operator = -1;
    StringBuffer sb = new StringBuffer();
    boolean doubleQuote = false;
    boolean eatingChars = false;
    boolean nextIsModifier = false;
    boolean isModifier = false;
    int i;
    
main_parse_loop:
    for (i = 0; i < rules.length(); i++)
      {
	char c = rules.charAt(i);
	int type = -1;
	
	if (!eatingChars &&
	    ((c >= 0x09 && c <= 0x0D) || (c == 0x20)))
	      continue;

	isModifier = nextIsModifier;
	nextIsModifier = false;

	if (eatingChars && c != '\'')
	  {
	    doubleQuote = false;
	    sb.append(c);
	    continue;
	  }
	if (doubleQuote && eatingChars)
	  {
	    sb.append(c);
	    doubleQuote = false;
	    continue;
	  }

	switch (c)
	  {
	  case '!':
	    throw new ParseException
	      ("Modifier '!' is not yet supported by Classpath", i + base_offset);
	  case '<':
	    type = CollationSorter.GREATERP;
	    break;
	  case ';':
	    type = CollationSorter.GREATERS;
	    break;
	  case ',':
	    type = CollationSorter.GREATERT;
	    break;
	  case '=':
	    type = CollationSorter.EQUAL;
	    break;
	  case '\'':
	    eatingChars = !eatingChars;
	    doubleQuote = true;
	    break;
	  case '@':
	    if (ignoreChars)
	      throw new ParseException
		("comparison list has not yet been started. You may only use"
		 + "(<,;=&)", i + base_offset);
	    // Inverse the order of secondaries from now on.
	    nextIsModifier = true;
	    type = CollationSorter.INVERSE_SECONDARY;
	    break;
	  case '&':
	    type = CollationSorter.RESET;
	    if (stop_on_reset)
	      break main_parse_loop;
	    break;
	  default:
	    if (operator < 0)
	      throw new ParseException
		("operator missing at " + (i + base_offset), i + base_offset);
	    if (! eatingChars
		&& ((c >= 0x21 && c <= 0x2F) 
		    || (c >= 0x3A && c <= 0x40)
		    || (c >= 0x5B && c <= 0x60)
		    || (c >= 0x7B && c <= 0x7E)))
	      throw new ParseException
		("unquoted punctuation character '" + c + "'", i + base_offset);

	    //type = ignoreChars ? CollationSorter.IGNORE : -1;
	    sb.append(c);
	    break;
	  }

	if (type  < 0)
	  continue;

	if (operator < 0)
	  {
	    operator = type;
	    continue;
	  }

	if (sb.length() == 0 && !isModifier)
	  throw new ParseException
	    ("text element empty at " + (i+base_offset), i+base_offset);

	if (operator == CollationSorter.RESET)
	  {
	    /* Reposition in the sorting list at the position
	     * indicated by the text element.
	     */
	    String subrules = rules.substring(i);
	    ArrayList sorted_rules = new ArrayList();
	    int idx;

	    // Parse the subrules but do not iterate through all
	    // sublist. This is the privilege of the first call.
	    idx = subParseString(true, sorted_rules, base_offset+i, subrules);
    
	    // Merge new parsed rules into the list.
	    mergeRules(base_offset+i, sb.toString(), v, sorted_rules);
	    sb.setLength(0);
	    
	    // Reset state to none.
	    operator = -1;
	    type = -1;
	    // We have found a new subrule at 'idx' but it has not been parsed.
	    if (idx >= 0)
	      {
		i += idx-1;
		continue main_parse_loop;
	      }
	    else
		// No more rules.
		break main_parse_loop;
	  }

	CollationSorter sorter = new CollationSorter();
	
	if (operator == CollationSorter.GREATERP)
	  ignoreChars = false;

	sorter.comparisonType = operator;
	sorter.textElement = sb.toString();
	sorter.hashText = sorter.textElement.hashCode();
	sorter.offset = base_offset+rules.length();
	sorter.ignore = ignoreChars;
	sb.setLength(0);

	v.add(sorter);
	operator = type;
      }

    if (operator >= 0)
      {
	CollationSorter sorter = new CollationSorter();
	int pos = rules.length() + base_offset;

	if ((sb.length() != 0 && nextIsModifier)
	    || (sb.length() == 0 && !nextIsModifier && !eatingChars))
	  throw new ParseException("text element empty at " + pos, pos);

	if (operator == CollationSorter.GREATERP)
	  ignoreChars = false;

	sorter.comparisonType = operator;
	sorter.textElement = sb.toString();
 	sorter.hashText = sorter.textElement.hashCode();
	sorter.offset = base_offset+pos;
	sorter.ignore = ignoreChars;
	v.add(sorter);
      }

    if (i == rules.length())
      return -1;
    else
      return i;
  }

  /**
   * This method creates a copy of this object.
   *
   * @return A copy of this object.
   */
  public Object clone()
  {
    return super.clone();
  }

  /**
   * This method completely parses a string 'rules' containing sorting rules.
   *
   * @param rules String containing the rules to be parsed. 
   * @return A set of sorting instructions stored in a Vector.
   * @throws ParseException if something turned wrong during the parsing. To get details
   * decode the message.
   */
  private ArrayList parseString(String rules) 
    throws ParseException
  {
    ArrayList v = new ArrayList();

    // result of the first subParseString is not absolute (may be -1 or a
    // positive integer). But we do not care.
    subParseString(false, v, 0, rules);
    
    return v;
  }

  /**
   * This method uses the sorting instructions built by {@link #parseString}
   * to build collation elements which can be directly used to sort strings.
   *
   * @param parsedElements Parsed instructions stored in a ArrayList.
   * @throws ParseException if the order of the instructions are not valid.
   */
  private void buildCollationVector(ArrayList parsedElements)
    throws ParseException
  {
    int primary_seq = 0;
    int last_tertiary_seq = 0;
    short secondary_seq = 0;
    short tertiary_seq = 0;
    short equality_seq = 0;
    boolean inverseComparisons = false;
    final boolean DECREASING = false;
    final boolean INCREASING = true;
    boolean secondaryType = INCREASING;
    ArrayList v = new ArrayList();

    // elts is completely sorted.
element_loop:
    for (int i = 0; i < parsedElements.size(); i++)
      {
	CollationSorter elt = (CollationSorter) parsedElements.get(i);
	boolean ignoreChar = false;

	switch (elt.comparisonType)
	  {
	  case CollationSorter.GREATERP:
	    primary_seq++;
	    if (inverseComparisons)
	      {
		secondary_seq = Short.MAX_VALUE;
		secondaryType = DECREASING;
	      }
	    else
	      {
		secondary_seq = 0;
		secondaryType = INCREASING;
	      }
	    tertiary_seq = 0;
	    equality_seq = 0;
	    inverseComparisons = false;
	    break;
	  case CollationSorter.GREATERS:
	    if (secondaryType == DECREASING)
	      secondary_seq--;
	    else
	      secondary_seq++;
	    tertiary_seq = 0;
	    equality_seq = 0;
	    break;
	  case CollationSorter.INVERSE_SECONDARY:
	    inverseComparisons = true;
	    continue element_loop;
	  case CollationSorter.GREATERT:
	    tertiary_seq++;
	    if (primary_seq == 0)
	      last_tertiary_seq = tertiary_seq;
	    equality_seq = 0;
	    break;
	  case CollationSorter.EQUAL:
	    equality_seq++;
	    break;
	  case CollationSorter.RESET:
	    throw new ParseException
	      ("Invalid reached state 'RESET'. Internal error", elt.offset);
	  default:
	    throw new ParseException
	      ("Invalid unknown state '" + elt.comparisonType + "'", elt.offset);
	  }

	v.add(new CollationElement(elt.textElement, primary_seq,
				   secondary_seq, tertiary_seq,
				   equality_seq, elt.expansionOrdering, elt.ignore));
      }

    this.inverseAccentComparison = inverseComparisons; 

    ce_table = v.toArray();

    last_primary_value = primary_seq+1;
    last_tertiary_value = last_tertiary_seq+1;
  }

  /**
   * Build a tree where all keys are the texts of collation elements and data is
   * the collation element itself. The tree is used when extracting all prefix
   * for a given text.
   */
  private void buildPrefixAccess()
  {
    prefix_tree = new HashMap();

    for (int i = 0; i < ce_table.length; i++)
      {
	CollationElement e = (CollationElement) ce_table[i];

	prefix_tree.put(e.key, e);
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
    CollationElement ord1block = null;
    CollationElement ord2block = null;
    boolean advance_block_1 = true;
    boolean advance_block_2 = true;

    cs = getCollationElementIterator(source);
    ct = getCollationElementIterator(target);

    for(;;)
      {
	int ord1;
	int ord2;

	/*
	 * We have to check whether the characters are ignorable.
	 * If it is the case then forget them. 
	 */
	if (advance_block_1)
	  {
	    ord1block = cs.nextBlock();
	    if (ord1block != null && ord1block.ignore)
	      continue;
	  }
	
	if (advance_block_2)
	  {
	    ord2block = ct.nextBlock();
	    if (ord2block != null && ord2block.ignore)
	      {
	        advance_block_1 = false;
	        continue;
	      }
	 }
	else
	  advance_block_2 = true;

	if (!advance_block_1)
	  advance_block_1 = true;

	if (ord1block != null)
	  ord1 = ord1block.getValue();
	else
	  {
	    if (ord2block == null)
	      return 0;
	    return -1;
	  }

	if (ord2block == null)
	  return 1;
	
	ord2 = ord2block.getValue();
	
	// We know chars are totally equal, so skip
        if (ord1 == ord2)
	  {
	    if (getStrength() == IDENTICAL)
	      if (!ord1block.key.equals(ord2block.key))
		return ord1block.key.compareTo(ord2block.key);
	    continue;
	  }

        // Check for primary strength differences
        int prim1 = CollationElementIterator.primaryOrder(ord1); 
        int prim2 = CollationElementIterator.primaryOrder(ord2); 
	
	if (prim1 == 0 && getStrength() < TERTIARY)
	  {
            advance_block_2 = false;
	    continue;
	  }
	else if (prim2 == 0 && getStrength() < TERTIARY)
	  {
	    advance_block_1 = false;
	    continue;
	  }

        if (prim1 < prim2)
          return -1;
        else if (prim1 > prim2)
          return 1;
        else if (getStrength() == PRIMARY)
          continue;

        // Check for secondary strength differences
        int sec1 = CollationElementIterator.secondaryOrder(ord1);
        int sec2 = CollationElementIterator.secondaryOrder(ord2);

	if (sec1 < sec2)
          return -1;
        else if (sec1 > sec2)
          return 1;
        else if (getStrength() == SECONDARY)
          continue;

        // Check for tertiary differences
        int tert1 = CollationElementIterator.tertiaryOrder(ord1);
        int tert2 = CollationElementIterator.tertiaryOrder(ord2);

        if (tert1 < tert2)
          return -1;
        else if (tert1 > tert2)
          return 1;
	else if (getStrength() == TERTIARY)
	  continue;

	// Apparently JDK does this (at least for my test case).
	return ord1block.key.compareTo(ord2block.key);    
      }
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
    if (obj == this)
      return true;
    else
      return false;
  }

  /**
   * This method builds a default collation element without invoking
   * the database created from the rules passed to the constructor.
   *
   * @param c Character which needs a collation element.
   * @return A valid brand new CollationElement instance.
   */
  CollationElement getDefaultElement(char c)
  {
    int v;

    // Preliminary support for generic accent sorting inversion (I don't know if all
    // characters in the range should be sorted backward). This is the place
    // to fix this if needed.
    if (inverseAccentComparison && (c >= 0x02B9 && c <= 0x0361))
      v = 0x0361 - ((int) c - 0x02B9);
    else
      v = (short) c;
    return new CollationElement("" + c, last_primary_value + v,
				(short) 0, (short) 0, (short) 0, null, false);
  }

  /**
   * This method builds a default collation element for an accented character
   * without invoking the database created from the rules passed to the constructor.
   *
   * @param c Character which needs a collation element.
   * @return A valid brand new CollationElement instance.
   */
  CollationElement getDefaultAccentedElement(char c)
  {
    int v;

    // Preliminary support for generic accent sorting inversion (I don't know if all
    // characters in the range should be sorted backward). This is the place
    // to fix this if needed.
    if (inverseAccentComparison && (c >= 0x02B9 && c <= 0x0361))
      v = 0x0361 - ((int) c - 0x02B9);
    else
      v = (short) c;
    return new CollationElement("" + c, (short) 0,
				(short) 0, (short) (last_tertiary_value + v), (short) 0, null, false);
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
    return new CollationElementIterator(this, source);
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

    return getCollationElementIterator(expand.toString());
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
    CollationElementIterator cei = getCollationElementIterator(source);
    ArrayList vect = new ArrayList();

    int ord = cei.next();
    cei.reset(); //set to start of string

    while (ord != CollationElementIterator.NULLORDER)
      {
	// If the primary order is null, it means this is an ignorable
	// character.
	if (CollationElementIterator.primaryOrder(ord) == 0)
	  {
            ord = cei.next();
	    continue;
	  }
        switch (getStrength())
          {
            case PRIMARY:
	      ord = CollationElementIterator.primaryOrder(ord);
	      break;
	      
            case SECONDARY:
	      ord = CollationElementIterator.primaryOrder(ord) << 8;
	      ord |= CollationElementIterator.secondaryOrder(ord);

            default:
               break;
          }

        vect.add(new Integer(ord)); 
	ord = cei.next(); //increment to next key
      }

    Object[] objarr = vect.toArray();
    byte[] key = new byte[objarr.length * 4];

    for (int i = 0; i < objarr.length; i++)
      {
        int j = ((Integer) objarr[i]).intValue();
        key [i * 4] = (byte) ((j & 0xFF000000) >> 24);
        key [i * 4 + 1] = (byte) ((j & 0x00FF0000) >> 16);
        key [i * 4 + 2] = (byte) ((j & 0x0000FF00) >> 8);
        key [i * 4 + 3] = (byte) (j & 0x000000FF);
      }

    return new CollationKey(this, source, key);
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
    return System.identityHashCode(this);
  }
}
