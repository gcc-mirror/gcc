/* CollationElementIterator.java -- Walks through collation elements
   Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004  Free Software Foundation

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

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 from http://www.javasoft.com.
 * Status: Believed complete and correct to JDK 1.1.
 */

/**
 * This class walks through the character collation elements of a 
 * <code>String</code> as defined by the collation rules in an instance of 
 * <code>RuleBasedCollator</code>.  There is no public constructor for
 * this class.  An instance is created by calling the
 * <code>getCollationElementIterator</code> method on 
 * <code>RuleBasedCollator</code>.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Guilhem Lavaux (guilhem.lavaux@free.fr)
 */
public final class CollationElementIterator
{
  /**
   * This is a constant value that is returned to indicate that the end of 
   * the string was encountered.
   */
  public static final int NULLORDER = -1;

  /**
   * This is the RuleBasedCollator this object was created from.
   */
  RuleBasedCollator collator;

  /**
   * This is the String that is being iterated over.
   */
  String text;

  /**
   * This is the index into the collation decomposition where we are currently scanning.
   */
  int index;

  /**
   * This is the index into the String where we are currently scanning.
   */
  int textIndex;

  /**
   * Array containing the collation decomposition of the
   * text given to the constructor.
   */
  private RuleBasedCollator.CollationElement[] text_decomposition;

  /**
   * Array containing the index of the specified block.
   */
  private int[] text_indexes;

  /**
   * This method initializes a new instance of <code>CollationElementIterator</code>
   * to iterate over the specified <code>String</code> using the rules in the
   * specified <code>RuleBasedCollator</code>.
   *
   * @param collator The <code>RuleBasedCollation</code> used for calculating collation values
   * @param text The <code>String</code> to iterate over.
   */
  CollationElementIterator(RuleBasedCollator collator, String text)
  {
    this.collator = collator;
    
    setText (text);    
  }

  RuleBasedCollator.CollationElement nextBlock()
  {
    if (index >= text_decomposition.length)
      return null;
    
    RuleBasedCollator.CollationElement e = text_decomposition[index];
    
    textIndex = text_indexes[index+1];

    index++;

    return e;
  }

  RuleBasedCollator.CollationElement previousBlock()
  {
    if (index == 0)
      return null;
    
    index--;
    RuleBasedCollator.CollationElement e = text_decomposition[index];

    textIndex = text_indexes[index+1];
    
    return e;
  }

  /**
   * This method returns the collation ordering value of the next character sequence
   * in the string (it may be an extended character following collation rules).
   * This method will return <code>NULLORDER</code> if the
   * end of the string was reached.
   *
   * @return The collation ordering value.
   */
  public int next()
  {
    RuleBasedCollator.CollationElement e = nextBlock();

    if (e == null)
      return NULLORDER;
    
    return e.getValue();
  }

  /**
   * This method returns the collation ordering value of the previous character
   * in the string.  This method will return <code>NULLORDER</code> if the
   * beginning of the string was reached.
   *
   * @return The collation ordering value.
   */
  public int previous()
  {
    RuleBasedCollator.CollationElement e = previousBlock();

    if (e == null)
      return NULLORDER;
    
    return e.getValue();
  }

  /**
   * This method returns the primary order value for the given collation
   * value.
   *
   * @param order The collation value returned from <code>next()</code> or 
   *              <code>previous()</code>.
   *
   * @return The primary order value of the specified collation value.  This is
   *         the high 16 bits.
   */
  public static int primaryOrder(int order)
  {
    // From the JDK 1.2 spec.
    return order >>> 16;
  }

  /**
   * This method resets the internal position pointer to read from the
   * beginning of the <code>String</code> again.
   */
  public void reset()
  {
    index = 0;
    textIndex = 0;
  }

  /**
   * This method returns the secondary order value for the given collation
   * value.
   *
   * @param order The collation value returned from <code>next()</code> or 
   *              <code>previous()</code>.
   *
   * @return The secondary order value of the specified collation value.  This 
   *         is the bits 8-15.
   */
  public static short secondaryOrder(int order)
  {
    // From the JDK 1.2 spec.
    return (short) ((order >>> 8) & 255);
  }

  /**
   * This method returns the tertiary order value for the given collation
   * value.
   *
   * @param order The collation value returned from <code>next()</code> or 
   *              <code>previous()</code>.
   *
   * @return The tertiary order value of the specified collation value.  This 
   *         is the low eight bits.
   */
  public static short tertiaryOrder(int order)
  {
    // From the JDK 1.2 spec.
    return (short) (order & 255);
  }

  /**
   * This method sets the <code>String</code> that it is iterating over
   * to the specified <code>String</code>.
   *
   * @param text The new <code>String</code> to iterate over.
   *
   * @since 1.2
   */
  public void setText(String text)
  {
    int idx = 0;
    int idx_idx = 0;
    int alreadyExpanded = 0;
    int idxToMove = 0;

    this.text = text;
    this.index = 0;

    String work_text = text.intern();

    ArrayList a_element = new ArrayList();
    ArrayList a_idx = new ArrayList();

    // Build element collection ordered as they come in "text".
    while (idx < work_text.length())
      {
	String key, key_old;

	Object object = null;
	int p = 1;
	
	// IMPROVE: use a TreeMap with a prefix-ordering rule.
	key_old = key = null;
	do
	  {
	    if (object != null)
	      key_old = key;
	    key = work_text.substring (idx, idx+p);
	    object = collator.prefix_tree.get (key);
	    if (object != null && idx < alreadyExpanded)
	      {
		RuleBasedCollator.CollationElement prefix = (RuleBasedCollator.CollationElement)object;
		if (prefix.expansion != null && 
		    prefix.expansion.startsWith(work_text.substring(0, idx)))
		{
		  object = null;
		  key = key_old;
		}
	      }
	    p++;
	  }
	while (idx+p <= work_text.length());
	
	if (object == null)
	  key = key_old;
	
	RuleBasedCollator.CollationElement prefix =
	  (RuleBasedCollator.CollationElement) collator.prefix_tree.get (key);

	/*
	 * First case: There is no such sequence in the database.
	 * We will have to build one from the context.
	 */
	if (prefix == null)
	  {
	    /*
	     * We are dealing with sequences in an expansion. They
	     * are treated as accented characters (tertiary order).
	     */
	    if (alreadyExpanded > 0)
	      {
		RuleBasedCollator.CollationElement e =
		  collator.getDefaultAccentedElement (work_text.charAt (idx));
		
		a_element.add (e);
		a_idx.add (new Integer(idx_idx));
		idx++;
		alreadyExpanded--;
		if (alreadyExpanded == 0)
		  {
		    /* There is not any characters left in the expansion set.
		     * We can increase the pointer in the source string.
		     */
		    idx_idx += idxToMove;
		    idxToMove = 0; 
		  }
		else
		  idx_idx++;
	      }
	    else
	      {
		/* This is a normal character. */
		RuleBasedCollator.CollationElement e =
		  collator.getDefaultElement (work_text.charAt (idx));
		Integer i_ref = new Integer(idx_idx);

		/* Don't forget to mark it as a special sequence so the
		 * string can be ordered.
		 */
		a_element.add (RuleBasedCollator.SPECIAL_UNKNOWN_SEQ);
		a_idx.add (i_ref);
		a_element.add (e);
		a_idx.add (i_ref);
		idx_idx++;
		idx++;
	      }
	    continue;
	  }
 
	/*
	 * Second case: Here we have found a matching sequence.
	 * Here we have an expansion string prepend it to the "work text" and
	 * add the corresponding sorting element. We must also mark 
	 */
	if (prefix.expansion != null)
	  {
	    work_text = prefix.expansion
	      + work_text.substring (idx+prefix.key.length());
	    idx = 0;
	    a_element.add (prefix);
	    a_idx.add (new Integer(idx_idx));
	    if (alreadyExpanded == 0)
	      idxToMove = prefix.key.length();
	    alreadyExpanded += prefix.expansion.length()-prefix.key.length();
	  }
	else
	  {
	    /* Third case: the simplest. We have got the prefix and it
	     * has not to be expanded.
	     */
	    a_element.add (prefix);
	    a_idx.add (new Integer(idx_idx));
	    idx += prefix.key.length();
	    /* If the sequence is in an expansion, we must decrease the
	     * counter.
	     */
	    if (alreadyExpanded > 0)
	      {
		alreadyExpanded -= prefix.key.length();
		if (alreadyExpanded == 0)
		  {
		    idx_idx += idxToMove;
		    idxToMove = 0;
		  }
	      }
	    else
	      idx_idx += prefix.key.length();
	  }
      }
    
    text_decomposition = (RuleBasedCollator.CollationElement[])
	   a_element.toArray(new RuleBasedCollator.CollationElement[a_element.size()]);
    text_indexes = new int[a_idx.size()+1];
    for (int i = 0; i < a_idx.size(); i++) 
      {
	text_indexes[i] = ((Integer)a_idx.get(i)).intValue();
      }
    text_indexes[a_idx.size()] = text.length();
  }

  /**
   * This method sets the <code>String</code> that it is iterating over
   * to the <code>String</code> represented by the specified
   * <code>CharacterIterator</code>.
   *
   * @param source The <code>CharacterIterator</code> containing the new
   * <code>String</code> to iterate over.
   */
  public void setText(CharacterIterator source)
  {
    StringBuffer expand = new StringBuffer();

    // For now assume we read from the beginning of the string.
    for (char c = source.first();
	 c != CharacterIterator.DONE;
	 c = source.next())
      expand.append(c);

    setText(expand.toString());
  }

  /**
   * This method returns the current offset into the <code>String</code>
   * that is being iterated over.
   *
   * @return The iteration index position.
   *
   * @since 1.2
   */
  public int getOffset()
  {
    return textIndex;
  }

  /**
   * This method sets the iteration index position into the current
   * <code>String</code> to the specified value.  This value must not
   * be negative and must not be greater than the last index position
   * in the <code>String</code>.
   *
   * @param offset The new iteration index position.
   *
   * @exception IllegalArgumentException If the new offset is not valid.
   */
  public void setOffset(int offset)
  {
    if (offset < 0)
      throw new IllegalArgumentException("Negative offset: " + offset);

    if (offset > (text.length() - 1))
      throw new IllegalArgumentException("Offset too large: " + offset);
    
    for (index = 0; index < text_decomposition.length; index++)
      {	
	if (offset <= text_indexes[index])
	  break;
      }
    /*
     * As text_indexes[0] == 0, we should not have to take care whether index is
     * greater than 0. It is always.
     */
    if (text_indexes[index] == offset)
      textIndex = offset;
    else
      textIndex = text_indexes[index-1];
  }

  /**
   * This method returns the maximum length of any expansion sequence that
   * ends with the specified collation order value.  (Whatever that means).
   *
   * @param value The collation order value
   *
   * @return The maximum length of an expansion sequence.
   */
  public int getMaxExpansion(int value)
  {
    return 1;
  }
}
