/* Query.java -- Static methods for query construction.
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package javax.management;

/**
 * Provides static methods for constructing queries.  Queries
 * may be used to list and enumerate management beans, via
 * the {@link MBeanServer}.  By using the methods in this class,
 * complex queries can be created from their more basic
 * components. 
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class Query
{

  /**
   * A code representing the {@link #plus(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int PLUS = 0;

  /**
   * A code representing the {@link #minus(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int MINUS = 1;
  
  /**
   * A code representing the {@link #times(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int TIMES = 2;

  /**
   * A code representing the {@link #div(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int DIV = 3;

  /**
   * A code representing the {@link #gt(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int GT = 0;

  /**
   * A code representing the {@link #lt(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int LT = 1;

  /**
   * A code representing the {@link #ge(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int GE = 2;

  /**
   * A code representing the {@link #le(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int LE = 3;

  /**
   * A code representing the {@link #eq(ValueExp, ValueExp)
   * query to be used in serialization.
   */
  public static final int EQ = 4;

  /**
   * Returns a query expression formed from the conjunction
   * of the two supplied query expressions.
   *
   * @param q1 the first query expression.
   * @param q2 the second query expression.
   * @return a query expression representing q1 && q2.  This
   *         will be serialized as the non-public class
   *         {@link AndQueryExp}.
   */
  public static QueryExp and(QueryExp q1, QueryExp q2)
  {
    return new AndQueryExp(q1, q2);
  }

  /**
   * Returns a query expression which checks that an
   * attribute value held by the specified
   * {@link AttributeValueExp} contains the string
   * specified by the given {@link StringValueExp}.
   *
   * @param attrib the attribute to match.
   * @param string the substring to find.
   * @return a query expression representing
   *         <code>attrib.matches("*" + string + "*")</code>.
   *         This will be serialized as the non-public class
   *         {@link MatchQueryExp}.
   */
  public static QueryExp anySubString(AttributeValueExp attrib,
				      StringValueExp string)
  {
    return new MatchQueryExp(attrib, "*" + string.getValue() + "*");
  }

  /**
   * Returns a value expression for the value of the
   * named attribute.  Evaluating this using an
   * {@link ObjectName} involves an underlying call
   * to {@link MBeanServer#getAttribute(ObjectName,String)}.
   *
   * @param name the name of the attribute.
   * @return a value expression which returns the value
   *         of the named attribute when applied.
   */
  public static AttributeValueExp attr(String name)
  {
    return new AttributeValueExp(name);
  }

  /**
   * Returns a value expression for the value of the
   * named attribute from the specified class.  Evaluating
   * this using an {@link ObjectName} involves an underlying call
   * to both {@link MBeanServer#getObjectInstance(ObjectName)} and
   * {@link MBeanServer#getAttribute(ObjectName,String)}.
   *
   * @param className the class containing the attribute.
   * @param name the name of the attribute.
   * @return a value expression which returns the value
   *         of the named attribute when applied.
   *         This will be serialized as the non-public class
   *         {@link QualifiedAttributeValueExp}.
   */
  public static AttributeValueExp attr(String className,
				       String name)
  {
    return new QualifiedAttributeValueExp(className, name);
  }

  /**
   * Returns a query expression representing the constraint
   * that the value, <code>v1</code>, lies between <code>v2</code>
   * and <code>v3</code>.
   *
   * @param v1 the value to compare against the boundaries.
   * @param v2 the lower boundary.
   * @param v3 the upper boundary.
   * @return a query expression representing a comparison
   *         of <code>v1</code> against <code>v2</code>
   *         and <code>v3</code>.  It returns true if
   *         <code>v2 <= v1 <= v3</code>.  This
   *         will be serialized as the non-public class
   *         {@link BetweenQueryExp}.
   */
  public static QueryExp between(ValueExp v1, ValueExp v2,
				 ValueExp v3)
  {
    return new BetweenQueryExp(v1, v2, v3);
  }

  /**
   * Returns a value expression which evaluates to the name of
   * the class of the bean when applied. Associating the expression
   * with an {@link ObjectName} involves an underlying call
   * to both {@link MBeanServer#getObjectInstance(ObjectName)}
   * to obtain this information.
   *
   * @return a value expression which returns the class name
   *         of the bean to which it is applied.
   *         This will be serialized as the non-public class
   *         {@link ClassAttributeValueExp}.
   */
  public static AttributeValueExp classattr()
  {
    return new ClassAttributeValueExp();
  }

  /**
   * Returns a value expression which evaluates to the result of
   * dividing <code>v1</code> by <code>v2</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the division when applied.  This will be serialized
   *         as the non-public class {@link BinaryOpValueExp}
   *         with an operation of {@link #DIV}.
   */
  public static ValueExp div(ValueExp v1, ValueExp v2)
  {
    return new BinaryOpValueExp(DIV, v1, v2);
  }

  /**
   * Returns a query expression which evaluates to the result of
   * comparing <code>v1</code> to <code>v2</code> for equality. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the comparison when applied.  This will be serialized
   *         as the non-public class {@link BinaryRelQueryExp}
   *         with an operation of {@link #EQ}.
   */
  public static QueryExp eq(ValueExp v1, ValueExp v2)
  {
    return new BinaryRelQueryExp(EQ, v1, v2);
  }

  /**
   * Returns a query expression which checks that an
   * attribute value held by the specified
   * {@link AttributeValueExp} ends with the string
   * specified by the given {@link StringValueExp}.
   *
   * @param attrib the attribute to match.
   * @param string the substring to find.
   * @return a query expression representing
   *         <code>attrib.matches("*" + string)</code>.
   *         This will be serialized as the non-public class
   *         {@link MatchQueryExp}.
   */
  public static QueryExp finalSubString(AttributeValueExp attrib,
					StringValueExp string)
  {
    return new MatchQueryExp(attrib, "*" + string.getValue());
  }

  /**
   * Returns a query expression which evaluates to the result of
   * comparing <code>v1</code> to <code>v2</code> to see if
   * <code>v1</code> is greater than or equal to <code>v2</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the comparison when applied.  This will be serialized
   *         as the non-public class {@link BinaryRelQueryExp}
   *         with an operation of {@link #GE}.
   */
  public static QueryExp geq(ValueExp v1, ValueExp v2)
  {
    return new BinaryRelQueryExp(GE, v1, v2);
  }

  /**
   * Returns a query expression which evaluates to the result of
   * comparing <code>v1</code> to <code>v2</code> to see if
   * <code>v1</code> is greater than <code>v2</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the comparison when applied.  This will be serialized
   *         as the non-public class {@link BinaryRelQueryExp}
   *         with an operation of {@link #GT}.
   */
  public static QueryExp gt(ValueExp v1, ValueExp v2)
  {
    return new BinaryRelQueryExp(GT, v1, v2);
  }

  /**
   * Returns a query expression representing the constraint
   * that the value, <code>v</code>, is a member of the
   * list, <code>vlist</code>.
   *
   * @param v the value to look for in the list.
   * @param vlist the list of allowed values.
   * @return a query expression representing a membership check
   *         of <code>v</code> against the list, <code>vlist</code>.
   *         This will be serialized as the non-public class
   *         {@link InQueryExp}.
   */
  public static QueryExp in(ValueExp v, ValueExp[] vlist)
  {
    return new InQueryExp(v, vlist);
  }

  /**
   * Returns a query expression which checks that an
   * attribute value held by the specified
   * {@link AttributeValueExp} starts with the string
   * specified by the given {@link StringValueExp}.
   *
   * @param attrib the attribute to match.
   * @param string the substring to find.
   * @return a query expression representing
   *         <code>attrib.matches(string + "*")</code>.
   *         This will be serialized as the non-public class
   *         {@link MatchQueryExp}.
   */
  public static QueryExp initialSubString(AttributeValueExp attrib,
					  StringValueExp string)
  {
    return new MatchQueryExp(attrib, string.getValue() + "*");
  }

  /**
   * Returns a query expression which checks that a
   * bean is an instance of the class specified
   * by the given {@link StringValueExp}.  Associating the
   * expression with an {@link ObjectName} involves an underlying
   * call to {@link MBeanServer#isInstanceOf(ObjectName, String)}
   * using the value of <code>((StringValueExp)
   * className.apply(objectName)).getValue()</code> as the
   * class name.
   *
   * @param className the name of the class which the bean
   *                  should be an instance of.
   * @return a query expression representing
   *         the inheritance check.  This will be serialized
   *         as the non-public class {@link InstanceOfQueryExp}.
   * @since 1.6
   */
  public static QueryExp isInstanceOf(StringValueExp className)
  {
    return new InstanceOfQueryExp(className);
  }

  /**
   * Returns a query expression which evaluates to the result of
   * comparing <code>v1</code> to <code>v2</code> to see if
   * <code>v1</code> is less than or equal to <code>v2</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the comparison when applied.  This will be serialized
   *         as the non-public class {@link BinaryRelQueryExp}
   *         with an operation of {@link #LE}.
   */
  public static QueryExp leq(ValueExp v1, ValueExp v2)
  {
    return new BinaryRelQueryExp(LE, v1, v2);
  }

  /**
   * Returns a query expression which evaluates to the result of
   * comparing <code>v1</code> to <code>v2</code> to see if
   * <code>v1</code> is less than <code>v2</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the comparison when applied.  This will be serialized
   *         as the non-public class {@link BinaryRelQueryExp}
   *         with an operation of {@link #LT}.
   */
  public static QueryExp lt(ValueExp v1, ValueExp v2)
  {
    return new BinaryRelQueryExp(LT, v1, v2);
  }

  /**
   * <p>
   * Returns a query expression which checks that an
   * attribute value matches the pattern
   * specified by the given {@link StringValueExp}.
   * The pattern uses file-globbing syntax:
   * </p>
   * <ul>
   * <li>'*' stands for any number of arbitrary characters.</li>
   * <li>'?' stands for a single arbitrary characters.</li>
   * <li>An expression within '[' and ']' specify a character
   * class.</li>
   * <ul>
   * <li>A range of characters can be specified by separating
   * the start and end character with '-'.</li>
   * <li>The complement of the class can be obtained by using
   * '!' as the first character of the class.</li>
   * <li>'?', '*' and '[' can occur freely within the class. '-'
   * may occur as the first or last character.  '!' may occur
   * normally in any position other than the first.  ']' may occur
   * as the first element of the class.</li>
   * </ul>
   * <li>'?', '*' and '[' may be escaped using a backslash
   * character, '\'.</li>
   * </ul>
   *
   * @param attrib the attribute to match.
   * @param string the substring to find.
   * @return a query expression representing the result of
   *         matching the pattern against the evaluated
   *         value of the attribute.  This will be serialized
   *         as the non-public class {@link MatchQueryExp}.
   */
  public static QueryExp match(AttributeValueExp attrib,
			       StringValueExp string)
  {
    return new MatchQueryExp(attrib, string.getValue());
  }

  /**
   * Returns a value expression which evaluates to the result of
   * subtracting <code>v2</code> from <code>v1</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the subtraction when applied.  This will be serialized
   *         as the non-public class {@link BinaryOpValueExp}
   *         with an operation of {@link #MINUS}.
   */
  public static ValueExp minus(ValueExp v1, ValueExp v2)
  {
    return new BinaryOpValueExp(MINUS, v1, v2);
  }

  /**
   * Returns a query expression representing the negation
   * of the specified query expression.
   *
   * @param q the query to negate.
   * @return a query expression representing the negation of
   *         <code>q</code>.  This will be serialized as the
   *         non-public class {@link NotQueryExp}.
   */
  public static QueryExp not(QueryExp q)
  {
    return new NotQueryExp(q);
  }

  /**
   * Returns a query expression formed from the disjunction
   * of the two supplied query expressions.
   *
   * @param q1 the first query expression.
   * @param q2 the second query expression.
   * @return a query expression representing q1 || q2.  This
   *         will be serialized as the non-public class
   *         {@link OrQueryExp}.
   */
  public static QueryExp or(QueryExp q1, QueryExp q2)
  {
    return new OrQueryExp(q1, q2);
  }

  /**
   * Returns a value expression which evaluates to the result of
   * adding <code>v1</code> to <code>v2</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the addition when applied.  This will be serialized
   *         as the non-public class {@link BinaryOpValueExp}
   *         with an operation of {@link #PLUS}.
   */
  public static ValueExp plus(ValueExp v1, ValueExp v2)
  {
    return new BinaryOpValueExp(PLUS, v1, v2);
  }

  /**
   * Returns a value expression which evaluates to the result of
   * multiplying <code>v1</code> by <code>v2</code>. 
   *
   * @param v1 the left-hand operand.
   * @param v2 the right-hand operand.
   * @return a value expression which returns the result of
   *         the multiplication when applied.  This will be serialized
   *         as the non-public class {@link BinaryOpValueExp}
   *         with an operation of {@link #TIMES}.
   */
  public static ValueExp times(ValueExp v1, ValueExp v2)
  {
    return new BinaryOpValueExp(TIMES, v1, v2);
  }

  /**
   * Returns a value expression wrapping the specified value. 
   *
   * @param val the boolean value to wrap.
   * @return a value expression wrapping <code>val</code>.  This
   *         will be serialized as the non-public class
   *         {@link BooleanValueExp}.
   */
  public static ValueExp value(boolean val)
  {
    return new BooleanValueExp(val);
  }

  /**
   * Returns a value expression wrapping the specified value. 
   *
   * @param val the double value to wrap.
   * @return a value expression wrapping <code>val</code>.  This
   *         will be serialized as the non-public class
   *         {@link NumericValueExp}.
   */
  public static ValueExp value(double val)
  {
    return new NumericValueExp(val);
  }

  /**
   * Returns a value expression wrapping the specified value. 
   *
   * @param val the float value to wrap.
   * @return a value expression wrapping <code>val</code>.  This
   *         will be serialized as the non-public class
   *         {@link NumericValueExp}.
   */
  public static ValueExp value(float val)
  {
    return new NumericValueExp(val);
  }

  /**
   * Returns a value expression wrapping the specified value. 
   *
   * @param val the integer value to wrap.
   * @return a value expression wrapping <code>val</code>.  This
   *         will be serialized as the non-public class
   *         {@link NumericValueExp}.
   */
  public static ValueExp value(int val)
  {
    return new NumericValueExp(val);
  }

  /**
   * Returns a value expression wrapping the specified value. 
   *
   * @param val the long value to wrap.
   * @return a value expression wrapping <code>val</code>.  This
   *         will be serialized as the non-public class
   *         {@link NumericValueExp}.
   */
  public static ValueExp value(long val)
  {
    return new NumericValueExp(val);
  }

  /**
   * Returns a value expression wrapping the specified value. 
   *
   * @param val the {@link Number} value to wrap.
   * @return a value expression wrapping <code>val</code>.  This
   *         will be serialized as the non-public class
   *         {@link NumericValueExp}.
   */
  public static ValueExp value(Number val)
  {
    return new NumericValueExp(val);
  }

  /**
   * Returns a value expression wrapping the specified string. 
   *
   * @param val the {@link String} to wrap.
   * @return a {@link StringValueExp} wrapping <code>val</code>.
   */
  public static StringValueExp value(String val)
  {
    return new StringValueExp(val);
  }

  /**
   * Representation of the conjunction formed using
   * {@link #and(QueryExp, QueryExp).
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class AndQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -1081892073854801359L;
    
    /**
     * The first operand.
     */
    private QueryExp exp1;

    /**
     * The second operand.
     */
    private QueryExp exp2;

    /**
     * Constructs a new {@link AndQueryExp} using
     * the two specified operands.
     *
     * @param exp1 the first query expression.
     * @param exp2 the second query expression.
     */
    public AndQueryExp(QueryExp exp1, QueryExp exp2)
    {
      this.exp1 = exp1;
      this.exp2 = exp2;
    }

    /**
     * Returns the conjunction of the two query
     * expressions.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the conjunction of applying the name
     *         to both operands.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      return exp1.apply(name) && exp2.apply(name);
    }

  }

  /**
   * Representation of a query that matches an
   * attribute's value against a given pattern.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class MatchQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -7156603696948215014L;
    
    /**
     * The attribute to match against.
     */
    private AttributeValueExp exp;

    /**
     * The pattern to be matched.
     */
    private String pattern;

    /**
     * Constructs a new {@link MatchQueryExp} using
     * the specified attribute value and pattern.
     *
     * @param exp the attribute value expression.
     * @param pattern the pattern.
     */
    public MatchQueryExp(AttributeValueExp exp,
			 String pattern)
    {
      this.exp = exp;
      this.pattern = pattern;
    }

    /**
     * Returns the result of matching the attribute
     * value against the pattern.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the result of the match.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      String val = ((StringValueExp) exp.apply(name)).getValue();
      int valPos = 0;
      int fallback = -1;
      int fallbackP = -1;
      boolean backslash = false;
      for (int a = 0; a < pattern.length(); ++a)
	{
	  boolean matched = false;
	  int next = pattern.codePointAt(a);
	  if (!backslash)
	    {
	      if (next == '?' && valPos < val.length())
		{
		  ++valPos;
		  matched = true;
		}
	      else if (next == '*')
		{
		  fallback = valPos;
		  fallbackP = a;
		  matched = true;
		}
	      else if (next == '[' && valPos < val.length()) 
		{
		  boolean negated = false;
		  int b = a + 1;
		  int classChar = pattern.codePointAt(b);
		  do
		    {
		      if (classChar == '!' && b == a + 1)
			negated = true;
		      else if (pattern.codePointAt(b + 1) == '-' &&
			       pattern.codePointAt(b + 2) != ']')
			{
			  if (classChar > pattern.codePointAt(b + 2))
			    throw new BadStringOperationException("Invalid range: " +
								  classChar + " to " +
								  pattern.codePointAt(b+2));
			  for (int c = classChar; c <= pattern.codePointAt(b+2); ++c)
			    if (val.codePointAt(valPos) == c)
			      matched = true;
			  b = b + 2;
			}
		      else if (val.codePointAt(valPos) == classChar)
			matched = true;
		      ++b;
		      classChar = pattern.codePointAt(b);
		    } while (classChar != ']');
		  if (negated)
		    matched = !matched;
		  ++valPos;
		  a = b;
		}
	      else if (next == '\\')
		backslash = true;
	      else if (valPos < val.length() && next == val.codePointAt(valPos))
		{
		  matched = true;
		  ++valPos;
		}
	    }
	  else
	    {
	      backslash = false;
	      if (valPos < val.length() && next == val.codePointAt(valPos))
		{
		  matched = true;
		  ++valPos;
		}	
	    }
	  if (!matched)
	    if (fallback != -1)
	      {
		++fallback;
		valPos = fallback;
		a = fallbackP;
		if (valPos == val.length())
		  return false;
		continue;
	      }
	    else
	      return false;
	}
      return true;
    }
  }

  /**
   * Representation of the retrieval of an attribute
   * value from a certain class for {@link #attr(String,String)}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class QualifiedAttributeValueExp
    extends AttributeValueExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = 8832517277410933254L;

    /**
     * The name of the class from which the attribute is taken.
     */
    private String className;

    /**
     * Constructs a new {@link QualifiedAttributeValueExp} using
     * the specified class name and attribute name.
     *
     * @param className the class name.
     * @param name the attribute name.
     */
    public QualifiedAttributeValueExp(String className, String name)
    {
      super(name);
      this.className = className;
    }

    /**
     * Applies the {@link AttributeValueExp} to the specified
     * management bean by checking that the attribute will be
     * obtained from the correct class (by a class to
     * {@link MBeanServer#getObjectInstance(ObjectName)} and
     * then obtaining the attribute value from the
     * {@link MBeanServer}, using it to create a
     * {@link StringValueExp}.
     *
     * @param name the {@link ObjectName} of the bean to obtain
     *             the value from.
     * @return a {@link StringValueExp} containing the result.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the value expression.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the value expression.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the value expression.
     * @throws InvalidApplicationException if the value expression is applied
     *                                     to the wrong type of bean.
     */
    public ValueExp apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      try
	{
	  if (!(QueryEval.getMBeanServer().getObjectInstance(name).getClassName().equals(className)))
	    throw new BadAttributeValueExpException("The value is not from " +
						    "the correct class.");
	}
      catch (InstanceNotFoundException e)
	{
	  throw (BadAttributeValueExpException)
	    new BadAttributeValueExpException("The named bean is not registered.").initCause(e);
	}
      return super.apply(name);
    }
 
  }

  /**
   * Representation of the comparison of a value with
   * a pair of bounds formed using
   * {@link #between(ValueExp, ValueExp, ValueExp).
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class BetweenQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -2933597532866307444L;
    
    /**
     * The value to compare.
     */
    private ValueExp exp1;

    /**
     * The lower boundary.
     */
    private ValueExp exp2;

    /**
     * The upper boundary.
     */
    private ValueExp exp3;

    /**
     * Constructs a new {@link BetweenQueryExp} using
     * the specified comparison value and the given
     * bounds.
     *
     * @param exp1 the value to compare.
     * @param exp2 the lower bound.
     * @param exp3 the upper bound.
     */
    public BetweenQueryExp(ValueExp exp1, ValueExp exp2,
			   ValueExp exp3)
    {
      this.exp1 = exp1;
      this.exp2 = exp2;
      this.exp3 = exp3;
    }

    /**
     * Returns the result of the comparison between
     * the value and the two bounds.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the result of the comparison.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      String v1 = exp1.apply(name).toString();
      String v2 = exp2.apply(name).toString();
      String v3 = exp3.apply(name).toString();
      return v1.compareTo(v2) >= 0 && v1.compareTo(v3) <= 0;
    }

  }

  /**
   * Representation of the retrieval of the name of
   * a bean's class for {@link #classattr()}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class ClassAttributeValueExp
    extends AttributeValueExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -1081892073854801359L;

    /**
     * Obtains the name of the specified bean's class using a call
     * to {@link MBeanServer#getObjectInstance(ObjectName)}.
     *
     * @param name the {@link ObjectName} of the bean to obtain
     *             the class name from.
     * @return a {@link StringValueExp} containing the result.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the value expression.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the value expression.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the value expression.
     * @throws InvalidApplicationException if the value expression is applied
     *                                     to the wrong type of bean.
     */
    public ValueExp apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      try
	{
	  return new StringValueExp(QueryEval.getMBeanServer().getObjectInstance(name).getClassName());
	}
      catch (InstanceNotFoundException e)
	{
	  throw (BadAttributeValueExpException)
	    new BadAttributeValueExpException("The named bean is not registered.").initCause(e);
	}
    }
 
  }

  /**
   * Representation of a binary operation formed using
   * {@link #div(ValueExp, ValueExp), {@link #plus(ValueExp,ValueExp)},
   * {@link #minus(ValueExp, ValueExp) or
   * {@link #times(ValueExp, ValueExp)}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class BinaryOpValueExp
    extends QueryEval
    implements ValueExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = 1216286847881456786L;
    
    /**
     * The operation to perform.
     */
    private int op;

    /**
     * The left-hand operand.
     */
    private ValueExp exp1;

    /**
     * The right-hand operand.
     */
    private ValueExp exp2;

    /**
     * Constructs a new {@link BinaryOpValueExp} using
     * the specified operation and the two values supplied.
     *
     * @param op the operation to perform.
     * @param exp1 the left-hand operand.
     * @param exp2 the right-hand operand.
     */
    public BinaryOpValueExp(int op, ValueExp exp1, ValueExp exp2)
    {
      this.op = op;
      this.exp1 = exp1;
      this.exp2 = exp2;
    }

    /**
     * Returns the result of performing the operation on
     * <code>exp1</code> and <code>exp2</code>.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the result of the operation.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public ValueExp apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      NumericValueExp v1 = (NumericValueExp) exp1.apply(name);
      NumericValueExp v2 = (NumericValueExp) exp2.apply(name);
      switch (op)
	{
	case PLUS:
	  return v1.plus(v2);
	case MINUS:
	  return v1.minus(v2);
	case TIMES:
	  return v1.times(v2);
	case DIV:
	  return v1.div(v2);
	default:
	  throw new BadBinaryOpValueExpException(this);
	}
    }

    /**
     * Returns a textual representation of the operation.
     *
     * @return a textual version of the operation.
     */
    public String toString()
    {
      String opS;
      switch (op)
	{
	case PLUS:
	  opS = "+";
	  break;
	case MINUS:
	  opS = "-";
	  break;
	case TIMES:
	  opS = "x";
	  break;
	case DIV:
	  opS = "/";
	  break;
	default:
	  opS = "?";
	}
      return exp1 + " " + opS + " " + exp2;
    }
  }

  /**
   * Representation of a binary operation formed using
   * {@link #eq(ValueExp, ValueExp), {@link #geq(ValueExp, ValueExp)},
   * {@link #leq(ValueExp, ValueExp), {@link #gt(ValueExp, ValueExp)}
   * or {@link #lt(ValueExp, ValueExp)}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class BinaryRelQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -5690656271650491000L;
    
    /**
     * The operation to perform.
     */
    private int relOp;

    /**
     * The left-hand operand.
     */
    private ValueExp exp1;

    /**
     * The right-hand operand.
     */
    private ValueExp exp2;

    /**
     * Constructs a new {@link BinaryRelQueryExp} using
     * the specified operation and the two values supplied.
     *
     * @param relOp the operation to perform.
     * @param exp1 the left-hand operand.
     * @param exp2 the right-hand operand.
     */
    public BinaryRelQueryExp(int relOp, ValueExp exp1, ValueExp exp2)
    {
      this.relOp = relOp;
      this.exp1 = exp1;
      this.exp2 = exp2;
    }

    /**
     * Returns the result of performing the operation on
     * <code>exp1</code> and <code>exp2</code>.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the result of the comparison.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      String v1 = exp1.apply(name).toString();
      String v2 = exp2.apply(name).toString();
      switch (relOp)
	{
	case EQ:
	  return v1.equals(v2);
	case GT:
	  return v1.compareTo(v2) > 0;
	case GE:
	  return v1.compareTo(v2) >= 0;
	case LE:
	  return v1.compareTo(v2) <= 0;
	case LT:
	  return v1.compareTo(v2) < 0;
	default:
	  throw new BadStringOperationException("Invalid operator: " + relOp);
	}
    }

    /**
     * Returns a textual representation of the operation.
     *
     * @return a textual version of the operation.
     */
    public String toString()
    {
      String op;
      switch (relOp)
	{
	case EQ:
	  op = "=";
	  break;
	case GT:
	  op = ">";
	  break;
	case GE:
	  op = ">=";
	  break;
	case LE:
	  op = "<=";
	  break;
	case LT:
	  op = "<";
	  break;
	default:
	  op = "?";
	}
      return exp1 + " " + op + " " + exp2;
    }
  }

  /**
   * Representation of the comparison of a value with
   * the members of a list formed using
   * {@link #in(ValueExp, ValueExp[]).
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class InQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -5801329450358952434L;
    
    /**
     * The value to look for.
     */
    private ValueExp val;

    /**
     * The array to search.
     */
    private ValueExp[] valueList;

    /**
     * Constructs a new {@link InQueryExp} using
     * the specified comparison value and the given
     * list.
     *
     * @param val the value to compare.
     * @param valueList the list of values.
     */
    public InQueryExp(ValueExp val, ValueExp[] valueList)
    {
      this.val = val;
      this.valueList = valueList;
    }

    /**
     * Returns the result of the comparison between
     * the value and the list of allowed values.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the result of the comparison.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      String v = val.apply(name).toString();
      for (ValueExp vl : valueList)
	if (v.equals(vl.apply(name).toString()))
	  return true;
      return false;
    }

  }

  /**
   * Representation of the inheritance check on a
   * bean for {@link #isInstanceOf(StringValueExp)}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.6
   */
  private static final class InstanceOfQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -1081892073854801359L;

    /**
     * The name of the class from which the attribute is taken.
     */
    private StringValueExp classNameValue;

    /**
     * Constructs a new {@link InstanceOfQueryExp} using
     * the specified class name.
     *
     * @param classNameValue the class name.
     */
    public InstanceOfQueryExp(StringValueExp classNameValue)
    {
      this.classNameValue = classNameValue;
    }

    /**
     * Checks that the bean specified by the supplied
     * {@link ObjectName} is of the correct class 
     * using {@link MBeanServer#isInstanceOf(ObjectName,String)}.
     * where the string is obtained by evaluating
     * <code>classNameValue</code>.
     *
     * @param name the {@link ObjectName} of the bean to obtain
     *             the value from.
     * @return true if the bean is an instance of the class.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the value expression.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the value expression.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the value expression.
     * @throws InvalidApplicationException if the value expression is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      try
	{
	  String className = ((StringValueExp)
			      classNameValue.apply(name)).getValue();
	  return QueryEval.getMBeanServer().isInstanceOf(name, className);
	}
      catch (InstanceNotFoundException e)
	{
	  throw (BadAttributeValueExpException)
	    new BadAttributeValueExpException("The named bean is not registered.").initCause(e);
	}
    }
 
  }

  /**
   * Representation of the negation of a query formed using
   * {@link #not(QueryExp).
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class NotQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = 5269643775896723397L;
    
    /**
     * The expression to negate.
     */
    private QueryExp exp;

    /**
     * Constructs a new {@link NotQueryExp} using
     * the specified query expression.
     *
     * @param exp the expression to negate.
     */
    public NotQueryExp(QueryExp exp)
    {
      this.exp = exp;
    }

    /**
     * Returns the result of the negation.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the result of the negation.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      return !(exp.apply(name));
    }

  }

  /**
   * Representation of the disjunction formed using
   * {@link #or(QueryExp, QueryExp).
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class OrQueryExp
    extends QueryEval
    implements QueryExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = 2962973084421716523L;
    
    /**
     * The first operand.
     */
    private QueryExp exp1;

    /**
     * The second operand.
     */
    private QueryExp exp2;

    /**
     * Constructs a new {@link OrQueryExp} using
     * the two specified operands.
     *
     * @param exp1 the first query expression.
     * @param exp2 the second query expression.
     */
    public OrQueryExp(QueryExp exp1, QueryExp exp2)
    {
      this.exp1 = exp1;
      this.exp2 = exp2;
    }

    /**
     * Returns the disjunction of the two query
     * expressions.
     *
     * @param name the {@link ObjectName} to apply
     *             the query to.
     * @return the disjunction of applying the name
     *         to both operands.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the query.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the query.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the query.
     * @throws InvalidApplicationException if the query is applied
     *                                     to the wrong type of bean.
     */
    public boolean apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      return exp1.apply(name) || exp2.apply(name);
    }

  }

  /**
   * Representation of a boolean being used as an argument
   * to a relational constraint, formed using
   * {@link #value(boolean)}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class BooleanValueExp
    extends QueryEval
    implements ValueExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = 7754922052666594581L;

    /**
     * The boolean value.
     */
    private boolean val;
    
    /**
     * Constructs a new {@link BooleanValueExp} using the
     * specified value.
     *
     * @param val the boolean value used for this expression.
     */
    public BooleanValueExp(boolean val)
    {
      this.val = val;
    }

    /**
     * Applies the {@link BooleanValueExp} to the specified
     * management bean by simply returning the value.
     *
     * @param name the {@link ObjectName} of the bean.
     * @return the {@link BooleanValueExp} itself.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the value expression.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the value expression.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the value expression.
     * @throws InvalidApplicationException if the value expression is applied
     *                                     to the wrong type of bean.
     */
    public ValueExp apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      return this;
    }
    
    /**
     * Returns the value as a string.
     *
     * @return the value in textual form.
     */
    public String toString()
    {
      return Boolean.toString(val);
    }

  }

  /**
   * Representation of a number being used as an argument
   * to a relational constraint, formed using
   * {@link #value(double)}, {@link #value(float)},
   * {@link #value(int)}, {@link #value(long)} or
   * {@link #value(Number)}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class NumericValueExp
    extends QueryEval
    implements ValueExp
  {

    /**
     * Compatible with JDK 1.6
     */
    private static final long serialVersionUID = -4679739485102359104L;

    /**
     * The numeric value.
     */
    private Number val;
    
    /**
     * Constructs a new {@link NumericValueExp} using the
     * specified value.
     *
     * @param val the numeric value used for this expression.
     */
    public NumericValueExp(Number val)
    {
      this.val = val;
    }

    /**
     * Applies the {@link NumericValueExp} to the specified
     * management bean by simply returning the value.
     *
     * @param name the {@link ObjectName} of the bean.
     * @return the {@link NumericValueExp} itself.
     * @throws BadStringOperationException if an invalid string
     *                                     operation is used by
     *                                     the value expression.
     * @throws BadBinaryOpValueExpException if an invalid expression
     *                                      is used by the value expression.
     * @throws BadAttributeValueExpException if an invalid attribute
     *                                       is used by the value expression.
     * @throws InvalidApplicationException if the value expression is applied
     *                                     to the wrong type of bean.
     */
    public ValueExp apply(ObjectName name)
      throws BadStringOperationException, BadBinaryOpValueExpException,
	     BadAttributeValueExpException, InvalidApplicationException
    {
      return this;
    }
    
    /**
     * Returns the value.
     */
    public Number getValue()
    {
      return val;
    }

    /**
     * Returns the value as a string.
     *
     * @return the value in textual form.
     */
    public String toString()
    {
      return val.toString();
    }

    /**
     * Return the result of adding the specified
     * {@link NumericValueExp} to this one.
     *
     * @param o the value to add.
     * @return the result of the addition.
     */
    public NumericValueExp plus(NumericValueExp o)
    {
      Number v = o.getValue();
      if (val instanceof Double)
	{
	  double d = val.doubleValue();
	  if (v instanceof Double)
	    return new NumericValueExp(d + v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(d + v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(d + v.longValue());
	  else
	    return new NumericValueExp(d + v.intValue());
	}
      else if (val instanceof Float)
	{
	  float f = val.floatValue();
	  if (v instanceof Double)
	    return new NumericValueExp(f + v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(f + v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(f + v.longValue());
	  else
	    return new NumericValueExp(f + v.intValue());
	}
      else if (val instanceof Long)
	{
	  long l = val.longValue();
	  if (v instanceof Double)
	    return new NumericValueExp(l + v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(l + v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(l + v.longValue());
	  else
	    return new NumericValueExp(l + v.intValue());
	}
      int i = val.intValue();
      if (v instanceof Double)
	return new NumericValueExp(i + v.doubleValue());
      else if (v instanceof Float)
	return new NumericValueExp(i + v.floatValue());
      else if (v instanceof Long)
	return new NumericValueExp(i + v.longValue());
      else
	return new NumericValueExp(i + v.intValue());   
    }   

    /**
     * Return New NumericValueExp(the result of subtracting the specified
     * {@link NumericValueExp} from this one.
     *
     * @param o the value to subtract.
     * @return new NumericValueExp(the result of the subtraction.
     */
    public NumericValueExp minus(NumericValueExp o)
    {
      Number v = o.getValue();
      if (val instanceof Double)
	{
	  double d = val.doubleValue();
	  if (v instanceof Double)
	    return new NumericValueExp(d - v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(d - v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(d - v.longValue());
	  else
	    return new NumericValueExp(d - v.intValue());
	}
      else if (val instanceof Float)
	{
	  float f = val.floatValue();
	  if (v instanceof Double)
	    return new NumericValueExp(f - v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(f - v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(f - v.longValue());
	  else
	    return new NumericValueExp(f - v.intValue());
	}
      else if (val instanceof Long)
	{
	  long l = val.longValue();
	  if (v instanceof Double)
	    return new NumericValueExp(l - v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(l - v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(l - v.longValue());
	  else
	    return new NumericValueExp(l - v.intValue());
	}
      int i = val.intValue();
      if (v instanceof Double)
	return new NumericValueExp(i - v.doubleValue());
      else if (v instanceof Float)
	return new NumericValueExp(i - v.floatValue());
      else if (v instanceof Long)
	return new NumericValueExp(i - v.longValue());
      else
	return new NumericValueExp(i - v.intValue());   
    }   

    /**
     * Return New NumericValueExp(the result of multiplying the specified
     * {@link NumericValueExp} to this one.
     *
     * @param o the value to multiply by.
     * @return new NumericValueExp(the result of the multiplication.
     */
    public NumericValueExp times(NumericValueExp o)
    {
      Number v = o.getValue();
      if (val instanceof Double)
	{
	  double d = val.doubleValue();
	  if (v instanceof Double)
	    return new NumericValueExp(d * v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(d * v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(d * v.longValue());
	  else
	    return new NumericValueExp(d * v.intValue());
	}
      else if (val instanceof Float)
	{
	  float f = val.floatValue();
	  if (v instanceof Double)
	    return new NumericValueExp(f * v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(f * v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(f * v.longValue());
	  else
	    return new NumericValueExp(f * v.intValue());
	}
      else if (val instanceof Long)
	{
	  long l = val.longValue();
	  if (v instanceof Double)
	    return new NumericValueExp(l * v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(l * v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(l * v.longValue());
	  else
	    return new NumericValueExp(l * v.intValue());
	}
      int i = val.intValue();
      if (v instanceof Double)
	return new NumericValueExp(i * v.doubleValue());
      else if (v instanceof Float)
	return new NumericValueExp(i * v.floatValue());
      else if (v instanceof Long)
	return new NumericValueExp(i * v.longValue());
      else
	return new NumericValueExp(i * v.intValue());   
    }   

    /**
     * Return New NumericValueExp(the result of dividing this
     * number by value of the specified
     * {@link NumericValueExp}.
     *
     * @param o the value to divide by.
     * @return new NumericValueExp(the result of the division.
     */
    public NumericValueExp div(NumericValueExp o)
    {
      Number v = o.getValue();
      if (val instanceof Double)
	{
	  double d = val.doubleValue();
	  if (v instanceof Double)
	    return new NumericValueExp(d / v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(d / v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(d / v.longValue());
	  else
	    return new NumericValueExp(d / v.intValue());
	}
      else if (val instanceof Float)
	{
	  float f = val.floatValue();
	  if (v instanceof Double)
	    return new NumericValueExp(f / v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(f / v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(f / v.longValue());
	  else
	    return new NumericValueExp(f / v.intValue());
	}
      else if (val instanceof Long)
	{
	  long l = val.longValue();
	  if (v instanceof Double)
	    return new NumericValueExp(l / v.doubleValue());
	  else if (v instanceof Float)
	    return new NumericValueExp(l / v.floatValue());
	  else if (v instanceof Long)
	    return new NumericValueExp(l / v.longValue());
	  else
	    return new NumericValueExp(l / v.intValue());
	}
      int i = val.intValue();
      if (v instanceof Double)
	return new NumericValueExp(i / v.doubleValue());
      else if (v instanceof Float)
	return new NumericValueExp(i / v.floatValue());
      else if (v instanceof Long)
	return new NumericValueExp(i / v.longValue());
      else
	return new NumericValueExp(i / v.intValue());   
    }   

  }

}

