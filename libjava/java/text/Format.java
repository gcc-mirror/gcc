/* Copyright (C) 1998, 1999, 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.text;

/**
 * @author Per Bothner <bothner@cygnus.com>
 * @date October 25, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

public abstract class Format implements java.io.Serializable, Cloneable
{
  public Format ()
  {
  }

  public abstract StringBuffer format (Object obj,
				       StringBuffer sbuf, FieldPosition pos);

  public final String format (Object obj)
  {
    StringBuffer sbuf = new StringBuffer();
    format(obj, sbuf, new FieldPosition(0));
    return sbuf.toString();
  }

  public abstract Object parseObject (String source, ParsePosition pos);

  public Object parseObject (String source) throws ParseException
  {
    ParsePosition pos = new ParsePosition(0);
    Object result = parseObject (source, pos);
    if (result == null)
      {
	int index = pos.getErrorIndex();
	if (index < 0)
	  index = pos.getIndex();
	throw new ParseException("parseObject failed", index);
      }
    return result;
  }

  public Object clone ()
  {
    return super.clone ();
  }
}
