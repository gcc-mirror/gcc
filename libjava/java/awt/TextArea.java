/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* A very incomplete placeholder. */

public class TextArea extends TextComponent
{
  public synchronized void append (String str)
  {
    replaceRange(str, length, length);
  }

  public synchronized void insert (String str, int pos)
  {
    replaceRange(str, pos, pos);
  }

  public synchronized void replaceRange (String str, int start, int end)
  {
    if (length == 0)
      setText (str);
    else
      {
	int len = str.length();
	int delta = len - (end - start);
	int new_length = length + delta;
	if (buffer.length < new_length)
	  {
	    int new_size = 2 * buffer.length;
	    if (new_size < new_length)
	      new_size = new_length;
	    char[] new_buffer = new char[new_size];
	    System.arraycopy(buffer, 0, new_buffer, 0, length);
	    buffer = new_buffer;
	  }
	if (len != end)
	  System.arraycopy(buffer, start, buffer, start + len, len - end);
	str.getChars(0, len, buffer, start);
	length += delta;
      }
  }
}
