/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt.event;
import java.awt.*;

/* A very incomplete placeholder. */

public class InputMethodEvent extends AWTEvent
{
  public static final int CARET_POSITION_CHANGED = 1101;
  public static final int INPUT_METHOD_FIRST = 1100;
  public static final int INPUT_METHOD_LAST = 1101;
  public static final int INPUT_METHOD_TEXT_CHANGED = 1100;

  /*
  public InputMethodEvent (Component source, int id,
			   AttributedCharacterIterator text,
			   int committedCharacterCount, TextHitInfo caret,
			   TextHitInfo visiblePosition)
  {
    if (id < INPUT_METHOD_FIRST
	|| id > INPUT_METHOD_LAST
	|| (id == CARET_POSITION_CHANGED && text != null)
	|| committedCharacterCount < 0
	|| (committedCharacterCount
	    > text.getEndIndex () - text.getBeginIndex ()))
      throw new IllegalArgumentException ();
  }

  public InputMethodEvent (Component source, int id, TextHitInfo caret,
			   TextHitInfo visiblePosition);

  public void consume ();
  public TextHitInfo getCaret ();
  public int getCommittedCharacterCount ();
  public AttributedCharacterIterator getText ();
  public TextHitInfo getVisiblePosition ();
  public boolean isConsumed ();

  public String paramString ()
  {
    String r;
    switch (id)
      {
        case CARET_POSITION_CHANGED:
	  r = "CARET_POSITION_CHANGED";
	break;
	case INPUT_METHOD_TEXT_CHANGED:
	  r = "INPUT_METHOD_TEXT_CHANGED";
	break;
      }
    r += ""; // FIXME
    return r;
  }
  */

  // FIXME: this is just to let it compile.
  private InputMethodEvent ()
  {
    super (null, -1);
  }
}
