/*
 * Javolution - Java(TM) Solution for Real-Time and Embedded Systems
 * Copyright (C) 2006 - Javolution (http://javolution.org/)
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software is
 * freely granted, provided that this notice is preserved.
 */

public final class PR31264 
{
  public static long fubar(double d, int n) 
  {
    long bits = Double.doubleToRawLongBits(d);
    int exp = ((int)(bits >> 52)) & 0x7FF;
    long m = bits & 0x000fffffffffffffL;
    if (exp == 0) 
      {
	if (m == 0) return 0L;
	return fubar(d * 18014398509481984L, n - 54); // 2^54 Exact.
      }
    return m;
  }

  public static void main(String[] argv)
  {
  }
}
