/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */
 
/**
 * @author Andrew Haley <aph@cygnus.com>
 * @date September 18, 1998.  
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */
 
package java.lang;

import java.util.Random;

public final class Math 
{
  private static Random random_;

  public static final double E  = 2.7182818284590452354;
  public static final double PI = 3.14159265358979323846;

  public static native double sin (double x);

  public static native double cos (double x);

  public static native double tan (double x);

  public static native double asin (double x);

  public static native double acos (double x);

  public static native double atan (double x);

  public static native double atan2(double y, double x);

  public static native double exp (double x);

  public static native double log (double x);

  public static native double sqrt (double x);

  public static native double pow (double x, double y);

  public static native double IEEEremainder (double x, double y);

  public static native double ceil (double x);

  public static native double floor (double x);

  public static native double rint (double x);

  public static native int round (float x);

  public static native long round (double x);
  
  public static synchronized double random ()
  {
    if (random_ == null)
      random_ = new Random ();
    return random_.nextDouble ();
  }

  public static int abs (int n)
  {
    return (n < 0 ? -n : n);
  }

  public static long abs (long n)
  {
    return (n < 0 ? -n : n);
  }

  public static native float abs (float x);

  public static native double abs (double x);

  public static int min (int a, int b)
  {
    return (a < b ? a : b);
  }

  public static long min (long a, long b)
  {
    return (a < b ? a : b);
  }

  public static native float min (float a, float b);

  public static native double min (double a, double b);

  public static int max (int a, int b)
  {
    return (a < b ? b : a);
  }

  public static long max (long a, long b)
  {
    return (a < b ? b : a);
  }

  public static native float max (float a, float b);

  public static native double max (double a, double b);

  // Don't allow objects to be made.
  private Math ()
  {
  }
}

