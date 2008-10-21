/* gnu_java_math_GMP.c -- Native MPI implemenetation over GNU MP
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "gnu_java_math_GMP.h"
#include <jcl.h>

#if defined(HAVE_GMP_H)
#include <gmp.h>
#endif /* defined(HAVE_GMP_H) */

#if defined(WITH_GNU_MP)
#else
#warning GNU MP not available or wanted!
#warning Invocation of natXXX() methods will raise an exception
#endif

/*
 * This is the implementation of the native BigInteger$NativeMPI methods which
 * use the GNU MP (GMP) library.
 *
 * In all the Java non-statically declared native methods, the second argument,
 * an instance of jobject, refers to the current NativeMPI instance; i.e.
 * "this" in Java parlance. The corresponding allocated data structure
 * representing the GMP's counter-part is pointed-to by the pointer stored in a
 * transient java field named "native_ptr". The first thing these methods do is
 * (a) access that field with a JNI GetObjectField() call to obtain the
 * reference to the gnu.classpath.Pointer subclass instance, then (b) obtain
 * the native GMP pointer (an mpz_ptr), in order to perform their job; e.g.
 *
 *   JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr))
 *
 * For static methods the second argument, an instance of jclass, is almost
 * always unused --except when initializing the library, which is the only time
 * we get hold of the native pointer field.
 *
 * This code was written and tested with GNU MP version 4.1.2. More recent
 * versions of that library offer more operations; e.g. in the implementation
 * of the method Java_java_math_BigInteger_00024NativeMPI_natFlipBit, mpz_combit()
 * should be used with GNU MP versions later than 4.2. As a consequence, this
 * code should be reviewed, from time to time, to use newer features of the GNU
 * MP library.
 */

static jfieldID native_ptr;

#ifdef DEBUG
#define TRACE(msg) fprintf (stderr, "%s(%s:%d) -- %s\n", __FUNCTION__, __FILE__, __LINE__, msg)
#else
#define TRACE(msg)
#endif

#define throw_config_exception(env) JCL_ThrowException (env, "java/lang/Error", "GNU MP was not specified/found by configure")

/*
 * Initialize the native library. Specifically this method:
 *
 * a. Pass NULLs to the mp_set_memory_functions implying that GMP should use
 *    malloc, realloc and free for memory allocation, and if they fail GMP
 *    will print a message to the standard error output and terminates the
 *    program.
 * b. Find out and store the reference to the NativeMPI class's 'native_ptr'
 *    field. This is done so later in the method invocations we can access that
 *    field and acquire the native value associated with that Pointer instance.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natInitializeLibrary (JNIEnv *env,
                                                               jclass nativeMPI)
{
#if defined(WITH_GNU_MP)
  TRACE("Loading GMP-based BigInteger native library");
  mp_set_memory_functions (NULL, NULL, NULL);
  native_ptr = (*env)->GetFieldID (env, nativeMPI, "native_ptr",
                                   "Lgnu/classpath/Pointer;");
  TRACE("Loaded GMP-based BigInteger native library");
#else /* !defined(WITH_GNU_MP) */
  (void) nativeMPI;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * Allocate and initialize the data structure for an instance of a NativeMPI.
 *
 * @param this  an instance of NativeMPI.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natInitialize(JNIEnv *env, jobject this)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this;

  TRACE("begin");
  _this = (mpz_ptr)JCL_malloc (env, sizeof (mpz_t));
  /* initialize --GMP sets the value to zero. */
  mpz_init (_this);
  /* instantiate the Pointer instance for this NativeMPI. */
  jobject native_ptr_fld = JCL_NewRawDataObject (env, _this);
  /* ... and assign it to the native_ptr field. */
  (*env)->SetObjectField (env, this, native_ptr, native_ptr_fld);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * Clear and free the data structure for an instance of a NativeMPI.
 *
 * @param this  an instance of NativeMPI.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natFinalize(JNIEnv *env, jobject this)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this;

  TRACE("begin");
  _this = (mpz_ptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  if (_this != NULL)
    {
      mpz_clear (_this);
      free (_this);
      _this = NULL;
    }
  else
    {
      TRACE("WARNING: Already cleared + freed");
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}


/*
 * @param this  an instance of NativeMPI. On exit, this will have a value of n.
 * @param n  a Java long primitive value.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natFromLong(JNIEnv *env, jobject this,
                                                     jlong n)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this;

  TRACE("begin");
  _this = (mpz_ptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  /* the size of jlong (64-bit) is either the same as a long, or that of a long long.
   * if it's the former, we use as is. */
  if (sizeof (jlong) == sizeof (long))
    {
      mpz_set_si (_this, (signed long)n);
    }
  else
    {
      /* ...otherwise, we operate on the two halves of the long long, each half
       * being 32-bit wide.  for simplicity, we work with positive
       * values negating, if necessary, the final outcome.
       */
      const int isnegative = n < 0 ? 1 : 0;
      if (isnegative)
        {
          n = -n;
        }
      mpz_set_ui (_this, (unsigned long)(((unsigned long long)n) >> 32));
      mpz_mul_2exp (_this, _this, 32); /* shift left by 32 bits */
      mpz_add_ui (_this, _this, (unsigned long)n);
      if (isnegative)
        {
          mpz_neg (_this, _this);
        }
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) n;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI. On exit, this will have the same
 *           value as x.
 * @param x  an instance of a NativeMPI's Pointer.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natFromBI(JNIEnv *env, jobject this,
                                                   jobject x)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this;
  mpz_srcptr _x;

  TRACE("begin");
  _this = (mpz_ptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  mpz_set (_this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI. On exit, this will have the value
 *           represented by the v Java byte array (in 2's complement with most
 *           significant byte at index position 0). The sign is implied by the
 *           value of the most significant byte.
 * @param v  a Java byte array containing the byte representation, in 2's
 *           complement of the signed value to assign to this.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natFromByteArray(JNIEnv *env,
                                                          jobject this,
                                                          jbyteArray v)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this;
  jbyte *_v;
  unsigned long b;
  int vlength, isnegative, i;

  TRACE("begin");
  _this = (mpz_ptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _v = (*env)->GetByteArrayElements (env, v, NULL);
  vlength = (*env)->GetArrayLength (env, v);
  b = (unsigned long)(_v[0] & 0xFF);
  isnegative = (b > 0x7F) ? 1 : 0;
  mpz_set_ui (_this, 0);
  for (i = 0; i < vlength; i++)
    {
      mpz_mul_2exp (_this, _this, 8); /* shift left 8 positions. */
      b = (unsigned long)(_v[i] & 0xFF);
      b = (isnegative) ? ~b : b;
      mpz_add_ui (_this, _this, (unsigned long)(b & 0xFF));
    }
  (*env)->ReleaseByteArrayElements (env, v, _v, JNI_ABORT);
  if (isnegative)
    {
      mpz_add_ui (_this, _this, 1);
      mpz_neg (_this, _this);
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) v;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI. On exit, this will have the value
 *           represented by the s Java string.
 * @param s  a Java string containing, a possibly signed, value to assign to
 *           this.
 * @param rdx  the base in which the symbols, in s, are represented.
 * @return  0 if the entire string is a valid number in base rdx. Otherwise it
 *          returns -1.
 *
 * Implementation note:
 * While the GMP library is more tolerant in what it accepts as parameter values
 * for conversions from strings, the BigInteger code, which calls this method,
 * ensures that the contract described in the RI's documentation is respected;
 * e.g. no white spaces in the middle, limited valid radix values, etc...
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natFromString(JNIEnv *env,
                                                       jobject this, jstring s,
                                                       jint rdx)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this;
  const char *_s;
  int result;

  TRACE("begin");
  _this = (mpz_ptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _s = (*env)->GetStringUTFChars (env, s, NULL);
  result = mpz_set_str (_this, _s, (int)rdx);
  JCL_free_cstring (env, s, _s);
  TRACE("end");
  return (result);
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) s;
  (void) rdx;
  throw_config_exception(env);
  return (-1);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI. On exit, this will have the value
 *           represented by the m Java byte array (most significant byte at
 *           index position 0). It will be positive, or negative, depending on
 *           the value of isnegative.
 * @param m  a Java byte array containing the byte representation of the
 *           absolute value (most significant byte being at index position 0)
 *           to assign to this.
 * @param isnegative  true if this should be negative and false if it should
 *           be positive.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natFromSignedMagnitude(JNIEnv *env,
                                                                jobject this,
                                                                jbyteArray m,
                                                                jboolean isnegative)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this;
  jbyte *_m;
  int mlength, i;

  TRACE("begin");
  _this = (mpz_ptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _m = (*env)->GetByteArrayElements (env, m, NULL);
  mlength = (*env)->GetArrayLength (env, m);
  mpz_set_ui (_this, 0);
  for (i = 0; i < mlength; i++)
    {
      mpz_mul_2exp (_this, _this, 8);
      mpz_add_ui (_this, _this, (unsigned long)(_m[i] & 0xFF));
    }
  (*env)->ReleaseByteArrayElements (env, m, _m, JNI_ABORT);
  if (isnegative == JNI_TRUE)
    {
      mpz_neg (_this, _this);
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) m;
  (void) isnegative;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  the base in which to represent this.
 * @return  the Java string representing the value of this in base n.
 */
JNIEXPORT jstring JNICALL
Java_gnu_java_math_GMP_natToString(JNIEnv *env, jobject this,
                                                     jint n)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  char *cstr;
  jstring result;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  cstr = mpz_get_str (NULL, (int)n, _this);
  result  = (*env)->NewStringUTF (env, cstr);
  free (cstr);
  TRACE("end");
  return (result);
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  throw_config_exception(env);
  return (NULL);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  a non-ZERO instance of NativeMPI.
 *
 * output:
 * @param r  a Java byte array which shall contain the byte representation of
 *           this.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natToByteArray(JNIEnv *env,
                                                        jobject this,
                                                        jbyteArray r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_t _w;  /* a temporary work mpi */
  jbyte *_r;
  int rlength, sign, i;
  unsigned long b;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (*env)->GetByteArrayElements (env, r, NULL);
  rlength = (*env)->GetArrayLength (env, r);
  mpz_init (_w);
  /* if this is negative set w to its 2's complement otherwise use as is. */
  sign = mpz_sgn (_this);
  if (sign == 1)
    {
      mpz_set (_w, _this);
    }
  else
    {
      mpz_neg (_w, _this);
      mpz_sub_ui (_w, _w, 1);
    }
  /* _w SHOULD be >= 0.
   * start filling the array starting from the least significant byte. */
  for (i = rlength; --i >= 0; )
    {
      b = mpz_tdiv_q_ui (_w, _w, 256);
      b = (sign == -1) ? ~b : b;
      _r[i] = (unsigned long)(b & 0xFF);
    }
  (*env)->ReleaseByteArrayElements (env, r, _r, JNI_COMMIT);
  /* if _w > 0 the byte array was short. */
  if (mpz_cmp_ui (_w, 0) > 0)
    {
      TRACE("WARNING: byte array is too short");
    }
  mpz_clear (_w);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @return  the "int" value (least significant 32 bits) of the absolute value
 *          of this NativeMPI. The calling code MUST handle the sign. We do
 *          this so we can use the same method when computing the "long" value
 *          as well.
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natAbsIntValue(JNIEnv *env,
                                                        jobject this)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  TRACE("end");
  return ((jint)mpz_get_ui (_this));
#else /* !defined(WITH_GNU_MP) */
  (void) s;
  throw_config_exception(env);
  return (-1);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @return  the, eventually truncated, double value of this NativeMPI.
 */
JNIEXPORT jdouble JNICALL
Java_gnu_java_math_GMP_natDoubleValue(JNIEnv *env,
                                                        jobject this)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  TRACE("end");
  return ((jdouble)mpz_get_d (_this));
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  throw_config_exception(env);
  return (0.0);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  a NativeMPI instance.
 * @param x  an instance of NativeMPI's Pointer.
 * @return  -1, 0, +1 if x is respectively less than, equal to, or greater
 *          than y.
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natCompare(JNIEnv *env, jobject this,
                                                    jobject x)
{
#if defined(WITH_GNU_MP)
  mpz_ptr _this, _x;
  int res;

  TRACE("begin");
  _this = (mpz_ptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_ptr)JCL_GetRawData (env, x);
  res = mpz_cmp (_this, _x);
  TRACE("end");
  if (res == 0)
    return ((jint)0);
  else if (res < 0)
    return ((jint)-1);
  else
    return ((jint)1);
#else /* !defined(WITH_GNU_MP) */
  (void) x;
  (void) y;
  throw_config_exception(env);
  return (0);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this + x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natAdd(JNIEnv *env, jobject this,
                                                jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_add (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this - x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natSubtract(JNIEnv *env, jobject this,
                                                     jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_sub (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this * x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natMultiply(JNIEnv *env, jobject this,
                                                     jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_mul (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this div x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natQuotient(JNIEnv *env, jobject this,
                                                     jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_tdiv_q (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this mod x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natRemainder(JNIEnv *env, jobject this,
                                                      jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_tdiv_r (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param q  a NativeMPI's Pointer such that q = this div x.
 * @param r  a NativeMPI's Pointer such that r = this mod x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natQuotientAndRemainder(JNIEnv *env,
                                                                 jobject this,
                                                                 jobject x,
                                                                 jobject q,
                                                                 jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _q, _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _q = (mpz_ptr)JCL_GetRawData (env, q);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_tdiv_qr (_q, _r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) q;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this mod x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natModulo(JNIEnv *env, jobject this,
                                                   jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_mod (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  a non-negative number to raise this to.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this ** n.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natPow(JNIEnv *env, jobject this,
                                                jint n, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_pow_ui (_r, _this, (unsigned long)n);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param e  an instance of NativeMPI's Pointer.
 * @param m  another instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = (this**e) mod m.
 *
 * @throws java.lang.ArithmeticException if e is negative and (1 / this) mod m
 *         has no multiplicative inverse.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natModPow(JNIEnv *env, jobject this,
                                                   jobject e, jobject m,
                                                   jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _e, _m;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _e = (mpz_srcptr)JCL_GetRawData (env, e);
  _m = (mpz_srcptr)JCL_GetRawData (env, m);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  /* the documentation of mpz_powm(rop, b, e, m) states that it: "Set rop to
   * (b raised to e) modulo m.  Negative e is supported if an inverse b^-1 mod m
   * exists.... If an inverse doesn't exist then a divide by zero is raised."
   * to work around this case we use the same code as in the pure java class;
   * i.e.:
   *     if (e.isNegative())
   *       return this.modInverse(m).modPow(e.negate(), m); */
  if (mpz_sgn (_e) == -1)
    {
      mpz_t _w;  /* a temporary work mpi */
      const int res = mpz_invert (_r, _this, _m);
      if (res == 0)
        {
          JCL_ThrowException (env, "java/lang/ArithmeticException",
                              "No multiplicative inverse modulo the designated number exists");
        }
      mpz_init (_w);
      mpz_neg (_w, _e);
      mpz_powm (_r, _r, _w, _m);
      mpz_clear (_w);
    }
  else
    {
      mpz_powm (_r, _this, _e, _m);
    }

  while (mpz_sgn (_r) == -1)
    {
      mpz_add (_r, _r, _m);
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) e;
  (void) m;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param m  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = (1 / this) mod m.
 * @throws java.lang.ArithmeticException if (1 / this) mod m has no
 *         multiplicative inverse.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natModInverse(JNIEnv *env,
                                                       jobject this,
                                                       jobject m, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _m;
  mpz_ptr _r;
  int res;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _m = (mpz_srcptr)JCL_GetRawData (env, m);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  res = mpz_invert (_r, _this, _m);
  if (res == 0)
    {
      JCL_ThrowException (env, "java/lang/ArithmeticException",
                          "No multiplicative inverse modulo the designated number exists");
    }

  while (mpz_sgn (_r) == -1)
    {
      mpz_add (_r, _r, _m);
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r is the GCD of this and x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natGCD(JNIEnv *env, jobject this,
                                                jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_gcd (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  number of Miller-Rabin tests to conduct.
 * @return  2 if this is definitely prime. Returns 1 if this is probably prime
 *          (without being certain). Finally, returns 0 if this is definitely
 *          composite.
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natTestPrimality(JNIEnv *env,
                                                          jobject this, jint n)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  TRACE("end");
  return ((jint)mpz_probab_prime_p (_this, (int)n));
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  throw_config_exception(env);
  return (0);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  the non-negative number of positions to shift right this by.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this * 2**n.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natShiftLeft(JNIEnv *env, jobject this,
                                                      jint n, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_mul_2exp (_r, _this, (unsigned long)n);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  the non-negative number of positions to shift left this by.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = floor(this / 2**n).
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natShiftRight(JNIEnv *env,
                                                       jobject this, jint n,
                                                       jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_fdiv_q_2exp (_r, _this, (unsigned long)n);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @return  the 0-based index of the lowest significant bit set (to 1) in this.
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natLowestSetBit(JNIEnv *env,
                                                         jobject this)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  TRACE("end");
  return ((jint)mpz_scan1 (_this, 0));
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  throw_config_exception(env);
  return (-1);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = abs(x).
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natAbs(JNIEnv *env, jobject this,
                                                jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_abs (_r, _this);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = -x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natNegate(JNIEnv *env, jobject this,
                                                   jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_neg (_r, _this);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @return  the number of bits needed to represent this.
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natBitLength(JNIEnv *env, jobject this)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  TRACE("end");
  return ((jint)mpz_sizeinbase (_this, 2));
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  throw_config_exception(env);
  return (-1);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI. It MUST be >= ZERO.
 * @return  the number of bits set (to 1) in this.
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natSetBitCount(JNIEnv *env,
                                                        jobject this)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _bi;
  unsigned long res = 0;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  switch (mpz_sgn (_this))
    {
      case -1:
        /* initialize --GMP sets the value to zero. */
        _bi = (mpz_ptr)JCL_malloc (env, sizeof (mpz_t));
        mpz_init (_bi);
        mpz_neg (_bi, _this);
        res = mpz_popcount (_bi);
        mpz_clear (_bi);
        free (_bi);
        break;
      case 0:
        res = 0;
        break;
      case 1:
        res = mpz_popcount (_this);
      default:
        JCL_ThrowException (env, "java/lang/Error",
                            "Unexpected sign value for a native MPI");
    }
  TRACE("end");
  return ((jint)res);
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  throw_config_exception(env);
  return (ULONG_MAX);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this ^ x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natXor(JNIEnv *env, jobject this,
                                                jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_xor (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this | x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natOr(JNIEnv *env, jobject this,
                                               jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_ior (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this & x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natAnd(JNIEnv *env, jobject this,
                                                jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_and (_r, _this, _x);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param x  an instance of NativeMPI's Pointer.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = this & ~x.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natAndNot(JNIEnv *env, jobject this,
                                                   jobject x, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this, _x;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _x = (mpz_srcptr)JCL_GetRawData (env, x);
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_com (_r, _x);
  mpz_and (_r, _this, _r);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) x;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  the 0-based index position of the bit to flip in this. n MUST be
 *           greater than, or equal to 0.
 *
 * output:
 * @param r  a copy of this NativeMPI's Pointer with its n-th bit flipped.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natFlipBit(JNIEnv *env, jobject this,
                                                    jint n, jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_set (_r, _this);
  /* GNU MP versions earlier than 4.2 do not define this method:
   *   mpz_combit (_r, (unsigned long)n); */
  if (mpz_tstbit (_r, (unsigned long)n) == 1)
    {
      mpz_clrbit (_r, (unsigned long)n);
    }
  else
    {
      mpz_setbit (_r, (unsigned long)n);
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  the 0-based index position of the bit to test in this. n MUST be
 *           greater than, or equal to 0.
 * @return  +1, or -1 depending on whether the n-th bit in this is set or not
 *          respectively.
 */
JNIEXPORT jint JNICALL
Java_gnu_java_math_GMP_natTestBit(JNIEnv *env, jobject this,
                                                    jint n)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  TRACE("end");
  return ((jint)mpz_tstbit (_this, (unsigned long)n));
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  throw_config_exception(env);
  return (-1);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 * @param n  the 0-based index position of the bit to set, or clear, in this.
 *           n MUST be greater than, or equal to 0.
 * @param setIt  if true, then the n-th bit in this will be set, otherwise it
 *           will be cleared.
 *
 * output:
 * @param r  a copy of this NativeMPI's Pointer with its n-th bit set or cleared
 *           as requested.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natSetBit(JNIEnv *env, jobject this,
                                                   jint n, jboolean setIt,
                                                   jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_set (_r, _this);
  if (setIt == JNI_TRUE)
    {
      mpz_setbit (_r, (unsigned long)n);
    }
  else
    {
      mpz_clrbit (_r, (unsigned long)n);
    }
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) n;
  (void) setIt;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}

/*
 * @param this  an instance of NativeMPI.
 *
 * output:
 * @param r  a NativeMPI's Pointer such that r = ~this.
 */
JNIEXPORT void JNICALL
Java_gnu_java_math_GMP_natNot(JNIEnv *env, jobject this,
                                                jobject r)
{
#if defined(WITH_GNU_MP)
  mpz_srcptr _this;
  mpz_ptr _r;

  TRACE("begin");
  _this = (mpz_srcptr)JCL_GetRawData (env, (*env)->GetObjectField (env, this, native_ptr));
  _r = (mpz_ptr)JCL_GetRawData (env, r);
  mpz_com (_r, _this);
  TRACE("end");
#else /* !defined(WITH_GNU_MP) */
  (void) this;
  (void) r;
  throw_config_exception(env);
#endif /* defined(WITH_GNU_MP) */
}
