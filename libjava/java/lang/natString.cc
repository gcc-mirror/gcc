// natString.cc - Implementation of java.lang.String native methods.

/* Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <string.h>
#include <stdlib.h>

#include <gcj/cni.h>
#include <java/lang/Character.h>
#include <java/lang/String.h>
#include <java/lang/IndexOutOfBoundsException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <java/lang/StringIndexOutOfBoundsException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/StringBuffer.h>
#include <java/io/ByteArrayOutputStream.h>
#include <java/io/OutputStreamWriter.h>
#include <java/io/ByteArrayInputStream.h>
#include <java/io/InputStreamReader.h>
#include <java/util/Locale.h>
#include <gnu/gcj/convert/UnicodeToBytes.h>
#include <gnu/gcj/convert/BytesToUnicode.h>
#include <gnu/gcj/runtime/StringBuffer.h>
#include <jvm.h>

static jstring* strhash = NULL;
static int strhash_count = 0;  /* Number of slots used in strhash. */
static int strhash_size = 0;  /* Number of slots available in strhash.
                               * Assumed be power of 2! */

// Some defines used by toUpperCase / toLowerCase.
#define ESSET     0x00df
#define CAPITAL_S 0x0053
#define SMALL_I   0x0069
#define CAPITAL_I_WITH_DOT 0x0130
#define SMALL_DOTLESS_I    0x0131
#define CAPITAL_I 0x0049

#define DELETED_STRING ((jstring)(~0))
#define SET_STRING_IS_INTERNED(STR) /* nothing */

#define UNMASK_PTR(Ptr) (((unsigned long) (Ptr)) & ~0x01)
#define MASK_PTR(Ptr) (((unsigned long) (Ptr)) | 0x01)
#define PTR_MASKED(Ptr) (((unsigned long) (Ptr)) & 0x01)

/* Find a slot where the string with elements DATA, length LEN,
   and hash HASH should go in the strhash table of interned strings. */
jstring*
_Jv_StringFindSlot (jchar* data, jint len, jint hash)
{
  JvSynchronize sync (&StringClass);

  int start_index = hash & (strhash_size - 1);
  int deleted_index = -1;

  int index = start_index;
  /* step must be non-zero, and relatively prime with strhash_size. */
  jint step = (hash ^ (hash >> 16)) | 1;
  for (;;)
    {
      jstring* ptr = &strhash[index];
      jstring value = (jstring) UNMASK_PTR (*ptr);
      if (value == NULL)
	{
	  if (deleted_index >= 0)
	    return (&strhash[deleted_index]);
	  else
	    return ptr;
	}
      else if (*ptr == DELETED_STRING)
	deleted_index = index;
      else if (value->length() == len
	       && memcmp(JvGetStringChars(value), data, 2*len) == 0)
	return (ptr);
      index = (index + step) & (strhash_size - 1);
      JvAssert (index != start_index);
    }
}

/* Calculate a hash code for the string starting at PTR at given LENGTH.
   This uses the same formula as specified for java.lang.String.hash. */

static jint
hashChars (jchar* ptr, jint length)
{
  jchar* limit = ptr + length;
  jint hash = 0;
  // Updated specification from
  // http://www.javasoft.com/docs/books/jls/clarify.html.
  while (ptr < limit)
    hash = (31 * hash) + *ptr++;
  return hash;
}

jint
java::lang::String::hashCode()
{
  if (cachedHashCode == 0)
    cachedHashCode = hashChars(JvGetStringChars(this), length());
  return cachedHashCode;
}

jstring*
_Jv_StringGetSlot (jstring str)
{
  jchar* data = JvGetStringChars(str);
  int length = str->length();
  return _Jv_StringFindSlot(data, length, hashChars (data, length));
}

void
java::lang::String::rehash()
{
  JvSynchronize sync (&StringClass);

  if (strhash == NULL)
    {
      strhash_size = 1024;
      strhash = (jstring *) _Jv_AllocBytes (strhash_size * sizeof (jstring));
      memset (strhash, 0, strhash_size * sizeof (jstring));
    }
  else
    {
      int i = strhash_size;
      jstring* ptr = strhash + i;
      int nsize = strhash_size * 2;
      jstring *next = (jstring *) _Jv_AllocBytes (nsize * sizeof (jstring));
      memset (next, 0, nsize * sizeof (jstring));

      while (--i >= 0)
	{
	  --ptr;
	  if (*ptr == NULL || *ptr == DELETED_STRING)
	    continue;

	  /* This is faster equivalent of
	   * *__JvGetInternSlot(*ptr) = *ptr; */
	  jstring val = (jstring) UNMASK_PTR (*ptr);
	  jint hash = val->hashCode();
	  jint index = hash & (nsize - 1);
	  jint step = (hash ^ (hash >> 16)) | 1;
	  for (;;)
	    {
	      if (next[index] == NULL)
		{
		  next[index] = *ptr;
		  break;
		}
	      index = (index + step) & (nsize - 1);
	    }
	}

      strhash_size = nsize;
      strhash = next;
    }
}

jstring
java::lang::String::intern()
{
  JvSynchronize sync (&StringClass);
  if (3 * strhash_count >= 2 * strhash_size)
    rehash();
  jstring* ptr = _Jv_StringGetSlot(this);
  if (*ptr != NULL && *ptr != DELETED_STRING)
    {
      // See description in _Jv_FinalizeString() to understand this.
      *ptr = (jstring) MASK_PTR (*ptr);
      return (jstring) UNMASK_PTR (*ptr);
    }
  jstring str = (this->data == this
		 ? this
		 : _Jv_NewString(JvGetStringChars(this), this->length()));
  SET_STRING_IS_INTERNED(str);
  strhash_count++;
  *ptr = str;
  // When string is GC'd, clear the slot in the hash table.
  _Jv_RegisterStringFinalizer (str);
  return str;
}

// The fake String finalizer.  This is only used when the String has
// been intern()d.  However, we must check this case, as it might be
// called by the Reference code for any String.
void
_Jv_FinalizeString (jobject obj)
{
  JvSynchronize sync (&StringClass);

  // We might not actually have intern()d any strings at all, if
  // we're being called from Reference.
  if (! strhash)
    return;

  jstring str = reinterpret_cast<jstring> (obj);
  jstring *ptr = _Jv_StringGetSlot(str);
  if (*ptr == NULL || *ptr == DELETED_STRING
      || (jobject) UNMASK_PTR (*ptr) != obj)
    return;

  // We assume the lowest bit of the pointer is free for our nefarious
  // manipulations.  What we do is set it to `0' (implicitly) when
  // interning the String.  If we subsequently re-intern the same
  // String, then we set the bit.  When finalizing, if the bit is set
  // then we clear it and re-register the finalizer.  We know this is
  // a safe approach because both intern() and _Jv_FinalizeString()
  // acquire the class lock; this bit can't be manipulated when the
  // lock is not held.  So if we are finalizing and the bit is clear
  // then we know all references are gone and we can clear the entry
  // in the hash table.  The naive approach of simply clearing the
  // pointer here fails in the case where a request to intern a new
  // string with the same contents is made between the time the
  // intern()d string is found to be unreachable and when the
  // finalizer is actually run.  In this case we could clear a pointer
  // to a valid string, and future intern() calls for that particular
  // value would spuriously fail.
  if (PTR_MASKED (*ptr))
    {
      *ptr = (jstring) UNMASK_PTR (*ptr);
      _Jv_RegisterStringFinalizer (obj);
    }
  else
    {
      *ptr = DELETED_STRING;
      strhash_count--;
    }
}

jstring
_Jv_NewStringUTF (const char *bytes)
{
  int size = strlen (bytes);
  unsigned char *p = (unsigned char *) bytes;

  int length = _Jv_strLengthUtf8 ((char *) p, size);
  if (length < 0)
    return NULL;

  jstring jstr = JvAllocString (length);
  jchar *chrs = JvGetStringChars (jstr);

  p = (unsigned char *) bytes;
  unsigned char *limit = p + size;
  while (p < limit)
    *chrs++ = UTF8_GET (p, limit);

  return jstr;
}

jstring
_Jv_NewStringUtf8Const (Utf8Const* str)
{
  jchar *chrs;
  jchar buffer[100];
  jstring jstr;
  unsigned char* data = (unsigned char*) str->data;
  unsigned char* limit = data + str->length;
  int length = _Jv_strLengthUtf8(str->data, str->length);

  if (length <= (int) (sizeof(buffer) / sizeof(jchar)))
    {
      jstr = NULL;
      chrs = buffer;
    }
  else
    {
      jstr = JvAllocString(length);
      chrs = JvGetStringChars(jstr);
    }

  jint hash = 0;
  while (data < limit)
    {
      jchar ch = UTF8_GET(data, limit);
      hash = (31 * hash) + ch;
      *chrs++ = ch;
    }
  chrs -= length;

  JvSynchronize sync (&StringClass);
  if (3 * strhash_count >= 2 * strhash_size)
    java::lang::String::rehash();
  jstring* ptr = _Jv_StringFindSlot (chrs, length, hash);
  if (*ptr != NULL && *ptr != DELETED_STRING)
    return (jstring) UNMASK_PTR (*ptr);
  strhash_count++;
  if (jstr == NULL)
    {
      jstr = JvAllocString(length);
      chrs = JvGetStringChars(jstr);
      memcpy (chrs, buffer, sizeof(jchar)*length);
    }
  jstr->cachedHashCode = hash;
  *ptr = jstr;
  SET_STRING_IS_INTERNED(jstr);
  // When string is GC'd, clear the slot in the hash table.  Note that
  // we don't have to call _Jv_RegisterStringFinalizer here, as we
  // know the new object cannot be referred to by a Reference.
  _Jv_RegisterFinalizer ((void *) jstr, _Jv_FinalizeString);
  return jstr;
}

jsize
_Jv_GetStringUTFLength (jstring string)
{
  jsize len = 0;
  jchar *ptr = JvGetStringChars (string);
  jsize i = string->length();
  while (--i >= 0)
    {
      jchar ch = *ptr++;
      if (ch > 0 && ch <= 0x7F)
	len += 1;
      else if (ch <= 0x7FF)
	len += 2;
      else
	len += 3;
    }
  return len;
}

// Not sure this quite matches GetStringUTFRegion.
// null-termination of result?  len?  throw exception?
jsize
_Jv_GetStringUTFRegion (jstring str, jsize start, jsize len, char *buf)
{
  jchar *sptr = JvGetStringChars (str) + start;
  jsize i = len;
  char *dptr = buf;
  while (--i >= 0)
    {
      jchar ch = *sptr++;
      if (ch > 0 && ch <= 0x7F)
	*dptr++ = (char) ch;
      else if (ch <= 0x7FF)
	{
	  *dptr++ = (char) (0xC0 + ((ch >> 6) & 0x1F));
	  *dptr++ = (char) (0x80 + (ch & 0x3F));
	}
      else
	{
	  *dptr++ = (char) (0xE0 + ((ch >> 12) & 0xF));
	  *dptr++ = (char) (0x80 + ((ch >> 6) & 0x3F));
	  *dptr++ = (char) (0x80 + (ch & 0x3F));
	}
    }
  return dptr - buf;
}

/* Put printed (decimal) representation of NUM in a buffer.
   BUFEND marks the end of the buffer, which must be at least 11 jchars long.
   Returns the COUNT of jchars written.  The result is in
   (BUFEND - COUNT) (inclusive) upto (BUFEND) (exclusive). */

jint
_Jv_FormatInt (jchar* bufend, jint num)
{
  register jchar* ptr = bufend;
  jboolean isNeg;
  if (num < 0)
    {
      isNeg = true;
      num = -(num);
      if (num < 0)
	{
	  // Must be MIN_VALUE, so handle this special case.
	  // FIXME use 'unsigned jint' for num.
	  *--ptr = '8';
	  num = 214748364;
	}
      }
    else
      isNeg = false;

    do
      {
        *--ptr = (jchar) ((int) '0' + (num % 10));
        num /= 10;
      }
    while (num > 0);

    if (isNeg)
      *--ptr = '-';
    return bufend - ptr;
}

jstring
java::lang::String::valueOf (jint num)
{
  // Use an array large enough for "-2147483648"; i.e. 11 chars.
  jchar buffer[11];
  int i = _Jv_FormatInt (buffer+11, num);
  return _Jv_NewString (buffer+11-i, i);
}

jstring
_Jv_AllocString(jsize len)
{
  jsize sz = sizeof(java::lang::String) + len * sizeof(jchar);

  // We assert that for strings allocated this way, the data field
  // will always point to the object itself.  Thus there is no reason
  // for the garbage collector to scan any of it.
  // Furthermore, we're about to overwrite the string data, so
  // initialization of the object is not an issue.
#ifdef ENABLE_JVMPI
  jstring obj = (jstring) _Jv_AllocPtrFreeObject(&StringClass, sz);
#else
  // Class needs no initialization, and there is no finalizer, so
  // we can go directly to the collector's allocator interface.
  jstring obj = (jstring) _Jv_AllocPtrFreeObj(sz, &StringClass);
#endif
  obj->data = obj;
  obj->boffset = sizeof(java::lang::String);
  obj->count = len;
  obj->cachedHashCode = 0;
  return obj;
}

jstring
_Jv_NewString(const jchar *chars, jsize len)
{
  jstring str = _Jv_AllocString(len);
  jchar* data = JvGetStringChars (str);
  while (--len >= 0)
    *data++ = *chars++;
  return str;
}

jstring
_Jv_NewStringLatin1(const char *bytes, jsize len)
{
  jstring str = JvAllocString(len);
  jchar* data = JvGetStringChars (str);
  while (--len >= 0)
    *data++ = *(unsigned char*)bytes++;
  return str;
}

void
java::lang::String::init(jcharArray chars, jint offset, jint count,
			 jboolean dont_copy)
{
  if (! chars)
    throw new NullPointerException;
  jsize data_size = JvGetArrayLength (chars);
  if (offset < 0 || count < 0 || offset + count < 0
      || offset + count > data_size)
    throw new ArrayIndexOutOfBoundsException;
  jcharArray array;
  jchar *pdst;
  if (! dont_copy)
    {
      array = JvNewCharArray(count);
      pdst = elements (array);
      memcpy (pdst, elements (chars) + offset, count * sizeof (jchar));
    }
  else
    {
      array = chars;
      pdst = &(elements(array)[offset]);
    }

  data = array;
  boffset = (char *) pdst - (char *) array;
  this->count = count;
}

void
java::lang::String::init(jbyteArray ascii, jint hibyte, jint offset,
			 jint count)
{
  if (! ascii)
    throw new NullPointerException;
  jsize data_size = JvGetArrayLength (ascii);
  if (offset < 0 || count < 0 || offset + count < 0
      || offset + count > data_size)
    throw new ArrayIndexOutOfBoundsException;
  jcharArray array = JvNewCharArray(count);
  jbyte *psrc = elements (ascii) + offset;
  jchar *pdst = elements (array);
  data = array;
  boffset = (char *) pdst - (char *) array;
  this->count = count;
  hibyte = (hibyte & 0xff) << 8;
  while (-- count >= 0)
    {
      *pdst++ = hibyte | (*psrc++ & 0xff);
    }
}

void
java::lang::String::init (jbyteArray bytes, jint offset, jint count,
			  jstring encoding)
{
  if (! bytes)
    throw new NullPointerException;
  jsize data_size = JvGetArrayLength (bytes);
  if (offset < 0 || count < 0 || offset + count < 0
      || offset + count > data_size)
    throw new ArrayIndexOutOfBoundsException;
  jcharArray array = JvNewCharArray (count);
  gnu::gcj::convert::BytesToUnicode *converter
    = gnu::gcj::convert::BytesToUnicode::getDecoder(encoding);
  jint outpos = 0;
  int avail = count;
  converter->setInput(bytes, offset, offset+count);
  while (converter->inpos < converter->inlength)
    {
      int done = converter->read(array, outpos, avail);
      if (done == 0)
	{
	  jint new_size = 2 * (outpos + avail);
	  jcharArray new_array = JvNewCharArray (new_size);
	  memcpy (elements (new_array), elements (array),
		  outpos * sizeof(jchar));
	  array = new_array;
	  avail = new_size - outpos;
	}
      else
	{
	  outpos += done;
	  avail -= done;
	}
    }
  converter->done ();
  this->data = array;
  this->boffset = (char *) elements (array) - (char *) array;
  this->count = outpos;
}

void
java::lang::String::init (gnu::gcj::runtime::StringBuffer *buffer)
{
  init (buffer->value, 0, buffer->count, true);
}

jboolean
java::lang::String::equals(jobject anObject)
{
  if (anObject == NULL)
    return false;
  if (anObject == this)
    return true;
  if (anObject->getClass() != &StringClass)
    return false;
  jstring other = (jstring) anObject;
  if (count != other->count)
    return false;
  /* if both are interned, return false. */
  jint i = count;
  jchar *xptr = JvGetStringChars (this);
  jchar *yptr = JvGetStringChars (other);
  while (--i >= 0)
    {
      if (*xptr++ != *yptr++)
	return false;
    }
  return true;
}

jboolean
java::lang::String::contentEquals(java::lang::StringBuffer* buffer)
{
  if (buffer == NULL)
    throw new NullPointerException;
  JvSynchronize sync(buffer);
  if (count != buffer->count)
    return false;
  if (data == buffer->value)
    return true; // Possible if shared.
  jint i = count;
  jchar *xptr = JvGetStringChars(this);
  jchar *yptr = elements(buffer->value);
  while (--i >= 0)
    if (*xptr++ != *yptr++)
      return false;
  return true;
}

jchar
java::lang::String::charAt(jint i)
{
  if (i < 0 || i >= count)
    throw new java::lang::StringIndexOutOfBoundsException(i);
  return JvGetStringChars(this)[i];
}

void
java::lang::String::getChars(jint srcBegin, jint srcEnd,
			     jcharArray dst, jint dstBegin)
{
  jint dst_length = JvGetArrayLength (dst);
  if (srcBegin < 0 || srcBegin > srcEnd || srcEnd > count)
    throw new java::lang::StringIndexOutOfBoundsException;
  // The 2nd part of the test below is equivalent to 
  // dstBegin + (srcEnd-srcBegin) > dst_length
  // except that it does not overflow.
  if (dstBegin < 0 || dstBegin > dst_length - (srcEnd-srcBegin))
    throw new ArrayIndexOutOfBoundsException;
  jchar *dPtr = elements (dst) + dstBegin;
  jchar *sPtr = JvGetStringChars (this) + srcBegin;
  jint i = srcEnd-srcBegin;
  while (--i >= 0)
    *dPtr++ = *sPtr++;
}

jbyteArray
java::lang::String::getBytes (jstring enc)
{
  jint todo = length();
  jint buflen = todo;
  jbyteArray buffer = JvNewByteArray(todo);
  jint bufpos = 0;
  jint offset = 0;
  gnu::gcj::convert::UnicodeToBytes *converter
    = gnu::gcj::convert::UnicodeToBytes::getEncoder(enc);
  while (todo > 0 || converter->havePendingBytes())
    {
      converter->setOutput(buffer, bufpos);
      int converted = converter->write(this, offset, todo, NULL);
      bufpos = converter->count;
      if (converted == 0 && bufpos == converter->count)
	{
	  buflen *= 2;
	  jbyteArray newbuffer = JvNewByteArray(buflen);
	  memcpy (elements (newbuffer), elements (buffer), bufpos);
	  buffer = newbuffer;
	}
      else
	bufpos = converter->count;

      offset += converted;
      todo -= converted;
    }
  converter->done ();
  if (bufpos == buflen)
    return buffer;
  jbyteArray result = JvNewByteArray(bufpos);
  memcpy (elements (result), elements (buffer), bufpos);
  return result;
}

void
java::lang::String::getBytes(jint srcBegin, jint srcEnd,
			     jbyteArray dst, jint dstBegin)
{
  jint dst_length = JvGetArrayLength (dst);
  if (srcBegin < 0 || srcBegin > srcEnd || srcEnd > count)
    throw new java::lang::StringIndexOutOfBoundsException;
  // The 2nd part of the test below is equivalent to 
  // dstBegin + (srcEnd-srcBegin) > dst_length
  // except that it does not overflow.
  if (dstBegin < 0 || dstBegin > dst_length - (srcEnd-srcBegin))
    throw new ArrayIndexOutOfBoundsException;
  jbyte *dPtr = elements (dst) + dstBegin;
  jchar *sPtr = JvGetStringChars (this) + srcBegin;
  jint i = srcEnd-srcBegin;
  while (--i >= 0)
    *dPtr++ = (jbyte) *sPtr++;
}

jcharArray
java::lang::String::toCharArray()
{
  jcharArray array = JvNewCharArray(count);
  jchar *dPtr = elements (array);
  jchar *sPtr = JvGetStringChars (this);
  jint i = count;
  while (--i >= 0)
    *dPtr++ = *sPtr++;
  return array;
}

jboolean
java::lang::String::equalsIgnoreCase (jstring anotherString)
{
  if (anotherString == NULL || count != anotherString->count)
    return false;
  jchar *tptr = JvGetStringChars (this);
  jchar *optr = JvGetStringChars (anotherString);
  jint i = count;
  while (--i >= 0)
    {
      jchar tch = *tptr++;
      jchar och = *optr++;
      if (tch != och
	  && (java::lang::Character::toLowerCase (tch)
	      != java::lang::Character::toLowerCase (och))
	  && (java::lang::Character::toUpperCase (tch)
	      != java::lang::Character::toUpperCase (och)))
	return false;
    }
  return true;
}

jboolean
java::lang::String::regionMatches (jint toffset,
				   jstring other, jint ooffset, jint len)
{
  if (toffset < 0 || ooffset < 0 || len < 0
      || toffset > count - len
      || ooffset > other->count - len)
    return false;
  jchar *tptr = JvGetStringChars (this) + toffset;
  jchar *optr = JvGetStringChars (other) + ooffset;
  jint i = len;
  while (--i >= 0)
    {
      if (*tptr++ != *optr++)
	return false;
    }
  return true;
}

jint
java::lang::String::compareTo (jstring anotherString)
{
  jchar *tptr = JvGetStringChars (this);
  jchar *optr = JvGetStringChars (anotherString);
  jint tlen = this->count;
  jint olen = anotherString->count;
  jint i = tlen > olen ? olen : tlen;
  while (--i >= 0)
    {
      jchar tch = *tptr++;
      jchar och = *optr++;
      if (tch != och)
	return (jint) tch - (jint) och;
    }
  return tlen - olen;
}

jboolean
java::lang::String::regionMatches (jboolean ignoreCase, jint toffset,
				   jstring other, jint ooffset, jint len)
{
  if (toffset < 0 || ooffset < 0 || len < 0
      || toffset > count - len
      || ooffset > other->count - len)
    return false;
  jchar *tptr = JvGetStringChars (this) + toffset;
  jchar *optr = JvGetStringChars (other) + ooffset;
  jint i = len;
  if (ignoreCase)
    while (--i >= 0)
      {
	jchar tch = *tptr++;
	jchar och = *optr++;
	if ((java::lang::Character::toLowerCase (tch)
	     != java::lang::Character::toLowerCase (och))
	    && (java::lang::Character::toUpperCase (tch)
		!= java::lang::Character::toUpperCase (och)))
	  return false;
      }
  else
    while (--i >= 0)
      {
	jchar tch = *tptr++;
	jchar och = *optr++;
	if (tch != och)
	  return false;
      }
  return true;
}

jboolean
java::lang::String::startsWith (jstring prefix, jint toffset)
{
  jint i = prefix->count;
  if (toffset < 0 || toffset > count - i)
    return false;
  jchar *xptr = JvGetStringChars (this) + toffset;
  jchar *yptr = JvGetStringChars (prefix);
  while (--i >= 0)
    {
      if (*xptr++ != *yptr++)
	return false;
    }
  return true;
}

jint
java::lang::String::indexOf (jint ch, jint fromIndex)
{
  if (fromIndex < 0)
    fromIndex = 0;
  jchar *ptr = JvGetStringChars(this);
  for (;; ++fromIndex)
    {
      if (fromIndex >= count)
	return -1;
      if (ptr[fromIndex] == ch)
	return fromIndex;
    }
}

jint
java::lang::String::indexOf (jstring s, jint fromIndex)
{
  const jchar *const xchars = JvGetStringChars(s);
  const jchar *const ychars = JvGetStringChars(this) + fromIndex;
  
  const int xlength = s->length ();
  const int ylength = length () - fromIndex;
  
  int i = 0;
  int j = 0;

  while (i < ylength && j < xlength)
    {
      if (xchars[j] != ychars[i])
	{
	  i = i - j + 1;
	  j = 0;
	}
      else
	i++, j++;
    }

  if (j >= xlength)
    return fromIndex + i - xlength;
  else
    return -1;
}
    
jint
java::lang::String::lastIndexOf (jint ch, jint fromIndex)
{
  if (fromIndex >= count)
    fromIndex = count - 1;
  jchar *ptr = JvGetStringChars(this);
  for (;; --fromIndex)
    {
      if (fromIndex < 0)
	return -1;
      if (ptr[fromIndex] == ch)
	return fromIndex;
    }
}

jstring
java::lang::String::substring (jint beginIndex, jint endIndex)
{
  if (beginIndex < 0 || endIndex > count || beginIndex > endIndex)
    throw new StringIndexOutOfBoundsException;
  if (beginIndex == 0 && endIndex == count)
    return this;
  jint newCount = endIndex - beginIndex;
  if (newCount <= 8)  // Optimization, mainly for GC.
    return JvNewString(JvGetStringChars(this) + beginIndex, newCount);
  jstring s = new String();
  s->data = data;
  s->count = newCount;
  s->boffset = boffset + sizeof(jchar) * beginIndex;
  return s;
}

jstring
java::lang::String::concat(jstring str)
{
  jint str_count = str->count;
  if (str_count == 0)
    return this;
  jstring result = JvAllocString(count + str_count);
  jchar *dstPtr = JvGetStringChars(result);
  jchar *srcPtr = JvGetStringChars(this);
  jint i = count;
  while (--i >= 0)
    *dstPtr++ = *srcPtr++;
  srcPtr = JvGetStringChars(str);
  i = str->count;
  while (--i >= 0)
    *dstPtr++ = *srcPtr++;
  return result;
}

jstring
java::lang::String::replace (jchar oldChar, jchar newChar)
{
  jint i;
  jchar* chrs = JvGetStringChars (this);
  for (i = 0;  ;  i++)
    {
      if (i == count)
	return this;
      if (chrs[i] == oldChar)
	break;
    }
  jstring result = JvAllocString (count);
  jchar *dPtr = JvGetStringChars (result);
  for (int j = 0;  j < i;  j++)
    *dPtr++ = chrs[j];
  for (; i < count;  i++)
    {
      jchar ch = chrs[i];
      if (ch == oldChar)
	ch = newChar;
      *dPtr++ = ch;
    }
  return result;
}

jstring
java::lang::String::toLowerCase (java::util::Locale *locale)
{
  jint i;
  jchar* chrs = JvGetStringChars(this);
  jchar ch = 0;

  bool handle_tr = false;
  if (locale != NULL)
    {
      String *lang = locale->getLanguage ();
      if (lang->length () == 2
	  && lang->charAt (0) == 't'
	  && lang->charAt (1) == 'r')
	handle_tr = true;
    }

  for (i = 0;  ;  i++)
    {
      if (i == count)
	return this;
      jchar origChar = chrs[i];

      if (handle_tr && (origChar == CAPITAL_I
			|| origChar == CAPITAL_I_WITH_DOT))
	break;

      ch = java::lang::Character::toLowerCase(origChar);
      if (ch != origChar)
	break;
    }
  jstring result = JvAllocString(count);
  jchar *dPtr = JvGetStringChars (result);
  for (int j = 0;  j < i;  j++)
    *dPtr++ = chrs[j];
  *dPtr++ = ch;  i++;
  for (; i < count;  i++)
    {
      if (handle_tr && chrs[i] == CAPITAL_I)
	*dPtr++ = SMALL_DOTLESS_I;
      else if (handle_tr && chrs[i] == CAPITAL_I_WITH_DOT)
	*dPtr++ = SMALL_I;
      else
	*dPtr++ = java::lang::Character::toLowerCase(chrs[i]);
    }
  return result;
}

jstring
java::lang::String::toUpperCase (java::util::Locale *locale)
{
  jint i;
  jchar* chrs = JvGetStringChars(this);
  jchar ch;

  // When handling a specific locale there might be special rules.
  // Currently all existing rules are simply handled inline, as there
  // are only two and they are documented in the online 1.2 docs.
  bool handle_esset = locale != NULL;
  bool handle_tr = false;
  if (locale != NULL)
    {
      String *lang = locale->getLanguage ();
      if (lang->length () == 2
	  && lang->charAt (0) == 't'
	  && lang->charAt (1) == 'r')
	handle_tr = true;
    }

  int new_count = count;
  bool new_string = false;
  for (i = 0;  ;  i++)
    {
      if (i == count)
	break;
      jchar origChar = chrs[i];

      if (handle_esset && origChar == ESSET)
	{
	  ++new_count;
	  new_string = true;
	}
      else if (handle_tr && (origChar == SMALL_I
			     || origChar == SMALL_DOTLESS_I))
	new_string = true;
      else
	{
	  ch = java::lang::Character::toUpperCase(origChar);
	  if (ch != origChar)
	    new_string = true;
	}

      if (new_string && ! handle_esset)
	break;
    }
  if (! new_string)
    return this;
  jstring result = JvAllocString(new_count);
  jchar *dPtr = JvGetStringChars (result);
  for (i = 0; i < count;  i++)
    {
      if (handle_esset && chrs[i] == ESSET)
	{
	  *dPtr++ = CAPITAL_S;
	  *dPtr++ = CAPITAL_S;
	}
      else if (handle_tr && chrs[i] == SMALL_I)
	*dPtr++ = CAPITAL_I_WITH_DOT;
      else if (handle_tr && chrs[i] == SMALL_DOTLESS_I)
	*dPtr++ = CAPITAL_I;
      else
	*dPtr++ = java::lang::Character::toUpperCase(chrs[i]);
    }
  return result;
}

jstring
java::lang::String::trim ()
{
  jchar* chrs = JvGetStringChars(this);
  if (count == 0 || (chrs[0] > ' ' && chrs[count-1] > ' '))
    return this;
  jint preTrim = 0;
  for (;; preTrim++)
    {
      if (preTrim == count)
	return new String();
      if (chrs[preTrim] > ' ')
	break;
    }
  jint endTrim = count;
  while (chrs[endTrim-1] <= ' ')
    endTrim--;
  return substring(preTrim, endTrim);
}

jstring
java::lang::String::valueOf(jcharArray data, jint offset, jint count)
{
  jint data_length = JvGetArrayLength (data);
  if (offset < 0 || count < 0 || offset > data_length - count)
    throw new ArrayIndexOutOfBoundsException;
  jstring result = JvAllocString(count);
  jchar *sPtr = elements (data) + offset;
  jchar *dPtr = JvGetStringChars(result);
  while (--count >= 0)
    *dPtr++ = *sPtr++;
  return result;
}

jstring
java::lang::String::valueOf(jchar c)
{
  jstring result = JvAllocString(1);
  JvGetStringChars (result)[0] = c;
  return result;
}
