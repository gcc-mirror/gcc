// natString.cc - Implementation of java.lang.String native methods.

/* Copyright (C) 1998, 1999  Free Software Foundation

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
#include <java/io/ByteArrayOutputStream.h>
#include <java/io/OutputStreamWriter.h>
#include <java/io/ByteArrayInputStream.h>
#include <java/io/InputStreamReader.h>
#include <gnu/gcj/convert/UnicodeToBytes.h>
#include <gnu/gcj/convert/BytesToUnicode.h>
#include <jvm.h>

static jstring* strhash = NULL;
static int strhash_count = 0;  /* Number of slots used in strhash. */
static int strhash_size = 0;  /* Number of slots available in strhash.
                               * Assumed be power of 2! */

#define DELETED_STRING ((jstring)(~0))
#define SET_STRING_IS_INTERNED(STR) /* nothing */

/* Find a slot where the string with elements DATA, length LEN,
   and hash HASH should go in the strhash table of interned strings. */
jstring*
_Jv_StringFindSlot (jchar* data, jint len, jint hash)
{
  JvSynchronize sync (&StringClass);

  int start_index = hash & (strhash_size - 1);
  int deleted_index = -1;

  register int index = start_index;
  /* step must be non-zero, and relatively prime with strhash_size. */
  int step = 8 * hash + 7;
  for (;;)
    {
      register jstring* ptr = &strhash[index];
      if (*ptr == NULL)
	{
	  if (deleted_index >= 0)
	    return (&strhash[deleted_index]);
	  else
	    return ptr;
	}
      else if (*ptr == DELETED_STRING)
	deleted_index = index;
      else if ((*ptr)->length() == len
	       && memcmp(JvGetStringChars(*ptr), data, 2*len) == 0)
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
  register jchar* limit = ptr + length;
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
  return hashChars(JvGetStringChars(this), length());
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
      register int i = strhash_size;
      register jstring* ptr = strhash + i;
      strhash_size *= 2;
      strhash = (jstring *) _Jv_AllocBytes (strhash_size * sizeof (jstring));
      memset (strhash, 0, strhash_size * sizeof (jstring));

      while (--i >= 0)
	{
	  --ptr;
	  if (*ptr == NULL || *ptr == DELETED_STRING)
	    continue;

	  /* This is faster equivalent of
	   * *__JvGetInternSlot(*ptr) = *ptr; */
	  jint hash = (*ptr)->hashCode();
	  jint index = hash & (strhash_size - 1);
	  jint step = 8 * hash + 7;
	  for (;;)
	    {
	      if (strhash[index] == NULL)
		{
		  strhash[index] = *ptr;
		  break;
		}
	      index = (index + step) & (strhash_size - 1);
	    }
	}
    }
}

jstring
java::lang::String::intern()
{
  JvSynchronize sync (&StringClass);
  if (4 * strhash_count >= 3 * strhash_size)
    rehash();
  jstring* ptr = _Jv_StringGetSlot(this);
  if (*ptr != NULL && *ptr != DELETED_STRING)
    return *ptr;
  SET_STRING_IS_INTERNED(this);
  strhash_count++;
  *ptr = this;
  return this;
}

/* Called by String fake finalizer. */
void
java::lang::String::unintern()
{
  JvSynchronize sync (&StringClass);
  jstring* ptr = _Jv_StringGetSlot(this);
  if (*ptr == NULL || *ptr == DELETED_STRING)
    return;
  *ptr = DELETED_STRING;
  strhash_count--;
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
  register unsigned char* data = (unsigned char*) str->data;
  register unsigned char* limit = data + str->length;
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

  while (data < limit)
    *chrs++ = UTF8_GET(data, limit);
  chrs -= length;

  JvSynchronize sync (&StringClass);
  if (4 * strhash_count >= 3 * strhash_size)
    java::lang::String::rehash();
  int hash = str->hash;
  jstring* ptr = _Jv_StringFindSlot (chrs, length, hash);
  if (*ptr != NULL && *ptr != DELETED_STRING)
    return *ptr;
  strhash_count++;
  if (jstr == NULL)
    {
      jstr = JvAllocString(length);
      chrs = JvGetStringChars(jstr);
      memcpy (chrs, buffer, sizeof(jchar)*length);
    }
  *ptr = jstr;
  SET_STRING_IS_INTERNED(jstr);
  return jstr;
}

jsize
_Jv_GetStringUTFLength (jstring string)
{
  register jsize len = 0;
  register jchar *ptr = JvGetStringChars (string);
  register jsize i = string->length();
  while (--i >= 0)
    {
      register jchar ch = *ptr++;
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
  register jchar *sptr = JvGetStringChars (str) + start;
  register jsize i = len;
  register char *dptr = buf;
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

jstring
_Jv_AllocString(jsize len)
{
  jsize sz = sizeof(java::lang::String) + len * sizeof(jchar);

  jstring obj = (jstring) JvAllocObject(&StringClass, sz);

  obj->data = obj;
  obj->boffset = sizeof(java::lang::String);
  obj->count = len;
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
java::lang::String::init ()
{
  count = 0;
  boffset = sizeof(java::lang::String);
  data = this;
}

void
java::lang::String::init(jcharArray chars, jint offset, jint count,
			 jboolean dont_copy)
{
  if (! chars)
    JvThrow (new NullPointerException);
  jsize data_size = JvGetArrayLength (chars);
  if (offset < 0 || count < 0 || offset + count < 0
      || offset + count > data_size)
    JvThrow (new StringIndexOutOfBoundsException());
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
      JvAssert (offset == 0);
      array = chars;
      pdst = elements (array);
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
    JvThrow (new NullPointerException);
  jsize data_size = JvGetArrayLength (ascii);
  if (offset < 0 || count < 0 || offset + count < 0
      || offset + count > data_size)
    JvThrow (new java::lang::StringIndexOutOfBoundsException());
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
    JvThrow (new NullPointerException);
  jsize data_size = JvGetArrayLength (bytes);
  if (offset < 0 || count < 0 || offset + count < 0
      || offset + count > data_size)
    JvThrow (new StringIndexOutOfBoundsException);
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
  this->data = array;
  this->boffset = (char *) elements (array) - (char *) array;
  this->count = outpos;
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
  register jint i = count;
  register jchar *xptr = JvGetStringChars (this);
  register jchar *yptr = JvGetStringChars (other);
  while (--i >= 0)
    {
      if (*xptr++ != *yptr++)
	return false;
    }
  return true;
}

jchar
java::lang::String::charAt(jint i)
{
  if (i < 0 || i >= count)
    JvThrow (new java::lang::StringIndexOutOfBoundsException());
  return JvGetStringChars(this)[i];
}

void
java::lang::String::getChars(jint srcBegin, jint srcEnd,
			     jcharArray dst, jint dstBegin)
{
  jint dst_length = JvGetArrayLength (dst);
  if (srcBegin < 0 || srcBegin > srcEnd || srcEnd > count
      || dstBegin < 0 || dstBegin + (srcEnd-srcBegin) > dst_length)
    JvThrow (new java::lang::StringIndexOutOfBoundsException());
  register jchar *dPtr = elements (dst) + dstBegin;
  register jchar *sPtr = JvGetStringChars (this) + srcBegin;
  register jint i = srcEnd-srcBegin;
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
  while (todo > 0)
    {
      converter->setOutput(buffer, bufpos);
      int converted = converter->write(this, offset, todo, NULL);
      bufpos = converter->count;
      if (converted == 0)
	{
	  buflen *= 2;
	  jbyteArray newbuffer = JvNewByteArray(buflen);
	  memcpy (elements (newbuffer), elements (buffer), bufpos);
	  buffer = newbuffer;
	}
      else
	{
	  offset += converted;
	  todo -= converted;
	}
    }
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
  if (srcBegin < 0 || srcBegin > srcEnd || srcEnd > count
      || dstBegin < 0 || dstBegin + (srcEnd-srcBegin) > dst_length)
    JvThrow (new java::lang::StringIndexOutOfBoundsException());
  register jbyte *dPtr = elements (dst) + dstBegin;
  register jchar *sPtr = JvGetStringChars (this) + srcBegin;
  register jint i = srcEnd-srcBegin;
  while (--i >= 0)
    *dPtr++ = (jbyte) *sPtr++;
}

jcharArray
java::lang::String::toCharArray()
{
  jcharArray array = JvNewCharArray(count);
  register jchar *dPtr = elements (array);
  register jchar *sPtr = JvGetStringChars (this);
  register jint i = count;
  while (--i >= 0)
    *dPtr++ = *sPtr++;
  return array;
}

jboolean
java::lang::String::equalsIgnoreCase (jstring anotherString)
{
  if (anotherString == NULL || count != anotherString->count)
    return false;
  register jchar *tptr = JvGetStringChars (this);
  register jchar *optr = JvGetStringChars (anotherString);
  register jint i = count;
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
  if (toffset < 0 || ooffset < 0
      || toffset + len > count
      || ooffset + len > other->count)
    return false;
  register jchar *tptr = JvGetStringChars (this) + toffset;
  register jchar *optr = JvGetStringChars (other) + ooffset;
  register jint i = len;
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
  register jchar *tptr = JvGetStringChars (this);
  register jchar *optr = JvGetStringChars (anotherString);
  jint tlen = this->count;
  jint olen = anotherString->count;
  register jint i = tlen > olen ? olen : tlen;
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
  if (toffset < 0 || ooffset < 0
      || toffset + len > count
      || ooffset + len > other->count)
    return false;
  register jchar *tptr = JvGetStringChars (this) + toffset;
  register jchar *optr = JvGetStringChars (other) + ooffset;
  register jint i = len;
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
  register jint i = prefix->count;
  if (toffset < 0 || toffset + i > count)
    return false;
  register jchar *xptr = JvGetStringChars (this) + toffset;
  register jchar *yptr = JvGetStringChars (prefix);
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
  register jchar *ptr = JvGetStringChars(this);
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
  register jchar *ptr = JvGetStringChars(this);
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
    JvThrow (new StringIndexOutOfBoundsException());
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
  register jchar *dstPtr = JvGetStringChars(result);
  register jchar *srcPtr = JvGetStringChars(this);
  register jint i = count;
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
java::lang::String::toLowerCase ()
{
  jint i;
  jchar* chrs = JvGetStringChars(this);
  jchar ch;
  for (i = 0;  ;  i++)
    {
      if (i == count)
	return this;
      jchar origChar = chrs[i];
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
      *dPtr++ = java::lang::Character::toLowerCase(chrs[i]);
    }
  return result;
}

jstring
java::lang::String::toUpperCase ()
{
  jint i;
  jchar* chrs = JvGetStringChars(this);
  jchar ch;
  for (i = 0;  ;  i++)
    {
      if (i == count)
	return this;
      jchar origChar = chrs[i];
      ch = java::lang::Character::toUpperCase(origChar);
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
  if (offset < 0 || count < 0 || offset+count > data_length)
    JvThrow (new java::lang::IndexOutOfBoundsException());
  register jstring result = JvAllocString(count);
  register jchar *sPtr = elements (data) + offset;
  register jchar *dPtr = JvGetStringChars(result);
  while (--count >= 0)
    *dPtr++ = *sPtr++;
  return result;
}

jstring
java::lang::String::valueOf(jchar c)
{
  register jstring result = JvAllocString(1);
  JvGetStringChars (result)[0] = c;
  return result;
}
