 /* GNU Objective C Runtime archiving
   Copyright (C) 1993, 1995, 1996, 1997, 2002, 2004 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
GCC; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include "tconfig.h"
#include "objc/runtime.h"
#include "objc/typedstream.h"
#include "objc/encoding.h"
#include <stdlib.h>

extern int fflush (FILE *);

#define ROUND(V, A) \
  ({ typeof (V) __v = (V); typeof (A) __a = (A);  \
     __a * ((__v + __a - 1)/__a); })

#define PTR2LONG(P) (((char *) (P))-(char *) 0)
#define LONG2PTR(L) (((char *) 0) + (L))

/* Declare some functions... */

static int
objc_read_class (struct objc_typed_stream *stream, Class *class);

int objc_sizeof_type (const char *type);

static int
objc_write_use_common (struct objc_typed_stream *stream, unsigned long key);

static int
objc_write_register_common (struct objc_typed_stream *stream,
			    unsigned long key);

static int 
objc_write_class (struct objc_typed_stream *stream,
			 struct objc_class *class);

const char *objc_skip_type (const char *type);

static void __objc_finish_write_root_object (struct objc_typed_stream *);
static void __objc_finish_read_root_object (struct objc_typed_stream *);

static inline int
__objc_code_unsigned_char (unsigned char *buf, unsigned char val)
{
  if ((val&_B_VALUE) == val)
    {
      buf[0] = val|_B_SINT;
      return 1;
    }
  else
    {
      buf[0] = _B_NINT|0x01;
      buf[1] = val;
      return 2;
    }
}

int
objc_write_unsigned_char (struct objc_typed_stream *stream,
			  unsigned char value)
{
  unsigned char buf[sizeof (unsigned char) + 1];
  int len = __objc_code_unsigned_char (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}

static inline int
__objc_code_char (unsigned char *buf, signed char val)
{
  if (val >= 0)
    return __objc_code_unsigned_char (buf, val);
  else
    {
      buf[0] = _B_NINT|_B_SIGN|0x01;
      buf[1] = -val;
      return 2;
    }
}

int
objc_write_char (struct objc_typed_stream *stream, signed char value)
{
  unsigned char buf[sizeof (char) + 1];
  int len = __objc_code_char (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}

static inline int
__objc_code_unsigned_short (unsigned char *buf, unsigned short val)
{
  if ((val&_B_VALUE) == val)
    {
      buf[0] = val|_B_SINT;
      return 1;
    }
  else 
    {
      int c, b;

      buf[0] = _B_NINT;

      for (c = sizeof (short); c != 0; c -= 1)
	if (((val >> (8*(c - 1)))%0x100) != 0)
	  break;

      buf[0] |= c;

      for (b = 1; c != 0; c--, b++)
	{
	  buf[b] = (val >> (8*(c - 1)))%0x100;
	}

      return b;
    }
}

int
objc_write_unsigned_short (struct objc_typed_stream *stream, 
			   unsigned short value)
{
  unsigned char buf[sizeof (unsigned short) + 1];
  int len = __objc_code_unsigned_short (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}
      
static inline int
__objc_code_short (unsigned char *buf, short val)
{
  int sign = (val < 0);
  int size = __objc_code_unsigned_short (buf, sign ? -val : val);
  if (sign)
    buf[0] |= _B_SIGN;
  return size;
}

int
objc_write_short (struct objc_typed_stream *stream, short value)
{
  unsigned char buf[sizeof (short) + 1];
  int len = __objc_code_short (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}
      

static inline int
__objc_code_unsigned_int (unsigned char *buf, unsigned int val)
{
  if ((val&_B_VALUE) == val)
    {
      buf[0] = val|_B_SINT;
      return 1;
    }
  else 
    {
      int c, b;

      buf[0] = _B_NINT;

      for (c = sizeof (int); c != 0; c -= 1)
	if (((val >> (8*(c - 1)))%0x100) != 0)
	  break;

      buf[0] |= c;

      for (b = 1; c != 0; c--, b++)
	{
	  buf[b] = (val >> (8*(c-1)))%0x100;
	}

      return b;
    }
}

int
objc_write_unsigned_int (struct objc_typed_stream *stream, unsigned int value)
{
  unsigned char buf[sizeof (unsigned int) + 1];
  int len = __objc_code_unsigned_int (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}

static inline int
__objc_code_int (unsigned char *buf, int val)
{
  int sign = (val < 0);
  int size = __objc_code_unsigned_int (buf, sign ? -val : val);
  if (sign)
    buf[0] |= _B_SIGN;
  return size;
}

int
objc_write_int (struct objc_typed_stream *stream, int value)
{
  unsigned char buf[sizeof (int) + 1];
  int len = __objc_code_int (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}

static inline int
__objc_code_unsigned_long (unsigned char *buf, unsigned long val)
{
  if ((val&_B_VALUE) == val)
    {
      buf[0] = val|_B_SINT;
      return 1;
    }
  else 
    {
      int c, b;

      buf[0] = _B_NINT;

      for (c = sizeof (long); c != 0; c -= 1)
	if (((val >> (8*(c - 1)))%0x100) != 0)
	  break;

      buf[0] |= c;

      for (b = 1; c != 0; c--, b++)
	{
	  buf[b] = (val >> (8*(c - 1)))%0x100;
	}

      return b;
    }
}

int
objc_write_unsigned_long (struct objc_typed_stream *stream, 
			  unsigned long value)
{
  unsigned char buf[sizeof (unsigned long) + 1];
  int len = __objc_code_unsigned_long (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}

static inline int
__objc_code_long (unsigned char *buf, long val)
{
  int sign = (val < 0);
  int size = __objc_code_unsigned_long (buf, sign ? -val : val);
  if (sign)
    buf[0] |= _B_SIGN;
  return size;
}

int
objc_write_long (struct objc_typed_stream *stream, long value)
{
  unsigned char buf[sizeof (long) + 1];
  int len = __objc_code_long (buf, value);
  return (*stream->write) (stream->physical, (char*)buf, len);
}


int
objc_write_string (struct objc_typed_stream *stream,
		   const unsigned char *string, unsigned int nbytes)
{
  unsigned char buf[sizeof (unsigned int) + 1];
  int len = __objc_code_unsigned_int (buf, nbytes);
  
  if ((buf[0]&_B_CODE) == _B_SINT)
    buf[0] = (buf[0]&_B_VALUE)|_B_SSTR;

  else /* _B_NINT */
    buf[0] = (buf[0]&_B_VALUE)|_B_NSTR;

  if ((*stream->write) (stream->physical, (char*)buf, len) != 0)
    return (*stream->write) (stream->physical, (char*)string, nbytes);
  else
    return 0;
}

int
objc_write_string_atomic (struct objc_typed_stream *stream,
			  unsigned char *string, unsigned int nbytes)
{
  unsigned long key;
  if ((key = PTR2LONG(objc_hash_value_for_key (stream->stream_table, string))))
    return objc_write_use_common (stream, key);
  else
    {
      int length;
      objc_hash_add (&stream->stream_table,
		     LONG2PTR(key=PTR2LONG(string)), string);
      if ((length = objc_write_register_common (stream, key)))
	return objc_write_string (stream, string, nbytes);
      return length;
    }
}

static int
objc_write_register_common (struct objc_typed_stream *stream, 
			    unsigned long key)
{
  unsigned char buf[sizeof (unsigned long)+2];
  int len = __objc_code_unsigned_long (buf + 1, key);
  if (len == 1)
    {
      buf[0] = _B_RCOMM|0x01;
      buf[1] &= _B_VALUE;
      return (*stream->write) (stream->physical, (char*)buf, len + 1);
    }
  else
    {
      buf[1] = (buf[1]&_B_VALUE)|_B_RCOMM;
      return (*stream->write) (stream->physical, (char*)buf + 1, len);
    }
}

static int
objc_write_use_common (struct objc_typed_stream *stream, unsigned long key)
{
  unsigned char buf[sizeof (unsigned long)+2];
  int len = __objc_code_unsigned_long (buf + 1, key);
  if (len == 1)
    {
      buf[0] = _B_UCOMM|0x01;
      buf[1] &= _B_VALUE;
      return (*stream->write) (stream->physical, (char*)buf, 2);
    }
  else
    {
      buf[1] = (buf[1]&_B_VALUE)|_B_UCOMM;
      return (*stream->write) (stream->physical, (char*)buf + 1, len);
    }
}

static inline int
__objc_write_extension (struct objc_typed_stream *stream, unsigned char code)
{
  if (code <= _B_VALUE)
    {
      unsigned char buf = code|_B_EXT;
      return (*stream->write) (stream->physical, (char*)&buf, 1);
    }
  else 
    {
      objc_error (nil, OBJC_ERR_BAD_OPCODE,
		  "__objc_write_extension: bad opcode %c\n", code);
      return -1;
    }
}

inline int
__objc_write_object (struct objc_typed_stream *stream, id object)
{
  unsigned char buf = '\0';
  SEL write_sel = sel_get_any_uid ("write:");
  if (object)
    {
      __objc_write_extension (stream, _BX_OBJECT);
      objc_write_class (stream, object->class_pointer);
      (*objc_msg_lookup (object, write_sel)) (object, write_sel, stream);
      return (*stream->write) (stream->physical, (char*)&buf, 1);
    }
  else
    return objc_write_use_common (stream, 0);
}

int 
objc_write_object_reference (struct objc_typed_stream *stream, id object)
{
  unsigned long key;
  if ((key = PTR2LONG(objc_hash_value_for_key (stream->object_table, object))))
    return objc_write_use_common (stream, key);

  __objc_write_extension (stream, _BX_OBJREF);
  return objc_write_unsigned_long (stream, PTR2LONG (object));
}

int 
objc_write_root_object (struct objc_typed_stream *stream, id object)
{
  int len = 0;
  if (stream->writing_root_p)
    objc_error (nil, OBJC_ERR_RECURSE_ROOT, 
		"objc_write_root_object called recursively");
  else
    {
      stream->writing_root_p = 1;
      __objc_write_extension (stream, _BX_OBJROOT);
      if ((len = objc_write_object (stream, object)))
	__objc_finish_write_root_object (stream);
      stream->writing_root_p = 0;
    }
  return len;
}

int 
objc_write_object (struct objc_typed_stream *stream, id object)
{
  unsigned long key;
  if ((key = PTR2LONG(objc_hash_value_for_key (stream->object_table, object))))
    return objc_write_use_common (stream, key);

  else if (object == nil)
    return objc_write_use_common (stream, 0);

  else
    {
      int length;
      objc_hash_add (&stream->object_table,
		     LONG2PTR(key=PTR2LONG(object)), object);
      if ((length = objc_write_register_common (stream, key)))
	return __objc_write_object (stream, object);
      return length;
    }
}

inline int
__objc_write_class (struct objc_typed_stream *stream, struct objc_class *class)
{
  __objc_write_extension (stream, _BX_CLASS);
  objc_write_string_atomic (stream, (unsigned char *) class->name,
			   strlen ((char *) class->name));
  return objc_write_unsigned_long (stream, class->version);
}


static int 
objc_write_class (struct objc_typed_stream *stream,
			 struct objc_class *class)
{
  unsigned long key;
  if ((key = PTR2LONG(objc_hash_value_for_key (stream->stream_table, class))))
    return objc_write_use_common (stream, key);
  else
    {
      int length;
      objc_hash_add (&stream->stream_table,
		     LONG2PTR(key = PTR2LONG(class)), class);
      if ((length = objc_write_register_common (stream, key)))
	return __objc_write_class (stream, class);
      return length;
    }
}


inline int 
__objc_write_selector (struct objc_typed_stream *stream, SEL selector)
{
  const char *sel_name;
  __objc_write_extension (stream, _BX_SEL);
  /* to handle NULL selectors */
  if ((SEL)0 == selector)
    return objc_write_string (stream, (unsigned char*)"", 0);
  sel_name = sel_get_name (selector);
  return objc_write_string (stream, (unsigned char*)sel_name, strlen ((char*)sel_name));
}

int 
objc_write_selector (struct objc_typed_stream *stream, SEL selector)
{
  const char *sel_name;
  unsigned long key;

  /* to handle NULL selectors */
  if ((SEL)0 == selector)
    return __objc_write_selector (stream, selector);

  sel_name = sel_get_name (selector);
  if ((key = PTR2LONG(objc_hash_value_for_key (stream->stream_table,
					       sel_name))))
    return objc_write_use_common (stream, key);
  else
    {
      int length;
      objc_hash_add (&stream->stream_table, 
		LONG2PTR(key = PTR2LONG(sel_name)), (char *) sel_name);
      if ((length = objc_write_register_common (stream, key)))
	return __objc_write_selector (stream, selector);
      return length;
    }
}



/*
** Read operations 
*/

inline int
objc_read_char (struct objc_typed_stream *stream, char *val)
{
  unsigned char buf;
  int len;
  len = (*stream->read) (stream->physical, (char*)&buf, 1);
  if (len != 0)
    {
      if ((buf & _B_CODE) == _B_SINT)
	(*val) = (buf & _B_VALUE);

      else if ((buf & _B_NUMBER) == 1)
	{
	  len = (*stream->read) (stream->physical, val, 1);
	  if (buf&_B_SIGN)
	    (*val) = -1 * (*val);
	}

      else
	objc_error (nil, OBJC_ERR_BAD_DATA,
		    "expected 8bit signed int, got %dbit int",
		    (int) (buf&_B_NUMBER)*8);
    }
  return len;
}


inline int
objc_read_unsigned_char (struct objc_typed_stream *stream, unsigned char *val)
{
  unsigned char buf;
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)&buf, 1)))
    {
      if ((buf & _B_CODE) == _B_SINT)
	(*val) = (buf & _B_VALUE);

      else if ((buf & _B_NUMBER) == 1)
	len = (*stream->read) (stream->physical, (char*)val, 1);

      else
	objc_error (nil, OBJC_ERR_BAD_DATA,
		    "expected 8bit unsigned int, got %dbit int",
		    (int) (buf&_B_NUMBER)*8);
    }
  return len;
}

inline int
objc_read_short (struct objc_typed_stream *stream, short *value)
{
  unsigned char buf[sizeof (short) + 1];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	{
	  int pos = 1;
	  int nbytes = buf[0] & _B_NUMBER;
	  if (nbytes > (int) sizeof (short))
	    objc_error (nil, OBJC_ERR_BAD_DATA,
		        "expected short, got bigger (%dbits)", nbytes*8);
	  len = (*stream->read) (stream->physical, (char*)buf + 1, nbytes);
	  (*value) = 0;
	  while (pos <= nbytes)
	    (*value) = ((*value)*0x100) + buf[pos++];
	  if (buf[0] & _B_SIGN)
	    (*value) = -(*value);
	}
    }
  return len;
}

inline int
objc_read_unsigned_short (struct objc_typed_stream *stream,
			  unsigned short *value)
{
  unsigned char buf[sizeof (unsigned short) + 1];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	{
	  int pos = 1;
	  int nbytes = buf[0] & _B_NUMBER;
	  if (nbytes > (int) sizeof (short))
	    objc_error (nil, OBJC_ERR_BAD_DATA,
		        "expected short, got int or bigger");
	  len = (*stream->read) (stream->physical, (char*)buf + 1, nbytes);
	  (*value) = 0;
	  while (pos <= nbytes)
	    (*value) = ((*value)*0x100) + buf[pos++];
	}
    }
  return len;
}


inline int
objc_read_int (struct objc_typed_stream *stream, int *value)
{
  unsigned char buf[sizeof (int) + 1];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	{
	  int pos = 1;
	  int nbytes = buf[0] & _B_NUMBER;
	  if (nbytes > (int) sizeof (int))
	    objc_error (nil, OBJC_ERR_BAD_DATA, "expected int, got bigger");
	  len = (*stream->read) (stream->physical, (char*)buf + 1, nbytes);
	  (*value) = 0;
	  while (pos <= nbytes)
	    (*value) = ((*value)*0x100) + buf[pos++];
	  if (buf[0] & _B_SIGN)
	    (*value) = -(*value);
	}
    }
  return len;
}

inline int
objc_read_long (struct objc_typed_stream *stream, long *value)
{
  unsigned char buf[sizeof (long) + 1];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	{
	  int pos = 1;
	  int nbytes = buf[0] & _B_NUMBER;
	  if (nbytes > (int) sizeof (long))
	    objc_error (nil, OBJC_ERR_BAD_DATA, "expected long, got bigger");
	  len = (*stream->read) (stream->physical, (char*)buf + 1, nbytes);
	  (*value) = 0;
	  while (pos <= nbytes)
	    (*value) = ((*value)*0x100) + buf[pos++];
	  if (buf[0] & _B_SIGN)
	    (*value) = -(*value);
	}
    }
  return len;
}

inline int
__objc_read_nbyte_uint (struct objc_typed_stream *stream,
			unsigned int nbytes, unsigned int *val)
{
  int len;
  unsigned int pos = 0;
  unsigned char buf[sizeof (unsigned int) + 1];

  if (nbytes > sizeof (int))
    objc_error (nil, OBJC_ERR_BAD_DATA, "expected int, got bigger");

  len = (*stream->read) (stream->physical, (char*)buf, nbytes);
  (*val) = 0;
  while (pos < nbytes)
    (*val) = ((*val)*0x100) + buf[pos++];
  return len;
}
  

inline int
objc_read_unsigned_int (struct objc_typed_stream *stream,
			unsigned int *value)
{
  unsigned char buf[sizeof (unsigned int) + 1];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	len = __objc_read_nbyte_uint (stream, (buf[0] & _B_VALUE), value);

    }
  return len;
}

int
__objc_read_nbyte_ulong (struct objc_typed_stream *stream,
		       unsigned int nbytes, unsigned long *val)
{
  int len;
  unsigned int pos = 0;
  unsigned char buf[sizeof (unsigned long) + 1];

  if (nbytes > sizeof (long))
    objc_error (nil, OBJC_ERR_BAD_DATA, "expected long, got bigger");

  len = (*stream->read) (stream->physical, (char*)buf, nbytes);
  (*val) = 0;
  while (pos < nbytes)
    (*val) = ((*val)*0x100) + buf[pos++];
  return len;
}
  

inline int
objc_read_unsigned_long (struct objc_typed_stream *stream,
			 unsigned long *value)
{
  unsigned char buf[sizeof (unsigned long) + 1];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      if ((buf[0] & _B_CODE) == _B_SINT)
	(*value) = (buf[0] & _B_VALUE);

      else
	len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), value);

    }
  return len;
}

inline int
objc_read_string (struct objc_typed_stream *stream,
		  char **string)
{
  unsigned char buf[sizeof (unsigned int) + 1];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      unsigned long key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register following */
	{
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read) (stream->physical, (char*)buf, 1);
	}

      switch (buf[0]&_B_CODE) {
      case _B_SSTR:
	{
	  int length = buf[0]&_B_VALUE;
	  (*string) = (char*)objc_malloc (length + 1);
	  if (key)
	    objc_hash_add (&stream->stream_table, LONG2PTR(key), *string);
	  len = (*stream->read) (stream->physical, *string, length);
	  (*string)[length] = '\0';
	}
	break;

      case _B_UCOMM:
	{
	  char *tmp;
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  tmp = objc_hash_value_for_key (stream->stream_table, LONG2PTR (key));
	  *string = objc_malloc (strlen (tmp) + 1);
	  strcpy (*string, tmp);
	}
	break;

      case _B_NSTR:
	{
	  unsigned int nbytes = buf[0]&_B_VALUE;
	  len = __objc_read_nbyte_uint (stream, nbytes, &nbytes);
	  if (len) {
	    (*string) = (char*)objc_malloc (nbytes + 1);
	    if (key)
	      objc_hash_add (&stream->stream_table, LONG2PTR(key), *string);
	    len = (*stream->read) (stream->physical, *string, nbytes);
	    (*string)[nbytes] = '\0';
	  }
	}
	break;
	
      default:
	objc_error (nil, OBJC_ERR_BAD_DATA,
		    "expected string, got opcode %c\n", (buf[0]&_B_CODE));
      }
    }

  return len;
}


int
objc_read_object (struct objc_typed_stream *stream, id *object)
{
  unsigned char buf[sizeof (unsigned int)];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      SEL read_sel = sel_get_any_uid ("read:");
      unsigned long key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register common */
	{
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read) (stream->physical, (char*)buf, 1);
	}

      if (buf[0] == (_B_EXT | _BX_OBJECT))
	{
	  Class class;

	  /* get class */
	  len = objc_read_class (stream, &class);

	  /* create instance */
	  (*object) = class_create_instance (class);

	  /* register? */
	  if (key)
	    objc_hash_add (&stream->object_table, LONG2PTR(key), *object);

	  /* send -read: */
	  if (__objc_responds_to (*object, read_sel))
	    (*get_imp (class, read_sel)) (*object, read_sel, stream);

	  /* check null-byte */
	  len = (*stream->read) (stream->physical, (char*)buf, 1);
	  if (buf[0] != '\0')
	    objc_error (nil, OBJC_ERR_BAD_DATA,
		        "expected null-byte, got opcode %c", buf[0]);
	}

      else if ((buf[0]&_B_CODE) == _B_UCOMM)
	{
	  if (key)
	    objc_error (nil, OBJC_ERR_BAD_KEY, "cannot register use upcode...");
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  (*object) = objc_hash_value_for_key (stream->object_table,
					       LONG2PTR(key));
	}

      else if (buf[0] == (_B_EXT | _BX_OBJREF))	/* a forward reference */
	{
	  struct objc_list *other;
	  len = objc_read_unsigned_long (stream, &key);
	  other 
	    = (struct objc_list *) objc_hash_value_for_key (stream->object_refs, 
							   LONG2PTR(key));
	  objc_hash_add (&stream->object_refs, LONG2PTR(key), 
			 (void *)list_cons (object, other));
	}

      else if (buf[0] == (_B_EXT | _BX_OBJROOT)) /* a root object */
	{
	  if (key)
	    objc_error (nil, OBJC_ERR_BAD_KEY,
		        "cannot register root object...");
	  len = objc_read_object (stream, object);
	  __objc_finish_read_root_object (stream);
	}

      else
	objc_error (nil, OBJC_ERR_BAD_DATA,
		    "expected object, got opcode %c", buf[0]);
    }
  return len;
}

static int
objc_read_class (struct objc_typed_stream *stream, Class *class)
{
  unsigned char buf[sizeof (unsigned int)];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      unsigned long key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register following */
	{
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read) (stream->physical, (char*)buf, 1);
	}

      if (buf[0] == (_B_EXT | _BX_CLASS))
	{
	  char temp[1] = "";
	  char *class_name = temp;
	  unsigned long version;

	  /* get class */
	  len = objc_read_string (stream, &class_name);
	  (*class) = objc_get_class (class_name);
	  objc_free (class_name);

	  /* register */
	  if (key)
	    objc_hash_add (&stream->stream_table, LONG2PTR(key), *class);

	  objc_read_unsigned_long (stream, &version);
	  objc_hash_add (&stream->class_table,
			 (*class)->name, (void *)version);
	}

      else if ((buf[0]&_B_CODE) == _B_UCOMM)
	{
	  if (key)
	    objc_error (nil, OBJC_ERR_BAD_KEY, "cannot register use upcode...");
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  *class = objc_hash_value_for_key (stream->stream_table,
					    LONG2PTR(key));
	  if (! *class)
	    objc_error (nil, OBJC_ERR_BAD_CLASS,
		        "cannot find class for key %lu", key);
	}

      else
	objc_error (nil, OBJC_ERR_BAD_DATA,
		    "expected class, got opcode %c", buf[0]);
    }
  return len;
}

int
objc_read_selector (struct objc_typed_stream *stream, SEL* selector)
{
  unsigned char buf[sizeof (unsigned int)];
  int len;
  if ((len = (*stream->read) (stream->physical, (char*)buf, 1)))
    {
      unsigned long key = 0;

      if ((buf[0]&_B_CODE) == _B_RCOMM)	/* register following */
	{
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  len = (*stream->read) (stream->physical, (char*)buf, 1);
	}

      if (buf[0] == (_B_EXT|_BX_SEL)) /* selector! */
	{
	  char temp[1] = "";
	  char *selector_name = temp;

	  /* get selector */
	  len = objc_read_string (stream, &selector_name);
	  /* To handle NULL selectors */
	  if (0 == strlen (selector_name))
	    {
	      (*selector) = (SEL)0;
	      return 0;
	    }
	  else 
	    (*selector) = sel_get_any_uid (selector_name);
	  objc_free (selector_name);

	  /* register */
	  if (key)
	    objc_hash_add (&stream->stream_table,
			   LONG2PTR(key), (void *) *selector);
	}

      else if ((buf[0]&_B_CODE) == _B_UCOMM)
	{
	  if (key)
	    objc_error (nil, OBJC_ERR_BAD_KEY, "cannot register use upcode...");
	  len = __objc_read_nbyte_ulong (stream, (buf[0] & _B_VALUE), &key);
	  (*selector) = objc_hash_value_for_key (stream->stream_table, 
						 LONG2PTR(key));
	}

      else
	objc_error (nil, OBJC_ERR_BAD_DATA,
		    "expected selector, got opcode %c", buf[0]);
    }
  return len;
}

/*
** USER LEVEL FUNCTIONS
*/

/*
** Write one object, encoded in TYPE and pointed to by DATA to the
** typed stream STREAM.  
*/

int
objc_write_type (TypedStream *stream, const char *type, const void *data)
{
  switch (*type) {
  case _C_ID:
    return objc_write_object (stream, *(id *) data);
    break;

  case _C_CLASS:
    return objc_write_class (stream, *(Class *) data);
    break;

  case _C_SEL:
    return objc_write_selector (stream, *(SEL *) data);
    break;

  case _C_CHR:
    return objc_write_char (stream, *(signed char *) data);
    break;
    
  case _C_UCHR:
    return objc_write_unsigned_char (stream, *(unsigned char *) data);
    break;

  case _C_SHT:
    return objc_write_short (stream, *(short *) data);
    break;

  case _C_USHT:
    return objc_write_unsigned_short (stream, *(unsigned short *) data);
    break;

  case _C_INT:
    return objc_write_int (stream, *(int *) data);
    break;

  case _C_UINT:
    return objc_write_unsigned_int (stream, *(unsigned int *) data);
    break;

  case _C_LNG:
    return objc_write_long (stream, *(long *) data);
    break;

  case _C_ULNG:
    return objc_write_unsigned_long (stream, *(unsigned long *) data);
    break;

  case _C_CHARPTR:
    return objc_write_string (stream,
			      *(unsigned char **) data, strlen (*(char **) data));
    break;

  case _C_ATOM:
    return objc_write_string_atomic (stream, *(unsigned char **) data, 
				     strlen (*(char **) data));
    break;

  case _C_ARY_B:
    {
      int len = atoi (type + 1);
      while (isdigit ((unsigned char) *++type))
	;
      return objc_write_array (stream, type, len, data);
    }
    break; 

  case _C_STRUCT_B:
    {
      int acc_size = 0;
      int align;
      while (*type != _C_STRUCT_E && *type++ != '=')
	; /* skip "<name>=" */
      while (*type != _C_STRUCT_E)
	{
	  align = objc_alignof_type (type);       /* padd to alignment */
	  acc_size += ROUND (acc_size, align);
	  objc_write_type (stream, type, ((char *) data) + acc_size);
	  acc_size += objc_sizeof_type (type);   /* add component size */
	  type = objc_skip_typespec (type);	 /* skip component */
	}
      return 1;
    }

  default:
    {
      objc_error (nil, OBJC_ERR_BAD_TYPE,
		  "objc_write_type: cannot parse typespec: %s\n", type);
      return 0;
    }
  }
}

/*
** Read one object, encoded in TYPE and pointed to by DATA to the
** typed stream STREAM.  DATA specifies the address of the types to
** read.  Expected type is checked against the type actually present
** on the stream. 
*/

int
objc_read_type(TypedStream *stream, const char *type, void *data)
{
  char c;
  switch (c = *type) {
  case _C_ID:
    return objc_read_object (stream, (id*)data);
    break;

  case _C_CLASS:
    return objc_read_class (stream, (Class*)data);
    break;

  case _C_SEL:
    return objc_read_selector (stream, (SEL*)data);
    break;

  case _C_CHR:
    return objc_read_char (stream, (char*)data);
    break;
    
  case _C_UCHR:
    return objc_read_unsigned_char (stream, (unsigned char*)data);
    break;

  case _C_SHT:
    return objc_read_short (stream, (short*)data);
    break;

  case _C_USHT:
    return objc_read_unsigned_short (stream, (unsigned short*)data);
    break;

  case _C_INT:
    return objc_read_int (stream, (int*)data);
    break;

  case _C_UINT:
    return objc_read_unsigned_int (stream, (unsigned int*)data);
    break;

  case _C_LNG:
    return objc_read_long (stream, (long*)data);
    break;

  case _C_ULNG:
    return objc_read_unsigned_long (stream, (unsigned long*)data);
    break;

  case _C_CHARPTR:
  case _C_ATOM:
    return objc_read_string (stream, (char**)data);
    break;

  case _C_ARY_B:
    {
      int len = atoi (type + 1);
      while (isdigit ((unsigned char) *++type))
	;
      return objc_read_array (stream, type, len, data);
    }
    break; 

  case _C_STRUCT_B:
    {
      int acc_size = 0;
      int align;
      while (*type != _C_STRUCT_E && *type++ != '=')
	; /* skip "<name>=" */
      while (*type != _C_STRUCT_E)
	{
	  align = objc_alignof_type (type);       /* padd to alignment */
	  acc_size += ROUND (acc_size, align);
	  objc_read_type (stream, type, ((char*)data)+acc_size);
	  acc_size += objc_sizeof_type (type);   /* add component size */
	  type = objc_skip_typespec (type);	 /* skip component */
	}
      return 1;
    }

  default:
    {
      objc_error (nil, OBJC_ERR_BAD_TYPE,
		  "objc_read_type: cannot parse typespec: %s\n", type);
      return 0;
    }
  }
}

/*
** Write the object specified by the template TYPE to STREAM.  Last
** arguments specify addresses of values to be written.  It might 
** seem surprising to specify values by address, but this is extremely
** convenient for copy-paste with objc_read_types calls.  A more
** down-to-the-earth cause for this passing of addresses is that values
** of arbitrary size is not well supported in ANSI C for functions with
** variable number of arguments.
*/

int 
objc_write_types (TypedStream *stream, const char *type, ...)
{
  va_list args;
  const char *c;
  int res = 0;

  va_start(args, type);

  for (c = type; *c; c = objc_skip_typespec (c))
    {
      switch (*c) {
      case _C_ID:
	res = objc_write_object (stream, *va_arg (args, id*));
	break;

      case _C_CLASS:
	res = objc_write_class (stream, *va_arg (args, Class*));
	break;

      case _C_SEL:
	res = objc_write_selector (stream, *va_arg (args, SEL*));
	break;
	
      case _C_CHR:
	res = objc_write_char (stream, *va_arg (args, char*));
	break;
	
      case _C_UCHR:
	res = objc_write_unsigned_char (stream,
					*va_arg (args, unsigned char*));
	break;
	
      case _C_SHT:
	res = objc_write_short (stream, *va_arg (args, short*));
	break;

      case _C_USHT:
	res = objc_write_unsigned_short (stream,
					 *va_arg (args, unsigned short*));
	break;

      case _C_INT:
	res = objc_write_int(stream, *va_arg (args, int*));
	break;
	
      case _C_UINT:
	res = objc_write_unsigned_int(stream, *va_arg (args, unsigned int*));
	break;

      case _C_LNG:
	res = objc_write_long(stream, *va_arg (args, long*));
	break;
	
      case _C_ULNG:
	res = objc_write_unsigned_long(stream, *va_arg (args, unsigned long*));
	break;

      case _C_CHARPTR:
	{
	  unsigned char **str = va_arg (args, unsigned char **);
	  res = objc_write_string (stream, *str, strlen ((char*)*str));
	}
	break;

      case _C_ATOM:
	{
	  unsigned char **str = va_arg (args, unsigned char **);
	  res = objc_write_string_atomic (stream, *str, strlen ((char*)*str));
	}
	break;

      case _C_ARY_B:
	{
	  int len = atoi (c + 1);
	  const char *t = c;
	  while (isdigit ((unsigned char) *++t))
	    ;
	  res = objc_write_array (stream, t, len, va_arg (args, void *));
	  t = objc_skip_typespec (t);
	  if (*t != _C_ARY_E)
	    objc_error (nil, OBJC_ERR_BAD_TYPE, "expected `]', got: %s", t);
	}
	break; 
	
      default:
	objc_error (nil, OBJC_ERR_BAD_TYPE, 
		    "objc_write_types: cannot parse typespec: %s\n", type);
      }
    }
  va_end(args);
  return res;
}


/* 
** Last arguments specify addresses of values to be read.  Expected
** type is checked against the type actually present on the stream. 
*/

int 
objc_read_types(TypedStream *stream, const char *type, ...)
{
  va_list args;
  const char *c;
  int res = 0;

  va_start (args, type);

  for (c = type; *c; c = objc_skip_typespec(c))
    {
      switch (*c) {
      case _C_ID:
	res = objc_read_object(stream, va_arg (args, id*));
	break;

      case _C_CLASS:
	res = objc_read_class(stream, va_arg (args, Class*));
	break;

      case _C_SEL:
	res = objc_read_selector(stream, va_arg (args, SEL*));
	break;
	
      case _C_CHR:
	res = objc_read_char(stream, va_arg (args, char*));
	break;
	
      case _C_UCHR:
	res = objc_read_unsigned_char(stream, va_arg (args, unsigned char*));
	break;
	
      case _C_SHT:
	res = objc_read_short(stream, va_arg (args, short*));
	break;

      case _C_USHT:
	res = objc_read_unsigned_short(stream, va_arg (args, unsigned short*));
	break;

      case _C_INT:
	res = objc_read_int(stream, va_arg (args, int*));
	break;
	
      case _C_UINT:
	res = objc_read_unsigned_int(stream, va_arg (args, unsigned int*));
	break;

      case _C_LNG:
	res = objc_read_long(stream, va_arg (args, long*));
	break;
	
      case _C_ULNG:
	res = objc_read_unsigned_long(stream, va_arg (args, unsigned long*));
	break;

      case _C_CHARPTR:
      case _C_ATOM:
	{
	  char **str = va_arg (args, char **);
	  res = objc_read_string (stream, str);
	}
	break;

      case _C_ARY_B:
	{
	  int len = atoi (c + 1);
	  const char *t = c;
	  while (isdigit ((unsigned char) *++t))
	    ;
	  res = objc_read_array (stream, t, len, va_arg (args, void *));
	  t = objc_skip_typespec (t);
	  if (*t != _C_ARY_E)
	    objc_error (nil, OBJC_ERR_BAD_TYPE, "expected `]', got: %s", t);
	}
	break; 
	
      default:
	objc_error (nil, OBJC_ERR_BAD_TYPE, 
		    "objc_read_types: cannot parse typespec: %s\n", type);
      }
    }
  va_end (args);
  return res;
}

/*
** Write an array of COUNT elements of TYPE from the memory address DATA.
** This is equivalent of objc_write_type (stream, "[N<type>]", data)
*/

int
objc_write_array (TypedStream *stream, const char *type,
		  int count, const void *data)
{
  int off = objc_sizeof_type(type);
  const char *where = data;

  while (count-- > 0)
    {
      objc_write_type(stream, type, where);
      where += off;
    }
  return 1;
}

/*
** Read an array of COUNT elements of TYPE into the memory address
** DATA.  The memory pointed to by data is supposed to be allocated
** by the callee.  This is equivalent of 
**   objc_read_type (stream, "[N<type>]", data)
*/

int
objc_read_array (TypedStream *stream, const char *type,
		 int count, void *data)
{
  int off = objc_sizeof_type(type);
  char *where = (char*)data;

  while (count-- > 0)
    {
      objc_read_type(stream, type, where);
      where += off;
    }
  return 1;
}

static int 
__objc_fread (FILE *file, char *data, int len)
{
  return fread(data, len, 1, file);
}

static int 
__objc_fwrite (FILE *file, char *data, int len)
{
  return fwrite(data, len, 1, file);
}

static int
__objc_feof (FILE *file)
{
  return feof(file);
}

static int 
__objc_no_write (FILE *file __attribute__ ((__unused__)),
		 const char *data __attribute__ ((__unused__)),
		 int len __attribute__ ((__unused__)))
{
  objc_error (nil, OBJC_ERR_NO_WRITE, "TypedStream not open for writing");
  return 0;
}

static int 
__objc_no_read (FILE *file __attribute__ ((__unused__)),
		const char *data __attribute__ ((__unused__)),
		int len __attribute__ ((__unused__)))
{
  objc_error (nil, OBJC_ERR_NO_READ, "TypedStream not open for reading");
  return 0;
}

static int
__objc_read_typed_stream_signature (TypedStream *stream)
{
  char buffer[80];
  int pos = 0;
  do
    (*stream->read) (stream->physical, buffer+pos, 1);
  while (buffer[pos++] != '\0')
    ;
  sscanf (buffer, "GNU TypedStream %d", &stream->version);
  if (stream->version != OBJC_TYPED_STREAM_VERSION)
    objc_error (nil, OBJC_ERR_STREAM_VERSION,
		"cannot handle TypedStream version %d", stream->version);
  return 1;
}

static int
__objc_write_typed_stream_signature (TypedStream *stream)
{
  char buffer[80];
  sprintf(buffer, "GNU TypedStream %d", OBJC_TYPED_STREAM_VERSION);
  stream->version = OBJC_TYPED_STREAM_VERSION;
  (*stream->write) (stream->physical, buffer, strlen (buffer) + 1);
  return 1;
}

static void __objc_finish_write_root_object(struct objc_typed_stream *stream)
{
  objc_hash_delete (stream->object_table);
  stream->object_table = objc_hash_new (64,
					(hash_func_type) objc_hash_ptr,
					(compare_func_type) objc_compare_ptrs);
}

static void __objc_finish_read_root_object(struct objc_typed_stream *stream)
{
  node_ptr node;
  SEL awake_sel = sel_get_any_uid ("awake");
  cache_ptr free_list = objc_hash_new (64,
				       (hash_func_type) objc_hash_ptr,
				       (compare_func_type) objc_compare_ptrs);

  /* resolve object forward references */
  for (node = objc_hash_next (stream->object_refs, NULL); node;
       node = objc_hash_next (stream->object_refs, node))
    {
      struct objc_list *reflist = node->value;
      const void *key = node->key;
      id object = objc_hash_value_for_key (stream->object_table, key);
      while (reflist)
	{
	  *((id*) reflist->head) = object;
	  if (objc_hash_value_for_key (free_list,reflist) == NULL)
	    objc_hash_add (&free_list,reflist,reflist);

	  reflist = reflist->tail;
	}
    }
    
  /* apply __objc_free to all objects stored in free_list */
  for (node = objc_hash_next (free_list, NULL); node;
       node = objc_hash_next (free_list, node))
    objc_free ((void *) node->key);

  objc_hash_delete (free_list);

  /* empty object reference table */
  objc_hash_delete (stream->object_refs);
  stream->object_refs = objc_hash_new (8, (hash_func_type) objc_hash_ptr,
				       (compare_func_type) objc_compare_ptrs);
  
  /* call -awake for all objects read  */
  if (awake_sel)
    {
      for (node = objc_hash_next (stream->object_table, NULL); node;
	   node = objc_hash_next (stream->object_table, node))
	{
	  id object = node->value;
	  if (__objc_responds_to (object, awake_sel))
	    (*objc_msg_lookup (object, awake_sel)) (object, awake_sel);
	}
    }

  /* empty object table */
  objc_hash_delete (stream->object_table);
  stream->object_table = objc_hash_new(64,
				       (hash_func_type)objc_hash_ptr,
				       (compare_func_type)objc_compare_ptrs);
}

/*
** Open the stream PHYSICAL in MODE
*/

TypedStream *
objc_open_typed_stream (FILE *physical, int mode)
{
  TypedStream *s = (TypedStream *) objc_malloc (sizeof (TypedStream));

  s->mode = mode;
  s->physical = physical;
  s->stream_table = objc_hash_new (64,
				   (hash_func_type) objc_hash_ptr,
				   (compare_func_type) objc_compare_ptrs);
  s->object_table = objc_hash_new (64,
				   (hash_func_type) objc_hash_ptr,
				   (compare_func_type) objc_compare_ptrs);
  s->eof = (objc_typed_eof_func) __objc_feof;
  s->flush = (objc_typed_flush_func) fflush;
  s->writing_root_p = 0;
  if (mode == OBJC_READONLY)
    {
      s->class_table 
	= objc_hash_new (8, (hash_func_type) objc_hash_string,
			 (compare_func_type) objc_compare_strings);
      s->object_refs = objc_hash_new (8, (hash_func_type) objc_hash_ptr,
				      (compare_func_type) objc_compare_ptrs);
      s->read = (objc_typed_read_func) __objc_fread;
      s->write = (objc_typed_write_func) __objc_no_write;
      __objc_read_typed_stream_signature (s);
    }
  else if (mode == OBJC_WRITEONLY)
    {
      s->class_table = 0;
      s->object_refs = 0;
      s->read = (objc_typed_read_func) __objc_no_read;
      s->write = (objc_typed_write_func) __objc_fwrite;
      __objc_write_typed_stream_signature (s);
    }      
  else
    {
      objc_close_typed_stream (s);
      return NULL;
    }
  s->type = OBJC_FILE_STREAM;
  return s;
}

/*
** Open the file named by FILE_NAME in MODE
*/

TypedStream*
objc_open_typed_stream_for_file (const char *file_name, int mode)
{
  FILE *file = NULL;
  TypedStream *s;

  if (mode == OBJC_READONLY)
    file = fopen (file_name, "r");
  else
    file = fopen (file_name, "w");

  if (file)
    {
      s = objc_open_typed_stream (file, mode);
      if (s)
	s->type |= OBJC_MANAGED_STREAM;
      return s;
    }
  else
    return NULL;
}

/*
** Close STREAM freeing the structure it self.  If it was opened with 
** objc_open_typed_stream_for_file, the file will also be closed.
*/

void
objc_close_typed_stream (TypedStream *stream)
{
  if (stream->mode == OBJC_READONLY)
    {
      __objc_finish_read_root_object (stream); /* Just in case... */
      objc_hash_delete (stream->class_table);
      objc_hash_delete (stream->object_refs);
    }

  objc_hash_delete (stream->stream_table);
  objc_hash_delete (stream->object_table);

  if (stream->type == (OBJC_MANAGED_STREAM | OBJC_FILE_STREAM))
    fclose ((FILE *)stream->physical);

  objc_free(stream);
}

BOOL
objc_end_of_typed_stream (TypedStream *stream)
{
  return (*stream->eof) (stream->physical);
}

void
objc_flush_typed_stream (TypedStream *stream)
{
  (*stream->flush) (stream->physical);
}

long
objc_get_stream_class_version (TypedStream *stream, Class class)
{
  if (stream->class_table)
    return PTR2LONG(objc_hash_value_for_key (stream->class_table,
					     class->name));
  else
    return class_get_version (class);
}

