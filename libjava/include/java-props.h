// java-props.h - Properties  -*- c++ -*-

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_PROPS_H__
#define __JAVA_PROPS_H__

typedef struct
{
  char *key;
  size_t      key_length;
  char *value;
  size_t      value_length;
} property_pair;

// Set to NULL-terminated list of properties set at compile time.
extern const char **_Jv_Compiler_Properties;

// Properties taken from the user's environment.
extern property_pair *_Jv_Environment_Properties;

#endif

