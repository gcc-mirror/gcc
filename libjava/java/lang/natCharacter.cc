// natCharacter.cc - Native part of Character class.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Character.h>

#include <java-chartables.h>



#define asize(x)  ((sizeof (x)) / sizeof (x[0]))

static jchar
to_lower_title (jchar ch)
{
  for (unsigned int i = 0; i < asize (title_to_upper_table); ++i)
    {
      // We can assume that the entries in the two tables are
      // parallel.  This is checked in the script.
      if (title_to_upper_table[i][1] == ch
	  || title_to_upper_table[i][0] == ch)
	return title_to_lower_table[i][1];
    }
  return ch;
}

static jchar
to_upper_title (jchar ch)
{
  for (unsigned int i = 0; i < asize (title_to_lower_table); ++i)
    {
      // We can assume that the entries in the two tables are
      // parallel.  This is checked in the script.
      if (title_to_lower_table[i][1] == ch
	  || title_to_lower_table[i][0] == ch)
	return title_to_upper_table[i][1];
    }
  return ch;
}

jboolean
java::lang::Character::isTitleCase (jchar ch)
{
  for (unsigned int i = 0; i < asize (title_to_lower_table); ++i)
    {
      if (title_to_lower_table[i][0] == ch)
	return true;
    }
  return false;
}

jchar
java::lang::Character::toTitleCase (jchar ch)
{
  // Both titlecase mapping tables have the same length.  This is
  // checked in the chartables script.
  for (unsigned int i = 0; i < asize (title_to_lower_table); ++i)
    {
      if (title_to_lower_table[i][0] == ch)
	return ch;
      if (title_to_lower_table[i][1] == ch)
	return title_to_lower_table[i][0];
      if (title_to_upper_table[i][1] == ch)
	return title_to_upper_table[i][0];
    }
  return toUpperCase (ch);
}

#ifdef COMPACT_CHARACTER

static int
table_search (const jchar table[][2], int table_len, jchar ch)
{
  int low, high, i, old;

  low = 0;
  high = table_len;
  i = high / 2;

  while (true)
    {
      if (ch < table[i][0])
	high = i;
      else if (ch > table[i][1])
	low = i;
      else
	return i;

      old = i;
      i = (high + low) / 2;
      if (i == old)
	break;
    }

  return -1;
}

jint
java::lang::Character::digit_value (jchar ch)
{
  int index = table_search (digit_table, asize (digit_table), ch);
  if (index == -1)
    return -1;

  jchar base = digit_table[index][0];
  // Tamil doesn't have a digit `0'.  So we special-case it here.
  if (base == TAMIL_DIGIT_ONE)
    return ch - base + 1;
  return ch - base;
}

jint
java::lang::Character::getNumericValue (jchar ch)
{
  jint d = digit (ch, 36);
  if (d != -1)
    return d;

  for (unsigned int i = 0; i < asize (numeric_table); ++i)
    {
      if (numeric_table[i] == ch)
	return numeric_value[i];
    }

  return -1;
}

jint
java::lang::Character::getType (jchar ch)
{
  int index = table_search (all_table, asize (all_table), ch);
  if (index != -1)
    return category_table[index];
  return UNASSIGNED;
}

jboolean
java::lang::Character::isLowerCase (jchar ch)
{
  if (ch >= 0x2000 && ch <= 0x2fff)
    return false;
  if (table_search (lower_case_table, asize (lower_case_table), ch) != -1)
    return true;

  int low, high, i, old;

  low = 0;
  high = asize (lower_anomalous_table);
  i = high / 2;

  while (true)
    {
      if (ch < lower_anomalous_table[i])
	high = i;
      else if (ch > lower_anomalous_table[i])
	low = i;
      else
	return true;

      old = i;
      i = (high + low) / 2;
      if (i == old)
	break;
    }

  return false;
}

jboolean
java::lang::Character::isSpaceChar (jchar ch)
{
  return table_search (space_table, asize (space_table), ch) != -1;
}

jboolean
java::lang::Character::isUpperCase (jchar ch)
{
  if (ch >= 0x2000 && ch <= 0x2fff)
    return false;
  return table_search (upper_case_table, asize (upper_case_table), ch) != -1;
}

jchar
java::lang::Character::toLowerCase (jchar ch)
{
  int index = table_search (upper_case_table, asize (upper_case_table), ch);
  if (index == -1)
    return to_lower_title (ch);
  return (jchar) (ch - upper_case_table[index][0]
		  + upper_case_map_table[index]);
}

jchar
java::lang::Character::toUpperCase (jchar ch)
{
  int index = table_search (lower_case_table, asize (lower_case_table), ch);
  if (index == -1)
    return to_upper_title (ch);
  return (jchar) (ch - lower_case_table[index][0]
		  + lower_case_map_table[index]);
}

#else /* COMPACT_CHARACTER */

jint
java::lang::Character::digit_value (jchar ch)
{
  if (type_table[ch] == DECIMAL_DIGIT_NUMBER)
    return attribute_table[ch];
  return -1;
}

jint
java::lang::Character::getNumericValue (jchar ch)
{
  jint d = digit (ch, 36);
  if (d != -1)
    return d;

  // Some characters require two attributes.  We special-case them here.
  if (ch >= ROMAN_START && ch <= ROMAN_END)
    return secondary_attribute_table[ch - ROMAN_START];
  if (type_table[ch] == LETTER_NUMBER || type_table[ch] == OTHER_NUMBER)
    return attribute_table[ch];
  return -1;
}

jint
java::lang::Character::getType (jchar ch)
{
  return type_table[ch];
}

jboolean
java::lang::Character::isLowerCase (jchar ch)
{
  if (ch >= 0x2000 && ch <= 0x2fff)
    return false;
  return type_table[ch] == LOWERCASE_LETTER;
}

jboolean
java::lang::Character::isSpaceChar (jchar ch)
{
  return (type_table[ch] == SPACE_SEPARATOR
	  || type_table[ch] == LINE_SEPARATOR
	  || type_table[ch] == PARAGRAPH_SEPARATOR);
}

jboolean
java::lang::Character::isUpperCase (jchar ch)
{
  if (ch >= 0x2000 && ch <= 0x2fff)
    return false;
  return type_table[ch] == UPPERCASE_LETTER;
}

jchar
java::lang::Character::toLowerCase (jchar ch)
{
  if (type_table[ch] == UPPERCASE_LETTER)
    return attribute_table[ch];
  return to_lower_title (ch);
}

jchar
java::lang::Character::toUpperCase (jchar ch)
{
  if (type_table[ch] == LOWERCASE_LETTER)
    return attribute_table[ch];
  return to_upper_title (ch);
}

#endif /* COMPACT_CHARACTER */
