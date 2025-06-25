/*
 * Copyright (c) 2021-2025 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <err.h>
#include <fcntl.h>
#include <unistd.h>

#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <algorithm>
#include <vector>

#include "config.h"
#include "libgcobol-fp.h"

#include "ec.h"
#include "io.h"
#include "common-defs.h"
#include "gcobolio.h"
#include "libgcobol.h"
#include "gfileio.h"
#include "charmaps.h"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#pragma GCC diagnostic ignored "-Wunused-result"

/*  Ordinary error-handling flow:

    file->errnum is always set to the value of the system ferror() function
    file->io_status is always set to the COBOL specific value established
          by the __gg__file_status_word function.

    When __gg__file_status_word is called with (FsErrno), it returns a value
    based on the value of errno.  Otherwise it returns the value it was called
    with.

    So, after each operation, if we can figure out the COBOL response, we set
    file->io_status t to that value.  Otherwise, we set it to FsErrno and
    let __gg__file_status_word establish the file->io_status_t value.

    */

/*  Incomplete notes on file organization, written at the time I reworked
    ORGANIZATION IS SEQUENTIAL

    GnucCOBOL:  Defaults to ORGANIZATION IS SEQUENTIAL
    GCOBOL:     Defaults to ORGANIZATION IS LINE SEQUENTIAL

    SEQUENTIAL  Where min/max record sizes are equal:
                All records are output verbatim
    SEQUENTIAL  Where FD min/max record sizes are not the same
                A 4-byte preamble is prepended to each record.
    SEQUENTIAL  Where FD record-clause is FORMAT 1
                                                 [RECORD CONTAINS 10 CHARACTERS]
                Just like sequential: The sizes of the FD records determine
                whether or not preambles are issued.  GnuCOBOL issues a warning
                if an FD record size is greater than the record-clause size
    SEQUENTIAL  Where FD record-clause is FORMAT 2 [RECORD VARYING]
                When all FD records are the same size:    No preamble
                When all FD records are different sizes:  Preamble
    SEQUENTIAL  Where FD record-clause is FORMAT 2 [RECORD VARYING 70 TO 72]
                When all FD records are the same size:    No preamble
                When all FD records are different sizes:  Preamble
    SEQUENTIAL  Where FD record-clause is FORMAT 2 [RECORD VARYING DEPENDING ON]
                There is always a preamble
    SEQUENTIAL  Where FD record-clause is FORMAT 3
                                            [RECORD CONTAINS I TO J CHARACTERS]
                There is always a preamble

    */

extern "C"
void
__gg__handle_error(const char *function, const char *msg)
  {
  if(0)
    {
    fflush(stdout);
    char ach[1024];
    snprintf(ach, sizeof(ach), "%s(): %s", function, msg);
    perror(ach);
    }
  }

static bool
handle_ferror(cblc_file_t *file, const char *function, const char *msg)
  {
  // This routine gets called after an operation that might result in a
  // failure, or in an end-of-file
  bool retval = false;    // Indicates no error
  file->errnum = ferror(file->file_pointer);

  // First, check for an end-of-file
  if( file->file_pointer )
    {
    if( feof(file->file_pointer) )
      {
      // This is an end-of-file, which has its own COBOL status code, so it
      // is error-ish:
      retval = true;
      file->io_status = FsEofSeq; // "10"
      file->prior_read_location = -1;
      }
    else if( ferror(file->file_pointer) )
      {
      // There was some kind of actionable error:
      retval = true;
      // Optionally tell the world of our troubles:
      if(0)
        {
        fflush(stdout);
        char ach[1024];
        snprintf(ach, sizeof(ach), "%s(): %s", function, msg);
        perror(ach);
        }

      // Set up for the next I/O operation by clearing out the error condition
      clearerr(file->file_pointer);
      }
    }
  else
    {
    // There is no file pointer.  The most likely cause an attempt to open a
    // file that doesn't exist.
    fprintf(stderr, "%s(): called with NULL file->file_pointer\n", __func__);
    abort();
    return true;
    }
  return retval;
  }

static bool
handle_errno(cblc_file_t *file, const char *function, const char *msg)
  {
  // This routine gets called after an operation that resulted in a
  // errno failure
  bool retval = false;    // Indicates no error
  file->errnum = errno;

  if( errno )
    {
    // There was some kind of actionable error:
    retval = true;
    // Optionally tell the world of our troubles:
    if(0)
      {
      fflush(stdout);
      char ach[1024];
      snprintf(ach, sizeof(ach), "%s(): %s", function, msg);
      perror(ach);
      }
    }
  return retval;
  }

static
char *
get_filename( const cblc_file_t *file,
              int is_quoted)
  {
  static size_t fname_size = MINIMUM_ALLOCATION_SIZE;
  static char *fname = static_cast<char *>(malloc(MINIMUM_ALLOCATION_SIZE));
  massert(fname);
  fname = internal_to_console(&fname,
                              &fname_size,
                              file->filename,
                              strlen(file->filename));

  if( !is_quoted )
    {
    // We have been given something that might be the name of an
    // environment variable that contains the filename:
    const char *p_from_environment = getenv(fname);
    if( p_from_environment )
      {
      if( strlen(p_from_environment)+1 > fname_size )
        {
        fname_size = strlen(p_from_environment)+1;
        free(fname);
        fname = static_cast<char *>(malloc(fname_size));
        massert(fname);
        }
      strcpy(fname, p_from_environment);
      }
    }

  if(*fname)
    {
    // COBOL strings are space-filled to the right, so we have to get rid
    // of any spaces out there.  If somebody *wants* a filename space-filled
    // to the right, well, at this juncture I am not prepared to be complicit
    // in that particular flavor of lunacy.
    size_t n = strlen(fname)-1;
    // Note the conditional that terminates the loop when n goes from zero
    // to a huge positive number in the event that the string is all SPACES
    while( n < strlen(fname) && fname[n] == ascii_space )
      {
      fname[n--] = '\0';
      }
    }
  return fname;
  }

static void
establish_status(cblc_file_t *file, long read_location)
  {
  // Call this routine with fs->errnum already set to errno.
  //
  // Establish file->io_status with either FsErrno or a specific value
  // before calling this routine

  // Some operations have some state associated with them:
  file->prior_read_location = read_location;

  // When this routine is called, file->io_status has been explicitly set to
  // a COBOL status (in which case it will be left alone), or it is still
  // at FsErrno, which means that errno will be used to translate to a COBOL
  // status word.
  file->io_status = __gg__file_status_word(file->io_status, file->errnum);
  __gg__int128_to_field(file->status,
                                  file->io_status,
                                  0,
                                  truncation_e,
                                  NULL);
  // Set the EC-EXCEPTION according to the status code
  __gg__set_exception_file(file);
  }

extern "C"
void
__gg__set_user_status(cblc_field_t *ustatus, cblc_file_t *file)
  {
  __gg__int128_to_field(ustatus,
                                  file->io_status,
                                  0,
                                  truncation_e,
                                  NULL);
  }

static long
max_value(const cblc_field_t *key)
  {
  long retval;
  if( key->digits )
    {
    retval = (long)__gg__power_of_ten(key->digits)-1 ;
    }
  else
    {
    switch( key->capacity )
      {
      case 1:
        retval = 99;
        break;
      case 2:
        retval = 9999;
        break;
      default:
        retval = 999999999;
        break;
      }
    }
  return retval;
  }

extern "C"
void
__gg__file_init(
  cblc_file_t   *file,
  const char    *name,
  size_t         symbol_table_index,
  cblc_field_t **keys,
  int           *key_numbers,
  int           *uniques,
  cblc_field_t  *default_record,
  cblc_field_t  *password,
  cblc_field_t  *user_status,
  cblc_field_t  *vsam_status,
  cblc_field_t  *record_length,
  cblc_field_t  *status,
  size_t reserve,
  int org,
  int padding,
  int access,
  int optional,
  size_t record_area_min,
  size_t record_area_max)
  {
  if( !(file->flags & file_flag_initialized_e) )
    {
    file->name                = strdup(name);
    file->symbol_table_index  = symbol_table_index;
    file->filename            = NULL ;
    file->file_pointer        = NULL ;
    file->keys                = keys;
    file->key_numbers         = key_numbers;
    file->uniques             = uniques;
    file->default_record      = default_record;
    file->password            = password      ;
    file->user_status         = user_status   ;
    file->vsam_status         = vsam_status   ;
    file->record_length       = record_length ;
    file->status              = status ;
    file->reserve             = reserve ;
    file->org                 = (cbl_file_org_t)org ;
    file->padding             = padding ;
    file->access              = (cbl_file_access_t)access ;
    file->errnum              = 0 ;
    file->io_status           = FsSuccess ;
    file->delimiter           = internal_newline ;
    file->flags               = file_flag_none_e;
        file->flags          |= (optional ? file_flag_optional_e : file_flag_none_e)
                                + file_flag_initialized_e;
    file->record_area_min     = record_area_min;
    file->record_area_max     = record_area_max;
    file->prior_read_location = 0;
    file->prior_op            = file_op_none;

    if( file->access == file_inaccessible_e )
      {
      file->access = file_access_seq_e;
      }
    }
  }

enum relative_file_mode
  {
  // MicroFocus uses a zero-byte prefix, and a two-byte postfix.  The
  // final byte is 0x0A for a valid record.
  rfm_microfocus_e,
  };

enum indexed_file_mode
  {
  // Data file is the same as rfm_microfocus.  We use maps and multimaps for
  // the keys, in an extravaganza of expedience.
  ifm_dubner_e,
  };

struct relative_file_parameters
  {
  long preamble_size;
  long payload_size;
  long postamble_size;
  long record_size;
  long file_size;
  long key_value;
  long record_position;
  long flag_position;
  long current_file_position;
  int  fd;
  bool inside_existing_file;
  };

#define IGNORE_LIMITS  false
#define RESPECT_LIMITS true
#define DONT_INIT_KEY  false
#define INIT_KEY       true

static bool
relative_file_parameters_get( struct relative_file_parameters &rfp,   // OUTPUT
                              relative_file_mode rfm,                 // INPUTS
                              cblc_file_t  *file,
                              bool respect_limits,
                              bool initialize_key,
                              bool is_random)
  {
  bool retval = false; // False means "okay"
  switch(rfm)
    {
    // Note that the rfm is a nice idea, but at this point is not really being
    // used.
    case rfm_microfocus_e:
      {
      if( file->record_area_min == file->record_area_max )
        {
        // Set MicroFocus-specific sizes:
        rfp.preamble_size = 0;
        rfp.payload_size = (long)file->record_area_max;
        rfp.postamble_size = 2;
        }
      else
        {
        rfp.preamble_size = 8;
        rfp.payload_size = (long)file->record_area_max;
        rfp.postamble_size = 0;
        }
      rfp.record_size = rfp.preamble_size
                          + rfp.payload_size
                          + rfp.postamble_size;

      // We need to know the current file size:
      errno = 0;
      file->errnum = 0;
      rfp.fd = fileno(file->file_pointer);
      if( rfp.fd == -1 )
        {
        file->io_status = FsErrno;
        handle_errno(file, __func__, "fileno() error" );
        retval = true;
        goto done;
        }

      struct stat file_status;
      errno = 0;
      file->errnum = 0;
      if( fstat(rfp.fd, &file_status) == -1 )
        {
        file->io_status = FsErrno;
        handle_errno(file, __func__, "fstat() error");
        retval = true;
        goto done;
        }
      rfp.file_size = file_status.st_size;

      rfp.current_file_position = ftell(file->file_pointer);
      if( handle_ferror(file, __func__,  "ftell() error") )
        {
        file->io_status = FsErrno;
        retval = true;
        goto done;
        }

      if( !is_random )
        {
        rfp.key_value = rfp.current_file_position/rfp.record_size + 1;
        rfp.record_position = (rfp.key_value-1) * rfp.record_size;
        rfp.inside_existing_file
                      = rfp.record_position + rfp.record_size <= rfp.file_size;
        }
      else
        {
        // Pick up the relative_key value:
        if( initialize_key )
          {
          rfp.key_value = rfp.current_file_position/rfp.record_size + 1;
          }
        else
          {
          int rdigits;
          if( !file->keys[0] )
            {
            warnx("%s(): %s file->keys[0] is NULL, and it shouldn't be\n",
                  __func__,
                  file->name);
            if( !file->keys[0] )
              {
              __gg__abort("relative_file_parameters_get(): file->keys is empty");
              }
            }
          rfp.key_value = (long)__gg__binary_value_from_field(&rdigits,
                                                              file->keys[0]);
          }

        rfp.record_position = (rfp.key_value-1) * rfp.record_size;
        if( rfp.record_position < 0 )
          {
          // The record can't be found before the beginning of the file
          file->io_status = FsNotFound;  // "23"
          retval = true;
          goto done;
          }

        rfp.inside_existing_file
                       = rfp.record_position + rfp.record_size <= rfp.file_size;

        if( respect_limits
            && !rfp.inside_existing_file
            && file->mode_char == 'r')
          {
          // This is a READ operation, but the targeted location is not inside
          // the file
          file->io_status = FsNotFound;  // "23"
          retval = true;
          goto done;
          }
        }

      // For Microfocus, the flag is the final byte of the record:
      rfp.flag_position = rfp.record_position + rfp.record_size - 1;

      break;
      }

    default:
      warnx("%s(): Unhandled relative_file_mode %d", __func__, rfm);
      exit(1);
      break;
    }
done:
  if( retval )
    {
    establish_status(file, -1);
    }
  return retval;
  }

static void
relative_file_delete_varying(cblc_file_t *file, bool is_random)
  {
  file->errnum = 0;
  file->io_status = FsErrno;

  size_t payload_length;

  unsigned char *stash = static_cast<unsigned char *>(malloc(file->default_record->capacity));
  massert(stash);
  memcpy(stash, file->default_record->data, file->default_record->capacity);
  long starting_pos = ftell(file->file_pointer);

  if( !file->file_pointer )
    {
    // Attempting to delete in a file that isn't open
    file->io_status = FsNoDelete;   // "49"
    goto done;
    }

  if( file->mode_char != '+' )
    {
    // We have to be in I-O mode
    file->io_status = FsNoDelete;   // "49"
    goto done;
    }

  relative_file_parameters rfp;
  if( relative_file_parameters_get(   rfp,
                                      rfm_microfocus_e,
                                      file,
                                      RESPECT_LIMITS,
                                      DONT_INIT_KEY,
                                      is_random) )
    {
    goto done;
    }

  if( !is_random )
    {
    // Check that the prior operation was a successful read:
    if( file->prior_read_location < 0)
      {
      file->io_status = FsNoRead; // "43"
      goto done;
      }

    // Turn that valid record into an empty one:
    fseek(file->file_pointer, file->prior_read_location, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }

    payload_length = 0;
    fwrite(&payload_length, 8, 1, file->file_pointer);
    if( handle_ferror(file, __func__, "fwrite() error") )
      {
      goto done;
      }
    }
  else
    {
    // We are doing a random access:

    // Let's check to make sure the slot for this record is currently
    // occupied:

    errno = 0;
    file->errnum = 0;

    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }

    fread(&payload_length, 8, 1, file->file_pointer);
    if( handle_ferror(file, __func__, "fread() error") )
      {
      goto done;
      }

    if( !payload_length )
      {
      // There isn't a record there for us to delete, which is an error
      file->io_status = FsNotFound;   // "23"
      goto done;
      }

    // Turn that valid record into an empty one:
    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }

    payload_length = 0;
    fwrite(&payload_length, 8, 1, file->file_pointer);
    if( handle_ferror(file, __func__, "fwrite() error") )
      {
      goto done;
      }
    }

done:
  memcpy(file->default_record->data, stash, file->default_record->capacity);
  free(stash);
  fseek(file->file_pointer, starting_pos, SEEK_SET);
  file->prior_op = file_op_delete;
  establish_status(file, -1);
  }

static void
relative_file_delete(cblc_file_t *file, bool is_random)
  {
  if( file->record_area_min != file->record_area_max )
    {
    return relative_file_delete_varying(file, is_random);
    }

  file->errnum = 0;
  file->io_status = FsErrno;

  char record_marker;

  unsigned char *stash = static_cast<unsigned char *>(malloc(file->default_record->capacity));
  massert(stash);
  memcpy(stash, file->default_record->data, file->default_record->capacity);

  long starting_pos = ftell(file->file_pointer);

  if( !file->file_pointer )
    {
    // Attempting to delete in a file that isn't open
    file->io_status = FsNoDelete;   // "49"
    goto done;
    }

  if( file->mode_char != '+' )
    {
    // We have to be in I-O mode
    file->io_status = FsNoDelete;   // "49"
    goto done;
    }

  relative_file_parameters rfp;
  if( relative_file_parameters_get(   rfp,
                                      rfm_microfocus_e,
                                      file,
                                      RESPECT_LIMITS,
                                      DONT_INIT_KEY,
                                      is_random) )
    {
    goto done;
    }

  if( !is_random )
    {
    // Check that the prior operation was a successful read:
    if( file->prior_read_location < 0)
      {
      file->io_status = FsNoRead; // "43"
      goto done;
      }

    // Turn that valid record into an empty one:
    record_marker = 0x00;
    errno = 0;
    file->errnum = 0;
    if( pwrite( rfp.fd,
                &record_marker,
                1,
                file->prior_read_location
                      + rfp.record_size - 1 ) == -1 )
      {
      handle_errno(file, __func__, "pwrite() error");
      goto done;
      }
    }
  else
    {
    // We are doing a random access:

    // Let's check to make sure the slot for this record is currently
    // occupied:

    errno = 0;
    file->errnum = 0;
    ssize_t presult = pread(rfp.fd, &record_marker, 1, rfp.flag_position);
    if( presult < 0 )
      {
      handle_errno(file, __func__, "pread() error");
      goto done;
      }

    if( presult == 0 || record_marker != internal_newline )
      {
      // There isn't a record there for us to delete, which is an error
      file->io_status = FsNotFound;   // "23"
      goto done;
      }

    // We now clobber the 0x0A record marker:
    record_marker = 0x00;
    errno = 0;
    file->errnum = 0;
    if( pwrite(rfp.fd, &record_marker, 1, rfp.flag_position) == -1 )
      {
      file->errnum = errno;
      handle_ferror(file, __func__, "pwrite() error");
      goto done;
      }
    }

done:
  memcpy(file->default_record->data, stash, file->default_record->capacity);
  free(stash);
  fseek(file->file_pointer, starting_pos, SEEK_SET);
  file->prior_op = file_op_delete;
  establish_status(file, -1);
  }

static std::vector<unsigned char>
file_indexed_make_key(cblc_file_t *file, int key_number)
  {
  std::vector<unsigned char> retval;
  int index = 0;
  while( file->key_numbers[index] != -1 ) // -1 is the guardrail
    {
    if( file->key_numbers[index] == key_number )
      {
      unsigned char *location = file->keys[index]->data;
      for(size_t i=0; i<file->keys[index]->capacity; i++ )
        {
        retval.push_back(*location++);
        }
      }
    index += 1;
    }
  return retval;
  }

static long
file_indexed_first_position(cblc_file_t *file, int key_number)
  {
  // We need to find the file position for the given key:
  long retval = -1;

  // Pick up our structure for this key_number
  file_index_t *file_index = &file->supplemental->indexes[key_number];

  // Build the key value for the given key_number
  std::vector<unsigned char> key = file_indexed_make_key(file, key_number);

  // Find the pair of pointers to the first and last matches
  std::pair < std::multimap<std::vector<unsigned char>, long>::iterator,
              std::multimap<std::vector<unsigned char>, long>::iterator> ret;
  ret = file_index->key_to_position.equal_range(key);
  file_index->current_iterator = ret.first;
  file_index->ending_iterator = ret.second;

  if( ret.first != ret.second )
    {
    // There is one or more entries that match the key.  We are returning
    // the location from the first.  We leave "recent_indicator" as the one
    // we found.
    retval = ret.first->second;
    }
  return retval;
  }

static int
read_an_indexed_record( cblc_file_t *file,
                        long max_bytes,
                        long &record_length,
                        int &flag)
  {
  int retval = 2;  // zero is okay; 1 is EOF, 2 is an ERROR
  size_t bytes_read;
  size_t bytes_to_read;

  // Call this routine with the file positioned at the record to read

  // We need to read in the four-byte preamble plus record_area_min bytes
  // of the record to be deleted.  We need those bytes in order to calculate
  // all of the keys of indexes that might have to be removed:

  unsigned char ach[4];
  bytes_read = fread(ach, 1, 4, file->file_pointer);
  file->errnum = ferror(file->file_pointer);
  if( feof(file->file_pointer) || bytes_read < 4 )
    {
    clearerr(file->file_pointer);
    retval = 1; // Flag the EOF
    goto done;
    }
  if( handle_ferror(file, __func__, "fread() error") )
    {
    goto done;
    }

  record_length  = static_cast<long>(ach[0])<<8;
  record_length += ach[1];
  if(ach[2] != 0)
    {
    warnx("Bad file format in read_an_indexed_record(). "
          "Third byte should be zero");
    abort();
    }

  flag = ach[3];

  bytes_to_read = record_length;
  if( record_length > max_bytes )
    {
    // The record length in the file is too big for us to read into our
    // record area.

    // Read as many bytes as we can:
    bytes_to_read = max_bytes;
    }

  file->errnum = 0;
  bytes_read = fread(file->default_record->data,
                            1,
                            bytes_to_read,
                            file->file_pointer);
  if( handle_ferror(file, __func__, "fread() error") )
    {
    goto done;
    }
  if( bytes_read != bytes_to_read)
    {
    // Weird.  We couldn't read a minimal number of bytes
    goto done;
    }

  if( record_length > max_bytes )
    {
    // The record length in the file was too big.  So, we read what we could.
    // Now we need to adjust the file pointer to skip past the bytes we weren't
    // able to read:
    fseek(file->file_pointer, record_length - max_bytes, SEEK_CUR);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    }
  retval = 0; // Indicate that all is well
  done:
  return retval;
  }

struct position_state_t
  {
  int recent_key;
  long starting_position;
  std::vector<std::multimap<std::vector<unsigned char>, long>::iterator> currents;
  std::vector<std::multimap<std::vector<unsigned char>, long>::iterator> endings;
  };

static void
position_state_preserve(cblc_file_t *file, position_state_t &state)
  {
  state.recent_key = file->recent_key;
  state.starting_position = ftell(file->file_pointer);
  if( handle_ferror(file, __func__, "ftell() error") )
    {
    exit(1);
    }
  for(size_t i=1; i<file->supplemental->indexes.size(); i++)
    {
    state.currents.push_back(file->supplemental->indexes[i].current_iterator);
    state.endings.push_back (file->supplemental->indexes[i].ending_iterator );
    }
  }

static void
position_state_restore(cblc_file_t *file, const position_state_t &state)
  {
  file->recent_key = state.recent_key;
  fseek(file->file_pointer, state.starting_position, SEEK_SET);
  if( handle_ferror(file, __func__, "fseek() error") )
    {
    exit(1);
    }
  for(size_t i=1; i<file->supplemental->indexes.size(); i++)
    {
    // Remember that key numbers range from 1 to N, whilst the vector we
    // we created to stash the iterators started at zero.
    file->supplemental->indexes[i].current_iterator = state.currents[i-1];
    file->supplemental->indexes[i].ending_iterator  = state.endings[i-1];
    }
  }

static void
indexed_file_delete(cblc_file_t *file, bool is_random)
  {
  file->errnum = 0;
  file->io_status = FsErrno;

  long fpos;
  unsigned char *stash = NULL;
  long record_length;
  int  flag;
  file_hole_t new_hole;
  position_state_t position_state;

  if( !file->file_pointer )
    {
    // Attempting to delete in a file that isn't open
    file->io_status = FsNoDelete;   // "49"
    goto done;
    }

  if( file->mode_char != '+' )
    {
    // We have to be in I-O mode
    file->io_status = FsNoDelete;   // "49"
    goto done;
    }

  if( !is_random && file->prior_read_location == -1 )
    {
    // When the access mode is sequential, the prior operation shall have been a
    // successful read statement.  It is that record which is removed.  Not
    // meeting that requirement gets its own error code:
    file->io_status = FsNoRead;    // "43"
    goto done;
    }

  // For both sequential and random access, we need to read the record to be
  // deleted into the record area.  It could be I am being overly didactic, but
  // my reading of the ISO spec indicates that there has to be a successful read
  // of the record in sequential mode, but I don't see a requirement that the]
  // programmer leave the record area untouched before trying to delete it.

  // Likewise, for random access, the primary key has to be valid, but I see
  // no requirement that the alternate keys be valid.  We need the alternate
  // keys in place in order to delete them from the indexes.

  // The requirements for a delete are that both the file position indicator
  // and the record area itself are unchanged by the delete operation.

  // So, we save the current record area:
  stash = static_cast<unsigned char *>(malloc(file->record_area_max));
  massert(stash);
  memcpy(stash, file->default_record->data, file->record_area_max);

  // And the position state of our file
  position_state_preserve(file, position_state);

  if( !is_random )
    {
    // For sequential, re-read the recent successful read:
    fpos = file->prior_read_location;
    }
  else
    {
    // We are doing a random read.  Figure out the position from the primary
    // key:
    fpos = file_indexed_first_position(file, 1);
    if( fpos == -1 )
      {
      // The primary key does not exist
      file->io_status = FsNotFound; // "23"
      goto done;
      }
    }

  // Read the record to be deleted into the record area:
  fseek(file->file_pointer, fpos, SEEK_SET);
  if( handle_ferror(file, __func__, "fseek() error") )
    {
    goto done;
    }
  if( read_an_indexed_record( file,
                              file->record_area_max,
                              record_length,
                              flag) )
    {
    goto done;
    }

  // The record we just read *has* to be a good record:
  if( flag != 1 )
    {
    warnx("Bad file format in indexed_file_delete(). "
          "Fourth byte should be one");
    abort();
    }

  // Because we are deleting it, we need to remove it from all of the indexes.
  // Given our indexes, we need to scan all of them looking for the matching
  // fpos value.  And then we have to start over again, because the deletion
  // changes the tree that implements the multimap.  If speed becomes an issue,
  // a bandaid fix would be to create a multi-map of fpos to iterator.

  // The real fix is a real database, but somewhere before that we need to
  // really design an indexed file system, instead of this business of putting
  // it on a stick an banging a few nails through it.

  for(size_t  key_number=1;
              key_number<file->supplemental->indexes.size();
              key_number++)
    {
    file_index_t *file_index = &file->supplemental->indexes[key_number];
    if( file->supplemental->uniques[key_number] )
      {
      // This key does not allow duplicates, so we don't have to scan for it,
      // because we know we have to delete it.
      file_indexed_first_position(file, key_number);
      file_index->key_to_position.erase(file_index->current_iterator);
      // and continue to the next key number
      continue;
      }
    else
      {
      // This particular key allows duplicates

      // We have to scan the entire index for keys that point to our fpos.  When
      // we find one, we check to see if the keys match.  If the keys don't
      // match, then we have to remove the existing one from the index.

      bool deleting = true;
      while(deleting)
        {
        deleting = false;
        std::multimap<std::vector<unsigned char>, long>::iterator it
              = file_index->key_to_position.begin();
        while( it != file_index->key_to_position.end() )
          {
          if( it->second == fpos )
            {
            // We have found an index that points to our record.
            // It needs to be deleted.
            file_index->key_to_position.erase(it);
            deleting = true;
            break;
            }

          it++;
          }
        }
      }
    }

  // We need to turn this record into a hole by changing the fourth byte from
  // one to zero

  fseek(file->file_pointer, fpos+3, SEEK_SET);
  if( handle_ferror(file, __func__, "fseek() error") )
    {
    goto done;
    }
  fputc(0, file->file_pointer);
  if( handle_ferror(file, __func__, "fputc() error") )
    {
    goto done;
    }

  // We just created a hole; put it in the list:
  new_hole.location = fpos;
  new_hole.size = record_length;
  file->supplemental->holes.push_back(new_hole);

done:
  if( stash )
    {
    // Restore the stashed record area:
    memcpy(file->default_record->data, stash, file->record_area_min);
    free(stash);
    stash = NULL;
    file->prior_op = file_op_delete;
    position_state_restore(file, position_state);
    }

  file->prior_op = file_op_delete;
  establish_status(file, -1);
  }

static void
__io__file_delete(cblc_file_t *file, bool is_random)
  {
  switch(file->org)
    {
    case file_relative_e:
      relative_file_delete(file, is_random);
      break;

    case file_indexed_e:
      indexed_file_delete(file, is_random);
      break;

    default:
      warnx("%s(): Unhandled file organization", __func__);
      exit(1);
      break;
    }

  if( file->io_status < FhNotOkay )
    {
    file->flags |= file_flag_existed_e;
    }
  }

static void
indexed_file_start( cblc_file_t *file,
                    int relop,
                    int key_number,
                    size_t length)
  {
  static const int first_key = 1;
  //fprintf(stderr, "   length is %ld\n", length);
  file->io_status = FsNotFound; // "23"

  size_t length_of_key = 0;
  bool direction_reverse;
  std::multimap<std::vector<unsigned char>, long>::iterator it;
  file_index_t *file_index;
  std::vector<unsigned char> the_key;

  if( key_number == -1 )
    {
    // This is FIRST:
    file_index = &file->supplemental->indexes[first_key];
    if( file_index->key_to_position.size() )
      {
      it = file_index->key_to_position.begin();
      file->io_status = FsErrno;
      key_number = first_key;
      }
    goto done;
    }
  if( key_number == -2 )
    {
    // This is LAST:
    file_index = &file->supplemental->indexes[first_key];
    if( file_index->key_to_position.size() )
      {
      it = file_index->key_to_position.end();
      it--;
      file->io_status = FsErrno;
      key_number = first_key;
      }
    goto done;
    }

  // Pick up our file index for this key number
  file_index = &file->supplemental->indexes[key_number];

  // We are going to need the key from the existing record:
  the_key = file_indexed_make_key(file, key_number);
  length_of_key = the_key.size();

  // Make sure the length parameter is within the size of the actual key
  if( length < 1 || length > length_of_key )
    {
    file->io_status = FsNotFound;   // "23"
    goto done;
    }

  direction_reverse = (relop==lt_op || relop==le_op);

  if( direction_reverse )
    {
    // We will do a forward search for the biggest index that is smaller than
    // the key.
    std::multimap<std::vector<unsigned char>, long>::iterator bestie =
                                             file_index->key_to_position.end();
    it = file_index->key_to_position.begin();
    while( it != file_index->key_to_position.end() )
      {
      const unsigned char *the_index = it->first.data();

      // Do the comparison:
      int result = 0;

      for(size_t i=0; i<length; i++)
        {
        result = the_index[i] - the_key[i] ;
        if( result )
          {
          break;
          }
        }

      if( result > 0 )
        {
        // The index is bigger than the key, so, for better or for worse, we
        // are done looking.
        break;
        }

      if( result == 0 )
        {
        // The index and key are equal
        if( relop == le_op )
          {
          // We have equal, and we were looking for equality.  Set a conditional
          // winner, and then keep looking in case there are more indexes that
          // are equal to the key
          bestie = it;
          file->io_status = FsErrno;
          }
        }
      else // if( result < 0 )
        {
        // The index is less than the key.
        if(    relop == lt_op
            || relop == le_op )
          {
          // This is a conditional winner, so we flag it as such and keep going,
          // because there might be more indexes that are bigger than this one
          // but less than the key
          bestie = it;
          file->io_status = FsErrno;
          }
        }
      // At this point, the index is less than the key, so we keep looking:
      it++;
      }
    if( file->io_status == FsErrno )
      {
      // We found something
      it = bestie;
      }
    }
  else
    {
    // We are doing a forward search for the first index entry that matches
    // the criterion.

    it = file_index->key_to_position.begin();
    while( it != file_index->key_to_position.end() )
      {
      const unsigned char *the_index = it->first.data();

      // Do the comparison:
      int result = 0;

      for(size_t i=0; i<length; i++)
        {
        result = the_index[i] - the_key[i] ;
        if( result )
          {
          break;
          }
        }

      if( result == 0 )
        {
        // The index and key are equal
        if(     relop == eq_op
            ||  relop == ge_op )
          {
          // We have equal, and we were looking for equality:
          file->io_status = FsErrno;
          goto done;
          }
        else
          {
          // The operation is gt_op, so we fall through and keep looking
          }
        }
      else if( result > 0 )
        {
        // The the index is bigger than the key
        if( relop == eq_op )
          {
          // The index is bigger than the key, so we will never find
          // equality.
          goto done;
          }
        // At the point, the operation has to be ge_op or gt_op, so our
        // criterion is satisfied
        file->io_status = FsErrno;
        goto done;
        }
      // At this point, the index is less than the key, so we keep looking:
      it++;
      }
    // We went through the whole index without finding anything.
    }
done:
  if( file->io_status == FsErrno )
    {
    // The it iterator points to a valid found index:
    file_index->current_iterator = it;
    file_index->ending_iterator  = file_index->key_to_position.end();

    fseek(file->file_pointer, it->second, SEEK_SET);
    handle_ferror(file, __func__, "fseek() error");

    file->recent_key = key_number;
    }
  }

static long
relative_file_start(cblc_file_t *file,
                    int first_last_key,
                    int relop)
  {
  long fpos = -1;
  relative_file_parameters rfp;
  int rdigits;
  long total_record_length;
  if( relative_file_parameters_get(   rfp,
                                      rfm_microfocus_e,
                                      file,
                                      IGNORE_LIMITS,
                                      INIT_KEY,
                                      true) )
    {
    goto done;
    }
  total_record_length
                  = rfp.preamble_size + rfp.payload_size + rfp.postamble_size;
  if( first_last_key == -1 )
    {
    // This is FIRST.  Search forward from the beginning
    rfp.key_value = 1;
    relop = ge_op;
    }
  else if( first_last_key == -2 )
    {
    // This is LAST.  Search reverse from the end
    rfp.key_value = rfp.file_size / total_record_length;
    relop = le_op;
    }
  else
    {
    rfp.key_value = (long)__gg__binary_value_from_field(&rdigits,
                                                        file->keys[0]);
    }

  bool forward;
  switch( relop )
    {
    case eq_op:
    case ge_op:
      forward = true;
      break;
    case le_op:
      forward = false;
      break;
    case gt_op:
      rfp.key_value += 1;
      forward = true;
      break;
    case lt_op:
      rfp.key_value -= 1;
      forward = false;
      break;
    default:
      warnx("%s(): relop is %d, which we don't know how to handle",
            __func__,
            relop);
      exit(1);
      break;
    }

  if( forward )
    {
    // We are searching forward, so we want the key_value to be 1 or more
    rfp.key_value = std::max( rfp.key_value, (long)1);
    }
  else
    {
    // We are searching backward, so we want the rfp.key_value to be at most
    // the last possible record based on the file size:
    rfp.key_value = std::min( rfp.key_value,
                              rfp.file_size / total_record_length);
    }

  // Let's initialize for searching for a valid record:
  rfp.record_position = (rfp.key_value-1) * total_record_length;
  rfp.flag_position   = rfp.record_position + rfp.record_size - 1;

  // And let the searching begin:

  while(      rfp.record_position >= 0
          &&  rfp.record_position+total_record_length <= rfp.file_size )
    {
    char record_marker;
    ssize_t presult = pread(rfp.fd, &record_marker, 1, rfp.flag_position);
    if( presult < 0 )
      {
      handle_errno(file, __func__, "pread() error");
      goto done;
      }
    if( presult == 0 )
      {
      // end of file
      goto done;
      }
    if( record_marker == internal_newline )
      {
      // The record is a valid one
      fpos = rfp.record_position;
      goto done;
      }
    // That position in the file did not have valid data in it.  Move on to the
    // next record
    if( forward )
      {
      rfp.key_value += 1;
      rfp.record_position += total_record_length;
      rfp.flag_position   += total_record_length;
      }
    else
      {
      rfp.key_value -= 1;
      rfp.record_position -= total_record_length;
      rfp.flag_position   -= total_record_length;
      }
    }

  if( fpos == -1)
    {
    // We didn't find a valid record
    file->io_status = FsNotFound;   // "23"
    goto done;
    }

  // Position the file at the requested record:
  done:
  if( fpos >= 0)
    {
    fseek(file->file_pointer, fpos, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    // It's unclear to me whether or not START is supposed to update keys like
    // this
    // Update the key with the one we found
    // __gg__int128_to_field(file->keys[0],
    //                                 rfp.key_value,
    //                                 0,
    //                                 truncation_e,
    //                                 NULL);
    }

  return fpos;
  }


static void
__io__file_start(  cblc_file_t *file,
                   int relop,
                   int first_last_key,
                   size_t length)
  {
  // According to IBM Language Reference 6.3, whether or not 'key' is specified,
  // the value used is from the RELATIVE KEY clause.  See page 421 "Relative
  // Files" "Whether or not the KEY phrase is specified, the key data item used
  // in the comparison is the RELATIVE KEY data item."
  file->errnum = 0;
  file->io_status = FsErrno;
  long fpos = -1;

  bool okay;

  if( !file->file_pointer )
    {
    // Attempting to START a file that isn't open
    if( file->flags & file_flag_optional_e )
      {
      file->io_status = FsNotFound;    // "23"
      }
    else
      {
      file->io_status = FsReadNotOpen; // "47"
      }
    goto done;
    }

  okay =      (file->org == file_indexed_e || file->org == file_relative_e)
              && (   file->access == file_access_seq_e
                  || file->access == file_access_dyn_e)
              && (file->mode_char == 'r' || file->mode_char == '+') ;
  if( !okay )
    {
    file->io_status = FsReadNotOpen;    // "47"
    goto done;
    }

  if( file->org == file_relative_e )
    {
    fpos = relative_file_start(file, first_last_key, relop);
    }
  else
    {
    // first_last_key is the key_number
    indexed_file_start(file, relop, first_last_key, length);
    if( file->io_status == FsErrno )
      {
      fpos = 0;
      }
    }

done:
  if( file->io_status == FsErrno )
    {
    file->flags |= file_flag_existed_e;
    }

  file->prior_op = file_op_start;
  establish_status(file, fpos);
  if( file->io_status < FhNotOkay )
    {
    file->flags |= file_flag_existed_e;
    }
  }

static void
sequential_file_rewrite( cblc_file_t *file, size_t length )
  {
  // Handles sequential files

  file->errnum = 0;
  file->io_status = FsErrno;

  long starting_position = -1;
  bool okay;
  size_t bytes_to_write;

  if( !file->file_pointer )
    {
    // Attempting to read a file that isn't open
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  okay = (file->mode_char == '+');

  if( !okay )
    {
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  starting_position = ftell(file->file_pointer);
  // Check for EOF before calling ferror, which clears feof
  if( feof( file->file_pointer) )
    {
    // Trying to do a rewrite at the end-of-file position has its own code:
    file->io_status = FsNoRead;    // "43"
    goto done;
    }
  if( handle_ferror(file, __func__, "ftell() error") )
    {
    goto done;
    }

  if( file->prior_read_location == -1 )
    {
    // The prior operation was not a successful read:
    file->io_status = FsNoRead; // "43"
    goto done;
    }

  fseek(file->file_pointer, file->prior_read_location, SEEK_SET);
  if( handle_ferror(file, __func__, "fseek() error") )
    {
    goto done;
    }

  size_t existing_size;
  if( file->record_area_min != file->record_area_max )
    {
    // Because of the different sizes, we are expecting a preamble:

    // An error at this point is either an error, or a true end-of-file.
    unsigned char preamble[4];
    size_t characters_read = fread(preamble, 1, 4, file->file_pointer);
    if( handle_ferror(file, __func__, "fread() error") )
      {
      // This also comes back true for an ordinary end-of-file
      goto done;
      }

    if( characters_read != 4 )
      {
      // If the preamble is incomplete, treat that as an EOF
      file->io_status = FsEofSeq; // "10"
      file->prior_read_location = -1;
      goto done;
      }

    // Extract the count of bytes in the record from the preamble
    existing_size = ((size_t)preamble[0]<<8) + preamble[1];
    }
  else
    {
    // Because the min and max are the same, we figure on writing that many
    // characters:
    existing_size = file->record_area_max;
    }

  // We need to make sure that the number of bytes we've been asked
  // to write is valid:

  // By default, we write what we've been asked to write.
  bytes_to_write = length;

  // But that can be overridden by a depending_on from the
  // FILE-CONTROL paragraph:

  if( file->record_length )
    {
    int rdigits;
    bytes_to_write = (size_t)__gg__binary_value_from_field(
                                                       &rdigits,
                                                       file->record_length);
    }

  if(    bytes_to_write < file->record_area_min
         || bytes_to_write > file->record_area_max
         || bytes_to_write != existing_size )
    {
    file->io_status = FsBoundWrite; // "44"
    goto done;
    }

  if( file->record_area_min != file->record_area_max )
    {
    const unsigned char preamble[4] =
      {
      (unsigned char)(bytes_to_write>>8),
      (unsigned char)(bytes_to_write),
      0,
      0
      };

    fwrite( preamble,
            4,
            1,
            file->file_pointer);
    if( handle_ferror(file, __func__, "fwrite() error") )
      {
      goto done;
      }
    }

  fwrite( file->default_record->data,
          bytes_to_write,
          1,
          file->file_pointer );
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

done:
  // Per the standard, return the file location pointer back to whence it came:
  fseek(file->file_pointer, starting_position, SEEK_SET);
  handle_ferror(file, __func__, "fseek() error");
  file->prior_op = file_op_rewrite;
  establish_status(file, starting_position);
  }

static void
relative_file_rewrite_varying( cblc_file_t *file, bool is_random )
  {
  file->errnum = 0;
  file->io_status = FsErrno;

  long starting_position = -1;
  relative_file_parameters rfp;
  size_t payload_size;

  if( !file->file_pointer )
    {
    // Attempting to rewrite a file that isn't open
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  if( file->mode_char != '+' )
    {
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  starting_position = ftell(file->file_pointer);
  // Check for EOF before calling ferror, which clears feof
  if( feof( file->file_pointer) )
    {
    // Trying to do a rewrite at the end-of-file position has its own code:
    file->io_status = FsNoRead;    // "43"
    goto done;
    }
  if( handle_ferror(file, __func__, "ftell() error") )
    {
    goto done;
    }

  if( !is_random )
    {
    // The access mode is sequential, so we can leverage
    // sequential_file_rewrite
    rfp.record_position = starting_position;
    goto do_the_write;
    }
  else
    {
    // This is like a write, except the place we are putting
    // it has to be occupied instead of empty.
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        RESPECT_LIMITS,
                                        DONT_INIT_KEY,
                                        is_random) )
      {
      goto done;
      }

    do_the_write:
    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    fread(&payload_size, 8, 1, file->file_pointer);
    if( handle_ferror(file, __func__, "fread() error") )
      {
      goto done;
      }

    if( !payload_size )
      {
      // The record is not empty:
      file->io_status = FsNotFound;   // "23"
      goto done;
      }

    payload_size = file->default_record->capacity;
    if(     payload_size < file->record_area_min
        ||  payload_size > file->record_area_max)
      {
      file->io_status = FsBoundWrite; // "44"
      goto done;
      }

    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    // We can overwrite the valid record:
    fwrite( &payload_size,
            8,
            1,
            file->file_pointer );
    if( handle_ferror(file, __func__, "fwrite() error") )
      {
      goto done;
      }

    fwrite( file->default_record->data,
            file->default_record->capacity,
            1,
            file->file_pointer );
    if( handle_ferror(file, __func__, "fwrite() error") )
      {
      goto done;
      }
    }

done:
  // Per the standard, return the file location pointer back to whence it came:
  fseek(file->file_pointer, starting_position, SEEK_SET);
  handle_ferror(file, __func__, "fseek() error");
  file->prior_op = file_op_rewrite;
  establish_status(file, starting_position);
  }

static void
relative_file_rewrite( cblc_file_t *file, size_t length, bool is_random )
  {
  if( file->record_area_min != file->record_area_max )
    {
    return relative_file_rewrite_varying(file, is_random);
    }

  file->errnum = 0;
  file->io_status = FsErrno;

  long starting_position = -1;
  relative_file_parameters rfp;

  if( !file->file_pointer )
    {
    // Attempting to rewrite a file that isn't open
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  if( file->mode_char != '+' )
    {
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  starting_position = ftell(file->file_pointer);
  // Check for EOF before calling ferror, which clears feof
  if( feof( file->file_pointer) )
    {
    // Trying to do a rewrite at the end-of-file position has its own code:
    file->io_status = FsNoRead;    // "43"
    goto done;
    }
  if( handle_ferror(file, __func__, "ftell() error") )
    {
    goto done;
    }

  if(!is_random)
    {
    // The access mode is sequential, so we can leverage
    // sequential_file_rewrite
    sequential_file_rewrite(file, length);
    goto done;
    }
  else
    {
    // This is like a write, except the place we are putting
    // it has to be occupied instead of empty.
    char record_marker;
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        RESPECT_LIMITS,
                                        DONT_INIT_KEY,
                                        is_random) )
      {
      goto done;
      }

    ssize_t presult = pread(rfp.fd, &record_marker, 1, rfp.flag_position);
    if( presult < 0 )
      {
      handle_errno(file, __func__, "pread() error");
      goto done;
      }

    if( presult == 0 || record_marker != internal_newline )
      {
      // The record is not specified:
      file->io_status = FsNotFound;   // "23"
      goto done;
      }

    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }

    // We can overwrite the valid record:
    fwrite( file->default_record->data,
            file->default_record->capacity,
            1,
            file->file_pointer );
    if( handle_ferror(file, __func__, "fwrite() error") )
      {
      goto done;
      }
    }

done:
  // Per the standard, return the file location pointer back to whence it came:
  fseek(file->file_pointer, starting_position, SEEK_SET);
  handle_ferror(file, __func__, "fseek() error");
  file->prior_op = file_op_rewrite;
  establish_status(file, starting_position);
  }

static bool
file_indexed_update_indices(cblc_file_t *file,
                            long record_position,
                            bool dupes_okay = false)
  {
  // We have a record in the file->record_area.

  bool okay = true;

  // We have to do this in two passes.

  // First, we check to see that for all keys flagged as unique, that
  // the key doesn't already exist:

  for(size_t key_number=1;
             key_number<file->supplemental->indexes.size();
             key_number++)
    {
    if( file->supplemental->uniques[key_number] )
      {
      // This key has to be unique:
      std::vector<unsigned char>key_value =
                                      file_indexed_make_key(file, key_number);
      file_index_t *file_index = &file->supplemental->indexes[key_number];
      std::multimap<std::vector<unsigned char>, long>::const_iterator it =
                                    file_index->key_to_position.find(key_value);
      if( it != file_index->key_to_position.end() )
        {
        // This key is already in the index
        if( !dupes_okay || it->second != record_position )
          {
          // We have a key violation:
          file->io_status = FsDupWrite;   // "22"
          okay = false;
          break;
          }
        }
      }
    }

  if( okay )
    {
    // There were no key violations, so we can do the second pass, which inserts
    // the keys into the indexes.
    for(size_t  key_number=1;
                key_number<file->supplemental->indexes.size();
                key_number++)
      {
      file_index_t *file_index = &file->supplemental->indexes[key_number];
      // But we don't want the key to end up in here twice, which we have to
      // avoid when updating the indexes after a REWRITE

      bool safe_to_insert = true;
      file_indexed_first_position(file, key_number);
      while( file_index->current_iterator != file_index->ending_iterator )
        {
        if( file_index->current_iterator->second == record_position)
          {
          safe_to_insert = false;
          break;
          }
        file_index->current_iterator++;
        }

      if( safe_to_insert )
        {
        std::vector<unsigned char>key_value =
                                        file_indexed_make_key(file, key_number);
        std::pair<std::vector<unsigned char>, long>
        to_insert(key_value, record_position);
        file_index->key_to_position.insert(to_insert);
        }
      }
    }
  return okay;
  }

static void
indexed_file_rewrite( cblc_file_t *file, size_t /*length*/, bool is_random )
  {
  file->errnum = 0;
  file->io_status = FsErrno;

  long fpos = -1;
  position_state_t position_state;
  long record_length;

  if( !file->file_pointer )
    {
    // Attempting to work on a file that isn't open
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  if( file->mode_char != '+' )
    {
    // For an indexed_file REWRITE, the mode has to be '+'
    file->io_status = FsNoDelete;    // "49"
    goto done;
    }

  // We need to preserve the file position state
  position_state_preserve(file, position_state);

  if( !is_random )
    {
    // In the case of sequential access, the prior operation needed to be a
    // successful read:
    if( file->prior_read_location == -1 )
      {
      // The prior operation was not a successful read:
      file->io_status = FsNoRead; // "43"
      goto done;
      }

    // The prior read was successful.  Let's see if the primary key has changed
    fpos = file_indexed_first_position(file, 1);
    if( fpos != file->prior_read_location )
      {
      // Between the successful read and this attempt at a rewrite, somebody
      // changed the primary key in the record.  This gets its own special error
      // code
      file->io_status = FsKeySeq; // "21"
      goto done;
      }
    }
  else
    {
    // It's not a sequential read, so it's got to be random.  Let's determine
    // that the primary key is valid:
    fpos = file_indexed_first_position(file, 1);
    if( fpos == -1 )
      {
      file->io_status = FsNotFound; // "23"
      goto done;
      }
    }

  // We need to know the record length:
  fseek(file->file_pointer, fpos, SEEK_SET);
  if( handle_ferror(file, __func__, "fseek() error") )
    {
    fpos = -1;
    goto done;
    }

  unsigned char preamble[4];
  fread(preamble, 1, 4, file->file_pointer);
  if( handle_ferror(file, __func__, "fread() error") )
    {
    fpos = -1;
    goto done;
    }
  record_length = (preamble[0]<<8) + preamble[1];
  // Note that after the read, the file position is at the beginning of the
  // record.

  // We know we have a good primary key.  It points to fpos.

  // We now need to check any alternate keys flagged as unique.  We need to
  // make sure that any such key this record has already points to fpos.

  // If it points somewhere else, then our attempt to rewrite the record would
  // end up that key pointing to two different records, which means a failure:

  for(size_t  key_number=2;
              key_number<file->supplemental->indexes.size();
              key_number++)
    {
    if( file->supplemental->uniques[key_number] )
      {
      // This particular alternative index does not allow duplicates
      long altfpos = file_indexed_first_position(file, key_number);
      if( altfpos != -1 && altfpos != fpos )
        {
        // This alternate key would create a duplicate index entry where none
        // such are allowed:
        file->io_status = FsDupWrite;    // "22"
        goto done;
        }
      }
    }

  // At this point we know we are going to write the record, because there will
  // be no key violations.  But because an alternate key might have changed, we
  // we have to scan for all such and remove them from the indexes:
  for(size_t  key_number=2;
              key_number<file->supplemental->indexes.size();
              key_number++)
    {
    if( !file->supplemental->uniques[key_number] )
      {
      // This particular alternative index allows duplicates
      file_index_t *file_index = &file->supplemental->indexes[key_number];

      // We have to scan the entire index for keys that point to our fpos.  When
      // we find one, we check to see if the keys match.  If the keys don't
      // match, then we have to remove the existing one from the index.

      std::vector<unsigned char> the_key
                                     = file_indexed_make_key(file, key_number);
      bool deleting = true;
      while(deleting)
        {
        deleting = false;
        std::multimap<std::vector<unsigned char>, long>::iterator it
              = file_index->key_to_position.begin();
        while( it != file_index->key_to_position.end() )
          {
          if( it->second == fpos )
            {
            // We have found an index entry that points to our record.  If the
            // index's key doesn't match our calculated key, we need to delete
            // it.
            const unsigned char *the_index = it->first.data();
            int result = 0;
            for(size_t i=0; i<the_key.size(); i++)
              {
              result = the_key[i] - the_index[i];
              if( result )
                {
                break;
                }
              }
            if(result)
              {
              // We found an index that needs to be deleted
              file_index->key_to_position.erase(it);
              deleting = true;
              break;
              }
            }
          it++;
          }
        }
      }
    }

  // To recap:  The primary key is valid, and any remaining alternative keys
  // are already pointing to fpos, and if a new one gets entered, it won't
  // conflict with any existing key flagged as unique.

  // We can now write the data out, at the given location:

  fwrite( file->default_record->data,
          1,
          record_length,
          file->file_pointer );
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

  // And it is safe to update the keys:
  file_indexed_update_indices(file,
                              fpos,
                              true);  // Set to true to indicate that might
  //                                  // occur and should be ignored

done:
  // Per the standard, return thefile location pointer back to whence it came:
  if( fpos != -1 )
    {
    position_state_restore(file, position_state);
    }
  file->prior_op = file_op_rewrite;
  establish_status(file, fpos);
  file->prior_read_location = -1;
  }

static void
__io__file_rewrite(cblc_file_t *file, size_t length, bool is_random)
  {
  switch(file->org)
    {
    case file_relative_e:
      relative_file_rewrite(file, length, is_random);
      break;

    case file_sequential_e:
      sequential_file_rewrite(file, length);
      break;

    case file_indexed_e:
      indexed_file_rewrite(file, length, is_random);
      break;

    default:
      warnx("%s(): Unhandled file organization", __func__);
      exit(1);
      break;
    }
  if( file->io_status < FhNotOkay )
    {
    file->flags |= file_flag_existed_e;
    }
  }

static void
relative_file_write_varying(cblc_file_t    *file,
                      const unsigned char  *location,
                            size_t          length,
                            bool            is_random)
  {
  // This routine handles variable-length writes to RELATIVE files
  file->errnum = 0;
  file->io_status = FsErrno;

  long necessary_file_size;
  size_t payload_length;

  relative_file_parameters rfp;

  if( is_random )
    {
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        IGNORE_LIMITS,
                                        DONT_INIT_KEY,
                                        is_random) )
      {
      goto done;
      }

    // If the file isn't big enough, we need to expand it:
    necessary_file_size =   rfp.record_position
                          + rfp.record_size;
    if( rfp.file_size < necessary_file_size )
      {
      // Position the file position indicator to the very end of the file
      fseek(file->file_pointer, 0, SEEK_END);
      if( handle_ferror(file, __func__, "fseek() error") )
        {
        goto done;
        }

      // Expand the file to the necessary length:
      while( rfp.file_size++ < necessary_file_size-1 )
        {
        fputc(0, file->file_pointer);
        if( handle_ferror(file, __func__, "fputc() error [2]") )
          {
          goto done;
          }
        }
      }

    // Position the file pointer at the slot:
    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }

    fread(&payload_length, 8, 1, file->file_pointer);
    if( handle_ferror(file, __func__, "fread() error") )
      {
      goto done;
      }

    if( payload_length != 0 )
      {
      // The slot has something in it already:
      file->io_status = FsDupWrite;   // "22"
      goto done;
      }

    // Position the file pointer at the slot:
    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    }
  else
    {
    // We need to do a sequential write:
    if( file->mode_char == 'a' )
      {
      // Position the file pointer at the end:
      fseek(file->file_pointer, 0, SEEK_END);
      if( handle_ferror(file, __func__, "fseek() error") )
        {
        goto done;
        }
      }
    }

  // Write out the record.
  // We need a length.

  if( file->record_length )
    {
    // df
    int rdigits;
    length = (size_t)__gg__binary_value_from_field(    &rdigits,
                                                       file->record_length);
    }

  payload_length = length;

  if(     payload_length < file->record_area_min
      ||  payload_length > file->record_area_max)
    {
    file->io_status = FsBoundWrite; // "44"
    goto done;
    }

  fwrite(&payload_length, 8, 1, file->file_pointer);
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

  fwrite(location, 1, payload_length, file->file_pointer);
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

  while( payload_length < file->record_area_max )
    {
    fputc(internal_space, file->file_pointer);
    if( handle_ferror(file, __func__, "fputc() error") )
      {
      goto done;
      }
    payload_length += 1;
    }

  if( is_random )
    {
    // Per the COBOL specification, put the file position back to where
    // it was when we started this exercise:
    fseek(file->file_pointer, rfp.current_file_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek(starting_pos) error") )
      {
      goto done;
      }
    }

done:
  file->prior_op = file_op_write;
  establish_status(file, -1);
  }

static void
relative_file_write(cblc_file_t    *file,
              const unsigned char  *location,
                    size_t          length,
                    bool            is_random)
  {
  // This routine handles writes to RELATIVE files

  if( file->record_area_min != file->record_area_max )
    {
    return relative_file_write_varying(file, location, length, is_random);
    }

  file->errnum = 0;
  file->io_status = FsErrno;

  long necessary_file_size;
  const unsigned char achPostamble[] = {internal_cr, internal_newline};

  relative_file_parameters rfp;

  if( is_random )
    {
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        IGNORE_LIMITS,
                                        DONT_INIT_KEY,
                                        is_random) )
      {
      goto done;
      }

    necessary_file_size =   rfp.record_position
                          + rfp.preamble_size
                          + rfp.payload_size
                          + rfp.postamble_size;
    if( rfp.file_size < necessary_file_size )
      {
      // Position the file position indicator to the very end of the file
      fseek(file->file_pointer, 0, SEEK_END);
      if( handle_ferror(file, __func__, "fseek() error") )
        {
        goto done;
        }

      // Expand the file to the necessary length:
      while( rfp.file_size++ < necessary_file_size-1 )
        {
        fputc(0, file->file_pointer);
        if( handle_ferror(file, __func__, "fputc() error [2]") )
          {
          goto done;
          }
        }
      }
    // Let's check to make sure the slot for this record is currently available:
    char record_marker;
    ssize_t presult = pread(rfp.fd, &record_marker, 1, rfp.flag_position);
    if( presult < 0 )
      {
      handle_errno(file, __func__, "pread() error");
      goto done;
      }

    if( presult == 1 && record_marker == internal_newline )
      {
      // The slot has something in it already:
      file->io_status = FsDupWrite;   // "22"
      goto done;
      }
    // Either the record is available, or else the write is taking place at the
    // end of the current file size.

    // Position the file pointer at the slot:
    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    }
  else
    {
    // We need to do a sequential write:
    if( file->mode_char == 'a' )
      {
      // Position the file pointer at the end:
      fseek(file->file_pointer, 0, SEEK_END);
      if( handle_ferror(file, __func__, "fseek() error") )
        {
        goto done;
        }
      }
    }

  // Write out the data:
  fwrite(location, length, 1, file->file_pointer);
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

  if( file->record_area_max > length )
    {
    size_t padding = file->record_area_max - length;
    while(padding--)
      {
      fputc(internal_space, file->file_pointer);
      }
    }

  // Write out the rfm_microfocus "valid record" postamble:
  fwrite(achPostamble, 1, 2, file->file_pointer);
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

  if( is_random )
    {
    // Per the COBOL specification, put the file position back to where
    // it was when we started this exercise:
    fseek(file->file_pointer, rfp.current_file_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek(starting_pos) error") )
      {
      goto done;
      }
    }

done:
  file->prior_op = file_op_write;
  establish_status(file, -1);
  }

static void
sequential_file_write(cblc_file_t    *file,
                const unsigned char  *location,
                      size_t          length,
                      int             after,
                      int             lines)
  {
  // This code handles SEQUENTIAL and LINE SEQUENTIAl
  char ch = '\0';
  size_t characters_to_write;

  int lcount;

  if( lines < -1 )
    {
    // We are using -666 for a form feed
    ch = internal_ff;  // Form feed
    lcount = 1;
    }
  else if( lines == -1 )
    {
    // -1 is a flag that no vertical control is requested
    lcount = 0;
    }
  else if( lines == 0 )
    {
    lcount = 1;
    ch = internal_return;
    }
  else /* if( lines > 0 ) */
    {
    lcount = lines;
    ch = internal_newline;
    }

  // By default, we write out the number of characters in the record area
  characters_to_write = length;

  // That gets overridden if there is a record_length
  if( file->record_length )
    {
    int rdigits;
    characters_to_write = (int)__gg__binary_value_from_field(
                                                      &rdigits,
                                                      file->record_length);
    }

  if( file->org == file_line_sequential_e )
    {
    // If file-sequential, then trailing spaces are removed:
    while(     characters_to_write > 0
               && location[characters_to_write-1] == internal_space )
      {
      characters_to_write -= 1;
      }
    }

  if( after && file->org == file_line_sequential_e && ch == internal_newline )
    {
    // In general, we terminate every line with a newline.  Because this
    // line is supposed to start with a newline, we decrement the line
    // counter by one if we had already sent one.
    if( lcount && (   file->recent_char == internal_newline
                     || file->recent_char == internal_ff) )
      {
      lcount -= 1;
      }
    }

  if( after )
    {
    while(lcount--)
      {
      fputc(ch, file->file_pointer);
      if( handle_ferror(file, __func__, "fputc() error [3]") )
        {
        goto done;
        }
      file->recent_char = ch;
      }
    // That might have been a formfeed; switch back to newline:
    ch = internal_newline;
    }

  switch(file->org)
    {
    case file_line_sequential_e:
      if( characters_to_write )
        {
        fwrite( location,
                characters_to_write,
                1,
                file->file_pointer);
        if( handle_ferror(file, __func__, "fwrite() error") )
          {
          goto done;
          }
        }
      file->recent_char = '\0';
      break;

    case file_sequential_e:
      if( characters_to_write )
        {
        // File sequential records can start off with a four-byte
        // preamble.

        if(    characters_to_write < file->record_area_min
               || characters_to_write > file->record_area_max)
          {
          file->io_status = FsBoundWrite; // "44"
          goto done;
          }

        if( file->record_area_min != file->record_area_max )
          {
          // Because of the min/max mismatch, we require a preamble:
          // The first two bytes are the big-endian character count
          const unsigned char preamble[4] =
            {
            (unsigned char)(characters_to_write>>8),
            (unsigned char)(characters_to_write),
            0,
            0
            };

          fwrite( preamble,
                  4,
                  1,
                  file->file_pointer);
          if( handle_ferror(file, __func__, "fwrite() error") )
            {
            goto done;
            }
          }

        fwrite( location,
                characters_to_write,
                1,
                file->file_pointer);
        if( handle_ferror(file, __func__, "fwrite() error") )
          {
          goto done;
          }
        }
      file->recent_char = '\0';
      break;

    default:
      fprintf(stderr,
              "%s(): Unhandled cbl_file_org_t %d\n",
              __func__,
              file->org);
      exit(1);
      break;
    }

  if( after && lines>0 && file->org == file_line_sequential_e )
    {
    // Special case:  when AFTER NON-ZERO lines, we stick a newline on the
    // end of this record:
    fputc(ch, file->file_pointer);
    if( handle_ferror(file, __func__, "fputc() error [4]") )
      {
      goto done;
      }
    file->recent_char = internal_newline;
    }

  if( !after  )
    {
    // We did the output BEFORE, so now it's time to send some internal_newlines
    while(lcount--)
      {
      fputc(ch, file->file_pointer);
      if( handle_ferror(file, __func__, "fputc() error [5]") )
        {
        goto done;
        }
      file->recent_char = ch;
      }
    }

done:
  file->prior_op = file_op_write;
  establish_status(file, -1);
  }

static void
indexed_file_write( cblc_file_t    *file,
              const unsigned char  *location,
                    size_t          length,
                    bool            is_random)
  {
  // This routine handles FILE WRITE to INDEXED files

  // Each record starts with a four-byte preamble.  The first two bytes are
  // a big-endian binary value indicating the length of the record (not
  // including the preamble).  The third byte is zero; the fourth byte is zero
  // for a deleted record and one for an active record.

  file->errnum = 0;
  file->io_status = FsErrno;
  long position_to_write;

  int key_number = 1; // We are concerned with the primary key:
  // Pick up our structure for the primary_key
  file_index_t *file_index = &file->supplemental->indexes[key_number];

  // Check to make sure the WRITE is consistent with the mode:
  if( !is_random )
    {
    // sequential mode is OUTPUT or EXTEND.  We can only add things at the end
    if( file->mode_char != 'w' && file->mode_char != 'a' )
      {
      file->io_status = FsNoWrite;    // "48"
      goto done;
      }

    if( file_index->key_to_position.size() == 0 )
      {
      // We are dealing with an empty file, so we'll be writing at
      // starting_position, which is set to the end
      }
    else
      {
      // The primary key for a new record in an indexed file in sequential
      // access mode has to be greater than the biggest existing one:

      std::multimap<std::vector<unsigned char>, long>::const_reverse_iterator
        last_element = file_index->key_to_position.crbegin();
      std::vector<unsigned char> biggest_key = last_element->first;

      std::vector<unsigned char> new_key = file_indexed_make_key( file,
                                                                  key_number);

      // Quick & dirty comparison to make sure our new key is greater than
      // the biggest_key:
      bool okay = (memcmp(new_key.data(), biggest_key.data(), new_key.size() ) > 0);
      if( !okay )
        {
        // Create out-of-sequence INVALID KEY condition
        file->io_status = FsKeySeq;    // "21"
        goto done;
        }
      // We are allowed to do the write.  Because this is "w" or "a",
      // it will be at the end
      }
    }
  else
    {
    // Because access is random or dynamic the mode has to be OUTPUT or I-O
    if( file->mode_char != 'w' && file->mode_char != '+' )
      {
      file->io_status = FsNoWrite;    // "48"
      goto done;
      }

    // We are allowed to do the write, but only if there will be no key
    // violations as a result:

    for(size_t  keynum=1;
                keynum<file->supplemental->indexes.size();
                keynum++)
      {
      if( file->supplemental->uniques[keynum] )
        {
        long record_position = file_indexed_first_position(file, keynum);
        if( record_position != -1 )
          {
          // No can do, because we already have a unique key with that value
          file->io_status = FsDupWrite;    // "22"
          goto done;
          }
        }
      }

    // This record is not in the data file; figure out where to put it
    if( file->mode_char == '+' )
      {
      // This is a '+' for I-O, so we can write into a hole.

      // See if we have a hole that is the right size:
      bool found_hole = false;
      for( size_t i=0; i<file->supplemental->holes.size(); i++ )
        {
        if( file->supplemental->holes[i].size == length )
          {
          // We found a hole, and we're going to use it

          // Pick up the position
          position_to_write = file->supplemental->holes[i].location;

          // Get rid of the hole we just filled:
          file->supplemental->holes[i] = file->supplemental->holes.back();
          file->supplemental->holes.pop_back();

          // Position the file, ready to write:
          fseek(file->file_pointer, position_to_write, SEEK_SET);
          if( handle_ferror(file, __func__, "fseek() error") )
            {
            goto done;
            }
          found_hole = true;
          break;
          }
        }
      if( !found_hole )
        {
        // There is no hole, so make sure we are writing at the end
        fseek(file->file_pointer, 0, SEEK_END);
        if( handle_ferror(file, __func__, "fseek() error") )
          {
          goto done;
          }
        }
      }
    }

  // The file is positioned for where the write is to take place.  And we know
  // that there will be no key violation when we update the indices.

  position_to_write = ftell(file->file_pointer);
  if( handle_ferror(file, __func__, "ftell() error") )
    {
    goto done;
    }

  // We are currently located where the new data must be written.
  unsigned char ach[4];
  ach[0] = (unsigned char)(length>>8);
  ach[1] = (unsigned char)length;
  ach[2] = 0;
  ach[3] = 1;

  // Write out the preamble:
  fwrite(ach, 4, 1, file->file_pointer);
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

  // Write out the data:
  fwrite(location, length, 1, file->file_pointer);
  if( handle_ferror(file, __func__, "fwrite() error") )
    {
    goto done;
    }

  file_indexed_update_indices(file, position_to_write);

done:
  file->prior_op = file_op_write;
  establish_status(file, -1);
  }

static void
__io__file_write(   cblc_file_t    *file,
              const unsigned char  *location,
                    size_t          length,
                    int             after,
                    int             lines,
                    int             is_random )
  {
  // After an epic discussion with Marty, a determination has been made to
  // ignore the IBM and ISO specifications, and treat an unadorned WRITE as
  // if it were the same as WRITE xxx BEFORE 1 LINE

  file->errnum = 0;
  file->io_status = FsErrno;

  if( !file->file_pointer )
    {
    // Attempting to write a file that isn't open
    file->io_status = FsNoWrite;    // "48"
    goto done;
    }

  switch( file->access )
    {
    case file_access_seq_e:
      if( file->mode_char != 'w' && file->mode_char != 'a' )
        {
        // File is open, but not in OUTPUT or EXTEND mode
        file->io_status = FsNoWrite;    // "48"
        goto done;
        }
      break;

    case file_access_rnd_e:
    case file_access_dyn_e:
      if( file->mode_char != 'w' && file->mode_char != '+' )
        {
        // File is open, but not in I-O or OUTPUT or
        file->io_status = FsNoWrite;    // "48"
        goto done;
        }
      break;

    default:
      warnx("%s(): Thanks for playing. Next contestant, please", __func__);
      abort();
      break;
    }

  if( file->record_length )
    {
    int rdigits;
    length = (size_t)__gg__binary_value_from_field( &rdigits,
                                                    file->record_length);
    }

  switch(file->org)
    {
    case file_line_sequential_e:
    case file_sequential_e:
      sequential_file_write(file, location, length, after, lines);
      break;

    case file_relative_e:
      {
      relative_file_write(file, location, length, is_random);
      break;
      }

    case file_indexed_e:
      {
      indexed_file_write(file, location, length, is_random);
      break;
      }

    default:
      fprintf(stderr, "%s(): Unhandled cbl_file_org_t %d\n",
              __func__,
              file->org);
      exit(1);
      break;
    }
done:
  file->prior_op = file_op_write;
  establish_status(file, -1);
  if( file->io_status < FhNotOkay )
    {
    file->flags |= file_flag_existed_e;
    }
  }

static void
line_sequential_file_read(  cblc_file_t *file)
  {
  file->errnum = 0;
  file->io_status = FsErrno;
  size_t characters_read = 0;
  size_t remaining;
  bool hit_eof;

  // According to IBM:

  // Characters are read one at a time until:
  // - A delimiter is reached.  It is discarded, and the
  //   record area is filled with spaces.
  // - The entire record area is filled.  If the next unread
  //   character is the delimiter, it is discarded.  Otherwise,
  //   it becomes the first character read by the next READ
  // - EOF is encountered; the remainder of the record area
  //   is filled with spaces.

  // This contradicts the ISO/IEC 2014 standard, which says
  // in section 14.9.29.3, paragraph 14) on page 554 that excess
  // characters are discarded, and too-short records have
  // characters to the right as undefined.  I'm going with IBM,
  // it makes more sense to me.

  // We first stage the data into the record area.
  int ch;

  long fpos = ftell(file->file_pointer);
  if( handle_ferror(file, __func__, "ftell() error") )
    {
    fpos = -1;
    goto done;
    }

  hit_eof = false;
  while( characters_read < file->record_area_max )
    {
    ch = fgetc(file->file_pointer);
    file->errnum = ferror(file->file_pointer);
    if( ch == file->delimiter )
      {
      break;
      }
    if( ch == EOF )
      {
      hit_eof = true;
      clearerr(file->file_pointer);
      break;
      }
    if( handle_ferror(file, __func__, "fgetc() error") )
      {
      fpos = -1;
      goto done;
      }
    file->default_record->data[characters_read] = (char)ch;
    characters_read += 1;
    }
  remaining = characters_read;
  while(remaining < file->record_area_max )
    {
    // Space fill shorty records
    file->default_record->data[remaining++] = internal_space;
    }

  if( hit_eof && !characters_read)
    {
    // We got an end-of-file without characters
    file->io_status = FsEofSeq; // "10"
    file->prior_read_location = -1;
    }
  else if( hit_eof )
    {
    // We got an end-of-file whilst reading characters
    // Override the FsEofSeq.  We'll get an actual EOF if the programmer
    // does another READ:
    file->io_status = FsErrno;
    }
  else if (characters_read < file->record_area_max)
    {
    // Just discard an early record delimiter
    file->io_status = FsRecordLength;   // "04"
    }
  else // We filled the whole record area.  Look ahead one character
    {
#ifdef POSSIBLY_IBM
    // In this code, unread characters before the internal_newline
    // are read next time.  See page 133 of the IBM Language Reference
    // Manual: "If the first unread character is the record delimiter, it
    // is discarded. Otherwise, the first unread character becomes the first
    // character read by the next READ statement."
    ch = fgetc(file->file_pointer);
    file->errnum = ferror();
    // If that next character isn't a delimiter, put it back:
    if( ch != file->delimiter && ch != EOF)
      {
      ungetc(ch, file->file_pointer);
      }
    else if( handle_ferror(file->file_pointer, __func__, "fgetc() error") )
      {
      fpos = -1;
      goto done;
      }
#else
    // In this code, extra characters before the internal_newline
    // are read next time are discarded.  GnuCOBOL works this way, and
    // the Michael Coughlin "Beginning COBOL" examples require this mode.
    // The ISO/IEC 2014 standard is silent on the question of LINE
    // SEQUENTIAL; it describes only SEQUENTIAL.
    for(;;)
      {
      ch = fgetc(file->file_pointer);
      file->errnum = ferror(file->file_pointer);
      // We can't use handle_ferror() directly, because an EOF is
      // a legitimate way to end the last line.
      if( ch == file->delimiter || ch == EOF)
        {
        clearerr(file->file_pointer);
        break;
        }
      if(     ferror(file->file_pointer)
          &&  handle_ferror(file, __func__, "fgetc() error") )
        {
        fpos = -1;
        goto done;
        }
      file->io_status = FsRecordLength;   // "04"
      }
#endif
    }

  if( file->record_length )
    {
    __gg__int128_to_field(file->record_length,
                                    characters_read,
                                    0,
                                    truncation_e,
                                    NULL);
    }
done:
  file->prior_op = file_op_read;
  establish_status(file, fpos);
  }

static size_t
sequential_file_read(  cblc_file_t  *file)
  {
  unsigned char preamble[4];
  size_t characters_read;
  size_t bytes_in_record;
  size_t bytes_to_read;
  long fpos;

  file->errnum = 0;

  if( file->io_status >= FsEofSeq ) // "10"
    {
    // There is a special error code for trying to read a file after an error
    // or after an EOF has been encountered
    file->io_status = FsReadError; // "46"
    fpos = -1;
    goto done;
    }

  file->io_status = FsErrno;

  fpos = ftell(file->file_pointer);
  if( handle_ferror(file, __func__, "ftell() error") )
    {
    fpos = -1;
    goto done;
    }

  if( file->record_area_min != file->record_area_max )
    {
    // Because of the different sizes, we are expecting a preamble:

    // An error at this point is either an error, or a true end-of-file.
    characters_read = fread(preamble, 1, 4, file->file_pointer);
    if( handle_ferror(file, __func__, "fread() error") )
      {
      // This also comes back true for an ordinary end-of-file
      fpos = -1;
      goto done;
      }

    if( characters_read != 4 )
      {
      // If the preamble is incomplete, treat that as an EOF
      file->io_status = FsEofSeq; // "10"
      file->prior_read_location = -1;
      fpos = -1;
      goto done;
      }

    // Extract the count of bytes in the record from the preamble
    bytes_in_record = ((size_t)preamble[0]<<8) + preamble[1];
    }
  else
    {
    // Because the min and max are the same, we figure on reading that many
    // characters:
    bytes_in_record = file->record_area_max;
    }


  // We are now going to read that many bytes from the file into the record area
  // Let's make sure that bogus input doesn't cause us to fall off the end of
  // the world:
  bytes_to_read = std::min( bytes_in_record,
                            file->record_area_max);

  characters_read = fread(file->default_record->data,
                          1,
                          bytes_to_read,
                          file->file_pointer);
  if( handle_ferror(file, __func__, "fread() error") )
    {
    fpos = -1;
    goto done;
    }
  if( characters_read < bytes_in_record )
    {
    memset(file->default_record->data, internal_space, bytes_to_read);
    file->io_status = FsEofSeq; // "10"
    fpos = -1;
    goto done;
    }

  // Let the caller know if we got too few or too many characters
  if(     bytes_in_record < file->record_area_min
          ||  bytes_in_record > file->record_area_max )
    {
    file->io_status = FsRecordLength;   // "04"
    }

  if( bytes_in_record > file->record_area_max )
    {
    // Let's fix the misalignment
    fseek(file->file_pointer,
          bytes_in_record - file->record_area_max,
          SEEK_CUR);
    }

  if( file->record_length )
    {
    __gg__int128_to_field(file->record_length,
                                    characters_read,
                                    0,
                                    truncation_e,
                                    NULL);
    }
done:
  file->prior_op = file_op_read;
  establish_status(file, fpos);
  return characters_read;
  }

static void
relative_file_read_varying( cblc_file_t *file,
                            int where)
  {
  //  where -2 PREVIOUS
  //  where -1 NEXT
  //  where 0 who the hell knows
  //  where 1 or more: random read

  bool is_random = where > 0;

  file->errnum = 0;
  file->io_status = FsErrno;

  relative_file_parameters rfp;

  size_t characters_read = 0;
  long fpos = -1;
  size_t payload_length;

  if( where >= 1 )
    {
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        RESPECT_LIMITS,
                                        DONT_INIT_KEY,
                                        is_random) )
      {
      goto done;
      }

    if( rfp.record_position >= rfp.file_size )
      {
      // We're falling off the end of the file, which means our key doesn't
      // exist
      file->io_status = FsNotFound;   // "23"
      goto done;
      }

    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    }
  else if( where == -1)
    {
    // This is a sequential NEXT read of a RELATIVE file
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        IGNORE_LIMITS,
                                        INIT_KEY,
                                        is_random) )
      {
      goto done;
      }
    }
  else
    {
    warnx("Whatever are we doing here?");
    abort();
    }

  // We are now poised to read a record,
  for(;;)
    {
    if( !rfp.inside_existing_file )
      {
      file->io_status = FsEofSeq; // "10"
      file->prior_read_location = -1;
      goto done;
      }

    fread(&payload_length, 8, 1, file->file_pointer);
    if( handle_ferror(file, __func__, "fread() error") )
      {
      fpos = -1;
      goto done;
      }

    if( payload_length )
      {
      // We have a good record to read:

      // Read the characters into the record area:
      fread(file->default_record->data,
            1,
            file->record_area_max,
            file->file_pointer);
      if( handle_ferror(file, __func__, "fread() error") )
        {
        goto done;
        }
      // Having read that that data, set up for a subsequent delete/rewrite.
      // Set fpos to the current_file_position, and then update the current
      // file position.
      fpos = rfp.current_file_position;

      // We need to change the file position pointer to point to the next
      // record.

      rfp.current_file_position += rfp.record_size;
      fseek(file->file_pointer,
            rfp.current_file_position,
            SEEK_SET);
      if( handle_ferror(file, __func__, "fseek() error") )
        {
        fpos = -1;
        goto done;
        }

      break;
      }
    else
      {
      // There isn't a record in this slot
      if( where == -1 )
        {
        // But we are in next_record mode.  It is our duty and obligation
        // to skip merrily through the file looking for the next valid
        // record for the lazy bums who called us.
        rfp.record_position       += rfp.record_size;
        rfp.flag_position         += rfp.record_size;
        rfp.inside_existing_file
                   = rfp.record_position + rfp.record_size <= rfp.file_size;
        rfp.key_value = 1 + rfp.record_position
                            / (rfp.preamble_size
                              + rfp.payload_size
                              + rfp.postamble_size);

        fseek(file->file_pointer, rfp.record_position, SEEK_SET);
        if( handle_ferror(file, __func__, "fseek() error") )
          {
          goto done;
          }
        rfp.current_file_position = rfp.record_position;
        continue;
        }
      break;
      }
    }

  characters_read = payload_length;
  if( characters_read == 0 )
    {
    file->io_status = FsNotFound;   // "23"
    goto done;
    }
  else
    {
    if( where < 0 )
      {
      // We did a FORMAT 1 read, so we need to update the key, if there is one
      if( file->keys[0] )
        {
        long max_key = max_value(file->keys[0]);
        if( rfp.key_value >= max_key )
          {
          // This is an oddball COBOL error: A sequential read would result in
          // reading a record whose relative record number is too big for
          // file->key to hold
          file->io_status = FsEofRel; // "14"
          goto done;
          }
        __gg__int128_to_field(file->keys[0],
                                        rfp.key_value,
                                        0,
                                        truncation_e,
                                        NULL);
        }
      }
    }
done:
  if( file->record_length )
    {
    __gg__int128_to_field(file->record_length,
                                    payload_length,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  file->prior_op = file_op_read;
  establish_status(file, fpos);
  }

static void
relative_file_read( cblc_file_t *file,
                    int where)
  {
  //  where -2 means PREVIOUS
  //  where -1 means NEXT
  //  where  0 means random read, based on the key

  if( file->record_area_min != file->record_area_max )
    {
    return relative_file_read_varying(file, where);
    }

  bool is_random = where > 0;

  file->errnum = 0;
  file->io_status = FsErrno;

  relative_file_parameters rfp;

  size_t characters_read = 0;
  long fpos = -1;

  if( where >= 1 )
    {
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        RESPECT_LIMITS,
                                        DONT_INIT_KEY,
                                        is_random) )
      {
      goto done;
      }

    if( rfp.record_position >= rfp.file_size )
      {
      // We're falling off the end of the file, which means our key doesn't
      // exist
      file->io_status = FsNotFound;   // "23"
      goto done;
      }

    fseek(file->file_pointer, rfp.record_position, SEEK_SET);
    if( handle_ferror(file, __func__, "fseek() error") )
      {
      goto done;
      }
    }
  else if( where == -1)
    {
    // This is a sequential NEXT read of a RELATIVE file
    if( relative_file_parameters_get(   rfp,
                                        rfm_microfocus_e,
                                        file,
                                        IGNORE_LIMITS,
                                        INIT_KEY,
                                        is_random) )
      {
      goto done;
      }
    }
  else
    {
    warnx("Whatever are we doing here?");
    abort();
    }

  // The following code is predicated on rfm_microfocus_e.

  // We are now poised to read a record, provided the flag byte is
  // indicates this is a good record
  for(;;)
    {
    if( !rfp.inside_existing_file )
      {
      file->io_status = FsEofSeq; // "10"
      file->prior_read_location = -1;
      goto done;
      }
    char record_marker;
    if( pread(rfp.fd, &record_marker, 1, rfp.flag_position) <= 0)
      {
      goto done;
      }
    if(record_marker == internal_newline)
      {
      // We have a good record to read:

      // We need to change the file_position_pointer to reflect any
      // preamble:
      if( rfp.preamble_size )
        {
        fseek(file->file_pointer, rfp.preamble_size, SEEK_CUR);
        if( handle_ferror(file, __func__, "fseek() error") )
          {
          goto done;
          }
        }
      // Read the characters into the record area:
      characters_read = fread(file->default_record->data,
                              1,
                              file->record_area_max,
                              file->file_pointer);
      if( handle_ferror(file, __func__, "fread() error") )
        {
        goto done;
        }
      // Having read that that data, set up for a subsequent delete/rewrite.
      // Set fpos to the current_file_position, and then update the current
      // file position.
      fpos = rfp.current_file_position;

      // We need to change the file position pointer to point to the next
      // record.

      rfp.current_file_position += rfp.record_size;
      fseek(file->file_pointer,
            rfp.current_file_position,
            SEEK_SET);
      if( handle_ferror(file, __func__, "fseek() error") )
        {
        fpos = -1;
        goto done;
        }

      break;
      }
    else
      {
      // There isn't a record in this slot
      if( where == -1 )
        {
        // But we are in next_record mode.  It is our duty and obligation
        // to skip merrily through the file looking for the next valid
        // record for the lazy bums who called us.
        rfp.record_position       += rfp.record_size;
        rfp.flag_position         += rfp.record_size;
        rfp.inside_existing_file
                   = rfp.record_position + rfp.record_size <= rfp.file_size;
        rfp.key_value = 1 + rfp.record_position
                            / (rfp.preamble_size
                              + rfp.payload_size
                              + rfp.postamble_size);

        fseek(file->file_pointer, rfp.record_position, SEEK_SET);
        if( handle_ferror(file, __func__, "fseek() error") )
          {
          goto done;
          }
        rfp.current_file_position = rfp.record_position;
        continue;
        }
      break;
      }
    }

  if( characters_read == 0 )
    {
    file->io_status = FsNotFound;   // "23"
    goto done;
    }
  else
    {
    if( where < 0 )
      {
      // We did a FORMAT 1 read, so we need to update the key, if there is one
      if( file->keys[0] )
        {
        long max_key = max_value(file->keys[0]);
        if( rfp.key_value >= max_key )
          {
          // This is an oddball COBOL error: A sequential read would result in
          // reading a record whose relative record number is too big for
          // file->key to hold
          file->io_status = FsEofRel; // "14"
          goto done;
          }
        __gg__int128_to_field(file->keys[0],
                                        rfp.key_value,
                                        0,
                                        truncation_e,
                                        NULL);
        }
      }
    }
done:
  if( file->record_length )
    {
    __gg__int128_to_field(file->record_length,
                                    characters_read,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  file->prior_op = file_op_read;
  establish_status(file, fpos);
  }

static void
indexed_file_read(  cblc_file_t  *file,
                    int key_number)
  {
  long record_length;
  int flag;
  int read_res;

  if( key_number == 0 )
    {
    // This is an implicit next
    key_number = -1;
    }

  size_t characters_read = 0;
  file_index_t *file_index;
  long fpos = -1;

  if( key_number >= 1 )
    {
    // It is meat and potatoes time.  We need to pick up the first record
    // with the specified key:
    file->errnum = 0;
    file->io_status = FsErrno;

    file->recent_key = key_number;
    file_index = &file->supplemental->indexes[key_number];
    fpos = file_indexed_first_position(file, key_number);
    if( fpos == -1 )
      {
      file->io_status = FsNotFound; // "23"
      goto done;
      }
    fpos = file_indexed_first_position(file, key_number);
    file_index->current_iterator++;
    }
  else if( key_number == -1 )
    {
    // We are ready to do a sequential read of an INDEXED file

    // There is a special code for trying to read after a preceding unsuccessful
    // statement:

    if( file->io_status >= FsEofSeq ) // "10"
      {
      file->io_status = FsReadError; // "46"
      goto done;
      }

    file->errnum = 0;
    file->io_status = FsErrno;

    file_index = &file->supplemental->indexes[file->recent_key];

    // When you arrive here, the current_indicator points to the iterator
    // *after* the prior successful read
    if( file_index->current_iterator == file_index->key_to_position.end() )
      {
      // We have hit the end of keys
      file->io_status = FsEofSeq; // "10"
      file->prior_read_location = -1;
      goto done;
      }

    // cppcheck-suppress derefInvalidIteratorRedundantCheck
    fpos = file_index->current_iterator->second;

    if( file_index->current_iterator == file_index->key_to_position.end() )
      {
      __gg__abort("indexed_file_read(): fell off of file_index->key_to_position");
      }
    file_index->current_iterator++;
    }
  else if( key_number == -2 )
    {
    // We are ready to do a sequential read PREVIOUS of an INDEXED file

    // There is a special code for trying to read after a preceding unsuccessful
    // statement:
    if( file->io_status >= FsEofSeq ) // "10"
      {
      file->io_status = FsReadError; // "46"
      goto done;
      }

    file->errnum = 0;
    file->io_status = FsErrno;

    file_index = &file->supplemental->indexes[file->recent_key];

    /*  We need to do a little thinking.  After an OPEN or START,
     *  file_index->current_iterator  points to the next record to read for
     *  either a READ NEXT or READ PREVIOUS.
     *
     *  But after a READ, a subsequent READ PREVIOUS needs to back the iterator
     *  down by 2 to get the correct record.
     */

    if( file->prior_op == file_op_read )
      {
      // We just read record 10, so the iterator points to 11.  We need to back
      // it down to 9, if we can.  (There might not be enough records)

      // We know we can back it down at least one record
      file_index->current_iterator--; // Make it point back to 10
      if( file_index->current_iterator == file_index->key_to_position.begin() )
        {
        // but we can't back it down another
        file->io_status = FsEofSeq; // "10"
        file->prior_read_location = -1;
        goto done;
        }
      file_index->current_iterator--; // Make it point back to 9
      }
    else if( file->prior_op == file_op_delete )
      {
      if( file->prior_read_location == -1 )
        {
        // The prior operation deleted the record we are set up to read.
        // Let's finesse the positioning.
        if( file_index->current_iterator == file_index->key_to_position.end() )
          {
          // We have hit the end of the keys.
          file->io_status = FsEofSeq; // "10"
          file->prior_read_location = -1;
          goto done;
          }
        file_index->current_iterator--;
        }
      }
    else
      {
      // This must be after an OPEN or START.  Check to make sure that the
      // file isn't actually empty

      if( file_index->current_iterator == file_index->key_to_position.end() )
        {
        // We have hit the end of the keys.
        file->io_status = FsEofSeq; // "10"
        file->prior_read_location = -1;
        goto done;
        }
      }

    // We are ready to proceed

    // cppcheck-suppress derefInvalidIteratorRedundantCheck
    fpos = file_index->current_iterator->second;
    if( file_index->current_iterator == file_index->key_to_position.end() )
      {
      __gg__abort("indexed_file_read(): fell off of file_index->key_to_position");
      }
    file_index->current_iterator++;
    }

  // fpos is where the data are:
  fseek(file->file_pointer, fpos, SEEK_SET);
  if( handle_ferror(file, __func__, "fseek() error") )
    {
    fpos = -1;
    goto done;
    }

  read_res = read_an_indexed_record(file,
                                    file->record_area_max,
                                    record_length,
                                    flag);
  if( read_res )
    {
    // We hit an end of file or an error
    fpos = -1;
    goto done;
    }

  if(flag != 1)
    {
    warnx("The file isn't right; key number %d found a deleted record",
          key_number);
    abort();
    }

  characters_read = record_length;

done:
  if( file->record_length )
    {
    __gg__int128_to_field(file->record_length,
                                    characters_read,
                                    0,
                                    truncation_e,
                                    NULL);
    }
  file->prior_op = file_op_read;
  establish_status(file, fpos);
  }

static void
__io__file_read(cblc_file_t *file,
                int where)
  {
  // where = -2 means PREVIOUS
  // where = -1 means NEXT
  // where =  1 or more means key N, where N is one-based

  file->errnum = 0;

  if(     !(file->flags & file_flag_existed_e)
      &&   (file->flags & file_flag_optional_e))
    {
    // Trying to read a file that didn't exist during file_open.
    if( file->org == file_sequential_e || file->org == file_line_sequential_e )
      {
      if( file->io_status < FhNotOkay )
        {
        // This is a format 1 read
        file->io_status = FsEofSeq; // "10"
        }
      else
        {
        file->io_status = FsReadError; // "46"
        }
      file->prior_op = file_op_read;
      establish_status(file, -1);
      return;
      }
    else
      {
      // The indexed or relative file didn't exist, so set an INVALID KEY
      // condition:
      if( where <= 0 )
        {
        if( file->io_status < FhNotOkay )
          {
          // This is a format 1 read
          file->io_status = FsEofSeq; // "10"
          }
        else
          {
          file->io_status = FsReadError; // "46"
          }
        file->prior_op = file_op_read;
        establish_status(file, -1);
        }
      else
        {
        // This is a format 2 read
        file->io_status = FsNotFound; // "23"
        file->prior_op = file_op_read;
        establish_status(file, -1);
        }
      return;
      }
    }

  if( !file->file_pointer )
    {
    // Attempting to read a file that isn't open
    file->io_status = FsReadNotOpen;    // "47"
    file->prior_op = file_op_read;
    establish_status(file, -1);
    return;
    }

  if( file->mode_char != 'r' && file->mode_char != '+' )
    {
    // The file is open, but not in INPUT or I-O mode:
    file->io_status = FsReadNotOpen;    // "47"
    file->prior_op = file_op_read;
    establish_status(file, -1);
    return;
    }

  switch(file->org)
    {
    case file_line_sequential_e:
      {
      line_sequential_file_read(file);
      break;
      }

    case file_sequential_e:
      {
      sequential_file_read(file);
      break;
      }

    case file_relative_e:
      {
      relative_file_read(file, where);
      break;
      }

    case file_indexed_e:
      {
      indexed_file_read(file, where);
      break;
      }

    default:
      fprintf(stderr,
              "%s(): Unhandled cbl_file_org_t %d\n",
              __func__,
              file->org);
      exit(1);
      break;
    }
  if( file->io_status < FhNotOkay )
    {
    file->flags |= file_flag_existed_e;
    }
  }

static void
file_indexed_open(cblc_file_t *file)
  {
  // The file was supposed to be open when you got here.
  if( !file->file_pointer )
    {
    __gg__abort("file_indexed_open(): file_pointer is NULL");
    }

  file->supplemental = new supplemental_t;

  // We need one multimap for each key. Whenever we are told about a key, it'll
  // be by key_number.  key_number 1 is by convention the primary key.  There
  // will be no key_number zero, so we are going to start our vector of indexes
  // with an empty placeholder:

  unsigned char *stash = NULL;

  file_index_t file_index;
  file->supplemental->indexes.push_back(file_index);
  file->supplemental->uniques.push_back(0);

  // Add one entry to the indexes for each key number:
  int current_key_number = 0;
  size_t index = 0;
  while(file->key_numbers[index] != -1)
    {
    if( file->key_numbers[index] != current_key_number )
      {
      file->supplemental->indexes.push_back(file_index);
      current_key_number = file->key_numbers[index];
      file->supplemental->uniques.push_back(file->uniques[index]);
      }
    index += 1;
    }

  // To build indexes, we need to be at the beginning:
  fseek(file->file_pointer, 0, SEEK_SET);
  if( handle_ferror(file, __func__, "fseek() after fopen() failed") )
    {
    goto done;
    }

  switch( file->mode_char )
    {
    case 'w':
      // OUTPUT mode causes an empty file to be created, so the indices
      // are empty as well
      break;

    case 'r':
    case 'a':
    case '+':
      if( file->flags & file_flag_existed_e )
        {
        // We need to open the file for reading, and build the
        // maps for each index:
        static size_t fname_size = MINIMUM_ALLOCATION_SIZE;
        static char *fname = static_cast<char *>(malloc(fname_size));
        massert(fname);

        internal_to_console(&fname,
                            &fname_size,
                            file->filename, strlen(file->filename));

        // We are going to scan through the entire file, building index
        // entries for each record.

        // It should already be at the beginning:
        if( ftell(file->file_pointer) != 0 )
          {
          __gg__abort("file_indexed_open():"
                      " file_pointer should be at the beginning");
          }

        // Stash the existing record area:
        stash = static_cast<unsigned char *>(malloc(file->record_area_max));
        massert(stash);
        memcpy( stash,
                file->default_record->data,
                file->record_area_max);

        for(;;)
          {
          // Remember where we are right now:
          long record_length;
          int  flag;
          long record_position = ftell(file->file_pointer);
          if( handle_ferror(file, __func__, "ftell() error") )
            {
            goto done;
            }

          int read_result = read_an_indexed_record( file,
                                                    file->record_area_max,
                                                    record_length,
                                                    flag);
          if( read_result == 1 )
            {
            // Don't panic; it's just an end-of-file
            break;
            }
          else if( read_result > 1 )
            {
            // It was an error of some kind
            goto done;
            }

          if( flag == 1 )
            {
            // We have a good record in our record area:

            if( !file_indexed_update_indices(file, record_position) )
              {
              // There must have been a duplicate UNIQUE index, or some other
              // problem.
              goto done;
              }
            }
          else
            {
            // This is a hole in the file; make it available for a WRITE or
            // EXTEND
            file_hole_t hole = {record_position, (size_t)record_length};
            file->supplemental->holes.push_back(hole);
            }
          }

        file->io_status = FsErrno;
        fseek(file->file_pointer, 0, SEEK_SET);
        if( handle_ferror(file, __func__, "fseek() after fopen() failed") )
          {
          goto done;
          }
        }
      break;

    default:
      warnx(  "%s(): This is weird.  mode_char is '%c' (%d)?\n",
              __func__,
              file->mode_char,
              file->mode_char);
      __gg__abort("file_indexed_open(): Unknown mode_char");
      break;
    }
done:
  // We need to initialize the iterators for every index:
  for( size_t i=1; i<file->supplemental->indexes.size(); i++ )
    {
    file->supplemental->indexes[i].current_iterator =
          file->supplemental->indexes[i].key_to_position.begin();
    file->supplemental->indexes[i].ending_iterator =
          file->supplemental->indexes[i].key_to_position.end();
    }
  file->recent_key = 1;

  if( stash )
    {
    // Restore the original record area:
    memcpy( file->default_record->data,
            stash,
            file->record_area_max);
    free(stash);
    }

  fseek(file->file_pointer, 0, SEEK_SET);
  handle_ferror(file, __func__, "fseek() error");

  }

static void
file_indexed_close(cblc_file_t *file)
  {
  delete file->supplemental;
  file->supplemental = NULL;
  }

extern "C"
void
__gg__file_reopen(cblc_file_t *file, int mode_char)
  {
  // This is a useful, although scary, little helper.  The file must not be
  // open.  The file->filename must be valid, as must the file_name_quoted_e flag.
  // You can see it used in __gg__file_open.  You can also see it in
  // __gg__sort_workfile, where the workfile is passed as open for read, and
  // which needs to be closed and reopened first in "r" and then "w" modes.

  // Note that when closing for reopening, you must simply use fclose() and not
  // __gg__file_close().  The second one does bookkeeping and cleanup that is
  // not appropriate here.

  bool the_file_exists;
  bool random_access_mode;
  char achMode[3];

  // Stash the mode_char for later analysis during READ and WRITE operations
  file->mode_char = mode_char;
  char *trimmed_name;
  trimmed_name = get_filename(file, !!(file->flags & file_name_quoted_e));
  if( !trimmed_name[0] )
    {
    bool all_spaces = true;
    for(size_t i=0; i<strlen(file->filename); i++)
      {
      if( file->filename[i] != internal_space )
        {
        all_spaces = false;
        }
      break;
      }
    if( all_spaces )
      {
      warnx("Warning: %s specified with a filename that is all spaces",
            file->name);
      file->io_status = FsNameError;    // "31"
      goto done;
      }

    static size_t fname_size = MINIMUM_ALLOCATION_SIZE;
    static char *fname = static_cast<char *>(malloc(fname_size));
    massert(fname)
    internal_to_console(&fname,
                        &fname_size,
                        file->filename,
                        strlen(file->filename));
    warnx(  "%s(): There is no environment variable named \"%s\"\n",
            __func__,
            fname);
    file->io_status = FsNoFile;    // "35"
    goto done;
    }

  // achMode is the mode string that gets passed down below to fopen().
  random_access_mode = (    file->access == file_access_rnd_e
                              || file->access == file_access_dyn_e);
  the_file_exists = access(trimmed_name, F_OK) == 0;
  file->flags |= the_file_exists ? file_flag_existed_e : file_flag_none_e ;

  // We have four operations: INPUT (r) OUTPUT (w) I-O (+) and EXTEND (a)
  // INPUT and I-O and EXTEND have different results based on is_optional
  // and whether or not the file exists.
  // Various modification take place if random_access_mode is true

  if( the_file_exists )
    {
    switch(mode_char)
      {
      case 'r':
        // OPEN INPUT
        // We need a vanilla read-only file:
        strcpy(achMode, "r");
        break;

      case 'w':
        // OPEN OUTPUT
        // This syntax means create a new file, or overwrite an existing
        // one.  For files with random access mode, we need to be
        // able to read as well as write, because we have to be able
        // to ascertain that a record slot is empty in the event that
        // the programmer tries to write to the same slot twice:
        if( random_access_mode )
          {
          strcpy(achMode, "w+");
          }
        else
          {
          strcpy(achMode, "w");
          }
        break;

      case 'a':
        // EXTEND is for sequential files:
        strcpy(achMode, "a");
        break;

      case '+':
        // I-O
        // We need to be able to read and write the existing file.
        strcpy(achMode, "r+");
        break;

      default:
        fprintf(stderr,
                "%s(): We were given an unknown mode_char %d\n",
                __func__,
                mode_char);
        exit(1);
        break;
      }
    }
  else
    {
    // The file *doesn't* exist
    switch(mode_char)
      {
      case 'r':
        // OPEN INPUT, but the file doesn't exist:
        if( file->flags & file_flag_optional_e )
          {
          // This is a weird condition.  OPTIONAL means "flag it as sort of
          // open but the first read causes AT END or INVALID KEY condition.
          file->io_status = FsUnavail;   // "05"
          goto done;
          }
        else
          {
          file->io_status = FsNoFile;    // "35"
          goto done;
          }
        break;

      case 'w':
        // OPEN OUTPUT
        // This syntax means create a new file, or overwrite an existing
        // one.  For files with random access mode, we need to be
        // able to read as well as write, because we have to be able
        // to ascertain that a record slot is empty in the event that
        // the programmer tries to write to the same slot twice:
        if( random_access_mode )
          {
          strcpy(achMode, "w+");
          }
        else
          {
          strcpy(achMode, "w");
          }
        break;

      case 'a':
        // EXTEND
        if( file->flags & file_flag_optional_e )
          {
          if( random_access_mode )
            {
            // For files that might be sequential or random:
            strcpy(achMode, "a+");
            }
          else
            {
            // For pure sequential files, we just do a straight "a"
            strcpy(achMode, "a");
            }
          file->io_status = FsUnavail;   // "05"
          }
        else
          {
          // Trying to extend a non-optional non-existing file is against the rules
          file->io_status = FsNoFile;    // "35"
          goto done;
          }
        break;

      case '+':
        // I-O
        if( file->flags & file_flag_optional_e )
          {
          // We need to be able to read and write a new file.
          strcpy(achMode, "w+");
          file->io_status = FsUnavail;   // "05"
          }
        else
          {
          file->io_status = FsNoFile;    // "35"
          goto done;
          }
        break;

      default:
        fprintf(stderr,
                "%s(): We were given an unknown mode_char %d\n",
                __func__,
                mode_char);
        exit(1);
        break;
      }
    }

  file->file_pointer = fopen(trimmed_name, achMode);
  if( file->file_pointer == NULL )
    {
    file->errnum = errno;
    // We were unable to open the file
    switch(mode_char)
      {
      case 'r':
      case 'a':
      case '+':
        file->io_status = FsNoFile;   // "35"
        goto done;
        break;

      case 'w':
        file->io_status = FsOsError;  // "30"
        goto done;
        break;
      }
    }
  file->errnum = ferror(file->file_pointer);

  // If this was a OPEN EXTEND, we want the file positioned at the
  // the very end (which it won't be when achMode is "r+"
  if( mode_char == 'a' )
    {
    fseek(file->file_pointer, 0, SEEK_END);
    if( handle_ferror(file, __func__, "fseek() after fopen() failed") )
      {
      goto done;
      }
    }
  if( file->org == file_indexed_e )
    {
    file_indexed_open(file);
    }
  file->recent_char = '\0';

  done:
  file->prior_op = file_op_open;
  }

static void
__io__file_open(cblc_file_t *file,
                char *filename,
                int mode_char,
                int is_quoted)
  {
  // Filename is a pointer to a malloc() buffer.

  // The complication:  A filename can be literal text, it can be from a COBOL
  // alphanumeric variable, or it can be the name of an environment variable
  // that contains the actual name of the file.  The consequence is that if
  // you want to call __gg__file_open from anywhere except the parser_file_open
  // routine, then you had best really know what you are doing.

  file->errnum = 0;
  file->io_status = FsErrno;
  if( file->file_pointer )
    {
    // The file is already open:
    file->io_status = FsIsOpen; // "41" 14.9.26.3 Paragraph 1
    }
  else
    {
    // filename is the result of a strdup or malloc.  We will free() it at
    // file close time.
    file->filename = filename;
    file->flags &= ~file_name_quoted_e;
    file->flags |= is_quoted ? file_name_quoted_e : file_flag_none_e;

    __gg__file_reopen(file, mode_char);
    }
  file->prior_op = file_op_open;
  establish_status(file, -1);
  }

static void
__io__file_close( cblc_file_t *file, int how )
  {
  // We are forcing line-sequential files to end with a newline:

  // if(    file->org == file_line_sequential_e
  // && ( file->mode_char == 'w' || file->mode_char == 'a' )
  // && file->recent_char != internal_newline )
  // {
  // int ch = internal_newline;
  // fputc(ch, file->file_pointer);
  // if( handle_ferror(file, __func__, "fputc() error [6]") )
  // {
  // goto done;
  // }
  // file->recent_char = ch;
  // }

  errno = 0;
  file->io_status = FsErrno;
  long fpos = -1;
  if( !file->file_pointer )
    {
    // Attempting to close a file that isn't open:
    file->io_status = FsCloseNotOpen;  // "42" 14.9.6.3 Paragraph 1
    goto done;
    }

  if( how == file_close_reel_unit_e )
    {
    // Closing a REEL unit.  Leave the file open, and return "07"
    file->io_status = FsNotaTape;  // "07"

    // We have to leave the file position alone:
    fpos = ftell(file->file_pointer);
    goto done;
    }

  if( fclose(file->file_pointer) != 0 )
    {
    handle_ferror(file, __func__, "fclose() error");
    }
  file->file_pointer = NULL;

  if( file->org == file_indexed_e )
    {
    file_indexed_close(file);
    }

  // The filename can be from a COBOL alphanumeric variable, which means it can
  // between a file_close and a subsequent file_open.  So, we get rid of it
  // here
  free(file->filename);
  file->filename = NULL;

  done:
  file->prior_op = file_op_close;
  establish_status(file, fpos);
  }

static cblc_file_t *stashed;

cblc_file_t *
__gg__file_stashed()
  {
  return stashed;
  }

extern "C"
void
__gg__file_stash( cblc_file_t *file )
  {
  ::stashed = file;
  }

    /*
     * The following constitutes a proposal for a public API to gcobol I/O.
     *
     * The class gcobol_io_t would be in a public header file that
     * would be used by any library that provides an I/O
     * implementation.  Additionally, that header would be where
     * cblc_file_t is defined, and where the valid file status values
     * (and relops) are enumerated.
     *
     * The library contructs the class, providing its own pointers,
     * and supplies a known function, gcobol_fileops, to return
     * it. gcobol-compiled programs call the functions, or others
     * supplied by a different implementation, through the pointers.
     *
     * This design makes it possible to run the same code, unaltered,
     * simply by relinking.
     *
     * This design is commonly used, and is similar to the one used in
     * GnuCOBOL.  One difference is that the file status is captured
     * in the cblc_file_t, whereas in GnuCOBOL it is the return value.
     *
     * There is no provision for using more than one implemetation at
     * a time in the same program, as would be needed for CODE-SET
     * support.  To achieve that in C++ without dynamic linking, there
     * would have to be a set of known implementations, each with its
     * own namespace, in which gcobol_fileops would be defined.
     */

class gcobol_io_t {
public:
  static const char constexpr marquee[64] = "libgcobol: gfileio.cc";

  typedef void (open_t)( cblc_file_t *file,
                         char *filename,
                         int mode_char,
                         int is_quoted );
  typedef void (close_t)( cblc_file_t *file,
                          int how );
  typedef void (start_t)( cblc_file_t *file,
                          int relop, // needs enum
                          int first_last_key,
                          size_t length );
  typedef void (read_t)( cblc_file_t *file,
                         int where );
  typedef void (write_t)( cblc_file_t *file,
                          const unsigned char  *location,
                          size_t length,
                          int after,
                          int lines,
                          int is_random );
  typedef void (rewrite_t)( cblc_file_t *file,
                            size_t length, bool is_random );
  typedef void (delete_t)( cblc_file_t *file,
                          bool is_random );

  open_t      *Open;
  close_t     *Close;
  start_t     *Start;
  read_t      *Read;
  write_t     *Write;
  rewrite_t   *Rewrite;
  delete_t    *Delete;

  gcobol_io_t()
    : Open(NULL)
    , Close(NULL)
    , Start(NULL)
    , Read(NULL)
    , Write(NULL)
    , Rewrite(NULL)
    , Delete(NULL)
  {}

  gcobol_io_t(  open_t      *Open,
                 close_t     *Close,
                 start_t     *Start,
                 read_t      *Read,
                 write_t     *Write,
                 rewrite_t   *Rewrite,
                 delete_t    *Delete )
    : Open(Open)
    , Close(Close)
    , Start(Start)
    , Read(Read)
    , Write(Write)
    , Rewrite(Rewrite)
    , Delete(Delete)
  {}

#if FILE_IO_IMPLEMENTED
  int     read_next();
  int     fildelete();
  void    ioinit();
  void    ioexit();
  int     iofork();
  int     iosync();
  int     commit();
  int     rollback();
  int     iounlock();
  char *  ioversion();
#endif
};


// Our implementation returns this populated structure

gcobol_io_t*
gcobol_fileops() {
  return new gcobol_io_t( __io__file_open,
                          __io__file_close,
                          __io__file_start,
                          __io__file_read,
                          __io__file_write,
                          __io__file_rewrite,
                          __io__file_delete );
}

/*
 * To use this structure, whether our gcobolio.so or another, initialize with:
 *
 *     gcobol_io_t * gfile = gcobol_filops(file);
 *
 * That means every gcobol binary expects to be linked to a library
 * that supplies gcobol_fileops().  By default, we link to ours.
 *
 * Then, in libgcobol, replace direct calls with calls through fileops.
 * That is, instead of
 *
 *     __gg__file_open("foo", "r", false );
 * use
 *     gfile->Open("foo", "r", false );
 *
 * You'll probably want some kind of trampoline to avoid the need to
 * generate the Gimple to call through a pointer to a structure:
 */

/*
 * I/O via interface
 */
static gcobol_io_t * gcobol_io = NULL;

static gcobol_io_t *
gcobol_io_funcs() {
  if( ! gcobol_io )
    {
      gcobol_io = gcobol_fileops();
      if( !gcobol_io )
        {
        __gg__abort("gcobol_io_funcs(): gcobol_io is NULL");
        }
    }
  return gcobol_io;
}

extern "C"
void
__gg__file_open(cblc_file_t *file,
                char *filename,
                int mode_char,
                int is_quoted)
  {
    gcobol_io_t *functions = gcobol_io_funcs();
    functions->Open(file, filename, mode_char, is_quoted);
  }

extern "C"
void
__gg__file_close( cblc_file_t *file, int how )
  {
    gcobol_io_t *functions = gcobol_io_funcs();
    functions->Close(file, how);
  }

extern "C"
void
__gg__file_start(  cblc_file_t *file,
                   int relop,
                   int first_last_key,
                   size_t length)
  {
    gcobol_io_t *functions = gcobol_io_funcs();
    functions->Start(file, relop, first_last_key, length);
  }

extern "C"
void
__gg__file_read(cblc_file_t *file,
                int where)
  {
    gcobol_io_t *functions = gcobol_io_funcs();
    functions->Read(file, where);
  }

extern "C"
void
__gg__file_write(   cblc_file_t    *file,
                    unsigned char  *location,
                    size_t          length,
                    int             after,
                    int             lines,
                    int             is_random )
  {
    gcobol_io_t *functions = gcobol_io_funcs();
    functions->Write(file, location, length, after, lines, is_random);
  }

extern "C"
void
__gg__file_rewrite(cblc_file_t *file, size_t length, bool is_random)
  {
    gcobol_io_t *functions = gcobol_io_funcs();
    functions->Rewrite(file, length, is_random);
  }

extern "C"
void
__gg__file_delete(cblc_file_t *file, bool is_random)
  {
    gcobol_io_t *functions = gcobol_io_funcs();
    functions->Delete(file, is_random);
  }

/* end interface functions */

