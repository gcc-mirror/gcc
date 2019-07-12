/* Newlib may not have been built yet.  */
typedef long int64_t;
typedef long size_t;
extern char *strncpy (char *dst, const char *src, size_t length);
extern void exit(int);

void gomp_print_string (const char *msg, const char *value);
void gomp_print_integer (const char *msg, int64_t value);
void gomp_print_double (const char *msg, double value);

/* This struct must match the one used by gcn-run and libgomp.
   It holds all the data output from a kernel (besides mapping data).

   The base address pointer can be found at kernargs+16.

   The next_output counter must be atomically incremented for each
   print output.  Only when the print data is fully written can the
   "written" flag be set.  */
struct output {
  int return_value;
  unsigned int next_output;
  struct printf_data {
    int written;
    char msg[128];
    int type;
    union {
      int64_t ivalue;
      double dvalue;
      char text[128];
    };
  } queue[1024];
  unsigned int consumed;
};

static struct printf_data *
reserve_print_slot (void) {
  /* The kernargs pointer is in s[8:9].
     This will break if the enable_sgpr_* flags are ever changed.  */
  char *kernargs;
  asm ("s_mov_b64 %0, s[8:9]" : "=Sg"(kernargs));

  /* The output data is at kernargs[2].  */
  struct output *data = *(struct output **)(kernargs + 16);

  /* Reserve the slot.  */
  unsigned int index = __atomic_fetch_add (&data->next_output, 1,
					   __ATOMIC_ACQUIRE);

  /* Spinlock while the host catches up.  */
  if (index >= 1024)
    while (__atomic_load_n (&data->consumed, __ATOMIC_ACQUIRE)
	   <= (index - 1024))
      asm ("s_sleep 64");

  if ((unsigned int)(index + 1) < data->consumed)
    {
      /* Overflow.  */
      exit (1);
    }
  return &(data->queue[index%1024]);
}

void
gomp_print_string (const char *msg, const char *value)
{
  struct printf_data *output = reserve_print_slot ();
  output->type = 2; /* String.  */

  strncpy (output->msg, msg, 127);
  output->msg[127] = '\0';
  strncpy (output->text, value, 127);
  output->text[127] = '\0';

  __atomic_store_n (&output->written, 1, __ATOMIC_RELEASE);
}

void
gomp_print_integer (const char *msg, int64_t value)
{
  struct printf_data *output = reserve_print_slot ();
  output->type = 0; /* Integer.  */

  strncpy (output->msg, msg, 127);
  output->msg[127] = '\0';
  output->ivalue = value;

  __atomic_store_n (&output->written, 1, __ATOMIC_RELEASE);
}

void
gomp_print_double (const char *msg, double value)
{
  struct printf_data *output = reserve_print_slot ();
  output->type = 1; /* Double.  */

  strncpy (output->msg, msg, 127);
  output->msg[127] = '\0';
  output->dvalue = value;

  __atomic_store_n (&output->written, 1, __ATOMIC_RELEASE);
}
