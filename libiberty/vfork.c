/* Emulate vfork using just plain fork, for systems without a real vfork.
   This function is in the public domain. */

int
vfork ()
{
  return (fork ());
}
