int
waitpid (pid, stat_loc, options)
	int pid, *stat_loc, options;
{
  for (;;)
    {
      int wpid = wait(stat_loc);
      if (wpid == pid || wpid == -1)
	return wpid;
    }
}
