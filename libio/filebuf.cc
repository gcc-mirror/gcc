#if _G_HAVE_IO_FILE_OPEN
  return (filebuf*)_IO_file_open (this, filename, posix_mode, prot,
				  read_write, 0);
#else
#endif
