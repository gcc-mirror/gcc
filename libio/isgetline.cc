      _gcount = _IO_getline_info(sb, buf, len - 1, delim, -1, &ch);
      if (ch != EOF)
	ch = sb->sbumpc();
      int ch;
      long count = _IO_getline_info(sbuf, buf, len - 1, delim, -1, &ch);
      if (_gcount == 0 && ch == EOF)
    _IO_size_t count = _IO_getline_info(sb, buf, CHUNK_SIZE, terminator,
				       -1, &ch);
    if (ch != EOF)
      ch = sb->sbumpc();
