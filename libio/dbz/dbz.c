	fprintf(f, "dbz %d %ld %d %c %ld %ld %d %d", dbzversion,
		(long)cp->tsize,
		cp->fieldsep, cp->casemap, (long)cp->tagenb,
		(long)cp->tagmask, cp->tagshift,
		cp->valuesize);

		fprintf(f, "%ld%c",
			(long)cp->used[i], (i < NUSEDS-1) ? ' ' : '\n');

