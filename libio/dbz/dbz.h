/* for dbm and dbz */
typedef struct {
	char *dptr;
	int dsize;
} datum;

/* standard dbm functions */
extern int dbminit();
extern datum fetch();
extern int store();
extern int delete();		/* not in dbz */
extern datum firstkey();	/* not in dbz */
extern datum nextkey();		/* not in dbz */
extern int dbmclose();		/* in dbz, but not in old dbm */

/* new stuff for dbz */
extern int dbzfresh();
extern int dbzagain();
extern datum dbzfetch();
extern int dbzstore();
extern int dbzsync();
extern long dbzsize();
extern int dbzincore();
extern int dbzcancel();
extern int dbzdebug();

/*
 * In principle we could handle unlimited-length keys by operating a chunk
 * at a time, but it's not worth it in practice.  Setting a nice large
 * bound on them simplifies the code and doesn't hurt anything.
 */
#define DBZMAXKEY	255
