#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#define INDEX_STATII "MADRC"
#define WORKING_STATII "MD"
#define STATUS_ORDER "UMADRCmd?"

void add (char *s, char c)
{
    char *p;
    for (p = s; *p != '\0' && *p != c; ++p);
    if (*p == '\0') { *p++ = c; *p = '\0'; }
}

int str2eq (char *s1, char *s2)
{
    return strncmp(s1, s2, 2) == 0;
}

static char *order = STATUS_ORDER;
ssize_t chrkey (const char *c)
{
    char *pos = strchr(order, *c);
    pos? order - pos : -1;
}

int chrcmp (const void *a, const void *b)
{
    return chrkey(a) - chrkey(b);
}

int main (void)
{
    char inbuf[8192];
    char outbuf[12];
    char *loc;
    int skipping = 0;
    size_t span;
    outbuf[0] = '\0';
    signal(SIGPIPE, SIG_IGN);
    while (fgets(inbuf, sizeof inbuf, stdin)) {
        if (skipping && strchr(inbuf, '\n')) {
            skipping = 0;
            continue;
        }
        skipping = (strchr(inbuf, '\n') == NULL);
        if (strlen(inbuf) < 4 || inbuf[2] != ' ') {
            continue;
        }
        if (str2eq(inbuf, "??")) { add(outbuf, '?'); }
        else if (inbuf[0] == 'U'
                 || inbuf[1] == 'U'
                 || str2eq(inbuf, "DD")
                 || str2eq(inbuf, "AA"))
            { add(outbuf, 'U'); }
        else {
            if ((loc = strchr(INDEX_STATII, inbuf[0])))
                { add(outbuf, *loc); }
            if ((loc = strchr(WORKING_STATII, inbuf[1])))
                { add(outbuf, tolower(*loc)); }
        }

    }
    if (ferror(stdin)) { perror("fgets"); return 1; }
    { char *tmp = getenv("PARSE_GIT_STATUS_ORDER"); if (tmp) { order = tmp; } }
    qsort(outbuf, strlen(outbuf), sizeof *outbuf, chrcmp);
    if (printf("%s\n", outbuf) < 0) { perror("printf"); return 1; }
    return 0;
}
