// DELWIN BIJU 241ADB008
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm

/*
  Simpler Pythonic Arithmetic Parser (single file)
  ------------------------------------------------
  Features: + - * / ** (right-assoc), parentheses, unary +/-,
            floats via strtod, # full-line comments,
            ERROR:<pos> (1-based across whole file), CLI dir batch.

  Why simple? One small lexer + 5 tiny parser functions (expr/term/power/unary/primary),
  minimal filesystem helpers, and consistent error reporting.
*/

#define _POSIX_C_SOURCE 200809L

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static const char *STUDENT_FIRST = "DELWIN";
static const char *STUDENT_LAST  = "BIJU";
static const char *STUDENT_ID    = "241ADB008";

/* ----------------------------- source ----------------------------- */

typedef struct {
    const char *buf;
    size_t len;
    size_t i;         // current index [0..len]
    size_t pos1;      // 1-based char position (i+1)
    bool at_line_start;
} Src;

static void src_init(Src *s, const char *b, size_t n) {
    s->buf = b; s->len = n; s->i = 0; s->pos1 = 1; s->at_line_start = true;
}

static int peek(Src *s)        { return (s->i < s->len) ? (unsigned char)s->buf[s->i] : EOF; }
static int peekn(Src *s,size_t n){ size_t k=s->i+n; return (k<s->len)?(unsigned char)s->buf[k]:EOF; }
static int getc_src(Src *s) {
    if (s->i >= s->len) return EOF;
    int c = (unsigned char)s->buf[s->i++];
    s->pos1++;
    if (c == '\n') s->at_line_start = true;
    else if (!isspace(c)) s->at_line_start = false;
    return c;
}
static void advance(Src *s, size_t n){ while(n--) (void)getc_src(s); }

/* Skip spaces and lines where first non-space char is '#'.
   Important: we only *ignore* semantically; positions still advance (we consume chars). */
static void skip_ws_and_comments(Src *s) {
    for (;;) {
        while (isspace(peek(s))) getc_src(s);
        if (s->at_line_start) {
            // look ahead past leading spaces
            size_t k = 0; int c;
            while ((c = peekn(s,k)) != EOF && (c==' ' || c=='\t')) k++;
            if (c == '#') {
                advance(s, k + 1);            // skip spaces + '#'
                while ((c = peek(s)) != EOF && c != '\n') getc_src(s);
                if (peek(s) == '\n') getc_src(s); // eat newline
                continue; // more lines may be comments
            }
        }
        break;
    }
}

/* ----------------------------- lexer ----------------------------- */

typedef enum {
    T_END=0, T_NUM,
    T_PLUS, T_MINUS, T_STAR, T_SLASH, T_POW, T_LPAREN, T_RPAREN, T_INVALID
} Tok;

typedef struct {
    Tok kind;
    size_t start; // 1-based starting position of token
    double num;   // if T_NUM
} Token;

typedef struct {
    Src *src;
    Token cur;
    bool has;
} Lex;

static Token tok(Tok k, size_t p){ Token t; t.kind=k; t.start=p; t.num=0.0; return t; }

static Token lex_number(Lex *L){
    Src *s=L->src; size_t st=s->pos1;
    char *endptr=NULL;
    double v = strtod(s->buf + s->i, &endptr);
    if (endptr == s->buf + s->i) return tok(T_INVALID, st);
    size_t used = (size_t)(endptr - (s->buf + s->i));
    advance(s, used);
    Token t = tok(T_NUM, st); t.num = v; return t;
}

static Token next(Lex *L){
    Src *s=L->src; skip_ws_and_comments(s);
    size_t st = s->pos1;
    int c = peek(s);
    if (c == EOF) return tok(T_END, st);
    if (isdigit(c) || c == '.') {
        if (c=='.' && !isdigit(peekn(s,1))) { getc_src(s); return tok(T_INVALID, st); }
        return lex_number(L);
    }
    if (c=='+'){ getc_src(s); return tok(T_PLUS,st); }
    if (c=='-'){ getc_src(s); return tok(T_MINUS,st); }
    if (c=='('){ getc_src(s); return tok(T_LPAREN,st); }
    if (c==')'){ getc_src(s); return tok(T_RPAREN,st); }
    if (c=='*'){
        if (peekn(s,1)=='*'){ advance(s,2); return tok(T_POW,st); }
        getc_src(s); return tok(T_STAR,st);
    }
    if (c=='/'){ getc_src(s); return tok(T_SLASH,st); }
    // anything else
    getc_src(s);
    return tok(T_INVALID, st);
}

static void lex_init(Lex *L, Src *s){ L->src=s; L->has=false; }
static Token peek_tok(Lex *L){ if(!L->has){ L->cur=next(L); L->has=true; } return L->cur; }
static Token pop_tok (Lex *L){ Token t=peek_tok(L); L->has=false; return t; }

/* ---------------------- parser / evaluator ---------------------- */

typedef struct { bool err; size_t pos; } Err;
typedef struct { Lex *L; Err *E; } P;

static void fail(Err *E, size_t p){ if(!E->err){ E->err=true; E->pos=p? p:1; } }

static bool is_intlike(double x){ double r = nearbyint(x); return fabs(x - r) < 1e-12; }

static double parse_expr(P *p); // fwd

// primary := NUMBER | '(' expr ')'
static double primary(P *p){
    Token t = peek_tok(p->L);
    if (t.kind == T_NUM){ pop_tok(p->L); return t.num; }
    if (t.kind == T_LPAREN){
        pop_tok(p->L);
        double v = parse_expr(p);
        if (p->E->err) return 0.0;
        Token r = peek_tok(p->L);
        if (r.kind != T_RPAREN){ fail(p->E, r.start); return 0.0; }
        pop_tok(p->L);
        return v;
    }
    fail(p->E, t.start);
    return 0.0;
}

// unary := ('+'|'-') unary | primary
static double unary(P *p){
    Token t = peek_tok(p->L);
    if (t.kind == T_PLUS){ pop_tok(p->L); return unary(p); }
    if (t.kind == T_MINUS){
        size_t mpos = t.start; pop_tok(p->L);
        Token a = peek_tok(p->L);
        if (a.kind==T_END || a.kind==T_INVALID || a.kind==T_RPAREN){ fail(p->E, mpos); return 0.0; }
        return -unary(p);
    }
    return primary(p);
}

// power := unary [ '**' power ]   (right-assoc)
static double power(P *p){
    double base = unary(p);
    if (p->E->err) return 0.0;
    Token t = peek_tok(p->L);
    if (t.kind == T_POW){
        size_t powpos = t.start; pop_tok(p->L);
        Token exp_t = peek_tok(p->L);
        if (exp_t.kind==T_END || exp_t.kind==T_INVALID || exp_t.kind==T_RPAREN){ fail(p->E, exp_t.start); return 0.0; }
        double e = power(p); // right assoc
        if (p->E->err) return 0.0;
        // reject negative base with non-integer exponent
        if (base < 0.0 && !is_intlike(e)){ fail(p->E, exp_t.start); return 0.0; }
        errno=0; double out = pow(base, e);
        if (errno || isnan(out) || isinf(out)){ fail(p->E, powpos); return 0.0; }
        return out;
    }
    return base;
}

// term := power { ('*' | '/') power }
static double term(P *p){
    double v = power(p);
    if (p->E->err) return 0.0;
    for(;;){
        Token t = peek_tok(p->L);
        if (t.kind == T_STAR){
            pop_tok(p->L);
            Token r = peek_tok(p->L);
            if (r.kind==T_END || r.kind==T_INVALID || r.kind==T_RPAREN){ fail(p->E, r.start); return 0.0; }
            double rhs = power(p); if (p->E->err) return 0.0; v *= rhs;
        } else if (t.kind == T_SLASH){
            pop_tok(p->L);
            Token r = peek_tok(p->L);
            if (r.kind==T_END || r.kind==T_INVALID || r.kind==T_RPAREN){ fail(p->E, r.start); return 0.0; }
            double rhs = power(p); if (p->E->err) return 0.0;
            if (rhs == 0.0){ fail(p->E, r.start); return 0.0; } // div by zero → at divisor
            v /= rhs;
        } else break;
    }
    return v;
}

// expr := term { ('+' | '-') term }
static double parse_expr(P *p){
    double v = term(p);
    if (p->E->err) return 0.0;
    for(;;){
        Token t = peek_tok(p->L);
        if (t.kind == T_PLUS){
            pop_tok(p->L);
            Token r = peek_tok(p->L);
            if (r.kind==T_END || r.kind==T_INVALID || r.kind==T_RPAREN){ fail(p->E, r.start); return 0.0; }
            double rhs = term(p); if (p->E->err) return 0.0; v += rhs;
        } else if (t.kind == T_MINUS){
            pop_tok(p->L);
            Token r = peek_tok(p->L);
            if (r.kind==T_END || r.kind==T_INVALID || r.kind==T_RPAREN){ fail(p->E, r.start); return 0.0; }
            double rhs = term(p); if (p->E->err) return 0.0; v -= rhs;
        } else break;
    }
    return v;
}

/* Parse one expression from a whole file buffer.
   After expr, only spaces/comment-lines allowed.
   Returns: ok? and either (int-like) or (double). */
static bool parse_and_eval(const char *buf, size_t len,
                           long long *out_ll, double *out_d, bool *out_is_int, size_t *out_err) {
    Src s; src_init(&s, buf, len);
    Lex L; lex_init(&L, &s);
    Err E = {0};
    P p = { .L=&L, .E=&E };

    double val = parse_expr(&p);
    if (!E.err) {
        skip_ws_and_comments(&s);
        Token trailing = next(&L);
        if (trailing.kind != T_END) { E.err=true; E.pos=trailing.start; }
    }
    if (E.err){ *out_err = E.pos; return false; }

    if (is_intlike(val)){ *out_ll = (long long)llround(val); *out_is_int = true; }
    else { *out_d = val; *out_is_int = false; }
    return true;
}

/* ------------------------- tiny filesystem ------------------------- */

static const char* base_name(const char *p){
    const char *b = strrchr(p, '/');
#ifdef _WIN32
    const char *b2 = strrchr(p, '\\'); if (!b || (b2 && b2 > b)) b = b2;
#endif
    return b ? b+1 : p;
}

static void strip_ext(char *s){ char *d = strrchr(s, '.'); if (d) *d = '\0'; }

static void ensure_dir(const char *path){
    struct stat st;
    if (stat(path, &st) == 0){ if (!S_ISDIR(st.st_mode)){ fprintf(stderr,"'%s' not a dir\n", path); exit(2);} return; }
    if (mkdir(path, 0775) != 0 && errno != EEXIST){ fprintf(stderr,"mkdir '%s': %s\n", path, strerror(errno)); exit(2); }
}

static bool read_file(const char *path, char **data, size_t *len){
    *data=NULL; *len=0;
    FILE *f=fopen(path,"rb"); if(!f) return false;
    if (fseek(f,0,SEEK_END)!=0){ fclose(f); return false; }
    long sz=ftell(f); if(sz<0){ fclose(f); return false; }
    if (fseek(f,0,SEEK_SET)!=0){ fclose(f); return false; }
    char *buf = (char*)malloc((size_t)sz+1); if(!buf){ fclose(f); return false; }
    size_t n=fread(buf,1,(size_t)sz,f); fclose(f);
    if (n != (size_t)sz){ free(buf); return false; }
    buf[n]='\0'; *data=buf; *len=n; return true;
}

static bool write_file(const char *path, const char *s){
    FILE *f=fopen(path,"wb"); if(!f) return false;
    size_t n=fwrite(s,1,strlen(s),f); fclose(f); return n==strlen(s);
}

static void build_default_outdir(char *dst, size_t n, const char *base){
    const char *user = getenv("USER"); if (!user) user = STUDENT_FIRST;
    snprintf(dst,n,"%s_%s_%s", base, user, STUDENT_ID);
}

static void build_outpath(char *dst, size_t n, const char *outdir, const char *inbase){
    snprintf(dst,n,"%s/%s_%s_%s_%s.txt", outdir, inbase, STUDENT_FIRST, STUDENT_LAST, STUDENT_ID);
}

/* --------------------------- main logic --------------------------- */

static int process_one(const char *inpath, const char *outdir){
    char *buf=NULL; size_t len=0;
    if (!read_file(inpath,&buf,&len)){ fprintf(stderr,"cannot read %s\n", inpath); return 1; }

    long long iv=0; double dv=0.0; bool is_int=false; size_t err=0;
    bool ok = parse_and_eval(buf,len,&iv,&dv,&is_int,&err);

    char base[512]; strncpy(base, base_name(inpath), sizeof(base)); base[sizeof(base)-1]='\0'; strip_ext(base);

    char outpath[1024]; build_outpath(outpath,sizeof(outpath), outdir, base);

    char outbuf[160];
    if (ok){
        if (is_int) snprintf(outbuf,sizeof(outbuf), "%lld\n", iv);
        else if (is_intlike(dv)) snprintf(outbuf,sizeof(outbuf), "%lld\n", (long long)llround(dv));
        else snprintf(outbuf,sizeof(outbuf), "%.15g\n", dv);
    } else {
        snprintf(outbuf,sizeof(outbuf), "ERROR:%zu\n", err);
    }

    ensure_dir(outdir);
    bool wrote = write_file(outpath, outbuf);
    free(buf);
    if (!wrote){ fprintf(stderr,"cannot write %s\n", outpath); return 1; }
    return 0;
}

static void usage(const char *p){
    fprintf(stderr,
      "Usage:\n"
      "  %s [-o OUTDIR|--output-dir OUTDIR] input.txt\n"
      "  %s -d DIR [-o OUTDIR|--output-dir OUTDIR]\n", p, p);
}

int main(int argc, char **argv){
    const char *dir=NULL, *outdir_arg=NULL, *single=NULL;
    for (int i=1;i<argc;i++){
        if (!strcmp(argv[i],"-d")||!strcmp(argv[i],"--dir")){ if(++i>=argc){usage(argv[0]);return 2;} dir=argv[i]; }
        else if(!strcmp(argv[i],"-o")||!strcmp(argv[i],"--output-dir")){ if(++i>=argc){usage(argv[0]);return 2;} outdir_arg=argv[i]; }
        else if (argv[i][0]=='-'){ usage(argv[0]); return 2; }
        else single=argv[i];
    }

    if (dir){
        // build default outdir from dir base if not provided
        char base[512]; strncpy(base, base_name(dir), sizeof(base)); base[sizeof(base)-1]='\0';
        char outdir[1024];
        if (outdir_arg){ strncpy(outdir, outdir_arg, sizeof(outdir)); outdir[sizeof(outdir)-1]='\0'; }
        else build_default_outdir(outdir, sizeof(outdir), base);
        ensure_dir(outdir);

        DIR *d = opendir(dir);
        if (!d){ fprintf(stderr,"cannot open dir %s: %s\n", dir, strerror(errno)); return 2; }
        struct dirent *e; int rc=0;
        while((e=readdir(d))){
            if (e->d_name[0]=='.') continue;
            size_t n=strlen(e->d_name); if (n<4 || strcmp(e->d_name+n-4,".txt")) continue;
            char path[1024]; snprintf(path,sizeof(path), "%s/%s", dir, e->d_name);
            struct stat st; if (stat(path,&st)!=0 || !S_ISREG(st.st_mode)) continue;
            rc |= process_one(path, outdir);
        }
        closedir(d);
        return rc;
    }

    if (!single){ usage(argv[0]); return 2; }

    char base[512]; strncpy(base, base_name(single), sizeof(base)); base[sizeof(base)-1]='\0'; strip_ext(base);
    char outdir[1024];
    if (outdir_arg){ strncpy(outdir, outdir_arg, sizeof(outdir)); outdir[sizeof(outdir)-1]='\0'; }
    else build_default_outdir(outdir, sizeof(outdir), base);
    ensure_dir(outdir);

    return process_one(single, outdir);
}
