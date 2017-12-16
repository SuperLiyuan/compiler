#include <stdio.h>

#define NRW        14     // number of reserved words
#define TXMAX      500    // length of identifier table
#define MAXNUMLEN  14     // maximum number of digits in numbers
#define NSYM       18     // maximum number of symbols in array ssym and csym
#define MAXIDLEN   10     // length of identifiers

#define MAXADDRESS 32767  // maximum address
#define MAXLEVEL   32     // maximum depth of nesting block
#define CXMAX      500    // size of code array

#define MAXSYM     30     // maximum number of symbols

#define STACKSIZE  1000   // maximum storage
#define MAXDIM     100    // maximum array dimensions

enum symtype
{
	SYM_NULL,
	SYM_IDENTIFIER,
	SYM_NUMBER,
	SYM_PLUS,
	SYM_MINUS,
	SYM_TIMES,
	SYM_SLASH,
	SYM_EQU,
	SYM_NEQ,
	SYM_LES,
	SYM_LEQ,
	SYM_GTR,
	SYM_GEQ,
	SYM_LPAREN,
	SYM_RPAREN,
	SYM_COMMA,
	SYM_SEMICOLON,
	SYM_PERIOD,
	SYM_BECOMES,
    SYM_BEGIN,
	SYM_END,
	SYM_IF,
	SYM_WHILE,
	SYM_DO,
	SYM_CALL,
	SYM_CONST,
	SYM_VAR,
	SYM_PROCEDURE,
	SYM_OR,
	SYM_AND,
	SYM_NOT,
	SYM_BITOR,              //2017.10.11
	SYM_BITAND,
	SYM_BITXOR,
	SYM_MOD,
	SYM_ELSE,
	SYM_BITNOT,
	SYM_EXIT,
	SYM_FOR,
	SYM_RET,
	SYM_LBRKET,    //'['
	SYM_RBRKET,    //']'
	SYM_PRINT
};

enum idtype
{
	ID_CONSTANT, ID_VARIABLE, ID_PROCEDURE
};

enum opcode
{
	LIT, OPR, LOD, STO, CAL, INT, JMP, JPC, EXT, POP, DIP, LDA, STA, PRT
};

enum oprcode
{
	OPR_RET, OPR_NEG, OPR_ADD, OPR_MIN,
	OPR_MUL, OPR_DIV, OPR_ODD, OPR_EQU,
	OPR_NEQ, OPR_LES, OPR_LEQ, OPR_GTR,
	OPR_AND, OPR_OR, OPR_NOT, OPR_GEQ,
	OPR_MOD, OPR_BitOR, OPR_BitAND, OPR_BitXOR,              //2017.10.11
    OPR_BitNOT
};


typedef struct
{
	int f; // function code
	int l; // level
	int a; // displacement address
} instruction;

//////////////////////////////////////////////////////////////////////
const char* err_msg[] =
{
/*  0 */    "",
/*  1 */    "Found ':=' when expecting '='.",
/*  2 */    "There must be a number or const to follow '='.",
/*  3 */    "There must be an '=' or dimension declaration to follow the identifier.",
/*  4 */    "There must be an identifier to follow 'const', 'var', or 'procedure'.",
/*  5 */    "Missing ',' or ';'.",
/*  6 */    "Incorrect procedure name.",
/*  7 */    "Statement expected.",
/*  8 */    "Follow the statement is an incorrect symbol.",
/*  9 */    "'.' expected.",
/* 10 */    "';' expected.",
/* 11 */    "Undeclared identifier.",
/* 12 */    "Illegal assignment.",
/* 13 */    "':=' expected.",
/* 14 */    "There must be an identifier to follow the 'call'.",
/* 15 */    "A constant or variable can not be called.",
/* 16 */    "'(' expected.",             //2017.10.14
/* 17 */    "';' or '}' expected.",
/* 18 */    "Missing ']'.",
/* 19 */    "Incorrect symbol.",
/* 20 */    "Not an array.",
/* 21 */    "Procedure identifier can not be in an expression.",
/* 22 */    "Missing ')'.",
/* 23 */    "The symbol can not be followed by a factor.",
/* 24 */    "The symbol can not be as the beginning of an expression.",
/* 25 */    "The number is too great.",
/* 26 */    "Redundant ';'.",
/* 27 */    "Unmatched 'else'.",
/* 28 */    "Incomplete 'for' statement.",
/* 29 */    "There must be an number or const in declaration.",
/* 30 */    "Missing '}'.",
/* 31 */    "Illegal dimension declaration.",
/* 32 */    "There are too many levels.",
/* 33 */    "Too many initializers.",
/* 34 */    "The number of actual parameters and virtual parameters aren't matched or Missing ')'",
/* 35 */    "There must be an number or const in dimensions of an const array in using.",
/* 36 */    "Out of array boundary.",
/* 37 */    "Too many dimensions.",
/* 38 */    "'{' expected.",
/* 39 */    "Initializers expected."
};

//////////////////////////////////////////////////////////////////////
char ch;         // last character read
int  sym;        // last symbol read
char id[MAXIDLEN + 1]; // last identifier read
int  num;        // last number read
int  cc;         // character count
int  ll;         // line length
int  kk;
int  err;
int  cx;         // index of current instruction to be generated.
int  vx;         // for const value initialization
int  level = 0;
int  tx = 0;
int  Flag = 0;
int  *value = NULL;    //array of value of const

char line[80];

instruction code[CXMAX];

const char* word[NRW + 1] =
{
	"", /* place holder */
	"begin", "call", "const", "end", "if",
	"procedure", "var", "while", "else", "do",
	"exit", "for", "return", "print"
};

int wsym[NRW + 1] =
{
	SYM_NULL, SYM_BEGIN, SYM_CALL, SYM_CONST, SYM_END,
	SYM_IF, SYM_PROCEDURE, SYM_VAR, SYM_WHILE, SYM_ELSE, SYM_DO,
	SYM_EXIT, SYM_FOR, SYM_RET, SYM_PRINT
};

int ssym[NSYM + 1] =
{
	SYM_NULL, SYM_PLUS, SYM_MINUS, SYM_TIMES, SYM_SLASH,
	SYM_LPAREN, SYM_RPAREN, SYM_EQU, SYM_COMMA, SYM_PERIOD, SYM_SEMICOLON,
	SYM_BITXOR, SYM_MOD, SYM_BITNOT, SYM_LBRKET, SYM_RBRKET, SYM_BEGIN, SYM_END
};

char csym[NSYM + 1] =
{
	' ', '+', '-', '*', '/', '(', ')', '=', ',', '.', ';','^','%','~','[',']','{','}'
};

#define MAXINS   14
const char* mnemonic[MAXINS] =
{
	"LIT", "OPR", "LOD", "STO", "CAL", "INT", "JMP", "JPC", "EXT", "POP", "DIP", "LDA", "STA", "PRT"
};

int *td;   //temporary dimensions

typedef struct
{
	char name[MAXIDLEN + 1];
	int  kind;
	int  *value;
	short empty;   //no use
	int  *dim;   //dim of const array
} comtab;

comtab table[TXMAX];

typedef struct
{
	char  name[MAXIDLEN + 1];
	int   kind;
	short level;
	short address;
	short prodn;
	int   *dim;    //dim of var array
} mask;

typedef struct cxnode{
	int cx;
	struct cxnode *next;
}*cxlist;

int factflag = 0;

FILE* infile;

// EOF PL0.h
