// pl0 compiler source code

#pragma warning(disable:4996)


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "PL0.h"
#include "set.c"

void top_expr(symset);
int dx;  // data allocation index
		 //////////////////////////////////////////////////////////////////////
		 // print error message.
void error(int n)
{
	int i;

	printf("      ");
	for (i = 1; i <= cc - 1; i++)
		printf(" ");
	printf("^\n");
	printf("Error %3d: %s\n", n, err_msg[n]);
	err++;
} // error

  //////////////////////////////////////////////////////////////////////
void getch(void)
{
	if (cc == ll)
	{
		ll = cc = 0;
		printf("%5d  ", cx);
		if (feof(infile))
		{
			printf("\nPROGRAM INCOMPLETE\n");
			exit(1);
		}
		while ((!feof(infile)) // added & modified by alex 01-02-09
			&& ((ch = getc(infile)) != '\n'))
		{
			printf("%c", ch);
			line[++ll] = ch;
		} // while
		printf("\n");
		line[++ll] = ' ';
	}
	ch = line[++cc];
} // getch

  //////////////////////////////////////////////////////////////////////
  // gets a symbol from input stream.
void getsym(void)
{
	int i, k;
	char a[MAXIDLEN + 1];

	while (ch == ' ' || ch == '\t')
		getch();

	if (isalpha(ch))
	{ // symbol is a reserved word or an identifier.
		k = 0;
		do
		{
			if (k < MAXIDLEN)
				a[k++] = ch;
			getch();
		} while (isalpha(ch) || isdigit(ch));
		a[k] = 0;
		strcpy(id, a);
		word[0] = id;
		i = NRW;
		while (strcmp(id, word[i--]));
		if (++i)
			sym = wsym[i]; // symbol is a reserved word
		else
			sym = SYM_IDENTIFIER;   // symbol is an identifier
	}
	else if (isdigit(ch))
	{ // symbol is a number.
		k = num = 0;
		sym = SYM_NUMBER;
		do
		{
			num = num * 10 + ch - '0';
			k++;
			getch();
		} while (isdigit(ch));
		if (k > MAXNUMLEN)
			error(25);     // The number is too great.
	}
	else if (ch == ':')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_BECOMES; // :=
			getch();
		}
		else
		{
			sym = SYM_NULL;       // illegal?
		}
	}
	else if (ch == '>')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_GEQ;     // >=
			getch();
		}
		else
		{
			sym = SYM_GTR;     // >
		}
	}
	else if (ch == '<')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_LEQ;     // <=
			getch();
		}
		else
		{
			sym = SYM_LES;     // <
		}
	}
	else if (ch == '!')                                  //2017.10.13
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_NEQ;     // !=
			getch();
		}
		else
		{
			sym = SYM_NOT;     // !
		}
	}
	else if (ch == '|') //2017-09-24
	{
		getch();
		if (ch == '|') { sym = SYM_OR; getch(); }
		else
		{
			sym = SYM_BITOR;                            //2017.10.11
		}
	}
	else if (ch == '&')//2017-09-24
	{
		getch();
		if (ch == '&') { sym = SYM_AND; getch(); }
		else
		{
			sym = SYM_BITAND;                           //2017.10.11
		}
	}
	else if (ch == '/')
	{
		getch();
		if (ch == '/') { cc = ll; getch(); getsym(); } // line comment
		else if (ch == '*') { // block-comment
			getch();
			while (1) {
				if (ch == '*') {
					getch();
					if (ch == '/') break;
				}
				getch();
			}
			getch();
			getsym();
		}
		else sym = SYM_SLASH;
	}
	else
	{ // other tokens
		i = NSYM;
		csym[0] = ch;
		while (csym[i--] != ch);
		if (++i)
		{
			sym = ssym[i];
			getch();
		}
		else
		{
			printf("Fatal Error: Unknown character:%d.\n", ch);
			exit(1);
		}
	}
} // getsym

  //////////////////////////////////////////////////////////////////////
  // generates (assembles) an instruction.
void gen(int x, int y, int z)
{
	if (cx > CXMAX)
	{
		printf("Fatal Error: Program too long.\n");
		exit(1);
	}
	code[cx].f = x;
	code[cx].l = y;
	code[cx++].a = z;
} // gen

  //////////////////////////////////////////////////////////////////////
  // tests if error occurs and skips all symbols that do not belongs to s1 or s2.
void test(symset s1, symset s2, int n)
{
	symset s;

	if (!inset(sym, s1))
	{
		error(n);
		s = uniteset(s1, s2);
		while (!inset(sym, s))
			getsym();
		destroyset(s);
	}
} // test

  //////////////////////////////////////////////////////////////////////
int arraylength()
{//calculate and return length of an array when declare
	int i = 0, sum = 1, k = 0;
	symset set, set1;

	while (sym == SYM_LBRKET) {  //'['
		getsym();
		if (k >= MAXDIM)
		{
			error(37);    //Too many dimensions
			set1 = createset(SYM_PLUS, SYM_MINUS, SYM_MOD, SYM_AND, SYM_BECOMES, SYM_EQU, SYM_NULL);
			test(set1, facbegsys, 0);
			destroyset(set1);
			break;
		}

		if (sym == SYM_IDENTIFIER || sym == SYM_NUMBER)
		{
			if (sym == SYM_IDENTIFIER) {
				if ((i = position(id)) == 0)
				{
					error(11); // Undeclared identifier.
				}
				else if (table[i].kind == ID_CONSTANT)
				{
					num = constvalue(i);
				}
				else
				{
					error(29); //There must be an number or const in dimension declaration
					getsym();
					if (sym == SYM_RBRKET) //']'
					{
						getsym();
					}
					else
					{
						error(22); //Missing ')' or ']'
					}
					continue;
				}
			}
			td[k++] = num;

			getsym();
			if (sym == SYM_RBRKET) //']'
			{
				getsym();
			}
			else
			{
				error(22); //Missing ')' or ']'
			}
		}
		else if (sym == SYM_RBRKET) {
			if (k == 0) {
				td[k++] = -1;
				getsym();
			}
			else {
				error(31);  //Illegal dimension declaration
				getsym();
			}
		}
		else
		{
			error(29); //There must be an number or const in declaration

			set1 = createset(SYM_LBRKET, SYM_NULL);
			set = createset(SYM_IDENTIFIER, SYM_BEGIN, SYM_EQU, SYM_NULL);
			test(set1, set, 0);
			destroyset(set1);
			destroyset(set);
		}
	}
	while (k--)
	{
		i = td[k];
		td[k] = sum;
		//printf("td[%d] = %d\n", k, sum);
		sum *= i;
	}
	return sum;   // >0 means all dimensions specified, <0 means "a[] = xxx"
}//arraylength

 //////////////////////////////////////////////////////////////////////
int initialize(int ID_type, int arrayLevel, int n) {  // n = boundary of current dimension
	int initer = 0, i;
	//printf("arrayLevel = %d\n", arrayLevel);
	if (td[arrayLevel]) {
		if (sym == SYM_BEGIN) { //'{'
			getsym();
			while (sym != SYM_END) {
				if (td[arrayLevel + 1])
					i = td[arrayLevel] / td[arrayLevel + 1];
				else
					i = 1;
				initialize(ID_type, arrayLevel + 1, i);
				initer++;
				if (sym == SYM_COMMA) {
					getsym();
				}
			}
			if (n == -1)
				n = initer;
			if (initer > n) {
				error(33);   // too many initializers
			}
			else if (initer < n) {
				// fill by 0
				if (ID_type == ID_CONSTANT) {
					vx += (n - initer)*td[arrayLevel];
				}
				else { // ID_VARIABLE
					initer = (n - initer)*td[arrayLevel];
					gen(LIT, 0, 0);
					while (initer--) {
						gen(STO, 0, dx++);
					}
					gen(POP, 0, 0);
				}
			}
			if (sym == SYM_END) { //'}'
				getsym();
			}
			else {
				error(30);  //missing '}'
			}
		}
		else {
			error(38); //'{' expected
		}
	}
	else {
		if (sym == SYM_NUMBER || sym == SYM_IDENTIFIER) {
			if (sym == SYM_IDENTIFIER)
			{
				if ((i = position(id)) == 0)
				{
					error(11); // Undeclared identifier.
				}
				else {
					getsym();
					if (table[i].kind == ID_CONSTANT)
					{
						num = constvalue(i);
					}
					else
					{
						error(2); // There must be a number or const to follow '='
					}
				}
			}
			else { //number
				getsym();;
			}
			if (ID_type == ID_CONSTANT) {
				value[vx++] = num;
			}
			else { //ID_type == ID_VARIABLE
				gen(LIT, 0, num);
				gen(STO, 0, dx++);
				gen(POP, 0, 0);
			}
		}
		else if (sym == SYM_END) {
			// {} means initial by 0
		}
		else {
			symset set, set1;
			set = createset(SYM_COMMA, SYM_NULL);
			set1 = createset(SYM_END, SYM_NULL);
			test(set, set1, 29);    //There must be an number or const in declaration
			destroyset(set);
			destroyset(set1);
		}
	}
	return initer;
}

//////////////////////////////////////////////////////////////////////
int varoffset(int a, symset fsys)
{//calculate and return the offset when using var array
	int i = 0, k = 0, temp[MAXDIM] = { 0 }, IsArray = 0;
	symset set, set1, set2;

	set = createset(SYM_RBRKET, SYM_NULL);
	set1 = uniteset(set, fsys);
	while (sym == SYM_LBRKET)
	{

		if (table[a].dim == NULL)
		{
			symset set;
			set = createset(SYM_COMMA, SYM_SEMICOLON, SYM_END, SYM_NULL);
			test(set, facbegsys, 20); //Not an array
			destroyset(set);
			return IsArray; //false
		}
		IsArray = 1;
		getsym();
		if (k >= MAXDIM)
		{
			error(37);    //Too many dimensions
			set2 = createset(SYM_PLUS, SYM_MINUS, SYM_MOD, SYM_AND, SYM_BECOMES, SYM_EQU, SYM_NULL);
			test(set2, facbegsys, 0);
			destroyset(set1);
			break;
		}
		top_expr(set1);
		gen(LIT, 0, table[a].dim[k++]);
		gen(OPR, 0, OPR_MUL);

		if (sym == SYM_RBRKET) //']'
		{
			getsym();
		}
		else
		{
			error(22); //Missing ')' or ']'
		}
	}
	if (k) k--; // k = dimension - 1
	while (k--)
	{
		gen(OPR, 0, OPR_ADD);
	}

	//printf("quit offset with sym == %d\n", sym);
	return IsArray;
}//varoffset

 //////////////////////////////////////////////////////////////////////
int constoffset(int a) {
	//calculate and return the offset when using array
	int i = 0, k = 0, temp[MAXDIM] = { 0 };
	while (sym == SYM_LBRKET)
	{
		if (table[a].dim == NULL)
		{
			symset set;
			set = createset(SYM_COMMA, SYM_SEMICOLON, SYM_END, SYM_NULL);
			test(set, facbegsys, 20); //Not an array
			destroyset(set);
			return 1;
		}
		getsym();
		if (k >= MAXDIM)
		{
			error(37);    //Too many dimensions
			symset set1;
			set1 = createset(SYM_PLUS, SYM_MINUS, SYM_MOD, SYM_AND, SYM_BECOMES, SYM_EQU, SYM_NULL);
			test(set1, facbegsys, 0);
			destroyset(set1);
			break;
		}

		if (sym == SYM_NUMBER || sym == SYM_IDENTIFIER) {
			if (sym == SYM_IDENTIFIER)
			{
				if ((i = position(id)) == 0)
				{
					error(11); // Undeclared identifier.
				}
				else if (table[i].kind == ID_CONSTANT)
				{
					num = constvalue(i);
				}
				else
				{
					error(35); //There must be an number or const in dimensions of an array in using
					getsym();
					if (sym == SYM_RBRKET) //']'
					{
						getsym();
					}
					else
					{
						error(22); //Missing ')' or ']'
					}
					continue;
				}
			}

			temp[k++] = num;

			getsym();
			if (sym == SYM_RBRKET) //']'
			{
				getsym();
			}
			else
			{
				error(22); //Missing ')' or ']'
			}
		}
		else
		{

			error(35); //There must be an number or const in dimensions of const array in using.

			symset set;
			set = createset(SYM_LBRKET, SYM_SEMICOLON, SYM_NULL);
			test(set, facbegsys, 0);
			destroyset(set);
		}
	}

	int sum = 0;
	while (k--)
	{
		sum += temp[k] * table[a].dim[k];
		//printf("temp[%d]=%d, dim[%d]=%d\n",k,temp[k],k,table[a].dim[k]);
	}
	//printf("quit offset with sum = %d, sym = %d\n", sum, sym);
	return sum;
}//constoffset

 //////////////////////////////////////////////////////////////////////
int constvalue(int a)
{//find and return the value of const
 //printf("constvalue %s\n",table[a].name);
	return table[a].value[constoffset(a)];
}//constvalue

 //////////////////////////////////////////////////////////////////////
 // enter object(constant, variable or procedre) into table.
void enter(int kind)
{
	mask* mk;

	tx++;
	strcpy(table[tx].name, id);
	*id = '\0';   //reset to empty char array
	table[tx].kind = kind;
	switch (kind)
	{
	case ID_CONSTANT:
		table[tx].value = value;
		value = NULL;
		table[tx].dim = td;
		td = NULL;
		break;
	case ID_VARIABLE:
		mk = (mask*)&table[tx];
		mk->level = level;
		mk->address = dx;
		mk->dim = td;
		td = NULL;
		break;
	case ID_PROCEDURE:
		mk = (mask*)&table[tx];
		mk->level = level;
		break;
	} // switch
} // enter

  //////////////////////////////////////////////////////////////////////
  // locates identifier in symbol table.
int position(char* id)
{
	int i;
	strcpy(table[0].name, id);
	i = tx + 1;
	while (strcmp(table[--i].name, id) != 0);
	return i;
} // position

  //////////////////////////////////////////////////////////////////////
void constdeclaration()
{
	int l = 1, k = 0, i;
	char id0[MAXIDLEN + 1];
	if (sym == SYM_IDENTIFIER)
	{
		strcpy(id0, id);
		getsym();
		if (sym == SYM_LBRKET) //'['
		{
			td = (int*)malloc(MAXDIM * sizeof(int));
			memset(td, 0, MAXDIM * sizeof(int));
			l = arraylength();
			if (l<0) {
				error(31);     //Illegal dimension declaration.
				l *= -20;
			}
			value = (int*)malloc(l * sizeof(int));
			memset(value, 0, l * sizeof(int));
			vx = 0;
			if (sym == SYM_EQU || sym == SYM_BECOMES)
			{
				if (sym == SYM_BECOMES)
					error(1); // Found ':=' when expecting '='.
				getsym();

				printf("dx = %d, l = %d\n", dx, l);
				initialize(ID_CONSTANT, 0, l / td[0]);
				strcpy(id, id0);
				enter(ID_CONSTANT);
			}
			else {
				error(3);  //There must be an '=' to follow the identifier.
				free(value);
				free(td);
			}
		}
		else if (sym == SYM_EQU || sym == SYM_BECOMES)
		{
			if (sym == SYM_BECOMES)
				error(1); // Found ':=' when expecting '='.
			getsym();

			if (sym == SYM_NUMBER || sym == SYM_IDENTIFIER)
			{
				if (sym == SYM_IDENTIFIER)
				{
					if ((i = position(id)) == 0)
					{
						error(11); // Undeclared identifier.
					}
					else {
						getsym();
						if (table[i].kind == ID_CONSTANT)
						{
							num = constvalue(i);
						}
						else
						{
							error(2); // There must be a number or const to follow '='
						}
					}
				}
				else {
					getsym();
				}
				value = (int*)malloc(sizeof(int));
				*value = num;
				strcpy(id, id0);
				enter(ID_CONSTANT);
				//printf("enter id = %s, value = %d\n",id,num);
			}
			else
			{
				error(2); // There must be a number or const to follow '='.
			}
		}
		else
		{
			error(3); // There must be an '=' to follow the identifier.
		}
	}
	else	error(4);// There must be an identifier to follow 'const', 'var', or 'procedure'.

} // constdeclaration

  //////////////////////////////////////////////////////////////////////
int vardeclaration()
{
	int i, l = 1; //length
	char id0[MAXIDLEN + 1];
	if (sym == SYM_IDENTIFIER)
	{
		strcpy(id0, id);
		getsym();
		if (sym == SYM_LBRKET)
		{   //// var array
			td = (int*)malloc(MAXDIM * sizeof(int));
			memset(td, 0, MAXDIM * sizeof(int));
			l = arraylength();
			if (l>0) {
				if (sym == SYM_EQU) {
					getsym();
					printf("dx = %d, l = %d\n", dx, l);
					initialize(ID_VARIABLE, 0, l / td[0]);
					printf("dx' = %d\n", dx);
					dx -= l;
				}
			}
			else { // "a[] = xxx"
				if (sym == SYM_EQU) {
					getsym();
					printf("dx = %d, l = %d\n", dx, l);
					l = initialize(ID_VARIABLE, 0, -1)*td[0];
					printf("dx' = %d\n", dx);
					dx -= l;
				}
				else {
					error(39);  //Initializers expected.
					l *= -20;
				}
			}
			strcpy(id, id0);
			enter(ID_VARIABLE);
			dx += l;
		}
		else {  //single var
			enter(ID_VARIABLE);
			if (sym == SYM_EQU || sym == SYM_BECOMES) {
				if (sym == SYM_BECOMES)
					error(1);  //Found ':=' when expecting '='.
				getsym();
				if (sym == SYM_NUMBER || sym == SYM_IDENTIFIER) {
					if (sym == SYM_IDENTIFIER)
					{
						if ((i = position(id)) == 0)
						{
							error(11); // Undeclared identifier.
						}
						else {
							getsym();
							if (table[i].kind == ID_CONSTANT)
							{
								num = constvalue(i);
							}
							else
							{
								error(2); // There must be a number or const to follow '='
							}
						}
					}
					else { //number
						getsym();
					}
					gen(LIT, 0, num);
					gen(STO, 0, dx);
					gen(POP, 0, 0);
				}
				else {
					error(2); // There must be a number or const to follow '='
				}
			}
			dx++;
		}
	}
	else
	{
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
	}
	return l;
} // vardeclaration

  //////////////////////////////////////////////////////////////////////
void listcode(int from, int to)
{
	int i;

	printf("\n");
	for (i = from; i < to; i++)
	{
		printf("%5d %s\t%d\t%d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
	}
	printf("\n");
} // listcode

  //////////////////////////////////////////////////////////////////////
void proceCall(mask*, symset);
//////////////////////////////////////////////////////////////////////
void factor(symset fsys)
{
	int i;
	symset set;

	if (factflag)  //assigned by top_expr          2017.10.24 , 2017.10.26
	{
		factflag = 0;
	}
	else {
		if (inset(sym, facbegsys))
		{
			if (sym == SYM_IDENTIFIER)
			{
				if ((i = position(id)) == 0)
				{
					error(11); // Undeclared identifier.
				}
				else
				{
					getsym();
					switch (table[i].kind)
					{
						mask* mk;
					case ID_CONSTANT:
						gen(LIT, 0, constvalue(i));
						break;
					case ID_VARIABLE:
						mk = (mask*)&table[i];
						if (varoffset(i, fsys))
							gen(LDA, level - mk->level, mk->address);
						else
							gen(LOD, level - mk->level, mk->address);
						break;
					case ID_PROCEDURE:
						mk = (mask*)&table[i];
						proceCall(mk, fsys);
						break;
					} // switch
				}
			}
			else if (sym == SYM_NUMBER)
			{
				gen(LIT, 0, num);
				getsym();
			}
			else if (sym == SYM_RDM) {
				int tmp_num;
				getsym();
				if (sym == SYM_LPAREN)
					getsym();
				else
					error(16);   //'(' expected
				if (sym == SYM_RPAREN) {
					gen(RDM, 0, 0);
					getsym();
				}
				else if (sym == SYM_NUMBER) {
					tmp_num = num;
					gen(RDM, 0, tmp_num);
					getsym();
					if (sym == SYM_RPAREN) {
						getsym();
					}
					else
						error(22);	//Missing ')'.
				}

			}
			else if (sym == SYM_LPAREN)
			{
				getsym();
				set = uniteset(createset(SYM_RPAREN, SYM_NULL), fsys);
				top_expr(set);
				destroyset(set);
				if (sym == SYM_RPAREN)
				{
					getsym();
				}
				else
				{
					error(22); // Missing ')'.
				}
			}
			else if (sym == SYM_MINUS) // factor -> '-' factor
			{
				getsym();
				factor(fsys);          //2017.10.13
				gen(OPR, 0, OPR_NEG);
			}
			else if (sym == SYM_NOT) // factor -> '!' factor
			{
				getsym();
				factor(fsys);          //2017.10.13
				gen(OPR, 0, OPR_NOT);
			}
			else if (sym == SYM_BITNOT)  // factor -> '~' factor
			{
				getsym();
				factor(fsys);
				gen(OPR, 0, OPR_BitNOT);
			}
			test(fsys, createset(SYM_LPAREN, SYM_NULL), 23);
		} // while
	}//else
} // factor

  //////////////////////////////////////////////////////////////////////
void term(symset fsys)
{
	int mulop;
	symset set;

	set = uniteset(fsys, createset(SYM_TIMES, SYM_SLASH, SYM_MOD, SYM_NULL));  //2017.10.11
	factor(set);
	while (sym == SYM_TIMES || sym == SYM_SLASH || sym == SYM_MOD)
	{
		mulop = sym;
		getsym();
		factor(set);
		if (mulop == SYM_TIMES)
		{
			gen(OPR, 0, OPR_MUL);
		}
		else if (mulop == SYM_SLASH)
		{
			gen(OPR, 0, OPR_DIV);
		}
		else
		{
			gen(OPR, 0, OPR_MOD);
		}
	} // while
	destroyset(set);
} // term

  //////////////////////////////////////////////////////////////////////
void expression(symset fsys)
{
	int addop;
	symset set;

	set = uniteset(fsys, createset(SYM_PLUS, SYM_MINUS, SYM_NULL));

	term(set);
	while (sym == SYM_PLUS || sym == SYM_MINUS)
	{
		addop = sym;
		getsym();
		term(set);
		if (addop == SYM_PLUS)
		{
			gen(OPR, 0, OPR_ADD);
		}
		else
		{
			gen(OPR, 0, OPR_MIN);
		}
	} // while

	destroyset(set);
} // expression


  //////////////////////////////////////////////////////////////////////
void rel_expr(symset fsys) //condition
{
	int relop;
	symset set;

	{
		set = uniteset(relset, fsys);
		expression(set);
		//destroyset(set);
		if (inset(sym, relset))          //2017.10.14
		{
			relop = sym;
			getsym();
			expression(fsys);
			switch (relop)
			{
			case SYM_EQU:
				gen(OPR, 0, OPR_EQU);
				break;
			case SYM_NEQ:
				gen(OPR, 0, OPR_NEQ);
				break;
			case SYM_LES:
				gen(OPR, 0, OPR_LES);
				break;
			case SYM_GEQ:
				gen(OPR, 0, OPR_GEQ);
				break;
			case SYM_GTR:
				gen(OPR, 0, OPR_GTR);
				break;
			case SYM_LEQ:
				gen(OPR, 0, OPR_LEQ);
				break;
			} // switch
		} // if
		destroyset(set);
	} // else
} // rel_expr , condition

  ///////////////////////////////////////////////////////////////////////
void bitand_expr(symset fsys)                     //2017.10.11
{
	symset set;

	set = uniteset(fsys, createset(SYM_BITAND, SYM_NULL));

	rel_expr(set);
	while (sym == SYM_BITAND)
	{
		getsym();
		rel_expr(set);
		gen(OPR, 0, OPR_BitAND);
	}//while

	destroyset(set);
}// bitand_expr

 ///////////////////////////////////////////////////////////////////////
void bitxor_expr(symset fsys)                   //2017.10.11
{
	symset set;

	set = uniteset(fsys, createset(SYM_BITXOR, SYM_NULL));

	bitand_expr(set);
	while (sym == SYM_BITXOR)
	{
		getsym();
		bitand_expr(set);
		gen(OPR, 0, OPR_BitXOR);
	}//while

	destroyset(set);
}// bitand_expr

 //////////////////////////////////////////////////////////////////////
void bitor_expr(symset fsys)                 //2017.10.11
{
	symset set;

	set = uniteset(fsys, createset(SYM_BITOR, SYM_NULL));

	bitxor_expr(set);
	while (sym == SYM_BITOR)
	{
		getsym();
		bitxor_expr(set);
		gen(OPR, 0, OPR_BitOR);
	}//while

	destroyset(set);
}// bitor_expr

 /////////////////////////////////////////////////////////////////////////
void and_expr(symset fsys)
{
	symset set;
	cxlist cx0, p;
	int cx1, flag = 0;

	cx0 = p = (cxlist)malloc(sizeof(struct cxnode));
	p->next = NULL;
	set = uniteset(fsys, createset(SYM_AND, SYM_NULL));

	bitor_expr(set);
	while (sym == SYM_AND)               //2017.10.24 , 2017.10.26
	{
		flag = 1;

		p = p->next = (cxlist)malloc(sizeof(struct cxnode));
		p->next = NULL;
		p->cx = cx;
		gen(JPC, 0, 0);    //short path

		getsym();
		bitor_expr(set);
	} // while
	if (flag)
	{
		gen(OPR, 0, OPR_NOT);
		gen(OPR, 0, OPR_NOT);
		cx1 = cx;
		gen(JMP, 0, 0);      //if did not JPC, avoid restoring
		while (cx0->next)
		{
			p = cx0;
			cx0 = cx0->next;
			free(p);
			code[cx0->cx].a = cx - (cx0->cx);
		}
		free(cx0);
		gen(LIT, 0, 0);        //if JPC, restore 0
		code[cx1].a = cx - cx1;
	}
	destroyset(set);
}
//////////////////////////////////////////////////////////////////////
void or_expr(symset fsys)
{
	symset set;
	cxlist cx0, p;
	int cx1, flag = 0;

	cx0 = p = (cxlist)malloc(sizeof(struct cxnode));
	p->next = NULL;
	set = uniteset(fsys, createset(SYM_OR, SYM_NULL));

	and_expr(set);
	while (sym == SYM_OR)               //2017.10.24 , 2017.10.26
	{
		flag = 1;

		gen(OPR, 0, 14);  // stack[top]=!stack[top];
		p = p->next = (cxlist)malloc(sizeof(struct cxnode));
		p->next = NULL;
		p->cx = cx;
		gen(JPC, 0, 0);  // short path

		getsym();
		and_expr(set);
	} // while
	if (flag)
	{
		gen(OPR, 0, OPR_NOT);
		gen(OPR, 0, OPR_NOT);
		cx1 = cx;
		gen(JMP, 0, 0);   //if did not JPC, avoid restoring
		while (cx0->next)
		{
			p = cx0;
			cx0 = cx0->next;
			free(p);
			code[cx0->cx].a = cx - (cx0->cx);
		}
		free(cx0);
		gen(LIT, 0, 1);        //if JPC, restore 1
		code[cx1].a = cx - cx1;
	}

	destroyset(set);
} // or_expr

  //////////////////////////////////////////////////////////////////////
void top_expr(symset fsys)              //2017.10.26
{
	int i, temp;
	test(facbegsys, fsys, 24); // The symbol can not be as the beginning of an expression.

	if (sym == SYM_IDENTIFIER)
	{
		mask* mk;
		if (!(i = position(id)))
		{
			error(11); // Undeclared identifier.
			getsym();
			test(facbegsys, fsys, 0);
		}
		else if (table[i].kind == ID_VARIABLE)
		{
			mk = (mask*)&table[i];
			getsym();
			int IsArray = varoffset(i, fsys);
			if (IsArray)
				gen(LDA, level - mk->level, mk->address);
			else
				gen(LOD, level - mk->level, mk->address);
			if (sym == SYM_BECOMES)
			{// assignment expression
				getsym();
				cx--;    //delete LOD instruction
				top_expr(fsys);
				if (IsArray)
					gen(STA, level - mk->level, mk->address);
				else
					gen(STO, level - mk->level, mk->address);
			}
			else
			{
				factflag = 1; // means a operant already LODed
				or_expr(fsys);
			}
		}
		else // ID_PROCEDURE or ID_VARIABLE
		{
			or_expr(fsys);
		}
	}
	else
	{
		or_expr(fsys);
	}
}

int prodn;
//////////////////////////////////////////////////////////////////////
void proceCall(mask* mk, symset fsys) { // procedure call, critical
	int j;
	symset set, set1;
	//printf("proceID = %s, prodn = %d\n",mk->name,mk->prodn);
	set1 = createset(SYM_RPAREN, SYM_COMMA, SYM_NULL);
	set = uniteset(set1, fsys);

	if (sym == SYM_LPAREN) {
		j = 0;
		do {
			getsym();

			if (sym == SYM_RPAREN)
			{
				break;
			}
			top_expr(set);
			j++;

		} while (sym == SYM_COMMA && j <= mk->prodn);
		if (sym == SYM_RPAREN && j <= mk->prodn)
		{
			getsym();
		}
		else
		{
			error(22); //missing ')'
		}
	}
	else
		error(16);  //'(' expected


	destroyset(set);
	destroyset(set1);
	if (j != mk->prodn)
	{
		error(34); //"The number of actual parameters and virtual parameters aren't matched or Missing ')'"
		if (j > mk->prodn) {
			set = createset(SYM_RPAREN, SYM_NULL);
			set1 = createset(SYM_RPAREN, SYM_SEMICOLON, SYM_END, SYM_NULL);
			test(set, set1, 0);
			destroyset(set);
			destroyset(set1);
		}
	}

	gen(DIP, level - mk->level, mk->prodn);
	gen(CAL, 0, mk->address);
}//proceCall

 //////////////////////////////////////////////////////////////////////
void statement(symset fsys)
{
	//printf("enter statement\n");
	int i, cx1, cx2, cx3;
	int temp, tmpnum, savedTx;
	symset set1, set;

	if (inset(sym, facbegsys))        //2017.10.26
	{ // variable assignment
		top_expr(fsys);
		if (sym == SYM_SEMICOLON)         //2017.10.22
		{
			getsym();
			gen(POP, 0, 0);    //pop stack
		}
		else
		{
			error(10);     // ';'expected
		}
	}
	else if (sym == SYM_IF)
	{ // if statement
		getsym();
		if (sym != SYM_LPAREN)           //2017.10.14
		{
			error(16);    //'(' expected
		}
		getsym();
		set1 = createset(SYM_RPAREN, SYM_NULL);
		set = uniteset(set1, fsys);
		top_expr(set);//condition(set);
		destroyset(set1);
		destroyset(set);

		if (sym != SYM_RPAREN)           //2017.10.14
		{
			error(22);       //missing ')'
		}
		getsym();

		cx1 = cx;
		gen(JPC, 0, 0);

		set1 = createset(SYM_ELSE, SYM_NULL);       //2017.10.22
		set = uniteset(set1, fsys);
		statement(set);
		destroyset(set1);
		destroyset(set);

		if (sym == SYM_ELSE)                         //2017.10.22
		{
			gen(JMP, 0, 0);
			code[cx1].a = cx - cx1;  //code[cx1] = JPC, 0, cx-cx1
			cx1 = cx - 1;          //cx1 = JMP,0,0
			getsym();
			statement(fsys);
			code[cx1].a = cx - cx1;
		}
		else
		{
			code[cx1].a = cx - cx1;     //cx1 = JPC,0,0
		}
	}
	else if (sym == SYM_ELSE)
	{
		error(27);  // Unmatched 'else'
		getsym();
	}
	else if (sym == SYM_BEGIN)
	{ //chunk, compound_statement
		savedTx = tx;
		getsym();
		//printf("enter begin\n");
		while (sym == SYM_VAR || sym == SYM_CONST) {
			if (sym == SYM_CONST)
			{ // constant declarations
				getsym();

				constdeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					constdeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			} // if
			else
			{  // var declaration
				getsym();
				vardeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					vardeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			}
		}
		set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
		set = uniteset(set1, fsys);
		statement(set);
		while (inset(sym, statbegsys) || sym == SYM_SEMICOLON)     //2017.10.25
		{//sys在不在statement的前缀中,while true, there are other more statements before 'end'
			if (sym == SYM_SEMICOLON)  //this is illegal, just for reporting error
			{
				error(26);   //redundant ';' which will cause "begin;"
				getsym();
			}
			statement(set);
		} // while
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_END)
		{
			tx = savedTx;
			getsym();
			//printf("quit begin\n");
		}
		else
		{
			error(17); // ';' or 'end' expected.
		}
	}
	else if (sym == SYM_FOR)  //2017.10.25
	{ // for statement
		int i;

		getsym();
		if (sym != SYM_LPAREN)
		{
			error(16);   // '(' expected
		}
		else
		{
			getsym();
			set1 = createset(SYM_SEMICOLON, SYM_RPAREN, SYM_NULL);
			set = uniteset(set1, fsys);
			top_expr(set);
			if (sym != SYM_SEMICOLON)
			{
				error(10);   //';' expected
				test(fsys, set1, 28);   //Incomplete 'for' statement.
			}
			else
			{
				getsym();
				gen(POP, 0, 0);
				cx1 = cx;  //come back here after loop
				top_expr(set);
				if (sym != SYM_SEMICOLON)
				{
					error(10);   //';' expected
					test(fsys, set1, 28);    //Incomplete 'for' statement.
				}
				else
				{
					getsym();

					cx2 = cx;  //if false, skip loop
					gen(JPC, 0, 0);

					cx3 = cx;  //beginning of codes that need move
					top_expr(set);
					if (sym == SYM_RPAREN)
					{
						getsym();
					}
					else
					{
						error(22);
					}
					destroyset(set);
					destroyset(set1);
					gen(POP, 0, 0);

					instruction temp[CXMAX];
					for (i = cx3; i<cx; i++)
					{
						temp[i - cx3] = code[i];
					}
					cx = cx3;
					cx3 = i - cx3; //the length of temp
					statement(fsys);
					for (i = 0; i<cx3; i++)
					{
						code[cx++] = temp[i];
					}
					cx1 = cx1 - cx;     //offset
					gen(JMP, 0, cx1); //come back to condition
					code[cx2].a = cx - cx2; //destination of false condition
				}
			}
		}
	} // sym == SYM_FOR
	else if (sym == SYM_WHILE)
	{ // while statement
		cx1 = cx;

		getsym();
		if (sym == SYM_LPAREN)
		{
			getsym();
		}
		else
		{
			error(16);   //'(' expected
		}

		set1 = createset(SYM_RPAREN, SYM_NULL);
		set = uniteset(set1, fsys);
		top_expr(set);//condition(set);
		destroyset(set1);
		destroyset(set);

		cx2 = cx;
		gen(JPC, 0, 0);
		if (sym == SYM_RPAREN)
		{
			getsym();
		}
		else
		{
			error(22); // missing ')'
		}

		statement(fsys);
		gen(JMP, 0, cx1 - cx);
		code[cx2].a = cx - cx2;
	}
	else if (sym == SYM_EXIT)
	{ //exit;
		gen(EXT, 0, 0);
		getsym();
		if (sym == SYM_SEMICOLON)   //2017.10.25
		{
			getsym();
		}
		else
		{
			error(10);     // ';'expected
		}
	}
	else if (sym == SYM_RET) {
		getsym();
		if (sym == SYM_SEMICOLON) {//return; 则把0放在被调用的栈顶，然后再放到原栈栈顶
			gen(LIT, 0, 0);
			gen(OPR, prodn, OPR_RET);
			getsym();
		}
		else {//return 1;return 1+x;return fact(n-1);
			top_expr(fsys);//调用后的结果存在栈顶
			gen(OPR, prodn, OPR_RET);
			if (sym != SYM_SEMICOLON)
				error(10);// missing ';'.
			else
				getsym();
		}
	}
	else if (sym == SYM_PRINT)
	{
		getsym();
		if (sym == SYM_LPAREN)
			getsym();
		else
			error(16);   //'(' expected
		set1 = createset(SYM_RPAREN, SYM_COMMA, SYM_NULL);
		set = uniteset(set1, fsys);
		if (sym == SYM_RPAREN)
		{
			getsym();
			gen(PRT, 0, 0);
		}
		else
		{
			while (inset(sym, facbegsys)) {
				top_expr(set);
				gen(PRT, 0, 1);
				if (sym == SYM_COMMA)
					getsym();
			}
			if (sym == SYM_RPAREN)
				getsym();
			else
				error(22);  //Missing ')'
		}
		if (sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);   //';' expected
	}
	else if (sym == SYM_SWITCH) {

		//switch (a) {case 1:return;}
		int case_num = 0;
		instruction tmp_stk[500], nul;
		for (int i = 0;i < 500;i++) {
			tmp_stk[i].l = 0;
			tmp_stk[i].a = 0;
			tmp_stk[i].f = 0;
		}
		nul.l = 0;
		nul.f = 0;
		nul.a = 0;
		int gen_cx;
		int stmt_cx;
		int cx1, cx2;
		int tmp_cnt;
		int cxiaddr = 0;
		int cxcase = 0;//执行语句的地址
		int cmp_num = 0;
		int next = 0;
		int i, j;
		int save_addr;
		int tmp_cx = cx;
		getsym();
		set1 = createset(SYM_RPAREN, SYM_NULL);
		set = uniteset(set1, fsys);

		if (sym != SYM_LPAREN)
			error(16);		//'（’ expected.

		else {
			top_expr(set);
			if (sym != SYM_RPAREN) error(22); //MISSING ')'.
			else getsym();
			if (sym != SYM_BEGIN) error(38); //'{' expected
			else getsym();
			while (sym != SYM_END) {
				if (sym != SYM_CASE)  error(40);
				else getsym();
				case_num++;
				if (sym != SYM_NUMBER) error(41);
				//LIT将常数置于栈顶,LOD将变量值置于栈顶
				gen(CPY, 0, 0);
				gen(LIT, 0, num);
				gen(OPR, 0, OPR_EQU);			/*如果不相等,为0 */
				gen(JPC, 0, 1);
				getsym();
				if (sym != SYM_COLOM) error(42); //缺少" :"
				else {							//先不在code[]中存放stmt的指令，而是存到tmp_stk[]中
					cx1 = cx;					//最后再一起移回code[]
					statement(fsys);
					cx2 = cx;
					if (sym == SYM_SEMICOLON) getsym();
					for (i = cx1;i < cx2;i++) {
						tmp_stk[i] = code[i];
						code[i] = nul;
						cx--;
					}
				}
			}
		}
		for (int i = tmp_cx;i < 4 * case_num + tmp_cx;i++) {
			if (code[i].a == JPC) {
				int tmp = 4 * (case_num + tmp_cx - i - 1);
				code[i].l = code[i].l + tmp;
			}

		}
		for (i = 0;tmp_stk[i].f == 0;i++);
		for (;tmp_stk[i].f != 0;i++) {
			code[cx] = tmp_stk[i];
			cx++;
		}
		getsym();
		destroyset(set);
		destroyset(set1);
	}
	else if (sym == SYM_DO) {
		cx1 = cx;
		getsym();
		if (sym != SYM_BEGIN) error(38);//'{'expected.
		getsym();
		set1 = createset(SYM_END, SYM_SEMICOLON, SYM_NULL);
		set = uniteset(set1, fsys);
		statement(set);
		while (inset(sym, statbegsys) || sym == SYM_SEMICOLON)     //把begin照抄下来了……
		{//sys在不在statement的前缀中,while true, there are other more statements before 'end',
			statement(set);
		} // while
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_END)
		{
			getsym();
			if (sym != SYM_WHILE)	error(43);// condition expected
			else {
				getsym();
				if (sym != SYM_LPAREN)		error(16);//'(' expected
				else {
					getsym();
				}
				set1 = createset(SYM_RPAREN, SYM_NULL);
				set = uniteset(set1, fsys);
				top_expr(set);
				gen(OPR, 0, OPR_NOT);
				gen(JPC, 0, cx1 - cx);
				if (sym == SYM_RPAREN) {
					getsym();
				}
				else {
					error(22); //Missing ')'.
				}
				if (sym == SYM_SEMICOLON) {
					getsym();
				}
				else {
					error(10);
				}
			}
		}
	}

	test(fsys, phi, 19);
} // statement

  //////////////////////////////////////////////////////////////////////
void block(symset fsys)
{
	int i, cx0, cx1 = 0; // initial code index
	mask* mk;
	int block_dx;
	int savedTx;
	symset set1, set;
	instruction temp[CXMAX];

	mk = (mask*)&table[tx - prodn];  //a critical bug appeared here
	mk->address = cx;
	mk->prodn = prodn;  //critical
	dx = 3;
	//printf("enter block\n");
	//block_dx = dx;

	gen(JMP, 0, 0);
	if (level > MAXLEVEL)
	{
		error(32); // There are too many levels.
	}
	do
	{
		if (sym == SYM_CONST)
		{ // constant declarations
			getsym();
			do
			{
				constdeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					constdeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			} while (sym == SYM_IDENTIFIER);
		} // if

		if (sym == SYM_VAR)
		{ // variable declarations
			cx0 = cx;  //store cx before var initialization
			getsym();
			do
			{
				vardeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					vardeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			} while (sym == SYM_IDENTIFIER);
			for (i = cx0;i<cx;i++) {
				//store codes for var initialization
				temp[cx1++] = code[i];
			}
			cx = cx0;
		} // if

		block_dx = dx; //save dx before handling procedure call!
		if (sym == SYM_PROCEDURE)
		{
			int k;
			char argumentID[TXMAX][MAXIDLEN + 1] = { 0 }, proceID[MAXIDLEN + 1] = { 0 };

			getsym();

			if (sym == SYM_IDENTIFIER)
			{
				enter(ID_PROCEDURE);
			}
			else
			{
				error(4);
			}

			savedTx = tx;   //critical
			level++;        //critical

			getsym();
			if (sym == SYM_LPAREN)
				getsym();
			else error(16);

			prodn = 0;
			if (sym == SYM_IDENTIFIER) {

				do {
					strcpy(argumentID[prodn++], id);  //?????????
					getsym();
					if (sym == SYM_COMMA)
						getsym();

				} while (sym == SYM_IDENTIFIER);//?????????в???
			}
			//printf("prodn = %d\n",prodn);
			k = prodn;
			while (k)
			{
				dx = -k;     //b?????????????dx?????
				strcpy(id, argumentID[prodn - (k--)]);
				enter(ID_VARIABLE);
			}

			if (sym == SYM_RPAREN)
				getsym();
			else error(22);//?????????

			set1 = createset(SYM_SEMICOLON, SYM_NULL);
			set = uniteset(set1, fsys);
			block(set);
			destroyset(set1);
			destroyset(set);
			tx = savedTx;
			level--;

			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}
		} // if
		dx = block_dx; //restore dx after handling procedure declaration! dx?????????????

	} while (inset(sym, declbegsys));

	set1 = createset(SYM_IDENTIFIER, SYM_NULL);
	set = uniteset(statbegsys, set1);
	test(set, declbegsys, 7);
	destroyset(set1);
	destroyset(set);

	code[mk->address].a = cx - (mk->address);
	mk->address = cx;
	cx0 = cx;//why save
			 //printf("befor statement dx = %d\n", dx);
	gen(INT, 0, dx);
	for (i = 0;i<cx1;i++) {
		code[cx++] = temp[i];
	}

	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
	set = uniteset(set1, fsys);
	prodn = mk->prodn;
	statement(set);
	destroyset(set1);
	destroyset(set);

	code[cx0].a = dx;
	gen(LIT, 0, 0);
	gen(OPR, prodn, OPR_RET);
	test(fsys, phi, 8); // test for error: Follow the statement is an incorrect symbol.
	listcode(cx0, cx);
} // block

  //////////////////////////////////////////////////////////////////////
int base(int stack[], int currentLevel, int levelDiff)
{
	int b = currentLevel;

	while (levelDiff--)
		b = stack[b];
	return b;
} // base

  //////////////////////////////////////////////////////////////////////
  // interprets and executes codes.
void interpret()
{
	int pc;        // program counter
	int stack[STACKSIZE];
	int top;       // top of stack
	int b;         // program, base, and top-stack register
	instruction i; // instruction register

	printf("Begin executing PL/0 program.\n");

	pc = 0;
	b = 1;
	top = 0;
	stack[1] = stack[2] = stack[3] = 0;
	do
	{
		//printf("pc = %d\n",pc);
		i = code[pc++];
		switch (i.f)
		{
		case LIT:
			stack[++top] = i.a;
			break;
		case OPR:
			switch (i.a) // operator
			{
			case OPR_RET: //put the return value in stack[b-i.l],since the argument isn't useful anymore,no need to worry that it may be covered.
				pc = stack[b + 2];
				stack[b - i.l] = stack[top];
				//printf("ret %d from [%d] to [%d]\n",stack[top],top,b-i.l);
				top = b - i.l;
				b = stack[top + i.l + 1];
				break;
			case OPR_NOT: //2017-09-24
				stack[top] = !stack[top];
				break;
			case OPR_AND:
				top--;
				stack[top] = stack[top] && stack[top + 1];
				break;
			case OPR_OR:
				top--;
				stack[top] = stack[top] || stack[top + 1];
				break;
			case OPR_NEG:
				stack[top] = -stack[top];
				break;
			case OPR_ADD:
				top--;
				//printf("add [%d]=%d, [%d]=%d\n",top,stack[top],top+1,stack[top+1]);
				stack[top] += stack[top + 1];
				break;
			case OPR_MIN:
				top--;
				//printf("min [%d]=%d, [%d]=%d\n",top,stack[top],top+1,stack[top+1]);
				stack[top] -= stack[top + 1];
				break;
			case OPR_MUL:
				top--;
				stack[top] *= stack[top + 1];
				break;
			case OPR_DIV:
				top--;
				if (stack[top + 1] == 0)
				{
					fprintf(stderr, "Runtime Error: Divided by zero.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] /= stack[top + 1];
				break;
			case OPR_EQU:
				top--;
				stack[top] = stack[top] == stack[top + 1];
				break;
			case OPR_NEQ:
				top--;
				stack[top] = stack[top] != stack[top + 1];
				break;
			case OPR_LES:
				top--;
				stack[top] = stack[top] < stack[top + 1];
				break;
			case OPR_GEQ:
				top--;
				stack[top] = stack[top] >= stack[top + 1];
				break;
			case OPR_GTR:
				top--;
				stack[top] = stack[top] > stack[top + 1];
				break;
			case OPR_LEQ:
				top--;
				stack[top] = stack[top] <= stack[top + 1];
				break;
			case OPR_MOD:                                    //2017.10.11
				top--;
				stack[top] = stack[top] % stack[top + 1];
				break;
			case OPR_BitAND:
				top--;
				stack[top] = stack[top] & stack[top + 1];
				break;
			case OPR_BitOR:
				top--;
				stack[top] = stack[top] | stack[top + 1];
				break;
			case OPR_BitXOR:
				top--;
				stack[top] = stack[top] ^ stack[top + 1];
				break;
			case OPR_BitNOT:
				stack[top] = ~stack[top];
			} // switch
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			//printf("%d from [%d] to [%d]\n", stack[top], base(stack, b, i.l) + i.a,top);
			break;
		case LDA:
			//printf("%d from [%d]\n",stack[base(stack,b,i.l)+i.a+stack[top]],base(stack,b,i.l)+i.a+stack[top]);
			stack[top] = stack[base(stack, b, i.l) + i.a + stack[top]];
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			//printf("STO %d to stack[%d]\n", stack[top], base(stack, b, i.l) + i.a);
			break;
		case STA:
			stack[base(stack, b, i.l) + i.a + stack[top - 1]] = stack[top];
			//printf("STO %d to stack[%d]\n", stack[top],base(stack, b, i.l) + i.a + stack[top-1]);
			stack[top - 1] = stack[top];
			top--;
			break;
		case INT:
			top += i.a;
			break;
		case POP:
			top--;
			break;
		case JMP:
			pc += i.a - 1;
			break;
		case DIP: //This instruction is added to transfer prodn with CAL  11.18
			stack[top + 1] = base(stack, b, i.l);
			stack[top + 2] = b;
			stack[top + 3] = pc + 1;
			b = top + 1;
			break;
		case CAL:
			pc = i.a;
			break;

		case JPC:
			if (stack[top] == 0)
				pc += i.a - 1;
			top--;
			break;
		case EXT:                      //2017.10.25
			pc = 0;
			printf("Exit Program");
			break;
		case PRT:
			if (i.a)
				printf("%d  ", stack[top--]);
			else
				printf("\n");
			break;
		case RDM:
			srand((unsigned)time(NULL));
			if (i.a)
				stack[top] = rand() % i.a;
			else
				stack[top] = rand();
			top++;
			break;
		case CPY://copy the value of stack[top]
			stack[top + 1] = stack[top];
			top++;
			break;
		} // switch
		  //printf("pc = %d, top = %d\n",pc,top);
	} while (pc);

	printf("\nEnd executing PL/0 program.\n");
} // interpret

  //////////////////////////////////////////////////////////////////////
int main()
{
	FILE* hbin;
	char s[80];
	int i;
	symset set, set1, set2;

	printf("Please input source file name: "); // get file name to be compiled
	scanf("%s", s);
	if ((infile = fopen(s, "r")) == NULL)
	{
		printf("File %s can't be opened.\n", s);
		exit(1);
	}

	phi = createset(SYM_NULL);
	relset = createset(SYM_EQU, SYM_NEQ, SYM_LES, SYM_LEQ, SYM_GTR, SYM_GEQ, SYM_NULL);

	// create begin symbol sets
	declbegsys = createset(SYM_CONST, SYM_VAR, SYM_PROCEDURE, SYM_NULL);
	facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_LPAREN, SYM_MINUS, SYM_NOT, SYM_BITNOT, SYM_RDM, SYM_NULL);
	set = createset(SYM_BEGIN, SYM_CALL, SYM_IF, SYM_WHILE, SYM_FOR, SYM_EXIT, SYM_RET, SYM_PRINT, SYM_DO, SYM_SWITCH, SYM_NULL);
	statbegsys = uniteset(facbegsys, set);
	destroyset(set);

	err = cc = cx = vx = ll = factflag = num = 0; // initialize global variables
	ch = ' ';
	kk = MAXIDLEN;
	*id = '\0';
	td = NULL;

	getsym();

	set1 = createset(SYM_PERIOD, SYM_NULL);
	set2 = uniteset(declbegsys, statbegsys);
	set = uniteset(set1, set2);
	block(set);
	destroyset(set1);
	destroyset(set2);
	destroyset(set);
	destroyset(phi);
	destroyset(relset);
	destroyset(declbegsys);
	destroyset(statbegsys);
	destroyset(facbegsys);

	if (sym != SYM_PERIOD)
		error(9); // '.' expected.
	if (err == 0)
	{
		hbin = fopen("hbin.txt", "w");
		for (i = 0; i < cx; i++)
			fwrite(&code[i], sizeof(instruction), 1, hbin);
		fclose(hbin);
	}
	if (err == 0)
		interpret();
	else
		printf("There are %d error(s) in PL/0 program.\n", err);
	//listcode(0, cx);
	system("pause");
} // main

  //////////////////////////////////////////////////////////////////////
  // eof pl0.c
