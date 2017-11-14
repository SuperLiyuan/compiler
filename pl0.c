// pl0 compiler source code

#pragma warning(disable:4996)


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "PL0.h"
#include "set.c"

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
		if (feof(infile))
		{
			printf("\nPROGRAM INCOMPLETE\n");
			exit(1);
		}
		ll = cc = 0;
		printf("%5d  ", cx);
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
	else if(ch == '!')                                  //2017.10.13
    {
        getch();
        if(ch == '=')
        {
            sym = SYM_NEQ;
            getch();
        }
        else
        {
            sym = SYM_NOT;
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
			getch(); //skip '*'
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
	else if(ch == -1)
	{
		error(31);  //incomplete program
		exit(1);
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
			printf("Fatal Error: Unknown character:%d.\n",ch);
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
int dx;  // data allocation index

// enter object(constant, variable or procedre) into table.
void enter(int kind)
{
	mask* mk;

	tx++;
	strcpy(table[tx].name, id);
	table[tx].kind = kind;
	switch (kind)
	{
	case ID_CONSTANT:
		if (num > MAXADDRESS)
		{
			error(25); // The number is too great.
			num = 0;
		}
		table[tx].value = num;
		break;
	case ID_VARIABLE:
		mk = (mask*)&table[tx];
		mk->level = level;
		mk->address = dx++;
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
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if (sym == SYM_EQU || sym == SYM_BECOMES)
		{
			if (sym == SYM_BECOMES)
				error(1); // Found ':=' when expecting '='.
			getsym();
			if (sym == SYM_NUMBER)
			{
				enter(ID_CONSTANT);
				getsym();
			}
			else
			{
				error(2); // There must be a number to follow '='.
			}
		}
		else
		{
			error(3); // There must be an '=' to follow the identifier.
		}
	}
	else	error(4);
	// There must be an identifier to follow 'const', 'var', or 'procedure'.
} // constdeclaration

//////////////////////////////////////////////////////////////////////
void vardeclaration(void)
{
	if (sym == SYM_IDENTIFIER)
	{
		enter(ID_VARIABLE);
		getsym();
	}
	else
	{
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
	}
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
void factor(symset fsys)
{
	void top_expr(symset fsys);
	int i;
	symset set;

	if (factflag)  //assigned by top_expr          2017.10.24 , 2017.10.26
	{
		factflag = 0;
	}
	else{
	    while (inset(sym, facbegsys))
	    {
		    if (sym == SYM_IDENTIFIER)
		    {
			    if ((i = position(id)) == 0)
			    {
				    error(11); // Undeclared identifier.
			    }
		    	else
			    {
				    switch (table[i].kind)
				    {
					    mask* mk;
				    case ID_CONSTANT:
					    gen(LIT, 0, table[i].value);
					    break;
				    case ID_VARIABLE:
					    mk = (mask*)&table[i];
					    gen(LOD, level - mk->level, mk->address);
					    break;
				    case ID_PROCEDURE:
						mk = (mask*)&table[i];
						gen(CAL, level - mk->level, mk->address);
						break;
					} // switch
				}
				getsym();
			}
			else if (sym == SYM_NUMBER)
			{
				if (num > MAXADDRESS)
				{
					error(25); // The number is too great.
					num = 0;
				}
				gen(LIT, 0, num);
				getsym();
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
			else if (sym == SYM_MINUS) // UMINUS,  Expr -> '-' Expr
			{
				getsym();
				factor(fsys);          //2017.10.13
				gen(OPR, 0, OPR_NEG);
			}
			else if (sym == SYM_NOT) // UMINUS,  Expr -> '!' Expr
			{
				getsym();
				factor(fsys);          //2017.10.13
				gen(OPR, 0, OPR_NOT);
			}
			else if (sym == SYM_ODD) //2017-09-24
			{
				getsym();
				factor(fsys);          //2017.10.13
				gen(OPR, 0, OPR_ODD);
			}
			else if (sym == SYM_BITNOT)
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
		else if(mulop == SYM_SLASH)
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

	/* //2017-0924
	if (sym == SYM_ODD)
	{
		getsym();
		expression(fsys);
		gen(OPR, 0, 6);
	}
	else
	*/
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
		} // else
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
			code[cx0->cx].a = cx-(cx0->cx);
		}
		free(cx0);
		gen(LIT, 0, 0);        //if JPC, restore 0
		code[cx1].a = cx-cx1;
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
			code[cx0->cx].a = cx-(cx0->cx);
		}
		free(cx0);
		gen(LIT, 0, 1);        //if JPC, restore 1
		code[cx1].a = cx-cx1;
	}

	destroyset(set);
} // or_expr

//////////////////////////////////////////////////////////////////////
void top_expr(symset fsys)              //2017.10.26
{
	int i;
	test(facbegsys, fsys, 24); // The symbol can not be as the beginning of an expression.

	if (sym == SYM_IDENTIFIER)
	{
		mask* mk;
		if (!(i = position(id)))
		{
			error(11); // Undeclared identifier.
		}
		else if (table[i].kind == ID_VARIABLE)
		{
			mk = (mask*)&table[i];
			gen(LOD, level - mk->level, mk->address);
			getsym();
			if (sym == SYM_BECOMES)
			{// assignment expression
				getsym();
				cx--;    //delete LOD instruction
				top_expr(fsys);
				gen(STO, level - mk->level, mk->address);
			}
			else
			{

				factflag = 1; // means a operant already LODed
				or_expr(fsys);
			}
		}
		else if (table[i].kind == ID_CONSTANT)
		{
			or_expr(fsys);
		}
		else // ID_PROCEDURE
		{
			; // procedure call
		}
	}
	else
	{
		or_expr(fsys);
	}
}

//////////////////////////////////////////////////////////////////////
void statement(symset fsys)
{
	int i, cx1, cx2, cx3;
	int temp,tmpnum;
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
	else if (sym == SYM_CALL)
	{ // procedure call
		getsym();
		if (sym != SYM_IDENTIFIER)
		{
			error(14); // There must be an identifier to follow the 'call'.
		}
		else
		{
			if (!(i = position(id)))
			{
				error(11); // Undeclared identifier.
			}
			else if (table[i].kind == ID_PROCEDURE)
			{
				mask* mk;
				mk = (mask*)&table[i];

				getsym();
				if( sym == SYM_LPAREN ){
					getsym;
				}
				else error( );

				for(j=1;j<=mk->prodn;i++)
					{
					if( sym == SYM_IDENTIFIER){
						if (!(i = position(id)))
						{
							error(11); // Undeclared identifier.
						}
						else if (table[i].kind == ID_PROCEDURE)

							error();//过程不能作为参数
						else if(table[i].kind == ID_VARIABLE){
							 mask* mk0 = (mask*)&table[i];
							 gen(STO, level - mk0->level, mk0->address);
						}
						else if(table[i].kind == ID_CONSTANT){
							gen(STO, 0, table[i].value );
						}
						getsym();
						if(sym == rparen)
							break;
						else if(sym == SYM_COMMA)
							getsym();
					}
					else if( sym == rparen)
						break;
					else error();//缺右括号
				}
				if(j != mk->prodn){
					error();//传参数目错误
				}

				gen(CAL, level - mk->level, mk->address);
			}
			else
			{
				error(15); // A constant or variable can not be called.
			}
			getsym();
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
		or_expr(set);//condition(set);
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
			code[cx1].a = cx-cx1;
			cx1 = cx - 1;          //cx1 = JMP,0,0
			getsym();
			statement(fsys);
			code[cx1].a = cx-cx1;
		}
		else if (inset(sym, statbegsys))
		{
			code[cx1].a = cx-cx1;     //cx1 = JPC,0,0
		}
	}
	else if (sym == SYM_ELSE)
	{
		error(27);  // Unmatched 'else'
		getsym();
	}
	else if (sym == SYM_BEGIN)
	{ //chunk
		getsym();
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
			getsym();
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
	    if( sym != SYM_LPAREN)
	    {
	        error(16);   // '(' expected
	    }
	    else
        {
            getsym();
            set1 = createset(SYM_SEMICOLON, SYM_RPAREN, SYM_NULL);
	        set = uniteset(set1, fsys);
	        top_expr(set);
	        if(sym != SYM_SEMICOLON)
	        {
                error(10);   //';' expected
	            test(fsys,set1,28);   //Incomplete 'for' statement.
	        }
	        else
            {
                getsym();
                gen(POP, 0, 0);
                cx1 = cx;  //come back here after loop
                top_expr(set);
                if(sym != SYM_SEMICOLON)
	            {
                    error(10);   //';' expected
	                test(fsys,set1,28);    //Incomplete 'for' statement.
                }
                else
                {
                    getsym();

                    cx2 = cx;  //if false, skip loop
                    gen(JPC, 0, 0);

                    cx3 = cx;  //beginning of codes that need move
                    top_expr(set);
                    if(sym == SYM_RPAREN)
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
                    for(i = cx3; i<cx; i++)
                    {
                        temp[i-cx3]=code[i];
                    }
                    cx = cx3;
                    cx3 = i-cx3; //the length of temp
                    statement(fsys);
                    for(i = 0; i<cx3; i++)
                    {
                        code[cx++]=temp[i];
                    }
                    cx1 = cx1-cx;     //offset
                    gen(JMP, 0, cx1); //come back to condition
                    code[cx2].a = cx-cx2; //destination of false condition
                }
            }
        }
	}
	else if (sym == SYM_WHILE)
	{ // while statement
		cx1 = cx;

		getsym();
		if(sym == SYM_LPAREN)
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
		gen(JMP, 0, cx1-cx);
		code[cx2].a = cx-cx2;
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
			gen(OPR, 0, OPR_RET);
		}
		else if (sym == )
		else {//return 1;return 1+x;return fact(n-1);
			expression();//调用后的结果存在栈顶
			gen(OPR, 0, OPR_RET);
		}
	}
	test(fsys, phi, 19);
} // statement

//////////////////////////////////////////////////////////////////////
void block(symset fsys)
{
	int cx0; // initial code index
	mask* mk;
	int block_dx;
	int savedTx;
	symset set1, set;

	mk = (mask*)&table[tx];
	mk->address = cx;
	dx = 3+mk->prodn;
	block_dx = dx;

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
		} // if
		block_dx = dx; //save dx before handling procedure call!
		/*while (sym == SYM_PROCEDURE)
		{ // procedure declarations
			getsym();
			if (sym == SYM_IDENTIFIER)
			{
				enter(ID_PROCEDURE);
				getsym();
			}
			else
			{
				error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
			}*/

//在这里添加参数的识别
	while( sym == SYM_PROCEDURE )
	{
		dx = 3;
		prodn = 0;

		getsym();

		if( sym == IDENTIFIER )
		{
			enter(ID_PROCEDURE);
		}
		else
		{
			error(4);
		}
		getsym();
		if( sym == LPAREN )
			getsym();

		else error();

		if(sym == SYM_INDENTIFIER ){

			do{
				enter( SYM_VARIABLE);   //记录参数名
				++prodn;
				getsym();
				if( sym == SYM_COMMA)

				getsym();
			}while(sym == SYM_INDENTIFIER);//处理完所有参数
		}

		if( sym == RPAREN )
        	getsym();
		else error();//缺少右括号

	}

/*			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}*/

			level++;
			savedTx = tx;
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
				set1 = createset(SYM_IDENTIFIER/*为什么有id*/, SYM_PROCEDURE, SYM_NULL);
				set = uniteset(statbegsys, set1);
				test(set, fsys, 6);
				destroyset(set1);
				destroyset(set);
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}
		} // while
		dx = block_dx+mk->prodn; //restore dx after handling procedure declaration! dx是参数和局部变量的总个数
		set1 = createset(SYM_IDENTIFIER, SYM_NULL);
		set = uniteset(statbegsys, set1);
		test(set, declbegsys, 7);
		destroyset(set1);
		destroyset(set);
	} while (inset(sym, declbegsys));//正常的情况下，此处while不会循环第二次？

	code[mk->address].a = cx-(mk->address);
	mk->address = cx;
	cx0 = cx;//why save
	gen(INT, 0, dx-3);//原本是block，改成dx
	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
	set = uniteset(set1, fsys);
	statement(set);
	destroyset(set1);
	destroyset(set);
	gen(OPR, 0, OPR_RET); // 是怎么知道要return的 ，看了statament，statement在处理完一个最外层的begin……end后就会返回
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
	top = 3;
	stack[1] = stack[2] = stack[3] = 0;
	do
	{
		i = code[pc++];
		switch (i.f)
		{
		case LIT:
			stack[++top] = i.a;
			break;
		case OPR:
			switch (i.a) // operator
			{
			case OPR_RET:
				stack[b] = stack[top];
				top = b;
				pc = stack[top + 3];
				b = stack[top + 2];
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
				stack[top] += stack[top + 1];
				break;
			case OPR_MIN:
				top--;
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
			case OPR_ODD:
				stack[top] %= 2;
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
                stack[top] = stack[top] & stack[top+1];
                break;
            case OPR_BitOR:
                top--;
                stack[top] = stack[top] | stack[top+1];
                break;
            case OPR_BitXOR:
                top--;
                stack[top] = stack[top] ^ stack[top+1];
				break;
			case OPR_BitNOT:
				stack[top] = ~stack[top];
			} // switch
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			printf("%d\n", stack[top]);
			break;
		case CAL:
			stack[top++] = base(stack, b, i.l);
			// generate new block mark
			stack[top++] = b;
			stack[top++] = pc;

			b = top -2;
			pc = i.a;
			break;
		case INT:
			top += i.a;
			break;
		case POP:
		    top--;
			break;
		case JMP:
			pc += i.a-1;
			break;
		case JPC:
			if (stack[top] == 0)
				pc += i.a-1;
			top--;
			break;
		case EXT:                      //2017.10.25
			pc = 0;
			break;
		} // switch
	} while (pc);

	printf("End executing PL/0 program.\n");
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
	facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_LPAREN, SYM_MINUS, SYM_ODD, SYM_NOT, SYM_BITNOT, SYM_NULL);
	set = createset(SYM_BEGIN, SYM_CALL, SYM_IF, SYM_WHILE, SYM_FOR, SYM_EXIT, SYM_NULL);
	statbegsys = uniteset(facbegsys, set);
	destroyset(set);

	err = cc = cx = ll = factflag = 0; // initialize global variables
	ch = ' ';
	kk = MAXIDLEN;

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
	listcode(0, cx);
} // main

//////////////////////////////////////////////////////////////////////
// eof pl0.c
