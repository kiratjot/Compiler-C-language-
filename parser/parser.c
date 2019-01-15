/*
* File Name: parser.c
* Compiler: MS Visual Studio 2015 Community
* Author: Gurkirat Singh 040876315, Shubham Ujinwal 040885893
* Course: CST 8152  Compilers
* Lab Section: 12
* Assignment: 3
* Date: 6 December 2018
* Professor: Sv. Ranev
* Purpose: To write a Recursive Descent Predictive Parser (RDPP) for the PLATYPUS language
* Function list:parser();
*				match();
*				syn_printe();
*				syn_eh(int);
*				gen_incode();
*				program();
*				optStatements();
*				statements();
*				statement();
*				statements_p();
*				assignmentStatement();
*				assignmentExpression();
*				selectionStatement();
*				iterationStatement();
*				preCondition();
*				inputStatement();
*				variableList();
*				variableIdentifier();
*				variableList_p();
*				outputStatement();
*				outputList();
*				arithmeticExpression();
*				unaryArithExp();
*				primaryArithExp();
*				addArithExp();
*				addArithExp_p();
*				multiArithExp();
*				multiArithExp_p();
*				stringExpression();
*				primaryStringExp();
*				stringExpression_p();
*				conditionalExpression();
*				logicalORexpression();
*				logicalORexpression_p();
*				logicalANDexpression();
*				logicalANDexpression_p();
*				relationExpression();
*				primaryArelationExp_p();
*				primaryArelationExp();
*				primarySrelationExp_p();
*				primarySrelationExp();
*
*/
#include <stdlib.h>
#include "parser.h"

/*******************************************************************************
Authors:			Svillen Ranev
*******************************************************************************/
void parser(void)
{
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
*******************************************************************************/
void match(int pr_token_code, int pr_token_attribute)
{
	/*If the match is unsuccessful, the function calls the error handler
	syn_eh() and returns.*/
	if (pr_token_code != lookahead.code)
	{
		syn_eh(pr_token_code);
		return;
	}

	/*If the token code matches with  KW_T, LOG_OP_T, ART_OP_T, REL_OP_T
	then attribute code will be compared.*/
	switch (pr_token_code)
	{
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			if (pr_token_attribute != lookahead.attribute.get_int)
			{
				syn_eh(pr_token_code);
				return;
			}
			break;
		default:
			break;
	}

	switch (lookahead.code)
	{
		/*If the match is successful and the lookahead is SEOF_T, the function returns.*/
		case SEOF_T:
			return;
		default:
			lookahead = malar_next_token();
			/*If the lookahead matches with ERR_T,  the function calls the error printing function
			syn_printe()*/
			if (lookahead.code == ERR_T)
			{
				syn_printe();
				lookahead = malar_next_token();
				synerrno++;
				return;
			}
	}
}



/*******************************************************************************
Authors:			Svillen Ranev
*******************************************************************************/
void syn_printe()
{
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code)
	{
		case  ERR_T: /* ERR_T     0   Error token */
			printf("%s\n", t.attribute.err_lex);
			break;
		case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
			printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
			break;
		case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
		case  SVID_T:/* SVID_T    3  String Variable identifier token */
			printf("%s\n", t.attribute.vid_lex);
			break;
		case  FPL_T: /* FPL_T     4  Floating point literal token */
			printf("%5.1f\n", t.attribute.flt_value);
			break;
		case INL_T: /* INL_T      5   Integer literal token */
			printf("%d\n", t.attribute.get_int);
			break;
		case STR_T:/* STR_T     6   String literal token */
			printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
			break;

		case SCC_OP_T: /* 7   String concatenation operator token */
			printf("NA\n");
			break;

		case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
			printf("NA\n");
			break;
		case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
			printf("%d\n", t.attribute.get_int);
			break;

		case  LPR_T: /*LPR_T    12  Left parenthesis token */
			printf("NA\n");
			break;
		case  RPR_T: /*RPR_T    13  Right parenthesis token */
			printf("NA\n");
			break;
		case LBR_T: /*    14   Left brace token */
			printf("NA\n");
			break;
		case RBR_T: /*    15  Right brace token */
			printf("NA\n");
			break;

		case KW_T: /*     16   Keyword token */
			printf("%s\n", kw_table[t.attribute.get_int]);
			break;

		case COM_T: /* 17   Comma token */
			printf("NA\n");
			break;
		case EOS_T: /*    18  End of statement *(semi - colon) */
			printf("NA\n");
			break;
		default:
			printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

 /*******************************************************************************
 Authors:			Gurkirat Singh, Shubham Ujinwal
 *******************************************************************************/
void syn_eh(int sync_token_code)
{
	/*calling syn_printe() and increments the error counter*/
	syn_printe();
	synerrno++;

	/*iterating lookahead until it matches sync_token_code*/
	while (lookahead.code != sync_token_code)
	{
		/*If the lookahead.code matches with SEOF_T. ,calls exit(synerrno) function. */
		if (lookahead.code == SEOF_T)
		{
			exit(synerrno);
		}

		lookahead = malar_next_token();
	}
	/*If the sync_token_code matches with SEOF_T, the funcion returns.*/
	if (sync_token_code == SEOF_T)
	{
		return;
	}

	lookahead = malar_next_token();
	return;


}
/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
*******************************************************************************/
void gen_incode(char *str)
{
	/*takes a string as an argument and prints it*/
	printf("%s\n", str);
}

/*******************************************************************************
Authors:			Svillen Ranev
Grammer:			<program> -> PLATYPUS {<opt_statements>}
First Set:			FIRST(<program>) = { KW_T(PLATYPUS) }
*******************************************************************************/
void program(void)
{
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	optStatements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}



/*******************************************************************************
Authors:			Svillen Ranev
Grammer:			<opt_statements> -> <statements> | ϵ
First Set:			FIRST(<opt_statements>) = { FIRST(<statements>) }
= { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE),
KW_T(READ), KW_T(WRITE), ϵ}
*******************************************************************************/
void optStatements()
{
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: statements(); break;
		case KW_T:
			/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here
			and in statements_p()*/
			if (lookahead.attribute.get_int != PLATYPUS
				&& lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN
				&& lookahead.attribute.get_int != REPEAT
				&& lookahead.attribute.get_int != TRUE
				&& lookahead.attribute.get_int != FALSE)
			{
				statements();
				break;
			}
		default: /*empty string – optional statements*/;
			gen_incode("PLATY: Opt_statements parsed");
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<statements> -> <statement><statements'>
First Set:			FIRST(<statements>) = { FIRST(<statement>) }
= { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ),
KW_T(WRITE) }
*******************************************************************************/
void statements()
{
	statement();
	statements_p();
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<statement> -> <assignment statement>| <selection statement>
| <iteration statement> | <input statement>
| <output statement>

First Set			FIRST(<statement>) = { FIRST(<assignment statement>),
FIRST(<selectionstatements>),
FIRST(<iteration statement>),
FIRST(<inputstatement>),
FIRST(<output statement>) }

= { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ),
KW_T(WRITE) }

*******************************************************************************/
void statement()
{
	/*First set: { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ),
	KW_T(WRITE) }*/
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: assignmentStatement(); break;
		case KW_T:
			/* look for IF, WHILE, READ and WRITE here */
			switch (lookahead.attribute.get_int)
			{
				case IF: selectionStatement(); break;
				case WHILE: iterationStatement(); break;
				case WRITE: outputStatement(); break;
				case READ: inputStatement(); break;

				default:
					break;
			}
			break;
		default:
			syn_printe();
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<statements'> -> <statement><statements'> | ϵ
First Set:			FIRST(<statements’>) = { FIRST(<statement>) | ϵ }

= { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE),
KW_T(READ),
KW_T(WRITE) , ϵ }

*******************************************************************************/
void statements_p()
{
	/*First set: { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE),KW_T(READ), KW_T(WRITE) , ϵ }*/
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T:
			statement();
			statements_p();
			break;

		case KW_T:
			/* look for IF, WHILE, READ and  WRITE here */
			switch (lookahead.attribute.get_int)
			{
				case IF:
				case WHILE:
				case READ:
				case WRITE:
					statement();
					statements_p();
					break;
				default:
					/*empty string – optional statements*/
					break;
			}
		default:
			break;
	}
}



/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<assignment statement> -> <assignment expression>;
First Set:			FIRST(<assignment statement>)= {FIRST(<assignment expression>)}

= { AVID_T, SVID_T }
*******************************************************************************/
void assignmentStatement()
{
	assignmentExpression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<assignment expression> ->  AVID = <arithmetic expression>
| SVID = <string expression>

First Set:			FIRST(<assignment expression>) = { AVID_T, SVID_T }
*******************************************************************************/
void assignmentExpression()
{
	/*First set: { AVID_T, SVID_T }*/
	switch (lookahead.code)
	{
		case AVID_T:
			match(AVID_T, NO_ATTR); match(ASS_OP_T, EQ);
			arithmeticExpression();
			gen_incode("PLATY: Assignment expression (arithmetic) parsed");
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR);
			match(ASS_OP_T, EQ);
			stringExpression();
			gen_incode("PLATY: Assignment expression (string) parsed");
			break;
		default:
			syn_printe();
			break;
	}
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<selection statement> -> IF <pre-condition> (<conditional expression>)
THEN { <opt_statements> }   ELSE { <opt_statements> } ;
First Set:			FIRST(<selection statement>) = { KW_T(IF) }
*******************************************************************************/
void selectionStatement()
{
	match(KW_T, IF);
	preCondition();
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	optStatements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	optStatements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<iteration statement> ->WHILE <pre-condition>(<conditional expression>)
REPEAT {<statements>};
First Set:			FIRST(<iteration statement>) = { KW_T(WHILE) }
*******************************************************************************/
void iterationStatement()
{
	match(KW_T, WHILE);
	preCondition();
	match(LPR_T, NO_ATTR);
	conditionalExpression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<pre-condition> -> {KW_T(TRUE),KW_T(FALSE)}
First Set:			FIRST(<pre-condition>) = {KW_T(TRUE),KW_T(FALSE)}
*******************************************************************************/
void preCondition()
{
	/*First set: {KW_T(TRUE),KW_T(FALSE)}*/
	switch (lookahead.code)
	{
		case KW_T:
			/*look for TRUE and FALSE here*/
			switch (lookahead.attribute.get_int)
			{
				case TRUE:
				case FALSE:match(KW_T, lookahead.attribute.get_int); break;
				default:
					syn_printe();
					break;
			}
			break;
		default:
			syn_printe();
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<input statement> -> READ (<variable list>);
First Set:			FIRST(<input statement>) = { KW_T(READ) }
*******************************************************************************/
void inputStatement()
{
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variableList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<variable list> -> <variable identifier> <variable list’>
First Set:			FIRST(<variable list>) = { FIRST(<variable identifier>) }
= { AVID_T, SVID_T }
*******************************************************************************/
void variableList()
{
	variableIdentifier();
	variableList_p();
	gen_incode("PLATY: Variable list parsed");
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer				<variable identifier> -> AVID_T | SVID_T
First Set:			FIRST(<variable identifier>) = { AVID_T, SVID_T }
*******************************************************************************/
void variableIdentifier()
{
	/*First set: { AVID_T, SVID_T }*/
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: match(lookahead.code, NO_ATTR); break;
		default:
			syn_printe();
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<variable list’> ->  ,<variable identifier><variable list’> | ϵ
First Set:			FIRST(<variable list’>) = { COM_T, AVID_T, SVID_T, ϵ }
*******************************************************************************/
void variableList_p()
{
	/*First set: { COM_T }*/
	switch (lookahead.code)
	{
		case COM_T:
			match(COM_T, NO_ATTR); variableIdentifier(); variableList_p(); break;
		default:
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<output statement> -> WRITE (<output list>);
First Set:			FIRST(<output statement>) = { KW_T(WRITE) }
*******************************************************************************/
void outputStatement()
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	outputList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<output list> -> <variable list> | STR_T | ϵ
First Set:			FIRST(output list> = { FIRST<variable list> }
= { AVID_T, SVID_T, STR_T, ϵ}
*******************************************************************************/
void outputList()
{
	/*First set: { AVID_T, SVID_T, STR_T, ϵ}*/
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: variableList(); break;
		case STR_T: match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (string literal) parsed");
			break;
		default:
			/*empty string – optional statements*/
			gen_incode("PLATY: Output list (empty) parsed");
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<arithmetic expression> - ><unary arithmetic expression>  |
<additive arithmetic expression>
First Set:			FIRST(<arithmetic expression>) = { FIRST(<unary arithmetic expression>,
FIRST(<additive arithmetic expression>) }

= { -, +, AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void arithmeticExpression()
{
	/*First set: { -, +, AVID_T, FPL_T, INL_T, ( }*/
	switch (lookahead.code)
	{
		case ART_OP_T:
			/*look for PLUS and MINUS*/
			switch (lookahead.attribute.get_int)
			{
				case PLUS:
				case MINUS: unaryArithExp(); break;

				default:
					syn_printe();
					break;
			}
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		case AVID_T:
		case FPL_T:
		case INL_T:
		case LPR_T: addArithExp();
			gen_incode("PLATY: Arithmetic expression parsed"); break;

		default:
			syn_printe();
			break;
	}
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<unary arithmetic expression> -> - <primary arithmetic expression>
| + <primary arithmetic expression>
First Set:			FIRST(<unary arithmetic expression>) = { -, + }
*******************************************************************************/
void unaryArithExp()
{
	/*First set: { -, + }*/
	switch (lookahead.attribute.get_int)
	{
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.get_int);
			primaryArithExp();
			break;
		default:
			syn_printe();
			break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<primary arithmetic expression> ->  AVID_T
| FPL_T
| INL_T
| (<arithmetic expression>)
First Set:			FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void primaryArithExp()
{
	/*First set: { AVID_T, FPL_T, INL_T, ( }*/
	switch (lookahead.code)
	{
		case AVID_T:
		case FPL_T:
		case INL_T: match(lookahead.code, NO_ATTR); break;
		case LPR_T:
			match(LPR_T, NO_ATTR);
			arithmeticExpression();
			match(RPR_T, NO_ATTR);
			break;
		default:
			/*empty string – optional statements*/
			break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<additive arithmetic expression> -> <multiplicative arithmetic expression>
<additive arithmetic expression’>
First Set:			FIRST(<additive arithmetic expression>) =
{ FIRST(<multiplicative arithmetic expression>),
FIRST(<primary arithmetic expression>) }

= { AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void addArithExp()
{
	multiArithExp();
	addArithExp_p();
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<additive arithmetic expression’> ->
+ <multiplicative arithmetic expression> <additive arithmetic expression’>
| - <multiplicative arithmetic expression> <additive arithmetic expression’>
| ϵ
First Set:			FIRST(<additive arithmetic expression’>) = { +, -, ϵ }
*******************************************************************************/
void addArithExp_p()
{
	/*First set: { +, -, ϵ }*/
	switch (lookahead.code)
	{
		case ART_OP_T:
			/*look for PLUS and MINUS*/
			switch (lookahead.attribute.get_int)
			{
				case PLUS:
				case MINUS:
					match(ART_OP_T, lookahead.attribute.get_int);
					multiArithExp();
					addArithExp_p();
					gen_incode("PLATY: Additive arithmetic expression parsed");
					break;

				default:
					syn_printe();
					break;
			}
			break;

		default:
			/*empty string – optional statements*/
			break;
	}

}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<multiplicative arithmetic expression> -><primary arithmetic expression>
< multiplicative arithmetic expression’>

First Set:			FIRST(<multiplicative arithmetic expression>) =
{ FIRST(<primary arithmetic expression>)}
= { AVID_T, FPL_T, INL_T, ( }
*******************************************************************************/
void multiArithExp()
{
	primaryArithExp();
	multiArithExp_p();
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<multiplicative arithmetic expression’> ->
* < primary arithmetic expression> < multiplicative arithmetic expression’>
| / < primary arithmetic expression> < multiplicative arithmetic expression’>
| ϵ
First Set:			FIRST(<multiplicative arithmetic expression’>) = { *, /, ϵ }
*******************************************************************************/
void multiArithExp_p()
{
	/*First set: { *, /, ϵ }*/
	switch (lookahead.attribute.get_int)
	{
		case MULT:
		case DIV:
			match(ART_OP_T, lookahead.attribute.get_int);
			primaryArithExp();
			multiArithExp_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		default:
			/*empty string – optional statements*/
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<string expression> -> <primary string expression><string expression’>
First Set:			FIRST(<string expression>) = { FIRST(<primary string expression>) }
= { SVID_T, STR_T }
*******************************************************************************/
void stringExpression()
{
	primaryStringExp();
	stringExpression_p();
	gen_incode("PLATY: String expression parsed");
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<primary string expression> ->  SVID_T| STR_T
First Set:			FIRST(<primary string expression>) = { SVID_T, STR_T }
*******************************************************************************/
void primaryStringExp()
{
	/*First set: { SVID_T, STR_T }*/
	switch (lookahead.code)
	{
		case SVID_T:
		case STR_T:  match(lookahead.code, NO_ATTR);
			break;

		default:
			syn_printe();
			break;

	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<string expression’> -> # <primary string expression> <string expression’>
| ϵ
First Set:			FIRST(<string expression’>) = { #, ϵ }
*******************************************************************************/
void stringExpression_p()
{
	/*First set: { #, ϵ }*/
	switch (lookahead.code)
	{
		case SCC_OP_T:
			match(SCC_OP_T, NO_ATTR);
			primaryStringExp();
			stringExpression_p();
			break;
		default:
			/*empty string – optional statements*/
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<conditional expression> -> <logical OR expression>
First Set:			FIRST(<conditional expression>)
= { FIRST(<logical OR expression>) }
= { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*******************************************************************************/
void conditionalExpression()
{
	logicalORexpression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<logical OR expression> ->	<logical AND expression>
<logical OR expression’>
First Set:			= { FIRST(<logical OR expression>)}
FIRST(<logical AND expression>),
FIRST(<relational expression>),
FIRST(<primary a_relational expression>),
FIRST(primary s_relational expression>) }

= { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*******************************************************************************/
void logicalORexpression()
{
	logicalANDexpression();
	logicalORexpression_p();
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<logical OR expression’> -> .OR.<logical AND expression>
<logical OR expression’>| ϵ
First Set:			FIRST(<logical OR expression’>) = { .OR. , ϵ }
*******************************************************************************/
void logicalORexpression_p()
{
	/*First set: { .OR. , ϵ }*/
	switch (lookahead.code)
	{
		case LOG_OP_T:
			/*look for OR and ϵ here*/
			switch (lookahead.attribute.get_int)
			{
				case OR:
					match(LOG_OP_T, lookahead.attribute.get_int);
					logicalANDexpression();
					logicalORexpression_p();
					gen_incode("PLATY: Logical OR expression parsed");
					break;
				default:
					/*empty string – optional statements*/;
					break;
			}
		default:
			/*empty string – optional statements*/;
			break;
	}
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<logical AND expression> -> <relational expression>
<logical AND expression’>
First Set:			= { FIRST(<logical AND expression>)}
FIRST(<relational expression>),
FIRST(<primary a_relational expression>),
FIRST(primary s_relational expression>) }

= { AVID_T, FPL_T, INL_T, SVID_T, STR_T }

*******************************************************************************/
void logicalANDexpression()
{
	relationExpression();
	logicalANDexpression_p();
}


/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<logical AND expression’> ->.AND.<relational expression>
<logical AND expression’> | ϵ
First Set:			FIRST(<logical AND expression’>) = { .AND., ϵ }
*******************************************************************************/
void logicalANDexpression_p()
{
	/*First set: { .AND., ϵ }*/
	switch (lookahead.code)
	{
		case LOG_OP_T:
			/*look for AND and ϵ here*/
			switch (lookahead.attribute.get_int)
			{
				case AND:
					match(LOG_OP_T, lookahead.attribute.get_int);
					relationExpression();
					logicalANDexpression_p();
					gen_incode("PLATY: Logical AND expression parsed");
					break;
				default:
					/*empty string – optional statements*/;
					break;
			}
		default:
			/*empty string – optional statements*/;
			break;
	}
}


/*******************************************************************************
Authors:	Gurkirat Singh, Shubham Ujinwal
Grammer:	<relational expression> ->
<primary a_relational expression> <primary a_relational expression’>
| <primary s_relational expression> <primary s_relational expression’>

First Set:  = { FIRST(<relational expression>) }
FIRST(<primary a_relational expression>),
FIRST(primary s_relational expression>) }

= { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*******************************************************************************/
void relationExpression()
{
	/*First set: { AVID_T, FPL_T, INL_T, SVID_T, STR_T }*/
	switch (lookahead.code)
	{
		case AVID_T:
		case FPL_T:
		case INL_T:
			primaryArelationExp();
			primaryArelationExp_p();
			break;
		case SVID_T:
		case STR_T:
			primarySrelationExp();
			primarySrelationExp_p();
			break;
		default:
			syn_printe();
			break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<primary a_relational expression’> ->
== <primary a_relational expression>
| <> <primary a_relational expression>
| > <primary a_relational expression>
| < <primary a_relational expression>

First Set:			FIRST(<primary a_relational expression’>) = { ==, <>, >, < }
*******************************************************************************/
void primaryArelationExp_p()
{
	/*look for relational expression*/
	switch (lookahead.code)
	{
		case REL_OP_T:
			/*First set: { ==, <>, >, < }*/
			switch (lookahead.attribute.get_int)
			{
				case EQ:
				case NE:
				case LT:
				case GT: match(REL_OP_T, lookahead.attribute.get_int); primaryArelationExp(); break;
				default:
					syn_printe();
					break;
			}
		default:
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<primary a_relational expression> -> AVID_T| FPL_T| INL_T
First Set:			FIRST(<primary a_relational expression>) = { AVID_T, FPL_T, INL_T }
*******************************************************************************/
void primaryArelationExp()
{
	/*First set: AVID_T, FPL_T, INL_T }*/
	switch (lookahead.code)
	{
		case AVID_T:
		case FPL_T:
		case INL_T: match(lookahead.code, NO_ATTR);
			break;
		default:
			syn_printe();
			break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}



/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<primary s_relational expression’> ->
== <primary s_relational expression>
| <> <primary s_relational expression>
| > <primary s_relational expression>
| < <primary s_relational expression>

First Set:			FIRST(<primary s_relational expression’>) = { ==, <>, >, < }
*******************************************************************************/
void primarySrelationExp_p()
{
	/*look relational operaters*/
	switch (lookahead.code)
	{
		case REL_OP_T:
			/*First set: { ==, <>, >, < }*/
			switch (lookahead.attribute.get_int)
			{
				case EQ:
				case NE:
				case LT:
				case GT: match(REL_OP_T, lookahead.attribute.get_int); primarySrelationExp(); break;
				default:
					syn_printe();
					break;
			}
		default:
			break;
	}
}

/*******************************************************************************
Authors:			Gurkirat Singh, Shubham Ujinwal
Grammer:			<primary s_relational expression> -> <primary string expression>
First Set:			FIRST(<primary s_relational_expression>)
= { FIRST(<primary string expression> }

= { SVID_T, STR_T }
*******************************************************************************/
void primarySrelationExp()
{
	/*First set: { SVID_T, STR_T }*/
	switch (lookahead.code)
	{
		case STR_T:
		case SVID_T:
			primaryStringExp();
			gen_incode("PLATY: Primary s_relational expression parsed");
			break;
		default:
			syn_printe();
			break;
	}

}