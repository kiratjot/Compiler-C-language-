#include "parser.h"


void parser(void)
{
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}


void match(int pr_token_code, int pr_token_attribute)
{

	//printf("%d %d,", pr_token_code, lookahead.code);
	if (pr_token_code != lookahead.code)
	{
		syn_eh(pr_token_code);
		return;
	}

	switch (pr_token_code)
	{
		case KW_T:
			//printf("asDAsdasDasd");
			if (pr_token_attribute != lookahead.attribute.kwt_idx)
			{
				syn_eh(pr_token_code);
				return;
			}
			break;
		case LOG_OP_T:
			if (pr_token_attribute != lookahead.attribute.log_op)
			{
				syn_eh(pr_token_code);
				return;
			}
			break;
		case ART_OP_T:
			if (pr_token_attribute != lookahead.attribute.arr_op)
			{
				syn_eh(pr_token_code);
				return;
			}
			break;
		case REL_OP_T:
			if (pr_token_attribute != lookahead.attribute.rel_op)
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
		case SEOF_T:
			return;
		default:
			lookahead = malar_next_token();

			if (lookahead.code == ERR_T)
			{
				syn_printe();
				lookahead = malar_next_token();
				synerrno++;
				return;
			}
	}
}



/* error printing function for Assignment 3 (Parser), F18 */
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


void syn_eh(int sync_token_code)
{
	syn_printe();
	synerrno++;

	while (lookahead.code != sync_token_code)
	{
		if (lookahead.code == SEOF_T)
		{
			exit(synerrno);
		}

		lookahead = malar_next_token();
	}
	if (sync_token_code == SEOF_T)
	{
		return;

	}
	lookahead = malar_next_token();
	return;


}

gen_incode(char *str)
{
	printf("%s\n", str);
}

void program(void)
{
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	optStatements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}
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

void statements()
{

	statement();
	statements_p();

}

void statement()
{


	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: assignmentStatement(); break;
		case KW_T:
			/* check for PLATYPUS, ELSE, THEN, REPEAT, TRUE, FALSE here */

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


		default: /*empty string – optional statements*/
			syn_printe();
			break;
	}

}
void statements_p()
{
	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T:
			statement();
			statements_p();
			break;

		case KW_T:
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

					break;
			}

		default:
			break;
	}
}




void assignmentStatement()
{
	assignmentExpression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

void assignmentExpression()
{

	//printf("\n ########  %d ####### \n", lookahead.code);

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

void iterationStatement()
{
	match(KW_T, WHILE);
	preCondition();

	//match(KW_T, TRUE);

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

void preCondition()
{


	switch (lookahead.code)
	{

		case KW_T:

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

void inputStatement()
{

	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variableList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

void variableList()
{

	variableIdentifier();
	variableList_p();
	gen_incode("PLATY: Variable list parsed");

}

void variableIdentifier()
{

	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: match(lookahead.code, NO_ATTR); break;
		default:
			syn_printe();
			break;
	}
}

void variableList_p()
{

	switch (lookahead.code)
	{
		case COM_T:
			match(COM_T, NO_ATTR); variableIdentifier(); variableList_p(); break;

		default:
			break;
	}
}

void outputStatement()
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	outputList();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");

}

void outputList()
{

	switch (lookahead.code)
	{
		case AVID_T:
		case SVID_T: variableList(); break;
		case STR_T: match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (string literal) parsed");
			break;
		default:
			gen_incode("PLATY: Output list (empty) parsed");
			break;
	}

}


void arithmeticExpression()
{
	switch (lookahead.code)
	{
		case ART_OP_T:
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

void unaryArithExp()
{//kjhvlkjvkbvlkbk

	switch (lookahead.code)
	{
		case ART_OP_T:
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
	default:
	break;
	}

	gen_incode("PLATY: Unary arithmetic expression parsed");

}

void primaryArithExp()
{

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
			break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");

}
void addArithExp()
{
	multiArithExp();
	addArithExp_p();
}

void addArithExp_p()
{


	switch (lookahead.code)
	{
		case ART_OP_T:
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

			break;
	}

}

void multiArithExp()
{

	primaryArithExp();
	multiArithExp_p();

}

void multiArithExp_p()
{//sgfsdfgdsfgsdfg
	switch (lookahead.code)
	{

		case ART_OP_T:
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
					break;
			}
		break;
		default:
			break;
	}

}


void stringExpression()
{

	primaryStringExp();

	stringExpression_p();
	gen_incode("PLATY: String expression parsed");

}

void primaryStringExp()
{

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
void stringExpression_p()
{

	switch (lookahead.code)
	{
		case SCC_OP_T:
			match(SCC_OP_T, NO_ATTR);
			primaryStringExp();
			stringExpression_p();
			break;
		default:
			break;
	}
	//gen_incode("PLATY: String expression parsed");

}

void conditionalExpression()
{

	logicalORexpression();

	gen_incode("PLATY: Conditional expression parsed");

}
void logicalORexpression()
{

	logicalANDexpression();
	logicalORexpression_p();

}

void logicalORexpression_p()
{
	switch (lookahead.code)
	{
		case LOG_OP_T:

			switch (lookahead.attribute.get_int)
			{
				case OR:
					match(LOG_OP_T, lookahead.attribute.get_int);
					logicalANDexpression();
					logicalORexpression_p();
					gen_incode("PLATY: Logical OR expression parsed");
					break;
				default:
					//	syn_printe();

					break;
			}


		default:
			break;
	}
}

void logicalANDexpression()
{
	relationExpression();
	logicalANDexpression_p();


}

void logicalANDexpression_p()
{

	switch (lookahead.code)
	{
		case LOG_OP_T:

			switch (lookahead.attribute.get_int)
			{
				case AND:
					match(LOG_OP_T, lookahead.attribute.get_int);
					relationExpression();
					logicalANDexpression_p();
					gen_incode("PLATY: Logical AND expression parsed");
					break;
				default:
					//	syn_printe();
					break;
			}


		default:
			break;
	}
}


void relationExpression()
{
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


void primaryArelationExp_p()
{

	switch (lookahead.code)
	{
		case REL_OP_T:
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
		//	syn_printe();

			break;
	}

}
void primaryArelationExp()
{
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




void primarySrelationExp_p()
{

	switch (lookahead.code)
	{
		case REL_OP_T:
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
		//	syn_printe();

			break;
	}

}
void primarySrelationExp()
{
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

