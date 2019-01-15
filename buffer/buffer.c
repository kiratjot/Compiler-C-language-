
/****************************************************************************************************************
* File Name: buffer.c
* Compiler: MS Visual Studio 2015 Community
* Author: Gurkirat Singh, 040876315
* Course: CST 8152 – Compilers
* Lab Section: 12
* Assignment: 1
* Date: 3 October 2018
* Professor: Sv. Ranev
* Purpose: The purpose of the assignment is to implement a buffer that can operate in three different modes:
*          an “additive self-incrementing” buffer, and a “multiplicative self-incrementing” buffer.
*		   implementation is based on two associated data structures: a Buffer Descriptor (or Buffer Handle)
*		   and an array of characters. Both structures are to be created “on demand” at run time, that is, they
*		   are tobe allocated dynamically. Functions made are “overly” protected and not abruptly terminate or
*		   “crash” at run-time due to invalid function, parameters, erroneous internal calculations, or memory
*		   violations.
* Function list: b_allocate();
*				 b_addc();
*				 b_clear();
*			 	 b_free();
*			  	 b_isfull();
*				 b_limit();
*				 b_capacity();
*				 b_mark();
*				 b_mode();
*				 b_incfactor();
*				 b_load();
*				 b_isempty();
*				 b_getc();
*				 b_eob();
*				 b_print();
*				 b_compact();
*				 b_rflag();
*				 b_retract();
*				 b_reset();
*				 b_getcoffset();
*				 b_rewind();
*				 b_location();
****************************************************************************************************************/

#include "buffer.h"

/****************************************************************************************************************
* Purpose: Function creates a new buffer in memory (on the program heap). If there is no space, the function
*		   increases the buffer capacity as long as it is less than the Maximum capacity that can be allocated.
*		   On success, the function returns a pointer to the Buffer structure.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: Calloc();
*					Malloc();
*					b_free();
* Parameters: short init_capacity - Range: 0 to SHRT_MAX-1
*			  char inc_factor - Range: 1 to UCHAR_MAX
*			  char o_mode - Values: 'f', 'a', 'm'
* Return value: Buffer* (inBuffer), NULL
* Algorithm: Allocate memory for buffer. Then, the memory is allocated for the character array, only if the
*			 init_capacity is valid. If the init_capacity, inc inc_factor, and o_mode is valid, their values
*			 are copied to the respective Buffer structure variables and the flag is set accordingly. If the
*			 parameters are invalid, the function returns NULL, else returns a pointer to the Buffer Structure.
****************************************************************************************************************/

Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	/*Allocating memory for one Buffer structure*/
	Buffer * inBuffer = (Buffer*) calloc(1, sizeof(Buffer));

	/*Declaring Unsigned char to hold the -ve values of signed inc_factor */
	unsigned char inc_Cfactor = inc_factor;

	/*Checking if the capacity provided by the user is valid, if not, freeing the memory, return NULL*/
	if (init_capacity >= ZERO && init_capacity < SHRT_MAX)
	{
		/*Allocating the memory for character array*/
		inBuffer->cb_head = (char*) malloc(init_capacity);

		/*Checking if mode is fixed. If yes, assigning the value to mode and inc_factor variables of the buffer*/
		if ((o_mode == 'f' || inc_Cfactor == ZERO) || (o_mode == 'f' && inc_Cfactor != ZERO))
		{
			if (init_capacity > ZERO)
			{
				inBuffer->mode = 0;
				inBuffer->inc_factor = 0;
			}
			else
			{
				b_free(inBuffer);
				return NULL;
			}
		}

		/*Checking if the mode is Additive and assigning the value to mode and inc_factor*/
		else if (o_mode == 'a' && inc_Cfactor >= 1 && inc_Cfactor <= UCHAR_MAX)
		{
			inBuffer->mode = POSITIVE1;
			inBuffer->inc_factor = inc_Cfactor;
		}

		/*Checking if the mode is Multiplicative and assigning the value to mode and inc_factor*/
		else if (o_mode == 'm' && inc_Cfactor >= POSITIVE1 && inc_Cfactor <= HUNDRED)
		{
			inBuffer->mode = NEGATIVE1;
			inBuffer->inc_factor = inc_Cfactor;
		}
		/*freeing the memeory allocated to character array and returning NULL if the conditions fails*/
		else
		{
			b_free(inBuffer);
			return NULL;
		}
	}
	else
	{
		b_free(inBuffer);
		return NULL;
	}
	/*Assigning the init_capacity to the capacity variable of the buffer structure */
	inBuffer->capacity = init_capacity;

	/*Setting the flag value to Default*/
	inBuffer->flags = DEFAULT_FALGS;
	return inBuffer;
}

/****************************************************************************************************************
* Purpose: This functions adds the character/symbol to the character array. The function returns NULL if the
*		   operation is unsuccessful, else pointer to the Buffer structure gets return.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: b_isfull(), Realloc()
* Parameters: pBuffer, char
* Return Value: pBuffer pBD, NULL
* Algorithm: Using a bitwise operation the function resets the flags field r_flag bit to 0 and tries to add the
			 character symbol to the character array of the given buffer pointed by pBD. If the buffer is
			 operational and it is not full, the symbol can be stored in the character buffer. In this case, the
			 function adds the character to the content of the character buffer, increments addc_offset by 1 and
			 return. If the character buffer is already full, the function will try to resize the buffer by
			 increasing the current capacity to a new capacity.
****************************************************************************************************************/
pBuffer b_addc(pBuffer const  pBD, char symbol)
{
	/*Checking if buffer pointer is not NULL*/
	if (pBD == NULL)
		return NULL;

	pBD->flags &= RESET_R_FLAG;

	/*declaring a variable to hold the new capacity*/
	short nCapacity;

	/*declaring variable of type int. If success is 1, the capacity has been increased, memeory needs to be reallocated*/
	int success = 0;

	/*If buffer is not full, adding the character to it and returns the pointer to the buffer*/
	if (!b_isfull(pBD))
	{
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}
	/*if the buffer is full, increasing the capacity according to the given mode*/
	switch (pBD->mode)
	{
		case ZERO:  /*fixed mode*/
			return NULL; /*capacity cannot be inncreased in fixed mode*/
			break;

		case POSITIVE1: /*Additive mode*/
			 nCapacity = (unsigned char) pBD->inc_factor + pBD->capacity;

			 /* IF the result from the operation is positive and does not exceed the MAXIMUM ALLOWED POSITIVE VALUE –1
			 * it assigns the new capacity to the capacity*/
			if (nCapacity > ZERO && nCapacity < SHRT_MAX)
			{
				pBD->capacity = nCapacity;
				success = POSITIVE1;
			}
			/*If the result from the operation is positive but exceeds the MAXIMUM ALLOWED POSITIVE VALUE –1 (minus 1),
			* it assigns the MAXIMUM ALLOWED POSITIVE VALUE –1 to the new capacity*/
			else if (nCapacity > ZERO && nCapacity >= SHRT_MAX)
			{
				pBD->capacity = SHRT_MAX - POSITIVE1;
				success = POSITIVE1;
			}
			else return NULL;
			break;

		case NEGATIVE1: /*Multiplicative mode*/

			/*returning NULL if the capacity is already assigned the maximum size available*/
			if ((pBD->capacity) == (SHRT_MAX - POSITIVE1))
			{
				return NULL;
			}
			 /*Calculating the new capacity*/
			short  aSpace = ((SHRT_MAX - POSITIVE1) - pBD->capacity);
			short nIncrement = (short) (aSpace * ((unsigned char) pBD->inc_factor / FHUNDRED));
			 nCapacity = ((pBD->capacity) + nIncrement);

			 /*If the new capacity is +ve and is less than the maximum capacity possible,  the new capacity value
			 *   is assignedto the capacity of the buffer*/
			if (((pBD->capacity) < nCapacity) && (nCapacity <= (SHRT_MAX - POSITIVE1)))
			{
				pBD->capacity = nCapacity;
				success = POSITIVE1;
			}
			/*if the capacity cannot be increased but is less than the maximum capacity possible, the max possible
			*   capacity is assigned to the capacity variable*/
			else if ((pBD->capacity) < (SHRT_MAX - POSITIVE1))
			{
				pBD->capacity = SHRT_MAX - POSITIVE1;
				success = POSITIVE1;
			}
			else return NULL;
			break;

		default:
			return NULL;
			break;
	}

	/*Checking if the capacity increased and then reallocating the buffer with new capacity and setting the flag
	* to reflect if the address of the character array has been changed*/
	if (success != ZERO)
	{
		char * newCbHead = pBD->cb_head;
		if ((pBD->cb_head = realloc(pBD->cb_head, pBD->capacity)) != NULL)
		{
			if (newCbHead != pBD->cb_head)
			{
				pBD->flags |= SET_R_FLAG;
			}
			pBD->cb_head[pBD->addc_offset++] = symbol;
			return pBD;
		}
		else return NULL;
	}
	return NULL;
}

/****************************************************************************************************************
* Purpose: The function returns 1 if the character buffer is full; it returns 0 otherwise. If a run-time error
*		   is possible, the function should return –1. The function is only used if the MACRO B_FULL is not
*		   defined. Else, the MACRO defined in the buffer.h is used.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, POSITIVE1, ZERO
* Algorithm:
****************************************************************************************************************/
#ifndef B_FULL
int b_isfull(Buffer * const pBD)
{
	return (pBD == NULL) ? RT_FAIL_1 : ((pBD->addc_offset) == (pBD->capacity)) ? POSITIVE1 : ZERO;
}
#endif

/****************************************************************************************************************
* Purpose: The function retains the memory space currently allocated to the buffer, but re-initializes all
*		   appropriate data members of the given Buffer structure (buffer descriptor), such that the buffer will
*		   appear empty and the next call to b_addc() will put the character at the beginning of the character
*		   Function  return –1 in order to notify the calling function about the failure.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, POSITIVE1
* Algorithm:
****************************************************************************************************************/
int b_clear(Buffer * const pBD)
{
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}
	pBD->addc_offset = 0;
	pBD->markc_offset = 0;
	return POSITIVE1;
}

/****************************************************************************************************************
* Purpose: The function de-allocates (frees) the memory occupied by the character buffer and the Buffer
*		   structure (buffer descriptor).
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: free();
* Parameters: Buffer*
* Return Value:
* Algorithm:
****************************************************************************************************************/
void b_free(Buffer * const pBD)
{
	free(pBD->cb_head);
	free(pBD);
}


/****************************************************************************************************************
* Purpose: The function returns the current limit of the character buffer.
*		   If a run-time error is possible, the function should return –1.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, (unsigned short)(pBD->addc_offset)
* Algorithm:
****************************************************************************************************************/
short b_limit(Buffer * const pBD)
{
	return (pBD == NULL) ? RT_FAIL_1 : (unsigned short) (pBD->addc_offset);
}

/****************************************************************************************************************
* Purpose: The function returns the current capacity of the character buffer.
*		   If a run-time error is possible, the function should return –1.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, (short) pBD->capacity
* Algorithm:
****************************************************************************************************************/
short b_capacity(Buffer * const pBD)
{
	return (pBD == NULL) ? RT_FAIL_1 : pBD->capacity;
}

/****************************************************************************************************************
* Purpose: The function sets markc_offset to mark and returns the currently set markc_offset.
*		   If a run-time error is possible, the function return –1.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: pBuffer, short - Range: 0 to pBD->addc_offset
* Return Value: RT_FAIL_1, (short) pBD->markc_offset
* Algorithm:
****************************************************************************************************************/
short b_mark(pBuffer const pBD, short mark)
{
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}
	/*Checking if the mark is not greater than where the new  character has not been added yet*/
	else if (mark >= ZERO && mark <= pBD->addc_offset)
	{
		pBD->markc_offset = mark;
		return pBD->markc_offset;
	}
	return RT_FAIL_1;
}

/****************************************************************************************************************
* Purpose: The function returns the value of mode to the calling function.
*		   If a run-time error is possible, the function return –1.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, (char) (pBD->mode)
* Algorithm:
****************************************************************************************************************/
int b_mode(Buffer * const pBD)
{
	return (pBD == NULL) ? RT_FAIL_1 : (pBD->mode);
}

/****************************************************************************************************************
* Purpose: The function returns the non-negative value of inc_factor to the calling function. If a run-time error
*		   is possible, the function should return 0x100.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: 0x100, (unsigned char) pBD->inc_factor
* Algorithm:
****************************************************************************************************************/
size_t b_incfactor(Buffer * const pBD)
{
	return (pBD == NULL) ? 0x100 : (unsigned char) pBD->inc_factor;
}

/****************************************************************************************************************
* Purpose: The function loads (reads) an open input file specified by fi into a buffer specified by pBD. If a
*		   run time error is possible, the function should return -1.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: fgetc(), feof(), b_addc(), printf(), ungetc()
* Parameters: FILE* , Buffer *
* Return Value: RT_FAIL_1, LOAD_FAIL, count
* Algorithm: The function use the standard function fgetc(fi) to read one character at a time and the function
*			 b_addc() to add the character to the buffer. If the current character cannot be added to the buffer,
*			 the function returns the character to the file stream (file buffer) using ungetc() library function,
*            then prints the returned character both as a character and as an integer and then returns -2
****************************************************************************************************************/
int b_load(FILE * const fi, Buffer * const pBD)
{
	/*Checking if the parameters are not null*/
	if (pBD == NULL || fi == NULL)
	{
		return RT_FAIL_1;
	}

	/*Variable count to track the number of characters added to the buffer*/
	int count = 0;

	/*variable to hold the single character read from the file*/
	char x;

	/*Reading the file, one character at a time*/
	while (POSITIVE1)
	{

		x = fgetc(fi); /*reading a character from the file*/
		if (feof(fi)) /*cheking the end of file*/
		{
			break;
		}
		if (b_addc(pBD, x) == NULL) /*adding the character to the buffer and returning */
		{
			/*printing the last character if adding the character to the buffer fails*/
			printf("The last character read from the file is: %c %d\n", x, x);
			ungetc(x, fi); /*ungetting the failed character*/
			return LOAD_FAIL;
		}
		count++; /*incrementing the count*/
	}
	return count;
}

/****************************************************************************************************************
* Purpose: The function checks if the buffer is empty or not.
*		If the addc_offset is 0, the function returns 1; otherwise it returns 0. If a run-time error is possible,
*		it should return –1.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, POSITIVE1, ZERO
* Algorithm:
****************************************************************************************************************/
int b_isempty(Buffer * const pBD)
{
	return (pBD == NULL) ? RT_FAIL_1 : (!(pBD->addc_offset)) ? POSITIVE1 : ZERO;
}

/****************************************************************************************************************
* Purpose: This function is used to read the buffer.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_2, (char *)pBD->cb_head[pBD->getc_offset++],ZERO ;
* Algorithm:
****************************************************************************************************************/
char b_getc(Buffer * const pBD)
{
	if (pBD == NULL) /*checking the run time error*/
	{
		return RT_FAIL_2;
	}
	/*checking if we are not getting the character where the character has not been added yet*/
	if (pBD->getc_offset != pBD->addc_offset)
	{
		pBD->flags &= RESET_EOB; /*setting the flag to reflect it is not end of characters*/
		return pBD->cb_head[pBD->getc_offset++]; /*returning the character from the character array*/
	}
	pBD->flags |= SET_EOB; /*setting the flag to reflect end of character array*/
	return ZERO;
}


/****************************************************************************************************************
* Purpose: The function returns the eob bit value to the calling function
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, int (1 or 0) ;
* Algorithm: And operation is used to check if the last bit is 0 or 1
****************************************************************************************************************/
int b_eob(Buffer * const pBD)
{
	return (pBD == NULL) ? RT_FAIL_1 : (pBD->flags) & CHECK_EOB;
}

/****************************************************************************************************************
* Purpose: Function prints character by character the contents of the character buffer to the standard output
*          It returns the number of characters printed. The function returns –1 on failure.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: b_isempty(), printf(), b_getc(), b_eob()
* Parameters: Buffer*
* Return Value: RT_FAIL_1, int (count) ;
* Algorithm: Before printing the content the function checks if the buffer is empty, and if it is, it prints the
*            following message Empty buffer! adding a new line at the end and returns.Next, the function prints
*			 the content calling b_getc() in a loop and using b_eob() to detect the end of buffer content.
****************************************************************************************************************/
int b_print(Buffer * const pBD)
{
	if (pBD == NULL) /*checking the run time error*/
	{
		return RT_FAIL_1;
	}

	/*variable count to track the number of characters printed*/
	int count = 0;

	/*variable c to hold the character to be printed*/
	char c;
	if (b_isempty(pBD))/*checking if the buffer is empty*/
	{
		printf("Empty buffer!\n");
		return RT_FAIL_1;
	}

	/*iterating through the character array and getting the character*/
	do
	{
		c = b_getc(pBD); /*getting the character from the character buffer*/
		if (!b_eob(pBD)) /*checking if the last character from the character array has been called already*/
			printf("%c", c);

		count++;
	} while (!b_eob(pBD));
	printf("\n");

	return count;
}

/****************************************************************************************************************
* Purpose: The function uses realloc() to adjust the new capacity, and then updates all the necessary members of
*		   the buffer descriptor structure. The function returns NULL if the operation cannot be performed
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: realloc()
* Parameters: Buffer*, char
* Return Value: NULL, Buffer* (pBD) ;
* Algorithm: The function uses realloc() to adjust the new capacity. structure. Before returning a pointer
*			 to Buffer, the function adds the symbol to the end of the character buffer. It then sets the
*			 r_flag bit accordingly.
****************************************************************************************************************/
Buffer * b_compact(Buffer * const pBD, char symbol)
{
	if (pBD == NULL) /*checking the run time error*/
	{
		return NULL;
	}
	char *  temp = pBD->cb_head; /*creating a pointer to type char to hold the address of the character array*/
	pBD->flags &= RESET_R_FLAG;

	/*Reallocating the cb_head according to the new capacity. If realloc fails, it returns NULL*/
	if ((pBD->cb_head = realloc(pBD->cb_head, pBD->addc_offset + POSITIVE1)) != NULL)
	{
		/*setting the r_flag if the cb_head is pointing to a new location after the reallocation*/
		if (temp != pBD->cb_head)
		{
			pBD->flags |= SET_R_FLAG;
		}
		pBD->cb_head[pBD->addc_offset] = symbol; /*adding the character to the next space in the character array*/
		pBD->capacity = pBD->addc_offset + POSITIVE1; /*incrementing the capacity by 1*/
		pBD->addc_offset++; /*incrementing the addc_offset*/
		return pBD;

	}
	else return NULL;
}

/****************************************************************************************************************
* Purpose: The function returns the r_flag bit value to the calling function
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, char (1 or 0) ;
* Algorithm: And operation is used to check if the r_flag bit is 0 or 1. The and operator makes all other bits 0
*			 except the r_flag bit. So, if the r_flag bit is on, conversion of hexa to decimal will give 2
****************************************************************************************************************/
char b_rflag(Buffer * const pBD)
{
	if (pBD == NULL) /*checking the run time error*/
	{
		return RT_FAIL_1;
	}
	int temp = (pBD->flags & CHECK_R_FLAG);
	return (temp == '2') ? POSITIVE1 : ZERO;
}

/****************************************************************************************************************
* Purpose: The function decrements getc_offset by 1. If a run-time error is possible, it should return –1;
*		   otherwise it returns getc_offset.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: b_isempty()
* Parameters: Buffer*
* Return Value: RT_FAIL_1, short (pBD->getc_offset - POSITIVE1) ;
* Algorithm:
****************************************************************************************************************/
short b_retract(Buffer * const pBD)
{
	return(b_isempty(pBD) || pBD == NULL) ? RT_FAIL_1 : pBD->getc_offset - POSITIVE1;
}

/****************************************************************************************************************
* Purpose: The function sets getc_offset to the value of the current markc_offset . If a run-time error is
*		   possible, it should return –1; otherwise it returns getc_offset.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, short ( pBD->getc_offset) ;
* Algorithm:
****************************************************************************************************************/
short b_reset(Buffer * const pBD)
{
	if (pBD == NULL) /*checking the run time error*/
	{
		return RT_FAIL_1;
	}
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/****************************************************************************************************************
* Purpose: The function returns getc_offset to the calling function. If a run-time error is possible, it should
*		   return –1.
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions: b_isempty()
* Parameters: Buffer*
* Return Value: RT_FAIL_1, short ( pBD->getc_offset) ;
* Algorithm:
****************************************************************************************************************/
short b_getcoffset(Buffer * const pBD)
{
	return ((pBD == NULL) || (pBD->getc_offset == pBD->addc_offset)) ? RT_FAIL_1 : pBD->getc_offset;
}

/****************************************************************************************************************
* Purpose: The function set the getc_offset and markc_offset to 0, so that the buffer can be reread again. If
*		  a run-time error is possible, it should return –1; otherwise it returns 0;
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*
* Return Value: RT_FAIL_1, ZERO
* Algorithm:
****************************************************************************************************************/
int b_rewind(Buffer * const pBD)
{
	if (pBD == NULL) /*checking run time error*/
	{
		return RT_FAIL_1;
	}
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return ZERO;
}

/****************************************************************************************************************
* Purpose: The function returns a pointer to a location of the character buffer indicated by loc_offset.
*		  if a run-time error is possible, it should return NULL
* Author: Gurkirat Singh
* History/Versions: 1.0 - 3 October, 2018
* Called functions:
* Parameters: Buffer*  , short
* Return Value: NULL,  (loc)
* Algorithm:
****************************************************************************************************************/
char * b_location(Buffer * const pBD, short loc_offset)
{
	if (pBD == NULL) /*checking the run time error*/
	{
		return NULL;
	}
	char * loc = pBD->cb_head;

	/*creating variable count to iterate through the character array*/
	int count = POSITIVE1;

	while (count != loc_offset)
	{
		loc++;
		count++;
	}
	return loc;
}
