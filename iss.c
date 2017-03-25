/* ============================================================================
*							SP Simulator (iss.c)
*			Or Perel 								Mordehay Zeitler
* ============================================================================
*/

/*	-- Assumptions:
 *	- Instructions that don't use one of the fields for their functionality can put any value in the field bits (even illegal values),
 *	  they'll be silently ignored.
 *	- When dst field is unused, we don't enforce usage of R0 but any value (even illegal ones) may be filled for the dst field.
 *	- For trace output - instruction count in hex is trimmed to 4 digits.
 */

#include <stdio.h>
#include <stdlib.h>


// Constants

static const int NUM_OF_REGS = 8;	// There are actually 6 physical registers in the hardware (R2-R7),
									// but we always store 0 inside R0 (for convenience), and create R1 but never store in it
									// (for alignment purposes - addressing registers fast by their number).
static const int MEM_SIZE = 65536;	// In bytes (2^16)
static const int INSTRUCTION_LENGTH_IN_BYTES = 8; // 32 bits
static const int REG0 = 0x0;	// Index for REG0
static const int REG1 = 0x1;	// Index for REG1
static const int REG7 = 0x7;	// Index for REG7

// Error codes
static const int ERROR_CODE_OK = 0x0; // Prompted when successful
static const int ERROR_CODE_INVALID_INPUT = 0x1; // Prompted when invalid args are passed to the program
static const int ERROR_CODE_INVALID_REG = 0x2; // Prompted when an inavlid register number is being accessed
static const int ERROR_CODE_WR_REG0 = 0x3; // Prompted when trying to write to REG0 which is a read only constant
static const int ERROR_CODE_WR_REG1 = 0x4; // Prompted when trying to write to REG1 which is a read only constant
static const int ERROR_CODE_INVALID_OPCODE = 0x5; // Prompted when an invalid opcode is used
static const int ERROR_CODE_INVALID_MEMORY_ADDRESS = 0x6; // Prompted when an address outside of the memory address space is accessed
static const int ERROR_CODE_IO_EXCEPTION = 0x7; // Prompted when an IO exception have occured
static const int ERROR_CODE_MEM_ALLOC = 0x8; // Prompted when the simulation fails to allocate memory

// I/O constants
static char* TRACE_PATH = "trace.txt";
static char* MEMORY_OUT_PATH = "sram_out.txt";


// Type defs

/* Error code for symbolizing errors in the simulation's code */
typedef int ErrorCode;

/* Enum for the possible opcodes supported by the cpu simulator, and their codes*/
typedef enum
{
	ADD = 0x00,
	SUB = 0x01,
	LSF = 0x02,
	RSF = 0x03,
	AND = 0x04,
	OR = 0x05,
	XOR = 0x06,
	LHI = 0x07,
	LD = 0x08,
	ST = 0x09,
	JLT = 0x10,
	JLE = 0x11,
	JEQ = 0x12,
	JNE = 0x13,
	JIN = 0x14,
	HLT = 0x18
} OPCODE;

/* A struct for keeping all the decoded information of a single instruction */
typedef struct
{
	int opcode;
	int dst;
	int src0;
	int src1;
	int immediate;

} instruction_decode;


// Functions

/*	 
 *	Output: Prints the contents of the registers to the traceFile.
 *			Returns ERROR_CODE_OK if successful, or an error code if an error have occured.
 */
ErrorCode printRegs(FILE* traceFile, int* regs)
{
	int i;
	for (i = 0; i < NUM_OF_REGS; i++)
	{
		fprintf(traceFile, "r[%d] = %08x ", i, regs[i]);
		if (ferror(traceFile))
			return ERROR_CODE_IO_EXCEPTION;

		if (i % 4 == 3)
		{
			fprintf(traceFile, "\n");
			if (ferror(traceFile))
				return ERROR_CODE_IO_EXCEPTION;
		}
	}

	return ERROR_CODE_OK;
}

/*	Creates a string describing the instructions results (a logical description).
 *	Input: decoded_data - Decoded data of the instruction.
 *		   regs - The registers file (containing the contents of each register)
 *		   mem - The main memory.
 *	Output:	exString - A buffer to be filled with the execution string. 
 */
void createExecutionString(instruction_decode* decoded_data, int* regs, int* mem, char* exString)
{
	int immd = decoded_data->immediate;
	int src0 = (decoded_data->src0 == REG1) ? immd : regs[decoded_data->src0]; // src0 contents
	int src1 = (decoded_data->src1 == REG1) ? immd : regs[decoded_data->src1]; // src1 contents
	int dst = decoded_data->dst; // dst index

	switch (decoded_data->opcode)
	{
		case (ADD) : {sprintf(exString, "R[%u] = %d ADD %d", dst, src0, src1); break; };
		case (SUB) : {sprintf(exString, "R[%u] = %d SUB %d", dst, src0, src1); break; };
		case (LSF) : {sprintf(exString, "R[%u] = %d LSF %d", dst, src0, src1); break; };
		case (RSF) : {sprintf(exString, "R[%u] = %d RSF %d", dst, src0, src1); break; };
		case (AND) : {sprintf(exString, "R[%u] = %08x AND %08x", dst, src0, src1); break; };
		case (OR) : {sprintf(exString, "R[%u] = %08x OR %08x", dst, src0, src1); break; };
		case (XOR) : {sprintf(exString, "R[%u] = %08x XOR %08x", dst, src0, src1); break; };
		case (LHI) : {sprintf(exString, "R[%u] = %04x CONCAT %08x[15:0]", dst, immd, regs[dst]); break; };
		case (LD) : {sprintf(exString, "R[%u] = MEM[%d]", dst, src1); break; };
		case (ST) : {sprintf(exString, "MEM[%d] = %d", src1, src0); break; };
		case (JLT) : {sprintf(exString, "if %d < %d then R[7] = PC, PC = %d", src0, src1, immd); break; };
		case (JLE) : {sprintf(exString, "if %d <= %d then R[7] = PC, PC = %d", src0, src1, immd); break; };
		case(JEQ) : {sprintf(exString, "if %d == %d then R[7] = PC, PC = %d", src0, src1, immd); break; };
		case (JNE) : {sprintf(exString, "if %d != %d then R[7] = PC, PC = %d", src0, src1, immd); break; };
		case (JIN) : {sprintf(exString, "R[7] = PC, PC = %d", src0); break; };
		case (HLT) : {sprintf(exString, "End simulation"); break; };

		default: {sprintf(exString, "Unknown opcode"); break; }; // Unknown opcode - this should never happen
	}
}

/*	Input: opcode (as id)
 *	Output: Returns the name of the opcode according to the opcode id.
 */
char* getCommandEnumString(OPCODE op)
{
	switch (op)
	{
		case (ADD) : {return "ADD"; break; };
		case (SUB) : {return "SUB"; break; };
		case (LSF) : {return "LSF"; break; };
		case (RSF) : {return "RSF"; break; };
		case (AND) : {return "AND"; break; };
		case (OR) : {return "OR"; break; };
		case (XOR) : {return "XOR"; break; };
		case (LHI) : {return "LHI"; break; };
		case (LD) : {return "LD"; break; };
		case (ST) : {return "ST"; break; };
		case (JLT) : {return "JLT"; break; };
		case (JLE) : {return "JLE"; break; };
		case(JEQ) : {return "JEQ"; break; };
		case (JNE) : {return "JNE"; break; };
		case (JIN) : {return "JIN"; break; };
		case (HLT) : {return "HLT"; break; };

		default: { return "Unknown"; /* Unknown opcode - do nothing (this should never happen)*/ };
	}
}

/*	Dumps execution information about a single instruction.
 *	Input: traceFile - a pointer to the traceFile to contain the output.
 *		   decoded_data - Decoded instruction data.
 *		   regs - The register file's contents.
 *		   mem - Pointer to main memory of simulated cpu.
 *		   pc - The contents of the PC register.
 *		   instruction - The raw instruction data before decoding (32 bits)
 *		   instructionCnt - Sequential number of instruction executed.
 */
ErrorCode dump_instruction(FILE* traceFile, instruction_decode* decoded_data, int* regs, int* mem, int pc, int instruction, int instructionCnt)
{
	fprintf(traceFile, "--- instruction %u (%04x) @ PC %u (%04x) -----------------------------------------------------------\n",
		instructionCnt,
		instructionCnt,
		pc,
		pc);
	if (ferror(traceFile))
		return ERROR_CODE_IO_EXCEPTION;

	fprintf(traceFile, "pc = %04x, inst = %08x, opcode = %u (%s), dst = %u, src0 = %u, src1 = %u, immediate = %08x\n",
			pc,
			instruction,
			decoded_data->opcode,
			getCommandEnumString((OPCODE)decoded_data->opcode),
			decoded_data->dst,
			decoded_data->src0,
			decoded_data->src1,
			decoded_data->immediate);
	if (ferror(traceFile))
		return ERROR_CODE_IO_EXCEPTION;

	ErrorCode errNo = printRegs(traceFile, regs);
	if (ERROR_CODE_OK != errNo)
		return errNo;

	fprintf(traceFile, "\n");
	if (ferror(traceFile))
		return ERROR_CODE_IO_EXCEPTION;
	
	char executionStringBuffer[1024];
	createExecutionString(decoded_data, regs, mem, executionStringBuffer);
	fprintf(traceFile, ">>>> EXEC: %s <<<<\n\n", executionStringBuffer);
	if (ferror(traceFile))
		return ERROR_CODE_IO_EXCEPTION;

	return ERROR_CODE_OK;
}

/*
 * Prints the error details of the error code given to the log.
 * This function is used to report errors that occur during the validation process.
 * Input: code - Error code number.
 *		  param0 - Additional parameter with data about the error. The purpose of this variable may change
 *				   for different errors.
 */
void report_error(ErrorCode code, unsigned int param0)
{
	// Print log by error type
	if (code == ERROR_CODE_INVALID_REG)
		printf("Simulation failed due to trying to access an illegal register value: %u\n", param0); // Print register number as unsigned num
	else if (code == ERROR_CODE_WR_REG0)
		printf("Simulation failed due to trying to write to register 0 (read only protected)\n");
	else if (code == ERROR_CODE_WR_REG1)
		printf("Simulation failed due to trying to write to register 1 (read only protected)\n");
	else if (code == ERROR_CODE_INVALID_OPCODE)
		printf("Simulation failed due to an invalid instruction opcode %u\n", param0); // Print opcode as unsigned num
	else if (code == ERROR_CODE_INVALID_MEMORY_ADDRESS)
		printf("Simulation failed due to an attempt to access an invalid memory address %u\n", param0); // Print address as unsigned num
	else
		printf("Simulation failed due to a general error\n");
}

/*
 *  Validates a register name is within the accepted range.
 *  Input: register_index - integer between 0-7 of a register number to update.
 *		   writeAccess - 1 if the register is being written, 0 if the register is being read.
 *	Output: Returns ERROR_CODE_INVALID_REG if the register_index is an invalid value,
 *			ERROR_CODE_WR_REG0 if trying to write to REG0,
 *			ERROR_CODE_OK if this register is available in the registers file.
 */
ErrorCode validate_reg(unsigned int register_index, int writeAccess)
{
	// Illegal register number
	if ((register_index > NUM_OF_REGS - 1))
		return ERROR_CODE_INVALID_REG;
	else if ((register_index == REG0) && (writeAccess == 1))
		return ERROR_CODE_WR_REG0; // Trying to write to register 0
	else if ((register_index == REG1) && (writeAccess == 1))
		return ERROR_CODE_WR_REG1; // dst register cannot be R1

	return ERROR_CODE_OK;
}

/*
 *  Validates the decoded instruction is legal SP ASM syntax and can be safely executed.
 *  Input: decoded_data - All decoded information about the instruction.
 *		   regs - The registers file of the CPU.
 *	Output: ERROR_CODE_OK if the instruction is valid, or other error codes in specific error cases.
 */
ErrorCode validate_decode(instruction_decode* decoded_data, int* regs)
{
	int result = ERROR_CODE_OK;

	int opcode = decoded_data->opcode;

	// Validate parameters only for legal opcodes.
	// Note: Since C enums don't allow a convenient way of accessing all values of an enum,
	//		 and since the enum values are not always sequential we had no choice but to compare them one by one.
	if ((opcode == ADD) || (opcode == SUB) || (opcode == LSF) || (opcode == RSF) || (opcode == AND) ||
		(opcode == OR) || (opcode == XOR))
	{
		// Validate register number accessed is legal
		if ((result = validate_reg(decoded_data->src0, 0)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using src0 register\n");
			report_error(result, decoded_data->src0);
		}
		else if ((result = validate_reg(decoded_data->src1, 0)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using src1 register\n");
			report_error(result, decoded_data->src1);
		}
		else if ((result = validate_reg(decoded_data->dst, 1)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using dst register\n");
			report_error(result, decoded_data->dst);
		}

		return result;
	}
	else if ((opcode == LHI))
	{
		if ((result = validate_reg(decoded_data->dst, 1)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using dst register\n");
			report_error(result, decoded_data->dst);
		}
		return result;
	}
	else if (opcode == LD || opcode == ST)
	{
		if ((result = validate_reg(decoded_data->src1, 0)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using src1 register\n");
			report_error(result, decoded_data->src1);
			return result;
		}

		// Avoid addresses that don't belong to the main memory address space
		unsigned int address = (unsigned int)(decoded_data->src1 != REG1 ? regs[decoded_data->src1] : decoded_data->immediate);
		if (address >(MEM_SIZE - 1))
		{
			report_error(result, address);
			result = ERROR_CODE_INVALID_MEMORY_ADDRESS;
			return result;
		}

		if (opcode == LD)
		{
			if ((result = validate_reg(decoded_data->dst, 1)) != ERROR_CODE_OK)
			{
				printf("Error was detected in using dst register\n");
				report_error(result, decoded_data->dst);
			}
		}
		else if (opcode == ST)
		{
			if ((result = validate_reg(decoded_data->src0, 0)) != ERROR_CODE_OK)
			{
				printf("Error was detected in using src0 register\n");
				report_error(result, decoded_data->src0);
			}
		}

		return result;
	}
	else if ((opcode == JLT) || (opcode == JLE) || (opcode == JEQ) || (opcode == JNE))
	{
		// Validate register number accessed is legal
		if ((result = validate_reg(decoded_data->src0, 0)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using src0 register\n");
			report_error(result, decoded_data->src0);
		}
		else if ((result = validate_reg(decoded_data->src1, 0)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using src1 register\n");
			report_error(result, decoded_data->src1);
		}
		return result;
	}
	else if ((opcode == JIN))
	{
		// Validate register number accessed is legal
		if ((result = validate_reg(decoded_data->src0, 0)) != ERROR_CODE_OK)
		{
			printf("Error was detected in using src0 register\n");
			report_error(result, decoded_data->src0);
		}
		return result;
	}
	else if (opcode == HLT)
	{
		return result;
	}

	// Else - invalid opcode
	report_error(ERROR_CODE_INVALID_OPCODE, opcode);
	return ERROR_CODE_INVALID_OPCODE;
}

/*
 *	Executes the jump logic common to all jump instructions.
 *	Input:  destination - A field containing the address to jump to
 *			pc - The original pc before jumping.
 *  Output: regs - Registers file to update REG7 with the original pc.
 *			pc - The pc gets updated with the new address.
 *			advancePC - always becomes 0 (because pc is already being taken care of by this routine)
 *
 */
void executeJumpLogic(int* regs, unsigned short* pc, int destination, int* advancePC)
{
	regs[REG7] = *pc;
	*pc = destination;
	*advancePC = 0;
}

/* Executes the required instruction and stores it within the destination register (writeback).
 * If memory access is required, it's also performed by this logic (mem stage).
 * The pc would be advanced in this routine according to the opcode executed, and if the program
 * should halt it would be also be decided here.
 * Note: this function assumes all reg & memory parameters are valid.
 * Input: The decoded opcode data (source, destination registers + opcode + immediate field).
 * Output: The register file / memory / pc is initialized with the opcode's results.
 *	   The next pc value is also advanced (when a non-jump opcode is executed).
 *	   The function returns if the simulation should halt or not (opcode was halt or not).
 */
int execute(instruction_decode* decoded_data, int* regs, int* mem, unsigned short* pc)
{
	// src0, src1 will contain the input *values* the opcode will execute
	// dst will contain the result

	// If src = R1 we take the immediate field
	// If src = R0 we load 0 instead (we simply load R0's contents because it always contains 0)
	// Immediate is already sign extended (assume it have occured during decode)
	int immd = decoded_data->immediate;
	int src0 = (decoded_data->src0 == REG1) ? immd : regs[decoded_data->src0];
	int src1 = (decoded_data->src1 == REG1) ? immd : regs[decoded_data->src1];

	int dst = regs[decoded_data->dst];	// Will contain the result of execution

	int keepRunning = 1;	// 1 - Should the simulation end after this execution (0 when not)
	int advancePC = 1;		// The offset of the next address we'll fetch

	// Perform logic according to opcode type
	switch (decoded_data->opcode)
	{
		case (ADD) : {dst = src0 + src1; break; };
		case (SUB) : {dst = src0 - src1; break; };
		case (LSF) : {dst = src0 << src1; break; };
		case (RSF) : {dst = src0 >> src1; break; };
		case (AND) : {dst = src0 & src1; break; };
		case (OR) : {dst = src0 | src1; break; };
		case (XOR) : {dst = src0 ^ src1; break; };
		case (LHI) : {dst = (dst & 0x0000FFFF) | (immd << 16); break; };
		case (LD) : {dst = mem[src1 % MEM_SIZE]; break; }; // What about R1 in src1
		case (ST) : {mem[src1 % MEM_SIZE] = src0; break; }; // SAME and all under
		case (JLT) :
		{
			if (src0 < src1)
				executeJumpLogic(regs, pc, immd & 0x0000FFFF /* Jump to immediate[15:0] */, &advancePC);
			break;
		};
		case (JLE) :
		{
			if (src0 <= src1)
				executeJumpLogic(regs, pc, immd & 0x0000FFFF /* Jump to immediate[15:0] */, &advancePC);
			break;
		};
		case (JEQ) :
		{
			if (src0 == src1)
				executeJumpLogic(regs, pc, immd & 0x0000FFFF /* Jump to immediate[15:0] */, &advancePC);
			break;
		};
		case (JNE) :
		{
			if (src0 != src1)
				executeJumpLogic(regs, pc, immd & 0x0000FFFF /* Jump to immediate[15:0] */, &advancePC);
			break;
		};
		case (JIN) :
		{
			executeJumpLogic(regs, pc, src0, &advancePC);
			break;
		};
		case (HLT) : {keepRunning = 0; break; };

		default: { printf("Simulation failed due to Unknown Opcode\n"); return 0; /* Unknown opcode - do nothing */ };
	}

	// If PC should be advanced to the next address, we do it here
	// pc is an unsigned short so it should advance in a cyclic manner anyway when pc
	// advances beyond MEM_SIZE. As a practice of good design & readable code
	// we add the modulo to protect against MEM overflow
	*pc = (*pc + advancePC) % MEM_SIZE;

	// Writeback step - update register file
	// Note: For some instructions that don't perform writeback (e.g: ST, Jumps, etc)
	// we re-write the original value of dst, so essentially no real writeback is performed
	// (for the simulatior - this is equal to checking whether writeback should happen and only if so, do it).
	regs[decoded_data->dst] = dst;

	return keepRunning;
}

/*
 *	Sign extends the 16 bit word into 32 bits word.
 *	Input: 16 bit word.
 *	Output: word converted to a 32 bit word, sign extended.
 */
int signExtend(short word) {

	int extension = (0x0000FFFF & word);
	int mask = 0x00008000;
	if (mask & word)
	{
		extension += 0xFFFF0000;
	}
	return extension;
}

/* Decodes the next instruction.
 * Input: opcode as 32 bits integer.
 * Output: decoded_data is initialized with decoded information.
 *		   Immediate field is also sign extended during decode.
 */
void decode(int instruction, instruction_decode* decoded_data)
{
	// The Instructions structure is:
	// |31, 30] [29, 28, 27, 26, 25] [24, 23, 22] [21, 20, 19] [18, 17, 16] [15, 14 .. 01, 00]
	//   < 0 >  <----- opcode -----> <--- dst --> <-- src0 --> <-- dst ---> <-- immediate --->
	
	// Sign extend immediate field here
	short immediate_short = instruction & 0x0000FFFF;
	decoded_data->immediate = signExtend(immediate_short);
	instruction >>= 16;
	decoded_data->src1 = instruction & 0x0000007;
	instruction >>= 3;
	decoded_data->src0 = instruction & 0x0000007;
	instruction >>= 3;
	decoded_data->dst = instruction & 0x0000007;
	instruction >>= 3;
	decoded_data->opcode = instruction & 0x000001F;
}

/*
 * Runs and executes the simulator on the contents of mem.
 * mem is expected to contain both instructions and possible some predefined memory state.
 * The simulator will execute the instructions until HALT is encountered.
 * If PC reaches the end of the mem boundary, it will advance in a cyclic manner.
 * Input: regs - The registers file, each register is accessed by index, and contains a 32 bit value.
 *		  mem - The contents of the main memory in the beginning of the simulation.
 *				mem contains both code and data segments.
 *		  traceFile - A pointer to the trace file that will be filled with the simulation description.
 */
void run_simulator(int* regs, int* mem, FILE* traceFile)
{
	printf("Starting simulation..\n");
	instruction_decode inst_decode; // Will hold decoded instructions information (a reused struct)
	unsigned short pc = 0;	// Execution starts at pc 0
	int isRunning = 1;		// Control variable for execution loop. When isRunning = 0, we halt
	int instructionsCnt = 0; // Number of instructions executed

	// While halt is not encountered
	while (isRunning)
	{
		int instruction = mem[pc]; // Fetch the next instruction
		decode(instruction, &inst_decode); // Decode the next instruction
		ErrorCode code = validate_decode(&inst_decode, regs); // Validate the next instruction's decoded params

		if (code != ERROR_CODE_OK)
		{ // Validation failed, quit
			printf("Error was detected in PC: %u\nTerminating simulation..", pc);
			isRunning = 0;
		}
		else
		{ // Instruction's fields are legal -- execute
			ErrorCode errNo = dump_instruction(traceFile, &inst_decode, regs, mem, pc, instruction, instructionsCnt); // Dump instructions data before executing
			if (ERROR_CODE_OK != errNo)
			{
				printf("Simulation failed due to an IO error while dumping trace file.");
				break;
			}

			isRunning = execute(&inst_decode, regs, mem, &pc);
			instructionsCnt++;
		}
	}

	printf("Simulation ended successfully. %d commands were executed..\n", instructionsCnt);
}

/*
 * Reads the input memory file and initializes the simulator's memory buffer with it's content.
 * Assumption: If the input memory file is bigger than MEM_SIZE we load only the first MEM_SIZE bytes and ignore the rest.
 * Input: inputPath - the path to the memory contents file on disk.
 * Output: mem - A pointer to the pre-allocated memory buffer in the size of MEM_SIZE.
 *				 mem will be initialized with inputPath's contents (any additional mem cells will
 *				 remain unaffected by this function).
 *		   On success ERROR_CODE_OK is returned. Otherwise an error return code is returned.
 */
ErrorCode loadMemory(char* inputPath, int* mem)
{
	printf("Loading memory from %s..\n", inputPath);
	FILE *fp;
	char line[INSTRUCTION_LENGTH_IN_BYTES + 1];

	fp = fopen(inputPath, "r+");
	if (NULL == fp)
		return ERROR_CODE_IO_EXCEPTION;

	int i = 0;

	// Read until we reach EOF or maximum memory size
	while (fgets(line, INSTRUCTION_LENGTH_IN_BYTES + 1, fp) != NULL && i < MEM_SIZE)
	{
		fgetc(fp);
		if (ferror(fp))
		{
			printf("IO Error while reading from memory input file: %s\n", inputPath);
			fclose(fp);
			return ERROR_CODE_IO_EXCEPTION;
		}

		unsigned int number = 0;
		sscanf(line, "%x", &number);
		mem[i] = number;
		i++;
	}

	if (ferror(fp))
	{
		printf("IO Error while reading from memory input file: %s\n", inputPath);
		fclose(fp);
		return ERROR_CODE_IO_EXCEPTION;
	}

	fclose(fp);
	printf("Finished loading memory. %d mem rows were loaded..\n", i);
	return ERROR_CODE_OK;
}

/*
 *	Input: A pointer to the main memory.
 *	Output: Dumps the entire content of the main memory to an output file
 *			Returns ERROR_CODE_OK on success, or an error code on failure.
 */
ErrorCode dumpMem(int* mem)
{
	FILE *fp;
	fp = fopen(MEMORY_OUT_PATH, "w+");
	if (NULL == fp)
		return ERROR_CODE_IO_EXCEPTION;

	int i;
	for (i = 0; i < MEM_SIZE - 1; i++)
	{
		fprintf(fp, "%08x\n", mem[i]);

		if (ferror(fp))
		{
			fclose(fp);
			return ERROR_CODE_IO_EXCEPTION;
		}
	}

	fclose(fp);
	printf("Memory dumped at: %s\n", MEMORY_OUT_PATH);
	return ERROR_CODE_OK;
}

/* 
 *	Creates & opens the trace output file for writing.
 *	Output: On success trace file will be created and opened for writing, and the pointer will be returned.
 *			On error - NULL is returned.
 */
FILE* openTraceFileForWriting()
{
	FILE* traceFile = fopen(TRACE_PATH, "w+");

	if (NULL == traceFile)
		printf("Failed to open trace file [%s] for writing due to an IO error (please check directory permissions).", TRACE_PATH);

	return traceFile;
}

/*
 *	Closes the trace file for writing, releasing all IO resources that belong to it.
 */
void closeTraceFileForWriting(FILE* traceFile)
{
	fclose(traceFile);
	printf("Trace file available at: %s\n", TRACE_PATH);
}

/*
 * Runs the SP architecture CPU simulator.
 * Input: argv contains an input file of the memory dump in the beginning of simulation.
 * Output: Output file of the memory file and a trace file of the instructions executed.
 */
int main(int argc, char *argv[])
{
	if (argc != 2)
	{
		printf("Incorrect parameters. Number of parameters is %u.\n Usage: iss <mem_path>\n", argc);
		return(ERROR_CODE_INVALID_INPUT);
	}

	char* path = argv[1];

	// -- Simulation initialization
	// Allocate space for simulator attributes (such as memory, registers..)
	int* regs = (int*)calloc(NUM_OF_REGS, sizeof(int));
	int* mem = (int*)calloc(MEM_SIZE, sizeof(int));
	if ((NULL == regs) || (NULL == mem))
	{
		printf("Failed to allocated memory for simulation. Terminating simulation..");
		return ERROR_CODE_MEM_ALLOC;
	}

	ErrorCode errNo = loadMemory(path, mem);
	if (ERROR_CODE_OK != errNo)
	{
		free(regs);
		free(mem);
		return errNo;
	}

	FILE* traceFile = openTraceFileForWriting();
	if (NULL == traceFile)
	{
		free(regs);
		free(mem);
		return ERROR_CODE_IO_EXCEPTION;
	}

	//  -- Simulator core code --
	run_simulator(regs, mem, traceFile);

	// -- Dump simulation results, cleanup resources
	closeTraceFileForWriting(traceFile);
	errNo = dumpMem(mem);
	if (ERROR_CODE_OK != errNo)
	{
		printf("Simulation ended normally, but failed to dump main memory to output file due to an IO error.");
		free(regs);
		free(mem);
		return ERROR_CODE_IO_EXCEPTION;
	}

	// Free allocated space
	free(regs);
	free(mem);
	printf("End of ISS - SP Terminating gracefully..\n");

	return ERROR_CODE_OK;
}
