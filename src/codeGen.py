import copy

fileName = "TAC/test1.txt"
file = open(fileName,"r")
code = file.readlines()

class CodeGenerator:
    
    def __init__(self):
        # contains all the registers including %ebp and %esp 
        self.all_registers = ["%ebx","%ecx","%esi","%edi","%edx","%eax","%ebp","%esp"]
        
        # general purpose registers -> register_stack contains all the free registers
        self.register_stack = ["%ebx","%eax","%ecx","%esi","%edi","%edx"]
        
        #  register_list contains all the registers that can be used(not necessarily free)
        self.register_list = copy.deepcopy(self.register_stack)
        self.final_code = []
        self.final_code.append(".data")
        self.final_code.append(".text")
        self.final_code.append(".globl main")
        self.final_code.append(".type main, @function") 
        self.final_code.append("\n")

    def emit_code(self, s1 = '', s2 = '', s3 = ''):
        self.final_code.append(s1 + " " + s2 + ", " + s3)

    def request_register(self, reg=None):
        if not self.register_stack:
            print("ERROR! No register available")
            return

        if reg is not None:
            if reg not in self.register_stack:
                swapreg = self.request_register()
                if swapreg:
                    self.final_code.append("movl " + reg + " " + swapreg)
                    return reg
            else:
                self.register_stack.remove(reg)
                return reg
            return 

        register = self.register_stack.pop()
        return register

    def free_register(self, register, start=None):
        if(register == None):
            return
        if (register not in self.register_list):
            # Do not free register if it is %ebp and %esp
            return
        
        if start is not None:
            self.register_stack.insert(0, register)
        else:
            self.register_stack.append(register)
    
    def move_var(self,src,dst):
        # Move in case more than one pair of ()
        if src == dst:
            return
        self.emit_code("movl",src,dst)
    
    def check_type(self,instruction, req_reg1=None, req_reg2=None):
        '''
        This function moves all the values into appropriate registers
        '''
        op = instruction[0]
        dest = instruction[1]
        src1 = instruction[2]
        src2 = ''
        reg1 = self.request_register(req_reg1)
        if not reg1:
            return False

        reg2 = None
        self.move_var(src1,reg1)
        instruction[2] = reg1
        if(len(instruction) > 3):
            src2 = instruction[3]
            reg2 = self.request_register(req_reg2)
            if not reg2:
                return False
            self.move_var(src2,reg2)
            instruction[3] = reg2

        return True    
    
    def op_add(self,instruction):
        '''
        This function is currently only implemented
        for integer addition
        '''
        self.check_type(instruction)
        # self.final_code.append("addl" + " " + instruction[2] + ", " + instruction[3])
        self.emit_code("addl",instruction[2],instruction[3])
        self.emit_code("movl",instruction[3],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3])

    def op_sub(self,instruction):
        '''
        This function is currently only implemented
        for integer
        '''
        self.check_type(instruction)
        self.emit_code("subl",instruction[3],instruction[2])
        self.emit_code("movl",instruction[2],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3])

    def op_eq(self,instruction):
        '''
        This function is currently only implemented
        for integer
        '''
        self.check_type(instruction)
        self.emit_code("movl",instruction[2],instruction[1])
        self.free_register(instruction[2])
    
    def op_mul(self,instruction):
        '''
        This function is currently only implemented
        for integer multiplication
        '''
        self.check_type(instruction)
        self.emit_code("imull",instruction[2],instruction[3])
        self.emit_code("movl",instruction[3],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3])

    def op_and(self,instruction):
        '''
        This function is currently only implemented
        for integer bitwise and
        '''
        self.check_type(instruction)
        self.emit_code("andl",instruction[2],instruction[3])
        self.emit_code("movl",instruction[3],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3])

    def op_or(self,instruction):
        '''
        This function is currently only implemented
        for integer bitwise or
        '''
        self.check_type(instruction)
        self.emit_code("orl",instruction[2],instruction[3])
        self.emit_code("movl",instruction[3],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3])
    
    def op_xor(self,instruction):
        '''
        This function is currently only implemented
        for integer bitwise xor
        '''
        self.check_type(instruction)
        self.emit_code("xorl",instruction[2],instruction[3])
        self.emit_code("movl",instruction[3],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3])

    # Change implementation with 8 bit register or directly use const
    # not the ones on register stack
    
    def op_shl(self,instruction):
        '''
        This function is currently only implemented
        for integer left bit shift
        '''
        if not self.check_type(instruction, None, '%ecx'):
            return

        self.emit_code("shl",'%cl',instruction[2])
        self.emit_code("movl",instruction[2],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3], True)

    def op_shr(self,instruction):
        '''
        This function is currently only implemented
        for integer right bit shift
        '''
        if not self.check_type(instruction, None, '%ecx'):
            return

        self.emit_code("shr",'%cl',instruction[2])
        self.emit_code("movl",instruction[2],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3], True)

    def op_function_start(self,instruction):
        self.final_code.append(instruction[0])
        self.final_code.append("push %ebp")
        self.final_code.append("mov %esp, %ebp")
        self.final_code.append("push %ebx")
        self.final_code.append("push %ecx")
        self.final_code.append("push %edx")
        self.final_code.append("push %esi")
        self.final_code.append("push %edi")
    
    def op_return(self,instruction):
        # Return can have atmost 2 arguments
        # first is retq and the second is return value that needs to go to 
        # %eax
        if(len(instruction) == 2):
            register = self.request_register("%eax")
            self.emit_code("movl",instruction[1],register)
            self.free_register(register,True)
        self.op_sub(["-_int","%esp","%ebp","$20"])
        
        self.final_code.append("pop %edi")
        self.final_code.append("pop %esi")
        self.final_code.append("pop %edx")
        self.final_code.append("pop %ecx")
        self.final_code.append("pop %ebx")
        self.final_code.append("mov %ebp, %esp")
        self.final_code.append("pop %ebp")
        self.final_code.append("ret ")

    def op_param(self,instruction):
        self.final_code.append("push " + instruction[1])
    
    def op_function_call(self,instruction):
        # Function call valid right now only for 4 bytes argument
        # Function calls can have 4 arguments
        if(len(instruction) == 4):
        # Function calls can have 3 arguments
            # isntruction[0] = call
            # instruction[1] = variable where return value is stored
            # instruction[2] = function name
            # instruction[3] = number of arguments
            self.final_code.append("call " + instruction[2])
            self.move_var("%eax",instruction[1])
            self.op_add(["+_int","%esp","%esp","$" + str(instruction[3]*4)])
        else:
            self.final_code.append("call " + instruction[1])
            self.op_add(["+_int","%esp","%esp","$" + str(instruction[2]*4)])
            # isntruction[0] = call
            # instruction[1] = function name
            # instruction[2] = number of arguments


    def gen_code(self, instruction):
        if not instruction:
            return
        # Currently these instructions only work for int
        if(instruction[0][0] == "+"):
            self.op_add(instruction)
        elif(instruction[0][0] == "-"):
            self.op_sub(instruction)
        elif(instruction[0][0] == "="):
            self.op_eq(instruction)
        elif(instruction[0][0] == "*"):
            self.op_mul(instruction)
        elif(instruction[0][0] == "|"):
            self.op_or(instruction)
        elif(instruction[0][0] == "^"):
            self.op_xor(instruction)
        elif(instruction[0][0] == "&"):
            self.op_and(instruction)
        elif(instruction[0][0:2] == "<<"):
            self.op_shl(instruction)
        elif(instruction[0][0:2] == ">>"):
            self.op_shr(instruction)
        elif((len(instruction)  == 1) and (instruction[0][-1] == ':') and (instruction[0][0] != '.')):
            self.op_function_start(instruction)
        elif(instruction[0] == "param"):
            self.op_param(instruction)
        elif(instruction[0] == "callq"):
            self.op_function_call(instruction)
        elif(instruction[0] == "retq"):
            self.op_return(instruction)
            self.final_code.append('')
        else:
            self.final_code.append(' '.join(instruction))
        # self.final_code.append('')

def main(file,code):
    codegen = CodeGenerator()
    for instr in code:
        instr = instr.split()[1:]
        codegen.gen_code(instr)
    for line in codegen.final_code:
        print(line)
    

main(file,code)