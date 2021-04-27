import copy
import sys
file = open(sys.argv[1], 'r')

code = file.readlines()
math_func_list = ['scanf', 'printf', 'sqrt', 'ceil', 'floor', 'pow', 'fabs', 'log', 'log10','fmod', 'exp', 'cos','sin' ,'acos', 'asin', 'tan', 'atan']
class CodeGenerator:
    
    def __init__(self):
        # contains all the registers including %ebp and %esp 
        self.all_registers = ["%ebx","%ecx","%esi","%edi","%edx","%eax","%ebp","%esp"]
        
        # general purpose registers -> register_stack contains all the free registers
        self.register_list = ["%ebx","%eax","%ecx","%esi","%edi","%edx"]

        self.register_stack = [i for i in range(len(self.register_list))]
        self.register_mapping = dict([(i, self.register_list[i]) for i in self.register_stack])
        self.reverse_mapping = dict([(self.register_list[i], i) for i in self.register_stack])

        self.eight_bit_register = {"%eax":"%al", "%ebx":"%bl", "%ecx":"%cl", "%edx":"%dl"}
        
        #  register_list contains all the registers that can be used(not necessarily free)
        # self.register_list = copy.deepcopy(self.register_stack)
        self.final_code = []
        self.final_code.append(".data")
        self.final_code.append(".text")
        self.final_code.append(".globl main")
        self.final_code.append(".type main, @function") 
        self.final_code.append("\n")
        self.label_list = {}
        self.label_num = 1

    def emit_code(self, s1 = '', s2 = '', s3 = ''):
        codeStr = s1
        if s2 != '' and isinstance(s2, int):
            s2 = self.register_mapping[s2]
        if s3 != '' and isinstance(s3, int):
            s3 = self.register_mapping[s3]
        if len(s2) > 0:
            codeStr += ' ' + s2
            if len(s3) > 0 :
                codeStr += ', ' + s3
        self.final_code.append(codeStr)

    def request_register(self, reg=None, instr=None):
        if not self.register_stack:
            print("ERROR! No register available")
            return None

        if reg is not None:
            reg_idx = self.reverse_mapping[reg]
            if reg_idx not in self.register_stack:
                swapreg_idx = self.request_register()
                if swapreg_idx is not None:
                    swapreg = self.register_mapping[swapreg_idx]
                    self.register_mapping[reg_idx], self.register_mapping[swapreg_idx] = self.register_mapping[swapreg_idx], self.register_mapping[reg_idx]
                    self.reverse_mapping[reg], self.reverse_mapping[swapreg] = self.reverse_mapping[swapreg], self.reverse_mapping[reg]
                    self.emit_code("movl", reg, swapreg)
                    return self.reverse_mapping[reg]
            else:
                self.register_stack.remove(reg_idx)
                return reg_idx
            return None

        register = self.register_stack.pop()
        return register

    def free_register(self, reg_idx, start=None):
        if(reg_idx == None):
            return
        if isinstance(reg_idx, int):
            register = self.register_mapping[reg_idx]
        else:
            register = reg_idx
        if (register not in self.register_list):
            # Do not free register if it is %ebp and %esp
            return

        if not isinstance(reg_idx, int):
            reg_idx = self.reverse_mapping[reg_idx]
        if start is not None:
            self.register_stack.insert(0, reg_idx)
        else:
            self.register_stack.append(reg_idx)
    
    def move_var(self,src,dst):
        # Move in case more than one pair of ()
        if src == dst:
            return
        # dst = self.register_mapping[dst]
        self.emit_code("movl",src,dst)
    
    def check_type(self,instruction, req_reg1=None, req_reg2=None, eight_bit1=False, eight_bit2=False):
        '''
        This function moves all the values into appropriate registers
        '''
        op = instruction[0]
        dest = instruction[1]
        src1 = instruction[2]
        src2 = ''
        
        if eight_bit1 == True:
            reg1_idx = self.request_register("%edx")
        else:
            reg1_idx = self.request_register(req_reg1)
        if not reg1_idx:
            return False

        reg2_idx = None
        if src1[0] == "%" and len(src1) > 4:
            offset = int(src1[4:])
            self.emit_code("leal", f'{offset}(%ebp)', reg1_idx)
        elif src1[0] == "(":
            self.move_var(src1[1:-1],reg1_idx)
            self.emit_code("movl", f'({self.register_mapping[reg1_idx]})', reg1_idx)
        else:
            self.move_var(src1,reg1_idx)
        instruction[2] = reg1_idx
        if(len(instruction) > 3):
            src2 = instruction[3]
            if eight_bit2 is True:
                reg2_idx = self.request_register("%ebx")
            else:
                reg2_idx = self.request_register(req_reg2)
            if not reg2_idx:
                return False
            if src2[0] == "(":
                self.move_var(src2[1:-1],reg2_idx)
                self.emit_code("movl", f'({self.register_mapping[reg2_idx]})', reg2_idx)
            else:
                self.move_var(src2,reg2_idx)
            instruction[3] = reg2_idx

        return True  

    def create_label(self, line):
        if int(line) not in self.label_list:
            label = f'.L{self.label_num}'
            self.label_num += 1
            self.label_list[int(line)] = label
            return label
        return self.label_list[int(line)]
    
    def deref(self, dest):
        if dest[0] != '(':
            return dest
        dest = dest[1:-1]
        register = self.request_register()
        self.emit_code("movl", dest, register)
        register = self.register_mapping[register]
        return f'({register})'

    def float_deref(self, instruction, rege1 = None, rege2 = None, rege3 = None):
        flag1, flag2, flag3 = 0, 0, 0
        reg1 = instruction[1]
        if instruction[1][0] == '(':
            reg1 = self.request_register(rege1)
            self.emit_code("movl", instruction[1][1:-1], reg1)
            reg1 = self.register_mapping[reg1]
            reg1 = f'({reg1})'
            flag1 = 1
        
        reg2 = None
        if len(instruction) > 2:
            reg2 = instruction[2]
        if len(instruction) > 2 and instruction[2][0] == '(':
            reg2 = self.request_register(rege2)
            self.emit_code("movl", instruction[2][1:-1], reg2)
            reg2 = self.register_mapping[reg2]
            reg2 = f'({reg2})'
            flag2 = 1
        
        reg3 = None
        if len(instruction) > 3:
            reg3 = instruction[3]
        if len(instruction) > 3 and instruction[3][0] == '(':
            reg3 = self.request_register(rege3)
            self.emit_code("movl", instruction[3][1:-1], reg3)
            reg3 = self.register_mapping[reg3]
            reg3 = f'({reg3})'
            flag3 = 1

        if flag1 == 1:
            self.free_register(reg1[1:-1])
        if flag2 == 1:
            self.free_register(reg2[1:-1])
        if flag3 == 1:
            self.free_register(reg3[1:-1])

        return reg1, reg2, reg3

    def op_add(self,instruction):
        '''
        This function is currently only implemented
        for integer addition and float
        '''
        if instruction[0][2:] =='float':
            reg1, reg2, reg3 = self.float_deref(instruction)
            self.emit_code("flds", reg2)
            self.emit_code("fadds", reg3)
            self.emit_code("fstps", reg1)
        else:
            self.check_type(instruction)
            instruction[1] = self.deref(instruction[1])
            self.emit_code("addl",instruction[2],instruction[3])
            self.emit_code("movl",instruction[3],instruction[1])
            self.free_register(instruction[2])
            self.free_register(instruction[3])
            if instruction[1][0] == '(':
                instruction[1] = instruction[1][1:-1]
                self.free_register(instruction[1])
                
    def op_sub(self,instruction):
        '''
        This function is currently only implemented
        for integer and float
        '''
        if instruction[0][2:] =='float':
            reg1, reg2, reg3 = self.float_deref(instruction)
            self.emit_code("flds", reg2)
            self.emit_code("fsubs", reg3)
            self.emit_code("fstps", reg1)
        else:
            self.check_type(instruction)
            instruction[1] = self.deref(instruction[1])
            self.emit_code("subl",instruction[3],instruction[2])
            self.emit_code("movl",instruction[2],instruction[1])
            self.free_register(instruction[2])
            self.free_register(instruction[3])
            if instruction[1][0] == '(':
                instruction[1] = instruction[1][1:-1]
                self.free_register(instruction[1])
        
    def op_eq(self,instruction):
        '''
        This function is currently only implemented
        for integer and float
        '''
        if instruction[0][2:] =='float':
            reg1, reg2, reg3 = self.float_deref(instruction)
            self.emit_code("flds", reg2)
            self.emit_code("fstps", reg1)
        elif instruction[0][2:] == 'char':
            self.check_type(instruction)
            instruction[1] = self.deref(instruction[1])
            regEax = self.request_register("%eax")

            self.emit_code("movl", instruction[2], "%eax")
            self.emit_code("movzbl", "%al", instruction[2])

            self.emit_code("movl",instruction[2],instruction[1])
            
            self.free_register(instruction[2])
            self.free_register("%eax")
            if instruction[1][0] == '(':
                instruction[1] = instruction[1][1:-1]
                self.free_register(instruction[1])
        else:
            self.check_type(instruction)
            instruction[1] = self.deref(instruction[1])
            self.emit_code("movl",instruction[2],instruction[1])
            self.free_register(instruction[2])
            if instruction[1][0] == '(':
                instruction[1] = instruction[1][1:-1]
                self.free_register(instruction[1])

    def op_div(self, instruction):
        '''
        '''
        if instruction[0][2:] =='int':
            edx = self.request_register('%edx')
            eax = self.request_register('%eax')
            # number is edx : eax
            if not edx or not eax:
                return
            self.check_type(instruction)
            self.emit_code("movl", instruction[2], "%eax")
            self.emit_code("cltd")
            self.emit_code("idivl", instruction[3])
            self.emit_code("movl", "%eax", instruction[1])
            self.free_register(instruction[2])
            self.free_register(instruction[3])
            self.free_register('%edx',True)
            self.free_register('%eax',True)
        elif instruction[0][2:] =='float':
            reg1, reg2, reg3 = self.float_deref(instruction)
            self.emit_code("flds", reg2)
            self.emit_code("fdivs", reg3)
            self.emit_code("fstps", reg1)

    def op_mod(self, instruction):
        '''
            Int implemented
            Float not needed
        '''
        edx = self.request_register('%edx')
        eax = self.request_register('%eax')
        # number is edx : eax
        if not edx or not eax:
            return
        self.check_type(instruction)
        self.emit_code("movl", instruction[2], "%eax")
        self.emit_code("cltd")
        self.emit_code("idivl", instruction[3])
        self.emit_code("movl", "%edx", instruction[1])
        self.free_register('%edx')
        self.free_register('%eax')
        self.free_register(instruction[2])
        self.free_register(instruction[3])

    def op_mul(self,instruction):
        '''
        This function is currently only implemented
        for integer multiplication and float
        '''
        if instruction[0][2:] =='int':
            self.check_type(instruction)
            self.emit_code("imull",instruction[2],instruction[3])
            self.emit_code("movl",instruction[3],instruction[1])
            self.free_register(instruction[2])
            self.free_register(instruction[3])
        elif instruction[0][2:] =='float':
            reg1, reg2, reg3 = self.float_deref(instruction)
            self.emit_code("flds", reg2)
            self.emit_code("fmuls", reg3)
            self.emit_code("fstps", reg1)

    def op_and(self,instruction):
        '''
        This function is currently only implemented
        for integer bitwise
        float not needed
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
        float not needed
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
        float not needed
        '''
        self.check_type(instruction)
        self.emit_code("xorl",instruction[2],instruction[3])
        self.emit_code("movl",instruction[3],instruction[1])
        self.free_register(instruction[2])
        self.free_register(instruction[3])
    
    def op_shl(self,instruction):
        '''
        This function is currently only implemented
        for integer left bit shift
        Float not needed
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
        Float not needed
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
        if(instruction[0] == "retq"):
            if(len(instruction) == 2):
                register = self.request_register("%eax")
                instruction[1] = self.deref(instruction[1])
                self.emit_code("movl",instruction[1],register)
                self.free_register(register,True)
                if instruction[1][0] == '(':
                    instruction[1] = instruction[1][1:-1]
                    self.free_register(instruction[1])
        else:
            # This is return for retq_struct
            register = self.request_register()
            register = self.register_mapping[register]
            self.emit_code("movl","8(%ebp)",register)
            # instruction[2] is the size
            # currently works only in offset of 4
            # instruction[1] has the return variable address
            nVariables = int(instruction[2])
            tempInstruction = copy.deepcopy(instruction[1])
            reg1 = None
            if instruction[1][0] == '(':
                reg1 = self.request_register()
                self.emit_code("movl",instruction[1][1:-1],reg1)
                # self.emit_code("movl",reg1,f'({reg1})')
                val = 0
                reg1 = self.register_mapping[reg1]
                for i in range(int(nVariables/4)):
                    address = val + i*4
                    if address == 0:
                        address = f'({reg1})'
                    else:
                        address = str(address) + f'({reg1})'
                    src = address
                    dst = "(" + str(register) + ")"
                    if i != 0 :
                        dst = str(int(i*4)) + dst
                    reg2 = self.register_mapping[self.request_register()]
                    self.move_var(src,reg2)
                    self.move_var(reg2,dst)
                    self.free_register(reg2)

                self.free_register(reg1)
            else:
                val = int(tempInstruction.split('(')[0])
                for i in range(int(nVariables/4)):
                    address = val + i*4
                    if address == 0:
                        address = "(%ebp)"
                    else:
                        address = str(address) + "(%ebp)"
                    src = address
                    dst = "(" + str(register) + ")"
                    if i != 0 :
                        dst = str(int(i*4)) + dst
                    reg2 = self.register_mapping[self.request_register()]
                    self.move_var(src,reg2)
                    self.move_var(reg2,dst)
                    self.free_register(reg2)
            
            self.free_register(register)
            



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
        if(len(instruction) == 2):
            instruction[1] = self.deref(instruction[1])
            if instruction[1][0] != '(':
                reg = self.request_register()
                self.emit_code("movl", instruction[1], reg)
                self.emit_code("push", reg)
                self.free_register(reg)
            else:
                self.final_code.append("push " + instruction[1])
            if instruction[1][0] == '(':
                instruction[1] = instruction[1][1:-1]
                self.free_register(instruction[1])
        else:
            nVariables = int(instruction[2][1:])
            if nVariables == 1:
                self.op_push_char(instruction)
            else:
                tempInstruction = copy.deepcopy(instruction[1])
                val = int(tempInstruction.split('(')[0])
                tempCodeList = []
                
                for i in range(int(nVariables/4)):
                    address = val + i*4
                    if address == 0:
                        address = "(%ebp)"
                    else:
                        address = str(address) + "(%ebp)"
                    src = address
                    tempCodeList.append("push " + src)
                # This is the case for struct
                for line in reversed(tempCodeList):
                    self.final_code.append(line)


    def op_function_call(self,instruction):
        # Function call valid right now only for 4 bytes argument
        # Function calls can have 4 arguments
        if(len(instruction) == 4):
        # Function calls can have 3 arguments
            # isntruction[0] = call
            # instruction[1] = variable where return value is stored
            # instruction[2] = function name
            # instruction[3] = number of arguments
             
            # original
            # self.final_code.append("call " + instruction[2])
            # self.emit_code("movl", "%eax", instruction[1])
            # self.op_add(["+_int","%esp","%esp","$" + str(int(instruction[3])*4)])
            
            self.emit_code("call ", instruction[2])
            if instruction[2] in math_func_list:
                self.emit_code('fstps', instruction[1])
                self.emit_code('addl', '$16', '%esp')
            else:   
                self.emit_code("movl", "%eax", instruction[1])
                # self.op_add(["+_int","%esp","%esp","$" + str(int(instruction[3])*4)])
        else:
            # original
            # self.final_code.append("call " + instruction[1])
            # self.op_add(["+_int","%esp","%esp","$" + str(int(instruction[2])*4)])
            
            self.emit_code("call ", instruction[1])
            # Due to the problems occurring in printf
            # if instruction[1] not in math_func_list or instruction[1] == "printf" or instruction[1] == "scanf":
            #     self.op_add(["+_int","%esp","%esp","$" + str(int(instruction[2])*4)])
            # else:
            #     self.emit_code('addl', '$16', '%esp')
            # isntruction[0] = call
            # instruction[1] = function name
            # instruction[2] = number of arguments

    def op_function_call_struct(self,instruction):
        
        # Function calls can have 4 arguments
        # isntruction[0] = call
        # instruction[1] = variable where return value is stored
        # instruction[2] = function name
        # instruction[3] = number of arguments
        
        register = self.request_register()
        self.emit_code("leal", instruction[1],register)
        self.emit_code("push ",register)
        self.free_register(register)
        self.emit_code("call ", instruction[2])
        self.op_add(["+_int","%esp","%esp","$" + str((int(instruction[3]) + 1)*4)])

    def op_neg(self, instruction):
        '''
        This function is currently only implemented
        for integer negation (2's complement)
        Float implemented
        '''
        if instruction[0][7:] =='int':
            self.check_type(instruction)
            self.emit_code("negl",instruction[2])
            self.emit_code("movl",instruction[2],instruction[1])
            self.free_register(instruction[2])
        elif instruction[0][7:] =='float':
            reg1, reg2, reg3 = self.float_deref(instruction)
            self.emit_code("flds", reg2)
            self.emit_code("fchs", '')
            self.emit_code("fstps", reg1) 
        
    def op_not(self, instruction):
        '''
        This function is currently only implemented
        for integer bitwise not (1's complement)
        Float not needed
        '''
        self.check_type(instruction)
        self.emit_code("notl",instruction[2])
        self.emit_code("movl",instruction[2],instruction[1])
        self.free_register(instruction[2])

    # def op_pre_inc(self, instruction):
    #     '''
    #     This function is currently only implemented
    #     for integer pre increment
    #     Float not needed
    #     '''
    #     self.check_type(instruction)
    #     self.emit_code("addl","$1",instruction[2])
    #     self.emit_code("movl",instruction[2],instruction[1])
    #     self.free_register(instruction[2])

    # def op_pre_dec(self, instruction):
    #     '''
    #     This function is currently only implemented
    #     for integer pre increment
    #     Float not needed
    #     '''
    #     self.check_type(instruction)
    #     self.emit_code("subl","$1",instruction[2])
    #     self.emit_code("movl",instruction[2],instruction[1])
    #     self.free_register(instruction[2])

    def op_ifnz_goto(self, instruction):
        reg = self.request_register()
        src = instruction[3]
        if src[0] == "(":
            self.move_var(src[1:-1],reg)
            self.emit_code("movl", f'({self.register_mapping[reg]})', reg)
        else:
            self.move_var(src,reg)
        self.emit_code("cmp", "$0", reg)
        label = self.create_label(instruction[2])
        self.emit_code("jne", label)
        self.free_register(reg)

    def op_goto(self, instruction):
        label = self.create_label(instruction[1])
        self.emit_code("jmp", label)

    def op_comparator(self, instruction):
        '''
        This function is currently only implemented
        for integer comparator
        '''
        if instruction[0][3:]=='int' or instruction[0][2:]=='int':
            self.check_type(instruction)
            self.emit_code("cmpl",instruction[3],instruction[2])
            
            reg = self.request_register("%edx")
            reg = self.register_mapping[reg]
            eight_reg = self.eight_bit_register[reg]

            if instruction[0][0:2] == "<=":
                self.emit_code("setle", eight_reg)
            elif instruction[0][0:2] == ">=":
                self.emit_code("setge", eight_reg)
            elif instruction[0][0:2] == "==":
                self.emit_code("sete", eight_reg)
            elif instruction[0][0:2] == "!=":
                self.emit_code("setne", eight_reg)
            elif instruction[0][0] == "<":
                self.emit_code("setl", eight_reg)
            elif instruction[0][0] == ">":
                self.emit_code("setg", eight_reg)
            self.emit_code("movzbl", eight_reg, instruction[3])
            self.emit_code("movl", instruction[3], instruction[1])
            self.free_register(instruction[2])
            self.free_register(instruction[3])
            self.free_register(reg)
        elif instruction[0][2:6]=='char' or instruction[0][3:7]=='char':
            self.check_type(instruction)
            self.emit_code("cmpl",instruction[3],instruction[2])
            
            reg = self.request_register("%edx")
            reg = self.register_mapping[reg]
            eight_reg = self.eight_bit_register[reg]

            if instruction[0][0:2] == "<=":
                self.emit_code("setle", eight_reg)
            elif instruction[0][0:2] == ">=":
                self.emit_code("setge", eight_reg)
            elif instruction[0][0:2] == "==":
                self.emit_code("sete", eight_reg)
            elif instruction[0][0:2] == "!=":
                self.emit_code("setne", eight_reg)
            elif instruction[0][0] == "<":
                self.emit_code("setl", eight_reg)
            elif instruction[0][0] == ">":
                self.emit_code("setg", eight_reg)
            self.emit_code("movzbl", eight_reg, instruction[3])
            self.emit_code("movl", instruction[3], instruction[1])
            self.free_register(instruction[2])
            self.free_register(instruction[3])
            self.free_register(reg)
    
        elif instruction[0][3:]=='float' or instruction[0][2:]=='float':
            reg = self.request_register("%edx")
            reg = self.register_mapping[reg]

            rege1, rege2, rege3 = self.float_deref(instruction, "%ebx", "%eax", "%esi")
        
            if instruction[0][0] == '<':
                self.emit_code('flds', rege2)
                self.emit_code('flds', rege3)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('seta', self.eight_bit_register[reg])
                self.emit_code('movzbl', self.eight_bit_register[reg], reg)
                self.emit_code('movl', reg, rege1)
            elif instruction[0][0] == '>':
                self.emit_code('flds', rege3)
                self.emit_code('flds', rege2)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('seta', self.eight_bit_register[reg])
                self.emit_code('movzbl', self.eight_bit_register[reg], reg)
                self.emit_code('movl', reg, rege1)
            elif instruction[0][0:2] == '<=':
                self.emit_code('flds', rege2)
                self.emit_code('flds', rege3)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('setnb', self.eight_bit_register[reg])
                self.emit_code('movzbl', self.eight_bit_register[reg], reg)
                self.emit_code('movl', reg, rege1)
            elif instruction[0][0:2] == '>=':
                self.emit_code('flds', rege3)
                self.emit_code('flds', rege2)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('setnb', self.eight_bit_register[reg])
                self.emit_code('movzbl', self.eight_bit_register[reg], reg)
                self.emit_code('movl', reg, rege1)
            elif instruction[0][0:2] == '==':
                reg1 = self.request_register("%ecx")
                reg1 = self.register_mapping[reg1]
                self.emit_code('flds', rege2)
                self.emit_code('flds', rege3)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('setnp', self.eight_bit_register[reg])
                self.emit_code('movl', '$0', reg1)
                self.emit_code('flds', rege2)
                self.emit_code('flds', rege3)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('cmovne', reg1, reg)
                self.emit_code('movzbl', self.eight_bit_register[reg], reg)
                self.emit_code('movl', reg, rege1)
                self.free_register(reg1)
            elif instruction[0][0:2] == '!=':
                reg1 = self.request_register("%ecx")
                reg1 = self.register_mapping[reg1]
                self.emit_code('flds', rege2)
                self.emit_code('flds', rege3)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('setp', self.eight_bit_register[reg])
                self.emit_code('movl', '$1', reg1)
                self.emit_code('flds', rege2)
                self.emit_code('flds', rege3)
                self.emit_code('fucomip', '%st(1)', '%st')
                self.emit_code('fstp', '%st(0)')
                self.emit_code('cmovne', reg1, reg)
                self.emit_code('movzbl', self.eight_bit_register[reg], reg)
                self.emit_code('movl', reg, rege1)
                self.free_register(reg1)
                
            self.free_register(reg)

    # def op_logical(self, instruction):
    #     self.check_type(instruction, "%edx", "%ecx")
    #     reg = self.request_register("%eax")
    #     reg = self.register_mapping[reg]
    #     self.emit_code("movl", "$0", reg)
    #     self.emit_code("testl", instruction[2], instruction[2])
    #     self.emit_code("setne", self.eight_bit_register["%eax"])
    #     self.emit_code("movl", "$0", instruction[2])
    #     self.emit_code("testl", instruction[3], instruction[3])
    #     self.emit_code("setne", self.eight_bit_register["%edx"])

    #     if instruction[0][0:2] == "&&":
    #         self.emit_code("andl",instruction[2],reg)
    #     elif instruction[0][0:2] == "||":
    #         self.emit_code("orl",instruction[2],reg)
    #     self.emit_code("movl", reg, instruction[1])

    #     self.free_register(instruction[2])
    #     self.free_register(instruction[3])
    #     self.free_register(reg)

    def op_logical_not(self, instruction):
        self.check_type(instruction)
        self.emit_code("cmpl", "$0", instruction[2])
        reg = self.request_register("%edx")
        reg = self.register_mapping[reg]
        reg = self.eight_bit_register[reg]
        self.emit_code("sete", reg)
        self.emit_code("movzbl", reg, instruction[2])
        self.emit_code("movl", instruction[2], instruction[1])

        self.free_register(instruction[2])
        self.free_register("%edx")

    def op_assgn(self, instruction):
        '''
        This function is currently only implemented
        for integer assignment operators
        '''
        reg = self.request_register()
        instruction.insert(1, instruction[1])
        instruction[1] = self.deref(instruction[1])

        if instruction[0][0] == '<' or instruction[0][0] == '>':
            instruction[0] = instruction[0][0:2] + instruction[0][3:]
        else:
            instruction[0] = instruction[0][0] + instruction[0][2:]
        if instruction[0][0] == '*':
            self.op_mul(instruction)
        if instruction[0][0] == '/':
            self.op_div(instruction)
        if instruction[0][0] == '%':
            self.op_mod(instruction)
        if instruction[0][0] == '+':
            self.op_add(instruction)
        if instruction[0][0] == '-':
            self.op_sub(instruction)
        if instruction[0][0] == '&':
            self.op_and(instruction)
        if instruction[0][0] == '^':
            self.op_xor(instruction)
        if instruction[0][0] == '|':
            self.op_or(instruction)
        if instruction[0][0] == '<':
            self.op_shl(instruction)
        if instruction[0][0] == '>':
            self.op_shr(instruction)
        
        if instruction[1][0] == '(':
            instruction[1] = instruction[1][1:-1]
            self.free_register(instruction[1])         

    def op_load_float(self, instruction):
        '''
        This function is for generating floats
        '''
        self.emit_code("flds", instruction[1])
        self.emit_code("fstps", instruction[2])

    def op_printf_push_float(self, instruction):
        '''
        This function handles pushing of float arguments for printf
        '''
        reg1, reg2, reg3 = self.float_deref(instruction)
        self.emit_code("flds", reg1)
        self.emit_code("subl", "$4", "%esp")
        self.emit_code("leal", "-8(%esp)", "%esp")
        self.emit_code("fstpl", "(%esp)")
    
    def op_math_func_push_float(self, instruction):
        '''
        This function handles pushing of float arguments for math funcs
        '''
        reg1, reg2, reg3 = self.float_deref(instruction)
        self.emit_code("flds", reg1)
        self.emit_code("subl", "$8", "%esp")
        self.emit_code("leal", "-8(%esp)", "%esp")
        self.emit_code("fstpl", "(%esp)")

    def op_math_func_push_int(self, instruction):
        '''
        This function handles pushing of int arguments for math funcs
        '''
        reg1, reg2, reg3 = self.float_deref(instruction)
        self.emit_code("fildl", reg1)
        self.emit_code("subl", "$8", "%esp")
        self.emit_code("leal", "-8(%esp)", "%esp")
        self.emit_code("fstpl", "(%esp)")

    def op_pow_func_push_int(self, instruction):
        '''
        This function handles pushing of int arguments for pow func
        '''
        reg1, reg2, reg3 = self.float_deref(instruction)
        self.emit_code("fildl", reg1)
        self.emit_code("leal", "-8(%esp)", "%esp")
        self.emit_code("fstpl", "(%esp)")

    def op_pow_func_push_float(self, instruction):
        '''
        This function handles pushing of int arguments for pow func
        '''
        reg1, reg2, reg3 = self.float_deref(instruction)
        self.emit_code("flds", reg1)
        self.emit_code("leal", "-8(%esp)", "%esp")
        self.emit_code("fstpl", "(%esp)")

    def op_push_char(self, instruction):
        reg1 = self.request_register("%eax")
        reg2 = self.request_register()
        # instruction[1] = self.deref(instruction[1])
        self.emit_code("movl", instruction[1], reg1)
        self.emit_code("movzbl", "%al", reg2)
        self.emit_code("movl", reg2, instruction[1])
        self.emit_code("push", instruction[1])
        self.free_register(reg1)
        self.free_register(reg2)

    def op_amp(self, instruction):
        '''
        This function handles the & operator
        '''
        reg = self.request_register()
        instruction[2] = self.deref(instruction[2])
        self.emit_code("leal", instruction[2] , reg)
        self.emit_code("movl", reg, instruction[1])
        self.free_register(reg)
        if instruction[2][0] == '(':
            instruction[2] = instruction[2][1:-1]
            self.free_register(instruction[2])
    
    def op_cast(self, instruction):
        # type[0] is dest, types[1] is src, instruction[1] is dest, instruction[2] is src
        types = instruction[3].split(',')
        if types[0]=='float' and (types[1]=='int' or types[1]=='unsigned_int'):
            self.emit_code('fildl', instruction[2])
            self.emit_code('fstps', instruction[1])
        elif (types[0]=='int' or types[0]=='unsigned_int') and types[1]=='float':
            self.emit_code('flds', instruction[2])
            self.emit_code('fisttpl', instruction[1])
    
    def gen_code(self, instruction):
        if not instruction:
            return
        # Currently these instructions only work for int
        #Add cases for lengths
        if instruction[0][0:2] == "*=" or instruction[0][0:2] == "/=" or instruction[0][0:2] == "%=" or instruction[0][0:2] == "+=" or instruction[0][0:2] == "-=" or instruction[0][0:2] == "&=" or instruction[0][0:2] == "^=" or instruction[0][0:2] == "|=":
            self.op_assgn(instruction)
        elif len(instruction[0]) > 2 and (instruction[0][0:3] == "<<=" or instruction[0][0:3] == ">>="):
            self.op_assgn(instruction)
        elif(instruction[0][0] == "+"):
            self.op_add(instruction)
        elif(instruction[0][0] == "-"):
            self.op_sub(instruction)
        elif instruction[0][0:2] == "<=" or instruction[0][0:2] == ">=" or instruction[0][0:2] == "==" or instruction[0][0:2] == "!=" or instruction[0][0] == "<" or instruction[0][0] == ">":
            self.op_comparator(instruction)
        # elif instruction[0][0:2] == "&&" or instruction[0][0:2] == "||":
        #     self.op_logical(instruction)
        elif instruction[0][0] == "=" or instruction[0][0:6] == "UNARY+" or instruction[0][0:6] == "UNARY*":
            if instruction[0][0:6] == "UNARY+" or instruction[0][0:6] == "UNARY*":
                instruction[0] = "=" + instruction[0][6:]
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
        elif((len(instruction)  == 1) and (instruction[0][-1] == ':') and (instruction[0][0] != '.') and ('.' not in instruction[0])):
            self.op_function_start(instruction)
        elif(instruction[0] == "param"):
            self.op_param(instruction)
        elif(instruction[0] == "callq"):
            self.op_function_call(instruction)
        elif(instruction[0] == "callq_struct"):
            self.op_function_call_struct(instruction)
        elif(instruction[0] == "retq" or instruction[0] == "retq_struct"):
            self.op_return(instruction)
            self.final_code.append('')
        elif(instruction[0][0] == "/"):
            self.op_div(instruction)
        elif(instruction[0][0] == "%"):
            self.op_mod(instruction)
        elif instruction[0][0:6] == "UNARY-":
            self.op_neg(instruction)
        elif instruction[0][0:6] == "UNARY~":
            self.op_not(instruction)
        elif instruction[0][0:6] == "UNARY!":
            self.op_logical_not(instruction)
        elif instruction[0][0:6] == "UNARY&":
            self.op_amp(instruction)
        # elif instruction[0][0:5] == "PRE++":
        #     self.op_pre_inc(instruction)
        # elif instruction[0][0:5] == "PRE--":
        #     self.op_pre_dec(instruction)
        elif instruction[0][0:4] == "ifnz" and instruction[1][0:4] == "goto":
            self.op_ifnz_goto(instruction)
        elif instruction[0][0:4] == "goto":
            self.op_goto(instruction)
        elif instruction[0] == 'load_float':
            self.op_load_float(instruction)
        elif instruction[0] == 'printf_push_float':
            self.op_printf_push_float(instruction)
        elif instruction[0] == 'push_char':
            self.op_push_char(instruction)
        elif instruction[0] == 'math_func_push_float':
            self.op_math_func_push_float(instruction)
        elif instruction[0] == 'math_func_push_int':
            self.op_math_func_push_int(instruction)
        elif instruction[0] == 'pow_func_push_int':
            self.op_pow_func_push_int(instruction)
        elif instruction[0] == 'pow_func_push_float':
            self.op_pow_func_push_float(instruction)
        elif instruction[0] == 'cast':
            self.op_cast(instruction)
        else:
            self.final_code.append(' '.join(instruction))

def main(file,code):
    codegen = CodeGenerator()
    for instr in code:
        codegen.final_code.append(f'label {instr.split()[0]}:')
        instr = instr.split()[1:]
        codegen.gen_code(instr)
        codegen.final_code.append('')
            
    to_print = []
    for line in codegen.final_code:
        if len(line) >= 5 and line[0:5] == "label":
            if int(line[6:-1]) in codegen.label_list:
                to_print.append(codegen.label_list[int(line[6:-1])]+":")
        else:
            to_print.append(line)
    codegen.final_code = to_print

    for line in codegen.final_code:
        print(line)
    
main(file,code)