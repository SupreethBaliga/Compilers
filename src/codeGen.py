import copy

# def __init__(self):
#     self.final_code = []
#     self.ST = None
#     self.final_instr_list = dict()
#     self.register_stack = ["ebx","ecx","esi","edi","edx","eax"]
#     self.register_list = self.register_stack

    
# def build(self,ST,TAC):
#     i = 0
#     for line in TAC.final_code:
#         if line[0] != None and len(str(line[0])) > 4:
#             if(str(line[0])[0] == '_' and str(line[0])[-1] == '_'):
#                 self.final_instr_list[line[0]] = i
#         if line[1] != None and len(str(line[1])) > 4:
#             if(str(line[1])[0] == '_' and str(line[1])[-1] == '_'):
#                 self.final_instr_list[line[1]] = i
#         if line[2] != None and len(str(line[2])) > 4:
#             if(str(line[2])[0] == '_' and str(line[2])[-1] == '_'):
#                 self.final_instr_list[line[2]] = i
#         if len(line) > 3 and line[3] != None and len(str(line[3])) > 4:
#             if(str(line[3])[0] == '_' and str(line[3])[-1] == '_'):
#                 self.final_instr_list[line[3]] = i
#         i += 1
    # for key in self.final_instr_list.keys():
        # print(str(key) + " " + str(self.final_instr_list[key]))



fileName = "test1.txt"
file = open(fileName,"r")
code = file.readlines()

class CodeGenerator:
    
    def __init__(self):
        # contains all the registers including %ebp and %esp 
        self.all_registers = ["%ebx","%ecx","%esi","%edi","%edx","%eax","%ebp","%esp"]
        
        # general purpose registers -> register_stack contains all the free registers
        self.register_stack = ["%ebx","%ecx","%esi","%edi","%edx","%eax"]
        
        #  register_list contains all the registers that can be used(not necessarily free)
        self.register_list = copy.deepcopy(self.register_stack)
        self.final_code = []
        self.final_code.append(".data")
        self.final_code.append(".text")
        self.final_code.append(".global main|")
        self.final_code.append(".type main|, @function") 
        self.final_code.append("\n")

    def emit_code(self, s1 = '', s2 = '', s3 = ''):
        self.final_code.append(s1 + " " + s2 + ", " + s3)

    def request_register(self):
        if not self.register_stack:
            print("ERROR! No register available")
            return
        register = self.register_stack.pop()
        return register

    def free_register(self, register):
        if(register == None):
            return
        if (register not in self.register_list):
            # Do not free register if it is %ebp and %esp
            return
        self.register_stack.append(register)
    
    def move_var(self,src,register):
        # Move in case more than one pair of ()
        if src == register:
            return
        self.emit_code("movl",src,register)
    
    def check_type(self,instruction):
        '''
        This function moves all the values into appropriate registers
        '''
        op = instruction[0]
        dest = instruction[1]
        src1 = instruction[2]
        src2 = ''
        reg1 = self.request_register()
        reg2 = None
        self.move_var(src1,reg1)
        instruction[2] = reg1
        if(len(instruction) > 3):
            src2 = instruction[3]
            reg2 = self.request_register()
            self.move_var(src2,reg2)
            instruction[3] = reg2
    
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
        

def main(file,code):
    codegen = CodeGenerator()
    for instr in code:
        instr = instr.split()
        codegen.gen_code(instr)
    for line in codegen.final_code:
        print(line)
    

main(file,code)