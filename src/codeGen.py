class CG():
    def __init__(self):
        self.final_code = []
        self.ST = None
        self.final_instr_list = dict()
        self.register_stack = ["ebx","ecx","esi","edi","edx","eax"]
        self.register_list = self.register_stack
    def build(self,ST,TAC):
        i = 0
        for line in TAC.final_code:
            if line[0] != None and len(str(line[0])) > 4:
                if(str(line[0])[0] == '_' and str(line[0])[-1] == '_'):
                    self.final_instr_list[line[0]] = i
            if line[1] != None and len(str(line[1])) > 4:
                if(str(line[1])[0] == '_' and str(line[1])[-1] == '_'):
                    self.final_instr_list[line[1]] = i
            if line[2] != None and len(str(line[2])) > 4:
                if(str(line[2])[0] == '_' and str(line[2])[-1] == '_'):
                    self.final_instr_list[line[2]] = i
            if len(line) > 3 and line[3] != None and len(str(line[3])) > 4:
                if(str(line[3])[0] == '_' and str(line[3])[-1] == '_'):
                    self.final_instr_list[line[3]] = i
            i += 1
        # for key in self.final_instr_list.keys():
            # print(str(key) + " " + str(self.final_instr_list[key]))
    
    def requestRegister(self):
        if(self.register_stack.empty()):
            print("ERROR! No register available")
            return
        register = self.register_stack.pop()
        return register
    
    def freeRegister(self, register):
        if (register not in self.register_list):
            print("The given string is not a register")
            return
        self.register_stack.append(register)
