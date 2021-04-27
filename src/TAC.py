# operator dest operand1 operand2 - structure of the 3AC
# For empty paramater pass ''
# For cast expression: 'cast' <dest> <tmp_to_caste> <type_to_caste_to>
# for unary expression: 'op' <dest> <tmp1> <none>

import copy
import sys
class TAC():
    def __init__(self):
        self.final_code = []
        self.temp_count = 0
        self.label_count = 0
        self.nextstat = 0
        self.strList = []
        self.floatvals = []
        self.scope_list = {}
        self.scope_counter = 0

    def newtemp(self):
        self.temp_count += 1
        temp_name = '!tmp' + str(self.temp_count) + '!'
        return temp_name

    def newlabel(self):
        self.label_count += 1
        label_name = '!lab' + str(self.label_count) + '!'

    def makelist(self, line_num = None):
        patch_list = []
        if line_num:
            patch_list.append(line_num)
        return patch_list

    def mergelist(self, plist1, plist2):
        return plist1 + plist2
    
    def backpatch(self, plist, line_num):
        for elem in plist:
            if 'goto' in self.final_code[elem][0].split():
                self.final_code[elem][1] = line_num + 1

    def quad(self, oper, dest, op1 = None, op2 = None):
        if (op1 is None) and (op2 is None):
            return [oper, dest]
        elif op2 is None:
            return [oper, dest, op1]
        else:
            return [oper, dest, op1, op2]

    def emit(self, oper, dest, op1=None, op2=None):
        self.final_code.append(self.quad(oper, dest, op1, op2))
        self.nextstat += 1
    
    def get_sym(self, ST, iden):
        if iden is None:
            return None
        else:
            ## here it is confirmed that iden exists in ST since semantic checks done
            found, entry = ST.ReturnSymTabEntry(iden)
            if "temp" in found.keys():
                return found["temp"]
            else:
                new_temp = self.newtemp()
                ST.ModifySymbol(iden, "temp", new_temp)
                return new_temp 


    def print_code(self, filename):
        file = f"TAC/{filename}.txt"
        outputFileTAC = open(file,"w")
        orig_stdout = sys.stdout
        sys.stdout = outputFileTAC
        for j in range(0, len(self.final_code)):
            print(j+1, end=' ')
            code = self.final_code[j]
            for i in range(0, len(code)):
                print(code[i], end = ' ')
            print('')
        sys.stdout = orig_stdout

    def clean_code(self):
        temp_code = copy.deepcopy(self.final_code)
        final_lines = dict()
        self.final_code = []
        deleted = 0
        for idx in range(0,len(temp_code)):
            code = temp_code[idx]
            final_lines[idx+1] = idx+1-deleted
            if 'goto' in code[0].split():
                if code[1] == '' or code[1] == None:
                    deleted += 1
                else:
                    self.final_code.append(code)
            elif 'retq' in code[0].split() and ('retq' in temp_code[idx-1][0].split() or 'retq_struct' in temp_code[idx-1][0].split()):
                deleted += 1
            else:
                self.final_code.append(code)
        
        for idx in range(0,len(self.final_code)):
            code = self.final_code[idx]
            if 'goto' in code[0].split():
                self.final_code[idx][1] = final_lines[self.final_code[idx][1]]
        
        for idx in range(0, len(self.final_code)):
            code = self.final_code[idx]
            if len(code[0])>0 and code[0][0] != '.' and code[0][-1] ==':':
                prev_code = self.final_code[idx-1]
                self.final_code[idx-1] = code
                self.final_code[idx] = prev_code
        
    def findStringIdx(self, target_str):
        for i in range(0, len(self.strList)):
            if self.strList[i] == target_str:
                return i
        
    def add_strings(self):
        for i in range(0,len(self.strList)):
            self.emit(f'.LC{i}:','','','')
            self.emit('.string', self.strList[i])
        for i in range(0,len(self.floatvals)):
            self.emit(f'.LF{i}:','','','')
            self.emit('.long', self.floatvals[i])
