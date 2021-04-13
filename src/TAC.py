# operator dest operand1 operand2 - structure of the 3AC

# For cast expression: 'cast' <dest> <tmp_to_caste> <type_to_caste_to>
# for unary expression: 'op' <dest> <tmp1> <none>

class TAC():
    def __init__(self):
        self.final_code = []
        self.temp_count = 0
        self.label_count = 0
        self.nextstat = 0

    def newtemp(self):
        self.temp_count += 1
        temp_name = '__tmp' + str(self.temp_count) + '__'
        return temp_name

    def newlabel(self):
        self.label_count += 1
        label_name = '__lab' + str(self.label_count) + '__'

    def makelist(self, line_num = None):
        patch_list = []
        if line_num:
            patch_list.append(line_num)
        return patch_list

    def mergelist(self, plist1, plist2):
        return plist1 + plist2
    
    def backpatch(self, plist, line_num):
        for elem in plist:
            if self.final_code[elem][0] == 'goto':
                self.final_code[elem][1] = line_num

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


    def print_code(self):
        for j in range(0, len(self.final_code)):
            print(j+1, end=' ')
            code = self.final_code[j]
            for i in range(0, len(code)):
                print(code[i], end = ' ')
            print('')
