from collections import OrderedDict

## This handles the struct and union case
# NOTE: WE HAVE PUT A RESTRICTION THAT A STRUCT AND UNION CANNOT HAVE THE SAME IDENTIFIER
# It's a dictionary with the key as the struct name and the value as another dict
# The value dict contains the following parameters:
# check: This will be 'STRUCT' or 'UNION'
# line : gives the line no. where this symbol was declared
# vars: This will again be a dictionary with the key as the variable name and the value as a pair
#       of attributes. First element will be type of variable, and the second will be the default 
#       of the variable if give, if not, it is None

class TypeTable() :
    def __init__(self):
       self.Table = []
       self.TopScope = OrderedDict()
       self.error = False

    def InsertSymbol(self, iden, type_name, line_num, path):
        # first check if something like this already present
        if path == 1:
            found = self.TopScope.get(iden, False)
            if found:
                print(f'Error: Redeclaration of existing data structure on {line_num}. Prior declaration at {found["line"]}')
                self.error = True
                return False
            else:
                self.TopScope[iden] = {}
                self.TopScope[iden]['check'] = type_name
                self.TopScope[iden]['line'] = line_num
                self.TopScope[iden]['vars'] = dict()
                return True
        else: #path = 2
            a = list(self.TopScope.items())
            if not a:
                print(f'No such data structure inside which this variable {iden} on line {line_num} can be inserted')
            else:
                strucName = a[-1][0]
                if iden in self.TopScope[strucName]['vars'].keys():
                    print(f'Error: Redeclaration of variable {iden} on line {line_num}. Prior declaration at line {self.TopScope[strucName]["vars"][iden]["line"]}')
                    self.error = True
                    return False
                else: 
                    self.TopScope[strucName]['vars'][iden] = dict()
                    self.TopScope[strucName]['vars'][iden]['line'] = line_num
                    return True

    def FindSymbolInTable(self, iden, path):
        Level_int = 1

        if path == 1:
            for Tree in reversed(self.Table):
                if Tree is not None and Tree.__contains__(iden):
                    return abs(Level_int-len(self.Table)), Tree.get(iden)
                Level_int += 1

        elif path == 2:
            for Tree in reversed(self.Table):
                if Tree is not None and Tree.__contains__(iden):
                    return Tree.get(iden)

        return False

    def FindSymbolInCurrentScope(self, iden):
        return self.TopScope.get(iden, False)

    def PushScope(self):
        self.Table.append(self.TopScope)
        self.TopScope = OrderedDict()
        return

    def PopScope(self):
        self.TopScope = self.Table.pop()

    
    def ModifySymbol(self, iden, field, val, statement_line, path):
        if path == 1:
            found = self.FindSymbolInCurrentScope(iden)
            if found:
                self.TopScope[iden][field] = val
                return True

            else:
                if statement_line:
                    print(f'Tried to modify the {field} of the undeclared symbol {iden} on line {statement_line}')
                else:
                    print(f'Tried to modify the {field} of the undeclared symbol {iden}')
                self.error = True
                return False
        else: # path =2
            a = list(self.TopScope.items())
            if not a:
                print(f'No such data structure inside which this variable {iden} on line {statement_line} can be inserted')
            else:
                strucName = a[-1][0]
                if iden in self.TopScope[strucName]['vars'].keys():
                    self.TopScope[strucName]['vars'][iden][field] = val
                else:
                    print(f'Tried to modify undeclared variable {iden} in the data structure {strucName} on line {statement_line}')
                    self.error = True
                    return False

    
    def ReturnTypeTabEntry(self, iden, type_name, statement_line=None):
        found = self.FindSymbolInCurrentScope(iden)
        if found:
            if found['check'].lower() == type_name:
                return found
            else:
                print(f'Error: The data structure {type_name} {iden} on line {statement_line} is not declared.')
                self.error = True
                return None
        else:
            found = self.FindSymbolInTable(iden, 2)
            if found:
                if found['check'].lower() == type_name:
                    return found
                else:
                    print(f'Error: The data structure {type_name} {iden} on line {statement_line} is not declared.')
                    self.error = True
                    return None
            else:
                print(f'Error: The data structure {type_name} {iden} on line {statement_line} is not declared.')
                self.error = True
                return None
