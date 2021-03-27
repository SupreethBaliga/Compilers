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

class TypeTable():
    def __init__(self):
        self.Table = OrderedDict()
        self.error = False

    def InsertSymbol(self, iden, type_name, line_num):
        # first check if something like this already present
        found = self.Table.get(iden, False)
        if found:
            print(f'Error: Redeclaration of existing data structure on {line_num}. Prior declaration at {found["line"]}')
            self.error = True
            return False
        else:
            self.Table[iden] = {}
            self.Table[iden]['check'] = type_name
            self.Table[iden]['line'] = line_num
            self.Table[iden]['vars'] = dict()
            return True
        
    def AddVars(self, iden, var_names, var_types, var_vals):
        # check if such an entry exists
        found = self.Table.get(iden, False)
        if not found:
            print(f'Tried to add variables to undeclared data structure {iden}.')
            self.error = True
            return False
        else:
            for i in range(len(var_names)):
                var_name = var_names[i]
                var_type = var_types[i]
                var_val = var_vals[i]
                if var_name in self.Table[iden]['vars']:
                    print(f'Error: Tried to redeclare the variable {var_name} in the data structure {iden}')
                    self.error = True
                    return False
                else:
                    self.Table[iden]['vars'][var_name] = (var_type, var_val)
        
    def PrintTypeTable(self):
        for x in self.Table.keys():
            print(x, self.Table[x])
    
    def LastElem(self): # to return the last inserted struct name
        temp = list(self.TopScope.items())
        if temp:
            return temp[-1][0]
        else:
            return None

