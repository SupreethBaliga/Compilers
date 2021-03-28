from collections import OrderedDict
from TypeTable import TypeTable
import json

# Structure of each entry of the symbol table --
# line: contains the line number where variable was declared
# check: For usual variables, the value for this will be 'VAR', for functions it will be 'FUNC'
#         Yet to decide regarding structs, enums and unions
# type: if check == 'VAR' -> this field contains the data type of the variable
#       if check == 'FUNC' -> this field contains the return type of the function
# params: if check == 'VAR' -> this field is not available or contains none
#         if check == 'FUNC' -> this field points to a dict where key is parameter names and the
#                               corresponding value is the type of the parameter
# value: stores the value of the of the entry (valid only if check == 'VAR')
# Any other temporary attribute can be stored with temp_<attr_name> just to denote that it is
# temporary 
#       

class SymbolTable() :
    def __init__(self):
        self.Table = []
        self.TopScope = OrderedDict()
        self.TT = TypeTable()
        self.error = False
        self.flag = 0 # 1 means adding struct name, 0 means going inside symbol table,2 means adding var inside struct
    
    def InsertSymbol(self, iden, line_num, type_name=None):
        
        if self.flag == 0:
            found = self.FindSymbolInCurrentScope(iden)
            if not found:
                found = self.FindSymbolInTable(iden,1)
                if found:
                    print("Warning:", iden, "on line", line_num, "is already declared at line", found[1]["line"])
                self.TopScope[iden] = OrderedDict()
                self.TopScope[iden]['line'] = line_num
            else:
                print("Error: Redeclaration of existing variable", iden,". Prior declaration is at line", found["line"])
                self.error = True
        elif self.flag == 1:
            self.TT.InsertSymbol(iden,type_name, line_num, 1)
            self.error = self.error or self.TT.error
        else: # flag 2
            self.TT.InsertSymbol(iden,None, line_num, 2)
            self.error = self.error or self.TT.error

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

        if len(self.Table) == 0:
            self.Table.append(self.TopScope)
            TopScopeName = list(self.TopScope.items())[-1][0]
            if TopScopeName != 'StructOrUnion':
                self.TopScope = list(self.TopScope.items())[-1][1]
                if '__scope__' not in self.TopScope:
                    self.TopScope['__scope__'] = []
                parScopeList = self.TopScope['__scope__']
                parScopeList.append(OrderedDict())
                self.TopScope = parScopeList[-1]
        else:
            
            if '__scope__' not in self.TopScope:
                self.TopScope['__scope__'] = []
            
            parScopeList = self.TopScope['__scope__']
            self.Table.append(self.TopScope)
            parScopeList.append(OrderedDict())
            self.TopScope = parScopeList[-1]
        
        self.TT.PushScope()
        self.error = self.error or self.TT.error
        return

    def StoreResults(self):
        self.TopScope['StructOrUnion'] = dict(self.TT.TopScope)
        self.PushScope()
        return

    def PopScope(self):
        self.TopScope['StructOrUnion'] = dict(self.TT.TopScope)
        self.TT.PopScope()
        self.error = self.error or self.TT.error
        TScope = self.TopScope

        if len(self.Table) > 0:
            self.TopScope = self.Table.pop()
        else:
            self.TopScope = None
        return TScope

    def PrintTable(self):
        print(json.dumps(self.Table, indent=2))

    def ModifySymbol(self, iden, field, val, statement_line=None):
        if self.flag == 0:
            found = self.FindSymbolInCurrentScope(iden)
            if found:
                self.TopScope[iden][field] = val
                return True

            else:
                found = self.FindSymbolInTable(iden,2)
                if found:
                    found[field] = val
                    return True
                else:
                    if statement_line:
                        print(f'Tried to modify the {field} of the undeclared symbol {iden} on line {statement_line}')
                    else:
                        print(f'Tried to modify the {field} of the undeclared symbol {iden}')
                    self.error = True
                    return False
        elif self.flag == 1:
            self.TT.ModifySymbol(iden, field, val, statement_line, 1)
            self.error = self.error or self.TT.error
        else: # flag = 2
            self.TT.ModifySymbol(iden, field, val, statement_line, 2)
            self.error = self.error or self.TT.error
        
    def ReturnSymTabEntry(self, iden, statement_line=None):
        found = self.FindSymbolInCurrentScope(iden)
        if found:
            return found
        else:
            found = self.FindSymbolInTable(iden, 2)
            if found:
                return found
            else:
                print(f'Error: The variable {iden} on line {statement_line} is not declared.')
                self.error = True