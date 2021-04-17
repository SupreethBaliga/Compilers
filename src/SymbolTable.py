from collections import OrderedDict
from TypeTable import TypeTable
import json
import copy

# Structure of each entry of the symbol table --
# line: contains the line number where variable was declared
# check: For usual variables, the value for this will be 'VAR', for functions it will be 'FUNC',
#        for structs, it will be "STRUCT" and for unions, it will be "UNION"
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
        self.offset = 0
        self.offsetList = []
        self.flag = 0 # 1 means adding struct name, 0 means going inside symbol table,2 means adding var inside struct
    
    def InsertSymbol(self, iden, line_num, type_name=None):
        
        if self.flag == 0:
            found, entry = self.FindSymbolInCurrentScope(iden)
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
                    return Tree.get(iden), Tree[iden]

        if path == 2:
            return False, []
        elif path == 1:
            return False

    def FindSymbolInCurrentScope(self, iden):
        found = self.TopScope.get(iden, False)
        if found:
            return found, self.TopScope[iden]
        else:
            return found, []

    def PushScope(self):

        self.offsetList.append(self.offset)
        # self.offset = 0
        if len(self.Table) == 0:
            self.Table.append(self.TopScope)
            TopScopeName = list(self.TopScope.items())[-1][0]
            if TopScopeName != '#StructOrUnion':
                self.TopScope = list(self.TopScope.items())[-1][1]
                if '#scope' not in self.TopScope:
                    self.TopScope['#scope'] = []
                parScopeList = self.TopScope['#scope']
                parScopeList.append(OrderedDict())
                self.TopScope = parScopeList[-1]
        else:
            
            if '#scope' not in self.TopScope:
                self.TopScope['#scope'] = []
            
            parScopeList = self.TopScope['#scope']
            self.Table.append(self.TopScope)
            parScopeList.append(OrderedDict())
            self.TopScope = parScopeList[-1]
        
        self.TT.PushScope()
        self.error = self.error or self.TT.error
        return

    def StoreResults(self):
        self.error = self.error or self.TT.error
        self.TopScope['#StructOrUnion'] = dict(self.TT.TopScope)
        self.PushScope()
        return

    def PopScope(self):
        self.TopScope['#StructOrUnion'] = dict(self.TT.TopScope)
        self.TT.PopScope()
        self.error = self.error or self.TT.error
        TScope = self.TopScope
        self.offset = self.offsetList[-1]
        self.offsetList.pop()

        if len(self.Table) > 0:
            self.TopScope = self.Table.pop()
        else:
            self.TopScope = None
        return TScope

    def DelStructOrUnion(self, tmp):
        list_copy = []
        for item in tmp["#scope"]:
            item.pop('#StructOrUnion', None)
            if "#scope" in item:
                self.DelStructOrUnion(item)
            if not item:
                continue
            list_copy.append(item)
        if len(list_copy) == 0:
            tmp.pop('#scope', None)
        else:
            tmp['#scope'] = list_copy

    def PrintTable(self):
        # print(json.dumps(self.Table[0], indent=2))

        print("Global Symbol Table : ")
        for key, value in self.Table[0].items():
            if key != "#StructOrUnion":
                print(key)
                for key2, value2 in value.items():
                    if key2 != "#scope":
                        print(f'"{key2}" : {value2}')
                print("\n")

        for key, value in self.Table[0].items():
            if "#scope" in value:
                tmp = copy.deepcopy(value["#scope"][0])
                del tmp['#StructOrUnion']
                if "#scope" in tmp:
                    self.DelStructOrUnion(tmp)

                if len(tmp) > 0:
                    print(f'Local Symbol Table for "{key}":')
                    print(json.dumps(tmp, indent=2))
                    print("\n")

    def ModifySymbol(self, iden, field, val, statement_line=None):
        if self.flag == 0:
            found, entry = self.FindSymbolInCurrentScope(iden)
            if found:
                self.TopScope[iden][field] = val
                if field == "sizeAllocInBytes":
                    if len(self.Table) > 0:
                        self.TopScope[iden]["offset"] = self.offset
                        self.offset += val
                elif field == "vars":
                    if len(self.Table) > 0:
                        curOffset = 0
                        for var in self.TopScope[iden][field]:
                            self.TopScope[iden][field][var]["offset"] = curOffset
                            curOffset += self.TopScope[iden][field][var]["sizeAllocInBytes"]
                return True

            else:
                found, entry = self.FindSymbolInTable(iden,2)
                if found:
                    found[field] = val
                    if field == "sizeAllocInBytes":
                        if len(self.Table) > 0:
                            self.TopScope[iden]["offset"] = self.offset
                            self.offset += val
                    elif field == "vars":
                        if len(self.Table) > 0:
                            curOffset = 0
                            for var in self.TopScope[iden][field]:
                                self.TopScope[iden][field][var]["offset"] = curOffset
                                curOffset += self.TopScope[iden][field][var]["sizeAllocInBytes"]
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
        found, entry = self.FindSymbolInCurrentScope(iden)
        if found:
            return found, entry
        else:
            found, entry = self.FindSymbolInTable(iden, 2)
            if found:
                return found, entry
            else:
                print(f'Error: The variable {iden} on line {statement_line} is not declared.')
                self.error = True
                return None,None
        
    def isGlobal(self, iden):
        if len(self.Table) == 0:
            return True
        else:
            return False
