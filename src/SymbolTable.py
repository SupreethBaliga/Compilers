from collections import OrderedDict

class SymbolTable() :
    def __init__(self):
       self.Table = []
       self.CompletedTable = dict()
       self.TopScope = OrderedDict()
       self.LastPushedInPrevScope = None
       self.error = False

    def InsertSymbol(self, iden, content_info):
        found = self.FindSymbolInCurrentScope(iden)
        if not found:
            found = self.FindSymbolInTable(iden)
            if found:
                print("Warning:", iden, "on line", content_info["line"], "is already declared at line", found[1]["line"])
            self.TopScope[iden] = content_info
        else:
            print("Error: Redeclaration of existing variable", iden,".\nPrior declaration is at line", found["line"])
            self.error = True

    def FindSymbolInTable(self, iden):
        Level_int = 1

        for Tree in reversed(self.Table):
            if Tree is not None and Tree.__contains__(iden):
                return abs(Level_int-len(self.Table)), Tree.get(iden)
            Level_int += 1

        return False

    def FindSymbolInCurrentScope(self, iden):
        return self.TopScope.get(iden, False)

    def PushScope(self):
        if self.TopScope:
            a = list(self.TopScope.items())
            self.LastPushedInPrevScope = a[-1][0]
        self.Table.append(self.TopScope)
        self.TopScope = OrderedDict()
        return

    def PopScope(self):
        TScope = self.TopScope
        if len(self.Table) == 1 :
            if self.LastPushedInPrevScope is not None:
                self.CompletedTable[self.LastPushedInPrevScope] = self.TopScope
            self.TopScope = self.Table.pop()
            self.LastPushedInPrevScope = None
        elif len(self.Table) > 1:
            self.TopScope = self.Table.pop()
            self.LastPushedInPrevScope = None
            scope = self.Table[-1]
            a = list(scope.items())
            if len(a) >= 1:
                self.LastPushedInPrevScope = a[-1][0]
        else:
            self.TopScope = None
        return TScope

    def PrintTable(self):
        if self.Table[0] is not None:
            for item in self.Table[0]:
                print(item, self.Table[0][item])
                if self.CompletedTable.__contains__(item):
                    print("start", item)
                    for it in self.CompletedTable[item]:
                        print(it)
                    print("end", item)


# AddType
# CT as Dict
# Table as Dict