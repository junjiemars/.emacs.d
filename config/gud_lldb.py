# lldb-emacs: from `(emacs-home% \"config/lldb.el\")'
import lldb
def lldb_emacs_apropos(what, max):
    d = lldb.debugger.GetCommandInterpreter()
    m = lldb.SBStringList()
    p = 0 if len(what) == 0 else len(what)
    # max is useless in HandleCompletion
    d.HandleCompletion(what, p, p, max, m)
    u = min(m.GetSize(), max)
    for x in range(0, u):
        print(m.GetStringAtIndex(x))
