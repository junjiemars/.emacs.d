// jshell-emacs: from `(emacs-home% \"config/jshell.el\")'
import java.util.*;
import java.lang.*;
class JshellEmacs {
    public static void apropos(String what, int max) {
        // TODO
        var alts = List.of("jshell", "emacs", "apropos");
        System.out.print("(");
        for (var a : alts) {
            System.out.printf("\"%s\" ", a);
        }
        System.out.print(")");
    }
}
// eof
