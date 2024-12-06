// node-emacs: from `(emacs-home% \"config/node.el\")'
function node_emacs_apropos(what, max) {
    global.repl.repl.complete(what, (err, data) => {
        if (!err && Array.isArray(data)) {
            let d = data[0];
            for (let i = 0; i < max; i++) {
                console.log(d[i]);
            }
        }
    });
}
