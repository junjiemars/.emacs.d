// node-emacs: from `(emacs-home% \"config/node.el\")'
function node_emacs_apropos(what, max) {
    global.repl.repl.complete(what, (err, data) => {
        if (err) {
            console.log('()');
        } else {
            if (Array.isArray(data)) {
                let out = [];
                for (let ss of data[0]) {
                    out.push('\"' + ss + '\"');
                }
                console.log('(%s)', out.join(' '));
            }
        }
    });
}
