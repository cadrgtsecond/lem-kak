# `lem-kak`
`lem-kak` is an attempt to bring the Kakoune exprience to Lem.

Unlike Helix, Meow or other "Kakoune-inspired" editors, we will try our best to stick to the Kakoune keybindings, for experience has shown that they are simply the most efficient.

However, we will not try to recreate any behaviour that is already provided by Lem. Syntax highlighting is built-into Lem. So is autocomplete. We will also not implement the command language because Lem already has a concept of commands. Furthermore, unlike Vi which is a glorified wrapper around `ex` that still needs its command language for full power, [Kakoune is its own editing language](https://github.com/mawww/kakoune/blob/master/doc/autoedit.asciidoc)

Furthermore, whereas Kakoune tries to be a citizen of Unix, `lem-kak` tries to be a citizen of the Lisp machine.

## Roadmap
- [ ] Kakoune bindings
- [ ] Clippy
- [ ] God Mode a la Emacs
