# Your init script
#
# Atom will evaluate this file each time a new window is opened. It is run
# after packages are loaded/activated and after the previous editor state
# has been restored.
#
# An example hack to log to the console when each text editor is saved.
#
# atom.workspace.observeTextEditors (editor) ->
#   editor.onDidSave ->
#     console.log "Saved! #{editor.getPath()}"

# https://gist.github.com/jasongilman/d1f70507bed021b48625
# These add some convenience commands for cutting, copying, pasting, deleting, and indenting Lisp expressions.

# Applies the function f and then reverts the cursor positions back to their original location
maintainingCursorPosition = (f)->
  editor = atom.workspace.getActiveTextEditor()
  currSelected = editor.getSelectedBufferRanges()
  f()
  editor.setSelectedScreenRanges(currSelected)

# Cuts the current block of lisp code.
atom.commands.add 'atom-text-editor', 'jason:cut-sexp', ->
  editor = atom.workspace.getActiveTextEditor()
  atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:up-sexp')
  atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:expand-selection')
  atom.commands.dispatch(atom.views.getView(editor), 'core:cut')

# Copies the current block of lisp code.
atom.commands.add 'atom-text-editor', 'jason:copy-sexp', ->
  maintainingCursorPosition ->
    editor = atom.workspace.getActiveTextEditor()
    atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:up-sexp')
    atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:expand-selection')
    atom.commands.dispatch(atom.views.getView(editor), 'core:copy')

# Pastes over current block of lisp code.
atom.commands.add 'atom-text-editor', 'jason:paste-sexp', ->
  editor = atom.workspace.getActiveTextEditor()
  atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:up-sexp')
  atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:expand-selection')
  atom.commands.dispatch(atom.views.getView(editor), 'core:paste')

# Deletes the current block of lisp code.
atom.commands.add 'atom-text-editor', 'jason:delete-sexp', ->
  editor = atom.workspace.getActiveTextEditor()
  atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:up-sexp')
  atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:expand-selection')
  atom.commands.dispatch(atom.views.getView(editor), 'core:delete')

# Indents the top level sexp.
atom.commands.add 'atom-text-editor', 'jason:indent-top-sexp', ->
  maintainingCursorPosition ->
    editor = atom.workspace.getActiveTextEditor()
    range = protoRepl.EditorUtils.getCursorInClojureTopBlockRange(editor)
    # Work around a lisp paredit bug where it can't indent a range if selected from the very beginning of the file
    start = range.start
    if start.column == 0 && start.row == 0
      start.column = 1

    editor.setSelectedScreenRange(range)
    atom.commands.dispatch(atom.views.getView(editor), 'lisp-paredit:indent')

# atom.commands.add 'atom-text-editor', 'mnewt:toggle-file-symbols', ->
#   editor = atom.workspace.getActiveTextEditor()
#   cursor = editor.getCursorScreenPosition()
#   atom.commands.dispatch(atom.views.getView(editor), 'atom-ctags:toggle-file-symbols')
#   editor.scrollToCursorPosition()
