# differential

Differential is a command-line viewer for unified diffs, written in Haskell.

## Usage

The following key bindings are available

  Key  | Action
  -----|-------
  TAB  | Switch focus between file list and diff (for control with arrow and page keys)
  n    | Show next file diff
  p    | Show previous file diff
  d    | Move to the next line in diff
  u    | Move to the previous line in diff
  SPC  | Move down one page in diff
  b    | Move up one page in diff
  DOWN | Same as either n or d
  UP   | Same as either p or u
  q    | Terminate the application

## Limitations

The diff cannot be read from stdin.  If you try to pipe to differential, you will see the following error message:

    differential: getTerminalAttributes: invalid argument (Bad file descriptor)

Use file names or shell redirects instead:

```sh
> differential random.patch
> differential <(hg diff)
```
