# gerrit

[Gerrit](https://www.gerritcodereview.com/) features a [[git]] based patchset system.
The unit of review and contributions are single commit which can be proposed indepently, as opposed to _Pull Request_ or _Merge Request_ which are bound to a single branch.

## NoteDB

Since the version 3.x, gerrit stores all the metadata in git through the _NoteDB_ interface.
I wrote a python library to query and update the git reference: [pynotedb](https://github.com/softwarefactory-project/pynotedb/).
