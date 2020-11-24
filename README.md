# Neuron Zettelkasten

The method I am using for my website <index>
Feel free to propose changes by clicking the `edit` button at the top of any page.

## Linking

Notes are linked by:

- direct link with `[[zettel]]`.
- branch with `<zettel>` or `[[[zettel]]]`, the other note becomes a branch of the current one.

## Update

Watch edits by running `neuron rib -w -S`.

Publish by running:

```
neuron rib && \
    git checkout pages && \
    rsync -a .neuron/output/ $(pwd)/ && \
    git add -A &&
    git commit -a -m "Updates" && \
    git push origin pages && \
    git checkout master
```
